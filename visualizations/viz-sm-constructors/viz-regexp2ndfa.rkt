#lang racket/base
(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../../fsm-core/private/regexp.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt"
         racket/list
         racket/function)

(provide regexp2ndfa-viz)

(define E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER FONT-SIZE
                                               (list (list ARROW-UP-KEY "Restart")
                                                     (list ARROW-RIGHT-KEY "Forward")
                                                     (list ARROW-LEFT-KEY "Backward")
                                                     (list ARROW-DOWN-KEY "Finish")
                                                     (list CURSOR "Hold to drag")
                                                     (list W-KEY "Zoom in")
                                                     (list S-KEY "Zoom out")
                                                     (list R-KEY "Min zoom")
                                                     (list E-KEY "Mid zoom")
                                                     (list F-KEY "Max zoom"))))

(define imsg-img (text "Starting ndfa" FONT-SIZE 'black))

(define E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height imsg-img)
                          (image-height E-SCENE-TOOLS)))
(define MIDDLE-E-SCENE (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))

(define RULE-YIELD-DIMS
  (bounding-limits 0
                   (image-width imsg-img)
                   E-SCENE-HEIGHT
                   (+ E-SCENE-HEIGHT (image-height imsg-img))))

(create-bounding-limits E-SCENE-WIDTH E-SCENE-HEIGHT (image-width E-SCENE-TOOLS) RULE-YIELD-DIMS FONT-SIZE LETTER-KEY-WIDTH-BUFFER INS-TOOLS-BUFFER
                        ((ARROW-UP-KEY "Restart")
                         (ARROW-RIGHT-KEY "Forward")
                         (ARROW-LEFT-KEY "Backward")
                         (ARROW-DOWN-KEY "Finish")
                         (CURSOR "Hold to drag")
                         (W-KEY "Zoom in")
                         (S-KEY "Zoom out")
                         (R-KEY "Min zoom")
                         (E-KEY "Mid zoom")
                         (F-KEY "Max zoom")))

(define R0 (make-unchecked-union (make-unchecked-singleton "a") (null-regexp)))

(define R1
  (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "a")
                                                   (make-unchecked-singleton "b"))))
(define R11
  (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "b")
                                                   (make-unchecked-singleton "c"))))
(define R12
  (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "a")
                                                   (make-unchecked-singleton "c"))))

(define R13 (make-unchecked-union (make-unchecked-union R1 R11) R12))
(define R8 (make-unchecked-concat (make-unchecked-singleton "a") (make-unchecked-singleton "b")))

(define R9 (make-unchecked-concat (make-unchecked-singleton "b") (make-unchecked-singleton "a")))

(define R10 (make-unchecked-union (make-unchecked-kleenestar R8) (make-unchecked-kleenestar R9)))

(define R2 (make-unchecked-concat (make-unchecked-singleton "m") R1))

(define R3 (make-unchecked-kleenestar R2))

(define R4 (make-unchecked-kleenestar (make-unchecked-union R3 R2)))

(define R5 (make-unchecked-concat (make-unchecked-singleton "m") R0))

(define R6 (make-unchecked-kleenestar R5))

(define R7 (make-unchecked-union R0 R5))

(define E-SCENE (empty-scene 1250 600))

;; grph is a (listof img) of graphs used to build an ndfa from regexp
;; edge is an edge that has been expanded to build an ndfa from regexp
(struct gedge (grph edge))

;; dgraph2logedges
;; dgraph edge --> (listof gedges)
;; Purpose: Create a list of digraph-edge structures
(define (dgraph2logedges dgraph edge . issimp?)

  ;; digraph --> Boolean
  (define (only-simple-edges? grph)
    (andmap
     (λ (e) (or (empty-regexp? (second e)) (singleton-regexp? (second e)) (null-regexp? (second e))))
     grph))

  ;; dgraph --> gedge
  ;; Purpose: To extract the first nonsimple edge in the dgraph
  ;; Assumption: dgraph has a nonsimple edge
  (define (extract-first-nonsimple grph)
    (first (filter (λ (e) (and (not (empty-regexp? (second e))) (not (singleton-regexp? (second e)))))
                   grph)))

  ;; dgraph edge (listof dgraph) --> (listof dgraph)
  ;; Purpose: To create a list of dgraphs with  the expanded edge removed
  ;; and replaced with the appropriate edges
  (define (bfs grph edge acc)

    (define grph-states (remove-duplicates (append-map (λ (e) (list (first e) (third e))) grph)))

    (cond
      [(only-simple-edges? grph) (cons (gedge grph edge) acc)]
      [(and (not (null? issimp?)) (first issimp?)) (cons (gedge grph void) acc)]
      [else
       (let* ([next-edge (extract-first-nonsimple grph)]
              [fromst (first next-edge)]
              [rexp (second next-edge)]
              [tost (third next-edge)])
         (cond
           [(union-regexp? rexp)
            (let* ([newi1 (gen-state grph-states)]
                   [newi2 (gen-state (cons newi1 grph-states))]
                   [newi3 (gen-state (append (list newi1 newi2) grph-states))]
                   [newi4 (gen-state (append (list newi1 newi2 newi3) grph-states))])
              (bfs (append (list (list fromst (empty-regexp) newi1)
                                 (list fromst (empty-regexp) newi2)
                                 (list newi1 (union-regexp-r1 rexp) newi3)
                                 (list newi2 (union-regexp-r2 rexp) newi4)
                                 (list newi3 (empty-regexp) tost)
                                 (list newi4 (empty-regexp) tost))
                           (remove next-edge grph))
                   next-edge
                   (cons (gedge grph edge) acc)))]
           [(concat-regexp? rexp)
            (let* ([istate1 (gen-state grph-states)] [istate2 (gen-state (cons istate1 grph-states))])
              (bfs (append (list (list fromst (concat-regexp-r1 rexp) istate1)
                                 (list istate1 (empty-regexp) istate2)
                                 (list istate2 (concat-regexp-r2 rexp) tost))
                           (remove next-edge grph))
                   next-edge
                   (cons (gedge grph edge) acc)))]
           [else
            (let* ([istart1 (gen-state grph-states)] [istart2 (gen-state (cons istart1 grph-states))])
              (bfs (append (list (list fromst (empty-regexp) istart1)
                                 (list istart1 (empty-regexp) tost)
                                 (list istart1 (empty-regexp) istart2)
                                 (list istart2 (kleenestar-regexp-r1 rexp) istart2)
                                 (list istart2 (empty-regexp) tost))
                           (remove next-edge grph))
                   next-edge
                   (cons (gedge grph edge) acc)))]))]))
  (bfs dgraph edge '()))

;; create-nodes
;; graph dgraph edge -> graph
;; Purpose: To add the nodes to the graph
(define (create-nodes graph dgraph edge)
  (define (states-only dgraph)
    (remove-duplicates (append-map (λ (e) (list (first e) (third e))) dgraph)))
  (foldl
   (λ (state result)
     (add-node result
               state
               #:atb
               (hash 'color
                     (cond
                       [(eq? state 'S)
                        (if (and (not (empty? edge)) (not (equal? void edge)) (eq? (first edge) 'S))
                            'violet
                            'darkgreen)]
                       [(eq? state 'F)
                        (if (and (not (empty? edge)) (not (equal? void edge)) (eq? (third edge) 'F))
                            'violet
                            'black)]
                       [(or (eq? state (first edge)) (eq? state (third edge))) 'violet]
                       [else 'black])
                     'shape
                     (if (eq? state 'F) 'doublecircle 'circle)
                     'label
                     (if (equal? state '()) 'ds state)
                     'fontcolor
                     'black
                     'font
                     "Sans")))
   graph
   (states-only dgraph)))

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges graph dgraph)
  (foldl (λ (rule result)
           (add-edge result
                     (printable-regexp (second rule))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14 'style 'solid 'fontname "Sans")))
         graph
         dgraph))

;; create-graphic
;; graph edge -> struct
;; Purpose: To create a graph structure for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graphic dgraph edge)
  (create-edges
   (create-nodes (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) dgraph edge)
   dgraph))

;; imsg
;; edge - edge to expand in the graph structure
(struct imsg-state (edge))

;; draw-imsg
;; imsg -> img
;; Purpose: To draw informative messages
(define (draw-imsg a-imsg)
  (cond
    [(zipper-at-begin? (imsg-state-edge a-imsg)) (text "Starting NDFA" 24 'black)]
    [(= (zipper-idx (imsg-state-edge a-imsg)) 1) (text "Simplified initial regexp" 24 'black)]
    [else
     (beside (text (format "Expanded regexp: ~a on edge from state"
                           (printable-regexp (second (zipper-current (imsg-state-edge a-imsg)))))
                   24
                   'black)
             (text (format " ~a" (first (zipper-current (imsg-state-edge a-imsg)))) 24 'violet)
             (text (format " to state ") 24 'black)
             (text (format "~a" (third (zipper-current (imsg-state-edge a-imsg)))) 24 'violet))]))

;; create-graphs
;; (listof gedges) -> (listof graph)
;; Purpose: To create a list of graph structures
(define (create-graphs gedges)
  (if (empty? gedges)
      empty
      (cons (create-graphic (gedge-grph (first gedges)) (gedge-edge (first gedges)))
            (create-graphs (rest gedges)))))

;; create-init-nodes
;; graph dgraph -> graph
;; Purpose: To add the nodes to the graph
(define (create-init-nodes graph)
  (add-node
   (add-node graph 'F #:atb (hash 'color 'black 'shape 'doublecircle 'fontcolor 'black 'font "Sans"))
   'S
   #:atb (hash 'color 'darkgreen 'shape 'circle 'fontcolor 'black 'font "Sans")))

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-init-edges graph dgraph)
  (add-edge graph
            (printable-regexp (second dgraph))
            (first dgraph)
            (third dgraph)
            #:atb (hash 'fontsize 14 'style 'solid 'fontname "Sans")))

;; create-init-graph
;; list -> graph
;; Purpose: To create an initial graph structure
(define (create-init-graph a-list)
  (create-init-edges (create-init-nodes (create-graph 'dgraph
                                                      #:atb (hash 'rankdir "LR" 'font "Sans")))
                     a-list))

;; viz-state -> viz-state
;; Updates the informative messages to the next stage of the seqeuence
(define (right-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy imsg-state
                                     a-imsg-state
                                     [edge (zipper-next (imsg-state-edge a-imsg-state))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the previous stage of the seqeuence
(define (left-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy imsg-state
                                     a-imsg-state
                                     [edge (zipper-prev (imsg-state-edge a-imsg-state))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the beginning of the seqeuence
(define (up-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy imsg-state
                                     a-imsg-state
                                     [edge (zipper-to-begin (imsg-state-edge a-imsg-state))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the end of the seqeuence
(define (down-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy imsg-state
                                     a-imsg-state
                                     [edge (zipper-to-end (imsg-state-edge a-imsg-state))])])]))))

(define viz-go-next
  (go-next E-SCENE-WIDTH
           E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))
(define viz-go-prev
  (go-prev E-SCENE-WIDTH
           E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))
(define viz-go-to-begin
  (go-to-begin E-SCENE-WIDTH
               E-SCENE-HEIGHT
               NODE-SIZE
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM-FLOOR
               PERCENT-BORDER-GAP))
(define viz-go-to-end
  (go-to-end E-SCENE-WIDTH
             E-SCENE-HEIGHT
             NODE-SIZE
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             PERCENT-BORDER-GAP))

(define viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))
(define viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))
(define viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))
(define viz-max-zoom-in
  (max-zoom-in E-SCENE-WIDTH
               E-SCENE-HEIGHT
               ZOOM-INCREASE
               ZOOM-DECREASE
               NODE-SIZE
               PERCENT-BORDER-GAP
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM))
(define viz-reset-zoom
  (reset-zoom E-SCENE-WIDTH
              E-SCENE-HEIGHT
              ZOOM-INCREASE
              ZOOM-DECREASE
              NODE-SIZE
              PERCENT-BORDER-GAP
              DEFAULT-ZOOM-CAP
              DEFAULT-ZOOM))

;; regexp2ndfa-viz
;; regexp -> void
(define (regexp2ndfa-viz regexp)
  (let* ([logedges
          (append (list (last (dgraph2logedges (list (list 'S (simplify-regexp regexp) 'F)) '() #t)))
                  (rest (reverse (dgraph2logedges (list (list 'S (simplify-regexp regexp) 'F))
                                                  '()))))]
         [graphs (cons (create-init-graph (list 'S regexp 'F)) (create-graphs logedges))])
    (run-viz graphs
             (list->vector (map (lambda (x) (lambda (y) y)) graphs))
             #;(lambda () (graph->bitmap (create-init-graph (list 'S regexp 'F))))
             MIDDLE-E-SCENE
             E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (imsg-state (list->zipper (cons (text "Starting NDFA" 24 'black)
                                                                   (map gedge-edge logedges))))
                                   (bounding-limits 0 0 0 0))
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width imsg-img)
                                                    E-SCENE-HEIGHT
                                                    (+ E-SCENE-HEIGHT (image-height imsg-img))))
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key ["right" viz-go-next right-key-pressed]
                                     ["left" viz-go-prev left-key-pressed]
                                     ["up" viz-go-to-begin up-key-pressed]
                                     ["down" viz-go-to-end down-key-pressed]
                                     ["w" viz-zoom-in identity]
                                     ["s" viz-zoom-out identity]
                                     ["r" viz-max-zoom-out identity]
                                     ["f" viz-max-zoom-in identity]
                                     ["e" viz-reset-zoom identity]
                                     ["wheel-down" viz-zoom-in identity]
                                     ["wheel-up" viz-zoom-out identity])
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ()
                                      ([ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                       [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                       [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                       [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                       [W-KEY-DIMS viz-zoom-in identity]
                                       [S-KEY-DIMS viz-zoom-out identity]
                                       [R-KEY-DIMS viz-max-zoom-out identity]
                                       [E-KEY-DIMS viz-reset-zoom identity]
                                       [F-KEY-DIMS viz-max-zoom-in identity]))
             'regexp2ndfa)))