#lang racket/base

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt"
         racket/list
         racket/function)
(provide concat-viz)

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

;; imsg-img
;; Image representing the informative messages
(define imsg-img
  (above (text "Concatenation of the ndfas" 20 'black)
         (text (format "Starting state:") 20 'black)
         (text (format "Final state(s):") 20 'black)
         (text (format "Generated edges:") 20 'black)))

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

;; L = nl
(define nl (make-unchecked-ndfa '(S) '(a b) 'S '() '()))

;; L = ab*
(define ab* (make-unchecked-ndfa '(S A) '(a b) 'S '(A) '((S a A) (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b*
  (make-unchecked-ndfa '(Z H B C D F)
                       '(a b)
                       'Z
                       '(F)
                       `((Z a H) (Z a B) (H a D) (D ,EMP F) (B a C) (C b F) (F b F))))
;; L = aab*
(define aab* (make-unchecked-ndfa '(W X Y) '(a b) 'W '(Y) '((W a X) (X a Y) (Y b Y))))
;; L = a*
(define a* (make-unchecked-dfa '(S D) '(a b) 'S '(S) '((S a S) (S b D) (D a D) (D b D)) 'no-dead))

;; CONCATENATION VISUALIZATION

;; graph-struct
;; grph - graph structure
;; inf - informative message
(struct graph-struct (grph inf))

;; make-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph los s f)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color
                                 (cond
                                   [(eq? state s) 'darkgreen]
                                   [else 'black])
                                 'shape
                                 (if (member state f) 'doublecircle 'circle)
                                 'label
                                 (if (equal? state '()) 'ds state)
                                 'fontcolor
                                 'black
                                 'font
                                 "Sans")))
         graph
         los))

;; make-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph M N)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (if (equal? (first rule) '()) 'ds (first rule))
                     (if (equal? (third rule) '()) 'ds (third rule))
                     #:atb (hash 'fontsize
                                 20
                                 'style
                                 'solid
                                 'color
                                 (cond
                                   [(member rule (sm-rules M)) 'violet]
                                   [(and (eq? (second rule) EMP)
                                         (and (not (member rule (sm-rules M)))
                                              (not (member rule (sm-rules N)))))
                                    'black]
                                   [else 'orange]))))
         graph
         (append (sm-rules M) (sm-rules N) (map (λ (f) (list f EMP (sm-start N))) (sm-finals M)))))

;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-first-edge-graph graph M new-start)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (if (equal? (first rule) '()) 'ds (first rule))
                     (if (equal? (third rule) '()) 'ds (third rule))
                     #:atb (hash 'fontsize
                                 20
                                 'style
                                 'solid
                                 'color
                                 (cond
                                   [(member rule (sm-rules M)) 'orange]
                                   [else 'black]))))
         graph
         (sm-rules M)))

;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-second-edge-graph graph M new-start)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (if (equal? (first rule) '()) 'ds (first rule))
                     (if (equal? (third rule) '()) 'ds (third rule))
                     #:atb (hash 'fontsize
                                 20
                                 'style
                                 'solid
                                 'color
                                 (cond
                                   [(member rule (sm-rules M)) 'violet]
                                   [else 'black]))))
         graph
         (sm-rules M)))

;; create-graph-structs
;; ndfa ndfa -> graph
;; Purpose: To create a graph structure for the concatenation
;; Assume: The intersection of the states of the given machines is empty
(define (create-graph-structs M N)
  (let* ([new-start (sm-start M)]
         [edge-added (map (λ (f) (list f EMP (sm-start N))) (sm-finals M))]
         [new-states (append (sm-states M) (sm-states N))]
         [new-finals (sm-finals N)])
    (graph-struct
     (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                       new-states
                                       new-start
                                       new-finals)
                      M
                      N)
     (above (text "Concatenation of the ndfas" 20 'black)
            (text (format "Starting state: ~a" new-start) 20 'black)
            (text (format "Final state(s): ~a" new-finals) 20 'black)
            (text (format "Generated edges: ~a" edge-added) 20 'black)))))

;; make-init-grph-struct
;; ndfa ndfa -> graph
;; Purpose: To make the graph structure of the initial ndfa's
(define (make-init-grph-struct M N)
  (let* ([graph1 (make-second-edge-graph
                  (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                   (sm-states M)
                                   (sm-start M)
                                   (sm-finals M))
                  M
                  (sm-start M))]
         [graph2 (make-first-edge-graph
                  (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                   (sm-states N)
                                   (sm-start N)
                                   (sm-finals N))
                  N
                  (sm-start N))])
    (graph-struct (list graph1 graph2)
                  (above (text "Initial ndfas" 20 'black) (text "" 20 'black)))))

;; draw-imsg
;; imsg -> img
;; Purpose: To draw the informative message
(define (draw-imsg a-imsg)
  (zipper-current (graph-struct-inf a-imsg)))

;; viz-state -> viz-state
;; Updates the informative messages to the next stage of the seqeuence
(define (right-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-next (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the previous stage of the seqeuence
(define (left-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-prev (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the beginning of the seqeuence
(define (up-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-to-begin (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the end of the seqeuence
(define (down-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-to-end (graph-struct-inf a-graph-struct))])])]))))

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

;; concat-viz
;; fsa fsa -> void
(define (concat-viz M N)
  (let* ([renamed-machine (if (ormap (λ (x) (member x (sm-states M))) (sm-states N))
                             (rename-states-fsa (sm-states M) N)
                             N)]
         [graphs (map graph-struct-grph
                  (list (make-init-grph-struct M N) (create-graph-structs M renamed-machine)))])
    (run-viz graphs
             (list->vector (map (lambda (x) (if (list? x)
                                                (lambda (y z) (above y z))
                                                (lambda (y) y))) graphs))
             #;(lambda ()
               (apply above (map graph->bitmap (graph-struct-grph (make-init-grph-struct M N)))))
             MIDDLE-E-SCENE
             E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages
              draw-imsg
              (graph-struct (list->zipper (map (lambda (x) '())
                                               (list (make-init-grph-struct M N)
                                                     (create-graph-structs M renamed-machine))))
                            (list->zipper (map graph-struct-inf
                                               (list (make-init-grph-struct M N)
                                                     (create-graph-structs M renamed-machine)))))
              (bounding-limits 0 0 0 0))
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       E-SCENE-HEIGHT
                                                       (bounding-limits-height RULE-YIELD-DIMS)
                                                       INS-TOOLS-BUFFER)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       E-SCENE-HEIGHT
                                                       (bounding-limits-height RULE-YIELD-DIMS)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))))
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
                                      ( [ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [W-KEY-DIMS viz-zoom-in identity]
                                        [S-KEY-DIMS viz-zoom-out identity]
                                        [R-KEY-DIMS viz-max-zoom-out identity]
                                        [E-KEY-DIMS viz-reset-zoom identity]
                                        [F-KEY-DIMS viz-max-zoom-in identity]
                                        ))
             'concat-viz)))
