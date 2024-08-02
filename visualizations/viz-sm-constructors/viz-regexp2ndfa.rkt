#lang racket
(require "../../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../viz-lib/resize-sm-image.rkt"
         ;"definitions-viz.rkt"
         ;"run-viz.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../../fsm-core/private/regexp.rkt"
         "../viz-lib/zipper.rkt")
(provide regexp2ndfa-viz)

(define FNAME "fsm")

(define R0 (make-unchecked-union (make-unchecked-singleton "a")
                                 (null-regexp)))

(define R1 (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "a")
                                                            (make-unchecked-singleton "b"))))
(define R11 (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "b")
                                                             (make-unchecked-singleton "c"))))
(define R12 (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "a")
                                                             (make-unchecked-singleton "c"))))

(define R13 (make-unchecked-union (make-unchecked-union R1 R11) R12))
(define R8 (make-unchecked-concat (make-unchecked-singleton "a")
                                  (make-unchecked-singleton "b")))

(define R9 (make-unchecked-concat (make-unchecked-singleton "b")
                                  (make-unchecked-singleton "a")))

(define R10 (make-unchecked-union (make-unchecked-kleenestar R8) (make-unchecked-kleenestar R9)))

(define R2 (make-unchecked-concat (make-unchecked-singleton "m") R1))

(define R3 (make-unchecked-kleenestar R2))

(define R4 (make-unchecked-kleenestar (make-unchecked-union R3 R2)))

(define R5 (make-unchecked-concat (make-unchecked-singleton "m") R0))

(define R6 (make-unchecked-kleenestar R5))

(define R7 (make-unchecked-union R0 R5))

(define E-SCENE (empty-scene 1250 600))

(define E-SCENE-TOOLS (overlay (beside (above (above (triangle 30 'solid 'black)
                                                     (rectangle 10 30 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Restart the visualization" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rectangle 30 10 'solid 'black)
                                                      (rotate 270 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Move one step forward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rotate 90 (triangle 30 'solid 'black))
                                                      (rectangle 30 10 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Move one step backward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (above (rectangle 10 30 'solid 'black)
                                                     (rotate 180 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Complete the visualization" 18 'black))
                                       )
                               (empty-scene 1250 100)))

;; grph is a (listof img) of graphs used to build an ndfa from regexp
;; edge is an edge that has been expanded to build an ndfa from regexp
(struct gedge (grph edge))

;; dgraph2logedges
;; dgraph edge --> (listof gedges)
;; Purpose: Create a list of digraph-edge structures
(define (dgraph2logedges dgraph edge . issimp?)
  
  ;; digraph --> Boolean
  (define (only-simple-edges? grph)
    (andmap (λ (e) (or (empty-regexp? (second e))
                       (singleton-regexp? (second e))
                       (null-regexp? (second e))))
            grph))

  ;; dgraph --> gedge
  ;; Purpose: To extract the first nonsimple edge in the dgraph
  ;; Assumption: dgraph has a nonsimple edge
  (define (extract-first-nonsimple grph)
    (first (filter (λ (e) (and (not (empty-regexp? (second e)))
                               (not (singleton-regexp? (second e)))))
                   grph)))
  
  ;; dgraph edge (listof dgraph) --> (listof dgraph)
  ;; Purpose: To create a list of dgraphs with  the expanded edge removed
  ;; and replaced with the appropriate edges
  (define (bfs grph edge acc)

    (define grph-states (remove-duplicates (append-map (λ (e) (list (first e) (third e))) grph)))
    
    (cond [(only-simple-edges? grph) (cons (gedge grph edge) acc)]
          [(and (not (null? issimp?))
                (first issimp?))
           (cons (gedge grph (void)) acc)]
          [else 
           (let* [(next-edge (extract-first-nonsimple grph))
                  (fromst (first next-edge))
                  (rexp (second next-edge))
                  (tost (third next-edge))]
             (cond [(union-regexp? rexp)
                    (let* [(newi1 (gen-state grph-states))
                           (newi2 (gen-state (cons newi1 grph-states)))
                           (newi3 (gen-state (append (list newi1 newi2) grph-states)))
                           (newi4 (gen-state (append (list newi1 newi2 newi3) grph-states)))]
                      (bfs
                       (append (list (list fromst (empty-regexp) newi1)
                                     (list fromst (empty-regexp) newi2)
                                     (list newi1 (union-regexp-r1 rexp) newi3)
                                     (list newi2 (union-regexp-r2 rexp) newi4)
                                     (list newi3 (empty-regexp) tost)
                                     (list newi4 (empty-regexp) tost))
                               (remove next-edge grph))
                       next-edge
                       (cons (gedge grph edge) acc)))]
                   [(concat-regexp? rexp)
                    (let* [(istate1 (gen-state grph-states))
                           (istate2 (gen-state (cons istate1 grph-states)))]
                      (bfs (append (list (list fromst (concat-regexp-r1 rexp) istate1)
                                         (list istate1 (empty-regexp) istate2)
                                         (list istate2 (concat-regexp-r2 rexp) tost))
                                   (remove next-edge grph))
                           next-edge
                           (cons (gedge grph edge) acc)))]
                   [else
                    (let* [(istart1 (gen-state grph-states))
                           (istart2 (gen-state (cons istart1 grph-states)))]
                      (bfs
                       (append (list (list fromst (empty-regexp) istart1)
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
    (remove-duplicates
     (append-map (λ (e) (list (first e) (third e))) dgraph)))
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state 'S)
                                      (if (and (not (empty? edge))
                                               (not (void? edge))
                                               (eq? (first edge) 'S))
                                          'violet
                                          'darkgreen)]
                                     [(eq? state 'F)
                                      (if (and (not (empty? edge))
                                               (not (void? edge))
                                               (eq? (third edge) 'F))
                                          'violet
                                          'black)]
                                     [(or (eq? state (first edge))
                                          (eq? state (third edge)))
                                      'violet]
                                     [else 'black])                                   
                        'shape (if (eq? state 'F)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
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
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         dgraph))


;; create-graph-img
;; graph edge -> img
;; Purpose: To create a graph img for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graph dgraph edge)
  (create-edges
   (create-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                      'font "Sans"))
    dgraph
    edge)
   dgraph))


;; imsg
(struct imsg-state (edge))


;; draw-imsg
;; imsg -> img
(define (draw-imsg a-imsg)
  (cond [(empty? (imsg-state-edge a-imsg) (text "Starting NDFA" 24 'black))]
        [(void? (imsg-state-edge a-imsg)) (text "Simplified initial regexp" 24 'black)]
        [else (beside (text (format "Expanded regexp: ~a on edge from state" (printable-regexp (second (imsg-state-edge a-imsg)))) 24 'black)
                      (text (format " ~a" (first (imsg-state-edge a-imsg))) 24 'violet)
                      (text (format " to state ") 24 'black)
                      (text (format "~a" (third edge)) 24 'violet))]))


;; create-graph-imgs
;; (listof gedges) -> (listof image)
;; Purpose: To create a list of graph images
(define (create-graphs gedges)
  (if (empty? gedges)
      empty
      (cons (create-graph (gedge-grph (first gedges))
                          (gedge-edge (first gedges)))
            (create-graphs (rest gedges)))))


;; create-init-nodes
;; graph dgraph -> graph
;; Purpose: To add the nodes to the graph
(define (create-init-nodes graph)
  (add-node
   (add-node
    graph
    'F
    #:atb (hash 'color 'black                                   
                'shape 'doublecircle
                'fontcolor 'black
                'font "Sans"))
   'S
   #:atb (hash 'color 'darkgreen                                   
               'shape 'circle
               'fontcolor 'black
               'font "Sans")))

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-init-edges graph dgraph)
  (add-edge graph
            (printable-regexp (second dgraph))
            (first dgraph)
            (third dgraph)
            #:atb (hash 'fontsize 14
                        'style 'solid
                        'fontname "Sans"
                        )))



;; create-init-graph
;; list -> img
;; Purpose: To create an initial graph image
(define (create-init-graph a-list)
  (create-init-edges
   (create-init-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                      'font "Sans")))
   a-list))





;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
#;(define (draw-world a-vs)
    (let [(width (image-width (first (viz-state-pimgs a-vs))))
          (height (image-height (first (viz-state-pimgs a-vs))))]
      (if (or (> width (image-width E-SCENE))
              (> height (image-height E-SCENE)))
          (above (overlay (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE))
                          E-SCENE) E-SCENE-TOOLS)
          (above (overlay (first (viz-state-pimgs a-vs)) E-SCENE) E-SCENE-TOOLS))))

;; regexp2ndfa-viz
;; regexp -> void
(define (regexp2ndfa-viz regexp)
  (let* [(logedges (append
                    (list
                     (last (dgraph2logedges
                            (list (list 'S (simplify-regexp regexp) 'F)) 
                            '()
                            #t)))
                    (rest (reverse (dgraph2logedges
                                    (list (list 'S (simplify-regexp regexp) 'F))
                                    '())))))
         (graphs (create-graphs logedges))]
    (run-viz (list (create-init-graph (list 'S regexp 'F)) graphs)
             (graph->bitmap (create-init-graph (list 'S regexp 'F)))
             MIDDLE-E-SCENE
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (imsg-state (list->zipper (gedge-edge logedges)))
                                   (bounding-limits 0 0 0 0)
                                   )
             (instructions-graphic
              E-SCENE-TOOLS
              (bounding-limits 0 0 0 0))
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key (list (list "right" go-next right-key-pressed)
                                           (list "left" go-prev left-key-pressed)
                                           (list "up" go-to-begin up-key-pressed)
                                           (list "down" go-to-end down-key-pressed)
                                           (list "w" zoom-in identity)
                                           (list "s" zoom-out identity)
                                           (list "r" max-zoom-out identity)
                                           (list "f" max-zoom-in identity)
                                           (list "e" reset-zoom identity)
                                           (list "wheel-down" zoom-in identity)
                                           (list "wheel-up" zoom-out identity)
                                           )
                                     )
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      (list)
                                      (list (list ARROW-UP-KEY-DIMS go-to-begin up-key-pressed)
                                            (list ARROW-DOWN-KEY-DIMS go-to-end down-key-pressed)
                                            (list ARROW-LEFT-KEY-DIMS go-prev left-key-pressed)
                                            (list ARROW-RIGHT-KEY-DIMS go-next right-key-pressed)
                                            (list W-KEY-DIMS zoom-in identity)
                                            (list S-KEY-DIMS zoom-out identity)
                                            (list R-KEY-DIMS max-zoom-out identity)
                                            (list E-KEY-DIMS reset-zoom identity)
                                            (list F-KEY-DIMS max-zoom-in identity)
                                            (list A-KEY-DIMS identity a-key-pressed)
                                            (list D-KEY-DIMS identity d-key-pressed)))
             )))




    
#; (run-viz (viz-state loimgs (list (create-init-graph (list 'S regexp 'F)))) draw-world 'regexp2ndfa)



