#lang fsm
(require "../../fsm-core/interface.rkt" "lib.rkt" "../../fsm-gui/graphViz/main.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)

(define FNAME "fsm")

;; L = nl
(define nl (make-ndfa '(S)
                      '(a b)
                      'S
                      '()
                      '()))

;; L = ab*
(define ab* (make-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-ndfa '(Z H B C D F)
                            '(a b)
                            'Z
                            '(F)
                            `((Z a H)
                              (Z a B)
                              (H a D)
                              (D ,EMP F)
                              (B a C)
                              (C b F)
                              (F b F))))
;; L = aab*
(define aab* (make-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))
;; L = a*
(define a* (make-dfa '(S D)
                     '(a b)
                     'S
                     '(S)
                     '((S a S)
                       (S b D)
                       (D a D)
                       (D b D))
                     'no-dead))


;; KLEENESTAR VISUZALIZATION


(define E-SCENE (empty-scene 1250 600))

;; upgi are unprocessed graphs
;; pgi are processed graph images
(struct viz-state (upgi pgi))

;; make-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph los s f)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state s) 'darkgreen]
                                     [else 'black])
                        'shape (if (member state f)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         los))

;; make-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph M new-start)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color (cond [(member rule (sm-rules M))
                                                              'violet]
                                                             [else 'black]))))
         graph
         (cons (list new-start EMP (sm-start M))
               (append (sm-rules M)
                       (map (λ (f) (list f EMP new-start))
                            (sm-finals M))))))

;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-init-edge-graph graph M new-start)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color (cond [(member rule (sm-rules M))
                                                              'violet]
                                                             [else 'black]))))
         graph
         (sm-rules M)))

;; create-graph-imgs
;; ndfa ndfa -> img
;; Purpose: To create a graph image for the union
;; Assume: The intersection of the states of the given machines is empty
(define (create-graph-img M)
  (let* [(new-start (generate-symbol 'K (sm-states M)))
         (new-states (cons new-start (sm-states M)))
         (new-finals (cons new-start (sm-finals M)))
         (added-edges (list (list new-start EMP (sm-start M))
                            (map (λ (f) (list f EMP new-start))
                                 (sm-finals M)))) ]
    (overlay (above (graph->bitmap (make-edge-graph (make-node-graph
                                                     (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                     new-states
                                                     new-start
                                                     new-finals)
                                                    M new-start))
                    (text "Kleenestar of the ndfa \n" 20 'black)
                    (text (format "Generated starting state: ~a \n" new-start) 20 'black)
                    (text (format "Added edges: ~a \n" added-edges) 20 'black))
             E-SCENE)))
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M)
  (overlay
   (above
    (graph->bitmap (make-init-edge-graph (make-node-graph
                                          (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                          (sm-states M)
                                          (sm-start M)
                                          (sm-finals M))
                                         M (sm-start M)))
    (text "Starting ndfa \n" 20 'black))
   E-SCENE))


;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (viz-state (viz-state-pgi a-vs)
                    (viz-state-upgi a-vs))]
        [(key=? "left" a-key)
         (viz-state (viz-state-pgi a-vs)
                    (viz-state-upgi a-vs))]           
        [else a-vs]))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (viz-state-pgi a-vs)))
        (height (image-height (viz-state-pgi a-vs)))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (overlay (resize-image (viz-state-pgi a-vs) (image-width E-SCENE) (image-height E-SCENE))
                 E-SCENE)
        (overlay (viz-state-pgi a-vs) E-SCENE))))

;; run-function
;; run-function
(define (run M)
  (begin
    (big-bang
        (viz-state (create-graph-img M) (make-init-grph-img M))
      [on-draw draw-world]
      [on-key process-key]
      [name "FSM: kleenestar visualization"]))
  (void))
