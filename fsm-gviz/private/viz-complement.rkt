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

(define M (make-ndfa '(Z H B C D)
                     '(a b)
                     'Z
                     '(C D)
                     `((Z a H)
                       (Z a B)
                       (H a D)
                       (B a C))))


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


;; COMPLEMENT VISUZALIZATION


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
            #:atb (hash 'color (cond [(and (eq? state s)
                                           (member state f))
                                      'violet]
                                     [(eq? state s) 'darkgreen]
                                     [(member state f)
                                      'violet] 
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
(define (make-edge-graph graph M)
  (let* [(new-finals (filter (λ (s) (not (member s (sm-finals (ndfa->dfa M))))) (sm-states (ndfa->dfa M))))]
    (foldl (λ (rule result) (add-edge result
                                      (second rule)
                                      (if (equal? (first rule) '())
                                          'ds
                                          (first rule))
                                      (if (equal? (third rule) '())
                                          'ds
                                          (third rule))
                                      #:atb (hash 'fontsize 20
                                                  'style 'solid )))
           graph
           (sm-rules (ndfa->dfa M)))))


;; create-graph-img
;; ndfa -> img
;; Purpose: To create a graph image for the union
(define (create-graph-img M)
  (eq? (sm-type M) 'dfa)
  (let* [(new-finals (filter (λ (s) (not (member s (sm-finals M)))) (sm-states M)))]
    (overlay (above (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                     (sm-states M) 
                                                                     (sm-start M)
                                                                     new-finals)
                                                    M))
                    (text "Complement of the ndfa" 20 'black)
                    (text (format "New final states: ~a" new-finals) 20 'black)
                    (text (format "Starting state: ~a" (sm-start M)) 20 'black))
             E-SCENE))
  (let* [(new-finals (filter (λ (s) (not (member s (sm-finals (ndfa->dfa M))))) (sm-states (ndfa->dfa M))))]
    (overlay (above (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                     (sm-states (ndfa->dfa M)) 
                                                                     (sm-start (ndfa->dfa M))
                                                                     new-finals)
                                                    M))
                    (text "Complement of the ndfa" 20 'black)
                    (text (format "New final states: ~a" new-finals) 20 'black)
                    (text (format "Starting state: ~a" (sm-start M)) 20 'black))
             E-SCENE)))

     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M)
  (overlay
   (above
    (sm-graph M)
    (text "Starting ndfa:" 20 'black))
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
  (viz-state-pgi a-vs))

;; run-function
;; run-function
(define (run M)
  (begin
    (big-bang
        (viz-state (create-graph-img M) (make-init-grph-img M))
      [on-draw draw-world]
      [on-key process-key]
      [name "FSM: complement visualization"]))
  (void))

(define M (make-ndfa '(Z H B C D)
                     '(a b)
                     'Z
                     '(C D)
                     `((Z a H)
                       (Z a B)
                       (H a D)
                       (B a C))))

(run M)
