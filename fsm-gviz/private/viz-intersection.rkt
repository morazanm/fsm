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

;; INTERSECTION VISUALIZATION



(define E-SCENE (empty-scene 1250 600))

;; upgi are unprocessed graphs
;; pgi are processed graph images
(struct viz-state (upgi pgi))

;; create-graph-imgs
;; ndfa ndfa -> img
;; Purpose: To create a graph image for the union
;; Assume: The intersection of the states of the given machines is empty
(define (create-graph-img M N)
  (define (complement-fsa ndfa)
    (let* [(new-finals (filter (λ (s) (not (member s (sm-finals ndfa)))) (sm-states ndfa)))]
      (make-dfa (sm-states ndfa)
                (sm-sigma ndfa)
                (sm-start ndfa)
                new-finals (sm-rules ndfa) 'no-dead)))
  (let* [(notM (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa M))))
         (notN (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa N))))]
    (overlay (above (sm-graph (complement-fsa (ndfa->dfa (sm-union notM notN))))
                    (text "Intersection of the ndfas" 20 'black))
             E-SCENE)))
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M N)
  (overlay (above (text "First ndfa:" 20 'black)
                  (sm-graph M)
                  (text "Second ndfa:" 20 'black)
                  (sm-graph N))
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
(define (run M N)
  (begin
    (big-bang
        (viz-state (create-graph-img M N) (make-init-grph-img M N))
      [on-draw draw-world]
      [on-key process-key]
      [name "FSM: intersection visualization"]))
  (void))