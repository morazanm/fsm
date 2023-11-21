#lang fsm
(require "../../../fsm-core/interface.rkt" "../lib.rkt" "../../../fsm-gui/graphViz/main.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)
(require "run-viz.rkt")
(provide complement-viz)

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
;; Purpose: To create a graph image for complement
(define (create-graph-img M)
  (let* [(new-finals (filter (λ (s) (not (member s (sm-finals M)))) (sm-states M)))]
    (overlay (above (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                     (sm-states M) 
                                                                     (sm-start M)
                                                                     new-finals)
                                                    M))
                    (text "Complement of MD" 20 'black)
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
    (if (eq? (sm-type M) 'dfa)
        (text "DM: input machine" 20 'black)
        (text "M: input machine" 20 'black)))
   E-SCENE))


;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upgi a-vs))
             a-vs
             (viz-state (rest (viz-state-upgi a-vs))
                        (cons (first (viz-state-upgi a-vs))
                              (viz-state-pgi a-vs))))]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pgi a-vs)) 1)
             a-vs
             (viz-state (cons (first (viz-state-pgi a-vs))
                              (viz-state-upgi a-vs))
                        (rest (viz-state-pgi a-vs))))]
        [(key=? "down" a-key)
         (viz-state '()
                    (append (reverse (viz-state-upgi a-vs))
                            (viz-state-pgi a-vs)))]
        [else a-vs]))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pgi a-vs))))
        (height (image-height (first (viz-state-pgi a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (overlay (resize-image (first (viz-state-pgi a-vs)) (image-width E-SCENE) (image-height E-SCENE))
                 E-SCENE)
        (overlay (first (viz-state-pgi a-vs)) E-SCENE))))

;; complement-viz
;; fsa -> void
(define (complement-viz M)
  (run-viz (viz-state (if (eq? (sm-type M) 'dfa)
                          (list (create-graph-img M))
                          (let [(machine (ndfa->dfa M))]
                            (list (above (sm-graph machine)
                                         (text "MD: M converted to a dfa" 20 'black))
                                  (create-graph-img machine))))
                      (list (make-init-grph-img M))) draw-world process-key 'complement-viz))

(define no-one-el (make-dfa '(S A B C D E F G)
                            '(a b c)
                            'S
                            '(D E F)
                            '((S a A)
                              (S b B)
                              (S c C)
                              (A a A)
                              (A b D)
                              (A c F)
                              (B a D)
                              (B b B)
                              (B c E)
                              (C a F)
                              (C b E)
                              (C c C)
                              (D a D)
                              (D b D)
                              (D c G)
                              (E a G)
                              (E b E)
                              (E c E)
                              (F a F)
                              (F b G)
                              (F c F)
                              (G a G)
                              (G b G)
                              (G c G))
                            'no-dead))




