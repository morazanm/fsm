#lang racket

(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "definitions-viz.rkt"
         "run-viz.rkt"
         "../fsm-core/interface.rkt")
(provide concat-viz)

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

;; CONCATENATION VISUALIZATION



(define E-SCENE (empty-scene 1250 600))


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
(define (make-edge-graph graph M N)
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
                                                             [(and (eq? (second rule) EMP)
                                                                   (and (not (member rule (sm-rules M)))
                                                                        (not (member rule (sm-rules N)))))
                                                              'black]
                                                             [else
                                                              'orange]))))
         graph
         (append (sm-rules M) (sm-rules N) (map (λ (f) (list f EMP (sm-start N)))
                                                (sm-finals M)))))

;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-first-edge-graph graph M new-start)
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
                                                              'orange]
                                                             [else 'black]))))
         graph
         (sm-rules M)))

;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-second-edge-graph graph M new-start)
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
(define (create-graph-img M N)
  (let* [(new-start (sm-start M))
         (edge-added (map (λ (f) (list f EMP (sm-start N)))
                          (sm-finals M)))
         (new-states (append (sm-states M) (sm-states N)))
         (new-finals (sm-finals N))]
    (overlay (above (graph->bitmap (make-edge-graph (make-node-graph
                                                     (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                     new-states new-start new-finals) M N))
                    (text "Concatenation of the ndfas \n" 20 'black)
                    (text (format "Starting state: ~a \n" new-start) 20 'black)
                    (text (format "Final state(s): ~a \n" new-finals) 20 'black)
                    (text (format "Generated edge: ~a \n" edge-added) 20 'black))
             E-SCENE)))
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M N)
  (overlay (above (text "First ndfa:" 20 'black)
                  (graph->bitmap (make-second-edge-graph (make-node-graph
                                                          (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                          (sm-states M)
                                                          (sm-start M)
                                                          (sm-finals M))
                                                         M (sm-start M)))
                  (text "Second ndfa:" 20 'black)
                  (graph->bitmap (make-first-edge-graph (make-node-graph
                                                         (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                         (sm-states N)
                                                         (sm-start N)
                                                         (sm-finals N))
                                                        N (sm-start N))))
           E-SCENE))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pimgs a-vs))))
        (height (image-height (first (viz-state-pimgs a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE))
        (first (viz-state-pimgs a-vs)))))

;; concat-viz
;; fsa fsa -> void
(define (concat-viz M N)
  (let [(renamed-machine (if (ormap (λ (x) (member x (sm-states M))) (sm-states N))
                             (sm-rename-states (sm-states M) N)
                             N))]
    (run-viz (viz-state (list (create-graph-img M renamed-machine)) (list (make-init-grph-img M N))) draw-world 'concat-viz)))

