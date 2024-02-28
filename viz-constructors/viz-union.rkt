#lang racket
(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "definitions-viz.rkt"
         "run-viz.rkt"
         "../fsm-core/interface.rkt")

(provide union-viz)

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

;; UNION VISUALIZATION

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
(define (make-edge-graph graph M N ns)
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
         (append (list (list ns EMP (sm-start M))
                       (list ns EMP (sm-start N)))
                 (sm-rules M)
                 (sm-rules N))))

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
  (let* [(new-start (generate-symbol 'S (append (sm-states M) (sm-states N))))
         (new-states (cons new-start
                           (append (sm-states M) (sm-states N))))
         (added-edges (list (list new-start EMP (sm-start M))
                            (list new-start EMP (sm-start N))))
         (new-finals (append (sm-finals M) (sm-finals N)))
         (graph (graph->bitmap (make-edge-graph (make-node-graph
                                                 (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                 new-states new-start new-finals) M N new-start)))
         (width (image-width graph))
         (height (image-height graph))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (above (resize-image graph
                             (image-width E-SCENE) (image-height E-SCENE))
               (text "Union of the ndfas \n" 20 'black)
               (text (format "Generated edges: ~a \n" added-edges) 20 'black)
               (text (format "Final states: ~a \n" new-finals) 20 'black)
               (text (format "Starting state: ~a \n" new-start) 20 'black))
        (above graph
               (text "Union of the ndfas \n" 20 'black)
               (text (format "Generated edges: ~a \n" added-edges) 20 'black)
               (text (format "Final states: ~a \n" new-finals) 20 'black)
               (text (format "Starting state: ~a \n" new-start) 20 'black)))))
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M N)
  (let* [(graph1 (graph->bitmap (make-first-edge-graph (make-node-graph
                                                        (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                        (sm-states N)
                                                        (sm-start N)
                                                        (sm-finals N))
                                                       N (sm-start N))))
         (graph2 (graph->bitmap (make-second-edge-graph (make-node-graph
                                                         (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                         (sm-states M)
                                                         (sm-start M)
                                                         (sm-finals M))
                                                        M (sm-start M))))
         (width1 (image-width graph1))
         (height1 (image-height graph1))
         (width2 (image-width graph2))
         (height2 (image-height graph2))]
    (overlay (beside (beside (text "First ndfa:" 20 'black)
                             (square 20 'solid 'white)
                             (if (or (> width1 (image-width E-SCENE))
                                     (> height1 (image-height E-SCENE)))
                                 (resize-image graph1 (image-width E-SCENE) (image-height E-SCENE))
                                 graph1)
                             )
                  
                     (square 50 'solid 'white)
                     (beside (text "Second ndfa:" 20 'black)
                             (square 20 'solid 'white)
                             (if (or (> width2 (image-width E-SCENE))
                                     (> height2 (image-height E-SCENE)))
                                 (resize-image graph2 (image-width E-SCENE) (image-height E-SCENE))
                                 graph2))
                     )
             E-SCENE)))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pimgs a-vs))))
        (height (image-height (first (viz-state-pimgs a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (above (overlay (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE)) E-SCENE) E-SCENE-TOOLS)               
        (above (overlay (first (viz-state-pimgs a-vs)) E-SCENE) E-SCENE-TOOLS))))

;; union-viz
;; fsa fsa -> void
(define (union-viz M N)
  (let [(renamed-machine (if (ormap (λ (x) (member x (sm-states M))) (sm-states N))
                             (sm-rename-states (sm-states M) N)
                             N))]
    (run-viz (viz-state (list (create-graph-img M renamed-machine)) (list (make-init-grph-img M N))) draw-world 'union-viz)))













