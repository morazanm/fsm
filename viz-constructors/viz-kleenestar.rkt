#lang racket
(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../visualizations/viz-lib/resize-sm-image.rkt"
         ;"definitions-viz.rkt"
         ;"run-viz.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/sm-getters.rkt"
         "../fsm-core/private/misc.rkt"
         "../visualizations/viz-lib/viz-constants.rkt"
         "../visualizations/viz-lib/viz-state.rkt"
         "../visualizations/viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../visualizations/viz-lib/viz-macros.rkt"
         "../visualizations/viz-lib/default-viz-function-generators.rkt"
         "../visualizations/viz-lib/viz.rkt"
         "../visualizations/viz-lib/bounding-limits.rkt"
         )


;(define test (go-next a-vs))

(provide kleenestar-viz)

(define FNAME "fsm")



#;(define E-SCENE-TOOLS
  (let [(ARROW (above (triangle 30 'solid 'black) (rectangle 10 30 'solid 'black)))]
    (beside/align "bottom"
                  (above ARROW-UP-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Restart" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above ARROW-RIGHT-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Forward" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above ARROW-LEFT-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Backward" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above ARROW-DOWN-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Finish" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above cursor
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Hold to drag" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (beside (above/align "middle" W-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Zoom in" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle"  S-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Zoom out" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle" R-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Min zoom" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle" E-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Mid zoom" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle" F-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Max zoom" (- FONT-SIZE 2) 'black))
                          )
                  )
    )
  )

;; L = nl
(define nl (make-unchecked-ndfa '(S)
                      '(a b)
                      'S
                      '()
                      '()))

;; L = ab*
(define ab* (make-unchecked-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-unchecked-ndfa '(Z H B C D F)
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
(define aab* (make-unchecked-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))
;; L = a*
(define a* (make-unchecked-dfa '(S D)
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



(define (create-graph-struct M)
   (let* [(new-start (generate-symbol 'K (sm-states M)))
         (new-states (cons new-start (sm-states M)))
         (new-finals (cons new-start (sm-finals M)))]
     (make-edge-graph (make-node-graph
                       (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                       new-states
                       new-start
                       new-finals)
                      M new-start)
     )
  )

(define (create-init-graph-struct M)
  (make-init-edge-graph (make-node-graph
                         (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                         (sm-states M)
                         (sm-start M)
                         (sm-finals M))
                        M (sm-start M))
  )

(struct imsg-state (start added-edges))

;; imsg-state -> image
(define (draw-imsg a-imsg-state)
  (above (text "Kleenestar of the ndfa \n" 20 'black)
         (text (format "Generated starting state: ~a \n" (imsg-state-start a-imsg-state)) 20 'black)
         (text (format "Added edges: ~a \n" (imsg-state-added-edges a-imsg-state)) 20 'black)))

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
                                 (sm-finals M))))]
    (overlay (above (void)
                    (text "Kleenestar of the ndfa \n" 20 'black)
                    (text (format "Generated starting state: ~a \n" new-start) 20 'black)
                    (text (format "Added edges: ~a \n" added-edges) 20 'black))
             E-SCENE)
    )
  )
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M)
  (let* [(graph (graph->bitmap (make-init-edge-graph (make-node-graph
                                                      (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                      (sm-states M)
                                                      (sm-start M)
                                                      (sm-finals M))
                                                     M (sm-start M))))
         (width (image-width graph))
         (height (image-height graph))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (above
         (resize-sm-image graph (image-width E-SCENE) (image-height E-SCENE))
         (text "Starting ndfa \n" 20 'black))
        (above
         graph
         (text "Starting ndfa \n" 20 'black))

        )))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
#;(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pimgs a-vs))))
        (height (image-height (first (viz-state-pimgs a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (above (overlay (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE)) E-SCENE) E-SCENE-TOOLS)
        (above (overlay (first (viz-state-pimgs a-vs)) E-SCENE) E-SCENE-TOOLS))))

(define (viz-go-next go-next)

;;kleenestar-viz
;; fsa -> void
(define (kleenestar-viz M)
  (run-viz (list (create-init-graph-struct M) (create-graph-struct M))
           (make-init-grph-img M)
           MIDDLE-E-SCENE
           DEFAULT-ZOOM
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           (informative-messages draw-imsg
                                 (let ([new-start (generate-symbol 'K (sm-states M))])
                                       (imsg-state new-start
                                             (list (list new-start EMP (sm-start M))
                                                   (map (λ (f) (list f EMP new-start))
                                                        (sm-finals M)))
                                             )
                                       )
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
           )
  )
  #;(run-viz (viz-state (list (create-graph-img M))
                      (list (make-init-grph-img M)))
           draw-world
           'kleenestar-viz)
