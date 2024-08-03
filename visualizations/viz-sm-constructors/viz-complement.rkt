;#lang fsm

(module viz-complement racket
  
  (require "../../fsm-gviz/private/lib.rkt"
           "../../fsm-gviz/interface.rkt"
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
           ;"../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
           "../viz-lib/viz-macros.rkt"
           "../viz-lib/default-viz-function-generators.rkt"
           "../viz-lib/viz.rkt"
           "../viz-lib/bounding-limits.rkt"
           "../../fsm-core/private/regexp.rkt"
           "../viz-lib/viz-imgs/cursor.rkt"
           "../viz-lib/zipper.rkt")
  (provide complement-viz)

  (define FNAME "fsm")

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
  (define a* (make-unchecked-ndfa '(S D)
                                  '(a b)
                                  'S
                                  '(S)
                                  '((S a S)
                                    (S b D)
                                    (D a D)
                                    (D b D))
                                  'no-dead))


  ;; COMPLEMENT VISUZALIZATION

  ;; graph-struct
  (struct graph-struct (grph inf))


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
      (graph-struct (list (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                            (sm-states M) 
                                                            (sm-start M)
                                                            new-finals)
                                           M))
                    (list  (text "Complement of MD" 20 'black)
                           (text (format "New final states: ~a" new-finals) 20 'black)
                           (text (format "Starting state: ~a" (sm-start M)) 20 'black)))))



     
  ;; make-init-grph-img
  ;; ndfa ndfa -> img
  ;; Purpose: To draw the graph of the initial ndfa's
  (define (make-init-grph-img M)
    (graph-struct
     (list (fsa->graph M))
     (list (if (eq? (sm-type M) 'dfa)
               (text "DM: input machine" 20 'black)
               (text "M: input machine" 20 'black)))))

  ;; draw-imsg
  ;; imsg -> img
  (define (draw-imsg a-imsg)
    (zipper-current (graph-struct-inf a-imsg)))
     

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

  ;; complement-viz
  ;; fsa -> void
  (define (complement-viz M)
    (if (eq? (sm-type M) 'dfa)
        (run-viz (list* (map graph-struct-grph (list (make-init-grph-img M) (create-graph-img M))))
                 (lambda () (graph->bitmap (create-graph-img M)))
                 MIDDLE-E-SCENE
                 DEFAULT-ZOOM
                 DEFAULT-ZOOM-CAP
                 DEFAULT-ZOOM-FLOOR
                 (informative-messages draw-imsg
                                       (graph-struct-inf (list->zipper (list (make-init-grph-img M) (create-graph-img M))))
                                       (bounding-limits 0 0 0 0)
                                       )
                 (instructions-graphic
                  E-SCENE-TOOLS
                  (bounding-limits 0 0 0 0))
                 (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
                 (create-viz-process-key (list (list "right" viz-go-next right-key-pressed)
                                               (list "left" viz-go-prev left-key-pressed)
                                               (list "up" viz-go-to-begin up-key-pressed)
                                               (list "down" viz-go-to-end down-key-pressed)
                                               (list "w" viz-zoom-in identity)
                                               (list "s" viz-zoom-out identity)
                                               (list "r" viz-max-zoom-out identity)
                                               (list "f" viz-max-zoom-in identity)
                                               (list "e" viz-reset-zoom identity)
                                               (list "wheel-down" viz-zoom-in identity)
                                               (list "wheel-up" viz-zoom-out identity)
                                               )
                                         )
                 (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                          CLICK-BUFFER-SECONDS
                                          (list)
                                          (list (list ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed)
                                                (list ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed)
                                                (list ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed)
                                                (list ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed)
                                                (list W-KEY-DIMS viz-zoom-in identity)
                                                (list S-KEY-DIMS viz-zoom-out identity)
                                                (list R-KEY-DIMS viz-max-zoom-out identity)
                                                (list E-KEY-DIMS viz-reset-zoom identity)
                                                (list F-KEY-DIMS viz-max-zoom-in identity)))
                 )
        (let [(machine (ndfa->dfa M))]
          (run-viz (list* (map graph-struct-grph (list (make-init-grph-img M) (fsa->graph machine) (create-graph-img machine))))
                   (lambda () (graph->bitmap (make-init-grph-img M)))
                   MIDDLE-E-SCENE
                   DEFAULT-ZOOM
                   DEFAULT-ZOOM-CAP
                   DEFAULT-ZOOM-FLOOR
                   (informative-messages draw-imsg
                                         (graph-struct-inf (list->zipper (list* (map graph-struct-inf (list (make-init-grph-img M) (fsa->graph machine) (create-graph-img machine))))))
                                         (bounding-limits 0 0 0 0)
                                         )
                   (instructions-graphic
                    E-SCENE-TOOLS
                    (bounding-limits 0 0 0 0))
                   (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
                   (create-viz-process-key (list (list "right" viz-go-next right-key-pressed)
                                                 (list "left" viz-go-prev left-key-pressed)
                                                 (list "up" viz-go-to-begin up-key-pressed)
                                                 (list "down" viz-go-to-end down-key-pressed)
                                                 (list "w" viz-zoom-in identity)
                                                 (list "s" viz-zoom-out identity)
                                                 (list "r" viz-max-zoom-out identity)
                                                 (list "f" viz-max-zoom-in identity)
                                                 (list "e" viz-reset-zoom identity)
                                                 (list "wheel-down" viz-zoom-in identity)
                                                 (list "wheel-up" viz-zoom-out identity)
                                                 )
                                           )
                   (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                            CLICK-BUFFER-SECONDS
                                            (list)
                                            (list (list ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed)
                                                  (list ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed)
                                                  (list ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed)
                                                  (list ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed)
                                                  (list W-KEY-DIMS viz-zoom-in identity)
                                                  (list S-KEY-DIMS viz-zoom-out identity)
                                                  (list R-KEY-DIMS viz-max-zoom-out identity)
                                                  (list E-KEY-DIMS viz-reset-zoom identity)
                                                  (list F-KEY-DIMS viz-max-zoom-in identity)))
                   ))))

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


  )
#;(viz-state (if (eq? (sm-type M) 'dfa)
                 (list (create-graph-img M))
                 (let [(machine (ndfa->dfa M))]
                   (list (above (sm-graph machine)
                                (text "MD: M converted to a dfa" 20 'black))
                         (create-graph-img machine))))
             (list (make-init-grph-img M)))

    
