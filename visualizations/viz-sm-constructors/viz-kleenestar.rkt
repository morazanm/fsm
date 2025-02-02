#lang racket/base
(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         racket/list
         racket/function)
(provide kleenestar-viz)

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

(define FNAME "fsm")

(define imsg-img
  (above (text "Kleenestar of the ndfa" FONT-SIZE 'black)
         (text (format "Generated starting state:") FONT-SIZE 'black)
         (text (format "Added edges:") FONT-SIZE 'black)))

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

;; KLEENESTAR VISUZALIZATION

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
(define (make-edge-graph graph M new-start)
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
         (cons (list new-start EMP (sm-start M))
               (append (sm-rules M) (map (λ (f) (list f EMP new-start)) (sm-finals M))))))

;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-init-edge-graph graph M new-start)
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

(struct imsg-state (phase start added-edges))

;; imsg-state -> image
(define (draw-imsg a-imsg-state)
  (overlay (if (= (imsg-state-phase a-imsg-state) 0)
               (text "Starting ndfa" FONT-SIZE 'black)
               (above (text "Kleenestar of the ndfa" FONT-SIZE 'black)
                      (text (format "Generated starting state: ~a" (imsg-state-start a-imsg-state))
                            FONT-SIZE
                            'black)
                      (text (format "Added edges: ~a" (imsg-state-added-edges a-imsg-state))
                            FONT-SIZE
                            'black)))
           (rectangle 1250 50 'solid 'white)))

;; create-graph-struct
;; ndfa ndfa -> graph
;; Purpose: To create a graph structure for the kleene star
(define (create-graph-struct M)
  (let* ([new-start (generate-symbol 'K (sm-states M))]
         [new-states (cons new-start (sm-states M))]
         [new-finals (cons new-start (sm-finals M))])
    (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                      new-states
                                      new-start
                                      new-finals)
                     M
                     new-start)))

;; make-init-grph-structure
;; ndfa -> dgraph
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-structure M)
  (let* ([graph (make-init-edge-graph
                 (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                  (sm-states M)
                                  (sm-start M)
                                  (sm-finals M))
                 M
                 (sm-start M))])

    graph))

;; ndfa -> graph
;; Creates the graph structure used to create the initially displayed graphic
(define (create-init-graph-struct M)
  (make-init-edge-graph (make-node-graph (create-graph 'dgraph
                                                       #:atb (hash 'rankdir "LR" 'font "Sans"))
                                         (sm-states M)
                                         (sm-start M)
                                         (sm-finals M))
                        M
                        (sm-start M)))

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

;;right-key-pressed
;;viz-state -> viz-state
;;Purpose: Steps the visualization forward once
(define (right-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy imsg-state
                   (informative-messages-component-state (viz-state-informative-messages a-vs))
                   [phase
                    (if (= (imsg-state-phase (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))
                           0)
                        1
                        (imsg-state-phase (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))]
                   [start
                    (imsg-state-start (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))]
                   [added-edges
                    (imsg-state-added-edges (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))])])]))

;;left-key-pressed
;;viz-state -> viz-state
;;Purpose: Steps the visualization backword once
(define (left-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy imsg-state
                   (informative-messages-component-state (viz-state-informative-messages a-vs))
                   [phase
                    (if (= (imsg-state-phase (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))
                           1)
                        0
                        (imsg-state-phase (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))]
                   [start
                    (imsg-state-start (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))]
                   [added-edges
                    (imsg-state-added-edges (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))])])]))
;;down-key-pressed
;;viz-state -> viz-state
;;Purpose: Finishes the visualization
(define (down-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy imsg-state
                   (informative-messages-component-state (viz-state-informative-messages a-vs))
                   [phase
                    (if (= (imsg-state-phase (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))
                           0)
                        1
                        (imsg-state-phase (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))]
                   [start
                    (imsg-state-start (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))]
                   [added-edges
                    (imsg-state-added-edges (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))])])]))

;;up-key-pressed
;;viz-state -> viz-state
;;Purpose: Restarts the visualization
(define (up-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy imsg-state
                   (informative-messages-component-state (viz-state-informative-messages a-vs))
                   [phase
                    (if (= (imsg-state-phase (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))
                           1)
                        0
                        (imsg-state-phase (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))]
                   [start
                    (imsg-state-start (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))]
                   [added-edges
                    (imsg-state-added-edges (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))])])]))

;;kleenestar-viz
;; fsa -> void
(define (kleenestar-viz M)
  (run-viz (list (create-init-graph-struct M) (create-graph-struct M))
           (lambda () (graph->bitmap (make-init-grph-structure M)))
           MIDDLE-E-SCENE
           DEFAULT-ZOOM
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           (informative-messages draw-imsg
                                 (let ([new-start (generate-symbol 'K (sm-states M))])
                                   (imsg-state 0
                                               new-start
                                               (list (list new-start EMP (sm-start M))
                                                     (map (λ (f) (list f EMP new-start))
                                                          (sm-finals M)))))
                                 RULE-YIELD-DIMS)
           (instructions-graphic E-SCENE-TOOLS
                                 (bounding-limits 0
                                                  (image-width imsg-img)
                                                  E-SCENE-HEIGHT
                                                  (+ E-SCENE-HEIGHT (image-height imsg-img))))
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
                                      [F-KEY-DIMS viz-max-zoom-in identity]))
           'kleenestar-viz))
