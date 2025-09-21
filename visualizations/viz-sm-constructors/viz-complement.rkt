#lang racket/base

(require "../../fsm-gviz/interface.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt"
         "../../sm-graph.rkt"
         racket/list
         racket/function)
(provide complement-viz)

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

(define imsg-img
  (above (text "Complement of Deterministic M" 20 'black)
         (text (format "New final states:") 20 'black)
         (text (format "Starting state:") 20 'black)))

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
(define a* (make-unchecked-ndfa '(S D) '(a b) 'S '(S) '((S a S) (S b D) (D a D) (D b D)) 'no-dead))

;; COMPLEMENT VISUZALIZATION

;; graph-struct
;; grph - graph structure
;; inf - informative message
(struct graph-struct (grph inf))

;; make-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph los s f)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color
                                 (cond
                                   [(and (eq? state s) (member state f)) 'violet]
                                   [(eq? state s) 'darkgreen]
                                   [(member state f) 'violet]
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
(define (make-edge-graph graph M)
  (let* ([new-finals (filter (λ (s) (not (member s (sm-finals (ndfa->dfa M)))))
                             (sm-states (ndfa->dfa M)))])
    (foldl (λ (rule result)
             (add-edge result
                       (second rule)
                       (if (equal? (first rule) '()) 'ds (first rule))
                       (if (equal? (third rule) '()) 'ds (third rule))
                       #:atb (hash 'fontsize 20 'style 'solid)))
           graph
           (sm-rules (ndfa->dfa M)))))

;; create-graph-structure
;; ndfa -> graph
;; Purpose: To create a graph structures for complement
(define (create-graph-structure M)
  (let* ([new-finals (filter (λ (s) (not (member s (sm-finals M)))) (sm-states M))])
    (graph-struct (make-edge-graph
                   (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                    (sm-states M)
                                    (sm-start M)
                                    new-finals)
                   M)
                  (above (text "Complement of Deterministic M" 20 'black)
                         ;(text "Complement of MD" 20 'black)
                         (text (format "New final states: ~a" new-finals) 20 'black)
                         (text (format "Starting state: ~a" (sm-start M)) 20 'black)))))

;; make-init-grph-structure
;; ndfa ndfa -> graph
;; Purpose: To make the graph structure of the initial ndfa's
(define (make-init-grph-structure M)
  (graph-struct (fsa->graph M 0)
                (if (eq? (sm-type M) 'dfa)
                    (text "DM: input machine" 20 'black)
                    (text "M: input machine" 20 'black))))

;; draw-imsg
;; imsg -> img
(define (draw-imsg a-imsg)
  (zipper-current (graph-struct-inf a-imsg)))

;; viz-state -> viz-state
;; Updates the informative messages to the next stage of the seqeuence
(define (right-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                         (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-next (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the previous stage of the seqeuence
(define (left-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                         (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-prev (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the beginning of the seqeuence
(define (up-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                         (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy viz-state
                     a-vs
                     [informative-messages
                      (struct-copy informative-messages
                                   (viz-state-informative-messages a-vs)
                                   [component-state
                                    (struct-copy graph-struct
                                                 a-graph-struct
                                                 [inf
                                                  (zipper-to-begin (graph-struct-inf
                                                                    a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the end of the seqeuence
(define (down-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                         (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-to-end (graph-struct-inf a-graph-struct))])])]))))

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

;; complement-viz
;; fsa -> void
(define (complement-viz M)
  (if (eq? (sm-type M) 'dfa)
      (run-viz
       (map graph-struct-grph (list (make-init-grph-structure M) (create-graph-structure M)))
       (lambda () (graph->bitmap (graph-struct-grph (make-init-grph-structure M))))
       MIDDLE-E-SCENE
       E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP
       DEFAULT-ZOOM
       DEFAULT-ZOOM-CAP
       DEFAULT-ZOOM-FLOOR
       (informative-messages draw-imsg
                             (graph-struct (list->zipper (map (lambda (x) '())
                                                              (list (make-init-grph-structure M)
                                                                    (create-graph-structure M))))
                                           (list->zipper (map (lambda (x) (graph-struct-inf x))
                                                              (list (make-init-grph-structure M)
                                                                    (create-graph-structure M)))))
                             (bounding-limits 0 0 0 0))
       (instructions-graphic E-SCENE-TOOLS RULE-YIELD-DIMS)
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
       'complement-viz)

      (let ([machine (ndfa->dfa M)])
        (run-viz
         (list (graph-struct-grph (make-init-grph-structure M))
               (fsa->graph machine 0)
               (graph-struct-grph (create-graph-structure machine)))
         (lambda () (graph->bitmap (graph-struct-grph (make-init-grph-structure M))))
         MIDDLE-E-SCENE
         DEFAULT-ZOOM
         DEFAULT-ZOOM-CAP
         DEFAULT-ZOOM-FLOOR
         (informative-messages
          draw-imsg
          (graph-struct (list->zipper (map (lambda (x) '())
                                           (list (make-init-grph-structure M)
                                                 (create-graph-structure M))))
                        (list->zipper (list (graph-struct-inf (make-init-grph-structure M))
                                            (text "Deterministic M" FONT-SIZE 'black)
                                            (graph-struct-inf (create-graph-structure machine)))))
          (bounding-limits 0 0 0 0))
         (instructions-graphic E-SCENE-TOOLS
                               (bounding-limits 0
                                                (image-width E-SCENE-TOOLS)
                                                (+ EXTRA-HEIGHT-FROM-CURSOR
                                                   E-SCENE-HEIGHT
                                                   (bounding-limits-height RULE-YIELD-DIMS)
                                                   INS-TOOLS-BUFFER)
                                                (+ EXTRA-HEIGHT-FROM-CURSOR
                                                   E-SCENE-HEIGHT
                                                   (bounding-limits-height RULE-YIELD-DIMS)
                                                   INS-TOOLS-BUFFER
                                                   (image-height ARROW-UP-KEY))))
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
         'complement-viz))))

(define no-one-el
  (make-unchecked-dfa '(S A B C D E F G)
                      '(a b c)
                      'S
                      '(D E F)
                      '((S a A) (S b B)
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
