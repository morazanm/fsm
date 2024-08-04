;#lang fsm

(module viz-complement racket
  
  (require "../../fsm-gviz/interface.rkt"
           2htdp/image
           "../../fsm-core/private/fsa.rkt"
           "../../fsm-core/private/constants.rkt"
           "../../fsm-core/private/sm-getters.rkt"
           "../viz-lib/viz-constants.rkt"
           "../viz-lib/viz-state.rkt"
           "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
           "../viz-lib/viz-macros.rkt"
           "../viz-lib/default-viz-function-generators.rkt"
           "../viz-lib/viz.rkt"
           "../viz-lib/bounding-limits.rkt"
           "../viz-lib/viz-imgs/cursor.rkt"
           "../viz-lib/zipper.rkt")
  (provide complement-viz)

  (define FNAME "fsm")



(define E-SCENE-TOOLS
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

(define imsg-img (text "Starting ndfa" FONT-SIZE 'black))

(define RULE-YIELD-DIMS (bounding-limits 0
                                         (image-width imsg-img)
                                         E-SCENE-HEIGHT
                                         (+ E-SCENE-HEIGHT (image-height imsg-img))))

(define ARROW-UP-KEY-DIMS
  (bounding-limits (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Restart" (- FONT-SIZE 2) 'black)) (image-width ARROW-UP-KEY)) 2))
                   (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Restart" (- FONT-SIZE 2) 'black)) (image-width ARROW-UP-KEY)) 2)
                      (image-width ARROW-UP-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-UP-KEY))))

(define ARROW-RIGHT-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Forward" (- FONT-SIZE 2) 'black)) (image-width ARROW-RIGHT-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Forward" (- FONT-SIZE 2) 'black)) (image-width ARROW-RIGHT-KEY)) 2)
                      (image-width ARROW-RIGHT-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-RIGHT-KEY))))

(define ARROW-LEFT-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Backward" (- FONT-SIZE 2) 'black)) (image-width ARROW-LEFT-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Backward" (- FONT-SIZE 2) 'black)) (image-width ARROW-LEFT-KEY)) 2)
                      (image-width ARROW-LEFT-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-LEFT-KEY))))

(define ARROW-DOWN-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Finish" 18 'black)) (image-width ARROW-DOWN-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Finish" (- FONT-SIZE 2) 'black)) (image-width ARROW-DOWN-KEY)) 2)
                      (image-width ARROW-DOWN-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-DOWN-KEY))))

(define W-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom in" (- FONT-SIZE 2) 'black)) (image-width W-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom in" (- FONT-SIZE 2) 'black)) (image-width W-KEY)) 2)
                      (image-width W-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height W-KEY))))

(define S-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom out" (- FONT-SIZE 2) 'black)) (image-width S-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom out" (- FONT-SIZE 2) 'black)) (image-width S-KEY)) 2)
                      (image-width S-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height S-KEY))))

(define R-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Min zoom" (- FONT-SIZE 2) 'black)) (image-width R-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black)) (image-width R-KEY)) 2)
                      (image-width R-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height R-KEY))))

(define E-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Mid zoom" (- FONT-SIZE 2) 'black)) (image-width E-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black)) (image-width E-KEY)) 2)
                      (image-width E-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height E-KEY))))

(define F-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Max zoom" (- FONT-SIZE 2) 'black)) (image-width F-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black)) (image-width F-KEY)) 2)
                      (image-width F-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height F-KEY))))

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
      (graph-struct (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                            (sm-states M) 
                                                            (sm-start M)
                                                            new-finals)
                                           M)
                    (above  (text "Complement of MD" 20 'black)
                           (text (format "New final states: ~a" new-finals) 20 'black)
                           (text (format "Starting state: ~a" (sm-start M)) 20 'black)))))



     
  ;; make-init-grph-img
  ;; ndfa ndfa -> img
  ;; Purpose: To draw the graph of the initial ndfa's
  (define (make-init-grph-img M)
    (graph-struct
     (fsa->graph M 0)
     (if (eq? (sm-type M) 'dfa)
               (text "DM: input machine" 20 'black)
               (text "M: input machine" 20 'black))))

  ;; draw-imsg
  ;; imsg -> img
  (define (draw-imsg a-imsg) (zipper-current (graph-struct-inf a-imsg)))


(define (right-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy graph-struct
                                           a-graph-struct
                                           [inf (zipper-next
                                                  (graph-struct-inf
                                                   a-graph-struct)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define (left-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy graph-struct
                                           a-graph-struct
                                           [inf (zipper-prev
                                                  (graph-struct-inf
                                                   a-graph-struct)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define (up-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy graph-struct
                                           a-graph-struct
                                           [inf (zipper-to-begin
                                                  (graph-struct-inf
                                                   a-graph-struct)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define (down-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy graph-struct
                                           a-graph-struct
                                           [inf (zipper-to-end
                                                  (graph-struct-inf
                                                   a-graph-struct)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define viz-go-next (go-next))
(define viz-go-prev (go-prev))
(define viz-go-to-begin (go-to-begin))
(define viz-go-to-end (go-to-end))
(define viz-zoom-in (zoom-in))
(define viz-zoom-out (zoom-out))
(define viz-max-zoom-out (max-zoom-out))
(define viz-max-zoom-in (max-zoom-in))
(define viz-reset-zoom (reset-zoom))




  
  ;; complement-viz
  ;; fsa -> void
  (define (complement-viz M)
    (if (eq? (sm-type M) 'dfa)
        (run-viz (map graph-struct-grph (list (make-init-grph-img M) (create-graph-img M)))
                 (lambda () (graph->bitmap (graph-struct-grph (create-graph-img M))))
                 MIDDLE-E-SCENE
                 DEFAULT-ZOOM
                 DEFAULT-ZOOM-CAP
                 DEFAULT-ZOOM-FLOOR
                 (informative-messages draw-imsg
                                       (graph-struct (list->zipper (map (lambda (x) '()) (list (make-init-grph-img M) (create-graph-img M))))
                                                     (list->zipper (map (lambda (x) (graph-struct-inf x)) (list (make-init-grph-img M) (create-graph-img M))))
                                                     )
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
          (run-viz (list (graph-struct-grph (make-init-grph-img M)) (fsa->graph machine 0) (graph-struct-grph (create-graph-img machine)))
                   (lambda () (graph->bitmap (graph-struct-grph (make-init-grph-img M))))
                   MIDDLE-E-SCENE
                   DEFAULT-ZOOM
                   DEFAULT-ZOOM-CAP
                   DEFAULT-ZOOM-FLOOR
                   (informative-messages draw-imsg
                                         (graph-struct (list->zipper (map (lambda (x) '()) (list (make-init-grph-img M) (create-graph-img M))))
                                                       (list->zipper (list (graph-struct-inf (make-init-grph-img M))
                                                                           (text "Starting NDFA" FONT-SIZE 'black)
                                                                           (graph-struct-inf (create-graph-img machine))
                                                                           ))
                                                     
                                                     )
                                         
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

  (define no-one-el (make-unchecked-dfa '(S A B C D E F G)
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

(complement-viz aab*)

  )
#;(viz-state (if (eq? (sm-type M) 'dfa)
                 (list (create-graph-img M))
                 (let [(machine (ndfa->dfa M))]
                   (list (above (sm-graph machine)
                                (text "MD: M converted to a dfa" 20 'black))
                         (create-graph-img machine))))
             (list (make-init-grph-img M)))

    
