#lang racket
(require "../../fsm-gviz/interface.rkt"
         2htdp/image
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt")

(provide intersection-viz)

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


;; imsg-img
;; Informative message image
(define imsg-img (above (text "N is an ndfa and needs to be converted to a dfa" 20 'black)
                        (text "M is an ndfa and needs to be converted to a dfa" 20 'black)))

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

;; L = ab*
(define ab*-deter (make-unchecked-ndfa '(S A)
                                       '(a b)
                                       'S
                                       '(A)
                                       '((S a A)
                                         (A b A))))

;; INTERSECTION VISUALIZATION


;; graph-struct
;; grph - graph structure
;; inf - informative message
(struct graph-struct (grph inf))


;; create-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (create-node-graph graph M)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state (sm-start M)) 'darkgreen]
                                     [else 'black])
                        'shape (if (member state (sm-finals M))
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         (sm-states M)))

;; create-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph graph M)
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
                                                )))
         graph
         (sm-rules M)))

;; create-edge-graph-n
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-n graph N)
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
                                                'color (if (member rule (sm-rules N))
                                                           'violet
                                                           'black)
                                                )))
         graph
         (sm-rules N)))

;; create-edge-graph-m
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-m graph M)
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
                                                'color (if (member rule (sm-rules M))
                                                           'orange
                                                           'black)
                                                )))
         graph
         (sm-rules M)))

;; create-edge-graph-union
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-union graph union M N)
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
                                                             [(member rule (sm-rules N))
                                                              'violet]
                                                             [else
                                                              'black]))))
         graph
         (sm-rules union)))


;; create-graph-structures
;; ndfa ndfa -> graph
;; Purpose: To create graph structures for the intersection
(define (create-graph-structures M N)
  (define (complement-fsa ndfa)
    (let* [(new-finals (filter (λ (s) (not (member s (sm-finals ndfa)))) (sm-states ndfa)))]
      (make-unchecked-dfa (sm-states ndfa)
                          (sm-sigma ndfa)
                          (sm-start ndfa)
                          new-finals (sm-rules ndfa) 'no-dead)))
  (let* [(Mdfa (ndfa->dfa M))
         (dfaM (graph-struct (create-edge-graph-m
                              (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                 Mdfa)
                              Mdfa)
                             (text "M-dfa: M converted to a dfa" 20 'black)))
         (Ndfa (ndfa->dfa N))
         (dfaN (graph-struct (create-edge-graph-n
                              (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                 Ndfa)
                              Ndfa)
                             (text "N-dfa: N converted to a dfa" 20 'black)))
         (Mcomplement (complement-fsa (ndfa->dfa M)))
         (cmplM (graph-struct (create-edge-graph-m
                               (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                  Mcomplement)
                               Mcomplement)
                              (text "CM-dfa: Complement of M-dfa" 20 'black)))
         (Ncomplement (complement-fsa (ndfa->dfa N)))
         (cmplN (graph-struct (create-edge-graph-n
                               (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                  Ncomplement)
                               Ncomplement)
                              (text "CN-dfa: Complement of N-dfa" 20 'black)))
         (nM (rename-states-fsa (list DEAD) (complement-fsa (ndfa->dfa M))))
         (nN (rename-states-fsa (sm-states nM) (complement-fsa (ndfa->dfa N))))
         (notM (create-edge-graph-m
                (create-node-graph
                 (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                 nM)
                nM))
         (notN (create-edge-graph-n (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                       nN)
                                    nN))
         (notMimg (graph-struct  notM (text "CM-dfa States Renamed" 20 'black)))
         (notNimg (graph-struct notN (text "CN-dfa States Renamed" 20 'black)))
         (notM-U-notN (union-fsa nM nN))
         (ndfa-un (ndfa->dfa notM-U-notN))
         (comp-un (complement-fsa ndfa-un))
         (unionMN (graph-struct(create-edge-graph-union (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                           notM-U-notN)
                                                        notM-U-notN nM nN)
                               (text "U-CM-dfa-CN-dfa: Union of CM-dfa and CN-dfa" 20 'black)))
         (dfaMN (graph-struct (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                    ndfa-un)
                                                 ndfa-un)
                              (text "DU-CM-dfa-CN-dfa: U-CM-dfa-CN-dfa converted to a dfa" 20 'black)))
         (final-graph (graph-struct (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                          comp-un)
                                                       comp-un)
                                    (text "Intersection of M and N: Complement of DU-CM-dfa-CN-dfa" 20 'black)))]
    (cond [(and (eq? (sm-type M) 'dfa)
                (eq? (sm-type N) 'ndfa))
           (list dfaN cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
          [(and (eq? (sm-type N) 'dfa)
                (eq? (sm-type M) 'ndfa))
           (list dfaM cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
          [(and (eq? (sm-type M) 'ndfa)
                (eq? (sm-type N) 'ndfa))
           (list dfaM dfaN cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
          [(and (eq? (sm-type M) 'dfa)
                (eq? (sm-type N) 'dfa))
           (list cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)])))
          
    
                    
     
;; make-init-grph-structure
;; ndfa ndfa -> graph
;; Purpose: To create the graph structure of the initial ndfa's
(define (make-init-grph-structure M N)
  (graph-struct (list
                 (create-edge-graph-m
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     M)
                  M)
                 (create-edge-graph-n
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     N)
                  N))
                (above (text (if (eq? (sm-type M) 'ndfa)
                                "M is an ndfa and needs to be converted to a dfa"
                                "M is a dfa") 20 'black)
                      (text (if (eq? (sm-type N) 'ndfa)
                                "N is an ndfa and needs to be converted to a dfa"
                                "N is a dfa") 20 'black))
                ))

;; draw-imsg
;; imsg -> img
;; Purpose: To draw informative messages
(define (draw-imsg a-imsg)
  (zipper-current (graph-struct-inf a-imsg)))
     

(define viz-go-next (go-next E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-prev (go-prev E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-begin (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-end (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))


(define viz-zoom-in (zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-zoom-out (zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-out (max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-in (max-zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-reset-zoom (reset-zoom E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))


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

;; intersection-viz
;; fsa fsa -> void
(define (intersection-viz M N)
  (let* [(imgs (create-graph-structures M N))]
    (run-viz (map graph-struct-grph (cons (make-init-grph-structure M N) (create-graph-structures M N)))
             (lambda () (apply above (map graph->bitmap (graph-struct-grph (make-init-grph-structure M N)))))
             MIDDLE-E-SCENE
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (let ([new-start (generate-symbol 'K (sm-states M))])
                                     (graph-struct (list->zipper (map (lambda (x) '()) (cons (make-init-grph-structure M N) (create-graph-structures M N))))
                                                   (list->zipper (map graph-struct-inf (cons (make-init-grph-structure M N) (create-graph-structures M N))))
                                      ))
                                   RULE-YIELD-DIMS)
             (instructions-graphic
              E-SCENE-TOOLS
              (bounding-limits 0
                               (image-width imsg-img)
                               E-SCENE-HEIGHT
                               (+ E-SCENE-HEIGHT (image-height imsg-img))))
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
             'intersection-viz)))



(intersection-viz ab* a-aUb-b*)

