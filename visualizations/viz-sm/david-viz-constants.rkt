#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         2htdp/image
         math/matrix
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/misc.rkt"
         "david-imsg-state.rkt"
         "../../fsm-core/interface.rkt")

(provide (all-defined-out))

(define FNAME "fsm")

(define HELD-INV-COLOR 'chartreuse4)
(define BRKN-INV-COLOR 'red2)
(define DARKGOLDENROD2 (make-color 238 173 14))
(define GRAPHVIZ-CUTOFF-GOLD 'darkgoldenrod2)

(define DUMMY-RULE (list (list EMP EMP EMP) (list EMP EMP)))

(define E-SCENE-WIDTH 1250)
(define E-SCENE-HEIGHT 490)
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))
(define FONT-SIZE 20)
(define TAPE-SIZE 42)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define PERCENT-BORDER-GAP 0.9)
(define HEIGHT-BUFFER 20)
(define LETTER-KEY-WIDTH-BUFFER 13)
(define ARROW-KEY-WIDTH-BUFFER 20)
(define INS-TOOLS-BUFFER 30)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define NODE-SIZE 50)

(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR .6)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))

(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))

;; (listof symbol) natnum (listof (list natnum symbol)) -> image
;; Returns an image of a tape of symbols, capable of sliding when its start-index is varied
(define (make-tape-img tape start-index color-pair)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let ([letter-imgs
         (build-list
          TAPE-SIZE
          (λ (i)
            (if (< (+ start-index i) (length tape))
                (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                               20
                               (cond [(empty? color-pair) 'black]
                                     [(and (not (empty? (first color-pair)))
                                           (< (+ start-index i) (first (first color-pair))))
                                      (second (first color-pair))]
                                     [(and (not (empty? (second color-pair)))
                                           (= (+ start-index i) (first (second color-pair))))
                                      (second (second color-pair))]
                                     [else 'black]))
                         (overlay (square 21 'solid 'white) (square (add1 21) 'solid 'white)))
                (overlay (square 21 'solid 'white) (square (add1 21) 'solid 'white)))))])
    (make-tape-img letter-imgs start-index)))

(define TAPE-IMG-HEIGHT (image-height (make-tape-img (list 'a) 0 '())))

(define RULE-YIELD-DIMS
  (let ([DREV (let ([drev-text (text "Deriving: " FONT-SIZE 'black)])
                (overlay drev-text
                         (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white)))]
        [YIELD (let ([yield-text (text "Current Yield: " FONT-SIZE 'black)])
                 (overlay yield-text
                          (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white)))]
        [RULE-USED (text "The rule used: " FONT-SIZE 'black)])
    (bounding-limits (+ (image-width (rectangle 1 (* 2 FONT-SIZE) "solid" 'white))
                        (image-width (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                             (above/align "right" RULE-USED DREV YIELD))))
                     (* E-SCENE-WIDTH 0.9)
                     (+ E-SCENE-HEIGHT)
                     (+ E-SCENE-HEIGHT
                        (image-height (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                              (above/align "right" RULE-USED DREV YIELD)))))))

(define cursor
  (let ([cursor-rect (let ([inner-white (rectangle 5 17.5 'solid 'white)]
                           [outer-black (rectangle 9 20 'solid 'black)]
                           [white-triangle-infill (rectangle 9 5 'solid 'white)])
                       (above white-triangle-infill (overlay/xy inner-white -2 0 outer-black)))]
        [cursor-tri
         (let ([inner-white (overlay/align/offset
                             "right"
                             "middle"
                             (rotate 250
                                     (overlay/align/offset "middle"
                                                           "bottom"
                                                           (triangle/aas 30 30 44 'solid 'white)
                                                           0
                                                           3
                                                           (triangle/aas 30 30 48 'solid 'black)))
                             -2
                             -1
                             (triangle/aas 38.94 70.54 74 'solid 'white))]
               [outer-black (overlay/align/offset "right"
                                                  "middle"
                                                  (rotate 250 (triangle/aas 30 30 60 'solid 'white))
                                                  -1
                                                  -1
                                                  (triangle/sss 60 90 90 'solid 'black))])
           (scale 0.5 (rotate 310 (overlay/xy inner-white -9 -3 outer-black))))])
    (overlay/xy (rotate 25 cursor-rect) -7 -26 cursor-tri)))



(define E-SCENE-TOOLS
  (let ([ARROW (above (triangle 30 'solid 'black) (rectangle 10 30 'solid 'black))])
    (beside/align
     "bottom"
     (above ARROW-UP-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Restart" (- FONT-SIZE 2) 'black))
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
     (above cursor (square HEIGHT-BUFFER 'solid 'white) (text "Hold to drag" (- FONT-SIZE 2) 'black))
     (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
     (beside (above/align "middle"
                          W-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Zoom in" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          S-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Zoom out" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          R-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Min zoom" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          E-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Mid zoom" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          F-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Max zoom" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          A-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Word start" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          D-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Word end" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          J-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Prv not inv" (- FONT-SIZE 2) 'black))
             (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
             (above/align "middle"
                          L-KEY
                          (square HEIGHT-BUFFER 'solid 'white)
                          (text "Nxt not inv" (- FONT-SIZE 2) 'black))))))

(define ARROW-UP-KEY-DIMS
  (bounding-limits
   (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
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
  (bounding-limits
   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
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
  (bounding-limits
   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
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
  (bounding-limits
   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
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
  (bounding-limits
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
  (bounding-limits
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
  (bounding-limits
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
  (bounding-limits
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
  (bounding-limits
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

(define A-KEY-DIMS
  (bounding-limits
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Word start" (- FONT-SIZE 2) 'black)) (image-width A-KEY)) 2))
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Word start" (- FONT-SIZE 2) 'black)) (image-width A-KEY)) 2)
      (image-width A-KEY))
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER)
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER
      (image-height A-KEY))))

(define D-KEY-DIMS
  (bounding-limits
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word start" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Word end" (- FONT-SIZE 2) 'black)) (image-width D-KEY)) 2))
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word start" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Word end" (- FONT-SIZE 2) 'black)) (image-width D-KEY)) 2)
      (image-width D-KEY))
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER)
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER
      (image-height D-KEY))))

(define J-KEY-DIMS
  (bounding-limits
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word start" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word end" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Prv not Inv." (- FONT-SIZE 2) 'black)) (image-width J-KEY)) 2)
      )
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word start" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word end" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Prv not Inv." (- FONT-SIZE 2) 'black)) (image-width J-KEY)) 2)
      (image-width J-KEY))
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER)
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER
      (image-height J-KEY))))

(define L-KEY-DIMS
  (bounding-limits
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word start" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word end" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Prv not Inv." (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Next not Inv." (- FONT-SIZE 2) 'black)) (image-width L-KEY)) 2)
      )
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
      (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word start" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Word end" (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (image-width (text "Prv not Inv." (- FONT-SIZE 2) 'black))
      LETTER-KEY-WIDTH-BUFFER
      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
      (/ (- (image-width (text "Next not Inv." (- FONT-SIZE 2) 'black)) (image-width L-KEY)) 2)
      (image-width L-KEY))
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER)
   (+ EXTRA-HEIGHT-FROM-CURSOR
      E-SCENE-HEIGHT
      (bounding-limits-height RULE-YIELD-DIMS)
      INS-TOOLS-BUFFER
      (image-height L-KEY))))


(define jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP))
(define jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP))
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