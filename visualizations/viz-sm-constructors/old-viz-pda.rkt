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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
A rule is a structure:
(make-rule triple pair)
triple is the first of the pda rule
pair is the second of the pda rule
|#
(struct rule (triple pair) #:transparent)

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rule is a rule-struct
|#
(struct trace (config rule) #:transparent)

#|
A computation is a structure: (make-computation LoC LoR LoT visited)
LoC is a (listof configuration)
LoR is a (listof rule)
visited is a (listof configuration)
|#
(struct computation (LoC LoR visited) #:transparent)

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func)
  (ormap (λ (L) (eq-func x L)) lst))

;;(listof configurations) -> (listof configurations)
;;Purpose: filters the given list of any empty transitions
(define (remove-empty a-LoC acc)
  (cond [(< (length a-LoC) 2) (reverse (append a-LoC acc))]
        [(and (equal? (second (first a-LoC)) (second (second a-LoC)))
              (equal? (third (first a-LoC)) (third (second a-LoC))))
         (remove-empty (rest a-LoC) acc)]
        [else (remove-empty (rest a-LoC) (cons (first a-LoC) acc))]))

;;rule -> boolean
;;Purpose: Determines if the given rule is an empty rule (e.i. reads, pops, and pushes empty)
(define (empty-rule? a-rule)
  (and (equal? (second (first a-rule)) EMP)
       (equal? (third (first a-rule)) EMP)
       (equal? (second (second a-rule)) EMP)))

;;config rule -> config
;;Purpose: Applys the given rule to the given config and returns the updated config
;;ASSUMPTION: The given rule can be applied to the config
(define (apply-rule a-comp a-rule)
  ;;config -> config
  ;;Purpose: Applies the read portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-read a-config)
    (if (equal? (second (first a-rule)) EMP)
        (list (first (second a-rule)) (second a-config) (third a-config))
        (list (first (second a-rule)) (rest (second a-config)) (third a-config))))
  ;;config -> config
  ;;Purpose: Applies the pop portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-pop a-config)
    (if (equal? (third (first a-rule)) EMP)
        a-config
        (list (first a-config) (second a-config)
              (drop (third a-config) (length (third (first a-rule)))))))
  ;;config -> config
  ;;Purpose: Applies the push portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-push a-config)
    (if (equal? (second (second a-rule)) EMP)
        a-config
        (list (first a-config) (second a-config)
              (append (second (second a-rule)) (third a-config)))))
  (struct-copy computation a-comp
               [LoC (cons (apply-push (apply-pop (apply-read (first (computation-LoC a-comp)))))
                          (computation-LoC a-comp))]
               [LoR (cons a-rule (computation-LoR a-comp))]
               [visited (cons (first (computation-LoC a-comp)) (computation-visited a-comp))]))
       
(define qempty? empty?)

(define E-QUEUE '())

;; (qof X) → X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; (listof X) (qof X) → (qof X)
;; Purpose: Add the given list of X to the given
;;          queue of X
(define (enqueue a-lox a-qox)
  (append a-qox a-lox))

;; (qof X) → (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))

;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start max-cmps)
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (list (append (list start) (list a-word) (list '())))
                                           '()
                                           '())])
    (make-computations lor
                       (enqueue (list starting-computation) E-QUEUE)
                       '()
                       max-cmps)))


;;(listof rules) (queueof computation) (listof computation) number -> (listof computation)
;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
;;     that are within the bounds of the max computation limit
(define (make-computations lor QoC path max-cmps)
  (cond [(qempty? QoC) path]
        [(> (length (computation-LoC (qfirst QoC))) max-cmps)
         (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)]
        [else (let* ([stack (third (first (computation-LoC (qfirst QoC))))]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume a first letter in the given configurations
                     [connected-read-rules (filter (λ (rule)
                                                     (and (not (empty? (second (first (computation-LoC (qfirst QoC))))))
                                                          (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                          (equal? (second (first rule)) (first (second (first (computation-LoC (qfirst QoC))))))))
                                                   lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume no input for the given configurations
                     [connected-read-E-rules (filter (λ (rule)
                                                       (and (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                            (equal? (second (first rule)) EMP)))
                                                     lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that can pop what is in the stack
                     [connected-pop-rules (filter (λ (rule)
                                                    (or (equal? (third (first rule)) EMP)
                                                        (and (>= (length stack) (length (third (first rule))))
                                                             (equal? (take stack (length (third (first rule)))) (third (first rule))))))
                                                  (append connected-read-E-rules connected-read-rules))]
                     [new-configs (filter (λ (new-c) 
                                            (not (member? (first (computation-LoC new-c)) (computation-visited new-c) equal?)))
                                          (map (λ (rule) (apply-rule (qfirst QoC) rule)) connected-pop-rules))])
                (if (empty? new-configs)
                    (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)
                    (make-computations lor (enqueue new-configs (dequeue QoC)) path max-cmps)))]))

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(empty? rules) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first (first rules))) EMP)))
         (let* ([rle (rule (list EMP EMP EMP) (list EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-trace(rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (empty-rule? (first rules)))
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (struct-copy trace (first acc)
                                  [rule (cons rle (trace-rule (first acc)))])])
           (make-trace (rest configs) (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) (list rle))])
                (make-trace (rest configs) (rest rules) (cons res acc)))]))

;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word configs)
  (append-map (λ (comp)
                (make-inv-configs-helper a-word (reverse (computation-LoC comp)) (length a-word)))
              configs))

;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs-helper a-word configs word-len)
  (let* ([config (filter (λ (config) (= (length (second config)) word-len)) configs)]
         [inv-config (map (λ (config)
                            (append (list (first config))
                                    (list (take a-word (- (length a-word) word-len)))
                                    (list (third config))))
                          config)])
    (if (empty? configs)
        '()
        (append inv-config
                (make-inv-configs-helper a-word (rest configs) (sub1 word-len))))))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
(define (get-inv-config-results inv-configs invs)
  (append-map (λ (comp)
                (get-inv-config-results-helper comp invs))
              inv-configs))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
(define (get-inv-config-results-helper inv-configs invs)
  (if (empty? inv-configs)
      '()
      (let* ([get-inv-for-inv-config (filter (λ (inv)
                                               (equal? (first inv) (first inv-configs)))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (append inv-configs
                                                  (list (inv-for-inv-config (second inv-configs)
                                                                            (third inv-configs))))))])
        (append inv-config-result
                (get-inv-config-results-helper (rest inv-configs) invs)))))

;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results a-word)
  (if (empty? inv-config-results)
      '()
      (return-brk-inv-configs-helper inv-config-results a-word (length a-word) '())))

;;(listof configurations) (listof sybmols) natnum (listof configurations) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
;;Acc = all invariants that fail when a given portion of the word has been consumed
(define (return-brk-inv-configs-helper inv-config-results a-word word-len acc)
  (if (< word-len 0)
      (filter (λ (res) (not (empty? res))) acc) ;;might remove if not can index using the length of the wht was processed
      (let* ([new-acc (filter (λ (config)
                                (and (equal? (second config) (take a-word (- (length a-word) word-len)))
                                     (not (fourth config))))
                              inv-config-results)])
        (return-brk-inv-configs-helper inv-config-results a-word (sub1 word-len) (append acc (list new-acc))))))

;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M max-cmps)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (second (first config))))
                     (map computation-LoC (get-computations a-word
                                                            (sm-rules M)
                                                            (sm-start M)
                                                            max-cmps))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M max-cmps)]
        [a-word]))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(empty? curr-path) prev-path]
        [(equal? (first prev-path) (first curr-path))
         (remove-similarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similarities (rest prev-path) (rest curr-path) (append acc (list (first curr-path))))]))

;;(listof rules) -> (listof rules)
;;Purpose: Converts the given (listof configurations)s to rules
(define (configs->rules curr-config)
  (make-rule-triples (remove-duplicates curr-config)))

;;word (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Counts the number of unique configurations for each stage of the word
(define (count-computations a-word a-LoC acc)
  ;;word -> number
  ;;Purpose: Counts the number of unique configurations based on the given word
  (define (get-config a-word)
    (length (remove-duplicates
             (append-map (λ (configs)
                           (filter (λ (config)
                                     (equal? a-word (second config)))
                                   configs))
                         a-LoC))))
  (if (empty? a-word)
      (reverse (cons (get-config a-word) acc))
      (count-computations (rest a-word) a-LoC (cons (get-config a-word) acc))))

;;(listof rule-struct) -> (listof rule)
;;Purpose: Remakes the rules extracted from the rule-struct
(define (remake-rules trace-rules)
  (append-map (λ (lor)
                (map (λ (rule)
                       (list (rule-triple rule)
                             (rule-pair rule)))
                     lor))
              trace-rules))

;;rule symbol (listof rules) -> boolean
;;Purpose: Determines if the given rule is a member of the given (listof rules)
;;         or similiar to one of the rules in the given (listof rules) 
(define (find-rule? rule dead lor)
  (or (member? rule lor equal?)
      (ormap (λ (p)
               (and (equal? (first rule) (first p))
                    (or (equal? (third rule) (third p))
                        (and (equal? (third rule) (third p))
                             (equal? (third rule) dead)))))
             lor)))

;;X -> X
;;Purpose: Returns X
(define (id x) x)

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (make-edge-label rule)
  (format "\n[~a ~a ~a]" (second (first rule)) (third (first rule)) (second (second rule))))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  (map (λ (rule)
         (append (list (first (first rule)))
                 (list (string->symbol (make-edge-label rule)))
                 (list (first (second rule)))))
       rules))

;;(listof trace) (X -> Y) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-X LoT map-func)
  (filter-map-acc empty? map-func not first LoT))

;;(listof symbol ((listof symbols) (listof symbols) -> boolean))) (X -> Y) -> (listof symbol ((listof symbols) (listof symbols) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) (listof symbols) -> boolean)))
(define (get-invariants LoI func)
  (filter-map-acc (λ (x) ((second (first x)) (second x) (third x))) first func first LoI))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not id LoT))

;;(listof symbols) (listof configurations) -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                              (equal? (second configs) word))
                        (computation-LoC config)))
              full-configs))

(define (get-cut-off LoC cmps)
  (filter-map (λ (comp)
                (and (>= (length comp) cmps)
                     (first comp)))
              #;(λ (comp) (and (= (length comp) cmps)
                     (last comp))
                     comp)
       LoC))


(define (get-farthest-consumed LoC acc)
  (cond [(empty? LoC) acc]
        [(< (length (second (first (first LoC)))) (length acc))
         (get-farthest-consumed (rest LoC) (second (first (first LoC))))]
        [else (get-farthest-consumed (rest LoC) acc)]))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M dead held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (sm-start M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv equal?) (member? state fail-inv equal?)) 'wedged]
                                              [(or (member? state held-inv equal?)
                                                   (member? state fail-inv equal?)
                                                   (member? state cut-off equal?)) 'filled]
                                              [(eq? state dead) 'dashed]
                                              [else 'solid])
                                 'shape (if (member? state (sm-finals M) equal?) 'doublecircle 'circle)
                                 'fillcolor (cond [(member? state cut-off equal?) GRAPHVIZ-CUTOFF-GOLD]
                                                  [(and (member? state held-inv equal?) (member? state fail-inv equal?))
                                                   "red:chartreuse4"]
                                                  [(member? state held-inv equal?) HELD-INV-COLOR ]
                                                  [(member? state fail-inv equal?) BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 ;'fontsize 24
                                 'fontcolor 'black
                                 'fontname (if (and (member? state held-inv equal?) (member? state fail-inv equal?))
                                               "times-bold"
                                               "Times-Roman"))))
         dgraph
         (sm-states M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-a-rules current-rules held-invs dead cut-off)
  (foldl (λ (rule graph)
           (add-edge graph
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(find-rule? rule dead current-a-rules)
                                               'green #;(if cut-off  GRAPHVIZ-CUTOFF-GOLD 'green)]
                                              [(find-rule? rule dead current-rules)
                                               'violetred #;(if cut-off GRAPHVIZ-CUTOFF-GOLD 'violetred)]
                                              [else 'black])
                                 'style (cond [(equal? (third rule) dead) 'dashed]
                                              [(member? rule current-a-rules equal?) 'bold]
                                              [else 'solid])
                                 'fontsize 20)))
         dgraph
         rules))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  (let* (;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of all configurations
         [r-rules (get-trace-X (building-viz-state-reject-traces a-vs) trace-rule)]

         ;;(listof configs)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs (get-portion-configs (building-viz-state-upci a-vs)
                                               (building-viz-state-acc-comp a-vs))
                          #;(get-trace-X (append (building-viz-state-accept-traces a-vs)
                                               (building-viz-state-reject-traces a-vs))
                                       trace-config) ]

         [cut-off-states (remove-duplicates (map first (get-cut-off (building-viz-state-computations a-vs)
                                                                    (building-viz-state-max-cmps a-vs))))]
         
         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [a-rules (get-trace-X (building-viz-state-accept-traces a-vs) trace-rule)]

         ;[a-rules (get-config-X (building-viz-state-accept-traces a-vs) trace-rule)]

         ;;(listof rules)
         ;;Purpose: Extracts the triple and pair from rule-structs
         [curr-accept-rules (remake-rules a-rules)]

         ;;(listof rules)
         ;;Purpose: Extracts the triple and pair from rule-structs
         [curr-reject-rules (remake-rules r-rules)]         
         
         ;;(listof rules)
         ;;Purpose: Converts the current rules from the rejecting computations and makes them usable for graphviz
         [current-r-rules (configs->rules (filter (λ (rule) (not (equal? rule DUMMY-RULE))) curr-reject-rules))]
                  
         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-a-rules (configs->rules (filter (λ (rule) (not (equal? rule DUMMY-RULE))) curr-accept-rules))]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (sm-rules (building-viz-state-M a-vs)))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (equal? (first invs) (first curr)))
                     (list invs (building-viz-state-pci a-vs) (third curr)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (get-invariants get-invs not)]
         
         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (get-invariants get-invs id)])
    (begin
      ;(displayln cut-off-states)
      ;(displayln current-configs)
      #|(displayln "configs")
      (displayln current-configs)
      (displayln "invs")
      (displayln get-invs)
      (displayln "brk")
      (displayln brkn-invs)
      (displayln "held")
      (displayln held-invs)|#
      (make-edge-graph
       (make-node-graph
        (create-graph 'pdagraph #:atb (hash 'rankdir "LR"))
        (building-viz-state-M a-vs)
        (building-viz-state-dead a-vs)
        held-invs
        brkn-invs
        cut-off-states)
       all-rules
       current-a-rules
       current-r-rules
       held-invs
       (building-viz-state-dead a-vs)
       #f))))
    
;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(and (empty? (building-viz-state-upci a-vs))
              (or (list? (building-viz-state-stack a-vs))
                  (zipper-at-end? (building-viz-state-stack a-vs))))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [(andmap (λ (comp-len) (= comp-len (building-viz-state-max-cmps a-vs))) (map length (building-viz-state-computations a-vs)))
         #;(length (map length (building-viz-state-computations a-vs)) (sub1 (building-viz-state-max-cmps a-vs)))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  [upci (if (empty? (building-viz-state-upci a-vs))
                                                            (building-viz-state-upci a-vs)
                                                            (rest (building-viz-state-upci a-vs)))]
                                                  [pci (if (empty? (building-viz-state-upci a-vs))
                                                           (building-viz-state-pci a-vs)
                                                           (append (building-viz-state-pci a-vs)
                                                                   (list (first (building-viz-state-upci a-vs)))))]
                                                  [stack (if (or (zipper-empty? (building-viz-state-stack a-vs))
                                                                 (zipper-at-end? (building-viz-state-stack a-vs)))
                                                             (building-viz-state-stack a-vs)
                                                             (zipper-next (building-viz-state-stack a-vs)))]
                                                  [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                                  [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (second (first config))))
                                   (map computation-LoC (get-computations (imsg-state-pci imsg-st)
                                                                          (sm-rules (imsg-state-M imsg-st))
                                                                          (sm-start (imsg-state-M imsg-st))
                                                                          (imsg-state-max-cmps imsg-st))))]
         
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed (imsg-state-pci imsg-st)
                                                  (imsg-state-M imsg-st)
                                                  (imsg-state-max-cmps imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (drop entire-word (length last-consumed-word)) #;(remove-similarities last-consumed-word entire-word '())]
         
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (if (zipper-empty? (imsg-state-stck imsg-st)) 
                            (imsg-state-stck imsg-st)
                            (third (zipper-current (imsg-state-stck imsg-st))))])
    (begin
      ;(displayln last-consumed-word)
      ;(displayln entire-word)
      ;(displayln unconsumed-word)
      (overlay/align
     'left 'middle
     (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaa" 20 'white)
                      (text "Word: " 20 'black)
                      (if (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept)
                          (text (format "~a" EMP) 20 'gray)
                          (text (format "~a" EMP) 20 'red)))
              (beside (text "Consumed: " 20 'black)
                      (if (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept)
                          (text (format "~a" EMP) 20 'black)
                          (text (format "~a" EMP) 20 'white))))]
            [(and (not (empty? (imsg-state-farthest-consumed imsg-st)))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                     (imsg-state-comps imsg-st)))
             (above/align 'left
                          (beside (text "aaaa" 20 'white)
                                  (text "Word: " 20 'black)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? (imsg-state-pci imsg-st))
                                                     '()
                                                     (list (list (length (imsg-state-pci imsg-st)) 'gray)
                                                           (list (length (imsg-state-pci imsg-st)) DARKGOLDENROD2)))))
                          (beside (text "Consumed: " 20 'black)
                                  (make-tape-img (imsg-state-pci imsg-st)
                                                 (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                     (imsg-state-word-img-offset imsg-st)
                                                     0)
                                                 '())))]
            [(and (not (empty? (imsg-state-pci imsg-st))) (not completed-config?))
             (above/align
              'left
              (beside (text "aaaa" 20 'white)
                      (text "Word: " 20 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length last-consumed-word) 'gray)
                                               (list (length last-consumed-word) 'red)))))
              (beside (text "Consumed: " 20 'black)
                      (if (empty? last-consumed-word)
                          (text "" 20 'black)
                          (make-tape-img last-consumed-word
                                         (if (> (length last-consumed-word) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaa" 20 'white)
                                       (text "Word: " 20 'black)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " 20 'black)
                                       (make-tape-img (imsg-state-pci imsg-st)
                                                      (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (cond [(zipper-empty? (imsg-state-stck imsg-st)) (text "aaaa" 20 'white)]
            [(empty? current-stack) (beside (text "aaak" 20 'white)
                                            (text "Stack: " 20 'black))]
            [else (beside (text "aaak" 20 'white)
                          (text "Stack: " 20 'black)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                     (number->string (if (= (length (imsg-state-pci imsg-st)) (imsg-state-max-cmps imsg-st))
                                         (list-ref (imsg-state-comps-len imsg-st)
                                               (sub1 (length (imsg-state-pci imsg-st))))
                                         (list-ref (imsg-state-comps-len imsg-st)
                                               (length (imsg-state-pci imsg-st))))))
             20
             'brown)
      (cond [(and (ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                         (imsg-state-comps imsg-st))
                  (eq? (imsg-state-upci imsg-st)
                       (imsg-state-farthest-consumed imsg-st)))
              (text "There are computations that exceed the cut-off limit." 20 DARKGOLDENROD2)]
             [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." 20 'red)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (zipper-empty? (imsg-state-stck imsg-st))
                       (zipper-at-end? (imsg-state-stck imsg-st)))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept))
              (text "There is a computation that accepts." 20 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (zipper-empty? (imsg-state-stck imsg-st))
                       (zipper-at-end? (imsg-state-stck imsg-st)))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'reject))
              (text "All computations end in a non-final state and the machine rejects." 20 'red)]
             [else (text "Word Status: accept " 20 'white)])
      #;(beside
       (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                     (number->string (if (= (length (imsg-state-pci imsg-st)) (imsg-state-max-cmps imsg-st))
                                         (list-ref (imsg-state-comps imsg-st)
                                               (sub1 (length (imsg-state-pci imsg-st))))
                                         (list-ref (imsg-state-comps imsg-st)
                                               (length (imsg-state-pci imsg-st))))))
             20
             'brown)
       (text "aaaaa" 20 'white)
       (cond [(= (length (imsg-state-pci imsg-st)) (imsg-state-max-cmps imsg-st))
              (text (format "All computations exceed the cut-off limit: ~a." (imsg-state-max-cmps imsg-st)) 20 DARKGOLDENROD2)]
             [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." 20 'red)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (zipper-empty? (imsg-state-stck imsg-st))
                       (zipper-at-end? (imsg-state-stck imsg-st)))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept))
              (text "There is a computation that accepts." 20 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (zipper-empty? (imsg-state-stck imsg-st))
                       (zipper-at-end? (imsg-state-stck imsg-st)))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'reject))
              (text "All computations end in a non-final state and the machine rejects." 20 'red)]
             [(text "Word Status: accept " 20 'white)])))
     (rectangle 1250 50 'solid 'white)))))

;;upci is the unprocessed consumed input (listof symbol)
;;pci is the proccessed consumed input (listof symbol)
;;configs is a (listof configs) that attempt to consume the ci
;;stack is a (zipperof computation)
;;accept-configs is a (listof configuration)
;;reject-configs is a (listof configuration)
;;M is a machine
;;inv is a the (listof (state (listof symbol -> boolean)))
;;dead is the sybmol of dead state
(struct building-viz-state (upci pci computations acc-comp stack accept-traces reject-traces M inv dead max-cmps))

(define E-SCENE (empty-scene 1250 600))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* ([completed-config? (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                                   (get-computations (imsg-state-pci (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs)))
                                                                          (sm-rules (imsg-state-M (informative-messages-component-state
                                                                                                   (viz-state-informative-messages a-vs))))
                                                                          (sm-start (imsg-state-M (informative-messages-component-state
                                                                                                   (viz-state-informative-messages a-vs))))
                                                                          (imsg-state-max-cmps (informative-messages-component-state
                                                                                                (viz-state-informative-messages a-vs)))))]
         ;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [pci (if (or (not completed-config?)
                      (empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (eq? (imsg-state-upci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                           (imsg-state-farthest-consumed (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))))
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (append (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                          (list (first (imsg-state-upci (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))))))]
         [pci-len (length pci)])
    (begin (displayln (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
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
                     [upci (if (or (not completed-config?)
                                   (empty? (imsg-state-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                   (eq? (imsg-state-upci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                        (imsg-state-farthest-consumed (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                               
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (zipper-at-end? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))))
                                     (imsg-state-acpt-trace (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                     (zipper-next (imsg-state-acpt-trace (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))]
                     [stck (if (or (zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                                   (zipper-at-end? (imsg-state-stck (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                               (imsg-state-stck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (zipper-next (imsg-state-stck (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))]
                     [invs-zipper (cond [(zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))))
                                         (imsg-state-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs)))))
                                              (>= pci-len (first (zipper-unprocessed
                                                                  (imsg-state-invs-zipper (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs)))))))
                                         (zipper-next (imsg-state-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))])])])]))))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [full-word (append (imsg-state-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                            (imsg-state-upci (informative-messages-component-state
                                              (viz-state-informative-messages a-vs))))]
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed
                              full-word
                              (imsg-state-M (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                              (imsg-state-max-cmps (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))]
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (remove-similarities last-consumed-word full-word '())]
         [zip (if (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                  (imsg-state-invs-zipper (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                  (zipper-to-idx (imsg-state-invs-zipper (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                 (imsg-state-inv-amt (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy
         imsg-state
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         [upci (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (imsg-state-upci (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))]
                     [(not (equal? last-consumed-word full-word))
                      (rest unconsumed-word)]
                     [else '()])]
         [pci (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))
                     (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs)))]
                    [(not (equal? last-consumed-word full-word))
                     (append last-consumed-word (take unconsumed-word 1))]
                    [else full-word])]
         [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                             (zipper-at-end? (imsg-state-acpt-trace (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                         (imsg-state-acpt-trace (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))
                         (zipper-to-end (imsg-state-acpt-trace (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
         [stck (cond [(zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs))))
                      (imsg-state-stck (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))]
                     [(or (zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
                          (zipper-at-end? (imsg-state-stck (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))
                      (imsg-state-stck (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))]
                     [else (zipper-to-end (imsg-state-stck (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))])]
         
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))
                                         (zipper-at-begin? (imsg-state-acpt-trace (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))
                                     (imsg-state-acpt-trace (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                     (zipper-prev (imsg-state-acpt-trace (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))]
         [next-rule (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))
                        (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))
                        (first (trace-rule (zipper-current (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))))]
         [rule (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))
                   DUMMY-RULE
                   (list (rule-triple next-rule) (rule-pair next-rule)))]
         [pci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
                      (and (equal? (second (first rule)) EMP)
                           (not (empty-rule? rule))))
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (take (imsg-state-pci (informative-messages-component-state
                                         (viz-state-informative-messages a-vs)))
                        (sub1 (length (imsg-state-pci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))))]
         [pci-len (length pci)])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state
                     (informative-messages-component-state
                      (viz-state-informative-messages a-vs))
                     [upci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                                   (and (equal? (second (first rule)) EMP)
                                        (not (empty-rule? rule))))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [acpt-trace acpt-trace]
                     [stck (if (or (zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                                   (zipper-at-begin? (imsg-state-stck (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                               (imsg-state-stck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (zipper-prev (imsg-state-stck (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))]
                     
                     [invs-zipper (cond [(zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))))
                                         (imsg-state-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                              (viz-state-informative-messages a-vs)))))
                                              (<= pci-len (first (zipper-processed
                                                                  (imsg-state-invs-zipper (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs)))))))
                                         (zipper-prev (imsg-state-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
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
                   (informative-messages-component-state
                    (viz-state-informative-messages a-vs))
                   [upci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))
                             (imsg-state-upci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                             (append (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                   [pci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                            (imsg-state-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                            '())]
                   [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                       (zipper-at-begin? (imsg-state-acpt-trace (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs)))))
                                   (imsg-state-acpt-trace (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))
                                   (zipper-to-begin (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))]
                   [stck (if (or (zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs))))
                                 (zipper-at-begin? (imsg-state-stck (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                             (imsg-state-stck (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                             (zipper-to-begin (imsg-state-stck (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))))]
                   [invs-zipper (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                        (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))))
                                    (imsg-state-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))) 0))])])]))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy informative-messages
                   (viz-state-informative-messages a-vs)
                   [component-state
                    (struct-copy imsg-state
                                 a-imsgs
                                 [word-img-offset 0]
                                 [scroll-accum 0])])])))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy viz-state
                 a-vs
                 [informative-messages
                  (struct-copy informative-messages
                               (viz-state-informative-messages a-vs)
                               [component-state
                                (struct-copy imsg-state
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))
          (and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs))))
               (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs))))))
          (< (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))))
                           (<= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                      (zipper-prev (imsg-state-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                      (imsg-state-invs-zipper (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             [partial-word (take full-word (zipper-current zip))])
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [upci (remove-similarities full-word partial-word '())]
                         [acpt-trace (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (imsg-state-acpt-trace (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))
                                         (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs)))
                                                        (zipper-current zip)))]
                         [stck (if (zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                                   (imsg-state-stck (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                                   (zipper-to-idx (imsg-state-stck (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))
                                                  (zipper-current zip)))]
                         [pci partial-word]
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))
          (and (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
               (not (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs))))))
          (> (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))
                           (>= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                      (zipper-next (imsg-state-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                      (imsg-state-invs-zipper (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             [partial-word (take full-word (zipper-current zip))])
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [upci (remove-similarities full-word partial-word '())]
                         [acpt-trace (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (imsg-state-acpt-trace (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))
                                         (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs)))
                                                        (zipper-current zip)))]
                         [stck (if (zipper-empty? (imsg-state-stck (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                                   (imsg-state-stck (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                                   (zipper-to-idx (imsg-state-stck (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))
                                                  (zipper-current zip)))]
                         [pci partial-word]
                         [invs-zipper zip])])]))))

;;machine -> machine
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (local [;;symbol
          ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
          ;;         otherwise uses DEAD
          (define dead (if (member? DEAD (sm-states M) equal?) (gen-state (sm-states M)) DEAD))
          ;;(listof symbols)
          ;;Purpose: Makes partial rules for every combination of states in M and symbols in sigma of M
          (define new-read-rules
            (for*/list ([states (sm-states M)]
                        [sigma (sm-sigma M)])
                (list states sigma)))
        
          ;;(listof rules)
          ;;Purpose: Makes rules for that empty the stack and transition to the ds
          (define dead-pop-rules
            (for*/list ([ds (list dead)]
                        [gamma (sm-gamma M)])
              (list (list ds EMP (list gamma)) (list ds EMP))))
        
          ;;(listof rules)
          ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
          (define dead-read-rules
            (for*/list ([ds (list dead)]
                        [sigma (sm-sigma M)])
              (list (list ds sigma EMP) (list ds EMP))))
          ;;(listof rules)
          ;;Purpose: Gets rules that are not currently in the original rules of M
          (define get-rules-not-in-M  (local [(define partial-rules (map (λ (rule)
                                                                           (list (first (first rule)) (second (first rule))))
                                                                         (sm-rules M)))]
                                        (filter (λ (rule)
                                                  (not (member? rule partial-rules equal?)))
                                                new-read-rules)))
          ;;(listof rules)
          ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
          (define rules-to-dead
            (map (λ (rule) (cons (append rule (list EMP)) (list (list dead EMP))))
                 get-rules-not-in-M))]
    (make-ndpda (append (sm-states M) (list dead))
                (sm-sigma M)
                (sm-gamma M)
                (sm-start M)
                (sm-finals M)
                (append (sm-rules M) rules-to-dead dead-read-rules dead-pop-rules))))

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


;;pda word [boolean] [natnum] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (pda-viz M a-word #:add-dead [add-dead #f] #:max-cmps [max-cmps 100] . invs)
  (cond [(not (equal? (sm-type M) 'pda)) (error "The given machine must be a pda.")]
        [(<= max-cmps 0) (error (format "The maximum amount of computations, ~a, must be greater than 0" max-cmps))]
        [else (let* (;;M ;;Purpose: A new machine with the dead state if add-dead is true
                     [new-M (if add-dead (make-new-M M) M)]
                     ;;symbol ;;Purpose: The name of the dead state
                     [dead-state (if add-dead (last (sm-states new-M)) 'no-dead)]
                     ;;(listof computations) ;;Purpose: All computations that the machine can have
                     [computations (get-computations a-word (sm-rules new-M) (sm-start new-M) max-cmps)]
                     ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
                     [LoC (map computation-LoC computations)]
                     ;;number ;;Purpose: The length of the word
                     [word-len (length a-word)]
                     ;;(listof computation) ;;Purpose: Extracts all accepting computations
                     [accepting-computations (filter (λ (comp)
                                                       (and (member? (first (first (computation-LoC comp))) (sm-finals new-M) equal?)
                                                            (empty? (second (first (computation-LoC comp))))
                                                            (empty? (third (first (computation-LoC comp))))))
                                                     computations)]
                     ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
                     [accepting-traces (map (λ (acc-comp)
                                              (make-trace (reverse (computation-LoC acc-comp))
                                                          (reverse (computation-LoR acc-comp))
                                                          '()))
                                            accepting-computations)]
                     ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
                     [cut-accept-traces (if (> word-len max-cmps)
                                            (map last accepting-traces)
                                            '())]
                     ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
                     [accept-cmps (if (empty? cut-accept-traces)
                                      accepting-traces
                                      (map (λ (configs last-reject)
                                             (append configs (list last-reject)))
                                           accepting-traces
                                           cut-accept-traces))]
                     ;;(listof computation) ;;Purpose: Extracts all rejecting computations
                     [rejecting-computations (filter (λ (config)
                                                       (not (member? config accepting-computations equal?)))
                                                     computations)]
                     ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
                     [rejecting-traces (map (λ (c)
                                              (make-trace (reverse (computation-LoC c))
                                                          (reverse (computation-LoR c))
                                                          '()))
                                            rejecting-computations)]
                     ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
                     [cut-reject-traces (if (> word-len max-cmps)
                                            (map last rejecting-traces)
                                            '())]
                     ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
                     [reject-cmps (if (empty? cut-reject-traces)
                                      rejecting-traces
                                      (map (λ (configs last-reject)
                                             (append configs (list last-reject)))
                                           rejecting-traces
                                           cut-reject-traces))]
                     ;;(zipperof computation) ;;Purpose: Gets the stack of the first accepting computation
                     [stack (list->zipper (remove-empty (if (empty? accepting-computations)
                                                            '()
                                                            (reverse (computation-LoC (first accepting-computations))))
                                                        '()))]
                     ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
                     [accepting-trace (if (empty? accept-cmps) '() (first accept-cmps))]
                     [least-consumed-word (if (ormap (λ (comp) (>= (length comp) max-cmps)) LoC)
                                                     (get-farthest-consumed LoC a-word)
                                                     'no-cut-off)]
                     ;;building-state struct
                     [building-state (building-viz-state a-word
                                                         '()
                                                         LoC
                                                         accepting-computations
                                                         stack
                                                         accept-cmps
                                                         reject-cmps
                                                         new-M
                                                         (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w s) #t)) invs) invs) 
                                                         dead-state
                                                         max-cmps)]
                     
                     ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
                     [graphs (create-graph-thunks building-state '())]
                     ;;(listof computation) ;;Purpose: Gets all the cut off computations if the length of the word is greater than max computations
                     [get-cut-off-comp (if (> word-len max-cmps)
                                           (map first LoC)
                                           '())]
                     ;;(listof computation) ;;Purpose: Makes the cut off computations if the length of the word is greater than max computations
                     [cut-off-comp (if (empty? get-cut-off-comp)
                                       LoC
                                       (map (λ (cut-off-comp comp)
                                              (append comp (list cut-off-comp)))
                                            get-cut-off-comp
                                            LoC))]
                     ;;(listof number) ;;Purpose: Gets the number of computations for each step
                     [computation-lens (count-computations a-word cut-off-comp '())]
                     ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
                     [inv-configs (map (λ (con)
                                         (length (second (first con))))
                                       (return-brk-inv-configs
                                        (get-inv-config-results
                                         (make-inv-configs a-word accepting-computations)
                                         invs)
                                        a-word))])
                ;(struct building-viz-state (upci pci M inv dead))
                ;(struct imsg-state (M upci pci))
                ;;ANCHOR
                ;(displayln least-consumed-word)
                ;(displayln (count-computations a-word cut-off-comp a-word))
                ;displayln (map displayln LoC))
                ;(displayln (length get-cut-off-comp))
                ;(displayln computation-lens)
                ;(map displayln cut-off-comp)
                ;(displayln  graphs)
                ;(displayln (list-ref graphs 0))
                ;(displayln (list-ref graphs max-cmps))
                ;(displayln (length graphs))
                ;(displayln (length accept-cmps))
                ;(displayln (computation-LoC (first accepting-computations)))
                ;(println reject-cmps)
                (run-viz graphs
                         (lambda () (graph->bitmap (first graphs)))
                         (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
                         DEFAULT-ZOOM
                         DEFAULT-ZOOM-CAP
                         DEFAULT-ZOOM-FLOOR
                         (informative-messages create-draw-informative-message
                                               (imsg-state new-M
                                                           a-word
                                                           '()
                                                           (list->zipper accepting-trace)
                                                           stack
                                                           least-consumed-word
                                                           (list->zipper inv-configs) 
                                                           (sub1 (length inv-configs))
                                                           computation-lens
                                                           LoC
                                                           max-cmps
                                                           0
                                                           (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                             (if (> 0 offset-cap) 0 offset-cap))
                                                           0)
                                               RULE-YIELD-DIMS)
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
                         (create-viz-process-key [ "right" viz-go-next right-key-pressed]
                                                 [ "left" viz-go-prev left-key-pressed]
                                                 [ "up" viz-go-to-begin up-key-pressed]
                                                 [ "down" viz-go-to-end down-key-pressed]
                                                 [ "w" viz-zoom-in identity]
                                                 [ "s" viz-zoom-out identity]
                                                 [ "r" viz-max-zoom-out identity]
                                                 [ "f" viz-max-zoom-in identity]
                                                 [ "e" viz-reset-zoom identity]
                                                 [ "a" identity a-key-pressed]
                                                 [ "d" identity d-key-pressed]
                                                 [ "wheel-down" viz-zoom-in identity]
                                                 [ "wheel-up" viz-zoom-out identity]
                                                 [ "j" jump-prev j-key-pressed]
                                                 [ "l" jump-next l-key-pressed]
                                                 )
                         (create-viz-process-tick E-SCENE-BOUNDING-LIMITS
                                                  NODE-SIZE
                                                  E-SCENE-WIDTH
                                                  E-SCENE-HEIGHT
                                                  CLICK-BUFFER-SECONDS
                                                  ( [RULE-YIELD-DIMS
                                                     (lambda (a-imsgs x-diff y-diff) a-imsgs)])
                                                  ( [ ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                                    [ ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                                    [ ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                                    [ ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                                    [ W-KEY-DIMS viz-zoom-in identity]
                                                    [ S-KEY-DIMS viz-zoom-out identity]
                                                    [ R-KEY-DIMS viz-max-zoom-out identity]
                                                    [ E-KEY-DIMS viz-reset-zoom identity]
                                                    [ F-KEY-DIMS viz-max-zoom-in identity]
                                                    [ A-KEY-DIMS identity a-key-pressed]
                                                    [ D-KEY-DIMS identity d-key-pressed]
                                                    [ J-KEY-DIMS jump-prev j-key-pressed]
                                                    [ L-KEY-DIMS jump-next l-key-pressed]))
                         'pda-viz))]))



(define SAME-NUM-AB (make-ndpda '(K H I)
                                '(a b)
                                '(a b)
                                'K
                                '(I)
                                `(((K ,EMP ,EMP)(H ,EMP))
                                  ((K a ,EMP)(K (b)))
                                  ((K b ,EMP)(K (a)))
                                  ((H ,EMP ,EMP)(K ,EMP))
                                  ((H b (b))(H ,EMP))
                                  ((H a (a))(H ,EMP))
                                  ((H ,EMP ,EMP)(I ,EMP)))))


(define P (make-ndpda '(S A B X)
                      '(a b)
                      '(b)
                      'S
                      '(X)
                      `(((S ε ε)(A ε)) ((S ε ε)(X ε)) ((S a ε)(S (b b)))
                                       ((A b ε)(A ε)) ((A a ε)(A ε)) ((A ε ε)(B ε))
                                       ((B ε ε)(A ε))
                                       ((X b (b b))(X ε)) ((X b (b))(X ε)))))

(define P2 (make-ndpda '(S H)
                      '(a b)
                      '(b)
                      'S
                      '(H)
                      `(((S ε ε)(H ε))     ((S a ε)(S (b b)))
                        ((H b (b b))(H ε)) ((H ε (b))(H ε)))))

(define P3 (make-ndpda '(S H)
                      '(a b)
                      '(b)
                      'S
                      '(H)
                      `(((S ε ε)(H ε))     ((S a ε)(S (b b)))
                        ((H b (b b))(H ε)) ((H b (b))(H ε)))))

(define aˆnbˆn (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

(define n (make-ndpda '(K H F M I)
                      '(a b)
                      '(a b)
                      'K
                      '(I)
                      `(((K b ,EMP)(H ,EMP))
                        ((H ,EMP ,EMP)(F ,EMP))
                        ((H ,EMP ,EMP)(M ,EMP))
                        ((F a ,EMP)(I ,EMP))
                        ((M a ,EMP)(I ,EMP)))))

(define nk (make-ndpda '(K H F M I)
                       '(a b)
                       '(a b)
                       'K
                       '(I)
                       `(((K b ,EMP)(H ,EMP))
                         ((H ,EMP ,EMP)(F ,EMP))
                         ((F ,EMP ,EMP)(M ,EMP))
                         ((M ,EMP ,EMP)(I ,EMP))
                         ((I ,EMP ,EMP)(H ,EMP)))))

(define a* (make-ndpda '(K H)
                       '(a b)
                       '(a)
                       'K
                       '(H)
                       `(((K ,EMP ,EMP)(H ,EMP))
                         ((H a ,EMP)(H ,EMP)))))

(define aa* (make-ndpda '(K H)
                        '(a b)
                        '(a)
                        'K
                        '(H)
                        `(((K a ,EMP)(H ,EMP))
                          ((H a ,EMP)(H ,EMP)))))

(define inf-a (make-ndpda '(K)
                          '(a)
                          '(a)
                          'K
                          '(K)
                          `(((K ,EMP ,EMP) (K, '(a))))))

(define wcw^r (make-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP) (P ,EMP))
                            ((P a ,EMP) (P (a)))
                            ((P b ,EMP) (P (b)))
                            ((P c ,EMP) (Q ,EMP))
                            ((Q a (a)) (Q ,EMP))
                            ((Q b (b)) (Q ,EMP))
                            ((Q ,EMP ,EMP) (F ,EMP)))))

(define PUW (make-ndpda '(S P Q F  A B X)
                        '(a b c)
                        '(a b)
                        'S
                        '(F X)
                        `(((S ,EMP ,EMP) (P ,EMP))
                          ((P a ,EMP) (P (a)))
                          ((P b ,EMP) (P (b)))
                          ((P c ,EMP) (Q ,EMP))
                          ((Q a (a)) (Q ,EMP))
                          ((Q b (b)) (Q ,EMP))
                          ((Q ,EMP ,EMP) (F ,EMP))
                          ((S ε ε)(A ε)) ((S ε ε)(X ε)) ((S a ε)(S (b b)))
                          ((A b ε)(A ε)) ((A a ε)(A ε)) ((A ε ε)(B ε))
                          ((B ε ε)(A ε))
                          ((X b (b b))(X ε)) ((X b (b))(X ε)))))

  
;;word stack-> boolean
;;purpose: Determine if the given word has an equal number of a's and b's
;;         and that the stack is empty
(define (K-INV a-word stck)
  (and (empty? stck)
       (= (length (filter (λ (w) (equal? w 'a)) a-word))
          (length (filter (λ (w) (equal? w 'b)) a-word)))))

;;word stack-> boolean
;;purpose: Determine if the given word has >= a's than b's with an a 
;;         and that the stack has only b's
(define (H-INV a-word stck)
  (and (andmap (λ (w) (not (equal? w 'a))) stck)
       (> (length (filter (λ (s) (equal? s 'a)) a-word))
          (length (filter (λ (s) (equal? s 'b)) a-word)))))

;;word stack-> boolean
;;purpose: Determine if the given word >= b's than a's than
;;         and that the stack has only a's
(define (F-INV a-word stck)
  (and (andmap (λ (w) (not (equal? w 'b))) stck))   
  (> (length (filter (λ (s) (equal? s 'b)) a-word))
     (length (filter (λ (s) (equal? s 'a)) a-word))))

;;word stack-> boolean
;;purpose: Determine if the given word has a differing amount of b's and a's
;;         and that the stack has the same amount of a's and b's 
(define (M-INV a-word stck)
  (not (and (= (length (filter (λ (w) (equal? w 'a)) stck))
               (length (filter (λ (w) (equal? w 'b)) stck)))
            (not (= (length (filter (λ (w) (equal? w 'b)) a-word))
                    (length (filter (λ (w) (equal? w 'a)) a-word)))))))

;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
#;(define (P-S-INV a-word stck)
  (let ([num-as (length (filter (λ (w) (eq? w 'a)) a-word))]
        [num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (and (or (<= num-as num-bs)
             (<= num-as (/ num-bs 2)))
         (andmap (λ (w) (eq? w 'b)) stck))))
;;Purpose: to determine if the number of b's in the stack is greater than or equal to the number of b's in the ci
(define (P-X-INV a-word stck)
  (let ([word-num-bs (length (filter (λ (w) (eq? w 'b)) a-word))]
        [stck-num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (>= stck-num-bs word-num-bs)))
;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-A-INV a-word stck)
  (let ([num-as (length (filter (λ (w) (eq? w 'a)) a-word))]
        [num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (and (or (<= num-as num-bs)
             (<= num-as (/ num-bs 2)))
         (andmap (λ (w) (eq? w 'b)) stck))))
;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-B-INV a-word stck)
  (let ([num-as (length (filter (λ (w) (eq? w 'a)) a-word))]
        [num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (and (or (<= num-as num-bs)
             (<= num-as (/ num-bs 2)))
         (andmap (λ (w) (eq? w 'b)) stck))))

;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-S-INV a-word stck)
  (and (andmap (λ (w) (eq? w 'b)) stck)
       (andmap (λ (w) (eq? w 'a)) a-word)
       (= (* 2 (length a-word)) (length stck))))
;;Purpose: to determine if the number of b's in the stack is greater than or equal to the number of b's in the ci
(define (P-H-INV ci stck)
  (let ([ci-as (filter (λ (w) (eq? w 'a)) ci)]
        [ci-bs (filter (λ (w) (eq? w 'b)) ci)])
    (and (equal? ci (append ci-as ci-bs))
         (andmap (λ (w) (eq? w 'b)) stck)
         (<= (length ci-as) (length (append ci-bs stck)) (* 2 (length ci-as))))))
#;(pda-viz P '(a a a b b b))

#;(pda-viz a* '(a a a a a) #:max-cmps 3
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (= (length w) 3)) (not (= (length w) 5)) (empty? s)))))
#;(pda-viz aa* '(a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (empty? w) (empty? s)))))
#;(pda-viz a* '(a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s)))))

#;(pda-viz a* '(a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a a a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s)))))

;(pda-viz P2 '(a a a b b b b))
;(pda-viz P2 '(a a a b b) (list 'S P-S-INV) (list 'X P-X-INV))
(pda-viz P3 '(a a a b b b) (list 'S P-S-INV) (list 'H P-X-INV))

;"note to self:"
;"edit A and D to scroll thru word, not jump to end"
;"max-cmps idea: check length of comp to determine if it has been cut-off "

(define pd (make-ndpda '(S A)
                       '(a b)
                       '(a b)
                       'S
                       '(A)
                       `(((S a ,EMP) (A (a)))
                         ((S a ,EMP) (A (b))))))

(define (pd-A-INV a-wrd a-stck)
  (andmap (λ (s) (eq? s 'a)) a-stck))

(define more-a-than-b (make-ndpda '(S A)
                                  '(a b)
                                  '(a)
                                  'S
                                  '(A)
                                  `(((S a ,EMP) (S (a)))
                                    ((S ,EMP ,EMP) (A ,EMP))
                                    ((A b (a)) (A ,EMP))
                                    ((A ,EMP (a)) (A ,EMP)))))

(define (s-inv wrd stck)
  (and (empty? wrd)
       (empty? stck)))

(define (a-inv wrd stck)
  (or (not (= (length (filter (λ (w) (equal? w 'a)) wrd)) 4))
      (not (= (length (filter (λ (w) (equal? w 'b)) wrd)) 2))))

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(define pd-numb>numa (grammar->sm numb>numa))

;(pda-viz pd-numb>numa '(a b) #:max-cmps 5)
(pda-viz pd-numb>numa '(a b) #:add-dead #t #:max-cmps 5)
;;track most consumed input