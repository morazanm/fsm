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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
A trace is a structure:
(make-trace config rules)
config is a single configuration
rule are a  (listof rule-struct)
|#
(struct multi-step-trace (config rules) #:transparent)


;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func)
  (ormap (λ (L) (eq-func x L)) lst))

#|

|#
(struct config-state (config a-word stack lor path limit visited) #:transparent)

#|

|#
(struct multi-config-state (config a-word rules stack lor path limit visited) #:transparent)

;;rule(struct) -> boolean
;;Purpose: Determines if the given rules is empty
;; (a rule is empty when nothing is consumed, pushed, and popped) 
(define (rule-empty? a-rule)
  (and (equal? (second (rule-triple a-rule)) EMP)
       (equal? (third (rule-triple a-rule)) EMP)
       (equal? (second (rule-pair a-rule)) EMP)))

(struct config-application (config rule) #:transparent)

(define (unique-config-application? config rule)
  (not (member? (config-application (config-state-config config) rule)
                (config-state-visited config)
                (lambda (x y) (and (equal? (config-application-config x)
                                           (config-application-config y))
                                   (equal? (config-application-rule x)
                                           (config-application-rule y)))))))

;;configs -> configs
;;Purpose: Removes the most recent visited from the config
(define (remove-visited config rule)
  (if (unique-config-application? (struct-copy config-state
                                               config
                                               [visited (rest (config-state-visited config))]
                                               )
                                  rule)
      config
      '()
      )
  )

;;config rule -> config
;;Purpose: Applys the given rule to the given config and returns the updated config
;;ASSUMPTION: The given rule can be applied to the config
(define (apply-rule a-config a-rule)
  ;;config -> config
  ;;Purpose: Applys the read portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-read a-config)
    (if (equal? (second (rule-triple a-rule)) EMP)
        (struct-copy config-state a-config
                     [config (list (first (rule-pair a-rule))
                                   (second (config-state-config a-config))
                                   (third (config-state-config a-config)))])
        (struct-copy config-state a-config
                     [config (list (first (rule-pair a-rule))
                                   (rest (second (config-state-config a-config)))
                                   (third (config-state-config a-config)))]
                     [a-word (rest (config-state-a-word a-config))])))
  ;;config -> config
  ;;Purpose: Applys the pop portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-pop a-config)
    (if (equal? (third (rule-triple a-rule)) EMP)
        a-config
        (struct-copy config-state a-config
                     [config (list (first (config-state-config a-config))
                                   (second (config-state-config a-config))
                                   (drop (third (config-state-config a-config))
                                         (length (third (rule-triple a-rule)))))]
                     [stack (drop (config-state-stack a-config) (length (third (rule-triple a-rule))))])))
  ;;config -> config
  ;;Purpose: Applys the push portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-push a-config)
    (if (equal? (second (rule-pair a-rule)) EMP)
        (struct-copy config-state a-config
                     [path (append (config-state-path a-config)
                                   (list (config-state-config a-config)))]
                     [visited (cons (config-application (config-state-config a-config) a-rule)
                                    (config-state-visited a-config))])
        (let ([new-config (list (first (config-state-config a-config))
                                (second (config-state-config a-config))
                                (append (second (rule-pair a-rule))
                                        (third (config-state-config a-config))))])
          (struct-copy config-state a-config
                       [config new-config]
                       [stack (append (second (rule-pair a-rule)) (list (config-state-stack a-config)))]
                       [path (append (config-state-path a-config) (list new-config))]
                       [visited (cons (config-application new-config a-rule) (config-state-visited a-config))]))))
  (apply-push (apply-pop (apply-read a-config))))

;;config (listof finals) -> boolean
;; Purpose: Determines if the given config is an accepting config
(define (accepting-config? a-config finals)
  (and (member? (first a-config) finals)
       (empty? (second a-config))
       (empty? (third a-config))))

;;config (listof finals) -> (listof configs)
;; Purpose: Makes the configs that the machine can take to consume the input 
(define (new-make-configs configs finals)
  
  ;;config (listof configs) -> (listof configs)
  ;; Purpose: Makes the configs that the machine can take to consume the input 
  (define (new-make-configs-helper configs accum paths-visited)
    ;;config -> boolean
    ;; Purpose: Determines if the given config is an accepting config
    (define (accepting-config? a-config)
      (and (member? (first (config-state-config a-config)) finals equal?)
           (empty? (second (config-state-config a-config)))
           (empty? (third (config-state-config a-config)))))
  
    ;;config rule -> boolean
    ;;Purpose: Determines if the given rule can be applied to the given config
    (define (can-apply-rule? config rule)
      ;;config rule -> boolean
      ;; Purpose: Determines if the given rule's read portion can be applied to the given config
      (define (can-read? config rule)
        (or (equal? EMP (second (rule-triple rule)))
            (and (not (empty? (config-state-a-word config)))
                 (equal? (second (rule-triple rule)) (first (config-state-a-word config)))))
        )
      ;;config rule -> boolean
      ;;Purpose: Determines if the given rule's pop portion can be applied to the given config
      (define (can-pop? config rule)
        (or (equal? EMP (third (rule-triple rule)))
            (and (<= (length (third (rule-triple rule))) (length (config-state-stack config)))
                 (equal? (third (rule-triple rule)) (take (config-state-stack config) (add1 (length (third (rule-triple rule)))))))
            )
        )
      (and (can-read? config rule)
           (can-pop? config rule)))
    

    ;;config -> multi-config
    ;;Purpose: Finds all the empties for the given config
    (define (find-empties config)
      ;(struct find-empties-state (config-st emp-path accum))
      (define (find-empties-helper configs accum)
        (define (find-all-multi-steps config)
          (define (find-all-multi-steps-helper configs visited accum)
            (if (empty? configs)
                accum
                (if (or (accepting-config? (first configs))
                        (>= (length (config-state-path (first configs))) (config-state-limit (first configs))))
                    (find-all-multi-steps-helper (rest configs) visited (append accum (list (first configs))))
                    (let* ([config (first configs)]
                           [possibly-applicable-rules (filter (lambda (x) (equal? (first (rule-triple x))
                                                                                  (first (config-state-config config))))
                                                              (config-state-lor config))]
                           [empty-rules (filter (lambda (x) (rule-empty? x)) possibly-applicable-rules)]
                           [applicable-empty-rules (filter (lambda (x) (not (member? (third (rule-triple x)) visited equal?))) empty-rules)]
                           [new-configs (filter (lambda (x) (not (empty? x)))
                                                (map (lambda (x) (if (unique-config-application? (struct-copy config-state
                                                                                                              (apply-rule config x)
                                                                                                              [visited (rest (config-state-visited (apply-rule config x)))]
                                                                                                              )
                                                                                                 x)
                                                                     (apply-rule config x)
                                                                     '()
                                                                     )
                                                       ) applicable-empty-rules)
                                                )]
                           )
                      (if (empty? applicable-empty-rules)
                          (find-all-multi-steps-helper (rest configs)
                                                       visited
                                                       (append accum (list (first configs))))
                          (find-all-multi-steps-helper (append (rest configs) new-configs)
                                                       (append visited
                                                               (map (lambda (x) (third (rule-triple x))) applicable-empty-rules))
                                                       accum)
                          )
                      )
                    )
                )
            )
          (find-all-multi-steps-helper (list config) '() '())
          )

        #;(displayln (format "empties-configs: ~a" (map (lambda (x) (config-state-path x)) configs)))
        (if (empty? configs)
            accum
            (if (accepting-config? (first configs))
                (find-empties-helper (rest configs) (append accum (list (first configs))))
                (if (>= (length (config-state-path (first configs))) (config-state-limit (first configs)))
                    (find-empties-helper (rest configs) (append accum (list (first configs))))
                    (let* ([config (first configs)]
                           [possibly-applicable-rules (filter (lambda (x) (equal? (first (rule-triple x))
                                                                                  (first (config-state-config config))))
                                                              (config-state-lor config))]
                           [empty-rules (filter (lambda (x) (rule-empty? x)) possibly-applicable-rules)]
                           [all-multi-steps '() #;(find-all-multi-steps config)] ;;TODO
                           [new-configs (filter (lambda (x) (not (empty? x)))
                                                (map (lambda (x) (if (unique-config-application? (struct-copy config-state
                                                                                                              (apply-rule config x)
                                                                                                              [visited (rest (config-state-visited (apply-rule config x)))]
                                                                                                              )
                                                                                                 x)
                                                                     (apply-rule config x)
                                                                     '())
                                                       ) empty-rules)
                                                #;(remove-visited (map (lambda (x) (apply-rule config x)) empty-rules)))]
                           
                           #;[test0 (map (lambda (y) (displayln (format "new-configs: ~a" y))) (map (lambda (x) (config-state-path x)) new-configs))]
                           )
                      (if (or (empty? empty-rules)
                              (and (empty? all-multi-steps)
                                   (empty? new-configs)))
                          (find-empties-helper (rest configs) (append accum (list (first configs))))
                          (find-empties-helper (append (rest configs) all-multi-steps new-configs) accum)
                          )
                      )
                    )
                )
            )
        )
      (find-empties-helper (list config) '())
      #;(let* ([possibly-applicable-rules (filter (lambda (x) (equal? (first (rule-triple x)) (first (config-state-config config)))) (config-state-lor config))]
               [empty-rules (filter (lambda (x) (rule-empty? x)) possibly-applicable-rules)]
               [new-configs (filter (lambda (x) (not (empty? x)))
                                    (remove-visited (map (lambda (y) (struct-copy config-state y
                                                                                  [visited (rest (config-state-visited y))]))
                                                         (map (lambda (x) (apply-rule config x)) empty-rules))))]
               ;[test0 (displayln (format "new-configs: ~a" new-configs))]
               ) ;;maybe we dont need map???
          (map (λ (rules) (multi-config-state (config-state-config config)
                                              (config-state-a-word config)
                                              rules
                                              (config-state-stack config)
                                              (config-state-lor config)
                                              (config-state-path config)
                                              (config-state-limit config)
                                              (config-state-visited config)))
               (append-map (lambda (x) (find-empties x)) new-configs))
          )
      )
    #;(displayln (format "new-make-configs accum: ~s" accum))
    ;(displayln (format "length of queue: ~a" (length configs)))
    ;(displayln "new loop")
    ;(map displayln (map config-state-path configs))
    (if (empty? configs)
        (begin
          #;(displayln "queue: ")
          #;(map displayln (map (lambda (x) (config-state-path x)) configs))
          ;(displayln "accum: ")
          (map displayln (map (lambda (x) (config-state-path x)) accum))
          accum
          )
        (if (>= (length (config-state-path (first configs))) (config-state-limit (first configs)))
            (begin
              #;(displayln "limit is working")
              (new-make-configs-helper (rest configs) (append accum (list (first configs))) paths-visited)
              )
            (if (accepting-config? (first configs))
                (new-make-configs-helper (rest configs) (append accum (list (first configs))) paths-visited)
                (let* ([config (first configs)]
                       [rules (config-state-lor config)]
                       ;[test0 (displayln (format "rules: ~a" rules))]
                       [possibly-applicable-rules (filter (lambda (x) (equal? (first (rule-triple x)) (first (config-state-config config)))) rules)]
                       [applicable-rules (filter (lambda (x) (can-apply-rule? config x)) possibly-applicable-rules)]
                 
                       [first-empties (filter (lambda (x) (rule-empty? x)) applicable-rules)]
                       ;[test0 (displayln applicable-rules)]
                       [extended-empties (begin
                                           #;(displayln "find-empties called")
                                           #;(map displayln (map (lambda (x) (config-state-path x)) (find-empties config)))
                                           (remove-duplicates
                                            (filter
                                             (lambda (x) (not (member? (config-state-path x)
                                                                       paths-visited
                                                                       equal?)))
                                             (find-empties config)
                                             )
                                            
                                            #:key config-state-path
                                            )
                                           )
                                         #;(append-map (lambda (x) (find-empties x)) (list (first config)))
                                         #;(append-map (lambda (x) (find-empties (apply-rule config x))) first-empties)]
                       #;'(((X (a a b b b b) ()) (#(struct:config-application (X (a a b b b b) ()) #(struct:rule (S ε ε) (X ε))) #(struct:config-application (S (a a b b b b) ()) ()))))
                       #;'(((S (a b b b b) (b b)) ((S (a b b b b) (b b)) #(struct:config-application (S (a a b b b b) ()) ()))))
                       #;[test0 (displayln (format "extended-empties: ~a" (map (lambda (x) (config-state-config x)) extended-empties)))]
                       [new-configs (remove-duplicates
                                     (filter
                                      (lambda (x) (not (member? (config-state-path x)
                                                                paths-visited
                                                                equal?)))
                                      (filter (lambda (x) (not (empty? x)))
                                              (map (lambda (x) (if (unique-config-application? (struct-copy config-state
                                                                                                            (apply-rule config x)
                                                                                                            [visited (rest (config-state-visited (apply-rule config x)))]
                                                                                                            )
                                                                                               x)
                                                                   (apply-rule config x)
                                                                   '())
                                                     ) applicable-rules)
                                              #;(remove-visited (map (lambda (x) (apply-rule config x)) empty-rules)))
                                      )
                                     #:key config-state-path
                                     )
                                    #;(map (lambda (x) (apply-rule config x)) applicable-rules ) ]
                       [new-to-compute (remove-duplicates
                                        (append new-configs extended-empties)
                                        #:key config-state-path
                                        )]
                       #;[test1 (displayln (map (lambda (x) (config-state-path x)) extended-empties))]
                       )
                  (if (or (empty? applicable-rules)
                          (empty? new-to-compute))
                      (new-make-configs-helper (rest configs) (append accum (list (first configs))) paths-visited)
                      (new-make-configs-helper (append (rest configs) new-to-compute) accum
                                               (append paths-visited
                                                       (map (lambda (x) (config-state-path x)) new-to-compute)
                                                       ))
                      )
                  )
                )
            )
        )
    )
  (new-make-configs-helper configs '() '())
  )

#;'(((S (a a b b b b) ()) (S (a b b b b) (b b)) (S (b b b b) (b b b b)) (X (b b b b) (b b b b)))
  ((S (a a b b b b) ()) (S (a b b b b) (b b)) (S (b b b b) (b b b b)) (A (b b b b) (b b b b)) (B (b b b b) (b b b b)) (A (b b b b) (b b b b))))

;(config a-word stack lor path limit visited)
;;(listof symbols) (listof rules) symbol -> (listof configurations)
;;Purpose: Returns all possible configurations from start that consume the given word
(define (get-configs a-word lor start max-cmps finals)
  (let (;;(listof configurations)
        ;;Purpose: All configurations from the start state
        [starting-configs (list (append (list start) (list a-word) (list '())))])
    
    (new-make-configs (list (config-state (first starting-configs)
                                          a-word
                                          (third (first starting-configs))
                                          (map (lambda (x) (rule (first x) (second x))) lor)
                                          (list (first starting-configs))
                                          max-cmps
                                          (list (config-application (first starting-configs) '()))
                  
                                          ))
                      finals
                      )))

;;(listof symbols) (listof rules) symbol -> (listof configurations)
;;Purpose: Returns all possible configurations from start that consume the given word
(define (get-configs2 a-word lor start max-cmps)
  (let (;;(listof configurations)
         ;;Purpose: All configurations from the start state
         [starting-configs (list (append (list start) (list a-word) (list '())))])
    (remove-duplicates
     (make-configs (first starting-configs)
                   a-word
                   (third (first starting-configs))
                   lor
                   (first starting-configs)
                   (list (first starting-configs))
                   max-cmps))))

;;configuration (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Explores all possible configurations using the given word, (listof rules), and visited
;;Visited = All configurations of the consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs config a-word stack lor visited path limit)
    (let* (;;(listof rules)
           ;;Purpose: Holds all rules that consume a first letter in the given configurations
           [connected-read-rules (if (empty? a-word)
                                     '()
                                     (filter (λ (rule)
                                               (and (equal? (first (first rule)) (first config))
                                                    (equal? (second (first rule)) (first (second config)))))
                                             lor))]
           ;;(listof rules)
           ;;Purpose: Holds all rules that can pop what is in the stack
           [connected-pop-rules (filter (λ (rule)
                                          (or (equal? (third (first rule)) EMP)
                                              (and (>= (length stack) (length (third (first rule))))
                                                   (equal? (take stack (length (third (first rule)))) (third (first rule))))))
                                        connected-read-rules)]
           ;;(listof configurations)
           ;;Purpose: Creates the preliminary configurations using the connected-pop-rules
           [prelim-config (map (λ (rule)
                                 (append (list (first (second rule)))
                                         (list (rest a-word))
                                         (list (if (eq? (third (first rule)) EMP) stack (drop stack (length (third (first rule))))))))
                               connected-pop-rules)]
           ;;(listof configurations)
           ;;Purpose: Creates the configurations using the connected-pop-rules and preliminary configurations
           [new-configs (filter (λ (config) (not (member? config visited equal?)))
                                (map (λ (rule config)
                                       (if (eq? (second (second rule)) EMP)
                                           config
                                           (append (list (first config))
                                                   (list (second config))
                                                   (list (append (second (second rule)) (third config))))))
                                     connected-pop-rules
                                     prelim-config))]
           ;;(listof rules)
           ;;Purpose: Holds all rules that consume no input for the given configurations
           [connected-read-E-rules (filter (λ (rule)
                                             (and (equal? (first (first rule)) (first config))
                                                  (equal? (second (first rule)) EMP)))
                                           lor)]
           ;;(listof rules)
           ;;Purpose: Holds all rules that can pop what is in the stack
           [connected-pop-E-rules (filter (λ (rule)
                                            (or (equal? (third (first rule)) EMP)
                                                (and (>= (length stack) (length (third (first rule))))
                                                     (equal? (take stack (length (third (first rule)))) (third (first rule))))))
                                          connected-read-E-rules)]
           ;;(listof configurations)
           ;;Purpose: Creates the preliminary e-tran configurations using the connected-pop-E-rules
           [prelim-E-config (map (λ (rule)
                                   (append (list (first (second rule)))
                                           (list a-word) 
                                           (list (if (eq? (third (first rule)) EMP) stack (drop stack (length (third (first rule))))))))
                                 connected-pop-E-rules)]
           ;;(listof configurations)
           ;;Purpose: Creates the configurations using the connected-pop-rules and preliminary configurations
           [new-E-configs (filter (λ (config) (not (member? config visited equal?)))
                                  (map (λ (rule config)
                                         (if (eq? (second (second rule)) EMP)
                                             config
                                             (append (list (first config))
                                                     (list (second config))
                                                     (list (append (second (second rule)) (third config))))))
                                       connected-pop-E-rules
                                       prelim-E-config))])
      (cond [(or (> (length path) limit) (and (empty? new-configs) (empty? new-E-configs)))
             (list path)]
            [(and (empty? new-configs) (not (empty? new-E-configs)))
             (make-configs-helper new-E-configs a-word lor (append new-E-configs visited) path limit)]
            [(and (not (empty? new-configs)) (empty? new-E-configs))
             (make-configs-helper new-configs (rest a-word) lor (append new-configs visited) path limit)]
            [else (append (make-configs-helper new-E-configs a-word lor (append new-E-configs visited) path limit)
                          (make-configs-helper new-configs (rest a-word) lor (append new-configs visited) path limit))])))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Ensures that all possible configurations are explored
;;Visited = All configurations that consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs-helper configs a-word lor visited path limit)
    (if (empty? configs)
        '()
        (append (make-configs (first configs) a-word (third (first configs)) lor visited (append path (list (first configs))) limit)
                (make-configs-helper (rest configs) a-word lor visited path limit))))


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

(define (apply-rule2 a-comp a-rule)
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
                 [LoT (cons (trace (first (computation-LoC a-comp)) (rule (first a-rule) (second a-rule)))
                            (computation-LoT a-comp))]
                 [visited (cons (first (computation-LoC a-comp)) (computation-visited a-comp))]))

;; trace (listof trace) -> boolean
(define (same-trace? a-trace a-LoT)
  (and (member? (trace-config a-trace) (map trace-config a-LoT) equal?)
       (member? (trace-rule a-trace) (map trace-rule a-LoT) equal?)))
       
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

(struct computation (LoC LoR LoT visited) #:transparent)

;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start max-cmps)
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-config (computation (list (append (list start) (list a-word) (list '())))
                                      '()
                                      '()
                                      '())])
    (make-computations lor
                       (enqueue (list starting-config) E-QUEUE)
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
                                            (not (member? (first (computation-LoC new-c)) (computation-visited new-c) equal?))
                                                 #;(not (same-trace? (first (computation-LoT new-c)) (rest (computation-LoT new-c)))))
                                          (map (λ (rule) (apply-rule2 (qfirst QoC) rule)) connected-pop-rules))])
                (if (empty? new-configs)
                    (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)
                    (make-computations lor (enqueue new-configs (dequeue QoC)) path max-cmps)))]))

;;(listof configurations) (listof symbols) (listof rule) (listof rules) -> (listof rules)
;;Purpose: Attaches the transition rules used between configurations
(define (attach-rules->configs configs word lor acc)
  (cond [(<= (length configs) 1) (reverse acc)]
        [(equal? (second (first configs)) (second (second configs)))
         (local [(define (split-path rule)
                   (if (equal? (second (second rule)) EMP)
                       (length (third (second configs)))
                       (length (second (second rule)))))
                 (define e-rule-used (filter (λ (rule)
                                               (and (equal? (first (first rule)) (first (first configs)))
                                                    (equal? (second (first rule)) EMP)
                                                    (equal? (first (second rule)) (first (second configs)))
                                                    ))
                                             lor))]
           (attach-rules->configs (rest configs) word lor (append e-rule-used acc)))]
        [else (local [(define (split-path rule)
                        (if (equal? (second (second rule)) EMP)
                            (length (third (second configs)))
                            (length (second (second rule)))))
                      (define rule-used (filter (λ (rule)
                                                  (and (equal? (first (first rule)) (first (first configs)))
                                                       (equal? (second (first rule)) (first word))
                                                       (equal? (first (second rule)) (first (second configs)))
                                                       ;(equal? (second (second rule)) (take (third (second configs)) (split-path rule)))
                                                       ))
                                                lor))]
                (attach-rules->configs (rest configs) (rest word) lor (append rule-used acc)))]))

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace word configs rules acc #:accept [accept #f])
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first (first rules))) EMP)))
         (let* ([rle (rule (list EMP EMP EMP) (list EMP EMP))]
                [res (trace (first configs) rle)])
           (make-trace (rest word) (rest configs) rules (cons res acc)))]
        [(empty? word)
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (trace (first configs) rle)])
           (make-trace word (rest configs) (rest rules) (cons res acc)))]
        #;[(and (not (empty? acc))
                (equal? (second (first (first rules))) EMP))
           (let* ([rle (rule (first (first rules)) (second (first rules)))]
                  [res (struct-copy trace (first acc)
                                    [rules (cons rle (trace-rules (first acc)))])])
             (make-trace word configs (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) rle)])
                (make-trace (rest word) (rest configs) (rest rules) (cons res acc)))]))


;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace2 configs rules acc #:accept [accept #f])
  (cond [(empty? rules)
         #;(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first (first rules))) EMP)))
         (let* ([rle (rule (list EMP EMP EMP) (list EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-trace2(rest configs) rules (cons res acc)))]
        #;[(empty? word)
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (trace (first configs) rle)])
           (make-trace2 (rest configs) (rest rules) (cons res acc)))]
        [(and (not (empty? acc))
              (empty-rule? (first rules)))
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (struct-copy trace (first acc)
                                  [rule (cons rle (trace-rule (first acc)))])])
           (make-trace2 (rest configs) (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) (list rle))])
                (make-trace2 (rest configs) (rest rules) (cons res acc)))]))

;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word configs)
  (append-map (λ (comp)
                (make-inv-configs-helper a-word (computation-LoC comp) (length a-word)))
                configs)
  ;(displayln configs)
  #;(if (empty? configs)
      '()
      (append (list (make-inv-configs-helper a-word (first configs) (length a-word)))
              (make-inv-configs a-word (rest configs)))))

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
              inv-configs)

  #;(if (or (empty? inv-configs)
            (empty? invs))
        '()
        (append (list (get-inv-config-results-helper (first inv-configs) invs))
                (get-inv-config-results (rest inv-configs) invs))))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
(define (get-inv-config-results-helper inv-configs invs)
  (if (empty? inv-configs)
      '()
      (let* ([get-inv-for-inv-config (filter (λ (inv)
                                               (equal? (first inv) (first (first inv-configs))))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (append (first inv-configs)
                                                  (list (inv-for-inv-config (second (first inv-configs))
                                                                            (third (first inv-configs)))))))])
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
      (let* ([new-acc (append-map (λ (inv-configs)
                                    (filter (λ (config)
                                              (and (equal? (second config) (take a-word (- (length a-word) word-len)))
                                                   (not (fourth config))))
                                            inv-configs))
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
                                                                       max-cmps)))#;(ormap (λ (config) (empty? (second (last config))))
                     (map config-state-path (get-configs a-word (sm-rules M) (sm-start M) max-cmps (sm-finals M)))))
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

;;(listof symbols) (listof configurations) boolean -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                          (equal? (second configs) word))
                        config))
              full-configs))

;;(listof rules) -> (listof rules)
;;Purpose: Converts the given (listof configurations)s to rules
(define (configs->rules curr-config)
  (make-rule-triples (remove-duplicates curr-config)))

;;(zipperof words) word (f-on-zip) -> zip
;;Purpose: Searches the zipper for the given word by applying the given function on the zipper
(define (zipper-search zip word zip-func)
  (if (equal? (second (zipper-current zip)) word)
      zip
      (zipper-search (zip-func zip) word zip-func)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M dead held-inv fail-inv)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (sm-start M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv equal?) (member? state fail-inv equal?)) 'wedged]
                                              [(or (member? state held-inv equal?) (member? state fail-inv equal?)) 'filled]                                              
                                              [else 'solid])
                                 'shape (if (member? state (sm-finals M) equal?) 'doublecircle 'circle)
                                 'fillcolor (cond [(and (member? state held-inv equal?) (member? state fail-inv equal?))
                                                   "red:green"]
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
                                               (if cut-off 'gold 'green)]
                                              [(find-rule? rule dead current-rules)
                                               (if cut-off 'gold 'violetred)]
                                              [else 'black])
                                 'style (cond [(equal? (third rule) dead) 'dashed]
                                              [(member? rule current-a-rules equal?) 'bold]
                                              [else 'solid])
                                 'fontsize 15)))
         dgraph
         rules))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (los2str los)
  (define (helper los)
    (if (empty? (rest los))
        (string-append (symbol->string (first los)) " ")
        (string-append (symbol->string (first los)) " " (helper (rest los)))))
  (helper los))

;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (make-edge-label los)
  (define (helper los)
    (if (empty? (rest los))
        (string-append (format "~a" (first los)) "]")
        (string-append (format "~a" (first los)) " " (helper (rest los)))))
  (string-append "[" (helper los)))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  (map (λ (rule)
         (append (list (first (first rule)))
                 (list (string->symbol (make-edge-label
                                        (list (second (first rule))
                                              (third (first rule))
                                              (second (second rule))))))
                 (list (first (second rule)))))
       rules))

;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  (let* (;;(listof configurations)
         ;;Purpose: Returns all configurations using the CI
         [current-config (get-portion-configs (if cut-off
                                                  (cons (last (building-viz-state-pci a-vs))
                                                        (building-viz-state-upci a-vs))
                                                  (building-viz-state-upci a-vs))
                                              (building-viz-state-configs a-vs))]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of all configurations
         [r-config (begin
                     ;(display (format "r-config ~v \n" (building-viz-state-reject-configs a-vs)))
                     ;(display (format "a-config ~v \n" (building-viz-state-accept-configs a-vs)))
                     (if (empty? (building-viz-state-reject-configs a-vs))
                         (building-viz-state-reject-configs a-vs)
                         (map (λ (configs) (trace-rule (first configs)))
                              (filter (λ (configs)
                                        (not (empty? configs))) (building-viz-state-reject-configs a-vs)))))]

         [r-config2 (begin
                     ;(display (format "r-config ~v \n" (building-viz-state-reject-configs a-vs)))
                     ;(display (format "a-config ~v \n" (building-viz-state-accept-configs a-vs)))
                     (if (empty? (building-viz-state-reject-configs a-vs))
                         (building-viz-state-reject-configs a-vs)
                         (map (λ (configs) (list (trace-config (first configs))
                                                 (trace-rule (first configs))))
                              (filter (λ (configs)
                                        (not (empty? configs))) (building-viz-state-reject-configs a-vs)))))]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [a-configs (if (empty? (building-viz-state-accept-configs a-vs))
                        (building-viz-state-accept-configs a-vs)
                        (map (λ (configs) (trace-rule (first configs))) (building-viz-state-accept-configs a-vs)))]

         [a-config (if (empty? (building-viz-state-accept-configs a-vs))
                        (building-viz-state-accept-configs a-vs)
                        (map (λ (configs) (list (trace-config (first configs))
                                                (trace-rule (first configs)))) (building-viz-state-accept-configs a-vs)))]
         
         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [curr-a-config (append-map (λ (lor)
                               (map (λ (rule)
                                      (list (rule-triple rule)
                                             (rule-pair rule)))
                                    lor))
                             a-configs)]

         [extracted-rules (append-map (λ (lor)
                               (map (λ (rule)
                                      (list (rule-triple rule)
                                             (rule-pair rule)))
                                    lor))
                             (append r-config a-configs))]

         [current-rules (configs->rules (filter (λ (rule) (not (equal? rule (list (list EMP EMP EMP) (list EMP EMP))))) extracted-rules))]
                  
         ;;(listof rules)
         ;;Purpose: The current rules of the accepting computations that the pda is using to consume the CI
         [current-a-rules (configs->rules (filter (λ (rule) (not (equal? rule (list (list EMP EMP EMP) (list EMP EMP))))) curr-a-config))]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (sm-rules (building-viz-state-M a-vs)))]
         
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-config]
                               #:when (equal? (first invs) (first curr)))
                     (list invs (building-viz-state-pci a-vs) (third curr)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (remove-duplicates
                     (append-map
                      (λ (inv)
                        (if (not ((second (first inv)) (second inv) (third inv)))
                            (list (first (first inv)))
                            '()))
                      get-invs))]
         
         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (append-map
                     (λ (inv)
                       (if ((second (first inv)) (second inv) (third inv))
                           (list (first (first inv)))
                           '()))
                     get-invs)]
         
         ;;(listof symbols)
         ;;Purpose: The states that the pda could be in
         [destin-states (begin
                          
                          ;(map (λ (con) (display (format "~s \n \n" con))) (building-viz-state-accept-configs a-vs))
                          ;(map (λ (con) (display (format "~s \n \n" con))) (building-viz-state-reject-configs a-vs))
                          ;(display (format "curr-a-config ~s \n" (first (building-viz-state-accept-configs a-vs)) #;curr-a-config))
                          ;(display (format "current-a-rules ~s \n \n" current-a-rules))
                          ;(display (format "curr-r-config ~s \n \n" (first (building-viz-state-reject-configs a-vs)) #;curr-r-config))
                          ;(display (format "current-rules ~s \n \n \n" current-rules))
                          ;(display (format "current-config ~s \n" current-config))
                          ;(display (format "invs ~s \n" (building-viz-state-inv a-vs)))
                          ;(display (format "get-invs ~s \n" get-invs))
                          ;(display (format "held-invs ~s \n" held-invs))
                          ;(display (format "brkn-invs ~s \n \n" brkn-invs))
                          ;(display (format "all rules ~s \n \n" (append current-rules current-a-rules)))
                          ;(display (format "r configs: ~s \n a configs: ~s \n \n" r-config a-configs))
                          #;(append (building-viz-state-reject-configs a-vs) (building-viz-state-accept-configs a-vs))
                          (if (not (empty? (building-viz-state-pci a-vs)))
                              (map last current-rules)
                              (cons (sm-start (building-viz-state-M a-vs)) (map last current-rules)))
                          #;(map last (append current-rules current-a-rules)))])
    
    (make-edge-graph
     (make-node-graph
      (create-graph 'pdagraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      (building-viz-state-dead a-vs)
      held-invs
      brkn-invs)
     all-rules
     current-a-rules
     current-rules
     held-invs
     (building-viz-state-dead a-vs)
     cut-off)
    #;(list (make-edge-graph
           (make-node-graph
            (create-graph 'pdagraph #:atb (hash 'rankdir "LR"))
            (building-viz-state-M a-vs)
            (building-viz-state-dead a-vs)
            held-invs
            brkn-invs)
           all-rules
           current-a-rules
           current-rules
           held-invs
           (building-viz-state-dead a-vs)
           cut-off)
          (length destin-states))))
    
;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(and (empty? (building-viz-state-upci a-vs))
              (or (list? (building-viz-state-stack a-vs))
                  (zipper-at-end? (building-viz-state-stack a-vs))))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [(> (length (building-viz-state-pci a-vs)) (sub1 (building-viz-state-max-cmps a-vs)))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [(not #;(ormap (λ (config) (empty? (second (last config))) #;(sm-finals (building-viz-state-M a-vs)))
                     (map config-state-path (get-configs (building-viz-state-pci a-vs)
                                                         (sm-rules (building-viz-state-M a-vs))
                                                         (sm-start (building-viz-state-M a-vs))
                                                         (building-viz-state-max-cmps a-vs)
                                                         (sm-finals (building-viz-state-M a-vs)))))
              (ormap (λ (config) (empty? (second (first config))))
                                   (map computation-LoC (get-computations (building-viz-state-pci a-vs)
                                                                       (sm-rules (building-viz-state-M a-vs))
                                                                       (sm-start (building-viz-state-M a-vs))
                                                                       (building-viz-state-max-cmps a-vs)))))
         (reverse (cons (create-graph-thunk a-vs) acc))]
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
                                                  [accept-configs (filter (λ (configs)
                                                                            (not (empty? configs)))
                                                                          (map rest (building-viz-state-accept-configs a-vs)))]
                                                  [reject-configs (filter (λ (configs)
                                                                            (not (empty? configs)))
                                                                          (map rest (building-viz-state-reject-configs a-vs)))])
                                     (cons next-graph acc)))]))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? #;(ormap (λ (config) (empty? (second (last config))))
                                   (map config-state-path (get-configs (imsg-state-pci imsg-st)
                                                                       (sm-rules (imsg-state-M imsg-st))
                                                                       (sm-start (imsg-state-M imsg-st))
                                                                       (imsg-state-max-cmps imsg-st)
                                                                       (sm-finals (imsg-state-M imsg-st)))))
                            (ormap (λ (config) (empty? (second (first config))))
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
         [unconsumed-word (remove-similarities last-consumed-word entire-word '())]
         
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (begin
                          ;(display (format "upstck ~s \n \n" (imsg-state-upstck imsg-st)))
                          ;(display (format "pstck ~s \n \n" (imsg-state-pstck imsg-st)))
                          (if (zipper-empty? (imsg-state-upstck imsg-st)) 
                           (imsg-state-pstck imsg-st)
                           (third (zipper-current (imsg-state-upstck imsg-st)))))])
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
            [(> (length (imsg-state-pci imsg-st)) (sub1 (imsg-state-max-cmps imsg-st)))
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
                                                           (list (length (imsg-state-pci imsg-st)) 'gold)))))
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
      (cond [(zipper-empty? (imsg-state-upstck imsg-st)) (text "aaaa" 20 'white)]
            [(empty? current-stack) (beside (text "aaak" 20 'white)
                                            (text "Stack: " 20 'black))]
            [else (beside (text "aaak" 20 'white)
                          (text "Stack: " 20 'black)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (beside
       (text (format "The current number of possible computations is ~a. "
                     (number->string (list-ref (imsg-state-comps imsg-st)
                                               (length (imsg-state-pci imsg-st)))))
             20
             'brown)
       (text "aaaaa" 20 'white)
       (cond [(> (length (imsg-state-pci imsg-st)) (sub1 (imsg-state-max-cmps imsg-st)))
              (text (format "All computations exceed the cut-off limit: ~a." (imsg-state-max-cmps imsg-st)) 20 'gold)]
             [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." 20 'red)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (list? (imsg-state-upstck imsg-st))
                       (zipper-at-end? (imsg-state-upstck imsg-st)))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept))
              (text "There is a computation that accepts." 20 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (list? (imsg-state-upstck imsg-st))
                       (zipper-at-end? (imsg-state-upstck imsg-st)))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'reject))
              (text "All computations end in a non-final state and the machine rejects." 20 'red)]
             [(text "Word Status: accept " 20 'white)])))
     (rectangle 1250 50 'solid 'white))))

;;upci is the unprocessed consumed input (listof symbols)
;;pci is the proccessed consumed input (listof symbols)
;;M is a machine
;;inv is a the (listof (state (listof symbols -> boolean)))
;;dead is the sybmol of dead state
(struct building-viz-state (upci pci configs stack accept-configs reject-configs M inv dead max-cmps)); upinv-con pinv-con))
;(struct viz-state-ndfa (upci pci M inv dead imsg instruct graphs))

(struct imsg-state (M upci pci upstck pstck invs-zipper inv-amt comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)

(define E-SCENE (empty-scene 1250 600))

;; image int int -> image
;; PurpScales a image to the given dimentions
(define (resize-image img max-width max-height)
  (let* ([src-width (image-width img)]
         [src-height (image-height img)]
         [aspect (/ src-width src-height)]
         [scale (min (/ max-width src-width) (/ max-height src-height))]
         [scaled-width (* src-width scale)]
         [scaled-height (* src-height scale)])
    (cond [(and (> scaled-width max-width) (<= scaled-height max-height))
           (list (scale/xy (/ max-width src-width) (/ (/ scaled-width aspect) src-height) img)
                 (/ max-width src-width)
                 (/ (/ scaled-width aspect) src-height))]
          [(and (<= scaled-width max-width) (> scaled-height max-height))
           (let ([scaled-aspect (/ scaled-width scaled-height)])
             (list (scale/xy (/ (* scaled-height scaled-aspect) src-width) (/ max-height src-height) img)
                   (/ (* scaled-height scaled-aspect) src-width)
                   (/ max-height src-height)))]
          [(and (> scaled-width max-width) (> scaled-height max-height))
           (let* ([new-scaled-height (/ max-width aspect)]
                  [scaled-aspect (/ max-width new-scaled-height)])
             (list (scale/xy (/ (* max-height scaled-aspect) src-width) (/ max-height src-height) img)
                   (/ (* max-height scaled-aspect) src-width)
                   (/ max-height src-height)))]
          [(and (<= scaled-width max-width) (<= scaled-height max-height))
           (list (scale/xy (/ scaled-width src-width) (/ scaled-height src-height) img)
                 (/ scaled-width src-width)
                 (/ scaled-height src-height))])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (second (first config))))
                                   (map computation-LoC (get-computations (imsg-state-pci (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))
                                                                       (sm-rules (imsg-state-M (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                                       (sm-start (imsg-state-M (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                                       (imsg-state-max-cmps (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs))))))
                            #;(ormap (λ (config)
                                     (and (empty? (second (last config)))
                                          (empty? (third (last config)))))
                                   (map config-state-path
                                        (get-configs (imsg-state-pci (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))) 
                                                     (sm-rules (imsg-state-M (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                     (sm-start (imsg-state-M (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                     (imsg-state-max-cmps (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))
                                                     (sm-finals (imsg-state-M (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs)))))))]
         [pci (if (empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                  ;;something about completed config
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (append (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                          (list (first (imsg-state-upci (informative-messages-component-state
                                                         (viz-state-informative-messages
                                                          a-vs)))))))]
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
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [upci (if (empty? (imsg-state-upci (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                               
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [upstck 
                      (cond #;[(list? (imsg-state-upstck (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                             (imsg-state-upstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))]
                            [(or (zipper-empty? (imsg-state-upstck (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                 (zipper-at-end? (imsg-state-upstck (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))))
                             (imsg-state-upstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))]
                            [else (zipper-next (imsg-state-upstck (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))])]
                     [pstck (imsg-state-pstck (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))]
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
                                                                       (viz-state-informative-messages a-vs)))])])])])))

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
         [upstck (cond [(zipper-empty? (imsg-state-upstck (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                        (imsg-state-upstck (informative-messages-component-state
                                            (viz-state-informative-messages a-vs)))]
                       [(or (zipper-empty? (imsg-state-upstck (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                            (zipper-at-end? (imsg-state-upstck (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))
                        (imsg-state-upstck (informative-messages-component-state
                                            (viz-state-informative-messages a-vs)))]
                       [else (zipper-to-end (imsg-state-upstck (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))])]
         [pstck (imsg-state-pstck (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))]
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([pci (if (empty? (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
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
                     [upci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [upstck 
                      (cond #;[(list? (imsg-state-upstck (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                             (imsg-state-upstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))]
                            [(or (zipper-empty? (imsg-state-upstck (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                                 (zipper-at-begin? (imsg-state-upstck (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))))
                             (imsg-state-upstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))]
                            [else (zipper-prev (imsg-state-upstck (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))])]
                     [pstck (imsg-state-pstck (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))]
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
                   [upstck (cond #;[(zipper-empty? (imsg-state-upstck (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                  (imsg-state-upstck (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))]
                                 [(or (zipper-empty? (imsg-state-upstck (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                      (zipper-at-begin? (imsg-state-upstck (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))
                                  (imsg-state-upstck (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))]
                                 [else (zipper-to-begin (imsg-state-upstck (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))])]
                   [pstck (if (empty? (imsg-state-upstck (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))   
                              (imsg-state-pstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                              '())]
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
                         [upstck (if (list? (imsg-state-upstck (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                     (imsg-state-upstck (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))
                                     (zipper-search (imsg-state-upstck (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))
                                                    (drop full-word (zipper-current zip))
                                                    zipper-prev))]
                         [pci partial-word]
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))))
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
                         [upstck (if (list? (imsg-state-upstck (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                     (imsg-state-upstck (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))
                                     (zipper-search (imsg-state-upstck (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))
                                                    (drop full-word (zipper-current zip))
                                                    zipper-next))]
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

;; (Listof trace) natnum -> (Listof trace)
;; Extends the list with empty lists so it is the same length as len
(define (extend-lst lst len)
  (append lst (make-list (- len (length lst)) '())))


;; (Listof trace) -> (Listof trace)
;; Extends the lists with empty lists so that they are all of the same length
(define (form-traces traces)
  (let ([max-len (apply max (map length traces))])
    (map (lambda (x) (extend-lst x max-len)) traces)))

;;(listof trace) number -> (f-on-list)
;;Purpose: Finds the trace before the last empty
;;acc =
(define (until-last-empty traces acc)
  (if (empty? traces)
      (lambda (x) (list-ref x acc))
      (if (rule-empty? (trace-rule (first traces)))
          (until-last-empty (rest traces) (add1 acc))
          (lambda (x) (list-ref x acc)))
      ))

;;(listof trace) number -> number
;;Purpose: Finds the index of the trace before the last empty
;;acc = 
(define (until-last-empty-idx traces acc)
  (if (empty? traces)
      acc
      (if (rule-empty? (trace-rule (first traces)))
          (until-last-empty-idx (rest traces) (add1 acc))
          acc)))

;; (Listof trace) -> (Listof trace)
;; Calculate number of computations for each step of the computation
(define (find-num-comps traces)
  (define max-len (apply max (map length traces)))
  (define (find-num-comps-helper traces)
    (if (andmap empty? traces)
        '()
        (cons (length (coallesce (map (lambda (x) (if (empty? x)
                                                      '()
                                                      (if (rule-empty? (trace-rule (first x)))
                                                          ((until-last-empty (rest x) 0) x)
                                                          (first x)))
                                        )  traces)))
              (find-num-comps-helper (map (lambda (x) (if (empty? x)
                                                          '()
                                                          (if (rule-empty? (trace-rule (first x)))
                                                              ((lambda (x) (drop x (until-last-empty-idx (rest x) 1))) x)
                                                              (rest x)
                                                              )
                                                          ))
                                          traces)
                                     ))
        )
    )
  (find-num-comps-helper traces ))

                                 

;; (Listof trace) -> (Listof trace)
;; Coallesces configurations having the same rule applied
(define (coallesce traces)
  (define (rule-equal? x y)
    (if (empty? x)
        (empty? y)
        (and (not (empty? y))
             (equal? (rule-triple x) (rule-triple y))
             (equal? (rule-pair x) (rule-pair y)))))
  (define visited (make-custom-hash (lambda (x y) (and (equal? (trace-config x) (trace-config y))
                                                       (rule-equal? (trace-rule x) (trace-rule y))))))
  (define (coallesce-helper traces)
    (if (empty? traces)
        '()
        (if (or (empty? (first traces))
                (dict-ref visited (first traces) #f))
            (coallesce-helper (rest traces))
            (begin
              (dict-set! visited (first traces) 1)
              (cons (first traces) (coallesce-helper (rest traces)))
              )
            )
        )
    )
  (coallesce-helper traces))

;;pda word [boolean] [natnum] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (pda-viz M a-word #:add-dead [add-dead #f] #:max-cmps [max-cmps 100] . invs)
  (if (not (equal? (sm-type M) 'pda))
      (error "The given machine must be a pda.")
      (let* ([new-M (if add-dead (make-new-M M) M)]
             [dead-state (if add-dead (last (sm-states new-M)) 'no-dead)]
             #;[test0 (displayln "hello?")]
             #;[configs (get-configs a-word (sm-rules new-M) (sm-start new-M) max-cmps (sm-finals new-M))]
             #;[test1 (displayln "done here")]
             #;[test2 (displayln (format "hope this works: ~a" (find-num-comps configs)))]
             ;[config (map config-state-path configs) ]
             [computations (get-computations a-word (sm-rules new-M) (sm-start new-M) max-cmps)]
             [accept-computations (filter (λ (c)
                                      (and (member? (first (first (computation-LoC c) #;(config-state-path config))) (sm-finals new-M) equal?)
                                           (empty? (second (first (computation-LoC c) #;(config-state-path config))))
                                           (empty? (third (first (computation-LoC c) #;(config-state-path config))))))
                                    computations)]
             #;[accept-rules (map (λ (configs)
                                  (attach-rules->configs configs a-word (sm-rules new-M) '()))
                                accept-config)]
             #;[accepting-config (map (λ (config rules)
                                      (make-trace a-word config rules '()))
                                    accept-config
                                    accept-rules)]
             [accepting-config (map (λ (c)
                                     (make-trace2 (reverse (computation-LoC c))
                                                  (reverse (computation-LoR c))
                                                  '()))
                                   accept-computations)]
             [cut-accept-configs (if (> (length a-word) max-cmps)
                                     (map last accepting-config)
                                     '())]
             [accept-cmps (if (empty? cut-accept-configs)
                              accepting-config
                              (map (λ (configs last-reject)
                                     (append configs (list last-reject)))
                                   accepting-config
                                   cut-accept-configs))]
             
             [reject-configs (filter (λ (config)
                                       (not (member? config accept-computations equal?)))
                                     computations)]
             #;[reject-rules (map (λ (config)
                                  (attach-rules->configs config a-word (sm-rules new-M) '()))
                                reject-configs)]
             #;[rejecting-configs (map (λ (configs rules)
                                       (make-trace a-word configs rules '()))
                                     reject-configs
                                     reject-rules)]
             [rejecting-configs (map (λ (c)
                                     (make-trace2 (reverse (computation-LoC c))
                                                  (reverse (computation-LoR c))
                                                  '()))
                                   reject-configs)]
             [cut-reject-configs (if (> (length a-word) max-cmps)
                                     (map last rejecting-configs)
                                     '())]
             [reject-cmps (if (empty? cut-reject-configs)
                              rejecting-configs
                              (map (λ (configs last-reject)
                                     (append configs (list last-reject)))
                                   rejecting-configs
                                   cut-reject-configs))]
             [accepting-rules (list->zipper (if (empty? accepting-config)
                                                '()
                                                (map trace-rule (first accepting-config))))]
             [stack (list->zipper (remove-empty (if (empty? accept-computations)
                                                    '()
                                                    (reverse (computation-LoC (first accept-computations))))
                                                '()))]
             [building-state (building-viz-state a-word
                                                 '()
                                                 (map computation-LoC computations)
                                                 stack
                                                 accept-cmps
                                                 reject-cmps
                                                 new-M
                                                 (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w s) #t)) invs) invs) 
                                                 dead-state
                                                 max-cmps)]
             [graphs (create-graph-thunks building-state '())]
             #;[graphs (map first graphs+comp-len)]
             [computation-lens (count-computations a-word (map computation-LoC computations) '())]
             ;[test1 (displayln (format "configs: ~a" configs))]
             #;[test0 (void) #;(begin
                               ;(void)
                               ;(displayln (map second (map config-state-path configs)))
                               (displayln (format "test: ~a" (coallesce (map first (append accept-cmps reject-cmps)))))
                               (displayln (format "test: ~a" (coallesce (map second (append accept-cmps reject-cmps)))))
                               (displayln (format "test: ~a" (coallesce (map third (append accept-cmps reject-cmps)))))
                               )]
             ;[test1 (displayln (format "rejected cmps: ~a" reject-cmps))]
             #;[test2 (displayln (format "hope this works: ~a" (find-num-comps (append accept-cmps reject-cmps)) 
                                       #;(length (coallesce (map first (append accept-cmps reject-cmps))))
                                       ))]
             #;[stack (if (empty? accept-config)
                          '()
                          (list->zipper (first accept-config)))]
             #;[upstck (if (empty? stack)
                           '()
                           (rest stack))]
             #;[pstck (if (empty? upstck)
                          '()
                          (first stack))]
             [inv-configs (map (λ (con)
                                 (length (second (first con))))
                               (return-brk-inv-configs
                                (get-inv-config-results
                                 (make-inv-configs a-word computations)
                                 invs)
                                a-word))])
        ;(struct building-viz-state (upci pci M inv dead))
        ;(struct imsg-state (M upci pci))
        ;;ANCHOR
        ;reject-configs
        ;reject-rules
        ;rejecting-configs
        ;accept-cmps
        ;accept-config
        ;(list (first config) (second config) (third config) (fourth config))
        ;;(length config)
        ;reject-cmps
        ;(map (λ (comp) (reverse (computation-LoR comp))) computations)
        ;(map (λ (comp) (reverse (computation-LoC comp))) computations)
        ;(map (λ (comp) (reverse (computation-LoT comp))) computations)
        ;accept-computations
        ;"done"
        ;reject-cmps
        ;(append rejecting-configs accepting-config)
        ;computation-lens
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
                                                   stack
                                                   accepting-rules
                                                   (list->zipper '()) ;'() #;(if (empty? inv-configs) '() (list->zipper inv-configs))
                                                   0 #;(sub1 (length inv-configs))
                                                   computation-lens
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
                                         ["left" viz-go-prev left-key-pressed]
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
                 'pda-viz))))

(define BUGGY-SAME-NUM-AB (make-ndpda '(K H F M)
                                      '(a b)
                                      '(a b)
                                      'K
                                      '(K)
                                      `(;((K ,EMP ,EMP) (K ,EMP))
                                        ((K a    ,EMP) (H (b)))
                                        ((K b    ,EMP) (F (a)))                                  
                                        ((H a    ,EMP) (H (b)))
                                        ((H ,EMP  (b)) (M ,EMP))                                  
                                        ((F ,EMP  (a)) (M ,EMP))
                                        ((F b    ,EMP) (F (a)))                                  
                                        ;((M a    ,EMP) (H ,EMP))
                                        ((M a    ,EMP) (K ,EMP))
                                        ;((M b    ,EMP) (F ,EMP))
                                        ((M b    ,EMP) (K ,EMP))
                                        )))

(define SAME-NUM-AB (make-ndpda '(K H F M)
                                '(a b)
                                '(a b)
                                'K
                                '(M)
                                `(((K ,EMP ,EMP) (H ,EMP))
                                  ((K ,EMP ,EMP) (F ,EMP))
                                  ((H a ,EMP) (H (b)))
                                  ((H b (b)) (F ,EMP))
                                  ((H ,EMP ,EMP) (M ,EMP))
                                  ((F b ,EMP) (F (a)))
                                  ((F a (a)) (H ,EMP))
                                  ((F ,EMP ,EMP) (M ,EMP)))))


(define P (make-ndpda '(S A B X)
                      '(a b)
                      '(b)
                      'S
                      '(X)
                      `(((S ε ε)(A ε)) ((S ε ε)(X ε)) ((S a ε)(S (b b)))
                                       ((A b ε)(A ε)) ((A a ε)(A ε)) ((A ε ε)(B ε))
                                       ((B ε ε)(A ε))
                                       ((X b (b b))(X ε)) ((X b (b))(X ε)))))

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

;(pda-viz P '(a b))

#;(pda-viz a* '(a a a a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (= (length w) 3)) (empty? s)))))
#;(pda-viz aa* '(a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (empty? w) (empty? s)))))
#;(pda-viz a* '(a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s)))))

#;(pda-viz a* '(a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                  a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s)))))

"note to self:"
"edit A and D to scroll thru word, not jump to end"
"transform funct to use trace and check visited using the trace i.e config and rule used"

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





(define (break-up-configs2 a-word a-LoT acc)
  (define (get-config a-word)
    (remove-duplicates (append-map (λ (traces)
                                     (filter (λ (trce)
                                               (equal? a-word (second (trace-config trce))))
                                             traces))
                                   a-LoT)))
    (if (empty? a-word)
        (reverse (cons (get-config a-word) acc))
        (break-up-configs2 (rest a-word) a-LoT (cons (get-config a-word) acc))))


#;'(((S (a a b b b b) ()) (A (a a b b b b) ()) (B (a a b b b b) ()) (X (a a b b b b) ()))

    
  ((S (a b b b b) (b b)) (A (a b b b b) (b b)) (B (a b b b b) (b b)) (X (a b b b b) (b b)) (A (a b b b b) ()) (B (a b b b b) ()) (ds (a b b b b) ()))

  
  ((A (b b b b) (b b b b)) (S (b b b b) (b b b b)) (B (b b b b) (b b b b)) (A (b b b b) (b b)) (B (b b b b) (b b)) (ds (b b b b) (b b))
                           (ds (b b b b) (b)) (ds (b b b b) ()) (A (b b b b) ()) (B (b b b b) ()) (X (b b b b) (b b b b)))

  
  ((A (b b b) (b b b b)) (B (b b b) (b b b b)) (ds (b b b) (b b b b)) (ds (b b b) (b b b)) (ds (b b b) (b b))(ds (b b b) (b)) (ds (b b b) ())
   (A (b b b) (b b)) (B (b b b) (b b)) (A (b b b) ()) (B (b b b) ()) (X (b b b) (b b b)) (X (b b b) (b b)))

  
  ((A (b b) (b b b b)) (B (b b) (b b b b)) (ds (b b) (b b b b)) (ds (b b) (b b b))  (ds (b b) (b b)) (ds (b b) (b)) (ds (b b) ())
   (A (b b) (b b)) (B (b b) (b b)) (A (b b) ())  (B (b b) ()) (X (b b) (b b)) (X (b b) (b)) (X (b b) ()))

  
  ((B (b) (b b b b)) (A (b) (b b b b)) (ds (b) (b b b b)) (ds (b) (b b b)) (ds (b) (b b)) (ds (b) (b)) (ds (b) ())
   (B (b) (b b)) (A (b) (b b)) (A (b) ()) (B (b) ()) (X (b) (b))  (X (b) ()))

  
  ((ds () ()) (ds () (b)) (ds () (b b)) (ds () (b b b)) (ds () (b b b b)) (B () (b b b b)) (A () (b b b b))
              (B () (b b)) (A () (b b))  (B () ()) (A () ()) (X () ())))