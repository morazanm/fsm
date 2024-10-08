#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         2htdp/image
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
         "david-imsg-state.rkt")

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
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rules) #:transparent)

#|
A rule is a structure:
(make-rule triple pair)
triple is the first of the pda rule
pair is the second of the pda rule
|#
(struct rule (triple pair) #:transparent)

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))


(struct config-fold-state (config a-word stack lor path limit visited) #:transparent)
;;(listof symbols) (listof rules) symbol -> (listof configurations)
;;Purpose: Returns all possible configurations from start that consume the given word
(define (get-configs a-word lor start max-cmps)
  (let (;;(listof configurations)
        ;;Purpose: All configurations from the start state
        [starting-configs (list (append (list start) (list a-word) (list '())))])
    ;(config a-word stack lor path limit)
    ;(displayln (format "actual lor: ~s" lor))
    (remove-duplicates
     (let* ([new-hash (make-custom-hash
                       (λ (x y)
                         (and (equal? (trace-config x) (trace-config y))
                              (equal? (trace-rules x) (trace-rules y)))))]
            [res (make-configs (list (config-fold-state
                                      (trace (first starting-configs) '())
                                      a-word
                                      (third (first starting-configs))
                                      lor
                          
                                      (list (first starting-configs))
                                      max-cmps
                                      (begin
                                        (dict-set! new-hash (trace (first starting-configs) '()) 1)
                                        new-hash)))
                              
                               '()
                               )])
       
       res)
     )))

;;configuration (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Explores all possible configurations using the given word, (listof rules), and visited
;;Visited = All configurations of the consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs configs accum)
  (if (empty? configs)
      accum
      (let* ([config (first configs)]
             [connected-read-rules (if (empty? (config-fold-state-a-word config))
                                       '()
                                       (begin
                                         ;(displayln "here")
                                         ;(displayln (format "lor:~s" (config-fold-state-lor config)))
                                         (filter (λ (rule)
                                                   (and (equal? (first (first rule)) (first (trace-config (config-fold-state-config config))))
                                                        (equal? (second (first rule)) (first (second (trace-config (config-fold-state-config config)))))))
                                                 (config-fold-state-lor config))
                                         )
                                       )]
             ;;(listof rules)
             ;;Purpose: Holds all rules that can pop what is in the stack
             [connected-pop-rules (begin
                                    ;(displayln (format "connected-read-rles: ~s" connected-read-rules))
                                    (filter (λ (rule)
                                              (or (equal? (third (first rule)) EMP)
                                                  (and (>= (length (config-fold-state-stack config)) (length (third (first rule))))
                                                       (equal? (take (config-fold-state-stack config) (length (third (first rule)))) (third (first rule))))))
                                            connected-read-rules)
                                    )]
             ;;(listof configurations)
             ;;Purpose: Creates the preliminary configurations using the connected-pop-rules
             [prelim-config (map (λ (rule)
                                   (append (list (first (second rule)))
                                           (list (rest (config-fold-state-a-word config)))
                                           (list (if (eq? (third (first rule)) EMP) (config-fold-state-stack config) (drop (config-fold-state-stack config) (length (third (first rule))))))))
                                 connected-pop-rules)]
             ;;(listof configurations)
             ;;Purpose: Creates the configurations using the connected-pop-rules and preliminary configurations
             [new-configs (begin
                            ;(displayln (format "prelim-cnfig:~s" prelim-config))
                            (filter (λ (config0) (not (dict-ref (config-fold-state-visited config) config0 #f)) #;(not (member? config visited)))
                                    (map (λ (rule config)
                                           (if (eq? (second (second rule)) EMP)
                                               (trace config rule)
                                               (trace (append (list (first config))
                                                       (list (second config))
                                                       (list (append (second (second rule)) (third config))))
                                                      rule)))
                                         connected-pop-rules
                                         prelim-config))
                            )]
             ;;(listof rules)
             ;;Purpose: Holds all rules that consume no input for the given configurations
             [connected-read-E-rules (filter (λ (rule)
                                               (and (equal? (first (first rule)) (first (trace-config (config-fold-state-config config))))
                                                    (equal? (second (first rule)) EMP)))
                                             (config-fold-state-lor config))]
             ;;(listof rules)
             ;;Purpose: Holds all rules that can pop what is in the stack
             [connected-pop-E-rules (filter (λ (rule)
                                              (or (equal? (third (first rule)) EMP)
                                                  (and (>= (length (config-fold-state-stack config)) (length (third (first rule))))
                                                       (equal? (take (config-fold-state-stack config) (length (third (first rule)))) (third (first rule))))))
                                            connected-read-E-rules)]
             ;;(listof configurations)
             ;;Purpose: Creates the preliminary e-tran configurations using the connected-pop-E-rules
             [prelim-E-config (map (λ (rule)
                                     (append (list (first (second rule)))
                                             (list (config-fold-state-a-word config)) 
                                             (list (if (eq? (third (first rule)) EMP) (config-fold-state-stack config) (drop (config-fold-state-stack config) (length (third (first rule))))))))
                                   connected-pop-E-rules)]
             ;;(listof configurations)
             ;;Purpose: Creates the configurations using the connected-pop-rules and preliminary configurations
             [new-E-configs (filter (λ (config0) (not (dict-ref (config-fold-state-visited config) config0 #f)))
                                    (map (λ (rule config)
                                           (if (eq? (second (second rule)) EMP)
                                               (trace config '())
                                               (trace (append (list (first config))
                                                       (list (second config))
                                                       (list (append (second (second rule)) (third config))))
                                                      rule)))
                                         connected-pop-E-rules
                                         prelim-E-config))])
        
        
        (cond [(or (> (length (config-fold-state-path config)) (config-fold-state-limit config)) (and (empty? new-configs) (empty? new-E-configs)))
               (make-configs (rest configs) (append accum (list (config-fold-state-path config))))]
              [(and (empty? new-configs) (not (empty? new-E-configs)))
               (make-configs (append (rest configs) (map (lambda (x) (struct-copy config-fold-state config
                                                                                  [config x]
                                                                                  [stack (third (trace-config x))]
                                                                                  [path (append (config-fold-state-path config)
                                                                                                (list x))]
                                                                                  [visited (foldr (lambda (x r visit)
                                                                                                    (begin
                                                                                                      (dict-set! visit (trace x r) 1)
                                                                                                      visit))
                                                                                                  (config-fold-state-visited config)
                                                                                                  connected-pop-E-rules
                                                                                                  new-E-configs)]))
                                                         new-E-configs))
                             accum
                             )
               ]
              [(and (not (empty? new-configs)) (empty? new-E-configs))
               (make-configs (append (rest configs) (map (lambda (x) (struct-copy config-fold-state config
                                                                                  [config x]
                                                                                  [stack (third (trace-config x))]
                                                                                  [a-word (rest (config-fold-state-a-word config))]
                                                                                  [path (append (config-fold-state-path config)
                                                                                                (list x))]
                                                                                  [visited (foldr (lambda (x r visit)
                                                                                                    (begin
                                                                                                      (dict-set! visit (trace x r) 1)
                                                                                                      visit))
                                                                                                  (config-fold-state-visited config)
                                                                                                  connected-pop-rules
                                                                                                  new-configs)]))
                                                         new-configs))
                             
                             accum)
               ]
              [else
               (make-configs (append (rest configs)
                                     (map (lambda (x) (struct-copy config-fold-state config
                                                                   [config x]
                                                                   [stack (third (trace-config x))]
                                                                   [path (append (config-fold-state-path config)
                                                                                 (list x))]
                                                                   [visited (foldr (lambda (x r visit)
                                                                                     (begin
                                                                                                      (dict-set! visit (trace x r) 1)
                                                                                                      visit))
                                                                                   (config-fold-state-visited config)
                                                                                   connected-pop-E-rules
                                                                                   new-E-configs)]))
                                          new-E-configs)
                                     (map (lambda (x) (struct-copy config-fold-state config
                                                                   [config x]
                                                                   [stack (third (trace-config x))]
                                                                   [a-word (rest (config-fold-state-a-word config))]
                                                                   [path (append (config-fold-state-path config)
                                                                                 (list x))]
                                                                   [visited
                                                                    (foldr (lambda (x r visit)
                                                                             (begin
                                                                                                      (dict-set! visit (trace x r) 1)
                                                                                                      visit))
                                                                           (config-fold-state-visited config)
                                                                           connected-pop-rules
                                                                           new-configs)]
                                                                   )) new-configs)
                                     )
                             
                             accum
                             )
               ]))   
      ))

;;(listof configurations) (listof symbols) (listof rule) (listof rules) -> (listof rules)
;;Purpose: Attaches the transition rules used between configurations
(define (attach-rules->configs configs word lor acc)
  (cond [(<= (length configs) 1) (reverse acc)]
        [(equal? (second (first configs)) (second (second configs)))
         (local [(define (split-path rule)
                   (if (equal? (second (second rule)) EMP)
                       (second (second rule))
                       (take (third (second configs)) (length (second (second rule))))))
                 (define e-rule-used (filter (λ (rule)
                                               (and (equal? (first (first rule)) (first (first configs)))
                                                    (equal? (second (first rule)) EMP)
                                                    (equal? (first (second rule)) (first (second configs)))
                                                    (split-path rule)
                                                    #;(equal? (second (second rule)) (take (third (second configs)) (split-path rule)))))
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
                                                       (split-path rule)
                                                       ;(equal? (second (second rule)) (take (third (second configs)) (split-path rule)))
                                                       #;(equal? (second (second rule)) (take (third (second configs)) (length (second (second rule)))))))
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
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) rle)])
                (make-trace (rest word) (rest configs) (rest rules) (cons res acc)))]))


(define (make-acc-trace word configs rules acc)
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first (first rules))) EMP)))
         (let ([res (trace (first configs) '())])
           (make-acc-trace (rest word) (rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (equal? (second (first (first rules))) EMP))
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (struct-copy trace (first acc)
                                  [rules (cons rle (trace-rules (first acc)))])])
           (make-acc-trace word configs (rest rules) (cons res (rest acc))))]
        [(empty? word)
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (trace (first configs)
                            (list rle))])
           (make-acc-trace word (rest configs) (rest rules) (cons res acc)))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs)
                                 (list rle))])
                (make-acc-trace (rest word) (rest configs) (rest rules) (cons res acc)))]))


;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word configs)
  (if (empty? configs)
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
  (if (or (empty? inv-configs)
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
        [(not (ormap (λ (config) (empty? (second (last config))))
                     (get-configs a-word (pda-getrules M) (pda-getstart M) max-cmps)))
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
  (or (member? rule lor)
      (ormap (λ (p)
               (and (equal? (first rule) (first p))
                    (or (equal? (third rule) (third p))
                        (and (equal? (third rule) (third p))
                             (equal? (third rule) dead)))))
             lor)))

(define (rule-empty? a-rule)
  (equal? (second (rule-triple a-rule)) EMP))

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

(define (zipper-search zip word zip-func)
  (cond [(equal? (second (zipper-current zip)) word) zip]
        [else (zipper-search (zip-func zip) word zip-func)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M dead held-inv fail-inv)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (pda-getstart M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv) (member? state fail-inv)) 'wedged]
                                              [(or (member? state held-inv) (member? state fail-inv)) 'filled]
                                              [else 'solid])
                                 'shape (if (member? state (pda-getfinals M)) 'doublecircle 'circle)
                                 'fillcolor (cond [(and (member? state held-inv) (member? state fail-inv))
                                                   "red:green"]
                                                  [(member? state held-inv) HELD-INV-COLOR ]
                                                  [(member? state fail-inv) BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 'fontcolor 'black
                                 'fontname (if (and (member? state held-inv) (member? state fail-inv))
                                               "times-bold"
                                               "Times-Roman"))))
         dgraph
         (pda-getstates M)))

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
                                              [(member? rule current-a-rules) 'bold]
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
         [r-config (if (empty? (building-viz-state-reject-configs a-vs))
                       (building-viz-state-reject-configs a-vs)
                       (map (λ (configs) (trace-rules (first configs)))
                                   (filter (λ (configs)
                                             (not (empty? configs))) (building-viz-state-reject-configs a-vs))))]

         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [curr-r-config (map (λ (rule) (list (rule-triple rule)
                                             (rule-pair rule))) r-config)]

         ;;(listof rules)
         ;;Purpose: The current rules that the pda is using to consume the CI
         [current-rules (configs->rules curr-r-config)]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [a-configs (if (empty? (building-viz-state-accept-configs a-vs))
                        (building-viz-state-accept-configs a-vs)
                        (map (λ (configs) (trace-rules (first configs))) (building-viz-state-accept-configs a-vs)))]
         
         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [curr-a-config (map (λ (rule) (list (rule-triple rule)
                                             (rule-pair rule))) a-configs)]
                  
         ;;(listof rules)
         ;;Purpose: The current rules of the accepting computations that the pda is using to consume the CI
         [current-a-rules (configs->rules curr-a-config)]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (pda-getrules (building-viz-state-M a-vs)))]
         
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
         [destin-states (if (not (empty? (building-viz-state-pci a-vs)))
                            (remove-duplicates current-config)
                            (cons (pda-getstart (building-viz-state-M a-vs)) (remove-duplicates current-config)))])
    
    (list (make-edge-graph
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
        [(not (ormap (λ (config) (and (empty? (second (trace-config (last config))))
                                      (empty? (third (trace-config (last config))))))
                     (get-configs (building-viz-state-pci a-vs)
                                  (pda-getrules (building-viz-state-M a-vs))
                                  (pda-getstart (building-viz-state-M a-vs))
                                  (building-viz-state-max-cmps a-vs))))
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
                                                  [stack (if (list? (building-viz-state-stack a-vs))
                                                             (building-viz-state-stack a-vs)
                                                             (zipper-next (building-viz-state-stack a-vs)))]
                                                  [accept-configs (filter (λ (configs)
                                                                            (not (empty? configs)))
                                                                          (map rest (building-viz-state-accept-configs a-vs)))]
                                                  [reject-configs (filter (λ (configs)
                                                                            (not (empty? configs)))
                                                                          (map (λ (c)
                                                                                 (if (empty? c)
                                                                                     c
                                                                                     (rest c)))
                                                                                 (building-viz-state-reject-configs a-vs)))])
                                     (cons next-graph acc)))]))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (and (empty? (second (trace-config (last config))))
                                                    (empty? (third (trace-config (last config))))))
                                   (get-configs (imsg-state-pci imsg-st)
                                                (pda-getrules (imsg-state-M imsg-st))
                                                (pda-getstart (imsg-state-M imsg-st))
                                                (imsg-state-max-cmps imsg-st)))]
         
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
         [current-stack (if (list? (imsg-state-upstck imsg-st))
                            (imsg-state-pstck imsg-st)
                            (third (zipper-current (imsg-state-upstck imsg-st))))])
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
                      (if (equal? (apply-pda (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept)
                          (text (format "~a" EMP) 20 'gray)
                          (text (format "~a" EMP) 20 'red)))
              (beside (text "Consumed: " 20 'black)
                      (if (equal? (apply-pda (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept)
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
      (cond [(list? (imsg-state-upstck imsg-st)) (text "aaaa" 20 'white)]
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
       (text (begin
               ;; Pretty sure this can just be moved along in lockstep of the visualization? -andres
               ;(displayln (format "comps: ~a" (imsg-state-comps imsg-st)))
               ;(displayln (format "pci: ~a" (imsg-state-pci imsg-st)))
               (format "The current number of possible computations is ~a. "
                     (number->string (list-ref (imsg-state-comps imsg-st)
                                               (length (imsg-state-pci imsg-st)))))
               )
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
                   (equal? (apply-pda (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept))
              (text "There is a computation that accepts." 20 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (or (list? (imsg-state-upstck imsg-st))
                       (zipper-at-end? (imsg-state-upstck imsg-st)))
                   (equal? (apply-pda (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'reject))
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

(define E-SCENE (empty-scene 1250 600))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config)
                                     (and (empty? (second (last config)))
                                          (empty? (third (last config)))))
                                   (get-configs (imsg-state-pci (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))) 
                                                (pda-getrules (imsg-state-M (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs))))
                                                (pda-getstart (imsg-state-M (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs))))
                                                (imsg-state-max-cmps (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))]
         [prules (if (or (list? (imsg-state-pstck (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                         (zipper-at-end? (imsg-state-pstck (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))
                     (imsg-state-pstck (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))
                     (zipper-next (imsg-state-pstck (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))))]
         [prule (cond [(list? (imsg-state-pstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))
                       (imsg-state-pstck (informative-messages-component-state
                                          (viz-state-informative-messages a-vs)))]
                      [(empty? (zipper-current (imsg-state-pstck (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))
                       (zipper-current (imsg-state-pstck (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))]
                      [else (second (rule-triple (first (zipper-current prules))))])]
         [pci (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (equal? prule EMP)
                      (not completed-config?))
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
                     [upci (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                   (equal? prule EMP)
                                   (not completed-config?))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [upstck 
                      (if (or (list? (imsg-state-upstck (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                              (zipper-at-end? (imsg-state-upstck (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))
                          (imsg-state-upstck (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))
                          (zipper-next (imsg-state-upstck (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))))]
                     [pstck prules]
                     [invs-zipper (cond [(list? (imsg-state-invs-zipper (informative-messages-component-state
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
         [zip (if (list? (imsg-state-invs-zipper (informative-messages-component-state
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
         [upstck (if (or (list? (imsg-state-upstck (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))
                         (zipper-at-end? (imsg-state-upstck (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))
                     (imsg-state-upstck (informative-messages-component-state
                                         (viz-state-informative-messages a-vs)))
                     (zipper-to-end (imsg-state-upstck (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))))]
         
         [pstck (if (or (list? (imsg-state-pstck (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))
                        (zipper-at-end? (imsg-state-pstck (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))))
                    (imsg-state-pstck (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))
                    (zipper-to-end (imsg-state-pstck (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))))]
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([prules (if (or (list? (imsg-state-pstck (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                         (zipper-at-begin? (imsg-state-pstck (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))
                     (imsg-state-pstck (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))
                     (zipper-prev (imsg-state-pstck (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))))]
         [prule (cond [(list? (imsg-state-pstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))
                       (imsg-state-pstck (informative-messages-component-state
                                          (viz-state-informative-messages a-vs)))]
                      [(empty? (zipper-current (imsg-state-pstck (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))
                       (zipper-current (imsg-state-pstck (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))]
                      [else (second (rule-triple (first (zipper-current (imsg-state-pstck (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs)))))))])]
         [pci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))
                      (equal? prule EMP))
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
                                   (equal? prule EMP))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [upstck (cond [(list? (imsg-state-upstck (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
                                    (imsg-state-upstck (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))]
                                   [(zipper-at-begin? (imsg-state-upstck (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                    (imsg-state-upstck (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))]
                                   [else (zipper-prev (imsg-state-upstck (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))])]
                     [pstck prules]
                     [invs-zipper (cond [(list? (imsg-state-invs-zipper (informative-messages-component-state
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
                   [upstck (cond [(list? (imsg-state-upstck (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                  (imsg-state-upstck (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))]
                                 [(zipper-at-begin? (imsg-state-upstck (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                  (imsg-state-upstck (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))]
                                 [else (zipper-to-begin (imsg-state-upstck (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))])]
                   [pstck (if (or (list? (imsg-state-pstck (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                  (zipper-at-begin? (imsg-state-pstck (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                              (imsg-state-pstck (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                              (zipper-to-begin (imsg-state-pstck (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))]
                   [invs-zipper (if (list? (imsg-state-invs-zipper (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
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
  (if (or (list? (imsg-state-invs-zipper (informative-messages-component-state
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
  (if (or (list? (imsg-state-invs-zipper (informative-messages-component-state
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
          (define dead (if (member? DEAD (pda-getstates M)) (gen-state (pda-getstates M)) DEAD))
          ;;(listof symbols)
          ;;Purpose: Makes partial rules for every combination of states in M and symbols in sigma of M
          (define new-read-rules
            (for*/list ([states (pda-getstates M)]
                        [sigma (pda-getalphabet M)])
              (list states sigma)))
        
          ;;(listof rules)
          ;;Purpose: Makes rules for that empty the stack and transition to the ds
          (define dead-pop-rules
            (for*/list ([ds (list dead)]
                        [gamma (pda-getgamma M)])
              (list (list ds EMP (list gamma)) (list ds EMP))))
        
          ;;(listof rules)
          ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
          (define dead-read-rules
            (for*/list ([ds (list dead)]
                        [sigma (pda-getalphabet M)])
              (list (list ds sigma EMP) (list ds EMP))))
          ;;(listof rules)
          ;;Purpose: Gets rules that are not currently in the original rules of M
          (define get-rules-not-in-M  (local [(define partial-rules (map (λ (rule)
                                                                           (list (first (first rule)) (second (first rule))))
                                                                         (pda-getrules M)))]
                                        (filter (λ (rule)
                                                  (not (member? rule partial-rules)))
                                                new-read-rules)))
          ;;(listof rules)
          ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
          (define rules-to-dead
            (map (λ (rule) (cons (append rule (list EMP)) (list (list dead EMP))))
                 get-rules-not-in-M))]
    (make-unchecked-ndpda (append (pda-getstates M) (list dead))
                          (pda-getalphabet M)
                          (pda-getgamma M)
                          (pda-getstart M)
                          (pda-getfinals M)
                          (append (pda-getrules M) rules-to-dead dead-read-rules dead-pop-rules))))


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

(define (extend-lst lst len)
    (append lst (make-list (- len (length lst)) '())))

(define (form-traces traces)
  (let ([max-len (apply max (map length traces))])
    (map (lambda (x) (extend-lst x max-len)) traces)))

(define (until-last-empty traces acc)
  (if (empty? traces)
      (lambda (x) (list-ref x acc))
      (if (rule-empty? (trace-rules (first traces)))
      (until-last-empty (rest traces) (add1 acc))
      (lambda (x) (list-ref x acc)))
      ))

(define (until-last-empty-idx traces acc)
  (if (empty? traces)
      acc
      (if (rule-empty? (trace-rules (first traces)))
      (until-last-empty-idx (rest traces) (add1 acc))
      acc)))
(define (find-num-comps traces)
  (define max-len (apply max (map length traces)))
  (define (find-num-comps-helper traces)
    (if (andmap empty? traces)
        '()
        (cons (length (coallesce (map (lambda (x) (if (empty? x)
                                                      '()
                                                      (if (rule-empty? (trace-rules (first x)))
                                                      ((until-last-empty (rest x) 0) x)
                                                      (first x)))
                                                      ) #;(lambda (x) (list-ref x curr-len)) traces)))
              (find-num-comps-helper (map (lambda (x) (if (empty? x)
                                                          '()
                                                          (if (rule-empty? (trace-rules (first x)))
                                                          ((lambda (x) (drop x (until-last-empty-idx (rest x) 1))) x)
                                                          (rest x)
                                                          )
                                                          ))
                                          traces)
                                          ))
        )
    )
 (find-num-comps-helper traces #;(form-traces traces)))
                                 

(define (coallesce traces)
  (define (rule-equal? x y)
    (if (empty? x)
        (empty? y)
        (and (not (empty? y))
             (equal? (rule-triple x) (rule-triple y))
             (equal? (rule-pair x) (rule-pair y)))))
  (define visited (make-custom-hash (lambda (x y) (and (equal? (trace-config x) (trace-config y))
                                                       (rule-equal? (trace-rules x) (trace-rules y))))))
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
(define (pda-viz M a-word #:add-dead [add-dead #f] #:max-cmps [max-cmps 20] . invs)
  (if (not (equal? (M 'whatami) 'pda))
      (error "The given machine must be a pda.")
      (let* ([new-M (if add-dead (make-new-M M) M)]
             [dead-state (if add-dead (last (pda-getstates new-M)) 'no-dead)]
             [configs (map rest (get-configs a-word (pda-getrules new-M) (pda-getstart new-M) max-cmps))]
             ;[test-1 (displayln (format "accepting cmps: ~a" configs))]
             [rc (map (λ (c) (map trace-config c)) configs)]
             ;[test- (displayln "")]
             ;[test-2 (displayln (format "accepting cmps: ~a" rc))]
             [rtc (map (λ (c) (cons (list (pda-getstart new-M) (list a-word) '()) c)) rc)]
             [accept-config (filter (λ (config)
                                      (and (member? (first (last config)) (pda-getfinals new-M))
                                           (empty? (second (last config)))
                                           (empty? (third (last config)))))
                                    rtc)]
             [accept-rules (map (λ (configs)
                                  (attach-rules->configs configs a-word (pda-getrules new-M) '()))
                                accept-config)]
             [accepting-config (map (λ (config rules)
                                      (make-trace a-word config rules '() #:accept #t))
                                    accept-config
                                    accept-rules)]
             ;; WTF? what does word length have to do with max cmps? how can a cutoff be accepting? -andres
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
                                       (not (member? config accept-config)))
                                     rtc)]
             [reject-rules (map (λ (config)
                                  (attach-rules->configs config a-word (pda-getrules new-M) '()))
                                reject-configs)]
             [rejecting-configs (map (λ (configs rules)
                                       (make-trace a-word configs rules '()))
                                     reject-configs
                                     reject-rules)]
             [cut-reject-configs (if (> (length a-word) max-cmps)
                                     (map last rejecting-configs)
                                     '())]
             [reject-cmps (if (empty? cut-reject-configs)
                              rejecting-configs
                              (map (λ (configs last-reject)
                                     (append configs (list last-reject)))
                                   rejecting-configs
                                   cut-reject-configs))]
             [stack (if (empty? accept-config)
                        '()
                        (list->zipper (first accept-config)))]
             [accepting-rules (if (empty? accept-config)
                                  '()
                                  (list->zipper (map trace-rules (first accepting-config))))]
             [test0 (displayln (format "accepting cmps: ~a" accept-cmps))]
             [test1 (displayln (format "rejected cmps: ~a" reject-cmps))]
             [test2 (displayln (format "hope this works: ~a" (find-num-comps (append accept-cmps reject-cmps)) 
                                       #;(length (coallesce (map first (append accept-cmps reject-cmps))))
                                       ))]
             [building-state (building-viz-state a-word
                                                 '()
                                                 rtc
                                                 stack
                                                 accept-cmps
                                                 reject-cmps
                                                 new-M
                                                 (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w s) #t)) invs) invs) 
                                                 dead-state
                                                 max-cmps)]
             [graphs+comp-len (create-graph-thunks building-state '())]
             [graphs (map first graphs+comp-len)]
             [computation-lens (map second graphs+comp-len)]
             [inv-configs (map (λ (con)
                                 (length (second (first con))))
                               (return-brk-inv-configs
                                (get-inv-config-results
                                 (make-inv-configs a-word rtc)
                                 invs)
                                a-word))])
        ;;ANCHOR
        
        ;accept-config
        ;accept-rules
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
                                                   (if (empty? inv-configs) '() (list->zipper inv-configs))
                                                   (sub1 (length inv-configs))
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



(define BUGGY-SAME-NUM-AB (make-unchecked-ndpda '(K H F M)
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

(define SAME-NUM-AB (make-unchecked-ndpda '(K H F M)
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

(define P (make-unchecked-ndpda '(S A B X)
                                '(a b)
                                '(b)
                                'S
                                '(X)
                                `(((S ε ε)(A ε)) ((S ε ε)(X ε)) ((S a ε)(S (b b)))
                                                 ((A b ε)(A ε)) ((A a ε)(A ε)) ((A ε ε)(B ε))
                                                 ((B ε ε)(A ε))
                                                 ((X b (b b))(X ε)) ((X b (b))(X ε)))))

(define aˆnbˆn (make-unchecked-ndpda '(S M F)
                                     '(a b)
                                     '(a)
                                     'S
                                     '(F)
                                     `(((S ,EMP ,EMP) (M ,EMP))
                                       ((S a ,EMP) (S (a)))
                                       ((M b (a)) (M ,EMP))
                                       ((M ,EMP ,EMP) (F ,EMP)))))

(define n (make-unchecked-ndpda '(K H F M I)
                                '(a b)
                                '(a b)
                                'K
                                '(I)
                                `(((K b ,EMP)(H ,EMP))
                                  ((H ,EMP ,EMP)(F ,EMP))
                                  ((H ,EMP ,EMP)(M ,EMP))
                                  ((F a ,EMP)(I ,EMP))
                                  ((M a ,EMP)(I ,EMP)))))

(define nk (make-unchecked-ndpda '(K H F M I)
                                 '(a b)
                                 '(a b)
                                 'K
                                 '(I)
                                 `(((K b ,EMP)(H ,EMP))
                                   ((H ,EMP ,EMP)(F ,EMP))
                                   ((F ,EMP ,EMP)(M ,EMP))
                                   ((M ,EMP ,EMP)(I ,EMP))
                                   ((I ,EMP ,EMP)(H ,EMP)))))

(define a* (make-unchecked-ndpda '(K H)
                                 '(a b)
                                 '(a)
                                 'K
                                 '(H)
                                 `(((K ,EMP ,EMP)(H ,EMP))
                                   ((H a ,EMP)(H ,EMP)))))

(define aa* (make-unchecked-ndpda '(K H)
                                  '(a b)
                                  '(a)
                                  'K
                                  '(H)
                                  `(((K a ,EMP)(H ,EMP))
                                    ((H a ,EMP)(H ,EMP)))))

(define inf-a (make-unchecked-ndpda '(K)
                                    '(a)
                                    '(a)
                                    'K
                                    '(K)
                                    `(((K ,EMP ,EMP) (K, '(a))))))

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
(define more-a-than-b (make-unchecked-ndpda '(S A)
                                            '(a b)
                                            '(a)
                                            'S
                                            '(A)
                                            `(((S a ,EMP) (S (a)))
                                              ((S ,EMP ,EMP) (A ,EMP))
                                              ((A b (a)) (A ,EMP))
                                              ((A ,EMP (a)) (A ,EMP)))))
#;(pda-viz more-a-than-b '(a a a a a b b))
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
"update trace to proccess emptys individually"

(define pd (make-unchecked-ndpda '(S A)
                                 '(a b)
                                 '(a b)
                                 'S
                                 '(A)
                                 `(((S a ,EMP) (A (a)))
                                   ((S a ,EMP) (A (b))))))

(define (pd-A-INV a-wrd a-stck)
  (andmap (λ (s) (eq? s 'a)) a-stck))

#;(define more-a-than-b (make-unchecked-ndpda '(S A)
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

        
#;'(
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b b) (a a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b b) (a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b b) (a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A b (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b b) (a a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b b) (a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A (b) (a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A b (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b b) (a a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b b) (a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () (a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A ε (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b b) (a a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A (b) (a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A b (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b b) (a a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A (b) (a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () (a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A ε (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b b) (a a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () (a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () (a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A ε (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b) (a a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A (b) (a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A b (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b) (a a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A (b) (a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A (b) (a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () (a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A ε (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b) (a a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A (b) (a a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () (a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () (a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A ε (a)) (A ε))))
  (#(struct:trace (S ((a a a a a b b)) ()) #(struct:rule (ε ε ε) (ε ε))) #(struct:trace (S (a a a a b b) (a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a a b b) (a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a a b b) (a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (a b b) (a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (S (b b) (a a a a a)) #(struct:rule (S a ε) (S (a)))) #(struct:trace (A (b b) (a a a a a)) #(struct:rule (S ε ε) (A ε))) #(struct:trace (A (b) (a a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () (a a a)) #(struct:rule (A b (a)) (A ε))) #(struct:trace (A () (a a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () (a)) #(struct:rule (A ε (a)) (A ε))) #(struct:trace (A () ()) #(struct:rule (A ε (a)) (A ε))))
  )