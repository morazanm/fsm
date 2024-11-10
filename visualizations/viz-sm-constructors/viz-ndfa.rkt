#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base [make-color loc-make-color] [make-pen loc-make-pen])
         2htdp/image
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         math/matrix
         "../../fsm-core/interface.rkt")

(define FNAME "fsm")

(define HELD-INV-COLOR 'chartreuse4)
(define BRKN-INV-COLOR 'red2)

(define E-SCENE-WIDTH 1250)
(define E-SCENE-HEIGHT 500)
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

;; num num num num boolean num num num (matrix [ [x] [y] [1] ]) -> (matrix [ [transformed-x] [transformed-y] [1] ])
;; Transforms a given point matrix based on the arguments provided
(define (affine-transform #:x-translate [x-translate 0]
                          #:y-translate [y-translate 0]
                          #:x-scale [x-scale 1]
                          #:y-scale [y-scale 1]
                          #:reflect [reflect #f]
                          #:rotate [rotate 0]
                          #:x-shear [x-shear 0]
                          #:y-shear [y-shear 0]
                          #:point point)
  (let* ([reflection (if reflect -1 1)]
         [result (matrix* (matrix [[(* reflection x-scale (cos rotate))
                                    (* x-shear (* -1 (sin rotate)))
                                    x-translate]
                                   [(* (sin rotate) y-shear) (* y-scale (cos rotate)) y-translate]
                                   [0 0 1]])
                          point)])
    result))

;; img posn num>0 -> matrix x y 1
;; Calculates the transform needed to zoom correctly
(define (zoom-affine-transform img img-posn scale)
  (let* ([transformed-x (* -1
                           (+ (- (/ E-SCENE-WIDTH 2) (+ (posn-x img-posn) (/ (image-width img) 2)))
                              (/ (image-width img) 2)))]
         [transformed-y (* -1
                           (+ (- (/ E-SCENE-HEIGHT 2) (+ (posn-y img-posn) (/ (image-height img) 2)))
                              (/ (image-height img) 2)))])
    (affine-transform #:x-translate (* -1 transformed-x)
                      #:y-translate (* -1 transformed-y)
                      #:point
                      (affine-transform #:x-scale scale
                                        #:y-scale scale
                                        #:point (affine-transform #:x-translate transformed-x
                                                                  #:y-translate transformed-y
                                                                  #:point (matrix [[0] [0] [1]]))))))

;; img num>0 -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define (calculate-viewport-limits scaled-image scale)
  (let* ([img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale))]
         [img-height-node-diff (- (/ (image-height scaled-image) 2) (* NODE-SIZE scale))]
         [scaled-node-size (* NODE-SIZE scale)]
         [MIN-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                    (- (* -1 (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH))
                       (- E-SCENE-WIDTH scaled-node-size))
                    (* -1 img-width-node-diff))]
         [MAX-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                    (+ (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH)
                       E-SCENE-WIDTH
                       (- E-SCENE-WIDTH scaled-node-size))
                    (+ E-SCENE-WIDTH img-width-node-diff))]
         [MIN-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                    (- (* -1 (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT))
                       (- E-SCENE-HEIGHT scaled-node-size))
                    (* -1 img-height-node-diff))]
         [MAX-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                    (+ (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT)
                       E-SCENE-HEIGHT
                       (- E-SCENE-HEIGHT scaled-node-size))
                    (+ E-SCENE-HEIGHT img-height-node-diff))])
    (bounding-limits MIN-X MAX-X MIN-Y MAX-Y)))

;; viz-state viewport-limits img num>0 -> viz-state
;; Returns a new viz-state where if the given image would be out of bounds of its viewport limits
;; It is placed into a position inbounds
(define (reposition-out-of-bounds-img a-vs viewport-lims new-img new-scale)
  (cond
    [(outside-west-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (bounding-limits-min-x viewport-lims) (posn-y (viz-state-image-posn a-vs)))]
                  [scale-factor new-scale])]
    [(outside-east-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (bounding-limits-max-x viewport-lims) (posn-y (viz-state-image-posn a-vs)))]
                  [scale-factor new-scale])]
    [(outside-north-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (posn-x (viz-state-image-posn a-vs)) (bounding-limits-min-y viewport-lims))]
                  [scale-factor new-scale])]
    [(outside-south-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (posn-x (viz-state-image-posn a-vs)) (bounding-limits-max-y viewport-lims))]
                  [scale-factor new-scale])]
    [(outside-west-north-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (bounding-limits-min-x viewport-lims) (bounding-limits-min-y viewport-lims))]
                  [scale-factor new-scale])]
    [(outside-west-south-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (bounding-limits-min-x viewport-lims) (bounding-limits-max-y viewport-lims))]
                  [scale-factor new-scale])]
    [(outside-east-north-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (bounding-limits-max-x viewport-lims) (bounding-limits-min-y viewport-lims))]
                  [scale-factor new-scale])]
    [(outside-east-south-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state
                  a-vs
                  [curr-image new-img]
                  [image-posn
                   (posn (bounding-limits-max-x viewport-lims) (bounding-limits-max-y viewport-lims))]
                  [scale-factor new-scale])]
    [(within-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
     (struct-copy viz-state a-vs [curr-image new-img] [scale-factor new-scale])]))

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
(define (zoom a-vs factor)
  (let* ([new-scale (* factor (viz-state-scale-factor a-vs))]
         [scalable? (cond
                      [(eq? factor ZOOM-INCREASE)
                       (> (viz-state-scale-factor-cap a-vs) new-scale)]
                      [(eq? factor ZOOM-DECREASE)
                       (< (viz-state-scale-factor-floor a-vs) new-scale)])])
    (if scalable?
        (let* ([scaled-image (scale new-scale (viz-state-curr-image a-vs))]
               [viewport-lims (calculate-viewport-limits scaled-image new-scale)]
               [scale-increase (/ new-scale (viz-state-scale-factor a-vs))]
               [affine-matrix (zoom-affine-transform (scale (viz-state-scale-factor a-vs)
                                                            (viz-state-curr-image a-vs))
                                                     (viz-state-image-posn a-vs)
                                                     scale-increase)])
          (reposition-out-of-bounds-img (struct-copy viz-state
                                                     a-vs
                                                     [image-posn
                                                      (posn (+ (posn-x (viz-state-image-posn a-vs))
                                                               (matrix-ref affine-matrix 0 0))
                                                            (+ (posn-y (viz-state-image-posn a-vs))
                                                               (matrix-ref affine-matrix 1 0)))]
                                                     [scale-factor new-scale])
                                        viewport-lims
                                        (viz-state-curr-image a-vs)
                                        new-scale))
        a-vs)))

;; viz-state ( Any* -> viz-state) -> viz-state
;; Purpose: Prevents holding down a left or right mouse click from spamming a function too fast
(define (buffer-held-click a-vs func)
  (if (= (viz-state-click-buffer a-vs) 0)
      (func (struct-copy viz-state
                         a-vs
                         [click-buffer 1]
                         [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))
      (if (= (viz-state-click-buffer a-vs))
          (struct-copy viz-state
                       a-vs
                       [click-buffer 0]
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])
          (struct-copy viz-state
                       a-vs
                       [click-buffer (add1 (viz-state-click-buffer a-vs))]
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))))

;; img -> boolean
;; Checks to see if an image needs to be resized
(define (does-img-need-resizing? img)
  (or (< E-SCENE-WIDTH (image-width img)) (< E-SCENE-HEIGHT (image-height img))))
#|
;; viz-state -> viz-state
;; Purpose: Moves the visualization to the next step of the derivation
(define (go-next a-vs)
  (if (zipper-at-end? (viz-state-imgs a-vs))
      a-vs
      (let* ([new-imgs (zipper-next (viz-state-imgs a-vs))]
             [new-curr-img ((zipper-current new-imgs))]
             [curr-pimgs-img ((zipper-current (viz-state-imgs a-vs)))]
             [img-resize (resize-image new-curr-img
                                       (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                       (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
             [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
             [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
        (if (does-img-need-resizing? new-curr-img)
            (let ([NEW-FLOOR (min (second img-resize) (third img-resize))])
              (cond
                [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor DEFAULT-ZOOM-CAP]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]
                [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor NEW-FLOOR]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]
                [else
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]))
            (let ([new-viz-state (struct-copy viz-state
                                              a-vs
                                              [imgs new-imgs]
                                              [curr-image new-curr-img]
                                              [image-posn
                                               (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                     (+ (posn-y (viz-state-image-posn a-vs))
                                                        growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR])])
              (reposition-out-of-bounds-img
               new-viz-state
               (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                          (viz-state-scale-factor a-vs))
               new-curr-img
               (viz-state-scale-factor a-vs)))))))
|#
;;viz-state -> viz-state
;;Purpose: Jumps the visulization to next broken invariant
(define (jump-next a-vs)
  (if (or (list? (imsg-state-invs-zipper (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))))
          (> (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))))
          
          
          )
      a-vs
      (let* ([idx (if (and (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))
                           (> (length (imsg-state-pci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs))))
                              (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))))
                      (zipper-current (zipper-next (imsg-state-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))
                      (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
             [new-imgs (zipper-to-idx (viz-state-imgs a-vs) idx)]
             [new-curr-img ((zipper-current new-imgs))]
             [curr-pimgs-img ((zipper-current (viz-state-imgs a-vs)))]
             [img-resize (resize-image new-curr-img
                                       (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                       (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
             [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
             [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
       
        (if (does-img-need-resizing? new-curr-img)
            (let ([NEW-FLOOR (min (second img-resize) (third img-resize))])
              (cond
                [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor DEFAULT-ZOOM-CAP]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]
                [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor NEW-FLOOR]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]
                [else
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]))
            (let ([new-viz-state (struct-copy viz-state
                                              a-vs
                                              [imgs new-imgs]
                                              [curr-image new-curr-img]
                                              [image-posn
                                               (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                     (+ (posn-y (viz-state-image-posn a-vs))
                                                        growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR])])
              (reposition-out-of-bounds-img
               new-viz-state
               (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                          (viz-state-scale-factor a-vs))
               new-curr-img
               (viz-state-scale-factor a-vs)))))))

;;viz-state -> viz-state
;;Purpose: Jumps the visulization to previous broken invariant
(define (jump-prev a-vs)
  (if (or (list? (imsg-state-invs-zipper (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))))
          (< (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))))
          )
        
      a-vs
      (let* ([idx (if (and (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))
                           (< (length (imsg-state-pci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs))))
                              (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))))
                      (zipper-current (zipper-prev (imsg-state-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))
                      (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
             [new-imgs (zipper-to-idx (viz-state-imgs a-vs) idx)]
             [new-curr-img ((zipper-current new-imgs))]
             [curr-pimgs-img ((zipper-current (viz-state-imgs a-vs)))]
             [img-resize (resize-image new-curr-img
                                       (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                       (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
             [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
             [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
        
        (if (does-img-need-resizing? new-curr-img)
            (let ([NEW-FLOOR (min (second img-resize) (third img-resize))])
              (cond
                [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor DEFAULT-ZOOM-CAP]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]
                [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor NEW-FLOOR]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]
                [else
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-curr-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-curr-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-curr-img
                    (viz-state-scale-factor new-viz-state)))]))
            (let ([new-viz-state (struct-copy viz-state
                                              a-vs
                                              [imgs new-imgs]
                                              [curr-image new-curr-img]
                                              [image-posn
                                               (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                     (+ (posn-y (viz-state-image-posn a-vs))
                                                        growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR])])
              (reposition-out-of-bounds-img
               new-viz-state
               (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                          (viz-state-scale-factor a-vs))
               new-curr-img
               (viz-state-scale-factor a-vs)))))))
#|
;; viz-state -> viz-state
;; Purpose: Moves the visualization one step back in the derivation
(define (go-prev a-vs)
  (if (zipper-at-begin? (viz-state-imgs a-vs))
      a-vs
      (let* ([new-imgs (zipper-prev (viz-state-imgs a-vs))]
             [new-pimgs-img ((zipper-current new-imgs))]
             [curr-pimgs-img ((zipper-current (viz-state-imgs a-vs)))]
             [img-resize (resize-image new-pimgs-img
                                       (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                       (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
             [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
             [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
        (if (does-img-need-resizing? new-pimgs-img)
            (let ([NEW-FLOOR (min (second img-resize) (third img-resize))])
              (cond
                [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                 (reposition-out-of-bounds-img
                  (struct-copy viz-state
                               a-vs
                               [imgs new-imgs]
                               [curr-image new-pimgs-img]
                               [image-posn
                                (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                      (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                               [scale-factor DEFAULT-ZOOM-CAP]
                               [scale-factor-cap DEFAULT-ZOOM-CAP]
                               [scale-factor-floor NEW-FLOOR])
                  (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                             (viz-state-scale-factor a-vs))
                  new-pimgs-img
                  (viz-state-scale-factor a-vs))]
                [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                 (reposition-out-of-bounds-img
                  (struct-copy viz-state
                               a-vs
                               [imgs new-imgs]
                               [curr-image new-pimgs-img]
                               [scale-factor NEW-FLOOR]
                               [scale-factor-cap DEFAULT-ZOOM-CAP]
                               [scale-factor-floor NEW-FLOOR])
                  (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                             (viz-state-scale-factor a-vs))
                  new-pimgs-img
                  (viz-state-scale-factor a-vs))]
                [else
                 (reposition-out-of-bounds-img
                  (struct-copy viz-state
                               a-vs
                               [imgs new-imgs]
                               [curr-image new-pimgs-img]
                               [image-posn
                                (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                      (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                               [scale-factor-cap DEFAULT-ZOOM-CAP]
                               [scale-factor-floor NEW-FLOOR])
                  (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                             (viz-state-scale-factor a-vs))
                  new-pimgs-img
                  (viz-state-scale-factor a-vs))]))
            (reposition-out-of-bounds-img
             (struct-copy viz-state
                          a-vs
                          [imgs new-imgs]
                          [curr-image new-pimgs-img]
                          [image-posn
                           (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                 (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                          [scale-factor-cap DEFAULT-ZOOM-CAP]
                          [scale-factor-floor DEFAULT-ZOOM-FLOOR])
             (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                        (viz-state-scale-factor a-vs))
             new-pimgs-img
             (viz-state-scale-factor a-vs))))))

;; viz-state -> viz-state
;; Purpose: Restarts the derivation in the visualization
(define (go-to-begin a-vs)
  (if (zipper-at-begin? (viz-state-imgs a-vs))
      a-vs
      (let* ([new-imgs (zipper-to-begin (viz-state-imgs a-vs))]
             [new-pimgs-img ((zipper-current new-imgs))]
             [curr-pimgs-img ((zipper-current (viz-state-imgs a-vs)))]
             [img-resize (resize-image new-pimgs-img
                                       (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                       (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
             [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
             [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
        (if (does-img-need-resizing? new-pimgs-img)
            (let ([NEW-FLOOR (min (second img-resize) (third img-resize))])
              (cond
                [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-pimgs-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor DEFAULT-ZOOM-CAP]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-pimgs-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-pimgs-img
                    (viz-state-scale-factor new-viz-state)))]
                [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-pimgs-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor NEW-FLOOR]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-pimgs-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-pimgs-img
                    (viz-state-scale-factor new-viz-state)))]
                [else
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-pimgs-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-pimgs-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-pimgs-img
                    (viz-state-scale-factor new-viz-state)))]))
            (let ([new-viz-state (struct-copy viz-state
                                              a-vs
                                              [imgs new-imgs]
                                              [curr-image new-pimgs-img]
                                              [image-posn
                                               (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                     (+ (posn-y (viz-state-image-posn a-vs))
                                                        growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR])])
              (reposition-out-of-bounds-img
               new-viz-state
               (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                          (viz-state-scale-factor new-viz-state))
               new-pimgs-img
               (viz-state-scale-factor new-viz-state)))))))

;; viz-state -> viz-state
;; Purpose: Finishes the derivations in the visualization
(define (go-to-end a-vs)
  (if (zipper-at-end? (viz-state-imgs a-vs))
      a-vs
      (let* ([new-imgs (zipper-to-end (viz-state-imgs a-vs))]
             [new-pimgs-img ((zipper-current new-imgs))]
             [curr-pimgs-img ((zipper-current (viz-state-imgs a-vs)))]
             [img-resize (resize-image new-pimgs-img
                                       (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                       (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
             [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
             [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
        (if (does-img-need-resizing? new-pimgs-img)
            (let ([NEW-FLOOR (min (second img-resize) (third img-resize))])
              (cond
                [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-pimgs-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor DEFAULT-ZOOM-CAP]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-pimgs-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-pimgs-img
                    (viz-state-scale-factor new-viz-state)))]
                [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                 (let ([new-viz-state (struct-copy viz-state
                                                   a-vs
                                                   [imgs new-imgs]
                                                   [curr-image new-pimgs-img]
                                                   [scale-factor NEW-FLOOR]
                                                   [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                   [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-pimgs-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-pimgs-img
                    (viz-state-scale-factor new-viz-state)))]
                [else
                 (let ([new-viz-state
                        (struct-copy viz-state
                                     a-vs
                                     [imgs new-imgs]
                                     [curr-image new-pimgs-img]
                                     [image-posn
                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                            (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                     [scale-factor-floor NEW-FLOOR])])
                   (reposition-out-of-bounds-img
                    new-viz-state
                    (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                      new-pimgs-img)
                                               (viz-state-scale-factor new-viz-state))
                    new-pimgs-img
                    (viz-state-scale-factor new-viz-state)))]))
            (let ([new-viz-state (struct-copy viz-state
                                              a-vs
                                              [imgs new-imgs]
                                              [curr-image new-pimgs-img]
                                              [image-posn
                                               (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                     (+ (posn-y (viz-state-image-posn a-vs))
                                                        growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR])])
              (reposition-out-of-bounds-img
               new-viz-state
               (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                          (viz-state-scale-factor new-viz-state))
               new-pimgs-img
               (viz-state-scale-factor new-viz-state)))))))

;; viz-state -> viz-state
;; Purpose: Zooms in on the visualization
(define (zoom-in a-vs)
  (zoom a-vs ZOOM-INCREASE))

;; viz-state -> viz-state
;; Purpose: Zooms out the visualization
(define (zoom-out a-vs)
  (zoom a-vs ZOOM-DECREASE))

;; viz-state -> viz-state
;; Purpose: Zooms all the way in on the visualization
(define (max-zoom-in a-vs)
  (zoom a-vs (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs))))

;; viz-state -> viz-state
;; Purpose: Zooms in a moderate amount on the visualization
(define (reset-zoom a-vs)
  (zoom a-vs (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs))))

;; viz-state -> viz-state
;; Purpose: Zooms all the way out in the visualization
(define (max-zoom-out a-vs)
  #;(zoom a-vs (/ DEFAULT-ZOOM-FLOOR (viz-state-scale-factor a-vs)))
  (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
          (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs))))
      (let ([img-resize (resize-image (viz-state-curr-image a-vs)
                                      (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                      (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))])
        (begin
          ;(display (format "image-width ~s \n" (image-width (viz-state-curr-image a-vs))))
          ;(display (format "image-height ~s \n" (image-height (viz-state-curr-image a-vs))))
          ;(display (format "second img-resize ~s \n" (second img-resize)))
          ;(display (format "third img-resize ~s \n" (third img-resize)))
          (zoom a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs)))))
      (struct-copy viz-state a-vs [scale-factor DEFAULT-ZOOM-FLOOR])))
|#

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rules))

#|
A rule is a structure:
(make-rule triple)
triple is the entire of the ndfa rule
|#
(struct rule (triple))

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond [(empty? prev-path) (cons curr-path acc)]
        [(empty? curr-path) prev-path]
        [(equal? (first prev-path) (first curr-path))
         (remove-similarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similarities (rest prev-path) (rest curr-path) (cons (first curr-path) acc))]))


;;(listof symbols) (listof rules) symbol -> (listof configurations)
;;Purpose: Returns all possible configurations from start that consume the given word
(define (get-configs a-word lor start)
  (let (;;configuration
        ;;Purpose: The starting configuration
        [starting-config (append (list start) (list a-word))])
    (make-configs starting-config
                  a-word
                  lor
                  (list starting-config)
                  (list starting-config))))

;;configuration (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Explores all possible configurations using the given word, (listof rules), and visited
;;Visited = All configurations of the consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs config a-word lor visited path)
  (let* (;;(listof rules)
         ;;Purpose: Returns all rules that consume a letter using the given configurations
         [connected-rules (if (empty? a-word)
                              '()
                              (filter (λ (rule)
                                        (and (equal? (first rule) (first config))
                                             (equal? (second rule) (first (second config)))))
                                      lor))]
         ;;(listof configurations)
         ;;Purpose: Makes new configurations using given word and connected-rules
         [new-config (filter (λ (new-config) (not (member? new-config visited)))
                             (map (λ (rule)
                                    (if (empty? a-word)
                                        (append (list (third rule)) (list a-word))
                                        (append (list (third rule)) (list (rest a-word)))))
                                  connected-rules))]
         ;;(listof rules)
         ;;Purpose: Returns all rules that have an empty transition using the given configurations
         [connected-rules-via-emp
          (filter (λ (rule) (and (equal? (first rule) (first config)) (equal? (second rule) EMP)))
                  lor)]
         ;;(listof configurations)
         ;;Purpose: Makes new configurations using the given word and connected-rules-via-emp
         [new-config-via-emp (filter (λ (new-config) (not (member? new-config visited)))
                                     (map (λ (rule) (append (list (third rule)) (list a-word)))
                                          connected-rules-via-emp))])
    (cond [(and (empty? new-config) (empty? new-config-via-emp))
           (list path)] ;;no rules found that connect the config
          [(and (empty? new-config) (not (empty? new-config-via-emp)))
           (make-configs-helper new-config-via-emp
                                a-word
                                lor
                                (append new-config-via-emp visited)
                                path)]
          [(and (not (empty? new-config)) (empty? new-config-via-emp))
           (make-configs-helper new-config 
                                (rest a-word)
                                lor
                                (append new-config visited)
                                path)]
          [else (append (make-configs-helper new-config 
                                             (rest a-word)
                                             lor
                                             (append new-config visited)
                                             path)
                        (make-configs-helper new-config-via-emp
                                             a-word
                                             lor
                                             (append new-config-via-emp visited)
                                             path))])))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Ensures that all possible configurations are explored
;;Visited = All configurations that consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs-helper configs a-word lor visited path)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited (append path (list (first configs))))
              (make-configs-helper (rest configs) a-word lor visited path))))



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



(struct computation (LoC LoR visited) #:transparent)


;;config rule -> config
;;Purpose: Applies the given rule to the given config
;;ASSUMPTION: the given rule is applicable to the given config
(define (apply-rule config rule)
  (let* ([new-config (list (third rule)
                           (if (eq? (second rule) EMP)
                               (second (first (computation-LoC config)))
                               (rest (second (first (computation-LoC config))))))])
    (struct-copy computation config
                 [LoC (cons new-config (computation-LoC config))]
                 [LoR (cons rule (computation-LoR config))]
                 [visited (cons (first (computation-LoC config)) (computation-visited config))])))

(define (trace-computations start word lor)
  (let (;;configuration
        ;;Purpose: The starting configuration
        [starting-config (computation (list (append (list start) (list word)))
                                      '()
                                      '())])
    (make-computations starting-config
                       lor
                       (enqueue (list starting-config) E-QUEUE)
                       '())))


(define (make-computations config lor QoC path)
  (if (qempty? QoC)
      path
      (let* (;;(listof rules)
             ;;Purpose: Returns all rules that consume a letter using the given configurations
             [connected-read-rules (if (empty? (second (first (computation-LoC (qfirst QoC)))))
                                       '()
                                       (filter (λ (rule)
                                                 (and (equal? (first rule) (first (first (computation-LoC (qfirst QoC)))))
                                                      (equal? (second rule) (first (second (first (computation-LoC (qfirst QoC))))))))
                                               lor))]
             ;;(listof rules)
             ;;Purpose: Returns all rules that have an empty transition using the given configurations
             [connected-emp-rules
              (filter (λ (rule) (and (equal? (first rule) (first (first (computation-LoC (qfirst QoC))))) (equal? (second rule) EMP)))
                      lor)]
             ;;(listof configurations)
             ;;Purpose: Makes new configurations using given word and connected-rules
             [new-configs (filter (λ (new-c) (not (member? (first (computation-LoC new-c)) (computation-visited new-c))))
                                  (map (λ (rule) (apply-rule (qfirst QoC) rule)) (append connected-read-rules connected-emp-rules)))])
        (if  (empty? new-configs)
             (make-computations config lor (dequeue QoC) (cons (qfirst QoC) path))
             (make-computations config lor (enqueue new-configs (dequeue QoC)) path)))))
  









;;(listof configurations) (listof symbols) (listof rule) (listof rules) -> (listof rules)
;;Purpose: Attaches the transition rules used between configurations
(define (attach-rules->configs configs word lor acc)
  (cond [(<= (length configs) 1) (reverse acc)]
        [(equal? (second (first configs)) (second (second configs)))
         (local [(define e-rule-used (filter (λ (rule)
                                               (and (equal? (first rule) (first (first configs)))
                                                    (equal? (second rule) EMP)
                                                    (equal? (third rule) (first (second configs)))))
                                             lor))]
           (attach-rules->configs (rest configs) word lor (append e-rule-used acc)))]
        [else (local [(define rule-used (filter (λ (rule)
                                                  (and (equal? (first rule) (first (first configs)))
                                                       (equal? (second rule) (first word))
                                                       (equal? (third rule) (first (second configs)))))
                                                lor))]
                (attach-rules->configs (rest configs) (rest word) lor (append rule-used acc)))]))

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first rules)) EMP)))
         (let ([res (trace (first configs) '())])
           (make-trace (rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (equal? (second (first rules)) EMP))
         (let* ([rle (rule (first rules))]
                [res (struct-copy trace (first acc)
                                  [rules (cons rle (trace-rules (first acc)))])])
           (make-trace configs (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first rules))]
                     [res (trace (first configs)
                                 (list rle))])
                (make-trace (rest configs) (rest rules) (cons res acc)))]))

;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (last (last config))))
                     (get-configs a-word (sm-rules M) (sm-start M))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
        [a-word]))

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
                                    (list (take a-word (- (length a-word) word-len)))))
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
                                                  (list (inv-for-inv-config (second (first inv-configs)))))))])
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
      (filter (λ (res) (not (empty? res))) (reverse acc)) ;;might remove if not can index using the length of the wht was processed
      (let* ([new-acc (append-map (λ (inv-configs)
                                    (filter (λ (config)
                                              (and (equal? (second config) (take a-word (- (length a-word) word-len)))
                                                   (not (third config))))
                                            inv-configs))
                                  inv-config-results)])
        (return-brk-inv-configs-helper inv-config-results a-word (sub1 word-len) (cons new-acc acc)))))

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

;;(listof symbols) (listof configurations) -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                          (equal? (second configs) word))
                        config))
              full-configs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graph machine (listof symbols) symbol (listof symbols) (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M dead held-inv brkn-inv)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color (if (eq? state (sm-start M)) 'darkgreen 'black)
                                 'style (cond [(or (member? state held-inv) (member? state brkn-inv)) 'filled]
                                              [(eq? state dead) 'dashed]
                                              [else 'solid])
                                 'shape (if (member? state (sm-finals M)) 'doublecircle 'circle)
                                 'fillcolor (cond [(member? state held-inv) HELD-INV-COLOR]
                                                  [(member? state brkn-inv) BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 'fontcolor 'black)))
         cgraph
         (sm-states M)))

;; graph machine word (listof rules) (listof rules) symbol -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M current-a-rules current-rules dead)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(find-rule? rule dead current-a-rules) 'green]
                                              [(find-rule? rule dead current-rules) 'violetred]
                                              [else 'black])
                                 'fontsize 20
                                 'style (cond [(equal? (third rule) dead) 'dashed]
                                              [(or (find-rule? rule dead current-rules)
                                                   (find-rule? rule dead current-a-rules))
                                               'bold]
                                              [else 'solid]))))
         cgraph
         (sm-rules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;upci is the unprocessed consumed input (listof symbols)
;;pci is the proccessed consumed input (listof symbols)
;;M is a machine
;;inv is a the (listof (state (listof symbols -> boolean)))
;;dead is the sybmol of dead state
(struct building-viz-state (upci pci M inv dead configs accept-configs reject-configs))
;(struct viz-state-ndfa (upci pci M inv dead imsg instruct graphs))

(struct imsg-state (M upci pci invs-zipper inv-amt comps word-img-offset word-img-offset-cap scroll-accum) #:transparent)

(define E-SCENE (empty-scene 1250 600))

;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (los2str los)
  (define (helper los)
    (if (empty? (rest los))
        (string-append (symbol->string (first los)) " ")
        (string-append (symbol->string (first los)) " " (helper (rest los)))))
  (helper los))

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

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (last (last config))))
                                   (get-configs (imsg-state-pci imsg-st)
                                                (sm-rules (imsg-state-M imsg-st))
                                                (sm-start (imsg-state-M imsg-st))))]
         
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed (imsg-state-pci imsg-st) (imsg-state-M imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (remove-similarities last-consumed-word entire-word '())]) ;;keep an eye on remove-similiarites
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
            [(and (not (empty? (imsg-state-pci imsg-st))) (not completed-config?))
             (above/align
              'left
              (beside (text "aaaa" 20 'white)
                      (text "Word: " 20 'black)
                      ;(if (empty? last-consumed-word)
                      ;(text "" 20 'gray)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length last-consumed-word) 'gray)
                                               (list (length last-consumed-word) 'red)))));)
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
      (beside
       (text (format "The current number of possible computations is ~a. "
                     (number->string (list-ref (imsg-state-comps imsg-st)
                                               (length (imsg-state-pci imsg-st)))))
             20
             'brown)
       (text "aaaaa" 20 'white)
       (cond [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." 20 'red)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept))
              (text "There is a computation that accepts." 20 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'reject))
              (text "All computations end in a non-final state and the machine rejects." 20 'red)]
             [(text "Word Status: accept " 20 'white)])))
     (rectangle 1250 50 'solid 'white))))


;;viz-state -> (list graph-thunk computation-length)
;;Purpose: Creates a graph thunk and finds the associated computation's length for a given viz-state
(define (create-graph-thunk a-vs)
  (let* (;;(listof configurations)
         ;;Purpose: Returns all configurations using the given word
         [all-configs (get-portion-configs (building-viz-state-upci a-vs)
                                           (building-viz-state-configs a-vs))]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of all configurations
         [r-config (append-map (λ (configs) (trace-rules (first configs))) (building-viz-state-reject-configs a-vs))]

         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [current-rules (map (λ (rule) (rule-triple rule)) r-config)]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [a-configs (if (empty? (building-viz-state-accept-configs a-vs))
                        (building-viz-state-accept-configs a-vs)
                        (trace-rules (first (building-viz-state-accept-configs a-vs))))
                        #;(append-map (λ (configs) (trace-rules (first configs)))
                                      (building-viz-state-accept-configs a-vs))]
         
         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [current-a-rules (map (λ (rule) (rule-triple rule)) a-configs)]
     
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr all-configs]
                               #:when (equal? (first invs) (first curr)))
                     (list invs (building-viz-state-pci a-vs)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (append-map
                     (λ (inv)
                       (if ((second (first inv)) (second inv))
                           (list (first (first inv)))
                           '()))
                     get-invs)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (append-map
                     (λ (inv)
                       (if (not (member? (first (first inv)) held-invs))
                           (list (first (first inv)))
                           '()))
                     get-invs)]
         ;;(listof symbols)
         ;;Purpose: The states that the ndfa could be in
         [destin-states (begin
                          (display (format "curR-a-config ~s \n" current-rules))
                          (display (format "curr-a-config ~s \n \n" current-a-rules))
                          ;(display (format "current-a-rules ~s \n" current-a-rules))
                          (if (not (empty? (building-viz-state-pci a-vs)))
                              (map last (remove-duplicates (append current-rules current-a-rules)))
                              #;(append r-config a-configs)
                              (cons (sm-start (building-viz-state-M a-vs))
                                    (remove-duplicates
                                     (map last #;current-rules
                                          (append current-rules current-a-rules))))))])
    (list (edge-graph
           (node-graph
            (create-graph 'ndfagraph #:atb (hash 'rankdir "LR"))
            (building-viz-state-M a-vs)
            (building-viz-state-dead a-vs)
            held-invs
            brkn-invs)
           (building-viz-state-M a-vs)
           current-a-rules
           current-rules
           (building-viz-state-dead a-vs))
          (length destin-states))))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(empty? (building-viz-state-upci a-vs)) (reverse (cons (create-graph-thunk a-vs) acc))]
        [(not (ormap (λ (config) (empty? (last (last config))))
                     (get-configs (building-viz-state-pci a-vs)
                                  (sm-rules (building-viz-state-M a-vs))
                                  (sm-start (building-viz-state-M a-vs)))))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  [upci (rest (building-viz-state-upci a-vs))]
                                                  [pci (append (building-viz-state-pci a-vs)
                                                               (list (first (building-viz-state-upci a-vs))))]
                                                  [accept-configs (if (empty? (building-viz-state-accept-configs a-vs))
                                                                      (building-viz-state-accept-configs a-vs)
                                                                      (rest (building-viz-state-accept-configs a-vs)))]
                                                  [reject-configs (filter (λ (configs)
                                                                            (not (empty? configs)))
                                                                          (map rest (building-viz-state-reject-configs a-vs)))])
                                     (cons next-graph acc)))]))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config)
                                     (empty? (last (last config))))
                                   (get-configs (imsg-state-pci (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))) 
                                                (sm-rules (imsg-state-M (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                                (sm-start (imsg-state-M (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))))]
         [pci (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
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
                                   (not completed-config?))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
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
                    (struct-copy imsg-state a-imsgs [word-img-offset 0] [scroll-accum 0])])])))

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
                                             [word-img-offset
                                              (imsg-state-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (if (or (list? (imsg-state-invs-zipper (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))))
          (< (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))))
          
          )
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
                                                      (viz-state-informative-messages a-vs)))))
          
          )
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
                         [pci partial-word]
                         [invs-zipper zip])])]))))

;;machine -> machine
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (cond
    [(eq? (sm-type M) 'ndfa)
     (local
       [;;symbol
        ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
        ;;         otherwise uses DEAD
        (define dead (if (member? DEAD (sm-states M)) (gen-state (sm-states M)) DEAD))
        ;;(listof symbols)
        ;;Purpose: Makes partial rules for every combination of states in M and symbols in sigma of M
        (define new-rules
          (for*/list ([states (sm-states M)]
                      [sigma (sm-sigma M)])
            (list states sigma)))
        ;;(listof rules)
        ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
        (define dead-rules
          (for*/list ([ds (list dead)]
                      [sigma (sm-sigma M)])
            (list ds sigma ds)))
        ;;(listof rules)
        ;;Purpose: Gets rules that are not currently in the original rules of M
        (define get-rules-not-in-M  (local [(define partial-rules (map (λ (rule)
                                                                         (append (list (first rule)) (list (second rule))))
                                                                       (sm-rules M)))]
                                      (filter (λ (rule)
                                                (not (member? rule partial-rules)))
                                              new-rules)))
        ;;(listof rules)
        ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
        (define rules-to-dead
          (map (λ (rule) (append rule (list dead)))
               get-rules-not-in-M))]
       (make-ndfa (append (sm-states M) (list dead))
                  (sm-sigma M)
                  (sm-start M)
                  (sm-finals M)
                  (append (sm-rules M) rules-to-dead dead-rules)))]
    [(and (eq? (sm-type M) 'dfa) (not (member? DEAD (sm-states M))))
     (make-dfa (sm-states M) (sm-sigma M) (sm-start M) (sm-finals M) (sm-rules M))]
    [else M]))

;;ndfa word [boolean] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (ndfa-viz M a-word #:add-dead [add-dead #f] . invs)
  (if (not (or (eq? (sm-type M) 'ndfa) (eq? (sm-type M) 'dfa)))
      (error "The given machine must be a ndfa.")
      (let* ([new-M (if add-dead (make-new-M M) M)]
             [dead-state (cond [(and add-dead (eq? (sm-type M) 'ndfa)) (last (sm-states new-M))]
                               [(and add-dead (eq? (sm-type M) 'dfa)) DEAD]
                               [else 'no-dead])]
             [all-configs (get-configs a-word (sm-rules new-M) (sm-start new-M))]
            
             [all-accept-configs (filter (λ (config)
                                       (and (member? (first (last config)) (sm-finals new-M))
                                            (empty? (second (last config)))))
                                     all-configs)]
             [accept-configs (if (empty? all-accept-configs) all-accept-configs (first all-accept-configs))]
             [accept-rules (attach-rules->configs accept-configs a-word (sm-rules new-M) '())]
             [accepting-configs (make-trace accept-configs accept-rules '())
                                #;(map (λ (config rules)
                                       (make-trace config rules '()))
                                     accept-configs
                                     accept-rules)]
             [reject-configs (filter (λ (config)
                                       (not (member? config accept-configs)))
                                     all-configs)]
             [reject-rules (map (λ (configs)
                                  (attach-rules->configs configs a-word (sm-rules new-M) '()))
                                reject-configs)]
             [rejecting-configs (map (λ (configs rules)
                                       (make-trace configs rules '()))
                                     reject-configs
                                     reject-rules)]
             [building-state (building-viz-state a-word
                                                 '()
                                                 new-M
                                                 (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w) #t)) invs) invs) 
                                                 dead-state
                                                 all-configs
                                                 accepting-configs
                                                 rejecting-configs)]
             [graphs+comp-len (create-graph-thunks building-state '())]
             [graphs (map first graphs+comp-len)]
             [computation-lens (map second graphs+comp-len)]
             [inv-configs (map (λ (con)
                                 (length (second (first con))))
                               (return-brk-inv-configs
                                (get-inv-config-results
                                 (make-inv-configs a-word all-configs)
                                 invs)
                                a-word))])
        ;(struct building-viz-state (upci pci M inv dead))
        ;(struct imsg-state (M upci pci))
        ;;ANCHOR
        
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
                                                   (if (empty? inv-configs) '() (list->zipper inv-configs))
                                                   (sub1 (length inv-configs))
                                                   computation-lens
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
                 (create-viz-process-key ["right" viz-go-next right-key-pressed]
                                         ["left" viz-go-prev left-key-pressed]
                                         ["up" viz-go-to-begin up-key-pressed]
                                         ["down" viz-go-to-end down-key-pressed]
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
                                          ([RULE-YIELD-DIMS
                                            (lambda (a-imsgs x-diff y-diff) a-imsgs)])
                                          ( [ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                            [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                            [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                            [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                            [W-KEY-DIMS viz-zoom-in identity]
                                            [S-KEY-DIMS viz-zoom-out identity]
                                            [R-KEY-DIMS viz-max-zoom-out identity]
                                            [E-KEY-DIMS viz-reset-zoom identity]
                                            [ F-KEY-DIMS viz-max-zoom-in identity]
                                            [ A-KEY-DIMS identity a-key-pressed]
                                            [ D-KEY-DIMS identity d-key-pressed]
                                            [ J-KEY-DIMS jump-prev j-key-pressed]
                                            [ L-KEY-DIMS jump-next l-key-pressed])
                                          )
                 'ndfa-viz))))

(define aa*Uab* (make-ndfa '(K B D)
                           '(a b)
                           'K
                           '(B D)
                           `((K a D) (K a B)
                                     (B a B)
                                     (D b D))))

(define AT-LEAST-ONE-MISSING
  (make-ndfa '(S A B C)
             '(a b c)
             'S
             '(A B C)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                          (A b A) (A c A)
                          (B a B) (B c B)
                          (C a C) (C b C))))

(define p2-ndfa
  (make-ndfa '(S A B C D E)
             '(a b)
             'S
             '(C E)
             `((S ,EMP A) (S ,EMP D)
                          (A a B) (A ,EMP C)
                          (B b A)
                          (C b C)
                          (D a E)
                          (E b E))))

(define AB*B*UAB*
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))

(define AB*B*UAB*2
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H) (H a S))))

(define aa-ab (make-ndfa `(S A B F)
                         '(a b)
                         'S
                         '(A B F)
                         `((S a A) (S a B) (S ,EMP F)
                                   (A a A)
                                   (B b B))))

(define ends-with-two-bs
  (make-ndfa `(S A B)
             '(a b)
             'S
             '(B)
             `((S a S) (S b S) (S b A)
                       (A b B)
                       (B b B))))
(define ENDS-WITH-TWO-Bs
  (make-ndfa `(S A B)
             '(a b)
             'S
             '(B)
             `((S a S) (S b A)
                       (A b B) (A a S)
                       (B b B) (B a S))))

(define missing-exactly-one
  (make-ndfa '(S A B C D E F G H I J K L M N O P)
             '(a b c)
             'S
             '(E G I K M O)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                          (A b D) (A c F)
                          (B a H) (B b J)
                          (C a L) (C c N)
                          (D b D) (D c E)
                          (F c F) (F b G)
                          (H a H) (H b I)
                          (J b J) (J a K)
                          (L a L) (L c M)
                          (N c N) (N a O)
                          (E c E) (E b E) (E a P)
                          (G b G) (G c G) (G a P)
                          (I b I) (I a I) (I c P)
                          (K a K) (K b K) (K c P)
                          (M a M) (M c M) (M b P)
                          (O a O) (O c O) (O b P)
                          (P a P) (P b P) (P c P))))

;; L = (aba)* U (ab)*
(define ND
  (make-ndfa '(S A B C D E)
             '(a b)
             'S
             '(S)
             `((S a A) (S a B)
                       (A b C)
                       (B b D)
                       (C a E)
                       (D ,EMP S)
                       (E ,EMP S))))

(define ND2
  (make-ndfa
   '(S A B C D E F)
   '(a b)
   'S
   '(D E)
   `((S ,EMP A) (S ,EMP B)
                (A ,EMP D)
                (D b D) (D ,EMP F)
                (B a E) (B b B)
                (E a E) (E b E) (E ,EMP C))))

(define ND3
  (make-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) (S ,EMP B)
                          (A a A) (A ,EMP C)
                          (C ,EMP D)
                          (D ,EMP B)
                          (B b B))))

(define ND4 (make-ndfa '(S ds)
                       '(a b)
                       'S
                       '(ds)
                       `((S a ds)
                         (ds a ds))))

(define ND5
  (make-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) 
               (A ,EMP B)
               (B ,EMP C)
               (C ,EMP D)
               (D ,EMP S))))

(define EVEN-NUM-Bs
  (make-dfa '(S F)
            '(a b)
            'S
            '(S)
            `((S a S) (S b F)
                      (F a F) (F b S))
            'no-dead))

(define M2 (make-dfa `(S A F ,DEAD)
                     '(a b)
                     'S
                     '(F)
                     `((S a A) (S b ,DEAD)
                               (A a ,DEAD) (A b F)
                               (F a ,DEAD) (F b F))))


#;(let ([res (graph->bitmap cgraph (current-directory) FNAME)])
    (begin
      (delete-file (string-append FNAME ".dot"))
      (delete-file (string-append FNAME ".png"))
      res))
#|
;;accept examples
(ndfa-viz AB*B*UAB* '(a b b))
(ndfa-viz p2-ndfa '(a b b))
(ndfa-viz missing-exactly-one '(a a a a b b b b a a b b a a))
(ndfa-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b))
(ndfa-viz aa-ab '(a a a a))
(ndfa-viz ends-with-two-bs '(a b a b a b b b))
(ndfa-viz ends-with-two-bs '(a b a b a b b a a a b b))
;;reject examples
(ndfa-viz AB*B*UAB* '(a b b a))
(ndfa-viz p2-ndfa '(a b b a))
(ndfa-viz missing-exactly-one '(a a a a b b b b a a b b a a c))
(ndfa-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b a))
(ndfa-viz aa-ab '(a b b b a)
(ndfa-viz AT-LEAST-ONE-MISSING '(a b c))
(ndfa-viz p2-ndfa '(a b a b))
(ndfa-viz AB*B*UAB* '(a b a b))

;;Invariant examples
(ndfa-viz AT-LEAST-ONE-MISSING '(a b c)
          (list 'S S-INV)
          (list 'A ALON-A-INV)
          (list 'B ALON-B-INV)
          (list 'C ALON-C-INV)) 
(ndfa-viz EVEN-NUM-Bs '(a b b b a b b) 
          (list 'S EVEN-NUM-Bs-S-INV)
          (list 'F EVEN-NUM-Bs-F-INV))
(ndfa-viz AB*B*UAB* '(a b b b b)
          (list 'S S-INV)
          (list 'K K-INV)
          (list 'B B-INV)
          (list 'C C-INV)
          (list 'H H-INV))

|#
;;word -> boolean
;;Purpose: Determines if the given word is missing an a
(define (ALON-A-INV a-word)
  (empty? (filter (λ (w) (equal? w 'a))
                  a-word)))

;;word -> boolean
;;Purpose: Determines if the given word is missing an b
(define (ALON-B-INV a-word)
  (empty? (filter (λ (w) (equal? w 'b))
                  a-word)))
;;word -> boolean
;;Purpose: Determines if the given word is missing an c
(define (ALON-C-INV a-word)
  (empty? (filter (λ (w) (equal? w 'c))
                  a-word)))

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (S-INV a-word)
  (empty? a-word))

;;word -> boolean
;;Purpose: Determines if the last letter in the given word is an b
(define (K-INV a-word)
  (or (empty? a-word) (equal? (last a-word) 'b)))

;;word -> boolean
;;Purpose: Determines if the last letter in the given word is an a
(define (B-INV a-word)
  (and (not (empty? a-word)) (not (equal? (last a-word) 'a))))

;;word -> boolean
;;Purpose: Determines if the given word has one a
(define (C-INV a-word)
  (= (length (filter (λ (w) (equal? w 'a)) a-word)) 1))

;;word -> boolean
;;Purpose: Determines if the given word is empty or if the last letter is an a or b
(define (H-INV a-word)
  ;(not
  (or (empty? a-word) (equal? (last a-word) 'a) (equal? (last a-word) 'b)))
;)

;;word -> boolean
;;Purpose: Determine if the given word has an even number of Bs
(define (EVEN-NUM-Bs-S-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))

;;word -> boolean
;;Purpose: Determine if the given word has an odd number of Bs
(define (EVEN-NUM-Bs-F-INV a-word)
  (odd? (length (filter (λ (w) (equal? w 'b)) a-word))))


(define (s-inv a-word)
  (empty? a-word))

(define (a-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (= num-a num-b)))

(define (b-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (> num-a num-b)))

(define (c-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (= num-b num-a)))

(define (d-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (= num-a num-b)))

(define (e-inv a-word)
  (andmap (λ (w) (equal? w 'b)) (rest a-word)))

(define inv-list (list (list 'S S-INV)
                       (list 'K K-INV)
                       (list 'B B-INV)
                       (list 'C C-INV)
                       (list 'H H-INV)))
(define (S-INV1 ci)
  (not (empty? ci)))

;; word -> Boolean
;; Purpose: To determine whether ci = aa*
(define (A-INV1 ci)
  (and (not (empty? ci))
       (andmap (λ (el) (eq? el 'a)) ci)))

;; word -> Boolean
;; Purpose: To determine whether ci = ab*
(define (B-INV1 ci)
  (and (not (empty? ci))
       (eq? (first ci) 'a)
       (andmap (λ (el) (eq? el 'b)) ci)))

;;word -> Boolean
;;Purpose: To determine whether ci = emp
(define (F-INV1 ci)
  (empty? ci))

(define nd (make-ndfa '(S Z Y A B)
                      '(a b)
                      'S
                      '(B)
                      `((S b Z)
                        (S b Y)
                        (Y a A)
                        (Z a A)
                        (A a B))))

(define n (make-ndfa '(K H F M I)
                     '(a b)
                     'K
                     '(I)
                     `((K b H)
                       (H ,EMP F)
                       (H ,EMP M)
                       (F a I)
                       (M a I))))

(define nk (make-ndfa '(K H F M I)
                      '(a b)
                      'K
                      '(I)
                      `((K b H)
                        (H ,EMP F)
                        (F ,EMP M)
                        (M ,EMP I)
                        (I ,EMP H))))

(define DNA-SEQUENCE (make-dfa '(K H F M I D B S R) ;C)
                               '(a t c g)
                               'K
                               '(K F I B R)
                               `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                         (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                         (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                         (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (DNA-K-INV a-word)
  (empty? a-word))

;;word -> boolean
;;Purpose: Determines if the given word has more a's than t's
(define (DNA-H-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of a's and t's
(define (DNA-F-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more t's than a's
(define (DNA-M-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-t num-a)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of t's and a's
(define (DNA-I-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more c's than g's
(define (DNA-D-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-c num-g)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of c's and g's
(define (DNA-B-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has more g's than c's
(define (DNA-S-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of g's and c's
(define (DNA-R-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

;(ndfa-viz n '(b a a))
;(ndfa-viz nk '(b a a))
;(ndfa-viz aa-ab '(a a a a b a))
;(ndfa-viz aa-ab '(a a a a b a) #:add-dead #t)
;(ndfa-viz aa-ab '(a a a a a a a) (list 'S S-INV1) (list 'A A-INV1) (list 'B B-INV1) (list 'F F-INV1));
;(ndfa-viz aa-ab '(a a a a a a a) (list 'S S-INV) (list 'A A-INV1) (list 'B B-INV1) (list 'F F-INV1))
;things that change end with a bang(!)
;combines computations that have similiar configurations
#;(ndfa-viz DNA-SEQUENCE '(a t c g t a c) (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV))
#;(ndfa-viz DNA-SEQUENCE '(c g c g a t a t g c t a g c a t)  (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV)) 

"notes to self:"
"scroll thru word instead of jumping to end"
"do dna problem for ndfas"
"update code and clean"


(define (makes-same-configs? lor word start)
  (let ([get-con (get-configs word lor start)]
        [get-comp (map (λ (comp) (reverse (computation-LoC comp))) (trace-computations start word lor))])
    (andmap (λ (comp) (member? comp get-con))
            get-comp)))