#lang racket

(require "../viz-lib/viz-macros.rkt"
         2htdp/image
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/viz-state.rkt"
         "../../fsm-gviz/private/lib.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         math/matrix)



(provide init-viz)
(define E-SCENE-WIDTH 1250)
(define E-SCENE-HEIGHT 500)
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))
(define FONT-SIZE 20)
(define TAPE-SIZE 42)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define PERCENT-BORDER-GAP 0.9)
(define HEIGHT-BUFFER 20)
(define LETTER-KEY-WIDTH-BUFFER 20)
(define ARROW-KEY-WIDTH-BUFFER 40)
(define INS-TOOLS-BUFFER 30)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define NODE-SIZE 50)

(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))

(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))

(struct imsg-state (rules yield input-word word-img-offset word-img-offset-cap
                          scroll-accum broken-invariants))
#|

(define S-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_s.png"))

(define W-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_w.png"))

(define R-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_r.png"))

(define F-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_f.png"))

(define E-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_e.png"))

(define A-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_a.png"))

(define D-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_d.png"))

(define ARROW-RIGHT-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_right.png"))

(define ARROW-LEFT-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_left.png"))

(define ARROW-UP-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_up.png"))

(define ARROW-DOWN-KEY (bitmap/file "../viz-lib/viz-imgs/keyboard_key_down.png"))
|#

;; Listof Symbol natnum -> Image
;; Returns an image of a tape of symbols, capable of sliding when its start-index is varied
(define (make-tape-img tape start-index)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let [(letter-imgs (build-list TAPE-SIZE
                                 (λ (i) (if (< (+ start-index i) (length tape))
                                            (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                           24
                                                           'black)
                                                     (overlay (square 25 'solid 'white)
                                                              (square (add1 25) 'solid 'white)))
                                            (overlay (square 25 'solid 'white)
                                                     (square (add1 25) 'solid 'white))))))]
    (make-tape-img letter-imgs start-index)))

(define TAPE-IMG-HEIGHT (image-height (make-tape-img (list 'a) 0)))


(define RULE-YIELD-DIMS
  (let [(DREV (let [(drev-text (text "Deriving: " FONT-SIZE 'black))]
                (overlay drev-text (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white))))
        (YIELD (let [(yield-text (text "Current Yield: " FONT-SIZE 'black))]
                 (overlay yield-text (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white))))
        (RULE-USED (text "The rule used: " FONT-SIZE 'black))]
    (bounding-limits (+ (image-width (rectangle 1 (* 2 FONT-SIZE) "solid" 'white))
                        (image-width (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                             (above/align "right" RULE-USED DREV YIELD))))
                     (* E-SCENE-WIDTH 0.9)
                     (+ E-SCENE-HEIGHT)
                     (+ E-SCENE-HEIGHT (image-height (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                                             (above/align "right" RULE-USED DREV YIELD)))))))

(define cursor
  (let [(cursor-rect (let [(inner-white (rectangle 5 17.5 'solid 'white))
                           (outer-black (rectangle 9 20 'solid 'black))
                           (white-triangle-infill (rectangle 9 5 'solid 'white))]
                       (above white-triangle-infill (overlay/xy inner-white -2 0 outer-black))))
        (cursor-tri (let [(inner-white
                           (overlay/align/offset "right"
                                                 "middle"
                                                 (rotate 250
                                                         (overlay/align/offset "middle"
                                                                               "bottom"
                                                                               (triangle/aas 30
                                                                                             30
                                                                                             44
                                                                                             'solid
                                                                                             'white)
                                                                               0
                                                                               3
                                                                               (triangle/aas 30
                                                                                             30
                                                                                             48
                                                                                             'solid
                                                                                             'black)))
                                                 -2
                                                 -1
                                                 (triangle/aas 38.94
                                                               70.54
                                                               74
                                                               'solid
                                                               'white)))
                          (outer-black (overlay/align/offset "right"
                                                             "middle"
                                                             (rotate 250
                                                                     (triangle/aas 30
                                                                                   30
                                                                                   60
                                                                                   'solid
                                                                                   'white))
                                                             -1
                                                             -1
                                                             (triangle/sss 60
                                                                           90
                                                                           90
                                                                           'solid
                                                                           'black)))]
                      (scale 0.5 (rotate 310 (overlay/xy inner-white
                                                         -9
                                                         -3
                                                         outer-black)))))]
    (overlay/xy (rotate 25 cursor-rect) -7 -26 cursor-tri)))

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
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)

                          (above/align "middle"  A-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Word start" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)

                          (above/align "middle"  D-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Word end" (- FONT-SIZE 2) 'black))))))

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

(define A-KEY-DIMS
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
  (let* [(reflection (if reflect
                         -1
                         1))
         (result (matrix* (matrix [[(* reflection x-scale (cos rotate)) (* x-shear (* -1 (sin rotate))) x-translate ]
                                   [(* (sin rotate) y-shear) (* y-scale (cos rotate)) y-translate]
                                   [0 0 1]])
                          point))]
    result))

;; img posn num>0 -> matrix x y 1
;; Calculates the transform needed to zoom correctly 
(define (zoom-affine-transform img img-posn scale)
  (let* [(transformed-x (* -1 (+ (- (/ E-SCENE-WIDTH 2)
                                    (+ (posn-x img-posn) (/ (image-width img) 2)))
                                 (/ (image-width img) 2))))
         (transformed-y (* -1 (+ (- (/ E-SCENE-HEIGHT 2)
                                    (+ (posn-y img-posn) (/ (image-height img) 2)))
                                 (/ (image-height img) 2))))]
    (affine-transform #:x-translate (* -1 transformed-x)
                      #:y-translate (* -1 transformed-y)
                      #:point (affine-transform #:x-scale scale
                                                #:y-scale scale
                                                #:point (affine-transform #:x-translate transformed-x
                                                                          #:y-translate transformed-y
                                                                          #:point (matrix [ [0] [0] [1] ]))))))

;; img num>0 -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define (calculate-viewport-limits scaled-image scale)
  (let* [(img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale)))
         (img-height-node-diff (- (/ (image-height scaled-image) 2) (* NODE-SIZE scale)))
         (scaled-node-size (* NODE-SIZE scale))
         (MIN-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                    (- (* -1 (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH)) (- E-SCENE-WIDTH scaled-node-size))
                    (* -1 img-width-node-diff)))
         (MAX-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                    (+ (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH) E-SCENE-WIDTH (- E-SCENE-WIDTH scaled-node-size))
                    (+ E-SCENE-WIDTH img-width-node-diff)))                                             
         (MIN-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                    (- (* -1 (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT)) (- E-SCENE-HEIGHT scaled-node-size))
                    (* -1 img-height-node-diff)))
         (MAX-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                    (+ (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT) E-SCENE-HEIGHT (- E-SCENE-HEIGHT scaled-node-size))
                    (+ E-SCENE-HEIGHT img-height-node-diff)))]
    (bounding-limits MIN-X MAX-X MIN-Y MAX-Y)))

;; viz-state viewport-limits img num>0 -> viz-state
;; Returns a new viz-state where if the given image would be out of bounds of its viewport limits
;; It is placed into a position inbounds
(define (reposition-out-of-bounds-img a-vs viewport-lims new-img new-scale)
  (cond [(outside-west-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (bounding-limits-min-x viewport-lims) (posn-y (viz-state-image-posn a-vs)))]
                      [scale-factor new-scale])]
        [(outside-east-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (bounding-limits-max-x viewport-lims) (posn-y (viz-state-image-posn a-vs)))]
                      [scale-factor new-scale])]
        [(outside-north-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (posn-x (viz-state-image-posn a-vs)) (bounding-limits-min-y viewport-lims))]
                      [scale-factor new-scale])]
        [(outside-south-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (posn-x (viz-state-image-posn a-vs)) (bounding-limits-max-y viewport-lims))]
                      [scale-factor new-scale])]
        [(outside-west-north-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (bounding-limits-min-x viewport-lims) (bounding-limits-min-y viewport-lims))]
                      [scale-factor new-scale])]
        [(outside-west-south-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (bounding-limits-min-x viewport-lims) (bounding-limits-max-y viewport-lims))]
                      [scale-factor new-scale])]
        [(outside-east-north-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (bounding-limits-max-x viewport-lims) (bounding-limits-min-y viewport-lims))]
                      [scale-factor new-scale])]
        [(outside-east-south-sides-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [image-posn (posn (bounding-limits-max-x viewport-lims) (bounding-limits-max-y viewport-lims))]
                      [scale-factor new-scale])]
        [(within-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
         (struct-copy viz-state a-vs
                      [curr-image new-img]
                      [scale-factor new-scale])]))

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
(define (zoom a-vs factor)
  (let* [(new-scale (* factor (viz-state-scale-factor a-vs)))
         (scalable? (cond [(eq? factor ZOOM-INCREASE) (> (viz-state-scale-factor-cap a-vs) new-scale)]
                          [(eq? factor ZOOM-DECREASE) (< (viz-state-scale-factor-floor a-vs) new-scale)]))]
    (if scalable?
        (let* [(scaled-image (scale new-scale (viz-state-curr-image a-vs)))
               (viewport-lims (calculate-viewport-limits scaled-image new-scale))
               (scale-increase (/ new-scale (viz-state-scale-factor a-vs)))
               (affine-matrix (zoom-affine-transform (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs))
                                                     (viz-state-image-posn a-vs)
                                                     scale-increase))]
          (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                     [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs))
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
      (func (struct-copy viz-state a-vs
                         [click-buffer 1]
                         [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))
      (if (= (viz-state-click-buffer a-vs) )
          (struct-copy viz-state a-vs
                       [click-buffer 0]
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])
          (struct-copy viz-state a-vs
                       [click-buffer (add1 (viz-state-click-buffer a-vs))]
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))))

;; image int int -> image
;; Scales a image to the given dimentions 
(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define aspect (/ src-width src-height))
  (define scale (min
                 (/ max-width src-width)
                 (/ max-height src-height)))

  (define scaled-width (* src-width scale))
  (define scaled-height (* src-height scale))

  (cond [(and (> scaled-width max-width)
              (<= scaled-height max-height))
         (list (scale/xy
                (/ max-width src-width)
                (/ (/ scaled-width aspect) src-height)
                img)
               (/ max-width src-width)
               (/ (/ scaled-width aspect) src-height))]
        [(and (<= scaled-width max-width)
              (> scaled-height max-height))
         (let ([scaled-aspect (/ scaled-width scaled-height)])
           (list (scale/xy
                  (/ (* scaled-height scaled-aspect) src-width)
                  (/ max-height src-height)
                  img)
                 (/ (* scaled-height scaled-aspect) src-width)
                 (/ max-height src-height)))]
        [(and (> scaled-width max-width)
              (> scaled-height max-height))
         (let* ([new-scaled-height (/ max-width aspect)]
                [scaled-aspect (/ max-width new-scaled-height)])
           (list (scale/xy
                  (/ (* max-height scaled-aspect) src-width)
                  (/ max-height src-height)
                  img)
                 (/ (* max-height scaled-aspect) src-width)
                 (/ max-height src-height)))]
        [(and (<= scaled-width max-width)
              (<= scaled-height max-height))
         (list (scale/xy
                (/ scaled-width src-width)
                (/ scaled-height src-height)
                img)
               (/ scaled-width src-width)
               (/ scaled-height src-height))]))

;; img -> boolean
;; Checks to see if an image needs to be resized
(define (does-img-need-resizing? img)
  (or (< E-SCENE-WIDTH (image-width img))
      (< E-SCENE-HEIGHT (image-height img))))

;; viz-state -> viz-state
;; Purpose: Moves the visualization to the next step of the derivation
(define (go-next a-vs)
  (if (zipper-at-end? (viz-state-imgs a-vs))
      a-vs
      (let* [(new-imgs (zipper-next (viz-state-imgs a-vs)))
             (new-curr-img ((zipper-current new-imgs)))
             (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
             (img-resize (resize-image new-curr-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
        (if (does-img-need-resizing? new-curr-img  )
            (let [(NEW-FLOOR (min (second img-resize) (third img-resize)))]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                     (let [(new-viz-state (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-curr-img]
                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                       [scale-factor DEFAULT-ZOOM-CAP]
                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                       [scale-factor-floor NEW-FLOOR]))]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                                     new-curr-img
                                                     (viz-state-scale-factor new-viz-state)))]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (let [(new-viz-state (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-curr-img]
                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                       [scale-factor NEW-FLOOR]
                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                       [scale-factor-floor NEW-FLOOR]))]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                                     new-curr-img
                                                     (viz-state-scale-factor new-viz-state)))]
                    [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                            [imgs new-imgs]
                                                            [curr-image new-curr-img]
                                                            [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                              (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                            [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                            [scale-factor-floor NEW-FLOOR]))]
                            (reposition-out-of-bounds-img new-viz-state
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                     (viz-state-scale-factor new-viz-state))
                                                          new-curr-img
                                                          (viz-state-scale-factor new-viz-state)))]))
            (let [(new-viz-state (struct-copy viz-state a-vs
                                              [imgs new-imgs]
                                              [curr-image new-curr-img]
                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR]))]
              (reposition-out-of-bounds-img new-viz-state
                                            (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                                                       (viz-state-scale-factor a-vs))
                                            new-curr-img
                                            (viz-state-scale-factor a-vs)))))))

;; viz-state -> viz-state
;; Purpose: Moves the visualization one step back in the derivation
(define (go-prev a-vs)
  (if (zipper-at-begin? (viz-state-imgs a-vs))
      a-vs
      (let* [(new-imgs (zipper-prev (viz-state-imgs a-vs)))
             (new-pimgs-img ((zipper-current new-imgs)))
             (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
        (if (does-img-need-resizing? new-pimgs-img  )
            (let [(NEW-FLOOR (min (second img-resize) (third img-resize)))]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                     (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                [imgs new-imgs]
                                                                [curr-image new-pimgs-img]
                                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                                [scale-factor DEFAULT-ZOOM-CAP]
                                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                [scale-factor-floor NEW-FLOOR])
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                              (viz-state-scale-factor a-vs))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor a-vs))]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                [imgs new-imgs]
                                                                [curr-image new-pimgs-img]
                                                                [scale-factor NEW-FLOOR]
                                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                [scale-factor-floor NEW-FLOOR])
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                              (viz-state-scale-factor a-vs))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor a-vs))]
                    [else (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                     [imgs new-imgs]
                                                                     [curr-image new-pimgs-img]
                                                                     [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                       (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                                     [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                     [scale-factor-floor NEW-FLOOR])
                                                        (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                   (viz-state-scale-factor a-vs))
                                                        new-pimgs-img
                                                        (viz-state-scale-factor a-vs))]))
            (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-pimgs-img]
                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
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
      (let* [(new-imgs (zipper-to-begin (viz-state-imgs a-vs)))
             (new-pimgs-img ((zipper-current new-imgs)))
             (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
        (if (does-img-need-resizing? new-pimgs-img  )
            (let [(NEW-FLOOR (min (second img-resize) (third img-resize)))]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                     (let [(new-viz-state (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-pimgs-img]
                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                       [scale-factor DEFAULT-ZOOM-CAP]
                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                       [scale-factor-floor NEW-FLOOR]))]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)))]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (let [(new-viz-state (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-pimgs-img]
                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                       [scale-factor NEW-FLOOR]
                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                       [scale-factor-floor NEW-FLOOR]))]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)))]
                    [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                            [imgs new-imgs]
                                                            [curr-image new-pimgs-img]
                                                            [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                              (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                            [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                            [scale-factor-floor NEW-FLOOR]))]
                            (reposition-out-of-bounds-img new-viz-state
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                     (viz-state-scale-factor new-viz-state))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor new-viz-state)))]))
            (let [(new-viz-state (struct-copy viz-state a-vs
                                              [imgs new-imgs]
                                              [curr-image new-pimgs-img]
                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR]))]
              (reposition-out-of-bounds-img new-viz-state
                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                       (viz-state-scale-factor new-viz-state))
                                            new-pimgs-img
                                            (viz-state-scale-factor new-viz-state)))))))

;; viz-state -> viz-state
;; Purpose: Finishes the derivations in the visualization
(define (go-to-end a-vs)
  (if (zipper-at-end? (viz-state-imgs a-vs))
      a-vs
      (let* [(new-imgs (zipper-to-end (viz-state-imgs a-vs)))
             (new-pimgs-img ((zipper-current new-imgs)))
             (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
        (if (does-img-need-resizing? new-pimgs-img)
            (let [(NEW-FLOOR (min (second img-resize) (third img-resize)))]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                     (let [(new-viz-state (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-pimgs-img]
                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                       [scale-factor DEFAULT-ZOOM-CAP]
                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                       [scale-factor-floor NEW-FLOOR]))]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)))]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (let [(new-viz-state (struct-copy viz-state a-vs
                                                       [imgs new-imgs]
                                                       [curr-image new-pimgs-img]
                                                       [scale-factor NEW-FLOOR]
                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                       [scale-factor-floor NEW-FLOOR]))]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)))]
                    [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                            [imgs new-imgs]
                                                            [curr-image new-pimgs-img]
                                                            [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                              (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                            [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                            [scale-factor-floor NEW-FLOOR]))]
                            (reposition-out-of-bounds-img new-viz-state
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                     (viz-state-scale-factor new-viz-state))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor new-viz-state)))]))
            (let [(new-viz-state (struct-copy viz-state a-vs
                                              [imgs new-imgs]
                                              [curr-image new-pimgs-img]
                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                              [scale-factor-floor DEFAULT-ZOOM-FLOOR]))]
              (reposition-out-of-bounds-img new-viz-state
                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                       (viz-state-scale-factor new-viz-state))
                                            new-pimgs-img
                                            (viz-state-scale-factor new-viz-state)))))))
;; viz-state -> viz-state
;; Purpose: Zooms in on the visualization
(define (zoom-in a-vs) (zoom a-vs ZOOM-INCREASE))

;; viz-state -> Image
;; Returns a image containing all the information regarding what is being derived and what the current yield is
(define (create-instructions a-imsgs)
  (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
          (local [(define DREV
                    (let [(drev-text (text "Deriving: " FONT-SIZE 'black))]
                      (overlay drev-text (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white))))
                  (define YIELD
                    (let [(yield-text (text "Current Yield: " FONT-SIZE 'black))]
                      (overlay yield-text (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white))))
                  (define INPUT-WORD
                    (make-tape-img (imsg-state-input-word a-imsgs)
                                   (if (> (length (imsg-state-input-word a-imsgs)) TAPE-SIZE)
                                       (imsg-state-word-img-offset a-imsgs)
                                       0)))
                  (define YIELD-WORD
                    (let [(normalized-p-yield (if (list? (zipper-current (imsg-state-yield a-imsgs)))
                                                  (zipper-current (imsg-state-yield a-imsgs))
                                                  (list (zipper-current (imsg-state-yield a-imsgs)))))]
                      (make-tape-img normalized-p-yield (if (> (length normalized-p-yield) TAPE-SIZE)
                                                            (imsg-state-word-img-offset a-imsgs)
                                                            0))))
                  (define RULE-USED
                    (if (equal? "" (zipper-current (imsg-state-rules a-imsgs)))
                        ;; Use white so its invisible, makes it so the words dont shift (using an empty string would make the words shift)
                        (text "The rule used: " FONT-SIZE 'white)
                        (text "The rule used: " FONT-SIZE 'black)))
                  (define RULE-USED-WORD
                    (if (equal? "" (zipper-current (imsg-state-rules a-imsgs)))
                        (text "" FONT-SIZE 'white)
                        (beside (text (format "~a" (substring (zipper-current (imsg-state-rules a-imsgs)) 0 1)) FONT-SIZE YIELD-COLOR)
                                (text (format " ~a" (substring (zipper-current (imsg-state-rules a-imsgs)) 1)) FONT-SIZE HEDGE-COLOR))))
                  ;(define INVARIANT-MSG (text "Invariant: " FONT-SIZE 'black))
                  (define INVARIANT-STATE
                    (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                        (text "All invariants hold." FONT-SIZE 'white)
                        (if (empty? (zipper-current (imsg-state-broken-invariants a-imsgs)))
                        (text "All invariants hold." FONT-SIZE 'black)
                        (text (format "~a invariant does not hold." (first (zipper-current (imsg-state-broken-invariants a-imsgs)))) FONT-SIZE 'black))))
                  (define spacer
                    (rectangle (- E-SCENE-WIDTH
                                  (image-width RULE-USED)
                                  (image-width RULE-USED-WORD)
                                  ;(image-width INVARIANT-MSG)
                                  (image-width INVARIANT-STATE)
                                  10)
                               (image-height RULE-USED)
                               'solid
                               'white))
                  (define RULE-YIELD-DREV-LABELS (above/align "right" RULE-USED DREV YIELD))
                  (define WORDS (above/align "left" (beside RULE-USED-WORD spacer
                                                            ;INVARIANT-MSG
                                                            INVARIANT-STATE) INPUT-WORD YIELD-WORD))]
            (beside RULE-YIELD-DREV-LABELS WORDS))))

;; viz-state -> viz-state
;; Purpose: Zooms out the visualization
(define (zoom-out a-vs) (zoom a-vs ZOOM-DECREASE))

;; viz-state -> viz-state
;; Purpose: Zooms all the way in on the visualization
(define (max-zoom-in a-vs) (zoom a-vs (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs))))

;; viz-state -> viz-state
;; Purpose: Zooms in a moderate amount on the visualization
(define (reset-zoom a-vs) (zoom a-vs (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs))))

;; viz-state -> viz-state
;; Purpose: Zooms all the way out in the visualization
(define (max-zoom-out a-vs)
  (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
          (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs))))
      (let [(img-resize (resize-image (viz-state-curr-image a-vs) (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))]
        (zoom a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs))))
      (struct-copy viz-state a-vs
                   [scale-factor DEFAULT-ZOOM])))
  

;; viz-state -> img
;; Returns the the instructions and e-scene-tools images combined into one
(define (create-instructions-and-tools a-imsgs)
  (above (create-instructions a-imsgs)
         (square INS-TOOLS-BUFFER 'solid 'white)
         E-SCENE-TOOLS))

;; viz-state -> viz-state
;; Updates the informative message so it displays the next rule yield
(define (right-key-pressed a-vs)
  (let [(a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs)))]
    (struct-copy viz-state a-vs
                 [informative-messages
                  (struct-copy informative-messages (viz-state-informative-messages a-vs)
                               [component-state
                                (if (zipper-at-end? (imsg-state-rules a-imsgs))
                                    a-imsgs
                                    (struct-copy imsg-state a-imsgs
                                                 [rules (if (zipper-at-end? (imsg-state-rules a-imsgs))
                                                            (imsg-state-rules a-imsgs)
                                                            (zipper-next (imsg-state-rules a-imsgs)))]
                                                 [yield (if (zipper-at-end? (imsg-state-yield a-imsgs))
                                                            (imsg-state-yield a-imsgs)
                                                            (zipper-next (imsg-state-yield a-imsgs)))]
                                                 [broken-invariants (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                                                                        'NO-INV
                                                                        (if (zipper-at-end? (imsg-state-broken-invariants a-imsgs))
                                                                        (imsg-state-broken-invariants a-imsgs)
                                                                        (zipper-next (imsg-state-broken-invariants a-imsgs))))]))])])))

;; viz-state -> viz-state
;; Updates the informative message so it displays the beginning state
(define (up-key-pressed a-vs)
  (let [(a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs)))]
    (struct-copy viz-state a-vs
                 [informative-messages
                  (struct-copy informative-messages (viz-state-informative-messages a-vs)
                               [component-state
                                (if (zipper-at-begin? (imsg-state-rules a-imsgs))
                                    a-imsgs
                                    (struct-copy imsg-state a-imsgs
                                                 [rules (zipper-to-begin (imsg-state-rules a-imsgs))]
                                                 [yield (zipper-to-begin (imsg-state-yield a-imsgs))]
                                                 [broken-invariants (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                                                                        'NO-INV
                                                                        (zipper-to-begin (imsg-state-broken-invariants a-imsgs)))]))])])))

;; viz-state -> viz-state
;; Updates the informative message so it displays the previous rule yield
(define (left-key-pressed a-vs)
  (let [(a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs)))]
    (struct-copy viz-state a-vs
                 [informative-messages
                  (struct-copy informative-messages (viz-state-informative-messages a-vs)
                               [component-state
                                (if (zipper-at-begin? (imsg-state-rules a-imsgs))
                                    a-imsgs
                                    (struct-copy imsg-state a-imsgs
                                                 [yield (if (zipper-at-begin? (imsg-state-yield a-imsgs))
                                                            (imsg-state-yield a-imsgs)
                                                            (zipper-prev (imsg-state-yield a-imsgs)))]
                                                 [rules (if (zipper-at-begin? (imsg-state-rules a-imsgs))
                                                            (imsg-state-rules a-imsgs)
                                                            (zipper-prev (imsg-state-rules a-imsgs)))]
                                                 [broken-invariants (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                                                                        'NO-INV
                                                                        (if (zipper-at-begin? (imsg-state-broken-invariants a-imsgs))
                                                                        (imsg-state-broken-invariants a-imsgs)
                                                                        (zipper-prev (imsg-state-broken-invariants a-imsgs))))]))])])))

;; viz-state -> viz-state
;; Updates the informative message so it displays the ending rule yield
(define (down-key-pressed a-vs)
  (let [(a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs)))]
    (struct-copy viz-state a-vs
                 [informative-messages
                  (struct-copy informative-messages (viz-state-informative-messages a-vs)
                               [component-state
                                (if (zipper-at-end? (imsg-state-rules a-imsgs))
                                    a-imsgs
                                    (struct-copy imsg-state a-imsgs
                                                 [rules (zipper-to-end (imsg-state-rules a-imsgs))]
                                                 [yield (zipper-to-end (imsg-state-yield a-imsgs))]
                                                 [broken-invariants (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                                                                        'NO-INV
                                                                        (zipper-to-end (imsg-state-broken-invariants a-imsgs)))]))])])))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-vs)
  (let [(a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs)))]
    (struct-copy viz-state a-vs
                 [informative-messages
                  (struct-copy informative-messages (viz-state-informative-messages a-vs)
                               [component-state
                                (struct-copy imsg-state a-imsgs
                                             [word-img-offset 0]
                                             [scroll-accum 0])])])))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-vs)
  (let [(a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs)))]
    (struct-copy viz-state a-vs
                 [informative-messages
                  (struct-copy informative-messages (viz-state-informative-messages a-vs)
                               [component-state
                                (struct-copy imsg-state a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-word-img-offset-cap a-imsgs)])])])))

;; node -> img
;; Creates the first img to be displayed since this is always a special case
(define (create-first-img node)
  (lambda () (graph->bitmap (add-node
                             (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                             node
                             #:atb (hash 'color 'black
                                         'shape 'circle
                                         'label node
                                         'fontcolor 'black
                                         'font "Sans")))))

(define (init-viz grammar word w-der rules graphs broken-invariants
                  #:cpu-cores [cpu-cores #f] #:special-graphs? [special-graphs? #f] #:rank-node-lst [rank-node-lst '()])
  (let []
    (run-viz graphs
             (create-first-img (first (first w-der)))
             (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages
              create-instructions
              (imsg-state (list->zipper rules)
                          (list->zipper w-der)
                          word
                          0
                          (let [(offset-cap (- (length word) TAPE-SIZE))]
                            (if (> 0 offset-cap)
                                0
                                offset-cap))
                          0
                          broken-invariants)
              RULE-YIELD-DIMS)
             (instructions-graphic
              E-SCENE-TOOLS
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
             (create-viz-process-key (list (list "right" go-next right-key-pressed)
                                           (list "left" go-prev left-key-pressed)
                                           (list "up" go-to-begin up-key-pressed)
                                           (list "down" go-to-end down-key-pressed)
                                           (list "w" zoom-in identity)
                                           (list "s" zoom-out identity)
                                           (list "r" max-zoom-out identity)
                                           (list "f" max-zoom-in identity)
                                           (list "e" reset-zoom identity)
                                           (list "a" identity a-key-pressed)
                                           (list "d" identity d-key-pressed)
                                           (list "wheel-down" zoom-in identity)
                                           (list "wheel-up" zoom-out identity)
                                           )

)
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                            CLICK-BUFFER-SECONDS
                                            (list (list RULE-YIELD-DIMS
                                                        (lambda (a-imsgs x-diff y-diff)
                                                          (let [(new-scroll-accum (+ (imsg-state-scroll-accum a-imsgs) x-diff))]
                                                            (cond [(and (>= (imsg-state-word-img-offset-cap a-imsgs) (imsg-state-word-img-offset a-imsgs))
                                                                        (<= (quotient (+ (imsg-state-scroll-accum a-imsgs) x-diff) 25) -1))
                                                                   (struct-copy imsg-state a-imsgs
                                                                                [word-img-offset (+ (imsg-state-word-img-offset a-imsgs) 1)]
                                                                                [scroll-accum 0])]
                                                                  [(and (> (imsg-state-word-img-offset a-imsgs) 0)
                                                                        (>= (quotient (+ (imsg-state-scroll-accum a-imsgs) x-diff) 25) 1))
                                                                   (struct-copy imsg-state a-imsgs
                                                                                [word-img-offset (- (imsg-state-word-img-offset a-imsgs) 1)]
                                                                                [scroll-accum 0])]
                                                                  [else (struct-copy imsg-state a-imsgs
                                                                                     [scroll-accum (+ (imsg-state-scroll-accum a-imsgs) x-diff)])])))))
                                            (list (list ARROW-UP-KEY-DIMS go-to-begin up-key-pressed)
                                                  (list ARROW-DOWN-KEY-DIMS go-to-end down-key-pressed)
                                                  (list ARROW-LEFT-KEY-DIMS go-prev left-key-pressed)
                                                  (list ARROW-RIGHT-KEY-DIMS go-next right-key-pressed)
                                                  (list W-KEY-DIMS zoom-in identity)
                                                  (list S-KEY-DIMS zoom-out identity)
                                                  (list R-KEY-DIMS max-zoom-out identity)
                                                  (list E-KEY-DIMS reset-zoom identity)
                                                  (list F-KEY-DIMS max-zoom-in identity)
                                                  (list A-KEY-DIMS identity a-key-pressed)
                                                  (list D-KEY-DIMS identity d-key-pressed)))
             'grammar-viz
             #:cpu-cores cpu-cores
             #:special-graphs? special-graphs?
             #:rank-node-lst rank-node-lst)))