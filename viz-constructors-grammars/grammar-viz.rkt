#lang racket

(require #;(for-syntax "viz-state.rkt"
                       "viz-macros.rkt")
         "viz-macros.rkt"
         2htdp/image
         "zipper.rkt"
         "bounding-limits.rkt"
         "viz.rkt"
         "viz-state.rkt"
         "default-viz-functions.rkt"
         "../fsm-gviz/private/lib.rkt"
         )
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
;(define INS-TOOLS-BUFFER 30)

(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))

(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))

(struct imsg-state (rules yield input-word word-img-offset word-img-offset-cap
                          scroll-accum broken-invariants))

(define S-KEY (bitmap/file "./keyboard_key_s.png"))

(define W-KEY (bitmap/file "./keyboard_key_w.png"))

(define R-KEY (bitmap/file "./keyboard_key_r.png"))

(define F-KEY (bitmap/file "./keyboard_key_f.png"))

(define E-KEY (bitmap/file "./keyboard_key_e.png"))

(define A-KEY (bitmap/file "./keyboard_key_a.png"))

(define D-KEY (bitmap/file "./keyboard_key_d.png"))

(define ARROW-RIGHT-KEY (bitmap/file "./keyboard_key_right.png"))

(define ARROW-LEFT-KEY (bitmap/file "./keyboard_key_left.png"))

(define ARROW-UP-KEY (bitmap/file "./keyboard_key_up.png"))

(define ARROW-DOWN-KEY (bitmap/file "./keyboard_key_down.png"))

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
                  (define INVARIANT-MSG (text "Invariant: " FONT-SIZE 'black))
                  (define INVARIANT-STATE
                    (if (empty? (zipper-current (imsg-state-broken-invariants a-imsgs)))
                        (text "All hold" FONT-SIZE 'black)
                        (text (format "~a's invariant broke" (first (zipper-current (imsg-state-broken-invariants a-imsgs)))) FONT-SIZE 'black)))
                  (define spacer
                    (rectangle (- E-SCENE-WIDTH
                                  (image-width RULE-USED)
                                  (image-width RULE-USED-WORD)
                                  (image-width INVARIANT-MSG)
                                  (image-width INVARIANT-STATE)
                                  10)
                               (image-height RULE-USED)
                               'solid
                               'white))
                  (define RULE-YIELD-DREV-LABELS (above/align "right" RULE-USED DREV YIELD))
                  (define WORDS (above/align "left" (beside RULE-USED-WORD spacer INVARIANT-MSG INVARIANT-STATE) INPUT-WORD YIELD-WORD))]
            (beside RULE-YIELD-DREV-LABELS WORDS))))



;; viz-state -> img
;; Returns the the instructions and e-scene-tools images combined into one
(define (create-instructions-and-tools a-imsgs)
  (above (create-instructions a-imsgs)
         (square INS-TOOLS-BUFFER 'solid 'white)
         E-SCENE-TOOLS))

(define (right-key-pressed a-imsgs)
  (struct-copy imsg-state a-imsgs
               [rules (if (zipper-at-end? (imsg-state-rules a-imsgs))
                          (imsg-state-rules a-imsgs)
                          (zipper-next (imsg-state-rules a-imsgs))
                          )]
               [yield (if (zipper-at-end? (imsg-state-yield a-imsgs))
                          (imsg-state-yield a-imsgs)
                          (zipper-next (imsg-state-yield a-imsgs)))]
               [broken-invariants (if (zipper-at-end? (imsg-state-broken-invariants a-imsgs))
                                      (imsg-state-broken-invariants a-imsgs)
                                      (zipper-next (imsg-state-broken-invariants a-imsgs)))]
               )
  )

(define (up-key-pressed a-imsgs)
  (struct-copy imsg-state a-imsgs
               [rules (zipper-to-begin (imsg-state-rules a-imsgs))]
               [yield (zipper-to-begin (imsg-state-yield a-imsgs))]
               [broken-invariants (zipper-to-begin (imsg-state-broken-invariants a-imsgs))]
               )
  )

(define (left-key-pressed a-imsgs)
  (struct-copy imsg-state a-imsgs
               [yield (if (zipper-at-begin? (imsg-state-yield a-imsgs))
                          (imsg-state-yield a-imsgs)
                          (zipper-prev (imsg-state-yield a-imsgs)))]
               [rules (if (zipper-at-begin? (imsg-state-rules a-imsgs))
                          (imsg-state-rules a-imsgs)
                          (zipper-prev (imsg-state-rules a-imsgs))
                          )]
               [broken-invariants (if (zipper-at-begin? (imsg-state-broken-invariants a-imsgs))
                                      (imsg-state-broken-invariants a-imsgs)
                                      (zipper-prev (imsg-state-broken-invariants a-imsgs))
                                      )]
               )
  )

(define (down-key-pressed a-imsgs)
  (struct-copy imsg-state a-imsgs
               [rules (zipper-to-end (imsg-state-rules a-imsgs))]
               [yield (zipper-to-end (imsg-state-yield a-imsgs))]
               [broken-invariants (zipper-to-end (imsg-state-broken-invariants a-imsgs))]
               )
  )

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-imsgs)
  (struct-copy imsg-state a-imsgs
               [word-img-offset 0]
               [scroll-accum 0]))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-imsgs)
  (struct-copy imsg-state a-imsgs
               [scroll-accum 0]
               [word-img-offset (imsg-state-word-img-offset-cap a-imsgs)]))

#;(rules yield input-word word-img-offset word-img-offset-cap
         scroll-accum broken-invariants)

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
             (informative-messages (create-imsg-process-key (list (list "a" a-key-pressed)
                                                                  (list "d" d-key-pressed)
                                                                  (list "up" up-key-pressed)
                                                                  (list "down" down-key-pressed)
                                                                  (list "left" left-key-pressed)
                                                                  (list "right" right-key-pressed)
                                                                  )
                                                            )
                                   (create-imsg-process-tick
                                    (list (list RULE-YIELD-DIMS (lambda (a-imsgs x-diff y-diff)
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
                                                                      
                                                                                             [scroll-accum (+ (imsg-state-scroll-accum a-imsgs) x-diff)])])
                                                                    )
                                                                  )
                                                )
                                          )
                                    )
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
                                   RULE-YIELD-DIMS
                                   )
             (instructions-graphic RULE-YIELD-DIMS
                                   E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       E-SCENE-HEIGHT
                                                       (bounding-limits-height RULE-YIELD-DIMS)
                                                       INS-TOOLS-BUFFER
                                                       )
                                                    ;(+ E-SCENE-HEIGHT (bounding-limits-height RULE-YIELD-DIMS))
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       E-SCENE-HEIGHT
                                                       (bounding-limits-height RULE-YIELD-DIMS)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))
                                                    #;(+ E-SCENE-HEIGHT (bounding-limits-height RULE-YIELD-DIMS) (image-height E-SCENE-TOOLS))
                                                    )
                                     
                                   (create-instructions-process-tick
                                    CLICK-BUFFER-SECONDS
                                    (list (list ARROW-UP-KEY-DIMS
                                                (lambda (a-vs)
                                                  ((go-to-begin E-SCENE-WIDTH
                                                                E-SCENE-HEIGHT
                                                                PERCENT-BORDER-GAP
                                                                DEFAULT-ZOOM-CAP
                                                                DEFAULT-ZOOM-FLOOR
                                                                NODE-SIZE)
                                                   (struct-copy viz-state a-vs
                                                                [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                                                   [component-state (up-key-pressed (informative-messages-component-state (viz-state-informative-messages a-vs))
                                                                                                                          
                                                                                                                                    )]
                                                                                                   )
                                                                                      ]
                                                                )
                                                   )
                                                  )
                                                )
                                          (list ARROW-DOWN-KEY-DIMS
                                                (lambda (a-vs)
                                                  ((go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
                                                   (struct-copy viz-state a-vs
                                                                [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                                                   [component-state (down-key-pressed (informative-messages-component-state (viz-state-informative-messages a-vs))
                                                                                                                          
                                                                                                                                      )]
                                                                                                   )
                                                                                      ]
                                                                )
                                                   )
                                                  )
                                                )
                                          (list ARROW-LEFT-KEY-DIMS
                                                (lambda (a-vs)
                                                  ((go-prev E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
                                                   (struct-copy viz-state a-vs
                                                                [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                                                   [component-state (left-key-pressed (informative-messages-component-state (viz-state-informative-messages a-vs))
                                                                                                                          
                                                                                                                                      )]
                                                                                                   )
                                                                                      ]
                                                                )
                                                   )
                                                  )
                                                )
                                          (list ARROW-RIGHT-KEY-DIMS
                                                (lambda (a-vs)
                                                  ((go-next E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
                                                   (struct-copy viz-state a-vs
                                                                [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                                                   [component-state (right-key-pressed (informative-messages-component-state (viz-state-informative-messages a-vs))
                                                                                                                          
                                                                                                                                       )]
                                                                                                   )
                                                                                      ]
                                                                )
                                                   )
                                                  )
                                                )
                                          (list W-KEY-DIMS (create-zoom-in ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                          (list S-KEY-DIMS (create-zoom-out ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                          (list R-KEY-DIMS (create-max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP
                                                                                ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM NODE-SIZE))
                                          (list E-KEY-DIMS (create-reset-zoom ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                          (list F-KEY-DIMS (create-max-zoom-in ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                          (list A-KEY-DIMS (lambda (a-vs)
                                                             (struct-copy viz-state a-vs
                                                                          [informative-messages
                                                                           (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                                        [component-state (a-key-pressed
                                                                                                          (informative-messages-component-state
                                                                                                           (viz-state-informative-messages a-vs)))
                                                                                                         ]
                                                                                        )
                                                                           ]
                                                                                         
                                                                          )
                                                                            
                                                             )
                                                )
                                          (list D-KEY-DIMS (lambda (a-vs)
                                                             (struct-copy viz-state a-vs
                                                                          [informative-messages
                                                                           (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                                        [component-state (d-key-pressed
                                                                                                          (informative-messages-component-state
                                                                                                           (viz-state-informative-messages a-vs)))
                                                                                                         ]
                                                                                        )
                                                                           ]
                                                                                         
                                                                          )
                                                                            
                                                             )
                                                )
                                          )
                                    )
                                   
                                   )
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key (list (list "right" (go-next E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE))
                                           (list "left" (go-prev  E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE))
                                           (list "up" (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE))
                                           (list "down" (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE))
                                           (list "w" (create-zoom-in ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                           (list "s" (create-zoom-out ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                           (list "r" (create-max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP
                                                                          ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM NODE-SIZE))
                                           (list "f" (create-max-zoom-in ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                           (list "e" (create-reset-zoom ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                           (list "wheel-down" (create-zoom-in ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                           (list "wheel-up" (create-zoom-out ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                                           )
                                     )
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
             #:cpu-cores cpu-cores
             #:special-graphs? special-graphs?
             #:rank-node-lst rank-node-lst)
    )
  )