#lang racket

(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base [make-color loc-make-color] [make-pen loc-make-pen])
         2htdp/image
         "../viz-constructors-grammars/viz.rkt"
         "../viz-constructors-grammars/zipper.rkt"
         "../viz-constructors-grammars/bounding-limits.rkt"
         "../viz-constructors-grammars/viz-state.rkt"
         "../viz-constructors-grammars/viz-macros.rkt"
         math/matrix
         "../fsm-core/interface.rkt")

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

(define S-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_s.png"))

(define W-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_w.png"))

(define R-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_r.png"))

(define F-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_f.png"))

(define E-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_e.png"))

(define A-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_a.png"))

(define D-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_d.png"))

(define ARROW-RIGHT-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_right.png"))

(define ARROW-LEFT-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_left.png"))

(define ARROW-UP-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_up.png"))

(define ARROW-DOWN-KEY (bitmap/file "../viz-constructors-grammars/keyboard_key_down.png"))

;; Listof Symbol natnum -> Image
;; Returns an image of a tape of symbols, capable of sliding when its start-index is varied
(define (make-tape-img tape start-index)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let ([letter-imgs
         (build-list
          TAPE-SIZE
          (λ (i)
            (if (< (+ start-index i) (length tape))
                (overlay (text (symbol->string (list-ref tape (+ start-index i))) 24 'black)
                         (overlay (square 25 'solid 'white) (square (add1 25) 'solid 'white)))
                (overlay (square 25 'solid 'white) (square (add1 25) 'solid 'white)))))])
    (make-tape-img letter-imgs start-index)))

(define TAPE-IMG-HEIGHT (image-height (make-tape-img (list 'a) 0)))

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
                          (text "Word end" (- FONT-SIZE 2) 'black))))))

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
                      [(eq? factor ZOOM-INCREASE) (> (viz-state-scale-factor-cap a-vs) new-scale)]
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
  (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
          (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs))))
      (let ([img-resize (resize-image (viz-state-curr-image a-vs)
                                      (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                      (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))])
        (zoom a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs))))
      (struct-copy viz-state a-vs [scale-factor DEFAULT-ZOOM])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

;;symbol (listof rules) -> (listof rules)
;;Purpose: Returns all empty transitions from the start state
(define (get-empties-from-start start lor)
  (append-map (λ (rule)
                (if (and (equal? (first rule) start) (equal? (second rule) EMP))
                    (append (list rule) (get-empties-from-start (third rule) lor))
                    '()))
              lor))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond
    [(empty? prev-path) (append acc curr-path)]
    [(empty? curr-path) prev-path]
    [(equal? (first prev-path) (first curr-path))
     (remove-similarities (rest prev-path) (rest curr-path) acc)]
    [(remove-similarities (rest prev-path) (rest curr-path) (append acc (list (first curr-path))))]))

;;(listof rules) (listof rules) -> (listof rules)
;;Purpose: Appends empty transitions to the end of the path if applicable
(define (attach-empties path lor)
  (if (empty? path)
      path
      (let* ([empty-from-path
              (filter (λ (rule) (and (equal? (last path) (first rule)) (equal? (second rule) EMP)))
                      lor)])
        (if (empty? empty-from-path)
            (append (list path) empty-from-path)
            (append (list path) (attach-empties (first empty-from-path) lor))))))

;;symbol (listof rules) -> (listof states)
;;Purpose: Returns all states that have an empty transitions from the given start state
(define (get-empty-states-from-start start lor)
  (flatten (map (λ (rule)
                  (if (and (equal? (first rule) start) (equal? (second rule) EMP))
                      (cons (third rule) (list (get-empty-states-from-start (third rule) lor)))
                      '()))
                lor)))

;;(listof symbols) (listof rules) symbol -> (listof configurations)
;;Purpose: Returns all possible configurations from start that consume the given word
(define (get-configs a-word lor start)
  (let* (;;(listof states)
         ;;Purpose: A list of states reachable from the start state on an empty transition
         [empty-states-frm-start (cons start (get-empty-states-from-start start lor))]
         ;;(listof configurations)
         ;;Purpose: All configurations from the start state
         [configs (map (λ (state) (append (list state) (list a-word))) empty-states-frm-start)])
    (get-configs-helper configs a-word lor configs)))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) -> (listof configurations)
;;Purpose: Returns all possible configurations using the given word and  (listof rules)
;;Visited = All configurations of the consumed portion of the given word
(define (get-configs-helper configs a-word lor visited)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited (list (first configs)))
              (get-configs-helper (rest configs) a-word lor visited))))

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
    (cond
      [(and (empty? new-config) (empty? new-config-via-emp))
       (list path)] ;;no rules found that connect the config
      [(and (empty? a-word)
            (empty? new-config-via-emp)
            (= (length new-config)
               1)) ;;word and new-config-via-emp is empty and there is exactly 1 new-config
       (make-configs (first new-config)
                     a-word
                     lor
                     (append new-config visited)
                     (append path new-config))]
      [(and (empty? a-word)
            (empty? new-config)
            (= (length new-config-via-emp)
               1)) ;;word and new-config is empty and there is exactly 1 new-config-via-emp
       (make-configs (first new-config-via-emp)
                     a-word
                     lor
                     (append new-config-via-emp visited)
                     (append path new-config-via-emp))]
      [(and
        (empty? a-word)
        (empty?
         new-config-via-emp)) ;;word and new-config-via-emp is empty and there are multiple new-configs
       (make-configs-helper new-config
                            a-word
                            lor
                            (append new-config visited)
                            (append path new-config))]
      [(and (empty? a-word)
            (empty?
             new-config)) ;;word and new-config is empty and there are multiple new-config-via-emp
       (make-configs-helper new-config-via-emp
                            a-word
                            lor
                            (append new-config visited)
                            (append path new-config))]
      [(and (empty? new-config-via-emp)
            (= (length new-config) 1)) ;;new-config-via-emp is empty and there is exactly 1 new-config
       (make-configs (first new-config)
                     (rest a-word)
                     lor
                     (append new-config visited)
                     (append path new-config))]
      [(and (empty? new-config-via-emp)
            (> (length new-config) 1)) ;;new-config-via-emp is empty and there are multiple new-config
       (make-configs-helper new-config
                            (rest a-word)
                            lor
                            (append new-config visited)
                            (append path new-config))]
      [(and (empty? new-config)
            (= (length new-config-via-emp)
               1)) ;;new-config is empty and there is exactly 1 new-config-via-emp
       (make-configs (first new-config-via-emp)
                     a-word
                     lor
                     (append new-config-via-emp visited)
                     (append path new-config-via-emp))]
      [(and (empty? new-config)
            (> (length new-config-via-emp)
               1)) ;;new-config is empty and there are multiple new-config-via-emp
       (make-configs-helper new-config-via-emp
                            a-word
                            lor
                            (append path new-config-via-emp)
                            (append path new-config-via-emp))]
      [else
       (append
        (make-configs-helper new-config ;;there is a combination of new-config and new-config-via-emp
                             (rest a-word)
                             lor
                             (append new-config visited)
                             (append path new-config))
        (make-configs-helper new-config-via-emp
                             a-word
                             lor
                             (append new-config-via-emp visited)
                             (append path new-config-via-emp)))])))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Ensures that all possible configurations are explored
;;Visited = All configurations that consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs-helper configs a-word lor visited path)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited path)
              (make-configs-helper (rest configs) a-word lor visited path))))

;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M)
  (cond
    [(empty? a-word) '()]
    [(not (ormap (λ (config) (empty? (last (last config))))
                 (get-configs a-word (sm-rules M) (sm-start M))))
     (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
    [a-word]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graph machine (listof symbols) symbol (listof symbols) (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M dead held-inv brkn-inv)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color
                                 (if (eq? state (sm-start M)) 'darkgreen 'black)
                                 'style
                                 (cond
                                   [(or (member? state held-inv) (member? state brkn-inv)) 'filled]
                                   [(eq? state dead) 'dashed]
                                   [else 'solid])
                                 'shape
                                 (if (member? state (sm-finals M)) 'doublecircle 'circle)
                                 'fillcolor
                                 (cond
                                   [(member? state held-inv) HELD-INV-COLOR]
                                   [(member? state brkn-inv) BRKN-INV-COLOR]
                                   [else 'white])
                                 'label
                                 state
                                 'fontcolor
                                 'black)))
         cgraph
         (sm-states M)))

;; graph machine word (listof rules) (listof rules) symbol -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M current-rules dead)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color
                                 (if (or (member? rule current-rules)
                                         (ormap (λ (p)
                                                  (and (equal? (first rule) (first p))
                                                       (or (equal? (third rule) (third p))
                                                           (and (equal? (third rule) (third p))
                                                                (equal? (third rule) dead)))))
                                                current-rules))
                                     'violetred
                                     'black)
                                 'fontsize
                                 20
                                 'style
                                 (cond
                                   [(equal? (third rule) dead) 'dashed]
                                   [(or (member? rule current-rules)
                                        (ormap (λ (p)
                                                 (and (equal? (first rule) (first p))
                                                      (equal? (third rule) (third p))))
                                               current-rules))
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
(struct building-viz-state (upci pci M inv dead))
;(struct viz-state-ndfa (upci pci M inv dead imsg instruct graphs))

(struct imsg-state (M upci pci) #:transparent)

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
    (cond
      [(and (> scaled-width max-width) (<= scaled-height max-height))
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

;;image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (last (last config))))
                                   (get-configs (imsg-state-pci imsg-st)
                                                (sm-rules (imsg-state-M imsg-st))
                                                (sm-start (imsg-state-M imsg-st))))]
         ;;for reference, a config(computation) is a pair(list) with a state as the first element and a word as the second
         ;;EX. (S (a b a))
         ;;(listof configurations)
         ;;Purpose: Returns all starting configurations of for the entire word
         [starting-configs
          (map (λ (state)
                 (append (list state)
                         (list (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st)))))
               (cons (sm-start (imsg-state-M imsg-st))
                     (get-empty-states-from-start (sm-start (imsg-state-M imsg-st))
                                                  (sm-rules (imsg-state-M imsg-st)))))]

         ;;(listof configurations)
         ;;Purpose: Returns all configurations using the CI
         [curr-config
          (append-map (λ (config)
                        (filter (λ (configs) (equal? (second configs) (imsg-state-upci imsg-st)))
                                config))
                      (get-configs (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))
                                   (sm-rules (imsg-state-M imsg-st))
                                   (sm-start (imsg-state-M imsg-st))))]

         ;;(listof configurations)
         ;;Purpose: Returns the all configurations using the CI that is one step behind
         [prev-config (cond
                        [(empty? (imsg-state-pci imsg-st)) '()]
                        [(= (length (imsg-state-pci imsg-st)) 1) starting-configs]
                        [else
                         (append-map (λ (config)
                                       (filter (λ (configs)
                                                 (equal? (second configs)
                                                         (cons (last (imsg-state-pci imsg-st))
                                                               (imsg-state-upci imsg-st))))
                                               config))
                                     (get-configs (append (imsg-state-pci imsg-st)
                                                          (imsg-state-upci imsg-st))
                                                  (sm-rules (imsg-state-M imsg-st))
                                                  (sm-start (imsg-state-M imsg-st))))])]

         ;;(listof rules)
         ;;Purpose: The current rules that the ndfa is using to consume the last of the CI
         ;;How: By checking every possible combintion, compares each state in the previous
         ;;     config with the first of the rule,  each state in the current config with
         ;;     the third of the rule, and the first of letter in the word of the previous
         ;;     config with the second of the rule to obtain the current rule(s) being applied
         [current-rules
          (if (empty? (imsg-state-pci imsg-st))
              (get-empties-from-start (sm-start (imsg-state-M imsg-st))
                                      (sm-rules (imsg-state-M imsg-st)))
              (append-map (λ (path) (attach-empties path (sm-rules (imsg-state-M imsg-st))))
                          (remove-duplicates
                           (for*/list ([prev prev-config]
                                       [curr curr-config]
                                       [rule (sm-rules (imsg-state-M imsg-st))]
                                       #:when (and (equal? (first prev) (first rule))
                                                   (equal? (first curr) (third rule))
                                                   (equal? (first (second prev)) (second rule))))
                             rule))))]

         ;;(listof symbols)
         ;;Purpose: The states that the ndfa could be in
         [destin-states (if (not (empty? (imsg-state-pci imsg-st)))
                            (map last current-rules)
                            (cons (sm-start (imsg-state-M imsg-st)) (map last current-rules)))]
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed (imsg-state-pci imsg-st) (imsg-state-M imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (remove-similarities last-consumed-word
                                               (append (imsg-state-pci imsg-st)
                                                       (imsg-state-upci imsg-st))
                                               '())])
    (above/align
     'left
     (cond
       [(and (empty? (imsg-state-pci imsg-st))
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
         (beside
          (text "aaaa" 20 'white)
          (text "Word: " 20 'black)
          (beside
           (text (if (empty? last-consumed-word) "" (los2str last-consumed-word)) 20 'gray)
           (text (los2str (list (first unconsumed-word))) 20 'red)
           (text (if (empty? (rest unconsumed-word)) "" (los2str (rest unconsumed-word))) 20 'black)))
         (beside (text "Consumed: " 20 'black)
                 (text (if (empty? last-consumed-word) "" (los2str last-consumed-word)) 20 'black)))]
       [(empty? (imsg-state-upci imsg-st))
        (above/align 'left
                     (beside (text "aaaa" 20 'white)
                             (text "Word: " 20 'black)
                             (text (los2str (imsg-state-pci imsg-st)) 20 'gray))
                     (beside (text "Consumed: " 20 'black)
                             (text (los2str (imsg-state-pci imsg-st)) 20 'black)))]
       [(empty? (imsg-state-pci imsg-st))
        (above/align 'left
                     (beside (text "aaaa" 20 'white)
                             (text "Word: " 20 'black)
                             (text (los2str (imsg-state-upci imsg-st)) 20 'black))
                     (text "Consumed: " 20 'black))]
       [else
        (above/align 'left
                     (beside (text "aaaa" 20 'white)
                             (text "Word: " 20 'black)
                             (beside (text (los2str (imsg-state-pci imsg-st)) 20 'gray)
                                     (text (los2str (imsg-state-upci imsg-st)) 20 'black)))
                     (beside (text "Consumed: " 20 'black)
                             (text (los2str (imsg-state-pci imsg-st)) 20 'black)))])
     (beside
      (text (format "The number of possible computations is ~a. "
                    (number->string (length destin-states)))
            20
            'brown)
      (text "aaaaa" 20 'white)
      (cond
        [(not completed-config?)
         (text "All computations do not consume the entire word and the machine rejects." 20 'red)]
        [(and (empty? (imsg-state-upci imsg-st))
              (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'accept))
         (text "There is a computation that accepts." 20 'forestgreen)]
        [(and (empty? (imsg-state-upci imsg-st))
              (equal? (sm-apply (imsg-state-M imsg-st) (imsg-state-pci imsg-st)) 'reject))
         (text "All computations end in a non-final state and the machine rejects." 20 'red)]
        [(text "Word Status: accept " 20 'white)]))
     (text "Word Status: accept " 20 'white))))

;;viz-state -> scene
;;Purpose: Draws the given viz-state onto the scene
(define (create-graph-thunk a-vs)
  (let* (;;for reference, a config(computation) is a pair(list) with a state as the first element and a word as the second
         ;;EX. (S (a b a))
         ;;(listof configurations)
         ;;Purpose: Returns all starting configurations of for the entire word
         [starting-configs (map (λ (state)
                                  (append (list state)
                                          (list (append (building-viz-state-pci a-vs)
                                                        (building-viz-state-upci a-vs)))))
                                (cons (sm-start (building-viz-state-M a-vs))
                                      (get-empty-states-from-start
                                       (sm-start (building-viz-state-M a-vs))
                                       (sm-rules (building-viz-state-M a-vs)))))]

         ;;(listof configurations)
         ;;Purpose: Returns all configurations using the CI
         [curr-config
          (append-map
           (λ (config)
             (filter (λ (configs) (equal? (second configs) (building-viz-state-upci a-vs))) config))
           (get-configs (append (building-viz-state-pci a-vs) (building-viz-state-upci a-vs))
                        (sm-rules (building-viz-state-M a-vs))
                        (sm-start (building-viz-state-M a-vs))))]

         ;;(listof configurations)
         ;;Purpose: Returns the all configurations using the CI that is one step behind
         [prev-config (cond
                        [(empty? (building-viz-state-pci a-vs)) '()]
                        [(= (length (building-viz-state-pci a-vs)) 1) starting-configs]
                        [else
                         (append-map (λ (config)
                                       (filter (λ (configs)
                                                 (equal? (second configs)
                                                         (cons (last (building-viz-state-pci a-vs))
                                                               (building-viz-state-upci a-vs))))
                                               config))
                                     (get-configs (append (building-viz-state-pci a-vs)
                                                          (building-viz-state-upci a-vs))
                                                  (sm-rules (building-viz-state-M a-vs))
                                                  (sm-start (building-viz-state-M a-vs))))])]

         ;;(listof rules)
         ;;Purpose: The current rules that the ndfa is using to consume the last of the CI
         ;;How: By checking every possible combintion, compares each state in the previous
         ;;     config with the first of the rule,  each state in the current config with
         ;;     the third of the rule, and the first of letter in the word of the previous
         ;;     config with the second of the rule to obtain the current rule(s) being applied
         [current-rules
          (if (empty? (building-viz-state-pci a-vs))
              (get-empties-from-start (sm-start (building-viz-state-M a-vs))
                                      (sm-rules (building-viz-state-M a-vs)))
              (append-map (λ (path) (attach-empties path (sm-rules (building-viz-state-M a-vs))))
                          (remove-duplicates
                           (for*/list ([prev prev-config]
                                       [curr curr-config]
                                       [rule (sm-rules (building-viz-state-M a-vs))]
                                       #:when (and (equal? (first prev) (first rule))
                                                   (equal? (first curr) (third rule))
                                                   (equal? (first (second prev)) (second rule))))
                             rule))))]
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr curr-config]
                               #:when (equal? (first invs) (first curr)))
                     (list invs (building-viz-state-pci a-vs)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (append-map
                     (λ (inv) (if ((second (first inv)) (second inv)) (list (first (first inv))) '()))
                     get-invs)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs
          (append-map
           (λ (inv) (if (not (member? (first (first inv)) held-invs)) (list (first (first inv))) '()))
           get-invs)])
    (edge-graph (node-graph (create-graph 'ndfagraph #:atb (hash 'rankdir "LR"))
                            (building-viz-state-M a-vs)
                            (building-viz-state-dead a-vs)
                            held-invs
                            brkn-invs)
                (building-viz-state-M a-vs)
                current-rules
                (building-viz-state-dead a-vs))))

(define (create-graph-thunks a-vs acc)
  (cond
    [(empty? (building-viz-state-upci a-vs)) (reverse (cons (create-graph-thunk a-vs) acc))]
    [(not (ormap (λ (config) (empty? (last (last config))))
                 (get-configs (building-viz-state-pci a-vs)
                              (sm-rules (building-viz-state-M a-vs))
                              (sm-start (building-viz-state-M a-vs)))))
     (reverse (cons (create-graph-thunk a-vs) acc))]
    [else
     (let ([next-graph (create-graph-thunk a-vs)])
       (create-graph-thunks (struct-copy building-viz-state
                                         a-vs
                                         [upci (rest (building-viz-state-upci a-vs))]
                                         [pci
                                          (append (building-viz-state-pci a-vs)
                                                  (list (first (building-viz-state-upci a-vs))))])
                            (cons next-graph acc)))]))

(define (right-key-pressed a-vs)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config?
          (ormap (λ (config) (empty? (last (last config))))
                 (get-configs
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs))) ;(viz-state-ndfa-pci a-vs)
                  (sm-rules (imsg-state-M (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
                  (sm-start (imsg-state-M (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))))])
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
                     [upci
                      (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                              (not completed-config?))
                          (imsg-state-upci (informative-messages-component-state
                                            (viz-state-informative-messages a-vs)))
                          (rest (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))))]
                     [pci
                      (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                              (not completed-config?))
                          (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                          (append (imsg-state-pci (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))
                                  (list (first (imsg-state-upci (informative-messages-component-state
                                                                 (viz-state-informative-messages
                                                                  a-vs)))))))])])])))

(define (down-key-pressed a-vs)
  (let* (;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed
                              (append (imsg-state-pci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))
                                      (imsg-state-upci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                              (imsg-state-M (informative-messages-component-state
                                             (viz-state-informative-messages a-vs))))]
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (remove-similarities
                           last-consumed-word
                           (append (imsg-state-pci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs)))
                                   (imsg-state-upci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                           '())])
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
         (informative-messages-component-state (viz-state-informative-messages a-vs))
         [upci
          (cond
            [(empty? (imsg-state-upci (informative-messages-component-state
                                       (viz-state-informative-messages a-vs))))
             (imsg-state-upci (informative-messages-component-state
                               (viz-state-informative-messages a-vs)))]
            [(not (equal? last-consumed-word
                          (append (imsg-state-pci (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))
                                  (imsg-state-upci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))))
             (rest unconsumed-word)]
            [else '()])]
         [pci
          (cond
            [(empty? (imsg-state-upci (informative-messages-component-state
                                       (viz-state-informative-messages a-vs))))
             (imsg-state-pci (informative-messages-component-state
                              (viz-state-informative-messages a-vs)))]
            [(not (equal? last-consumed-word
                          (append (imsg-state-pci (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))
                                  (imsg-state-upci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))))
             (append last-consumed-word (take unconsumed-word 1))]
            [else
             (append (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs)))
                     (imsg-state-upci (informative-messages-component-state
                                       (viz-state-informative-messages a-vs))))])])])])))

(define (left-key-pressed a-vs)
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
                   [upci
                    (if (empty? (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))
                        (imsg-state-upci (informative-messages-component-state
                                          (viz-state-informative-messages a-vs)))
                        (cons (last (imsg-state-pci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                              (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs)))))]
                   [pci
                    (if (empty? (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))
                        (imsg-state-pci (informative-messages-component-state
                                         (viz-state-informative-messages a-vs)))
                        (take (imsg-state-pci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                              (sub1 (length (imsg-state-pci (informative-messages-component-state
                                                             (viz-state-informative-messages
                                                              a-vs)))))))])])]))

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
                   (informative-messages-component-state (viz-state-informative-messages a-vs))
                   [upci
                    (if (empty? (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))
                        (imsg-state-upci (informative-messages-component-state
                                          (viz-state-informative-messages a-vs)))
                        (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))))]
                   [pci
                    (if (empty? (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))
                        (imsg-state-pci (informative-messages-component-state
                                         (viz-state-informative-messages a-vs)))
                        '())])])]))
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-vs)
  a-vs)

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-vs)
  a-vs)

;; process-key
;; viz-state key -> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond
    [(key=? "right" a-key) (right-key-pressed a-vs)]
    [(key=? "left" a-key) (left-key-pressed a-vs)]
    [(key=? "down" a-key) (down-key-pressed a-vs)]
    [(key=? "up" a-key) (up-key-pressed a-vs)]
    [else a-vs]))

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
       ;;(listof rules)
       ;;Purpose: Makes rules for every combination of states in M and symbols in sigma of M
       (define new-rules
         (for*/list ([states (sm-states M)] [sigma (sm-sigma M)])
           (list states sigma states)))
       ;;(listof rules)
       ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
       (define dead-rules
         (for*/list ([ds (list dead)] [sigma (sm-sigma M)])
           (list ds sigma ds)))
       ;;(listof rules)
       ;;Purpose: Gets rules that are not currently in the original rules of M
       (define get-rules-not-in-M (filter (λ (rule) (not (member? rule (sm-rules M)))) new-rules))
       ;;(listof rules)
       ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
       (define rules-to-dead
         (map (λ (rule) (append (list (first rule)) (list (second rule)) (list dead)))
              get-rules-not-in-M))]
      (make-ndfa (append (sm-states M) (list dead))
                 (sm-sigma M)
                 (sm-start M)
                 (sm-finals M)
                 (append (sm-rules M) rules-to-dead dead-rules)))]
    [(and (eq? (sm-type M) 'dfa) (not (member? DEAD (sm-states M))))
     (make-dfa (sm-states M) (sm-sigma M) (sm-start M) (sm-finals M) (sm-rules M))]
    [else M]))

;;ndfa word -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (ndfa-viz M a-word #:add-dead [add-dead #f] . invs)
  (if (not (or (eq? (sm-type M) 'ndfa) (eq? (sm-type M) 'dfa)))
      (error "The given machine must be a ndfa.")
      (let* ([new-M (make-new-M M)]
             [dead-state (cond
                           [(and add-dead (eq? (sm-type M) 'ndfa)) (last (sm-states new-M))]
                           [(and add-dead (eq? (sm-type M) 'dfa)) DEAD]
                           [else 'no-dead])]
             [graphs (create-graph-thunks
                      (building-viz-state a-word '() (if add-dead new-M M) invs dead-state)
                      '())])
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
                                       (imsg-state (if add-dead new-M M) a-word '())
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
                                               (list "wheel-up" zoom-out identity)))
                 (create-viz-process-tick E-SCENE-BOUNDING-LIMITS
                                          NODE-SIZE
                                          E-SCENE-WIDTH
                                          E-SCENE-HEIGHT
                                          CLICK-BUFFER-SECONDS
                                          (list (list RULE-YIELD-DIMS
                                                      (lambda (a-imsgs x-diff y-diff) a-imsgs)))
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
                                                (list D-KEY-DIMS identity d-key-pressed)))))))

(define aa*Uab* (make-ndfa '(K B D) '(a b) 'K '(B D) `((K a D) (K a B) (B a B) (D b D))))

(define AT-LEAST-ONE-MISSING
  (make-ndfa '(S A B C)
             '(a b c)
             'S
             '(A B C)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C) (A b A) (A c A) (B a B) (B c B) (C a C) (C b C))))

(ndfa-viz AT-LEAST-ONE-MISSING '(a b c))

(define p2-ndfa
  (make-ndfa '(S A B C D E)
             '(a b)
             'S
             '(C E)
             `((S ,EMP A) (S ,EMP D) (A a B) (B b A) (A ,EMP C) (C b C) (D a E) (E b E))))

(define AB*B*UAB*
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C) (K a B) (K ,EMP H) (B b K) (C ,EMP H) (H b H))))

(define AB*B*UAB*2
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C) (K a B) (K ,EMP H) (B b K) (C ,EMP H) (H b H) (H a S))))

(define aa-ab (make-ndfa `(S A B F) '(a b) 'S '(A B F) `((S a A) (S a B) (S ,EMP F) (A a A) (B b B))))

(define ends-with-two-bs
  (make-ndfa `(S A B) '(a b) 'S '(B) `((S a S) (S b S) (S b A) (A b B) (B b B))))
(define ENDS-WITH-TWO-Bs
  (make-ndfa `(S A B) '(a b) 'S '(B) `((S a S) (S b A) (A b B) (A a S) (B b B) (B a S))))

(define missing-exactly-one
  (make-ndfa '(S A B C D E F G H I J K L M N O P)
             '(a b c)
             'S
             '(E G I K M O)
             `((S ,EMP A) (S ,EMP B)
                          (S ,EMP C)
                          (A b D)
                          (A c F)
                          (B a H)
                          (B b J)
                          (C a L)
                          (C c N)
                          (D b D)
                          (D c E)
                          (F c F)
                          (F b G)
                          (H a H)
                          (H b I)
                          (J b J)
                          (J a K)
                          (L a L)
                          (L c M)
                          (N c N)
                          (N a O)
                          (E c E)
                          (E b E)
                          (E a P)
                          (G b G)
                          (G c G)
                          (G a P)
                          (I b I)
                          (I a I)
                          (I c P)
                          (K a K)
                          (K b K)
                          (K c P)
                          (M a M)
                          (M c M)
                          (M b P)
                          (O a O)
                          (O c O)
                          (O b P)
                          (P a P)
                          (P b P)
                          (P c P))))

;; L = (aba)* U (ab)*
(define ND
  (make-ndfa '(S A B C D E)
             '(a b)
             'S
             '(S)
             `((S a A) (S a B) (A b C) (B b D) (C a E) (D ,EMP S) (E ,EMP S))))

(define ND2
  (make-ndfa
   '(S A B C D E F)
   '(a b)
   'S
   '(D E)
   `((S ,EMP A) (S ,EMP B) (A ,EMP D) (D b D) (D ,EMP F) (B a E) (B b B) (E a E) (E b E) (E ,EMP C))))

(define ND3
  (make-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) (S ,EMP B) (A a A) (A ,EMP C) (C ,EMP D) (D ,EMP B) (B b B))))

(define ND4 (make-ndfa '(S ds) '(a b) 'S '(ds) `((S a ds) (ds a ds))))

(define EVEN-NUM-Bs
  (make-dfa '(S F) (list 'a 'b) 'S (list 'S) `((S a S) (S b F) (F a F) (F b S)) 'no-dead))

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
;;reject examples
(ndfa-viz AB*B*UAB* '(a b b a))
(ndfa-viz p2-ndfa '(a b b a))
(ndfa-viz missing-exactly-one '(a a a a b b b b a a b b a a c))
(ndfa-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b a))
(ndfa-viz aa-ab '(a b b b a)
;;buggy examples
(ndfa-viz ends-with-two-bs '(a b a b a b b b))
(ndfa-viz ends-with-two-bs '(a b a b a b b a a a b b))
(ndfa-viz p2-ndfa '(a b a b))
(ndfa-viz AB*B*UAB* '(a b a b))
|#

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
  (and (not (empty? a-word)) (equal? (last a-word) 'a)))

;;word -> boolean
;;Purpose: Determines if the given word has one a
(define (C-INV a-word)
  (= (length (filter (λ (w) (equal? w 'a)) a-word)) 1))

;;word -> boolean
;;Purpose: Determines if the given word is empty or if the last letter is an a or b
(define (H-INV a-word)
  (or (empty? a-word) (equal? (last a-word) 'a) (equal? (last a-word) 'b)))

;;word -> boolean
;;Purpose: Determine if the given word has an even number of Bs
(define (EVEN-NUM-Bs-S-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))

;;word -> boolean
;;Purpose: Determine if the given word has an odd number of Bs
(define (EVEN-NUM-Bs-F-INV a-word)
  (odd? (length (filter (λ (w) (equal? w 'b)) a-word))))
