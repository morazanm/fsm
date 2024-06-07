#lang racket
(require 2htdp/universe
         2htdp/image
         "../fsm-core/interface.rkt"
         "../fsm-core/private/regular-grammar.rkt"
         math/matrix
         math/array
         "../fsm-gviz/private/parallel.rkt"
         "../fsm-gviz/private/lib.rkt"
         )
(provide run-viz)

;; Bounding limits is a struct containing the bounding X and Y
;; values of where we want the image to be placed
;; num num num num -> bounding-limits
(struct bounding-limits (min-x max-x min-y max-y))

;; posn is a structure that has
;; x coordinate
;; y coordinate
(struct posn (x y))

(define E-SCENE-WIDTH 1250)
(define E-SCENE-HEIGHT 500)
(define E-SCENE (empty-scene E-SCENE-WIDTH E-SCENE-HEIGHT))
(define E-SCENE-CENTER (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define PERCENT-BORDER-GAP 0.9)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))
(define NODE-SIZE 50)
(define FONT-SIZE 20)
(define TAPE-SIZE 42)
(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))

;; Listof Symbol natnum -> Image
;; Returns an image of a tape of symbols, capable of sliding when its start-index is varied
(define (make-tape-img tape start-index)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi)
                (make-tape-img (rest loi) (add1 start-index)))
        )
    )
  (let [(letter-imgs (build-list TAPE-SIZE
                                 (λ (i) (if (< (+ start-index i) (length tape))
                                            (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                           24
                                                           'black)
                                                     (overlay (square 25 'solid 'white)
                                                              (square (add1 25) 'solid 'white)))
                                            (overlay (square 25 'solid 'white)
                                                     (square (add1 25) 'solid 'white)))))
                     )
        ]
    (make-tape-img letter-imgs start-index)
    )
  )

(define TAPE-IMG-HEIGHT (image-height (make-tape-img (list 'a) 0)))
(define RULE-YIELD-DIMS
  (let [
        (DREV (let [(drev-text (text "Deriving: " FONT-SIZE 'black))]
                (overlay drev-text (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white))
                ))
        (YIELD (let [(yield-text (text "Current Yield: " FONT-SIZE 'black))]
                 (overlay yield-text (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white))
                 ))
        (RULE-USED (text "The rule used: " FONT-SIZE 'black))
        ]
    (bounding-limits (+ (image-width (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)) (image-width (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                                                                                        (above/align "right" RULE-USED DREV YIELD)
                                                                                                        )
                                                                                                )
                        )
                     (* E-SCENE-WIDTH 0.9)
                     (+ E-SCENE-HEIGHT)
                     (+ E-SCENE-HEIGHT (image-height (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                                             (above/align "right" RULE-USED DREV YIELD)
                                                             )
                                                     )
                        )
                     )
    )
  )

(define (bounding-limits-height b-limit) (- (bounding-limits-max-y b-limit) (bounding-limits-min-y b-limit)))
(define (bounding-limits-width b-limit) (- (bounding-limits-max-x b-limit) (bounding-limits-min-x b-limit)))
(define (within-bounding-limits? b-limit a-posn)
  (and (<= (bounding-limits-min-y b-limit) (posn-y a-posn))
       (>= (bounding-limits-max-y b-limit) (posn-y a-posn))
       (<= (bounding-limits-min-x b-limit) (posn-x a-posn))
       (>= (bounding-limits-max-x b-limit) (posn-x a-posn))
       )
  )

;; viz-state is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
;; curr-image - current image to be drawn to the screen
;; image-posn - position of the graph image
;; scale-factor - mulitplicative factor used to scale the image while zooming
;; scale-factor-cap - maximum value for scale-factor
;; scale-factor-floor - minimum value for scale-factor
;; curr-mouse-posn - position of the mouse
;; dest-mouse-posn - position where the mouse is dragged
;; mouse-pressed - boolean indicating whether the mouse is pressed
;; up-dgraph - unprocessed dgraphs
;; p-dgraph - processed dgraphs
;; up-yield - unprocessed yield
;; p-yield - processed yield
;; input-word - The word given by the user to visualize
;; word-img-offset - an index into the tape img used to display a subset of the word that will fit on the screen
;; word-img-offset-cap - the largest maximum value of word-img-offset
;; scroll-accum - save the amount that the user has scrolled the instructions (allows smooth scrolling while allowing the discrete movements of the tape)
(struct viz-state (upimgs pimgs curr-image image-posn
                          scale-factor scale-factor-cap scale-factor-floor
                          curr-mouse-posn dest-mouse-posn mouse-pressed
                          up-rules p-rules up-yield p-yield
                          input-word word-img-offset word-img-offset-cap scroll-accum
                          click-buffer
                          )
  )

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

(define cursor
  (let (
        (cursor-rect (let (
                           (inner-white (rectangle 5
                                                   17.5
                                                   'solid
                                                   'white)
                                        )
                           (outer-black (rectangle 9
                                                   20
                                                   'solid
                                                   'black)
                                        )
                           (white-triangle-infill (rectangle 9 5 'solid 'white))
                           )
                       (above white-triangle-infill
                              (overlay/xy inner-white
                                          -2
                                          0
                                          outer-black
                                          )
                              )
                       )
                     )
        (cursor-tri (let
                        (
                         (inner-white (overlay/align/offset "right"
                                                            "middle"
                                                            (rotate 250
                                                                    (overlay/align/offset "middle"
                                                                                          "bottom"
                                                                                          (triangle/aas 30
                                                                                                        30
                                                                                                        44
                                                                                                        'solid
                                                                                                        'white
                                                                                                        )
                                                                                          0
                                                                                          3
                                                                                          (triangle/aas 30
                                                                                                        30
                                                                                                        48
                                                                                                        'solid
                                                                                                        'black
                                                                                                        )
                                                                                          )
                                                                    )
                                                            -2
                                                            -1
                                                            (triangle/aas 38.94
                                                                          70.54
                                                                          74
                                                                          'solid
                                                                          'white
                                                                          )
                                                            )
                                      )
                         (outer-black (overlay/align/offset "right"
                                                            "middle"
                                                            (rotate 250
                                                                    (triangle/aas 30
                                                                                  30
                                                                                  60
                                                                                  'solid
                                                                                  'white
                                                                                  )
                                                                    )
                                                            -1
                                                            -1
                                                            (triangle/sss 60
                                                                          90
                                                                          90
                                                                          'solid
                                                                          'black
                                                                          )
                                                            )
                                      )
                         )
                      (scale 0.5
                             (rotate 310
                                     (overlay/xy inner-white
                                                 -9
                                                 -3
                                                 outer-black
                                                 )
                                     )
                             )
                      )
                          
                    )
        )
    (overlay/xy (rotate 25 cursor-rect)
                -7
                -26
                cursor-tri
                )
    )
  )

(define HEIGHT-BUFFER 20)
(define LETTER-KEY-WIDTH-BUFFER 20)
(define ARROW-KEY-WIDTH-BUFFER 40)
(define INS-TOOLS-BUFFER 30)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define E-SCENE-TOOLS
  (let (
        (ARROW (above (triangle 30 'solid 'black)
                      (rectangle 10 30 'solid 'black))
               )
        )
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

                          (above/align "middle"  D-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Word end" (- FONT-SIZE 2) 'black))
                          )
                  )
    )
  )

(define ARROW-UP-KEY-DIMS
  (bounding-limits (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Restart" (- FONT-SIZE 2) 'black)) (image-width ARROW-UP-KEY)) 2)
                      )
                   (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Restart" (- FONT-SIZE 2) 'black)) (image-width ARROW-UP-KEY)) 2)
                      (image-width ARROW-UP-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-UP-KEY)
                      )
                   )
  )
(define ARROW-RIGHT-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Forward" (- FONT-SIZE 2) 'black)) (image-width ARROW-RIGHT-KEY)) 2)
                      )
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Forward" (- FONT-SIZE 2) 'black)) (image-width ARROW-RIGHT-KEY)) 2)
                      (image-width ARROW-RIGHT-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-RIGHT-KEY)
                      )
                   )
  )
(define ARROW-LEFT-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Backward" (- FONT-SIZE 2) 'black)) (image-width ARROW-LEFT-KEY)) 2)
                      )
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Backward" (- FONT-SIZE 2) 'black)) (image-width ARROW-LEFT-KEY)) 2)
                      (image-width ARROW-LEFT-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-LEFT-KEY)
                      )
                   )
  )

(define ARROW-DOWN-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Finish" 18 'black)) (image-width ARROW-DOWN-KEY)) 2)
                      )
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Finish" (- FONT-SIZE 2) 'black)) (image-width ARROW-DOWN-KEY)) 2)
                      (image-width ARROW-DOWN-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-DOWN-KEY)
                      )
                   )
  )

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
                      (/ (- (image-width (text "Zoom in" (- FONT-SIZE 2) 'black)) (image-width W-KEY)) 2)
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
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom in" (- FONT-SIZE 2) 'black)) (image-width W-KEY)) 2)
                      (image-width W-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height W-KEY)
                      )
                   )
  )

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
                      (/ (- (image-width (text "Zoom out" (- FONT-SIZE 2) 'black)) (image-width S-KEY)) 2)
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
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom out" (- FONT-SIZE 2) 'black)) (image-width S-KEY)) 2)
                      (image-width S-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height S-KEY)
                      )
                   )
  )

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
                      (/ (- (image-width (text "Min zoom" (- FONT-SIZE 2) 'black)) (image-width R-KEY)) 2)
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
                      
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black)) (image-width R-KEY)) 2)
                      (image-width R-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height R-KEY)
                      )
                   )
  )

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
                      (/ (- (image-width (text "Mid zoom" (- FONT-SIZE 2) 'black)) (image-width E-KEY)) 2)
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
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black)) (image-width E-KEY)) 2)
                      (image-width E-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height E-KEY)
                      )
                   )
  )

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
                      (/ (- (image-width (text "Max zoom" (- FONT-SIZE 2) 'black)) (image-width F-KEY)) 2)
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
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black)) (image-width F-KEY)) 2)
                      (image-width F-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height F-KEY)
                      )
                   )
  )

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
                      (/ (- (image-width (text "Word start" (- FONT-SIZE 2) 'black)) (image-width A-KEY)) 2)
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
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Word start" (- FONT-SIZE 2) 'black)) (image-width A-KEY)) 2)
                      (image-width A-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height A-KEY)
                      )
                   )
  )
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
                      (/ (- (image-width (text "Word end" (- FONT-SIZE 2) 'black)) (image-width D-KEY)) 2)
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
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Word end" (- FONT-SIZE 2) 'black)) (image-width D-KEY)) 2)
                      (image-width D-KEY)
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      )
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height D-KEY)
                      )
                   )
  )

;; viz-state -> Image
;; Returns a image containing all the information regarding what is being derived and what the current yield is
(define (create-instructions a-vs)
  (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
          (local [
                  (define DREV (let [(drev-text (text "Deriving: " FONT-SIZE 'black))]
                                 (overlay drev-text (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white))
                                 )
                    )
                  (define YIELD (let [(yield-text (text "Current Yield: " FONT-SIZE 'black))]
                                  (overlay yield-text (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white))
                                  )
                    )
                  (define INPUT-WORD (make-tape-img (viz-state-input-word a-vs) (if (> (length (viz-state-input-word a-vs)) TAPE-SIZE)
                                                                                    (viz-state-word-img-offset a-vs)
                                                                                    0
                                                                                    )
                                                    )
                    )
                  (define YIELD-WORD
                    (let [
                          (normalized-p-yield (if (list? (first (viz-state-p-yield a-vs)))
                                                  (first (viz-state-p-yield a-vs))
                                                  (list (first (viz-state-p-yield a-vs)))
                                                  )
                                              )
                          ]
                      (make-tape-img normalized-p-yield (if (> (length normalized-p-yield) TAPE-SIZE)
                                                            (viz-state-word-img-offset a-vs)
                                                            0
                                                            )
                                     )
                      )
                    )
                  (define RULE-USED (if (equal? "" (first (viz-state-p-rules a-vs)))
                                        ;; Use white so its invisible, makes it so the words dont shift (using an empty string would make the words shift)
                                        (text "The rule used: " FONT-SIZE 'white)
                                        (text "The rule used: " FONT-SIZE 'black)
                                        )
                    )
                  (define RULE-USED-WORD (if (equal? "" (first (viz-state-p-rules a-vs)))
                                             (text "" FONT-SIZE 'white)
                                             (beside (text (format "~a" (substring (first (viz-state-p-rules a-vs)) 0 1)) FONT-SIZE YIELD-COLOR)
                                                     (text (format " ~a" (substring (first (viz-state-p-rules a-vs)) 1)) FONT-SIZE HEDGE-COLOR)
                                                     )
                                             )
                    )
                                              
                  (define RULE-YIELD-DREV-LABELS (above/align "right" RULE-USED DREV YIELD))
                  (define WORDS (above/align "left" RULE-USED-WORD INPUT-WORD YIELD-WORD))
                  ]
            (beside RULE-YIELD-DREV-LABELS WORDS)
            )
          )
  )

;; viz-state -> img
;; Returns the the instructions and e-scene-tools images combined into one
(define (create-instructions-and-tools a-vs)
  (above (create-instructions a-vs)
         (square INS-TOOLS-BUFFER 'solid 'white)
         E-SCENE-TOOLS
         )
  )

;; upper?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is upper case
(define (upper? symbol)
  (char-upper-case? (string-ref (symbol->string symbol) 0)))

;; lower?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is down case
(define (lower? symbol)
  (not (char-upper-case? (string-ref (symbol->string symbol) 0))))

;; create-graph-imgs
;; (listof dgraph) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-graph-imgs graphs #:cpu-cores [cpu-cores #f])
  (if (empty? graphs)
      '()
      (if (not cpu-cores)
          (parallel-graphs->bitmap-thunks graphs)
          (parallel-graphs->bitmap-thunks graphs #:cpu-cores cpu-cores)
          )
      )
  )

;; resize-image :: image -> int -> int -> image
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
              (<= scaled-height max-height)
              )
         (list (scale/xy
                (/ max-width src-width)
                (/ (/ scaled-width aspect) src-height)
                img)
               (/ max-width src-width)
               (/ (/ scaled-width aspect) src-height)
               )
         ]
        [(and (<= scaled-width max-width)
              (> scaled-height max-height)
              )
         (let ([scaled-aspect (/ scaled-width scaled-height)])
           (list (scale/xy
                  (/ (* scaled-height scaled-aspect) src-width)
                  (/ max-height src-height)
                  img)
                 (/ (* scaled-height scaled-aspect) src-width)
                 (/ max-height src-height)
                 )
           )
         ]
        [(and (> scaled-width max-width)
              (> scaled-height max-height)
              )
         (let* (
                [new-scaled-height (/ max-width aspect)]
                [scaled-aspect (/ max-width new-scaled-height)]
                )
           (list (scale/xy
                  (/ (* max-height scaled-aspect) src-width)
                  (/ max-height src-height)
                  img)
                 (/ (* max-height scaled-aspect) src-width)
                 (/ max-height src-height)
                 )
           )
         ]
        [(and (<= scaled-width max-width)
              (<= scaled-height max-height)
              )
         (list (scale/xy
                (/ scaled-width src-width)
                (/ scaled-height src-height)
                img)
               (/ scaled-width src-width)
               (/ scaled-height src-height)
               )
         ]
        )
  )

;; img num>0 -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define (calculate-viewport-limits scaled-image scale)
  (let* [
         (img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale)))
         (img-height-node-diff (- (/ (image-height scaled-image) 2) (* NODE-SIZE scale)))
         (scaled-node-size (* NODE-SIZE scale))
         (MIN-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                    (- (* -1 (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH)) (- E-SCENE-WIDTH scaled-node-size) )
                    (* -1 img-width-node-diff)
                    )
                )
         (MAX-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                    (+ (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH) E-SCENE-WIDTH (- E-SCENE-WIDTH scaled-node-size) )
                    (+ E-SCENE-WIDTH img-width-node-diff)
                    )
                )
                                                               
         (MIN-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                    (- (* -1 (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT)) (- E-SCENE-HEIGHT scaled-node-size))
                    (* -1 img-height-node-diff)
                    )
                )
         (MAX-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                    (+ (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT) E-SCENE-HEIGHT (- E-SCENE-HEIGHT scaled-node-size))
                    (+ E-SCENE-HEIGHT img-height-node-diff)
                    )
                )
         ]
    (bounding-limits MIN-X MAX-X MIN-Y MAX-Y)
    )
  )

;; viz-state viewport-limits img num>0 -> viz-state
;; Returns a new viz-state where if the given image would be out of bounds of its viewport limits
;; It is placed into a position inbounds
(define (reposition-out-of-bounds-img a-vs viewport-lims new-img new-scale)
  (let
      (
       (MIN-X (bounding-limits-min-x viewport-lims))
       (MAX-X (bounding-limits-max-x viewport-lims))
       (MIN-Y (bounding-limits-min-y viewport-lims))
       (MAX-Y (bounding-limits-max-y viewport-lims))
       )
    (cond [(and (> MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MIN-X (posn-y (viz-state-image-posn a-vs)))
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )                           
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (> (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MAX-X (posn-y (viz-state-image-posn a-vs)))
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )                   
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (> MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn (posn-x (viz-state-image-posn a-vs)) MIN-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (> (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn (posn-x (viz-state-image-posn a-vs)) MAX-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          [(and (> MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (> MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MIN-X MIN-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          [(and (> MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (> (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MIN-X MAX-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (> (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (> MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MAX-X MIN-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (> (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (> (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MAX-X MAX-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (viz-state-image-posn a-vs)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-rules a-vs)
                      (viz-state-p-rules a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      (viz-state-scroll-accum a-vs)
                      (viz-state-click-buffer a-vs)
                      )
           ]
          )
    )
  )

;; num num num num boolean num num num (matrix [ [x] [y] [1] ]) -> (matrix [ [transformed-x] [transformed-y] [1] ])
;; Transforms a given a point matrix based on the arguments provided
(define (affine-transform #:x-translate [x-translate 0]
                          #:y-translate [y-translate 0]
                          #:x-scale [x-scale 1]
                          #:y-scale [y-scale 1]
                          #:reflect [reflect #f]
                          #:rotate [rotate 0]
                          #:x-shear [x-shear 0]
                          #:y-shear [y-shear 0]
                          #:point point)
  (let* [
         (reflection (if reflect
                         -1
                         1
                         )
                     )
         (result (matrix* (matrix [
                                   [(* reflection x-scale (cos rotate)) (* x-shear (* -1 (sin rotate))) x-translate ]
                                   [(* (sin rotate) y-shear) (* y-scale (cos rotate)) y-translate]
                                   [0 0 1]
                                   ]
                                  )
                          point
                          )
                 )
         ]
    result
    )
  )

;; img posn num>0 -> matrix x y 1
;; Calculates the transform needed to zoom correctly
(define (zoom-affine-transform img img-posn scale)
  (let* [
         (transformed-x (* -1 (+ (- (/ E-SCENE-WIDTH 2)
                                    (+ (posn-x img-posn) (/ (image-width img) 2))
                                    )
                                 (/ (image-width img) 2)
                                 )
                           )
                        )
         (transformed-y (* -1 (+ (- (/ E-SCENE-HEIGHT 2)
                                    (+ (posn-y img-posn) (/ (image-height img) 2))
                                    )
                                 (/ (image-height img) 2)
                                 )
                           )
                        )
         ]
    (affine-transform #:x-translate (* -1 transformed-x)
                      #:y-translate (* -1 transformed-y)
                      #:point (affine-transform #:x-scale scale
                                                #:y-scale scale
                                                #:point (affine-transform #:x-translate transformed-x
                                                                          #:y-translate transformed-y
                                                                          #:point (matrix [ [0] [0] [1] ])
                                                                          )
                                                )
                      )
    )
  )

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
(define (zoom a-vs factor)
  (let*  [
          (new-scale (* factor (viz-state-scale-factor a-vs)))
          (scalable? (cond [(eq? factor ZOOM-INCREASE) (> (viz-state-scale-factor-cap a-vs) new-scale)]
                           [(eq? factor ZOOM-DECREASE) (< (viz-state-scale-factor-floor a-vs) new-scale)]
                           )
                     )
          ]
    (if scalable?
        (let* [
               (scaled-image (scale new-scale (viz-state-curr-image a-vs)))
               (viewport-lims (calculate-viewport-limits scaled-image new-scale))
               (scale-increase (/ new-scale (viz-state-scale-factor a-vs)))
               (affine-matrix (zoom-affine-transform (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)) (viz-state-image-posn a-vs) scale-increase))             
               ]
          (reposition-out-of-bounds-img (viz-state (viz-state-upimgs a-vs)
                                                   (viz-state-pimgs a-vs)
                                                   (viz-state-curr-image a-vs)
                                                   (posn (+ (posn-x (viz-state-image-posn a-vs)) (matrix-ref affine-matrix 0 0)) (+ (posn-y (viz-state-image-posn a-vs)) (matrix-ref affine-matrix 1 0)))
                                                   new-scale
                                                   (viz-state-scale-factor-cap a-vs)
                                                   (viz-state-scale-factor-floor a-vs)
                                                   (viz-state-curr-mouse-posn a-vs)
                                                   (viz-state-dest-mouse-posn a-vs)
                                                   (viz-state-mouse-pressed a-vs)
                                                   (viz-state-up-rules a-vs)
                                                   (viz-state-p-rules a-vs)
                                                   (viz-state-up-yield a-vs)
                                                   (viz-state-p-yield a-vs)
                                                   (viz-state-input-word a-vs)
                                                   (viz-state-word-img-offset a-vs)
                                                   (viz-state-word-img-offset-cap a-vs)
                                                   (viz-state-scroll-accum a-vs)
                                                   (viz-state-click-buffer a-vs)
                                                   )
                                        viewport-lims
                                        (viz-state-curr-image a-vs)
                                        new-scale
                                        )
          )
        a-vs
        )
    )
  )

;; viz-state -> viz-state
;; Purpose: Moves the visualization to the next step of the derivation
(define (right-key-pressed a-vs)
  (if (empty? (viz-state-upimgs a-vs))
      a-vs
      (let* [
             (new-up-yield (if (empty? (viz-state-up-yield a-vs))
                               '()
                               (rest (viz-state-up-yield a-vs))))
             (new-p-yield (if (empty? (viz-state-up-yield a-vs))
                              (viz-state-p-yield a-vs)
                              (cons (first (viz-state-up-yield a-vs))
                                    (viz-state-p-yield a-vs))))
             (new-pimgs (cons (first (viz-state-upimgs a-vs))
                              (viz-state-pimgs a-vs)))
             (new-pimgs-img (
                             (first new-pimgs)
                             )
                            )
                    
             (curr-pimgs-img (
                              (first (viz-state-pimgs a-vs))
                              )
                             )
             (new-p-rules (cons (first (viz-state-up-rules a-vs))
                                (viz-state-p-rules a-vs)))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
                    
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                          )
                       )
                    
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                          )
                       )
             ]
        (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                )
            (let
                [
                 (NEW-FLOOR (min (second img-resize) (third img-resize)))
                 ]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                     (let [
                           (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                     new-pimgs
                                                     new-pimgs-img     
                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                     DEFAULT-ZOOM-CAP
                                                     DEFAULT-ZOOM-CAP
                                                     NEW-FLOOR
                                                     (viz-state-curr-mouse-posn a-vs)
                                                     (viz-state-dest-mouse-posn a-vs)
                                                     (viz-state-mouse-pressed a-vs)
                                                     (rest (viz-state-up-rules a-vs))
                                                     new-p-rules
                                                     new-up-yield
                                                     new-p-yield
                                                     (viz-state-input-word a-vs)
                                                     (viz-state-word-img-offset a-vs)
                                                     (viz-state-word-img-offset-cap a-vs)
                                                     (viz-state-scroll-accum a-vs)
                                                     (viz-state-click-buffer a-vs)
                                                     )
                                          )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (let [
                           (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                     new-pimgs
                                                     new-pimgs-img 
                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                     NEW-FLOOR
                                                     DEFAULT-ZOOM-CAP
                                                     NEW-FLOOR
                                                     (viz-state-curr-mouse-posn a-vs)
                                                     (viz-state-dest-mouse-posn a-vs)
                                                     (viz-state-mouse-pressed a-vs)
                                                     (rest (viz-state-up-rules a-vs))
                                                     new-p-rules
                                                     new-up-yield
                                                     new-p-yield
                                                     (viz-state-input-word a-vs)
                                                     (viz-state-word-img-offset a-vs)
                                                     (viz-state-word-img-offset-cap a-vs)
                                                     (viz-state-scroll-accum a-vs)
                                                     (viz-state-click-buffer a-vs)
                                                     )
                                          )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    [else
                     (let [
                           (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                     new-pimgs
                                                     new-pimgs-img
                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                     (viz-state-scale-factor a-vs)
                                                     DEFAULT-ZOOM-CAP
                                                     NEW-FLOOR
                                                     (viz-state-curr-mouse-posn a-vs)
                                                     (viz-state-dest-mouse-posn a-vs)
                                                     (viz-state-mouse-pressed a-vs)
                                                     (rest (viz-state-up-rules a-vs))
                                                     new-p-rules
                                                     new-up-yield
                                                     new-p-yield
                                                     (viz-state-input-word a-vs)
                                                     (viz-state-word-img-offset a-vs)
                                                     (viz-state-word-img-offset-cap a-vs)
                                                     (viz-state-scroll-accum a-vs)
                                                     (viz-state-click-buffer a-vs)
                                                     )
                                          )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    )
              )
            (let [
                  (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                            new-pimgs
                                            new-pimgs-img   
                                            (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                            (viz-state-scale-factor a-vs)
                                            DEFAULT-ZOOM-CAP
                                            DEFAULT-ZOOM-FLOOR
                                            (viz-state-curr-mouse-posn a-vs)
                                            (viz-state-dest-mouse-posn a-vs)
                                            (viz-state-mouse-pressed a-vs)
                                            (rest (viz-state-up-rules a-vs))
                                            new-p-rules
                                            new-up-yield
                                            new-p-yield
                                            (viz-state-input-word a-vs)
                                            (viz-state-word-img-offset a-vs)
                                            (viz-state-word-img-offset-cap a-vs)
                                            (viz-state-scroll-accum a-vs)
                                            (viz-state-click-buffer a-vs)
                                            )
                                 )
                  ]
              (reposition-out-of-bounds-img new-viz-state
                                            (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                            new-pimgs-img
                                            (viz-state-scale-factor a-vs)
                                            )
              )
            )
        )
      )
  )

;; viz-state -> viz-state
;; Purpose: Restarts the derivation in the visualization
(define (up-key-pressed a-vs)
  (if (= (length (viz-state-pimgs a-vs)) 1)
      a-vs
      (let* [(new-up-yield (rest (append (reverse (viz-state-p-yield a-vs))
                                         (viz-state-up-yield a-vs))))
             (new-p-yield (list (first (append (reverse (viz-state-p-yield a-vs))
                                               (viz-state-up-yield a-vs)))))
             (new-pimgs (list (first (append (reverse (viz-state-pimgs a-vs))
                                             (viz-state-upimgs a-vs)))))
             (new-pimgs-img (
                             (first new-pimgs)
                             )
                            )
             (curr-pimgs-img (
                              (first (viz-state-pimgs a-vs))
                              )
                             )
             (new-p-dgraph (list (first (append (reverse (viz-state-p-rules a-vs))
                                                (viz-state-up-rules a-vs)))))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))

             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                          )
                       )
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                          )
                       )
             ]
        (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                )
            (let
                [
                 (NEW-FLOOR (min (second img-resize) (third img-resize)))
                 ]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                     (let [
                           (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                    (viz-state-upimgs a-vs)))
                                                      new-pimgs
                                                      new-pimgs-img
                                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                      DEFAULT-ZOOM-CAP
                                                      DEFAULT-ZOOM-CAP
                                                      NEW-FLOOR
                                                      (viz-state-curr-mouse-posn a-vs)
                                                      (viz-state-dest-mouse-posn a-vs)
                                                      (viz-state-mouse-pressed a-vs)
                                                      (rest (append (reverse (viz-state-p-rules a-vs))
                                                                    (viz-state-up-rules a-vs)))
                                                      new-p-dgraph
                                                      new-up-yield
                                                      new-p-yield
                                                      (viz-state-input-word a-vs)
                                                      (viz-state-word-img-offset a-vs)
                                                      (viz-state-word-img-offset-cap a-vs)
                                                      (viz-state-scroll-accum a-vs)
                                                      (viz-state-click-buffer a-vs)
                                                      )
                                           )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (let [
                           (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                    (viz-state-upimgs a-vs)))
                                                      new-pimgs
                                                      new-pimgs-img
                                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                      NEW-FLOOR
                                                      DEFAULT-ZOOM-CAP
                                                      NEW-FLOOR
                                                      (viz-state-curr-mouse-posn a-vs)
                                                      (viz-state-dest-mouse-posn a-vs)
                                                      (viz-state-mouse-pressed a-vs)
                                                      (rest (append (reverse (viz-state-p-rules a-vs))
                                                                    (viz-state-up-rules a-vs)))
                                                      new-p-dgraph
                                                      new-up-yield
                                                      new-p-yield
                                                      (viz-state-input-word a-vs)
                                                      (viz-state-word-img-offset a-vs)
                                                      (viz-state-word-img-offset-cap a-vs)
                                                      (viz-state-scroll-accum a-vs)
                                                      (viz-state-click-buffer a-vs)
                                                      )
                                           )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    [else (let [
                                (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                         (viz-state-upimgs a-vs)))
                                                           new-pimgs
                                                           new-pimgs-img
                                                           (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                           (viz-state-scale-factor a-vs)
                                                           DEFAULT-ZOOM-CAP
                                                           NEW-FLOOR
                                                           (viz-state-curr-mouse-posn a-vs)
                                                           (viz-state-dest-mouse-posn a-vs)
                                                           (viz-state-mouse-pressed a-vs)
                                                           (rest (append (reverse (viz-state-p-rules a-vs))
                                                                         (viz-state-up-rules a-vs)))
                                                           new-p-dgraph
                                                           new-up-yield
                                                           new-p-yield
                                                           (viz-state-input-word a-vs)
                                                           (viz-state-word-img-offset a-vs)
                                                           (viz-state-word-img-offset-cap a-vs)
                                                           (viz-state-scroll-accum a-vs)
                                                           (viz-state-click-buffer a-vs)
                                                           )
                                                )
                                ]
                            (reposition-out-of-bounds-img new-viz-state
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor new-viz-state)
                                                          )
                            )
                          ]
                    )
              )
            (let [
                  (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                           (viz-state-upimgs a-vs)))
                                             new-pimgs
                                             new-pimgs-img
                                             (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                             (viz-state-scale-factor a-vs)
                                             DEFAULT-ZOOM-CAP
                                             DEFAULT-ZOOM-FLOOR
                                             (viz-state-curr-mouse-posn a-vs)
                                             (viz-state-dest-mouse-posn a-vs)
                                             (viz-state-mouse-pressed a-vs)
                                             (rest (append (reverse (viz-state-p-rules a-vs))
                                                           (viz-state-up-rules a-vs)))
                                             new-p-dgraph
                                             new-up-yield
                                             new-p-yield
                                             (viz-state-input-word a-vs)
                                             (viz-state-word-img-offset a-vs)
                                             (viz-state-word-img-offset-cap a-vs)
                                             (viz-state-scroll-accum a-vs)
                                             (viz-state-click-buffer a-vs)
                                             )
                                  )
                  ]
              (reposition-out-of-bounds-img new-viz-state
                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                            new-pimgs-img
                                            (viz-state-scale-factor new-viz-state)
                                            )
              )
            )
        )
      )
  )

;; viz-state -> viz-state
;; Purpose: Moves the visualization one step back in the derivation
(define (left-key-pressed a-vs)
  (if (= (length (viz-state-pimgs a-vs)) 1)
      a-vs
      (let* [(new-up-yield (cons (first (viz-state-p-yield a-vs))
                                 (viz-state-up-yield a-vs)))
             (new-p-yield (rest (viz-state-p-yield a-vs)))
             (new-pimgs (rest (viz-state-pimgs a-vs)))
             (new-pimgs-img (
                             (first new-pimgs)
                             )
                            )
             (curr-pimgs-img (
                              (first (viz-state-pimgs a-vs))
                              )
                             )
             (new-p-dgraph (rest (viz-state-p-rules a-vs)))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2) (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2) (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
             ]
        (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                )
            (let
                [
                 (NEW-FLOOR (min (second img-resize) (third img-resize)))
                 ]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                     (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                    (viz-state-upimgs a-vs))
                                                              new-pimgs
                                                              new-pimgs-img    
                                                              (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                              DEFAULT-ZOOM-CAP
                                                              DEFAULT-ZOOM-CAP
                                                              NEW-FLOOR
                                                              (viz-state-curr-mouse-posn a-vs)
                                                              (viz-state-dest-mouse-posn a-vs)
                                                              (viz-state-mouse-pressed a-vs)
                                                              (cons (first (viz-state-p-rules a-vs))
                                                                    (viz-state-up-rules a-vs))
                                                              new-p-dgraph
                                                              new-up-yield
                                                              new-p-yield
                                                              (viz-state-input-word a-vs)
                                                              (viz-state-word-img-offset a-vs)
                                                              (viz-state-word-img-offset-cap a-vs)
                                                              (viz-state-scroll-accum a-vs)
                                                              (viz-state-click-buffer a-vs)
                                                              )
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor a-vs)
                                                   )
                     ]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                    (viz-state-upimgs a-vs))
                                                              new-pimgs
                                                              new-pimgs-img     
                                                              (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                              NEW-FLOOR
                                                              DEFAULT-ZOOM-CAP
                                                              NEW-FLOOR
                                                              (viz-state-curr-mouse-posn a-vs)
                                                              (viz-state-dest-mouse-posn a-vs)
                                                              (viz-state-mouse-pressed a-vs)
                                                              (cons (first (viz-state-p-rules a-vs))
                                                                    (viz-state-up-rules a-vs))
                                                              new-p-dgraph
                                                              new-up-yield
                                                              new-p-yield
                                                              (viz-state-input-word a-vs)
                                                              (viz-state-word-img-offset a-vs)
                                                              (viz-state-word-img-offset-cap a-vs)
                                                              (viz-state-scroll-accum a-vs)
                                                              (viz-state-click-buffer a-vs)
                                                              )
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor a-vs)
                                                   )
                     ]
                    [else
                     (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                    (viz-state-upimgs a-vs))
                                                              new-pimgs
                                                              new-pimgs-img
                                                              (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                              (viz-state-scale-factor a-vs)
                                                              DEFAULT-ZOOM-CAP
                                                              NEW-FLOOR
                                                              (viz-state-curr-mouse-posn a-vs)
                                                              (viz-state-dest-mouse-posn a-vs)
                                                              (viz-state-mouse-pressed a-vs)
                                                              (cons (first (viz-state-p-rules a-vs))
                                                                    (viz-state-up-rules a-vs))
                                                              new-p-dgraph
                                                              new-up-yield
                                                              new-p-yield
                                                              (viz-state-input-word a-vs)
                                                              (viz-state-word-img-offset a-vs)
                                                              (viz-state-word-img-offset-cap a-vs)
                                                              (viz-state-scroll-accum a-vs)
                                                              (viz-state-click-buffer a-vs)
                                                              )
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor a-vs)
                                                   )
                     ]
                    )
              )
            (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                           (viz-state-upimgs a-vs))
                                                     new-pimgs
                                                     new-pimgs-img      
                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                     (viz-state-scale-factor a-vs)
                                                     DEFAULT-ZOOM-CAP
                                                     DEFAULT-ZOOM-FLOOR
                                                     (viz-state-curr-mouse-posn a-vs)
                                                     (viz-state-dest-mouse-posn a-vs)
                                                     (viz-state-mouse-pressed a-vs)
                                                     (cons (first (viz-state-p-rules a-vs))
                                                           (viz-state-up-rules a-vs))
                                                     new-p-dgraph
                                                     new-up-yield
                                                     new-p-yield
                                                     (viz-state-input-word a-vs)
                                                     (viz-state-word-img-offset a-vs)
                                                     (viz-state-word-img-offset-cap a-vs)
                                                     (viz-state-scroll-accum a-vs)
                                                     (viz-state-click-buffer a-vs)
                                                     )
                                          (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                          new-pimgs-img
                                          (viz-state-scale-factor a-vs)
                                          )
            )
        )
      )
  )

;; viz-state -> viz-state
;; Purpose: Finishes the derivations in the visualization
(define (down-key-pressed a-vs)
  (if (empty? (viz-state-upimgs a-vs))
      a-vs
      (let* [(new-p-yield (append (reverse (viz-state-up-yield a-vs))
                                  (viz-state-p-yield a-vs)))
             (new-pimgs (append (reverse (viz-state-upimgs a-vs))
                                (viz-state-pimgs a-vs)))
             (new-pimgs-img (
                             (first new-pimgs)
                             )
                            )
             (curr-pimgs-img (
                              (first (viz-state-pimgs a-vs))
                              )
                             )
             (new-p-dgraph (append (reverse (viz-state-up-rules a-vs))
                                   (viz-state-p-rules a-vs)))
             (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
             (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                          )
                       )
             (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                          (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                          )
                       )
             ]
        (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                )
            (let
                [
                 (NEW-FLOOR (min (second img-resize) (third img-resize)))
                 ]
              (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                     (let [
                           (new-viz-state (viz-state '()
                                                      new-pimgs
                                                      new-pimgs-img
                                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                      DEFAULT-ZOOM-CAP
                                                      DEFAULT-ZOOM-CAP
                                                      NEW-FLOOR
                                                      (viz-state-curr-mouse-posn a-vs)
                                                      (viz-state-dest-mouse-posn a-vs)
                                                      (viz-state-mouse-pressed a-vs)
                                                      '()
                                                      new-p-dgraph
                                                      '()
                                                      new-p-yield
                                                      (viz-state-input-word a-vs)
                                                      (viz-state-word-img-offset a-vs)
                                                      (viz-state-word-img-offset-cap a-vs)
                                                      (viz-state-scroll-accum a-vs)
                                                      (viz-state-click-buffer a-vs)
                                                      )
                                           )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                     (let [
                           (new-viz-state  (viz-state '()
                                                      new-pimgs
                                                      new-pimgs-img
                                                      (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                      NEW-FLOOR
                                                      DEFAULT-ZOOM-CAP
                                                      NEW-FLOOR
                                                      (viz-state-curr-mouse-posn a-vs)
                                                      (viz-state-dest-mouse-posn a-vs)
                                                      (viz-state-mouse-pressed a-vs)
                                                      '()
                                                      new-p-dgraph
                                                      '()
                                                      new-p-yield
                                                      (viz-state-input-word a-vs)
                                                      (viz-state-word-img-offset a-vs)
                                                      (viz-state-word-img-offset-cap a-vs)
                                                      (viz-state-scroll-accum a-vs)
                                                      (viz-state-click-buffer a-vs)
                                                      )
                                           )
                           ]
                       (reposition-out-of-bounds-img new-viz-state
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                     new-pimgs-img
                                                     (viz-state-scale-factor new-viz-state)
                                                     )
                       )
                     ]
                    [else (let [
                                (new-viz-state  (viz-state '()
                                                           new-pimgs
                                                           new-pimgs-img
                                                           (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                           (viz-state-scale-factor a-vs)
                                                           DEFAULT-ZOOM-CAP
                                                           NEW-FLOOR
                                                           (viz-state-curr-mouse-posn a-vs)
                                                           (viz-state-dest-mouse-posn a-vs)
                                                           (viz-state-mouse-pressed a-vs)
                                                           '()
                                                           new-p-dgraph
                                                           '()
                                                           new-p-yield
                                                           (viz-state-input-word a-vs)
                                                           (viz-state-word-img-offset a-vs)
                                                           (viz-state-word-img-offset-cap a-vs)
                                                           (viz-state-scroll-accum a-vs)
                                                           (viz-state-click-buffer a-vs)
                                                           )
                                                )
                                ]
                            (reposition-out-of-bounds-img new-viz-state
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor new-viz-state)
                                                          )
                            )
                          ]
                    )
              )
            (let [
                  (new-viz-state  (viz-state '()
                                             new-pimgs
                                             new-pimgs-img
                                             (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                             (viz-state-scale-factor a-vs)
                                             DEFAULT-ZOOM-CAP
                                             DEFAULT-ZOOM-FLOOR
                                             (viz-state-curr-mouse-posn a-vs)
                                             (viz-state-dest-mouse-posn a-vs)
                                             (viz-state-mouse-pressed a-vs)
                                             '()
                                             new-p-dgraph
                                             '()
                                             new-p-yield
                                             (viz-state-input-word a-vs)
                                             (viz-state-word-img-offset a-vs)
                                             (viz-state-word-img-offset-cap a-vs)
                                             (viz-state-scroll-accum a-vs)
                                             (viz-state-click-buffer a-vs)
                                             )
                                  )
                  ]
              (reposition-out-of-bounds-img new-viz-state
                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                            new-pimgs-img
                                            (viz-state-scale-factor new-viz-state)
                                            )
              )
            )
        )
      )
  )

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-vs)
  (viz-state (viz-state-upimgs a-vs)
             (viz-state-pimgs a-vs)
             (viz-state-curr-image a-vs)
             (viz-state-image-posn a-vs)
             (viz-state-scale-factor a-vs)
             (viz-state-scale-factor-cap a-vs)
             (viz-state-scale-factor-floor a-vs)
             (viz-state-dest-mouse-posn a-vs)
             (viz-state-dest-mouse-posn a-vs)
             (viz-state-mouse-pressed a-vs)
             (viz-state-up-rules a-vs)
             (viz-state-p-rules a-vs)
             (viz-state-up-yield a-vs)
             (viz-state-p-yield a-vs)
             (viz-state-input-word a-vs)
             0
             (viz-state-word-img-offset-cap a-vs)
             0
             (viz-state-click-buffer a-vs)
             )
  )

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-vs)
  (viz-state (viz-state-upimgs a-vs)
             (viz-state-pimgs a-vs)
             (viz-state-curr-image a-vs)
             (viz-state-image-posn a-vs)
             (viz-state-scale-factor a-vs)
             (viz-state-scale-factor-cap a-vs)
             (viz-state-scale-factor-floor a-vs)
             (viz-state-dest-mouse-posn a-vs)
             (viz-state-dest-mouse-posn a-vs)
             (viz-state-mouse-pressed a-vs)
             (viz-state-up-rules a-vs)
             (viz-state-p-rules a-vs)
             (viz-state-up-yield a-vs)
             (viz-state-p-yield a-vs)
             (viz-state-input-word a-vs)
             (viz-state-word-img-offset-cap a-vs)
             (viz-state-word-img-offset-cap a-vs)
             0
             (viz-state-click-buffer a-vs)
             )
  )

;; viz-state -> viz-state
;; Purpose: Zooms all the way out in the visualization
(define (r-key-pressed a-vs)
  (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
          (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs)))
          )
      (let [
            (img-resize (resize-image (viz-state-curr-image a-vs) (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
            ]
        (zoom a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs)))
        )
      (viz-state (viz-state-upimgs a-vs)
                 (viz-state-pimgs a-vs)
                 (viz-state-curr-image a-vs)
                 (viz-state-image-posn a-vs)
                 DEFAULT-ZOOM
                 (viz-state-scale-factor-cap a-vs)
                 (viz-state-scale-factor-floor a-vs)
                 (viz-state-dest-mouse-posn a-vs)
                 (viz-state-dest-mouse-posn a-vs)
                 (viz-state-mouse-pressed a-vs)
                 (viz-state-up-rules a-vs)
                 (viz-state-p-rules a-vs)
                 (viz-state-up-yield a-vs)
                 (viz-state-p-yield a-vs)
                 (viz-state-input-word a-vs)
                 (viz-state-word-img-offset a-vs)
                 (viz-state-word-img-offset-cap a-vs)
                 (viz-state-scroll-accum a-vs)
                 (viz-state-click-buffer a-vs)
                 )
      )
  )

;; viz-state -> viz-state
;; Purpose: Zooms in on the visualization
(define (w-key-pressed a-vs) (zoom a-vs ZOOM-INCREASE))

;; viz-state -> viz-state
;; Purpose: Zooms out the visualization
(define (s-key-pressed a-vs) (zoom a-vs ZOOM-DECREASE))

;; viz-state -> viz-state
;; Purpose: Zooms all the way in on the visualization
(define (f-key-pressed a-vs) (zoom a-vs (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs))))

;; viz-state -> viz-state
;; Purpose: Zooms in a moderate amount on the visualization
(define (e-key-pressed a-vs) (zoom a-vs (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs))))

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (right-key-pressed a-vs)
         ]
        [(key=? "left" a-key)
         (left-key-pressed a-vs)
         ]
        [(key=? "down" a-key)
         (down-key-pressed a-vs)
         ]
        [(key=? "up" a-key)
         (up-key-pressed a-vs)
         ]
        [(key=? "w" a-key) (w-key-pressed a-vs)]
        [(key=? "s" a-key) (s-key-pressed a-vs)]
        [(key=? "a" a-key)
         (a-key-pressed a-vs)
         ]
        [(key=? "d" a-key)
         (d-key-pressed a-vs)
         ]
        [(key=? "r" a-key)
         (r-key-pressed a-vs)
         ]
        [(key=? "f" a-key)
         (f-key-pressed a-vs)
         ]
        [(key=? "e" a-key)
         (e-key-pressed a-vs)
         ]
        [else a-vs]
        )
  )

;; viz-state int int MouseEvent
;; Updates viz-state as to whether the mouse is currently being pressed while on the visualization
(define (process-mouse a-vs x y mouse-event)
  (cond [(string=? mouse-event "button-down")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #t
                    (viz-state-up-rules a-vs)
                    (viz-state-p-rules a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    (viz-state-scroll-accum a-vs)
                    (viz-state-click-buffer a-vs)
                    )
         ]
        [(string=? mouse-event "button-up")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #f
                    (viz-state-up-rules a-vs)
                    (viz-state-p-rules a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    (viz-state-scroll-accum a-vs)
                    0
                    )
         ]
        ;; Want to keep the mouse updating while it is being dragged
        [(string=? mouse-event "drag")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #t
                    (viz-state-up-rules a-vs)
                    (viz-state-p-rules a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    (viz-state-scroll-accum a-vs)
                    (viz-state-click-buffer a-vs)
                    )
         ]
                                                   
        ;; Can happen in both clicked and unclicked states so leave it in whatever it was
        [(string=? mouse-event "move")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    (viz-state-mouse-pressed a-vs)
                    (viz-state-up-rules a-vs)
                    (viz-state-p-rules a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    (viz-state-scroll-accum a-vs)
                    (viz-state-click-buffer a-vs)
                    )
         ]

        ;; This one is ambigious, think its better to leave as whatever it already was
        [(string=? mouse-event "enter")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    (viz-state-mouse-pressed a-vs)
                    (viz-state-up-rules a-vs)
                    (viz-state-p-rules a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    (viz-state-scroll-accum a-vs)
                    (viz-state-click-buffer a-vs)
                    )
         ]

        ;; Stop updating if the mouse leaves the visualization screen
        [(string=? mouse-event "leave")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #f
                    (viz-state-up-rules a-vs)
                    (viz-state-p-rules a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    (viz-state-scroll-accum a-vs)
                    (viz-state-click-buffer a-vs)
                    )
         ]
        [else a-vs]
        )
  )

(define (buffer-held-click a-vs func)
  (if (= (viz-state-click-buffer a-vs) 0)
      (func (viz-state (viz-state-upimgs a-vs)
                       (viz-state-pimgs a-vs)
                       (viz-state-curr-image a-vs)
                       (viz-state-image-posn a-vs)
                       (viz-state-scale-factor a-vs)
                       (viz-state-scale-factor-cap a-vs)
                       (viz-state-scale-factor-floor a-vs)
                       (viz-state-dest-mouse-posn a-vs)
                       (viz-state-dest-mouse-posn a-vs)
                       (viz-state-mouse-pressed a-vs)
                       (viz-state-up-rules a-vs)
                       (viz-state-p-rules a-vs)
                       (viz-state-up-yield a-vs)
                       (viz-state-p-yield a-vs)
                       (viz-state-input-word a-vs)
                       (viz-state-word-img-offset a-vs)
                       (viz-state-word-img-offset-cap a-vs)
                       (viz-state-scroll-accum a-vs)
                       1
                       )
            )
      (if (= (viz-state-click-buffer a-vs) CLICK-BUFFER-SECONDS)
          (viz-state (viz-state-upimgs a-vs)
                     (viz-state-pimgs a-vs)
                     (viz-state-curr-image a-vs)
                     (viz-state-image-posn a-vs)
                     (viz-state-scale-factor a-vs)
                     (viz-state-scale-factor-cap a-vs)
                     (viz-state-scale-factor-floor a-vs)
                     (viz-state-dest-mouse-posn a-vs)
                     (viz-state-dest-mouse-posn a-vs)
                     (viz-state-mouse-pressed a-vs)
                     (viz-state-up-rules a-vs)
                     (viz-state-p-rules a-vs)
                     (viz-state-up-yield a-vs)
                     (viz-state-p-yield a-vs)
                     (viz-state-input-word a-vs)
                     (viz-state-word-img-offset a-vs)
                     (viz-state-word-img-offset-cap a-vs)
                     (viz-state-scroll-accum a-vs)
                     0
                     )
          (viz-state (viz-state-upimgs a-vs)
                     (viz-state-pimgs a-vs)
                     (viz-state-curr-image a-vs)
                     (viz-state-image-posn a-vs)
                     (viz-state-scale-factor a-vs)
                     (viz-state-scale-factor-cap a-vs)
                     (viz-state-scale-factor-floor a-vs)
                     (viz-state-dest-mouse-posn a-vs)
                     (viz-state-dest-mouse-posn a-vs)
                     (viz-state-mouse-pressed a-vs)
                     (viz-state-up-rules a-vs)
                     (viz-state-p-rules a-vs)
                     (viz-state-up-yield a-vs)
                     (viz-state-p-yield a-vs)
                     (viz-state-input-word a-vs)
                     (viz-state-word-img-offset a-vs)
                     (viz-state-word-img-offset-cap a-vs)
                     (viz-state-scroll-accum a-vs)
                     (add1 (viz-state-click-buffer a-vs))
                     )
          )
      )
  )

;; viz-state
;; Updates the position of the image displayed based on the movement of the mouse
(define (process-tick a-vs)
  (let* [
         ;; Determines the movement of the mouse that occured since the last tick
         (x-diff (- (posn-x (viz-state-dest-mouse-posn a-vs)) (posn-x (viz-state-curr-mouse-posn a-vs))))
         (y-diff (- (posn-y (viz-state-dest-mouse-posn a-vs)) (posn-y (viz-state-curr-mouse-posn a-vs))))

         (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
         (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))

         (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
         (viewport-lims (calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs)))

         (MIN-X (bounding-limits-min-x viewport-lims))
         (MAX-X (bounding-limits-max-x viewport-lims))
         (MIN-Y (bounding-limits-min-y viewport-lims))
         (MAX-Y (bounding-limits-max-y viewport-lims))
         (scroll-dimensions RULE-YIELD-DIMS)
         ]
    (if (viz-state-mouse-pressed a-vs)
        (cond [(and (<= 0 (posn-x (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-x (viz-state-curr-mouse-posn a-vs)) E-SCENE-WIDTH)
                    (<= 0 (posn-y (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-y (viz-state-curr-mouse-posn a-vs)) E-SCENE-HEIGHT)
                    )
               (cond [(and (<= MIN-X new-img-x)
                           (<= new-img-x MAX-X)
                           (<= MIN-Y new-img-y)
                           (<= new-img-y MAX-Y)
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (posn new-img-x new-img-y)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-rules a-vs)
                                 (viz-state-p-rules a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 (viz-state-scroll-accum a-vs)
                                 (viz-state-click-buffer a-vs)
                                 )
                      ]
                     [(and (or (> MIN-X new-img-x)
                               (> new-img-x MAX-X)
                               )
                           (<= MIN-Y new-img-y)
                           (<= new-img-y MAX-Y)
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-rules a-vs)
                                 (viz-state-p-rules a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 (viz-state-scroll-accum a-vs)
                                 (viz-state-click-buffer a-vs)
                                 )
                      ]
                     [(and (<= MIN-X new-img-x)
                           (<= new-img-x MAX-X)
                           (or (> MIN-Y new-img-y)
                               (> new-img-y MAX-Y)
                               )
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (posn new-img-x (posn-y (viz-state-image-posn a-vs)))
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-rules a-vs)
                                 (viz-state-p-rules a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 (viz-state-scroll-accum a-vs)
                                 (viz-state-click-buffer a-vs)
                                 )
                      ]
                     [(and (or (> MIN-X new-img-x)
                               (> new-img-x MAX-X)
                               )
                           (or (> MIN-Y new-img-y)
                               (> new-img-y MAX-Y)
                               )
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (viz-state-image-posn a-vs)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-rules a-vs)
                                 (viz-state-p-rules a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 (viz-state-scroll-accum a-vs)
                                 (viz-state-click-buffer a-vs)
                                 )
                      ]
                     )
               ]
              [(and (<= (bounding-limits-min-x scroll-dimensions) (posn-x (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-x (viz-state-curr-mouse-posn a-vs)) (bounding-limits-max-x scroll-dimensions))
                    (<= (bounding-limits-min-y scroll-dimensions) (posn-y (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-y (viz-state-curr-mouse-posn a-vs)) (bounding-limits-max-y scroll-dimensions))
                    )
               (let [
                     (new-scroll-accum (+ (viz-state-scroll-accum a-vs) x-diff))
                     ]
                 (cond [(and (>= (viz-state-word-img-offset-cap a-vs) (viz-state-word-img-offset a-vs))
                             (<= (quotient new-scroll-accum 25) -1)
                             )
                        (viz-state (viz-state-upimgs a-vs)
                                   (viz-state-pimgs a-vs)
                                   (viz-state-curr-image a-vs)
                                   (viz-state-image-posn a-vs)
                                   (viz-state-scale-factor a-vs)
                                   (viz-state-scale-factor-cap a-vs)
                                   (viz-state-scale-factor-floor a-vs)
                                   (viz-state-dest-mouse-posn a-vs)
                                   (viz-state-dest-mouse-posn a-vs)
                                   (viz-state-mouse-pressed a-vs)
                                   (viz-state-up-rules a-vs)
                                   (viz-state-p-rules a-vs)
                                   (viz-state-up-yield a-vs)
                                   (viz-state-p-yield a-vs)
                                   (viz-state-input-word a-vs)
                                   (+ (viz-state-word-img-offset a-vs) 1)
                                   (viz-state-word-img-offset-cap a-vs)
                                   0
                                   (viz-state-click-buffer a-vs)
                                   )]
                       [(and (> (viz-state-word-img-offset a-vs) 0)
                             (>= (quotient new-scroll-accum 25) 1)
                             )
                        (viz-state (viz-state-upimgs a-vs)
                                   (viz-state-pimgs a-vs)
                                   (viz-state-curr-image a-vs)
                                   (viz-state-image-posn a-vs)
                                   (viz-state-scale-factor a-vs)
                                   (viz-state-scale-factor-cap a-vs)
                                   (viz-state-scale-factor-floor a-vs)
                                   (viz-state-dest-mouse-posn a-vs)
                                   (viz-state-dest-mouse-posn a-vs)
                                   (viz-state-mouse-pressed a-vs)
                                   (viz-state-up-rules a-vs)
                                   (viz-state-p-rules a-vs)
                                   (viz-state-up-yield a-vs)
                                   (viz-state-p-yield a-vs)
                                   (viz-state-input-word a-vs)
                                   (- (viz-state-word-img-offset a-vs) 1)
                                   (viz-state-word-img-offset-cap a-vs)
                                   0
                                   (viz-state-click-buffer a-vs)
                                   )
                        ]
                       [else (viz-state (viz-state-upimgs a-vs)
                                        (viz-state-pimgs a-vs)
                                        (viz-state-curr-image a-vs)
                                        (viz-state-image-posn a-vs)
                                        (viz-state-scale-factor a-vs)
                                        (viz-state-scale-factor-cap a-vs)
                                        (viz-state-scale-factor-floor a-vs)
                                        (viz-state-dest-mouse-posn a-vs)
                                        (viz-state-dest-mouse-posn a-vs)
                                        (viz-state-mouse-pressed a-vs)
                                        (viz-state-up-rules a-vs)
                                        (viz-state-p-rules a-vs)
                                        (viz-state-up-yield a-vs)
                                        (viz-state-p-yield a-vs)
                                        (viz-state-input-word a-vs)
                                        (viz-state-word-img-offset a-vs)
                                        (viz-state-word-img-offset-cap a-vs)
                                        new-scroll-accum
                                        (viz-state-click-buffer a-vs)
                                        )
                             ]
                       )
                 )
               ]
              [(within-bounding-limits? ARROW-UP-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs up-key-pressed)
               ]
              [(within-bounding-limits? ARROW-RIGHT-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs right-key-pressed)
               ]
              [(within-bounding-limits? ARROW-LEFT-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs left-key-pressed)
               ]
              [(within-bounding-limits? ARROW-DOWN-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs down-key-pressed)
               ]
              [(within-bounding-limits? W-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (w-key-pressed a-vs)
               ]
              [(within-bounding-limits? S-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (s-key-pressed a-vs)
               ]
              [(within-bounding-limits? A-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs a-key-pressed)
               ]
              [(within-bounding-limits? D-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs d-key-pressed)
               ]
              [(within-bounding-limits? R-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs r-key-pressed)
               ]
              [(within-bounding-limits? E-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs e-key-pressed)
               ]
              [(within-bounding-limits? F-KEY-DIMS (viz-state-curr-mouse-posn a-vs))
               (buffer-held-click a-vs f-key-pressed)
               ]
              [else
               a-vs
               ]
              )
        (viz-state (viz-state-upimgs a-vs)
                   (viz-state-pimgs a-vs)
                   (viz-state-curr-image a-vs)
                   (viz-state-image-posn a-vs)
                   (viz-state-scale-factor a-vs)
                   (viz-state-scale-factor-cap a-vs)
                   (viz-state-scale-factor-floor a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-mouse-pressed a-vs)
                   (viz-state-up-rules a-vs)
                   (viz-state-p-rules a-vs)
                   (viz-state-up-yield a-vs)
                   (viz-state-p-yield a-vs)
                   (viz-state-input-word a-vs)
                   (viz-state-word-img-offset a-vs)
                   (viz-state-word-img-offset-cap a-vs)
                   (viz-state-scroll-accum a-vs)
                   (viz-state-click-buffer a-vs)
                   )
        )
    )
  )

;; create-first-img
;; node -> img
;; Purpose: To create the first graph img
(define (create-first-img node)
  (lambda () (graph->bitmap (add-node
                             (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                             node
                             #:atb (hash 'color 'black
                                         'shape 'circle
                                         'label node
                                         'fontcolor 'black
                                         'font "Sans"))
                            )
    )
  )

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [
        (PARSE-TREE-IMG (place-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)) 
                                     (posn-x (viz-state-image-posn a-vs))
                                     (posn-y (viz-state-image-posn a-vs))
                                     (rectangle E-SCENE-WIDTH E-SCENE-HEIGHT 'outline 'white)
                                     )
                        )
        ]
    (above PARSE-TREE-IMG (create-instructions-and-tools a-vs))
    )
  )

;; vst --> void
(define (viz a-vs draw-etc a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-etc]
      [on-key process-key]
      [on-mouse process-mouse]
      [on-tick process-tick TICK-RATE]
      [name a-name]))
  (void))

(define (rg-viz word w-der rules graphs #:cpu-cores [cpu-cores #f])
  (let* [
         (first-img (create-first-img (first (first w-der))))
         (imgs (cons first-img (rest (create-graph-imgs graphs #:cpu-cores cpu-cores))))
         ]
    (viz (viz-state (rest imgs)
                    (list (first imgs))
                    (
                     (first imgs)
                     )
                    (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
                    DEFAULT-ZOOM
                    DEFAULT-ZOOM-CAP
                    DEFAULT-ZOOM-FLOOR
                    (posn 0 0)
                    (posn 0 0)
                    #f
                    (rest rules)
                    (list (first rules))
                    (rest w-der)
                    (first w-der)
                    word
                    0
                    (let [(offset-cap (- (length word) TAPE-SIZE))]
                      (if (> 0 offset-cap)
                          0
                          offset-cap
                          )
                      )
                    0
                    0
                    )
         draw-world 'grammar-viz)
    )
  )

(define (run-viz grammar word w-der rules graphs) (rg-viz word w-der rules graphs))