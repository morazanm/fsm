#lang racket
(require "../../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../viz-lib/resize-sm-image.rkt"
         ;"definitions-viz.rkt"
         ;"run-viz.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         ;"../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../../fsm-core/private/regexp.rkt"
         
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt")
(provide regexp2ndfa-viz)

(define FNAME "fsm")



(define S-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_s.png"))

(define W-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_w.png"))

(define R-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_r.png"))

(define F-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_f.png"))

(define E-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_e.png"))

(define A-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_a.png"))

(define D-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_d.png"))

(define ARROW-RIGHT-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_right.png"))

(define ARROW-LEFT-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_left.png"))

(define ARROW-UP-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_up.png"))

(define ARROW-DOWN-KEY (bitmap/file "../../visualizations/viz-lib/viz-imgs/keyboard_key_down.png"))

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

(define imsg-img (text "Starting ndfa" FONT-SIZE 'black))

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

(define R0 (make-unchecked-union (make-unchecked-singleton "a")
                                 (null-regexp)))

(define R1 (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "a")
                                                            (make-unchecked-singleton "b"))))
(define R11 (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "b")
                                                             (make-unchecked-singleton "c"))))
(define R12 (make-unchecked-kleenestar (make-unchecked-union (make-unchecked-singleton "a")
                                                             (make-unchecked-singleton "c"))))

(define R13 (make-unchecked-union (make-unchecked-union R1 R11) R12))
(define R8 (make-unchecked-concat (make-unchecked-singleton "a")
                                  (make-unchecked-singleton "b")))

(define R9 (make-unchecked-concat (make-unchecked-singleton "b")
                                  (make-unchecked-singleton "a")))

(define R10 (make-unchecked-union (make-unchecked-kleenestar R8) (make-unchecked-kleenestar R9)))

(define R2 (make-unchecked-concat (make-unchecked-singleton "m") R1))

(define R3 (make-unchecked-kleenestar R2))

(define R4 (make-unchecked-kleenestar (make-unchecked-union R3 R2)))

(define R5 (make-unchecked-concat (make-unchecked-singleton "m") R0))

(define R6 (make-unchecked-kleenestar R5))

(define R7 (make-unchecked-union R0 R5))

(define E-SCENE (empty-scene 1250 600))

#;(define E-SCENE-TOOLS (overlay (beside (above (above (triangle 30 'solid 'black)
                                                       (rectangle 10 30 'solid 'black))
                                                (square 20 'solid 'white)
                                                (text "Restart the visualization" 18 'black))
                                         (square 40 'solid 'white)
                                         (above (beside (rectangle 30 10 'solid 'black)
                                                        (rotate 270 (triangle 30 'solid 'black)))
                                                (square 20 'solid 'white)
                                                (text "Move one step forward" 18 'black))
                                         (square 40 'solid 'white)
                                         (above (beside (rotate 90 (triangle 30 'solid 'black))
                                                        (rectangle 30 10 'solid 'black))
                                                (square 20 'solid 'white)
                                                (text "Move one step backward" 18 'black))
                                         (square 40 'solid 'white)
                                         (above (above (rectangle 10 30 'solid 'black)
                                                       (rotate 180 (triangle 30 'solid 'black)))
                                                (square 20 'solid 'white)
                                                (text "Complete the visualization" 18 'black))
                                         )
                                 (empty-scene 1250 100)))

;; grph is a (listof img) of graphs used to build an ndfa from regexp
;; edge is an edge that has been expanded to build an ndfa from regexp
(struct gedge (grph edge))

;; dgraph2logedges
;; dgraph edge --> (listof gedges)
;; Purpose: Create a list of digraph-edge structures
(define (dgraph2logedges dgraph edge . issimp?)
  
  ;; digraph --> Boolean
  (define (only-simple-edges? grph)
    (andmap (λ (e) (or (empty-regexp? (second e))
                       (singleton-regexp? (second e))
                       (null-regexp? (second e))))
            grph))

  ;; dgraph --> gedge
  ;; Purpose: To extract the first nonsimple edge in the dgraph
  ;; Assumption: dgraph has a nonsimple edge
  (define (extract-first-nonsimple grph)
    (first (filter (λ (e) (and (not (empty-regexp? (second e)))
                               (not (singleton-regexp? (second e)))))
                   grph)))
  
  ;; dgraph edge (listof dgraph) --> (listof dgraph)
  ;; Purpose: To create a list of dgraphs with  the expanded edge removed
  ;; and replaced with the appropriate edges
  (define (bfs grph edge acc)

    (define grph-states (remove-duplicates (append-map (λ (e) (list (first e) (third e))) grph)))
    
    (cond [(only-simple-edges? grph) (cons (gedge grph edge) acc)]
          [(and (not (null? issimp?))
                (first issimp?))
           (cons (gedge grph void) acc)]
          [else 
           (let* [(next-edge (extract-first-nonsimple grph))
                  (fromst (first next-edge))
                  (rexp (second next-edge))
                  (tost (third next-edge))]
             (cond [(union-regexp? rexp)
                    (let* [(newi1 (gen-state grph-states))
                           (newi2 (gen-state (cons newi1 grph-states)))
                           (newi3 (gen-state (append (list newi1 newi2) grph-states)))
                           (newi4 (gen-state (append (list newi1 newi2 newi3) grph-states)))]
                      (bfs
                       (append (list (list fromst (empty-regexp) newi1)
                                     (list fromst (empty-regexp) newi2)
                                     (list newi1 (union-regexp-r1 rexp) newi3)
                                     (list newi2 (union-regexp-r2 rexp) newi4)
                                     (list newi3 (empty-regexp) tost)
                                     (list newi4 (empty-regexp) tost))
                               (remove next-edge grph))
                       next-edge
                       (cons (gedge grph edge) acc)))]
                   [(concat-regexp? rexp)
                    (let* [(istate1 (gen-state grph-states))
                           (istate2 (gen-state (cons istate1 grph-states)))]
                      (bfs (append (list (list fromst (concat-regexp-r1 rexp) istate1)
                                         (list istate1 (empty-regexp) istate2)
                                         (list istate2 (concat-regexp-r2 rexp) tost))
                                   (remove next-edge grph))
                           next-edge
                           (cons (gedge grph edge) acc)))]
                   [else
                    (let* [(istart1 (gen-state grph-states))
                           (istart2 (gen-state (cons istart1 grph-states)))]
                      (bfs
                       (append (list (list fromst (empty-regexp) istart1)
                                     (list istart1 (empty-regexp) tost)
                                     (list istart1 (empty-regexp) istart2)
                                     (list istart2 (kleenestar-regexp-r1 rexp) istart2)
                                     (list istart2 (empty-regexp) tost))
                               (remove next-edge grph))
                       next-edge
                       (cons (gedge grph edge) acc)))]))]))
  (bfs dgraph edge '()))


;; create-nodes
;; graph dgraph edge -> graph
;; Purpose: To add the nodes to the graph
(define (create-nodes graph dgraph edge)
  (define (states-only dgraph)
    (remove-duplicates
     (append-map (λ (e) (list (first e) (third e))) dgraph)))
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state 'S)
                                      (if (and (not (empty? edge))
                                               (not (equal? void edge))
                                               (eq? (first edge) 'S))
                                          'violet
                                          'darkgreen)]
                                     [(eq? state 'F)
                                      (if (and (not (empty? edge))
                                               (not (equal? void edge))
                                               (eq? (third edge) 'F))
                                          'violet
                                          'black)]
                                     [(or (eq? state (first edge))
                                          (eq? state (third edge)))
                                      'violet]
                                     [else 'black])                                   
                        'shape (if (eq? state 'F)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         (states-only dgraph)))                         

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges graph dgraph)
  (foldl (λ (rule result)
           (add-edge result
                     (printable-regexp (second rule))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         dgraph))


;; create-graph-img
;; graph edge -> img
;; Purpose: To create a graph img for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graphic dgraph edge)
  (create-edges
   (create-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                      'font "Sans"))
    dgraph
    edge)
   dgraph))


;; imsg
(struct imsg-state (edge))


;; draw-imsg
;; imsg -> img
(define (draw-imsg a-imsg)
  (cond [(zipper-at-begin? (imsg-state-edge a-imsg)) (text "Starting NDFA" 24 'black)]
        [(= (zipper-idx (imsg-state-edge a-imsg)) 1) (text "Simplified initial regexp" 24 'black)]
        [else (beside (text (format "Expanded regexp: ~a on edge from state" (printable-regexp (second (zipper-current (imsg-state-edge a-imsg)))  #;(zipper-current (zipper-to-idx (imsg-state-edge a-imsg) 1)))) 24 'black)
                      (text (format " ~a" (first (zipper-current (imsg-state-edge a-imsg)))  #;(printable-regexp (zipper-current (imsg-state-edge a-imsg)))) 24 'violet)
                      (text (format " to state ") 24 'black)
                      (text (format "~a" (third (zipper-current (imsg-state-edge a-imsg))) #;(printable-regexp (zipper-current (zipper-to-idx (imsg-state-edge a-imsg) 2)))) 24 'violet))
                
              ]))


;; create-graph-imgs
;; (listof gedges) -> (listof image)
;; Purpose: To create a list of graph images
(define (create-graphs gedges)
  (if (empty? gedges)
      empty
      (cons (create-graphic (gedge-grph (first gedges))
                            (gedge-edge (first gedges)))
            (create-graphs (rest gedges)))))


;; create-init-nodes
;; graph dgraph -> graph
;; Purpose: To add the nodes to the graph
(define (create-init-nodes graph)
  (add-node
   (add-node
    graph
    'F
    #:atb (hash 'color 'black                                   
                'shape 'doublecircle
                'fontcolor 'black
                'font "Sans"))
   'S
   #:atb (hash 'color 'darkgreen                                   
               'shape 'circle
               'fontcolor 'black
               'font "Sans")))

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-init-edges graph dgraph)
  (add-edge graph
            (printable-regexp (second dgraph))
            (first dgraph)
            (third dgraph)
            #:atb (hash 'fontsize 14
                        'style 'solid
                        'fontname "Sans"
                        )))



;; create-init-graph
;; list -> img
;; Purpose: To create an initial graph image
(define (create-init-graph a-list)
  (create-init-edges
   (create-init-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                      'font "Sans")))
   a-list))





;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
#;(define (draw-world a-vs)
    (let [(width (image-width (first (viz-state-pimgs a-vs))))
          (height (image-height (first (viz-state-pimgs a-vs))))]
      (if (or (> width (image-width E-SCENE))
              (> height (image-height E-SCENE)))
          (above (overlay (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE))
                          E-SCENE) E-SCENE-TOOLS)
          (above (overlay (first (viz-state-pimgs a-vs)) E-SCENE) E-SCENE-TOOLS))))



(define (right-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy viz-state a-vs
                     [informative-messages
                      (struct-copy informative-messages
                                   (viz-state-informative-messages a-vs)
                                   [component-state
                                    (struct-copy imsg-state
                                                 a-imsg-state
                                                 [edge (zipper-next
                                                        (imsg-state-edge
                                                         a-imsg-state)
                                                        )
                                                       ]
                                                 )])]))
    )
  )

(define (left-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy viz-state a-vs
                     [informative-messages
                      (struct-copy informative-messages
                                   (viz-state-informative-messages a-vs)
                                   [component-state
                                    (struct-copy imsg-state
                                                 a-imsg-state
                                                 [edge (zipper-prev
                                                        (imsg-state-edge
                                                         a-imsg-state)
                                                        )
                                                       ]
                                                 )])]))
    )
  )

(define (up-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy viz-state a-vs
                     [informative-messages
                      (struct-copy informative-messages
                                   (viz-state-informative-messages a-vs)
                                   [component-state
                                    (struct-copy imsg-state
                                                 a-imsg-state
                                                 [edge (zipper-to-begin
                                                        (imsg-state-edge
                                                         a-imsg-state)
                                                        )
                                                       ]
                                                 )])]))
    )
  )

(define (down-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (imsg-state-edge a-imsg-state))
        a-vs
        (struct-copy viz-state a-vs
                     [informative-messages
                      (struct-copy informative-messages
                                   (viz-state-informative-messages a-vs)
                                   [component-state
                                    (struct-copy imsg-state
                                                 a-imsg-state
                                                 [edge (zipper-to-end
                                                        (imsg-state-edge
                                                         a-imsg-state)
                                                        )
                                                       ]
                                                 )])]))
    )
  )



(define viz-go-next (go-next))
(define viz-go-prev (go-prev))
(define viz-go-to-begin (go-to-begin))
(define viz-go-to-end (go-to-end))
(define viz-zoom-in (zoom-in))
(define viz-zoom-out (zoom-out))
(define viz-max-zoom-out (max-zoom-out))
(define viz-max-zoom-in (max-zoom-in))
(define viz-reset-zoom (reset-zoom))

;; regexp2ndfa-viz
;; regexp -> void
(define (regexp2ndfa-viz regexp)
  (let* [(logedges (append
                    (list
                     (last (dgraph2logedges
                            (list (list 'S (simplify-regexp regexp) 'F)) 
                            '()
                            #t)))
                    (rest (reverse (dgraph2logedges
                                    (list (list 'S (simplify-regexp regexp) 'F))
                                    '())))))
         (graphs (create-graphs logedges))
         ]
    (run-viz (cons (create-init-graph (list 'S regexp 'F)) graphs)
             (lambda () (graph->bitmap (create-init-graph (list 'S regexp 'F))))
             MIDDLE-E-SCENE
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (imsg-state (list->zipper (cons (text "Starting NDFA" 24 'black) (map gedge-edge logedges))))
                                   (bounding-limits 0 0 0 0)
                                   )
             (instructions-graphic
              E-SCENE-TOOLS
              (bounding-limits 0 0 0 0))
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
             )))



;(regexp2ndfa-viz R5)
    
#; (run-viz (viz-state loimgs (list (create-init-graph (list 'S regexp 'F)))) draw-world 'regexp2ndfa)



