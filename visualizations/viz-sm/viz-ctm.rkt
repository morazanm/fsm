#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../../fsm-core/private/tm.rkt"
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/callgraphs/transdiagram-ctm6.rkt"
         "../../visualizations/viz-lib/zipper.rkt"
         "../../visualizations/viz-lib/viz-constants.rkt"
         "../../visualizations/viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../../visualizations/viz-lib/viz-imgs/cursor.rkt"
         "../../visualizations/viz-lib/bounding-limits.rkt"
         "../../visualizations/viz-lib/default-viz-function-generators.rkt"
         "../../visualizations/viz-lib/viz-macros.rkt"
         "../../visualizations/viz-lib/viz-state.rkt"
         "../../visualizations/viz-lib/viz.rkt"
         )


(provide ctm-viz)

(define FNAME "fsm")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

                          (above/align "middle" A-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Tape left" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)

                          (above/align "middle" D-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Tape right" (- FONT-SIZE 2) 'black))
                          )
                  )
    )
  )

(define RULE-YIELD-DIMS (bounding-limits 0
                                         1250
                                         E-SCENE-HEIGHT
                                         (+ E-SCENE-HEIGHT 120)))

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

;; imsg-struct
(struct imsg-struct (tapes vars))
(define E-SCENE (empty-scene 1250 600))
(define TAPE-SIZE 20)


;; graph is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
(struct graph (upimgs pimgs))

;; tapelist is a structure that has
;; utape which is unprocessed tape images
;; ptape which is processed tape image
(struct tapelist (utape ptape))

;; var is a structure that has
;; uvar which is unprocessed labels
;; pvar which is processed labels
(struct var (uvar pvar))

;; viz-state is a structure that has
;; graph which is a structure
;; tape which is a list that contains all elements
;; needed to create a tape image
;; tapeimg is the image of the current tape
;(struct viz-state (graph tapelist tapeimg var))

;; create-nodes
;; graph (listof node) -> graph
;; Purpose: To add the nodes to the graph
(define (create-nodes dgraph lon edge)
  (foldl (λ (state result)
           (add-node
            result
            (string->symbol (first state))
            #:atb (hash 'color (second (first (second state)))
                        'style (if (equal? (first state) (first edge))
                                   'bold
                                   'solid)
                        'shape (second (second (second state)))
                        'label (second (third (second state)))
                        'fontcolor 'black
                        'font "Sans")))
         dgraph
         lon))                         

;; create-edges
;; graph (listof edge) edge -> graph
;; Purpose: To create graph of edges
(define (create-edges dgraph loe edge)
  (foldl (λ (rule result) (add-edge result
                                    (if (equal? (second (first (third rule))) '_)
                                        'BLANK
                                        (second (first (third rule))))
                                    (string->symbol (first rule))
                                    (string->symbol (second rule))
                                    #:atb (hash 'fontsize 14
                                                'style (second (second (third rule)))
                                                'color (cond [(and (equal? (first rule) (first edge))
                                                                   (equal? (second rule) (second edge)))
                                                              'dodgerblue2]
                                                             [(equal? (second (third (third rule))) "white")
                                                              'white]    
                                                             [else 'black])
                                                'headlabel (second (fourth (third rule)))
                                                )))
         dgraph
         loe))


(define (right-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-end? (imsg-struct-tapes
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-next (imsg-struct-tapes
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-end? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-next (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))

(define (left-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-begin? (imsg-struct-tapes
                                                                         (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-prev (imsg-struct-tapes
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-begin? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-prev (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))

(define (up-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-begin? (imsg-struct-tapes
                                                                         (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-to-begin (imsg-struct-tapes
                                                                        (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-begin? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-to-begin (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))


(define (down-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-end? (imsg-struct-tapes
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-to-end (imsg-struct-tapes
                                                                      (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-end? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-to-end (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))


(define (d-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (zipper-set (imsg-struct-tapes
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))
                                                              (list (first (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs)))))
                                                                    (add1 (second (zipper-current (imsg-struct-tapes
                                                                                                   (informative-messages-component-state
                                                                                                    (viz-state-informative-messages a-vs))))))
                                                                    (third (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs))))))
                                                              )
                                                  ]
                                           )])]))

(define (a-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (zipper-set (imsg-struct-tapes
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))
                                                              (list (first (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs)))))
                                                                    (if (< 0 (second (zipper-current (imsg-struct-tapes
                                                                                                      (informative-messages-component-state
                                                                                                       (viz-state-informative-messages a-vs))))))
                                                                        (sub1 (second (zipper-current (imsg-struct-tapes
                                                                                                       (informative-messages-component-state
                                                                                                        (viz-state-informative-messages a-vs))))))
                                                                        (second (zipper-current (imsg-struct-tapes
                                                                                                 (informative-messages-component-state
                                                                                                  (viz-state-informative-messages a-vs))))))
                                                                    (third (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs)))))))
                                                  ]
                                           )])]))

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.


;; list2string
;; list -> string
;; Purpose: To convert all elements of the list into a string
(define (list2string a-list)
  (if (empty? a-list)
      (rectangle 0.1 40 'outline 'black)
      (beside (overlay (text (format "~s" (first a-list)) 30 'black)
                       (rectangle 36 40 'outline 'black))
              (list2string (rest a-list)))))

;; draw-imsg
;; list number number var -> image
;; Purpose: To make a informative message image
(define (draw-imsg a-tape)
  (let* [(tape (first (zipper-current (imsg-struct-tapes a-tape))))
         (start-index (second (zipper-current (imsg-struct-tapes a-tape))))
         (head-pos (third (zipper-current (imsg-struct-tapes a-tape))))
         (var (zipper-current (imsg-struct-vars a-tape)))]
    (define (make-tape-img loi start-index)
      (if (empty? (rest loi))
          (above (first loi)
                 (square 5 'solid 'white)
                 (text (number->string start-index) 10 'black))
          (beside (above (first loi)
                         (square 5 'solid 'white)
                         (text (number->string start-index) 10 'black))
                  (make-tape-img (rest loi) (add1 start-index)))))
    (let [(letter-imgs (build-list TAPE-SIZE
                                   (λ (i) (if (< (+ start-index i) (length tape))
                                              (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                             24
                                                             (if (= i (- head-pos start-index))
                                                                 'red
                                                                 'black))
                                                       (overlay (square 50 'solid 'white)
                                                                (square (add1 50) 'solid 'black)))
                                              (overlay (square 50 'solid 'white)
                                                       (square (add1 50) 'solid 'black))))))]
      (if (zipper-at-end? (imsg-struct-tapes a-tape))
          (above (text "The machine halts" 20 'purple)
                 (square 30 'solid 'white)
                 (make-tape-img letter-imgs start-index)
                 (square 30 'solid 'white)
                 )
          (above var
                 (square 30 'solid 'white)
                 (make-tape-img letter-imgs start-index)
                 (square 30 'solid 'white)
                 )
          )
      )))


;; create-graph-img
;; (listof edge) (listof node) edge -> img
;; Purpose: To create a graph img for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graphic loe lon edge)
  (create-edges
   (create-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                      'font "Sans"))
    lon edge)
   loe edge))


;; create-tape
;; (listof trace) -> (listof (listof tape))
;; Purpose: To create a list of lists of tapes
(define (create-tape lot)
  (if (empty? lot)
      empty
      (cons (list (tmconfig-tape (first lot))
                  (if (> (length (tmconfig-tape (first lot))) TAPE-SIZE)
                      (- (length (tmconfig-tape (first lot))) TAPE-SIZE)
                      0)
                  (tmconfig-index (first lot)))
            (create-tape (rest lot)))))

;; create-graph-imgs
;; (listof edge) (listof node) (listof edge) -> (listof image)
;; Purpose: To create a list of transition diagram images
(define (create-graphics loe lon comp-edges)
  (if (empty? comp-edges)
      empty
      (cons (create-graphic loe lon (first comp-edges))
            (create-graphics loe lon (rest comp-edges)))))

;; resize-image :: image -> int -> int -> image
;; Scales a image to the given dimentions. This solution was adapted from
;; one of the answers found here: https://stackoverflow.com/questions/3008772/how-to-smart-resize-a-displayed-image-to-original-aspect-ratio
(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define resize-width src-width)
  (define resize-height src-height)
  (define aspect (/ resize-width resize-height))
  (define scale (min
                 (/ max-width src-width) ; scale-x
                 (/ max-height src-height))) ;scale-y

  (set! resize-width (* resize-width scale))
  (set! resize-height (* resize-height scale))
  
  (when (> resize-width max-width)
    (set! resize-width max-width)
    (set! resize-height (/ resize-width aspect)))

  (when (> resize-height max-height)
    (set! aspect (/ resize-width resize-height))
    (set! resize-height max-height)
    (set! resize-width (* resize-height aspect)))

  (scale/xy
   (/ resize-width src-width)
   (/ resize-height src-height)
   img))

;; extract-labels
;; loe -> lol
;; Purpose: To extract a list of labels from edges
(define (extract-labels loe)
  (if (empty? loe)
      '()
      (cons (first (third (first loe)))
            (extract-labels (rest loe)))))

;; loe -> loe
;; Purpose: To fix the blank label in computation edges
(define (fix-blank-label loe)
  (map (λ (rule) (if (equal? (second (first (third rule))) "_")
                     (list (first rule) (second rule)
                           (list '(label "BLANK")
                                 (second (third rule))
                                 (third (third rule))
                                 (fourth (third rule))))
                     rule)) loe))


;; references
;; (listof trace) accum -> (listof number)
;; Purpose: To extract the listrefs of VARS in the trace
(define (references lot accum)
  (if (> accum (sub1 (length lot)))
      empty
      (if (not (tmconfig? (list-ref lot accum)))
          (cons accum (references lot (add1 accum)))
          (references lot (add1 accum)))))



;; remove-configs
;; (listof num) (listof configs) -> (listof configs)
;; Purpose: To remove tmconfigs in the place of vars
(define (remove-configs refs configs)
  (if (empty? refs)
      configs
      (remove (list-ref configs (first refs)) (remove-configs (rest refs) configs))))

(define viz-go-next (go-next E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-prev (go-prev E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-begin (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-end (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))


(define viz-zoom-in (zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-zoom-out (zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-out (max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-in (max-zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-reset-zoom (reset-zoom E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))

;; ctm-viz
;; ctm a-list (listof symbol) number -> void
(define (ctm-viz ctm ctm-list tape head)
  (let* [(ce (fix-blank-label (computation-edges ctm ctm-list tape head))
             )
         (last-node (second (last ce)))
         (comp-edges (append (list (list "dummy-edge" "edge-dummy" (list '(label "dummy")'(style "dummy") '(color "dummy") '(headlabel "dummy"))))
                             ce
                             (list (list last-node "edge-dummy" (list '(label "dummy") '(style "dummy")'(color "dummy") '(headlabel "dummy"))))
                             )
                     )
         (loedges (fix-blank-label (clean-list (dot-edges (parse-program ctm-list))))
                  )
         (lonodes (clean-list (dot-nodes (parse-program ctm-list))))
         (tmconfigs (filter (λ (x) (tmconfig? x)) (ctm-apply ctm tape head #t)))
         (all-vars (map (λ (x) (third x))
                        (filter (λ (x) (and (not (tmconfig? x))
                                            (equal? 'VAR (first x)))) (ctm-apply ctm tape head #t))))
         (tmc-var (filter (λ (x) (or (tmconfig? x)
                                     (equal? 'VAR (first x)))) (ctm-apply ctm tape head #t)))
         (refs (map (λ (x) (sub1 x)) (references tmc-var 0)))
         (tmconf-clean (remove-configs refs tmc-var))
         (varimgs (append (map (λ (var) (if (tmconfig? var)
                                            (text "" 20 'black)
                                            (text (format "~a = ~a" (second var)(third var)) 20 'black))) tmconf-clean)))
                                    
         (lographs (create-graphics loedges lonodes comp-edges))
         (tapes (imsg-struct (list->zipper (drop-right (create-tape tmconfigs) 1)) (list->zipper (drop-right varimgs 1))))
         (lovars (extract-labels comp-edges))
         ]
    (run-viz lographs
             (lambda () (graph->bitmap (first lographs)))
             MIDDLE-E-SCENE
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (imsg-struct (list->zipper (drop-right (create-tape tmconfigs) 1)) (list->zipper (drop-right varimgs 1)))
                                   (bounding-limits 0 0 0 0)
                                   )
             (instructions-graphic
              E-SCENE-TOOLS
              (bounding-limits 0 0 0 0))
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key ["right" viz-go-next right-key-pressed]
                                     ["left" viz-go-prev left-key-pressed]
                                     ["up" viz-go-to-begin up-key-pressed]
                                     ["down" viz-go-to-end down-key-pressed]
                                     ["w" viz-zoom-in identity]
                                     ["s" viz-zoom-out identity]
                                     ["r" viz-max-zoom-out identity]
                                     ["f" viz-max-zoom-in identity]
                                     ["e" viz-reset-zoom identity]
                                     ["wheel-down" viz-zoom-in identity]
                                     ["wheel-up" viz-zoom-out identity]
                                     ["a" identity a-key-pressed]
                                     ["d" identity d-key-pressed]
                                           
                                     )
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ()
                                      ( [ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [W-KEY-DIMS viz-zoom-in identity]
                                        [S-KEY-DIMS viz-zoom-out identity]
                                        [R-KEY-DIMS viz-max-zoom-out identity]
                                        [E-KEY-DIMS viz-reset-zoom identity]
                                        [F-KEY-DIMS viz-max-zoom-in identity]
                                        [A-KEY-DIMS identity a-key-pressed]
                                        [D-KEY-DIMS identity d-key-pressed]
                                        )
                                      )
             
             'ctm-viz)
    ))


