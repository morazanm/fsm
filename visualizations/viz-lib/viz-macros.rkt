#lang racket/base
(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     )
         (only-in racket/gui
                  get-display-size
                  get-display-count)
         "../2htdp/universe.rkt"
         "../2htdp/image.rkt"
         "viz-state.rkt"
         "bounding-limits.rkt"
         "default-viz-function-generators.rkt"
         "os-dependent-constants.rkt")

(provide create-viz-process-key
         create-viz-draw-world
         create-viz-process-tick)

(define-syntax (create-viz-process-key stx)
  (syntax-parse stx
    #:literals (list)
    [(_ [key viz-func imsg-func]...)
     #'(lambda (a-vs key-pressed)
         (cond [(key=? key key-pressed)
                ((compose1 viz-func imsg-func) a-vs)]
               ...
               [else a-vs]))]))

(define-syntax (create-viz-process-tick stx)
  (syntax-parse stx
    #:literals (list quote)
    [(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
        CLICK-BUFFER-SECONDS ([imsg-b-limit imsg-process-tick-func]...)
        ([instruction-b-limit viz-key-func imsg-key-func]...))
     #'(lambda (a-vs)
         (if (viz-state-mouse-pressed a-vs)
             (let [(x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                   (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                   (viewport-lims (calculate-viewport-limits (viz-state-scaled-curr-image a-vs)
                                                             (viz-state-scale-factor a-vs)
                                                             E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE))]
               (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                      (let [(new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))]
                        (struct-copy viz-state a-vs
                                     [image-posn (posn (if (outside-x-axis-bounding-limits? viewport-lims new-img-posn)
                                                           (posn-x (viz-state-image-posn a-vs))
                                                           (posn-x new-img-posn))
                                                       (if (outside-y-axis-bounding-limits? viewport-lims new-img-posn)
                                                           (posn-y (viz-state-image-posn a-vs))
                                                           (posn-y new-img-posn)))]
                                     [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))]
                     [(within-bounding-limits? imsg-b-limit (viz-state-prev-mouse-posn a-vs))
                      (struct-copy viz-state a-vs
                                   [informative-messages
                                    (struct-copy informative-messages
                                                 (viz-state-informative-messages a-vs)
                                                 [component-state (imsg-process-tick-func
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))
                                                                   x-diff y-diff)])]
                                   [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                     ...
                     [(within-bounding-limits? instruction-b-limit (viz-state-prev-mouse-posn a-vs))
                      (buffer-held-click
                       (struct-copy viz-state a-vs
                                    [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])
                       (lambda (a-vs) (viz-key-func (imsg-key-func a-vs))) CLICK-BUFFER-SECONDS)]
                     ...
                     [else (struct-copy viz-state a-vs
                                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]))
             (struct-copy viz-state a-vs
                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])))]))

(define (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
  (lambda (a-vs)
    (let* [(INFORMATIVE-MESSAGES ((informative-messages-draw-component (viz-state-informative-messages a-vs))
                                  (informative-messages-component-state (viz-state-informative-messages a-vs))))
           (INSTRUCTIONS-GRAPHIC (instructions-graphic-img (viz-state-instructions-graphic a-vs)))
           (PARSE-TREE-IMG (place-image (viz-state-scaled-curr-image a-vs)
                                        (posn-x (viz-state-image-posn a-vs))
                                        (posn-y (viz-state-image-posn a-vs))
                                        (rectangle E-SCENE-WIDTH (- (* 0.9 WINDOW-HEIGHT)
                                                                    (image-height INFORMATIVE-MESSAGES)
                                                                    (image-height INSTRUCTIONS-GRAPHIC))
                                                   'outline 'white)))
           (WINDOW-FRAME (rectangle (* 0.95 WINDOW-WIDTH) (* 0.9 WINDOW-HEIGHT) 'outline 'white))]
      (overlay/align
       "middle"
       "bottom"
       ;; scale-factor * display-scaling = 1
       (above (scale (/ 1 SCREEN-SCALING)
                     PARSE-TREE-IMG)
              (scale (/ 1 SCREEN-SCALING)
                     ((informative-messages-draw-component (viz-state-informative-messages a-vs))
                      (informative-messages-component-state (viz-state-informative-messages a-vs))))
              (scale (/ 1 SCREEN-SCALING)
                     (square INS-TOOLS-BUFFER 'solid 'white))
              (scale (/ 1 SCREEN-SCALING)
                     (instructions-graphic-img (viz-state-instructions-graphic a-vs))))
       (scale (/ 1 SCREEN-SCALING)
              WINDOW-FRAME)))))