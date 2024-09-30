#lang racket
(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
         2htdp/universe
         2htdp/image
         "viz-state.rkt"
         "bounding-limits.rkt"
         "default-viz-functions.rkt")

(provide  create-viz-process-key
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
               [else a-vs]))]
    #;[(_ '((key viz-func imsg-func)...))
     #'(lambda (a-vs key-pressed)
         (cond [(key=? key key-pressed)
                ((compose1 viz-func imsg-func) a-vs)]
               ...
               [else a-vs]))]
    #;[(_ (list (~or* '(key viz-func imsg-func)
                    (list key viz-func imsg-func))...))
     #'(lambda (a-vs key-pressed)
         (cond [(key=? key key-pressed)
                ((compose1 viz-func imsg-func) a-vs)]
               ...
               [else a-vs]))])
  )

(define-syntax (create-viz-process-tick stx)
  (syntax-parse stx
    #:literals (list quote)
    [(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
        CLICK-BUFFER-SECONDS ([imsg-b-limit imsg-process-tick-func]...)
        ([instruction-b-limit viz-key-func imsg-key-func]...))
     #'(lambda (a-vs)
         (collect-garbage 'incremental)
         (if (viz-state-mouse-pressed a-vs)
             (let* [;; Determines the movement of the mouse that occured since the last tick
                    (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                    (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                    (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                    (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                    (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                    (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                    (viewport-lims (create-calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs) NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                    ]
               (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                      (cond [(within-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn new-img-posn]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn new-img-x (posn-y (viz-state-image-posn a-vs)))]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-and-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])])
                      
                      ]
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
                                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]) )
             (struct-copy viz-state a-vs
                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])))]
    #;[(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
        CLICK-BUFFER-SECONDS (list (~or* '(imsg-b-limit imsg-process-tick-func) (list imsg-b-limit imsg-process-tick-func))...)
        (list (~or* '(instruction-b-limit viz-key-func imsg-key-func) (list instruction-b-limit viz-key-func imsg-key-func))...))
     #'(lambda (a-vs)
         (if (viz-state-mouse-pressed a-vs)
             (let* [;; Determines the movement of the mouse that occured since the last tick
                    (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                    (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                    (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                    (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                    (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                    (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                    (viewport-lims (create-calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs) NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))]
               (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                      (cond [(within-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn new-img-posn]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn new-img-x (posn-y (viz-state-image-posn a-vs)))]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-and-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])])]
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
                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])))]
    #;[(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
        CLICK-BUFFER-SECONDS (quote (imsg-b-limit imsg-process-tick-func)...)
        (list (~or* '(instruction-b-limit viz-key-func imsg-key-func) (list instruction-b-limit viz-key-func imsg-key-func))...))
     #'(lambda (a-vs)
         (if (viz-state-mouse-pressed a-vs)
             (let* [;; Determines the movement of the mouse that occured since the last tick
                    (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                    (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                    (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                    (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                    (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                    (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                    (viewport-lims (create-calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs) NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))]
               (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                      (cond [(within-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn new-img-posn]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn new-img-x (posn-y (viz-state-image-posn a-vs)))]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-and-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])])]
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
             (struct-copy viz-state a-vs [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])))]
    #;[(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
        CLICK-BUFFER-SECONDS (list (~or* '(imsg-b-limit imsg-process-tick-func) (list imsg-b-limit imsg-process-tick-func))...)
        (quote (instruction-b-limit viz-key-func imsg-key-func)...))
     #'(lambda (a-vs)
         (if (viz-state-mouse-pressed a-vs)
             (let* [;; Determines the movement of the mouse that occured since the last tick
                    (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                    (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                    (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                    (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                    (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                    (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                    (viewport-lims (create-calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs) NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))]
               (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                      (cond [(within-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn new-img-posn]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [image-posn (posn new-img-x (posn-y (viz-state-image-posn a-vs)))]
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                            [(outside-x-and-y-axis-bounding-limits? viewport-lims new-img-posn)
                             (struct-copy viz-state a-vs
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])])]
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

(define-syntax (create-viz-draw-world stx)
  (syntax-parse stx
    #:literals (list)
    [(_ E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
     #'(lambda (a-vs)
         (let* [(PARSE-TREE-IMG (place-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs))
                                            (posn-x (viz-state-image-posn a-vs))
                                            (posn-y (viz-state-image-posn a-vs))
                                            (rectangle E-SCENE-WIDTH E-SCENE-HEIGHT 'outline 'white)
                                            ))
               (INFORMATIVE-MESSAGES ((informative-messages-draw-component (viz-state-informative-messages a-vs))
                   (informative-messages-component-state (viz-state-informative-messages a-vs))))
               (INSTRUCTIONS-GRAPHIC (instructions-graphic-img (viz-state-instructions-graphic a-vs)))
               (WINDOW-FRAME #;(empty-scene 1250 700) (rectangle 1250 700 'outline 'white))
               ]
           (overlay/align
            "middle"
            "bottom"
            (above PARSE-TREE-IMG
                  ((informative-messages-draw-component (viz-state-informative-messages a-vs))
                   (informative-messages-component-state (viz-state-informative-messages a-vs)))
                  (square INS-TOOLS-BUFFER 'solid 'white)
                  (instructions-graphic-img (viz-state-instructions-graphic a-vs)))
            WINDOW-FRAME)
            ))]))