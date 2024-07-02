#lang racket
(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt")
         2htdp/universe
         2htdp/image
         "viz-state.rkt"
         "bounding-limits.rkt"
         ;"viz.rkt"
         "default-viz-functions.rkt"
         )
(provide create-imsg-process-key create-imsg-process-tick create-instructions-process-tick create-viz-process-key
         create-viz-draw-world create-viz-process-tick)

(define-syntax (create-imsg-process-key stx)
  (syntax-parse stx
    #:literals (list)
    [(_ '((key func)...))
     #'(lambda (a-imsgs key-pressed)
         (cond [(key=? key key-pressed) (func a-imsgs)]...
               [else a-imsgs]
               ))]
    [(_ (list (~or* '(key func) (list key func))...))
     #'(lambda (a-imsgs key-pressed)
         (cond [(key=? key key-pressed) (func a-imsgs)]...
               [else a-imsgs]
               ))]))

(define-syntax (create-imsg-process-tick stx)
  (syntax-parse stx
    #:literals (list)
    [(_ '((b-limit func)...))
     #'(lambda (a-vs x-diff y-diff)
         (let [(updated-vs (struct-copy viz-state a-vs
                                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs))
                  (struct-copy viz-state updated-vs
                               [informative-messages (struct-copy informative-messages (viz-state-informative-messages updated-vs)
                                                                  [component-state (func (informative-messages-component-state (viz-state-informative-messages updated-vs))
                                                                                         x-diff
                                                                                         y-diff)]
                                                                  )
                                                     ]
                               )
                  ]...
                   [else updated-vs]
                   )
           )
         )]
    [(_ (list (~or* '(b-limit func) (list b-limit func))...))
     #'(lambda (a-vs x-diff y-diff)
         (let [(updated-vs (struct-copy viz-state a-vs
                                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs))
                  (struct-copy viz-state updated-vs
                               [informative-messages (struct-copy informative-messages (viz-state-informative-messages updated-vs)
                                                                  [component-state (func (informative-messages-component-state (viz-state-informative-messages updated-vs))
                                                                                         x-diff y-diff)]
                                                                  )
                                                     ]
                               )
                  ]...
                   [else updated-vs]
                   )
           )
         )]))

(define-syntax (create-instructions-process-tick stx)
  (syntax-parse stx
    #:literals (list)
    [(_ CLICK-BUFFER-SECONDS '((b-limit func)...))
     #'(lambda (a-vs)
         (let [
               (updated-vs (struct-copy viz-state a-vs
                                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs)) (buffer-held-click updated-vs func CLICK-BUFFER-SECONDS)]...
                 [else updated-vs]
                 )
           )
         )]
    [(_ CLICK-BUFFER-SECONDS (list (~or* '(b-limit func) (list b-limit func))...))
     #'(lambda (a-vs)
         (let [(updated-vs (struct-copy viz-state a-vs
                                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs)) (buffer-held-click updated-vs func CLICK-BUFFER-SECONDS)]...
                 [else updated-vs]
                 )
           )
         )]))


(define-syntax (create-viz-process-key stx)
  (syntax-parse stx
    #:literals (list)
    [(_ '((key func)...))
     #'(lambda (a-vs key-pressed)
         (let [(updated-vs (struct-copy viz-state a-vs
                                        [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                           [component-state ((informative-messages-process-key (viz-state-informative-messages a-vs))
                                                                                             (informative-messages-component-state (viz-state-informative-messages a-vs)) key-pressed)]
                                                                           )]))]
           (cond [(key=? key key-pressed) (func updated-vs)]...
                 [else updated-vs]
                 )
           )
         )
     ]
    [(_ (list (~or* '(key func) (list key func))...))
     #'(lambda (a-vs key-pressed)
         (let [(updated-vs (struct-copy viz-state a-vs
                                        [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                           [component-state ((informative-messages-process-key (viz-state-informative-messages a-vs))
                                                                                             (informative-messages-component-state (viz-state-informative-messages a-vs)) key-pressed)]
                                                                           )]))]
           (cond [(key=? key key-pressed) (func updated-vs)]...
                 [else updated-vs]
                 )
           )
         )
     ]
    )
  )

(define-syntax (create-viz-process-tick stx)
  (syntax-parse stx
    #:literals (list)
    [(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
     #'(lambda (a-vs)
         (if (viz-state-mouse-pressed a-vs)
             (let* [;; Determines the movement of the mouse that occured since the last tick
                           (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                           (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                           (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                           (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                           (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                           (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                           (viewport-lims (create-calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs) NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                           
                           ;(scroll-dimensions RULE-YIELD-DIMS)
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
                   [(within-bounding-limits? (informative-messages-bounding-limits (viz-state-informative-messages a-vs))
                                             (viz-state-prev-mouse-posn a-vs))
                    ((informative-messages-process-tick (viz-state-informative-messages a-vs)) a-vs x-diff y-diff)
                    #;(struct-copy viz-state a-vs
                                 [informative-messages (struct-copy informative-messages (viz-state-informative-messages a-vs)
                                                                    [component-state ((informative-messages-process-tick (viz-state-informative-messages a-vs))
                                                                                      (informative-messages-component-state (viz-state-informative-messages a-vs)))])
                                                       ])
                    ]
                   #;[(within-bounding-limits? (instructions-graphic-bounding-limits (viz-state-instructions-graphic a-vs))
                                             (viz-state-prev-mouse-posn a-vs))
                    ((instructions-graphic-process-tick (viz-state-instructions-graphic a-vs)) a-vs)
                    ]
                   #;[else (struct-copy viz-state a-vs
                                      [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                   [else ((instructions-graphic-process-tick (viz-state-instructions-graphic a-vs)) a-vs)]
                   )
               )
             (struct-copy viz-state a-vs
                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])
             )
         )
     ]
    [(_ E-SCENE-BOUNDING-LIMITS)
     #'(lambda (a-vs)
         (if (viz-state-mouse-pressed a-vs)
             (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                    (let* [;; Determines the movement of the mouse that occured since the last tick
                           (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                           (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                           (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                           (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                           (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                           (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                           (viewport-lims (calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs) NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
         
                           ;(scroll-dimensions RULE-YIELD-DIMS)
                           ]
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
                      )
                    ]
                   [(within-bounding-limits? (informative-messages-bounding-limits (viz-state-informative-messages a-vs))
                                             (viz-state-prev-mouse-posn a-vs))
                    ((informative-messages-process-tick a-vs) (informative-messages-component-state (viz-state-informative-messages a-vs)))
                    ]
                   [(within-bounding-limits? (instructions-graphic-bounding-limits (viz-state-instructions-graphic a-vs))
                                             (viz-state-prev-mouse-posn a-vs))
                    ((instructions-graphic-process-tick (viz-state-instructions-graphic a-vs)) a-vs)
                    ]
                   [else (struct-copy viz-state a-vs
                                      [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                   )
             (struct-copy viz-state a-vs
                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])
             )
         )
     ]
    )
  )

(define-syntax (create-viz-draw-world stx)
  (syntax-parse stx
    #:literals (list)
    [(_ E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
     #'(lambda (a-vs)
         (let [(PARSE-TREE-IMG (place-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)) 
                                            (posn-x (viz-state-image-posn a-vs))
                                            (posn-y (viz-state-image-posn a-vs))
                                            (rectangle E-SCENE-WIDTH E-SCENE-HEIGHT 'outline 'white)
                                            ))]
           (above PARSE-TREE-IMG
                  ((informative-messages-draw-component (viz-state-informative-messages a-vs)) (informative-messages-component-state (viz-state-informative-messages a-vs)))
                  (square INS-TOOLS-BUFFER 'solid 'white)
                  (instructions-graphic-img (viz-state-instructions-graphic a-vs)))))]))