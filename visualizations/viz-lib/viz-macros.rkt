#lang racket/base
(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/syntax)
         (only-in "viz-constants.rkt"
                  WINDOW-WIDTH
                  WINDOW-HEIGHT)
         "../2htdp/universe.rkt"
         "../2htdp/image.rkt"
         "viz-state.rkt"
         "bounding-limits.rkt"
         "default-viz-function-generators.rkt")

(provide  create-viz-process-key
          create-viz-draw-world
          create-viz-process-tick)

(define-syntax (create-viz-process-key stx)
  (define-syntax-class
    key-triple
    (pattern (key-name viz-func0 imsg-func0)
      #:with key #'key-name
      #:with viz-func #'viz-func0
      #:with imsg-func #'imsg-func0))
  (define-syntax-class
    key-pair
    (pattern (key-name func0)
      #:with key #'key-name
      #:with func #'func0))
  (syntax-parse stx
    [(_ (~var key-tuple key-triple) ...)
     #'(create-viz-process-key (key-tuple ...) ())]
    [(_ ((~var key-tuple key-triple) (~seq (~var rest-key-tuples key-triple) ...)) ())
     #:with temp-id (generate-temporary)
     #'(let ([temp-id (compose1 key-tuple.viz-func key-tuple.imsg-func)])
         (create-viz-process-key (rest-key-tuples ...) ((key-tuple.key temp-id))))]
    [(_ ((~var key-tuple key-triple) (~seq (~var rest-key-tuples key-triple) ...)) ((~var key-pairs key-pair) ...))
     #:with temp-id (generate-temporary)
     #'(let ([temp-id (compose1 key-tuple.viz-func key-tuple.imsg-func)])
         (create-viz-process-key (rest-key-tuples ...) ((key-tuple.key temp-id) key-pairs ...)))]
    [(_ () ((~var key-pairs key-pair) ...))
     #'(lambda (a-vs key-pressed)
         (cond [(key=? key-pairs.key key-pressed)
                (key-pairs.func a-vs)]
               ...
               [else a-vs]))]))

(define (clicked-inside-main-viz-generator E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
  (define calculate-viewport-limits (calculate-viewport-limits-generator E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE))
  (lambda (a-vs x-diff y-diff)
    (define new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
    (define scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
    (define viewport-lims (calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs)))
    (cond [(within-bounding-limits? viewport-lims new-img-posn)
           (struct-copy viz-state a-vs
                        [image-posn new-img-posn]
                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
          [(outside-x-axis-bounding-limits? viewport-lims new-img-posn)
           (struct-copy viz-state a-vs
                        [image-posn (posn (posn-x (viz-state-image-posn a-vs)) (posn-y new-img-posn))]
                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
          [(outside-y-axis-bounding-limits? viewport-lims new-img-posn)
           (struct-copy viz-state a-vs
                        [image-posn (posn (posn-x new-img-posn) (posn-y (viz-state-image-posn a-vs)))]
                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
          [(outside-x-and-y-axis-bounding-limits? viewport-lims new-img-posn)
           (struct-copy viz-state a-vs
                        [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])])))

(define (clicked-inside-imsg a-vs imsg-process-tick-func x-diff y-diff)
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state (imsg-process-tick-func
                                               (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))
                                               x-diff y-diff)])]
               [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))

(define (update-mouse-posn a-vs)
  (struct-copy viz-state a-vs
               [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))

(define ((clicked-inside-instructions-generator CLICK-BUFFER-SECONDS) a-vs viz-key-func imsg-key-func)
  (let ([buffer-held-click (buffer-held-click-generator CLICK-BUFFER-SECONDS)]
        [func (compose1 viz-key-func imsg-key-func)])
    (buffer-held-click (update-mouse-posn a-vs) func)))

(define-syntax (create-viz-process-tick stx)
  (syntax-parse stx
    [(_ E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
        CLICK-BUFFER-SECONDS ([imsg-b-limit imsg-process-tick-func]...)
        ([instruction-b-limit viz-key-func imsg-key-func]...))
     #'(let ([clicked-inside-main-viz (clicked-inside-main-viz-generator E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)]
             [clicked-inside-instructions (clicked-inside-instructions-generator CLICK-BUFFER-SECONDS)])
         (lambda (a-vs)
           (if (viz-state-mouse-pressed a-vs)
               (let [(x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                     (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))]
                 (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
                        (clicked-inside-main-viz a-vs x-diff y-diff)]
                       [(within-bounding-limits? imsg-b-limit (viz-state-prev-mouse-posn a-vs))
                        (clicked-inside-imsg a-vs imsg-process-tick-func x-diff y-diff)]
                       ...
                       [(within-bounding-limits? instruction-b-limit (viz-state-prev-mouse-posn a-vs))
                        (clicked-inside-instructions a-vs viz-key-func imsg-key-func)]
                       ...
                       [else (update-mouse-posn a-vs)]))
               (update-mouse-posn a-vs))))]))

(define (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER INSTRUCTIONS-GRAPHIC)
  (define WINDOW-FRAME (rectangle (* 0.95 WINDOW-WIDTH) (* 0.9 WINDOW-HEIGHT) 'outline 'white))
  (define INS-TOOL-BUFFER-SQUARE (square INS-TOOLS-BUFFER 'solid 'white))
  (define INSTRUCTIONS-GRAPHIC-HEIGHT (image-height INSTRUCTIONS-GRAPHIC))
  (lambda (a-vs)
    (define INFORMATIVE-MESSAGES ((informative-messages-draw-component (viz-state-informative-messages a-vs))
                                  (informative-messages-component-state (viz-state-informative-messages a-vs))))
    (define PARSE-TREE-IMG (place-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs))
                                        (posn-x (viz-state-image-posn a-vs))
                                        (posn-y (viz-state-image-posn a-vs))
                                        (rectangle E-SCENE-WIDTH (- (* 0.9 WINDOW-HEIGHT)
                                                                    (image-height INFORMATIVE-MESSAGES)
                                                                    INSTRUCTIONS-GRAPHIC-HEIGHT)
                                                   'outline 'white)))
  
    (overlay/align
     "middle"
     "bottom"
     (above PARSE-TREE-IMG
            INFORMATIVE-MESSAGES
            INS-TOOL-BUFFER-SQUARE
            INSTRUCTIONS-GRAPHIC)
     WINDOW-FRAME)))