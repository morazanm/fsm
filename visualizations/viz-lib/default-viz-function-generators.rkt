#lang racket
(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt")
         "zipper.rkt"
         "viz-state.rkt"
         math/matrix
         "bounding-limits.rkt"
         2htdp/image
         "resize-viz-image.rkt"
         "viz-constants.rkt"
         )

(provide (all-defined-out))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                              THESE MACROS ARE INTENTIONALLY UNHYGENIC
;                        THE CONSTANTS NEED TO BE DEFINED IN THE CALLING CONTEXT
;                        THE VISUALIZATION LITERALLY CANNOT RUN IF THEY ARE NOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; img posn num>0 num num-> matrix x y 1
;; Calculates the transform needed to zoom correctly
(define-syntax (zoom-affine-transform stx)
  (syntax-parse stx
    [(_ img img-posn scale)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)])
     #'(let* [(transformed-x (* -1 (+ (- (/ E-SCENE-WIDTH 2)
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
                                                                              #:point (matrix [ [0] [0] [1] ])))))
         
       )
     ]
    )
  )


;; img num>0 num num num -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define-syntax (calculate-viewport-limits stx)
  (syntax-parse stx
    [(_ scaled-image scale)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)])
     #'(let* [(img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale)))
                (img-height-node-diff (- (/ (image-height scaled-image) 2) (* NODE-SIZE scale)))
                (scaled-node-size (* NODE-SIZE scale))
                (MIN-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                           (- (* -1 (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH)) (- E-SCENE-WIDTH scaled-node-size) )
                           (* -1 img-width-node-diff)))
                (MAX-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                           (+ (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH) E-SCENE-WIDTH (- E-SCENE-WIDTH scaled-node-size) )
                           (+ E-SCENE-WIDTH img-width-node-diff)))                                             
                (MIN-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                           (- (* -1 (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT)) (- E-SCENE-HEIGHT scaled-node-size))
                           (* -1 img-height-node-diff)))
                (MAX-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                           (+ (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT) E-SCENE-HEIGHT (- E-SCENE-HEIGHT scaled-node-size))
                           (+ E-SCENE-HEIGHT img-height-node-diff)))]
           (bounding-limits MIN-X MAX-X MIN-Y MAX-Y))
         )]
    )
  )

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
;ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
(define-syntax (zoom stx)
  (syntax-parse stx
    [(_ a-vs factor)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)]
                   [ZOOM-INCREASE (datum->syntax stx 'ZOOM-INCREASE)]
                   [ZOOM-DECREASE (datum->syntax stx 'ZOOM-DECREASE)])
  #'(let* [(new-scale (* factor (viz-state-scale-factor a-vs)))
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
          a-vs))
  )
     ]
    )
  )

;; viz-state ( Any* -> viz-state) -> viz-state
;; Purpose: Prevents holding down a left or right mouse click from spamming a function too fast
(define-syntax (buffer-held-click stx)
  (syntax-parse stx
    [(_ a-vs func)
     (with-syntax ([CLICK-BUFFER-SECONDS (datum->syntax stx 'CLICK-BUFFER-SECONDS)])
  #'(if (= (viz-state-click-buffer a-vs) 0)
      (func (struct-copy viz-state a-vs
                         [click-buffer 1]
                         [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))
      (if (= (viz-state-click-buffer a-vs) CLICK-BUFFER-SECONDS)
          (struct-copy viz-state a-vs
                       [click-buffer 0]
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])
          (struct-copy viz-state a-vs
                       [click-buffer (add1 (viz-state-click-buffer a-vs))]
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])))
  
       )
     ]
    )
  )


;; img -> boolean
;; Checks to see if an image needs to be resized
(define-syntax (does-img-need-resizing? stx)
  (syntax-parse stx
    [(_ img)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)])
  #'(or (< E-SCENE-WIDTH (image-width img))
      (< E-SCENE-HEIGHT (image-height img)))
       )
     ]
    )
  )

;; viz-state -> viz-state
;; Purpose: Moves the visualization to the next step of the derivation
(define-syntax (go-next stx)
  (syntax-parse stx
    [(_)
    (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [DEFAULT-ZOOM-FLOOR (datum->syntax stx 'DEFAULT-ZOOM-FLOOR)]
                   [PERCENT-BORDER-GAP (datum->syntax stx 'PERCENT-BORDER-GAP)]
                   )
        #'(lambda (a-vs)
            (if (zipper-at-end? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (zipper-next (viz-state-imgs a-vs)))
               (new-curr-img (if (list? (zipper-current new-imgs))
                                   (apply beside (map (lambda (img) (img)) (zipper-current new-imgs)))
                                   
                                 ((zipper-current new-imgs))
                                 )
                             )
               (curr-pimgs-img (viz-state-curr-image a-vs))
               (img-resize (resize-image new-curr-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (does-img-need-resizing? new-curr-img)
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
    )
    ]
  )
  )

;; viz-state -> viz-state
;; Purpose: Moves the visualization one step back in the derivation
(define-syntax (go-prev stx)
  ;E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (syntax-parse stx
    [(_)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [DEFAULT-ZOOM-FLOOR (datum->syntax stx 'DEFAULT-ZOOM-FLOOR)]
                   [PERCENT-BORDER-GAP (datum->syntax stx 'PERCENT-BORDER-GAP)]
                   )
    #'(lambda (a-vs)
        (if (zipper-at-begin? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (zipper-prev (viz-state-imgs a-vs)))
               (new-pimgs-img (if (list? (zipper-current new-imgs))
                                   (apply beside (map (lambda (img) (img)) (zipper-current new-imgs)))
                                   
                                 ((zipper-current new-imgs))
                                 )
                             )
               (curr-pimgs-img (viz-state-curr-image a-vs))
               #;(new-pimgs-img ((zipper-current new-imgs)))
               ;(curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (does-img-need-resizing? new-pimgs-img)
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
    )
     ]
  )
  )


;; viz-state -> viz-state
;; Purpose: Restarts the derivation in the visualization
(define-syntax (go-to-begin stx)
  ;E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (syntax-parse stx
    [(_)
    (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [DEFAULT-ZOOM-FLOOR (datum->syntax stx 'DEFAULT-ZOOM-FLOOR)]
                   [PERCENT-BORDER-GAP (datum->syntax stx 'PERCENT-BORDER-GAP)]
                   )
    #'(lambda (a-vs)
        (if (zipper-at-begin? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (zipper-to-begin (viz-state-imgs a-vs)))
               (new-pimgs-img (if (list? (zipper-current new-imgs))
                                   (apply beside (map (lambda (img) (img)) (zipper-current new-imgs)))
                                   
                                 ((zipper-current new-imgs))
                                 )
                             )
               (curr-pimgs-img (viz-state-curr-image a-vs))
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
                                                         [scale-factor-floor NEW-FLOOR]
                                                         ))]
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
                                                         [scale-factor-floor NEW-FLOOR]
                                                         ))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                         (viz-state-scale-factor new-viz-state))
                                                       new-pimgs-img
                                                       (viz-state-scale-factor new-viz-state)))
                       ]
                      [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                              [imgs new-imgs]
                                                              [curr-image new-pimgs-img]
                                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                              [scale-factor-floor NEW-FLOOR]
                                                              ))]
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
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                ))]
                (reposition-out-of-bounds-img new-viz-state
                                              (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                              new-pimgs-img
                                              (viz-state-scale-factor new-viz-state)))))))
    )
    ]
    )
  )

;; viz-state -> viz-state
;; Purpose: Finishes the derivations in the visualization
(define-syntax (go-to-end stx)
  ;E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (syntax-parse stx
      [(_)
       (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [DEFAULT-ZOOM-FLOOR (datum->syntax stx 'DEFAULT-ZOOM-FLOOR)]
                   [PERCENT-BORDER-GAP (datum->syntax stx 'PERCENT-BORDER-GAP)]
                   )
    #'(lambda (a-vs)
        (if (zipper-at-end? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (zipper-to-end (viz-state-imgs a-vs)))
               (new-pimgs-img (if (list? (zipper-current new-imgs))
                                   (apply beside (map (lambda (img) (img)) (zipper-current new-imgs)))
                                   
                                 ((zipper-current new-imgs))
                                 )
                             )
               (curr-pimgs-img (viz-state-curr-image a-vs))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               ]
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
                                                         [scale-factor-floor NEW-FLOOR]
                                                         ))]
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
                                                         [scale-factor-floor NEW-FLOOR]
                                                        ))]
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
                                                              [scale-factor-floor NEW-FLOOR]
                                                              ))]
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
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                ))]
                (reposition-out-of-bounds-img new-viz-state
                                              (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state))
                                              new-pimgs-img
                                              (viz-state-scale-factor new-viz-state)))))))
    )
       ]
    )
  )

;; viz-state -> viz-state
;; Purpose: Zooms in on the visualization
(define-syntax (zoom-in stx)
  ;ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (syntax-parse stx
    [(_)
     (with-syntax  ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]

                   [ZOOM-INCREASE (datum->syntax stx 'ZOOM-INCREASE)]
                   [ZOOM-DECREASE (datum->syntax stx 'ZOOM-DECREASE)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)])

                   #'(lambda (a-vs) (zoom a-vs ZOOM-INCREASE))
                   )]
                   ))

;; viz-state -> viz-state
;; Purpose: Zooms out the visualization
(define-syntax (zoom-out stx)
  (syntax-parse stx
    [(_)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]

                   [ZOOM-INCREASE (datum->syntax stx 'ZOOM-INCREASE)]
                   [ZOOM-DECREASE (datum->syntax stx 'ZOOM-DECREASE)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)])
         ;ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  #'(lambda (a-vs) (zoom a-vs ZOOM-DECREASE)))]))

;; viz-state -> viz-state
;; Purpose: Zooms all the way in on the visualization
(define-syntax (max-zoom-in stx)
  (syntax-parse stx
    ;ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  [(_) (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [ZOOM-INCREASE (datum->syntax stx 'ZOOM-INCREASE)]
                   [ZOOM-DECREASE (datum->syntax stx 'ZOOM-DECREASE)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)])
               #'(lambda (a-vs) (zoom a-vs (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs)))))
             ]
    )
  )


;; viz-state -> viz-state
;; Purpose: Zooms in a moderate amount on the visualization
(define-syntax (reset-zoom stx)
  ;ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (syntax-parse stx
    [(_)
     (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [ZOOM-INCREASE (datum->syntax stx 'ZOOM-INCREASE)]
                   [ZOOM-DECREASE (datum->syntax stx 'ZOOM-DECREASE)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)])
       #'(lambda (a-vs) (zoom a-vs (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs))
               )))]))

;; viz-state -> viz-state
;; Purpose: Zooms all the way out in the visualization
(define-syntax (max-zoom-out stx)
  ;;E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM NODE-SIZE)
  (syntax-parse stx
    [(_) (with-syntax ([E-SCENE-WIDTH (datum->syntax stx 'E-SCENE-WIDTH)]
                   [E-SCENE-HEIGHT (datum->syntax stx 'E-SCENE-HEIGHT)]
                   [DEFAULT-ZOOM-CAP (datum->syntax stx 'DEFAULT-ZOOM-CAP)]
                   [ZOOM-INCREASE (datum->syntax stx 'ZOOM-INCREASE)]
                   [ZOOM-DECREASE (datum->syntax stx 'ZOOM-DECREASE)]
                   [NODE-SIZE (datum->syntax stx 'NODE-SIZE)])
    #'(lambda (a-vs)
        (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
          (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs)))
          )
      (let [(img-resize (resize-image (viz-state-curr-image a-vs) (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))]
        (zoom a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs))))
      (struct-copy viz-state a-vs
                   [scale-factor DEFAULT-ZOOM]))))]
    ))
  