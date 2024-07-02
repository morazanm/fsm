#lang racket

(require "zipper.rkt"
         "viz-state.rkt"
         math/matrix
         "bounding-limits.rkt"
         2htdp/image
         )

(provide create-zoom-in create-zoom-out create-max-zoom-in create-max-zoom-out create-reset-zoom
         go-next go-prev go-to-begin go-to-end buffer-held-click create-calculate-viewport-limits
         )

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

;; img posn num>0 num num-> matrix x y 1
;; Calculates the transform needed to zoom correctly 
(define (create-zoom-affine-transform img img-posn scale E-SCENE-WIDTH E-SCENE-HEIGHT)
  (let* [(transformed-x (* -1 (+ (- (/ E-SCENE-WIDTH 2)
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

;; img num>0 num num num -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define (create-calculate-viewport-limits scaled-image scale NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (let* [(img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale)))
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
    
  )

;; viz-state viewport-limits img num>0 -> viz-state
;; Returns a new viz-state where if the given image would be out of bounds of its viewport limits
;; It is placed into a position inbounds
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

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
(define (create-zoom ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (lambda (a-vs factor)
    (let* [(new-scale (* factor (viz-state-scale-factor a-vs)))
           (scalable? (cond [(eq? factor ZOOM-INCREASE) (> (viz-state-scale-factor-cap a-vs) new-scale)]
                            [(eq? factor ZOOM-DECREASE) (< (viz-state-scale-factor-floor a-vs) new-scale)]))]
      (if scalable?
          (let* [(scaled-image (scale new-scale (viz-state-curr-image a-vs)))
                 (viewport-lims (create-calculate-viewport-limits scaled-image new-scale NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT))
                 (scale-increase (/ new-scale (viz-state-scale-factor a-vs)))
                 (affine-matrix (create-zoom-affine-transform (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs))
                                                              (viz-state-image-posn a-vs)
                                                              scale-increase
                                                              E-SCENE-WIDTH E-SCENE-HEIGHT))]
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
  )

;; viz-state ( Any* -> viz-state) -> viz-state
;; Purpose: Prevents holding down a left or right mouse click from spamming a function too fast
(define (buffer-held-click a-vs func CLICK-BUFFER-SECONDS)
  (if (= (viz-state-click-buffer a-vs) 0)
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

;; image int int -> image
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

;; img -> boolean
;; Checks to see if an image needs to be resized
(define (create-does-img-need-resizing? img E-SCENE-WIDTH E-SCENE-HEIGHT)
  (or (< E-SCENE-WIDTH (image-width img))
      (< E-SCENE-HEIGHT (image-height img))))

;; viz-state -> viz-state
;; Purpose: Moves the visualization to the next step of the derivation
(define (go-next E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (lambda (a-vs)
    (if (zipper-at-end? (viz-state-imgs a-vs))
        a-vs
        (let* [#;(new-yield (if (zipper-at-end? (viz-state-yield a-vs))
                                (viz-state-yield a-vs)
                                (zipper-next (viz-state-yield a-vs))))
               (new-imgs (zipper-next (viz-state-imgs a-vs)))
               (new-curr-img ((zipper-current new-imgs)))
               (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
               (img-resize (resize-image new-curr-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (create-does-img-need-resizing? new-curr-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let [(NEW-FLOOR (min (second img-resize) (third img-resize)))]
                (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                       (let [(new-viz-state (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-curr-img]
                                                         [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                           (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                         [scale-factor DEFAULT-ZOOM-CAP]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor NEW-FLOOR]
                                                         #;[rules (zipper-next (viz-state-rules a-vs))]
                                                         #;[yield new-yield]
                                                         #;[broken-invariants (zipper-next (viz-state-broken-invariants a-vs))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                         (viz-state-scale-factor new-viz-state)
                                                                                         NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
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
                                                         [scale-factor-floor NEW-FLOOR]
                                                         #;[rules (zipper-next (viz-state-rules a-vs))]
                                                         #;[yield new-yield]
                                                         #;[broken-invariants (zipper-next (viz-state-broken-invariants a-vs))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                         (viz-state-scale-factor new-viz-state)
                                                                                         NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                       new-curr-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                              [imgs new-imgs]
                                                              [curr-image new-curr-img]
                                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                              [scale-factor-floor NEW-FLOOR]
                                                              #;[rules (zipper-next (viz-state-rules a-vs))]
                                                              #;[yield new-yield]
                                                              #;[broken-invariants (zipper-next (viz-state-broken-invariants a-vs))]))]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                              (viz-state-scale-factor new-viz-state)
                                                                                              NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                            new-curr-img
                                                            (viz-state-scale-factor new-viz-state)))]))
              (let [(new-viz-state (struct-copy viz-state a-vs
                                                [imgs new-imgs]
                                                [curr-image new-curr-img]
                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                #;[rules (zipper-next (viz-state-rules a-vs))]
                                                #;[yield new-yield]
                                                #;[broken-invariants (zipper-next (viz-state-broken-invariants a-vs))]))]
                (reposition-out-of-bounds-img new-viz-state
                                              (create-calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                                                                (viz-state-scale-factor a-vs)
                                                                                NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                              new-curr-img
                                              (viz-state-scale-factor a-vs))))))
    )
  )

;; viz-state -> viz-state
;; Purpose: Moves the visualization one step back in the derivation
(define (go-prev E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (lambda (a-vs)
    (if (zipper-at-begin? (viz-state-imgs a-vs))
        a-vs
        (let* [#;(new-yield (if (zipper-at-begin? (viz-state-yield a-vs))
                                (viz-state-yield a-vs)
                                (zipper-prev (viz-state-yield a-vs))))
               (new-imgs (zipper-prev (viz-state-imgs a-vs)))
               (new-pimgs-img ((zipper-current new-imgs)))
               (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (create-does-img-need-resizing? new-pimgs-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let [(NEW-FLOOR (min (second img-resize) (third img-resize)))]
                (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                       (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                  [imgs new-imgs]
                                                                  [curr-image new-pimgs-img]
                                                                  [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                    (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                                  [scale-factor DEFAULT-ZOOM-CAP]
                                                                  [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                  [scale-factor-floor NEW-FLOOR]
                                                                  #;[rules (zipper-prev (viz-state-rules a-vs))]
                                                                  #;[yield new-yield]
                                                                  #;[broken-invariants (zipper-prev (viz-state-broken-invariants a-vs))]
                                                                  )
                                                     (create-calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                       (viz-state-scale-factor a-vs)
                                                                                       NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                     new-pimgs-img
                                                     (viz-state-scale-factor a-vs))]
                      [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                       (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                  [imgs new-imgs]
                                                                  [curr-image new-pimgs-img]
                                                                  [scale-factor NEW-FLOOR]
                                                                  [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                  [scale-factor-floor NEW-FLOOR]
                                                                  #;[rules (zipper-prev (viz-state-rules a-vs))]
                                                                  #;[yield new-yield]
                                                                  #;[broken-invariants (zipper-prev (viz-state-broken-invariants a-vs))]
                                                                  )
                                                     (create-calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                       (viz-state-scale-factor a-vs)
                                                                                       NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                     new-pimgs-img
                                                     (viz-state-scale-factor a-vs))]
                      [else (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                       [imgs new-imgs]
                                                                       [curr-image new-pimgs-img]
                                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                       [scale-factor-floor NEW-FLOOR]
                                                                       #;[rules (zipper-prev (viz-state-rules a-vs))]
                                                                       #;[yield new-yield]
                                                                       #;[broken-invariants (zipper-prev (viz-state-broken-invariants a-vs))]
                                                                       )
                                                          (create-calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                            (viz-state-scale-factor a-vs)
                                                                                            NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                          new-pimgs-img
                                                          (viz-state-scale-factor a-vs))]))
              (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-pimgs-img]
                                                         [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                           (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                         #;[rules (zipper-prev (viz-state-rules a-vs))]
                                                         #;[yield new-yield]
                                                         #;[broken-invariants (zipper-prev (viz-state-broken-invariants a-vs))]
                                                         )
                                            (create-calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                              (viz-state-scale-factor a-vs)
                                                                              NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                            new-pimgs-img
                                            (viz-state-scale-factor a-vs)))))
    )
  )

;; viz-state -> viz-state
;; Purpose: Restarts the derivation in the visualization
(define (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (lambda (a-vs)
    (if (zipper-at-begin? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (zipper-to-begin (viz-state-imgs a-vs)))
               (new-pimgs-img ((zipper-current new-imgs)))
               (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (create-does-img-need-resizing? new-pimgs-img E-SCENE-WIDTH E-SCENE-HEIGHT)
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
                                                         #;[rules (zipper-to-begin (viz-state-rules a-vs))]
                                                         #;[yield (zipper-to-begin (viz-state-yield a-vs))]
                                                         #;[broken-invariants (zipper-to-begin (viz-state-broken-invariants a-vs))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                         (viz-state-scale-factor new-viz-state)
                                                                                         NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
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
                                                         #;[rules (zipper-to-begin (viz-state-rules a-vs))]
                                                         #;[yield (zipper-to-begin (viz-state-yield a-vs))]
                                                         #;[broken-invariants (zipper-to-begin (viz-state-broken-invariants a-vs))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                         (viz-state-scale-factor new-viz-state)
                                                                                         NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
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
                                                              #;[rules (zipper-to-begin (viz-state-rules a-vs))]
                                                              #;[yield (zipper-to-begin (viz-state-yield a-vs))]
                                                              #;[broken-invariants (zipper-to-begin (viz-state-broken-invariants a-vs))]))]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                              (viz-state-scale-factor new-viz-state)
                                                                                              NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)))]))
              (let [(new-viz-state (struct-copy viz-state a-vs
                                                [imgs new-imgs]
                                                [curr-image new-pimgs-img]
                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                #;[rules (zipper-to-begin (viz-state-rules a-vs))]
                                                #;[yield (zipper-to-begin (viz-state-yield a-vs))]
                                                #;[broken-invariants (zipper-to-begin (viz-state-broken-invariants a-vs))]))]
                (reposition-out-of-bounds-img new-viz-state
                                              (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state)
                                                                                NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                              new-pimgs-img
                                              (viz-state-scale-factor new-viz-state))))))
    )
  )

;; viz-state -> viz-state
;; Purpose: Finishes the derivations in the visualization
(define (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR NODE-SIZE)
  (lambda (a-vs)
    (if (zipper-at-end? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (zipper-to-end (viz-state-imgs a-vs)))
               (new-pimgs-img ((zipper-current new-imgs)))
               (curr-pimgs-img ((zipper-current (viz-state-imgs a-vs))))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               ]
          (if (create-does-img-need-resizing? new-pimgs-img E-SCENE-WIDTH E-SCENE-HEIGHT)
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
                                                         #;[rules (zipper-to-end (viz-state-rules a-vs))]
                                                         #;[yield (zipper-to-end (viz-state-yield a-vs))]
                                                         #;[broken-invariants (zipper-to-end (viz-state-broken-invariants a-vs))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                         (viz-state-scale-factor new-viz-state)
                                                                                         NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                       new-pimgs-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                       (let [(new-viz-state (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-pimgs-img]
                                                         [scale-factor NEW-FLOOR]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor NEW-FLOOR]
                                                         #;[rules (zipper-to-end (viz-state-rules a-vs))]
                                                         #;[yield (zipper-to-end (viz-state-yield a-vs))]
                                                         #;[broken-invariants (zipper-to-end (viz-state-broken-invariants a-vs))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                         (viz-state-scale-factor new-viz-state)
                                                                                         NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                       new-pimgs-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                              [imgs new-imgs]
                                                              [curr-image new-pimgs-img]
                                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                              [scale-factor-floor NEW-FLOOR]
                                                              #;[rules (zipper-to-end (viz-state-rules a-vs))]
                                                              #;[yield (zipper-to-end (viz-state-yield a-vs))]
                                                              #;[broken-invariants (zipper-to-end (viz-state-broken-invariants a-vs))]))]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                              (viz-state-scale-factor new-viz-state)
                                                                                              NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)))]))
              (let [(new-viz-state (struct-copy viz-state a-vs
                                                [imgs new-imgs]
                                                [curr-image new-pimgs-img]
                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                #;[rules (zipper-to-end (viz-state-rules a-vs))]
                                                #;[yield (zipper-to-end (viz-state-yield a-vs))]
                                                #;[broken-invariants (zipper-to-end (viz-state-broken-invariants a-vs))]))]
                (reposition-out-of-bounds-img new-viz-state
                                              (create-calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                (viz-state-scale-factor new-viz-state)
                                                                                NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
                                              new-pimgs-img
                                              (viz-state-scale-factor new-viz-state))))))
    )
  )

;; viz-state -> viz-state
;; Purpose: Zooms in on the visualization
(define (create-zoom-in ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (lambda (a-vs) ((create-zoom ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT) a-vs ZOOM-INCREASE)))

;; viz-state -> viz-state
;; Purpose: Zooms out the visualization
(define (create-zoom-out ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (lambda (a-vs) ((create-zoom ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT) a-vs ZOOM-DECREASE)))

;; viz-state -> viz-state
;; Purpose: Zooms all the way in on the visualization
(define (create-max-zoom-in ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (lambda (a-vs) ((create-zoom ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT) a-vs (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs)))))

;; viz-state -> viz-state
;; Purpose: Zooms in a moderate amount on the visualization
(define (create-reset-zoom ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM-CAP NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT)
  (lambda (a-vs) ((create-zoom ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT) a-vs (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs))
               )))

;; viz-state -> viz-state
;; Purpose: Zooms all the way out in the visualization
(define (create-max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP ZOOM-INCREASE ZOOM-DECREASE DEFAULT-ZOOM NODE-SIZE)
  (lambda (a-vs) (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
          (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs)))
          )
      (let [(img-resize (resize-image (viz-state-curr-image a-vs) (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))]
        ((create-zoom ZOOM-INCREASE ZOOM-DECREASE) a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs))))
      (struct-copy viz-state a-vs
                   [scale-factor DEFAULT-ZOOM]))
    )
  )