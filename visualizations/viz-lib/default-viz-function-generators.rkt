#lang racket/base
(require "vector-zipper.rkt"
         "zipper.rkt"
         "viz-state.rkt"
         math/matrix
         "bounding-limits.rkt"
         "../2htdp/image.rkt"
         "resize-viz-image.rkt"
         "typed-matrices.rkt"
         racket/promise)

(provide (all-defined-out))

(define (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER FONT-SIZE elems-lst)
  (let ([rev-elems-lst (reverse elems-lst)])
    (foldl (lambda (val accum)
           (beside/align
            "bottom"
            (above/align "middle"
                         (car val)
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text (cadr val) (- FONT-SIZE 2) 'black))
            (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
            accum))
         (above/align "middle"
                (car (car rev-elems-lst))
                (square HEIGHT-BUFFER 'solid 'white)
                (text (cadr (car rev-elems-lst)) (- FONT-SIZE 2) 'black))
         (cdr rev-elems-lst))))

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


(define (reposition-out-of-bounds-img a-vs viewport-lims new-img new-scale)
  (if (outside-north-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
      (if (outside-west-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
          (struct-copy viz-state a-vs
                       [curr-image new-img]
                       [image-posn (posn (bounding-limits-min-x viewport-lims) (bounding-limits-min-y viewport-lims))]
                       [scale-factor new-scale])
          (if (outside-east-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
              (struct-copy viz-state a-vs
                           [curr-image new-img]
                           [image-posn (posn (bounding-limits-max-x viewport-lims) (bounding-limits-min-y viewport-lims))]
                           [scale-factor new-scale])
              (struct-copy viz-state a-vs
                           [curr-image new-img]
                           [image-posn (posn (posn-x (viz-state-image-posn a-vs)) (bounding-limits-min-y viewport-lims))]
                           [scale-factor new-scale])))
      (if (outside-south-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
          (if (outside-west-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
              (struct-copy viz-state a-vs
                           [curr-image new-img]
                           [image-posn (posn (bounding-limits-min-x viewport-lims) (bounding-limits-max-y viewport-lims))]
                           [scale-factor new-scale])
              (if (outside-east-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
                  (struct-copy viz-state a-vs
                               [curr-image new-img]
                               [image-posn (posn (bounding-limits-max-x viewport-lims) (bounding-limits-max-y viewport-lims))]
                               [scale-factor new-scale])
                  (struct-copy viz-state a-vs
                               [curr-image new-img]
                               [image-posn (posn (posn-x (viz-state-image-posn a-vs)) (bounding-limits-max-y viewport-lims))]
                               [scale-factor new-scale])))
          (if (outside-west-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
              (struct-copy viz-state a-vs
                           [curr-image new-img]
                           [image-posn (posn (bounding-limits-min-x viewport-lims) (posn-y (viz-state-image-posn a-vs)))]
                           [scale-factor new-scale])
              (if (outside-east-side-bounding-limits? viewport-lims (viz-state-image-posn a-vs))
                  (struct-copy viz-state a-vs
                               [curr-image new-img]
                               [image-posn (posn (bounding-limits-max-x viewport-lims) (posn-y (viz-state-image-posn a-vs)))]
                               [scale-factor new-scale])
                  (struct-copy viz-state a-vs
                               [curr-image new-img]
                               [scale-factor new-scale]))))))


;; img num>0 num num num -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define (calculate-viewport-limits scaled-image scale E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
  (let [(img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale)))
        (img-height-node-diff (- (/ (image-height scaled-image) 2) (* NODE-SIZE scale)))
        (scaled-node-size (* NODE-SIZE scale))]
    (let [(MIN-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
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
      (bounding-limits MIN-X MAX-X MIN-Y MAX-Y))))

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
;ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
(define (zoom factor E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE)
  (lambda (a-vs)
    (let* [(new-scale (* factor (viz-state-scale-factor a-vs)))
           (scalable? (cond [(eq? factor ZOOM-INCREASE) (> (viz-state-scale-factor-cap a-vs) new-scale)]
                            [(eq? factor ZOOM-DECREASE) (< (viz-state-scale-factor-floor a-vs) new-scale)]))]
      (if scalable?
          (let* [(scaled-image (scale new-scale (viz-state-curr-image a-vs)))
                 (viewport-lims (calculate-viewport-limits scaled-image new-scale E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE))
                 (scale-increase (/ new-scale (viz-state-scale-factor a-vs)))
                 (affine-matrix (zoom-affine-transform (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs))
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
          a-vs))))

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
                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))))

;; img -> boolean
;; Checks to see if an image needs to be resized
(define (does-img-need-resizing? img E-SCENE-WIDTH E-SCENE-HEIGHT)
  (or (< E-SCENE-WIDTH (image-width img))
      (< E-SCENE-HEIGHT (image-height img))))

(define (load-image new-img)
  (if (list? new-img)
      (force (delay/thread (lambda () (apply above (map (lambda (img) ((force img))) (force new-img))))))
      (force (delay/thread ((force new-img))))))

(define (jump-prev-inv E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP imsg-state-invs-zipper-func)
  (lambda ( a-vs)
    (if (zipper-empty? (imsg-state-invs-zipper-func (informative-messages-component-state (viz-state-informative-messages a-vs))))
        a-vs
        (let* ([new-imgs (viz-state-imgs a-vs)]
               [new-curr-img (if (image? (vector-zipper-current new-imgs))
                                 (vector-zipper-current new-imgs)
                                 (if (list? (vector-zipper-current new-imgs))
                                     (apply above (map (lambda (img) ((force img))) (vector-zipper-current new-imgs)))
                                     (let [(cache (force (vector-zipper-current new-imgs)))]
                                       (if (list? cache)
                                           (apply above (map (lambda (img) ((force img))) cache))
                                           (cache)))))]
               [curr-pimgs-img (viz-state-curr-image a-vs)]
               [img-resize (resize-image new-curr-img
                                         (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                         (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
               [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
               [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
        
          (if (does-img-need-resizing? new-curr-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let ([NEW-FLOOR (min (cadr img-resize) (caddr img-resize))])
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
                                       [scale-factor-floor NEW-FLOOR]
                                       [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                       'BEGIN
                                                       (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                       [next-image (if (vector-zipper-at-end? new-imgs)
                                                       'END
                                                       (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                     (reposition-out-of-bounds-img
                      new-viz-state
                      (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                        new-curr-img)
                                                 (viz-state-scale-factor new-viz-state)
                                                 E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                       [scale-factor-floor NEW-FLOOR]
                                       [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                       'BEGIN
                                                       (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                       [next-image (if (vector-zipper-at-end? new-imgs)
                                                       'END
                                                       (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                     (reposition-out-of-bounds-img
                      new-viz-state
                      (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                        new-curr-img)
                                                 (viz-state-scale-factor new-viz-state)
                                                 E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                       [scale-factor-floor NEW-FLOOR]
                                       [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                       'BEGIN
                                                       (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                       [next-image (if (vector-zipper-at-end? new-imgs)
                                                       'END
                                                       (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                     (reposition-out-of-bounds-img
                      new-viz-state
                      (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                        new-curr-img)
                                                 (viz-state-scale-factor new-viz-state)
                                                 E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                                'BEGIN
                                                                (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                                [next-image (if (vector-zipper-at-end? new-imgs)
                                                                'END
                                                                (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                (reposition-out-of-bounds-img
                 new-viz-state
                 (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                            (viz-state-scale-factor a-vs)
                                            E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                 new-curr-img
                 (viz-state-scale-factor a-vs))))))))

(define (jump-next-inv E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP imsg-state-invs-zipper-func)
     (lambda (a-vs)
       (if (zipper-empty? (imsg-state-invs-zipper-func (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
             a-vs
             (let* ([new-imgs (viz-state-imgs a-vs)]
                    [new-curr-img (if (image? (vector-zipper-current new-imgs))
                                      (vector-zipper-current new-imgs)
                                      (if (list? (vector-zipper-current new-imgs))
                                          (apply above (map (lambda (img) ((force img))) (vector-zipper-current new-imgs)))
                                          (let [(cache (force (vector-zipper-current new-imgs)))]
                                            (if (list? cache)
                                                (apply above (map (lambda (img) ((force img))) cache))
                                                (cache)))))]
                    [curr-pimgs-img (viz-state-curr-image a-vs)]
                    [img-resize (resize-image new-curr-img
                                              (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                              (* E-SCENE-HEIGHT PERCENT-BORDER-GAP))]
                    [growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                                 (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))]
                    [growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                                 (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2))])
       
               (if (does-img-need-resizing? new-curr-img E-SCENE-WIDTH E-SCENE-HEIGHT)
                   (let ([NEW-FLOOR (min (cadr img-resize) (caddr img-resize))])
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
                                            [scale-factor-floor NEW-FLOOR]
                                            [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                                     'BEGIN
                                                                     (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                            [next-image (if (vector-zipper-at-end? new-imgs)
                                                            'END
                                                            (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                          (reposition-out-of-bounds-img
                           new-viz-state
                           (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                             new-curr-img)
                                                      (viz-state-scale-factor new-viz-state)
                                                      E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                            [scale-factor-floor NEW-FLOOR]
                                            [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                                     'BEGIN
                                                                     (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                            [next-image (if (vector-zipper-at-end? new-imgs)
                                                            'END
                                                            (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                          (reposition-out-of-bounds-img
                           new-viz-state
                           (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                             new-curr-img)
                                                      (viz-state-scale-factor new-viz-state)
                                                      E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                            [scale-factor-floor NEW-FLOOR]
                                            [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                                     'BEGIN
                                                                     (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                            [next-image (if (vector-zipper-at-end? new-imgs)
                                                            'END
                                                            (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                          (reposition-out-of-bounds-img
                           new-viz-state
                           (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state)
                                                             new-curr-img)
                                                      (viz-state-scale-factor new-viz-state)
                                                      E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                                     [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                     [prev-image (if (vector-zipper-at-begin? new-imgs)
                                                                     'BEGIN
                                                                     (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                                     [next-image (if (vector-zipper-at-end? new-imgs)
                                                                     'END
                                                                     (load-image (vector-zipper-current (vector-zipper-next new-imgs))))])])
                     (reposition-out-of-bounds-img
                      new-viz-state
                      (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                                 (viz-state-scale-factor a-vs)
                                                 E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                      new-curr-img
                      (viz-state-scale-factor a-vs))))))))

;; viz-state -> viz-state
;; Purpose: Moves the visualization to the next step of the derivation
(define (go-next E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP)
  (lambda (a-vs)
    (if (vector-zipper-at-end? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (vector-zipper-next (viz-state-imgs a-vs)))
               (new-curr-img (if (image? (viz-state-next-image a-vs))
                                 (viz-state-next-image a-vs)
                                 (if (list? (viz-state-next-image a-vs))
                                     (apply above (map (lambda (img) ((force img))) (viz-state-next-image a-vs)))
                                     (let [(cache (force (viz-state-next-image a-vs)))]
                                       (if (list? cache)
                                           (apply above (map (lambda (img) ((force img))) cache))
                                           (cache))))))
               (curr-pimgs-img (viz-state-curr-image a-vs))
               (img-resize (resize-image new-curr-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-curr-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (does-img-need-resizing? new-curr-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let [(NEW-FLOOR (min (cadr img-resize) (caddr img-resize)))]
                (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                       (let [(new-viz-state (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-curr-img]
                                                         [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                           (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                         [scale-factor DEFAULT-ZOOM-CAP]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor NEW-FLOOR]
                                                         [prev-image (viz-state-curr-image a-vs)]
                                                         [next-image (if (vector-zipper-at-end? new-imgs)
                                                                         'END
                                                                         (load-image (vector-zipper-current (vector-zipper-next new-imgs))))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                  (viz-state-scale-factor new-viz-state)
                                                                                  E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                                         [prev-image (viz-state-curr-image a-vs)]
                                                         [next-image (if (vector-zipper-at-end? new-imgs)
                                                                         'END
                                                                         (load-image (vector-zipper-current (vector-zipper-next new-imgs))))]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                  (viz-state-scale-factor new-viz-state)
                                                                                  E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                       new-curr-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                              [imgs new-imgs]
                                                              [curr-image new-curr-img]
                                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                              [scale-factor-floor NEW-FLOOR]
                                                              [prev-image (viz-state-curr-image a-vs)]
                                                              [next-image (if (vector-zipper-at-end? new-imgs)
                                                                              'END
                                                                              (load-image (vector-zipper-current (vector-zipper-next new-imgs))))]))]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-curr-img)
                                                                                       (viz-state-scale-factor new-viz-state)
                                                                                       E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                            new-curr-img
                                                            (viz-state-scale-factor new-viz-state)))]))
              (let [(new-viz-state (struct-copy viz-state a-vs
                                                [imgs new-imgs]
                                                [curr-image new-curr-img]
                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                [prev-image (viz-state-curr-image a-vs)]
                                                [next-image (if (vector-zipper-at-end? new-imgs)
                                                                'END
                                                                (load-image (vector-zipper-current (vector-zipper-next new-imgs))))]))]
                (reposition-out-of-bounds-img new-viz-state
                                              (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-curr-img)
                                                                         (viz-state-scale-factor a-vs)
                                                                         E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                              new-curr-img
                                              (viz-state-scale-factor a-vs))))))))

;; viz-state -> viz-state
;; Purpose: Moves the visualization one step back in the derivation
(define (go-prev E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP)
  (lambda (a-vs)
    (if (vector-zipper-at-begin? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (vector-zipper-prev (viz-state-imgs a-vs)))
               (new-pimgs-img (if (image? (viz-state-prev-image a-vs))
                                  (viz-state-prev-image a-vs)
                                  (if (list? (viz-state-prev-image a-vs))
                                      (apply above (map (lambda (img) ((force img))) (viz-state-prev-image a-vs)))
                                      (let [(cache (force (viz-state-prev-image a-vs)))]
                                        (if (list? cache)
                                            (apply above (map (lambda (img) ((force img))) cache))
                                            (cache))))))
               (curr-pimgs-img (viz-state-curr-image a-vs))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (does-img-need-resizing? new-pimgs-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let [(NEW-FLOOR (min (cadr img-resize) (caddr img-resize)))]
                (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                       (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                  [imgs new-imgs]
                                                                  [curr-image new-pimgs-img]
                                                                  [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                    (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                                  [scale-factor DEFAULT-ZOOM-CAP]
                                                                  [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                  [scale-factor-floor NEW-FLOOR]
                                                                  [prev-image
                                                                   (if (vector-zipper-at-begin? new-imgs)
                                                                       'BEGIN
                                                                       (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                                                  [next-image (viz-state-curr-image a-vs)])
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                (viz-state-scale-factor a-vs)
                                                                                E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                     new-pimgs-img
                                                     (viz-state-scale-factor a-vs))]
                      [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                       (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                  [imgs new-imgs]
                                                                  [curr-image new-pimgs-img]
                                                                  [scale-factor NEW-FLOOR]
                                                                  [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                  [scale-factor-floor NEW-FLOOR]
                                                                  [prev-image
                                                                   (if (vector-zipper-at-begin? new-imgs)
                                                                       'BEGIN
                                                                       (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                                                  [next-image (viz-state-curr-image a-vs)])
                                                     (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                (viz-state-scale-factor a-vs)
                                                                                E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                     new-pimgs-img
                                                     (viz-state-scale-factor a-vs))]
                      [else (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                                       [imgs new-imgs]
                                                                       [curr-image new-pimgs-img]
                                                                       [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                         (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                                       [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                                       [scale-factor-floor NEW-FLOOR]
                                                                       [prev-image
                                                                        (if (vector-zipper-at-begin? new-imgs)
                                                                            'BEGIN
                                                                            (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                                                       [next-image (viz-state-curr-image a-vs)])
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                                     (viz-state-scale-factor a-vs)
                                                                                     E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                          new-pimgs-img
                                                          (viz-state-scale-factor a-vs))]))
              (reposition-out-of-bounds-img (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-pimgs-img]
                                                         [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                           (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                         [prev-image
                                                          (if (vector-zipper-at-begin? new-imgs)
                                                              'BEGIN
                                                              (load-image (vector-zipper-current (vector-zipper-prev new-imgs))))]
                                                         [next-image (viz-state-curr-image a-vs)])
                                            (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img)
                                                                       (viz-state-scale-factor a-vs)
                                                                       E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                            new-pimgs-img
                                            (viz-state-scale-factor a-vs)))))))

;; viz-state -> viz-state
;; Purpose: Restarts the derivation in the visualization
(define (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP)
  (lambda (a-vs)
    (if (vector-zipper-at-begin? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (vector-zipper-to-begin (viz-state-imgs a-vs)))
               (new-pimgs-img (if (list? (viz-state-begin-image a-vs))
                                  (apply above (map (lambda (img) ((force img))) (viz-state-begin-image a-vs)))
                                  (viz-state-begin-image a-vs)))
               (curr-pimgs-img (viz-state-curr-image a-vs))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (does-img-need-resizing? new-pimgs-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let [(NEW-FLOOR (min (cadr img-resize) (caddr img-resize)))]
                (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                       (let [(new-viz-state (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-pimgs-img]
                                                         [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                           (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                         [scale-factor DEFAULT-ZOOM-CAP]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor NEW-FLOOR]
                                                         [next-image (load-image (vector-zipper-current (vector-zipper-next new-imgs)))]
                                                         [prev-image 'BEGIN]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                  (viz-state-scale-factor new-viz-state)
                                                                                  E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
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
                                                         [next-image (load-image (vector-zipper-current (vector-zipper-next new-imgs)))]
                                                         [prev-image 'BEGIN]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                  (viz-state-scale-factor new-viz-state)
                                                                                  E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                       new-pimgs-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                              [imgs new-imgs]
                                                              [curr-image new-pimgs-img]
                                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                              [scale-factor-floor NEW-FLOOR]
                                                              [next-image (load-image (vector-zipper-current (vector-zipper-next new-imgs)))]
                                                              [prev-image 'BEGIN]))]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                       (viz-state-scale-factor new-viz-state)
                                                                                       E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)))]))
              (let [(new-viz-state (struct-copy viz-state a-vs
                                                [imgs new-imgs]
                                                [curr-image new-pimgs-img]
                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                [next-image (load-image (vector-zipper-current (vector-zipper-next new-imgs)))]
                                                [prev-image 'BEGIN]))]
                (reposition-out-of-bounds-img new-viz-state
                                              (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                         (viz-state-scale-factor new-viz-state)
                                                                         E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                              new-pimgs-img
                                              (viz-state-scale-factor new-viz-state))))))))

;; viz-state -> viz-state
;; Purpose: Finishes the derivations in the visualization
(define (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP)
  (lambda (a-vs)
    (if (vector-zipper-at-end? (viz-state-imgs a-vs))
        a-vs
        (let* [(new-imgs (vector-zipper-to-end (viz-state-imgs a-vs)))
               (new-pimgs-img (if (list? (viz-state-end-image a-vs))
                                  (apply above (map (lambda (img) ((force img))) (viz-state-end-image a-vs)))
                                  (viz-state-end-image a-vs)))
               (curr-pimgs-img (viz-state-curr-image a-vs))
               (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
               (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
               (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                            (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))]
          (if (does-img-need-resizing? new-pimgs-img E-SCENE-WIDTH E-SCENE-HEIGHT)
              (let [(NEW-FLOOR (min (cadr img-resize) (caddr img-resize)))]
                (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                       (let [(new-viz-state (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-pimgs-img]
                                                         [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                           (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                         [scale-factor DEFAULT-ZOOM-CAP]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor NEW-FLOOR]
                                                         [prev-image (load-image (vector-zipper-current (vector-zipper-prev new-imgs)))]
                                                         [next-image 'END]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                  (viz-state-scale-factor new-viz-state)
                                                                                  E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                       new-pimgs-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                       (let [(new-viz-state (struct-copy viz-state a-vs
                                                         [imgs new-imgs]
                                                         [curr-image new-pimgs-img]
                                                         [scale-factor NEW-FLOOR]
                                                         [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                         [scale-factor-floor NEW-FLOOR]
                                                         [prev-image (load-image (vector-zipper-current (vector-zipper-prev new-imgs)))]
                                                         [next-image 'END]))]
                         (reposition-out-of-bounds-img new-viz-state
                                                       (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                  (viz-state-scale-factor new-viz-state)
                                                                                  E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                       new-pimgs-img
                                                       (viz-state-scale-factor new-viz-state)))]
                      [else (let [(new-viz-state (struct-copy viz-state a-vs
                                                              [imgs new-imgs]
                                                              [curr-image new-pimgs-img]
                                                              [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                                (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                              [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                              [scale-factor-floor NEW-FLOOR]
                                                              [prev-image (load-image (vector-zipper-current (vector-zipper-prev new-imgs)))]
                                                              [next-image 'END]))]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                                       (viz-state-scale-factor new-viz-state)
                                                                                       E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)))]))
              (let [(new-viz-state (struct-copy viz-state a-vs
                                                [imgs new-imgs]
                                                [curr-image new-pimgs-img]
                                                [image-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x)
                                                                  (+ (posn-y (viz-state-image-posn a-vs)) growth-y))]
                                                [scale-factor-cap DEFAULT-ZOOM-CAP]
                                                [scale-factor-floor DEFAULT-ZOOM-FLOOR]
                                                [prev-image (load-image (vector-zipper-current (vector-zipper-prev new-imgs)))]
                                                [next-image 'END]))]
                (reposition-out-of-bounds-img new-viz-state
                                              (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img)
                                                                         (viz-state-scale-factor new-viz-state)
                                                                         E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE)
                                              new-pimgs-img
                                              (viz-state-scale-factor new-viz-state))))))))

;; viz-state -> viz-state
;; Purpose: Zooms in on the visualization
(define (zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM)
  (zoom ZOOM-INCREASE E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE))

;; viz-state -> viz-state
;; Purpose: Zooms out the visualization
(define (zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM)
  (zoom ZOOM-DECREASE E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE))

;; viz-state -> viz-state
;; Purpose: Zooms all the way in on the visualization
(define (max-zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM) 
  (lambda (a-vs) ((zoom (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs))
                        E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE) a-vs)))

;; viz-state -> viz-state
;; Purpose: Zooms in a moderate amount on the visualization
(define (reset-zoom E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM)
  (lambda (a-vs) ((zoom (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs))
                        E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE) a-vs)))

;; viz-state -> viz-state
;; Purpose: Zooms all the way out in the visualization
(define (max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM)
  (lambda (a-vs)
    (if (or (< E-SCENE-WIDTH (image-width (viz-state-curr-image a-vs)))
            (< E-SCENE-HEIGHT (image-height (viz-state-curr-image a-vs))))
        (let [(img-resize (resize-image (viz-state-curr-image a-vs) (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))]
          ((zoom (/ (min (cadr img-resize) (caddr img-resize)) (viz-state-scale-factor a-vs))
                 E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE) a-vs))
        (struct-copy viz-state a-vs
                     [scale-factor DEFAULT-ZOOM]))))