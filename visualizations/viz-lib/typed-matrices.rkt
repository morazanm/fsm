#lang typed/racket/base
(require math/matrix
         math/flonum
         (only-in typed/racket/gui
                  Bitmap%)
         )
(require/typed "bounding-limits.rkt"
               [#:struct posn
                ([x : Real]
                 [y : Real])]
               )
(require/typed 2htdp/image
               [#:opaque htdp:image image?]
               [image-width (Image -> Nonnegative-Integer)]
               [image-height (Image -> Nonnegative-Integer)])

(provide zoom-affine-transform)

(define-type Image (U htdp:image (Instance Bitmap%)))

(: affine-transform (->* ((Matrix Float))
                         (#:x-translate Flonum
                          #:y-translate Float
                          #:x-scale Float
                          #:y-scale Float
                          #:reflect Boolean
                          #:rotate Float
                          #:x-shear Float
                          #:y-shear Float)
                         (Matrix Float)))
(define (affine-transform #:x-translate [x-translate 0.0]
                          #:y-translate [y-translate 0.0]
                          #:x-scale [x-scale 1.0]
                          #:y-scale [y-scale 1.0]
                          #:reflect [reflect #f]
                          #:rotate [rotate 0.0]
                          #:x-shear [x-shear 0.0]
                          #:y-shear [y-shear 0.0]
                          point)
  (let [(reflection (if reflect
                        -1.0
                        1.0))
        ]
    (matrix* (matrix [[(fl* reflection x-scale (flcos rotate)) (fl* x-shear (fl* -1.0 (flsin rotate))) x-translate ]
                      [(fl* (flsin rotate) y-shear) (fl* y-scale (flcos rotate)) y-translate]
                      [0.0 0.0 1.0]])
             point)))

(: zoom-affine-transform (-> Image posn Real Real Real (Matrix Float)))
(define (zoom-affine-transform img img-posn scale E-SCENE-WIDTH E-SCENE-HEIGHT)
  (let* [(fl-image-height (fl (image-height img)))
         (fl-image-width (fl (image-width img)))
         (transformed-x (fl* -1.0 (fl+ (fl- (fl/ (fl E-SCENE-WIDTH) 2.0)
                                            (fl+ (real->double-flonum (posn-x img-posn)) (fl/ fl-image-width 2.0)))
                                       (fl/ fl-image-width 2.0))))
         (transformed-y (fl* -1.0 (fl+ (fl- (fl/ (fl E-SCENE-HEIGHT) 2.0)
                                            (fl+ (real->double-flonum (posn-y img-posn)) (fl/ fl-image-height 2.0)))
                                       (fl/ fl-image-height 2.0))))]
    (affine-transform #:x-translate (fl* -1.0 transformed-x)
                      #:y-translate (fl* -1.0 transformed-y)
                      (affine-transform #:x-scale (real->double-flonum scale)
                                                #:y-scale (real->double-flonum scale)
                                                (affine-transform #:x-translate transformed-x
                                                                          #:y-translate transformed-y
                                                                          (matrix [ [0.0] [0.0] [1.0] ]))))))