#lang racket/base
(require "../2htdp/image.rkt"
         "call-with-values-chained.rkt"
         )
(provide resize-image
         find-new-floor)

(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define aspect (/ src-width src-height))
  (define scale (min
                 (/ max-width src-width)
                 (/ max-height src-height)))
  (define (resize-img-width scaled-width scaled-height)
    (if (> scaled-width max-width)
        (values max-width (/ max-width aspect))
        (values scaled-width scaled-height)))
  (define (resize-img-height scaled-width scaled-height)
    (if (> scaled-height max-height)
        (values (* max-height (/ scaled-width scaled-height)) max-height)
        (values scaled-width scaled-height)))
  (define (scale-img scaled-width scaled-height)
    (scale/xy
     (/ scaled-width src-width)
     (/ scaled-height src-height)
     img))
  (call-with-values-chained scale-img resize-img-height (resize-img-width (* src-width scale) (* src-height scale))))

(define (find-new-floor img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define aspect (/ src-width src-height))
  (define scale (min
                 (/ max-width src-width)
                 (/ max-height src-height)))
  (define (resize-img-width scaled-width scaled-height)
    (if (> scaled-width max-width)
        (values max-width (/ max-width aspect))
        (values scaled-width scaled-height)))
  (define (resize-img-height scaled-width scaled-height)
    (if (> scaled-height max-height)
        (values (* max-height (/ scaled-width scaled-height)) max-height)
        (values scaled-width scaled-height)))
  (define (normalize-values scaled-width scaled-height)
    (values (/ scaled-width src-width) (/ scaled-height src-height)))
  (call-with-values-chained min normalize-values resize-img-height (resize-img-width (* src-width scale) (* src-height scale))))