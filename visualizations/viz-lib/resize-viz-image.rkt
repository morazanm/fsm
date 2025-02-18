#lang racket/base
(require "../2htdp/image.rkt")
(provide resize-image resize-image-generator)

(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define aspect (/ src-width src-height))
  (define scale (min
                 (/ max-width src-width)
                 (/ max-height src-height)))

  (define scaled-width (* src-width scale))
  (define scaled-height (* src-height scale))

  (let* ([src-width (image-width img)]
         [src-height (image-height img)]
         [aspect (/ src-width src-height)]
         [scale (min (/ max-width src-width)
                     (/ max-height src-height))])
    (if (> scaled-width max-width)
        (if (> scaled-height max-height)
            (let* ([new-scaled-height (/ max-width aspect)]
                   [scaled-aspect (/ max-width new-scaled-height)])
              (list (scale/xy
                     (/ (* max-height scaled-aspect) src-width)
                     (/ max-height src-height)
                     img)
                    (/ (* max-height scaled-aspect) src-width)
                    (/ max-height src-height)))
            (list (scale/xy
                   (/ max-width src-width)
                   (/ (/ scaled-width aspect) src-height)
                   img)
                  (/ max-width src-width)
                  (/ (/ scaled-width aspect) src-height))
            )
        (if (> scaled-height max-height)
            (let ([scaled-aspect (/ scaled-width scaled-height)])
              (list (scale/xy
                     (/ (* scaled-height scaled-aspect) src-width)
                     (/ max-height src-height)
                     img)
                    (/ (* scaled-height scaled-aspect) src-width)
                    (/ max-height src-height)))
            (list (scale/xy
                   (/ scaled-width src-width)
                   (/ scaled-height src-height)
                   img)
                  (/ scaled-width src-width)
                  (/ scaled-height src-height))))))

(define ((resize-image-generator  max-width max-height) img)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define aspect (/ src-width src-height))
  (define scale (min
                 (/ max-width src-width)
                 (/ max-height src-height)))

  (define scaled-width (* src-width scale))
  (define scaled-height (* src-height scale))

  (let* ([src-width (image-width img)]
         [src-height (image-height img)]
         [aspect (/ src-width src-height)]
         [scale (min (/ max-width src-width)
                     (/ max-height src-height))])
    (if (> scaled-width max-width)
        (if (> scaled-height max-height)
            (let* ([new-scaled-height (/ max-width aspect)]
                   [scaled-aspect (/ max-width new-scaled-height)])
              (list (scale/xy
                     (/ (* max-height scaled-aspect) src-width)
                     (/ max-height src-height)
                     img)
                    (/ (* max-height scaled-aspect) src-width)
                    (/ max-height src-height)))
            (list (scale/xy
                   (/ max-width src-width)
                   (/ (/ scaled-width aspect) src-height)
                   img)
                  (/ max-width src-width)
                  (/ (/ scaled-width aspect) src-height))
            )
        (if (> scaled-height max-height)
            (let ([scaled-aspect (/ scaled-width scaled-height)])
              (list (scale/xy
                     (/ (* scaled-height scaled-aspect) src-width)
                     (/ max-height src-height)
                     img)
                    (/ (* scaled-height scaled-aspect) src-width)
                    (/ max-height src-height)))
            (list (scale/xy
                   (/ scaled-width src-width)
                   (/ scaled-height src-height)
                   img)
                  (/ scaled-width src-width)
                  (/ scaled-height src-height))))))