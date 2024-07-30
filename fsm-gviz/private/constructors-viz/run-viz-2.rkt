#lang fsm
(require 2htdp/universe 2htdp/image rackunit "definitions-viz-2.rkt")
(provide run-viz resize-image)


;; vst --> void
(define (run-viz a-vs draw-etc a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-etc]
      [on-key process-key]
      [name a-name]))
  (void))


;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (viz-state (rest (viz-state-upimgs a-vs))
                        (cons (first (viz-state-upimgs a-vs))
                              (viz-state-pimgs a-vs))))]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (viz-state (cons (first (viz-state-pimgs a-vs))
                              (viz-state-upimgs a-vs))
                        (rest (viz-state-pimgs a-vs))
                        ))]
        [(key=? "down" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (viz-state '()
                        (append (reverse (viz-state-upimgs a-vs))
                                (viz-state-pimgs a-vs))))]
        [(key=? "up" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                      (viz-state-upimgs a-vs)))
                        (list (first (append (reverse (viz-state-pimgs a-vs))
                                             (viz-state-upimgs a-vs))))))]
        [else a-vs]))



;; resize-image :: image -> int -> int -> image
;; Scales a image to the given dimentions. This solution was adapted from
;; one of the answers found here: https://stackoverflow.com/questions/3008772/how-to-smart-resize-a-displayed-image-to-original-aspect-ratio
(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define resize-width src-width)
  (define resize-height src-height)
  (define aspect (/ resize-width resize-height))
  (define scale (min
                 (/ max-width src-width) ; scale-x
                 (/ max-height src-height))) ;scale-y

  (set! resize-width (* resize-width scale))
  (set! resize-height (* resize-height scale))
  
  (when (> resize-width max-width)
    (set! resize-width max-width)
    (set! resize-height (/ resize-width aspect)))

  (when (> resize-height max-height)
    (set! aspect (/ resize-width resize-height))
    (set! resize-height max-height)
    (set! resize-width (* resize-height aspect)))

  (scale/xy
   (/ resize-width src-width)
   (/ resize-height src-height)
   img))