#lang racket/base

(require (only-in racket/gui
                  get-display-size
                  get-display-count
                  get-display-backing-scale))

(provide set-new-window-size
         SCREEN-SCALING
         WINDOW-WIDTH
         WINDOW-HEIGHT)

(define SCREEN-SCALING (get-display-backing-scale))

(define (get-window-size)
  (if (>= (get-display-count) 1)
      (let-values ([(pre-window-width pre-window-height)
                    (with-handlers ([exn:fail? (lambda (e) (values 1200 500))]) (get-display-size))])
        (values (min 2000 pre-window-width) (min 2000 pre-window-height)))
      (values 1200 500)))

(define-values (WINDOW-WIDTH WINDOW-HEIGHT)
  (get-window-size))

(define (set-new-window-size)
  (set!-values (WINDOW-WIDTH WINDOW-HEIGHT) (get-window-size))
  (set! SCREEN-SCALING (get-display-backing-scale)))