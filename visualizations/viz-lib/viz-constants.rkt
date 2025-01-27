#lang racket/base
(require "bounding-limits.rkt"
         (only-in racket/gui
                  get-display-size
                  get-display-count))
(provide (all-defined-out))

(define-values (WINDOW-WIDTH WINDOW-HEIGHT) (if (>= (get-display-count) 1)
                                                (with-handlers ([exn:fail? (lambda (e) (values 1200 500))]) (get-display-size))
                                                (values 1200 500)
                                                )
                                                )
(define E-SCENE-WIDTH (* 0.95 WINDOW-WIDTH))
;(define E-SCENE-HEIGHT (* 0.7 WINDOW-HEIGHT))
;(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))
(define FONT-SIZE 20)
(define TAPE-SIZE 42)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define PERCENT-BORDER-GAP 0.9)
(define HEIGHT-BUFFER 20)
(define LETTER-KEY-WIDTH-BUFFER 20)
(define ARROW-KEY-WIDTH-BUFFER 40)
(define INS-TOOLS-BUFFER 1)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define NODE-SIZE 50)
;(define MIDDLE-E-SCENE (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))

(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))

(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))
(define VIZ-FRAME-HEIGHT 700)