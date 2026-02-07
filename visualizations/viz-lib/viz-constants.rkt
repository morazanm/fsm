#lang racket/base
(require (only-in racket/gui
                  get-display-size
                  get-display-count)
         "os-dependent-constants.rkt")

(provide (all-defined-out)
         WINDOW-HEIGHT) 

(define E-SCENE-WIDTH (* 0.95 WINDOW-WIDTH))
(define FONT-SIZE 20)
(define TAPE-SIZE 42)
(define TM-TAPE-SIZE 24)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define PERCENT-BORDER-GAP 0.9)
(define HEIGHT-BUFFER 10)
(define LETTER-KEY-WIDTH-BUFFER 20)
(define ARROW-KEY-WIDTH-BUFFER 40)
(define INS-TOOLS-BUFFER 1)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define NODE-SIZE 50)

(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))

(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))
(define VIZ-FRAME-HEIGHT 700)