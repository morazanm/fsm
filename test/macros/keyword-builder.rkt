#lang racket
(require rackunit "../../Macros/keyword-builder.rkt" "../test-helpers.rkt")

  
(keyword-builder point (x y [z 10]))
(define p1 (make-point 10 10 #:z 20))
(define p2 (make-point 10 10))

(check-equal? p1 (point 10 10 20) "Macro with specialized")
(check-equal? p2 (point 10 10 10) "Macro with default")


(struct posn (x y) #:transparent)
(keyword-builder button (height width location [color 'green] [fnt-size 10] [func 'void]))
(define btn1 (make-button 10 20 (posn 100 10)
                          #:color 'blue
                          #:func 'new))

(define btn2 (make-button 10 20 (posn 100 10)))

(define btn3 (make-button 10 20 (posn 100 10)
                          #:fnt-size 100
                          #:color 'blue
                          #:func 'new))

(check-equal? btn1 (button 10 20 (posn 100 10) 'blue 10 'new) "Macro with some specialized")
(check-equal? btn2 (button 10 20 (posn 100 10) 'green 10  'void) "Macro with none specialized")
(check-equal? btn3 (button 10 20 (posn 100 10) 'blue 100 'new) "Macro with all specialized")