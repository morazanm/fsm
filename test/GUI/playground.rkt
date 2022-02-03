#lang racket
(require "../../main.rkt")




(define a*a (make-dfa '(S F A) '(a b) 'S '(F)
                      '((S a F)
                        (F a F)
                        (F b A)
                        (A a F)
                        (A b A))))

(define S-INV null?)

(define (F-INV consumed-input)
  (and (not (null? consumed-input))
       (eq? (first consumed-input) 'a)
       (eq? (last consumed-input) 'a)))

(define (A-INV consumed-input)
  (and (not (null? consumed-input))
       (eq? (first consumed-input) 'a)
       (eq? (last consumed-input) 'a)))

(define (DS-INV consumed-input)
  (and (not (null? consumed-input))
       (not (eq? (first consumed-input) 'a))))



(sm-visualize-new a*a
              `(S ,S-INV)
              `(F ,F-INV)
              `(A ,A-INV))