#lang racket
(require "constructors.rkt"
         "../constants.rkt"
         "../fsa.rkt"
         "../../../main.rkt")


(define dfa-temp (make-dfa2
                  `(A B)
                  '(a b)
                  'A
                  '(A)
                  (list '(A a A)
                        '(B a A)
                        '(A b B)
                        '(B b B)
                        )
                  #:accepts (list '(a a a) '(a a))
                  #:rejects (list '(b b b b) '(b b))
                  ))
(dfa-temp '(a a a a b a))