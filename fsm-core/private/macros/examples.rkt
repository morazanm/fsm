#lang racket
(require "constructors.rkt"
         "../constants.rkt"
         "../fsa.rkt")


(define dfa-temp (make-dfa
                  `(A B)
                  '(a)
                  'A
                  '(A)
                  (list '(A a A)
                        '(B a B)
                        )
                  #:accepts (list '(a a a) '(a a))
                  #:rejects '(bbbb)
                  ))
(dfa-temp '(a a a a))