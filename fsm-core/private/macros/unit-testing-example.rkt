#lang fsm
(require "constructors.rkt"
         "../constants.rkt")


(define endsWithA (make-dfa2
                  '(A B)
                  '(a b)
                  'A
                  '(B)
                  (list '(A b A)
                        '(A a B)
                        '(B a B)
                        '(B b A))
                  #:accepts (list '(b b b b a) '(b a a)) 
                  #:rejects (list '(a a a a b) '(a a b))))