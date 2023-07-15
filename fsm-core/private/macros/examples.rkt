#lang racket
(require "constructors.rkt"
         "../constants.rkt"
         "../fsa.rkt"
         "../../../main.rkt")
(local-require test-engine/racket-tests)


(define dfa-temp (make-dfa2
                  '(A B)
                  '(a b)
                  'A
                  '(A)
                  (list '(A a A)
                        '(B a A)
                        '(B b B)
                        '(A b B)
                        '(A a B))
                  #:accepts (list '(a a a) '(a a))
                  #:rejects (list '(b b b b) '(b b))
                  ))
(dfa-temp '(a a a a b))

(check-error
 (make-dfa2
  `(A B)
  '(a b)
  'A
  '(A)
  (list '(A a A)
        '(B a A)
        '(B b B)
        '(A b B))
  #f
  #:accepts (list '(a a a b) '(a a))
  #:rejects (list '(b b b b) '(b b))
  )
 "Does not accept the predicted value:  ((a a a b))")