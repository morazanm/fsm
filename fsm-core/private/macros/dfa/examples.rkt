#lang racket
(require "constructors.rkt"
         "../../constants.rkt"
         "../../../../main.rkt")
(local-require test-engine/racket-tests)

(define dfa-temp (make-dfa2
                  '(A B)
                  '(a b)
                  'A
                  '(B)
                  (list '(A b A)
                        '(A a B)
                        '(B b A))
                  #:accepts (list '(b b b b a))
                  #:rejects (list '(a a a a b))))

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

(define ndfa-temp (make-ndfa2
                  '(A B)
                  '(a b)
                  'A
                  '(B)
                  `((A b A)
                    (A a B)
                    (B ,EMP A))
                  #:accepts (list '(b b b b a))
                  #:rejects (list '(a a a a b))))

