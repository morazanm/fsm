#lang racket
(require "constructors.rkt"
         "../constants.rkt"
         "../fsa.rkt"
         "../tm.rkt"
         "../../../main.rkt")

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

(define tm-temp (make-tm2
                  '(S Y N)
                    `(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                  ;#:accepts (list '(a a a) '(a a))
                  ;#:rejects (list '(b b b b) '(b b))
                  ))
(dfa-temp '(a a a a))
(sm-apply tm-temp`(,LM a a a b a a))

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
