#lang racket
(require "constructors.rkt"
         "../../constants.rkt"
         "../../tm.rkt"
         "../../../../main.rkt")
(local-require test-engine/racket-tests)

(define tm-temp (make-tm2
                  '(S Y N)
                    `(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                  #:accepts (list '(a a a) '(a a))
                  #:rejects (list '(b b b b) '(b b))
                  ))

(sm-apply tm-temp`(,LM a a a b a a))

