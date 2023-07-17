#lang racket
(require "constructors.rkt"
         "../../constants.rkt"
         "../../../../main.rkt")
(local-require test-engine/racket-tests)

(define wcw^r (make-ndpda2 '(S P Q F)
                           '(a b c)
                           '(a b)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (P ,EMP))
                             ((P a ,EMP) (P (a)))
                             ((P b ,EMP) (P (b)))
                             ((P c ,EMP) (Q ,EMP))
                             ((Q a (a)) (Q ,EMP))
                             ((Q b (b)) (Q ,EMP))
                             ((Q ,EMP ,EMP) (F ,EMP)))
                           #:accepts (list '(a a a b b b))
                           #:rejects (list '(a a a b b))
                           )
  )

(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))


