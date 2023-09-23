#lang fsm

(require "constructors.rkt")

;; L = a^nb^2n
(define P (make-ndpda2 '(S F)
                       '(a b)
                       '(a)
                       'S
                       '(F W)
                       `(((S a ,EMP) (S (a a)))
                         ((s ,EMP ,EMP) (F ,EMP))
                         ((f b (A)) (F ,EMP)))))

(check-equal? (sm-apply P '(b a)) 'reject)
(check-equal? (sm-apply P '(a a a)) 'reject)
(check-equal? (sm-apply P '(a b b a a b b b b)) 'reject)
(check-equal? (sm-apply P '()) 'accept)
(check-equal? (sm-apply P '(a a b b b b)) 'accept)
