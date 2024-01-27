#lang racket
(require "cg-defs.rkt" "callgraphs-pda.rkt"
         "../../interface.rkt" rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = {a^i b^j | i ≤ j ≤ 2i}
;; Σ = {a b}
;; States:
;;  S: number bs in stack = 2* number as in ci, ci = a*, stack = b*, start state
;;  X: number as in ci <= (number bs in stack + number bs in ci) <= 2* number as in ci, ci = a*b*, stack = b*, final state
;; Stack:
;;  Stack is a (listof b), max of bs that can be read
(define P (make-ndpda '(S X A B)
                      '(a b)
                      '(b)
                      'S
                      '(X)
                      '(((S ε ε) (X ε))
                        ((S a ε) (S (b b)))
                        ((X b (b)) (X ε))
                        ((X b (b b)) (X ε))
                        ((S ε ε) (A ε))
                        ((A ε ε) (B ε))
                        ((B ε ε) (A ε))
                        ((A a ε) (A ε))
                        ((A b ε) (A ε)))))

;; Tests for P
(check-equal? (sm-apply P '(b a a)) 'reject)
(check-equal? (sm-apply P '(b b b b)) 'reject)
(check-equal? (sm-apply P '(a a a b b)) 'reject)
(check-equal? (sm-apply P '()) 'accept)
(check-equal? (sm-apply P '(a a b b b)) 'accept)
(check-equal? (sm-apply P '(a a a b b b b b b)) 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         