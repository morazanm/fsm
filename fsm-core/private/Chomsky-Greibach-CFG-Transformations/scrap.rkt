#lang fsm

;; L = {a^n b^n | n >= 0}
;; Σ = {a b}
;; States
;; S: ci = a* = stack, start state
;; M: ci = (append (listof a) (listof b))
;;         ∧ stack = a*
;;         ∧ |ci as| = |stack| + |ci bs|
;; F: ci = (append (listof a) (listof b))
;;         ∧ |stack| = 0
;;         ∧ |ci as| = |ci bs|, final state
;; The stack is a (listof a)
(define anbn (make-ndpda '(S M F)
                         '(a b)
                         '(a)
                         'S
                         '(F)
                         '(((S ε ε) (M ε))
                           ((S a ε) (S (a)))
                           ((M b (a)) (M ε))
                           ((M ε ε) (F ε)))))

(sm-graph anbn)

;; L = {w | w has an equal number of as and bs}
;; Σ = {a b}
;; States:
;;  S: ci = number as in ci = nunber bs in ci + number bs in stack, start state, final state
;; Stack:
;;  The stack is a (listof a) or (listof b)
(define as=bs (make-ndpda '(S)
                          '(a b)
                          '(a b)
                          'S
                          '(S)
                          '(((S a ε) (S (b)))
                            ((S b ε) (S (a)))
                            ((S a (a)) (S ε))
                            ((S b (b)) (S ε)))))

(sm-graph as=bs)

;; L = {a^i b^j | i ≤ j ≤ 2i}
;; Σ = {a b}
;; States:
;;  S: number bs in stack = 2* number as in ci, ci = a*, stack = b*, start state
;;  X: number as in ci <= (number bs in stack + number bs in ci) <= 2* number as in ci, ci = a*b*, stack = b*, final state
;; Stack:
;;  Stack is a (listof b), max of bs that can be read
(define aibj (make-ndpda '(S X)
                         '(a b)
                         '(b)
                         'S
                         '(X)
                         '(((S ε ε) (X ε))
                           ((S a ε) (S (b b)))
                           ((X b (b)) (X ε))
                           ((X b (b b)) (X ε)))))

(sm-graph aibj)

;; L = {a^n b^m a^n | n, m ≥ 0}
;; Σ = {a b}
;; States:
;;  S: ci = a^n and stack = a^n, start state
;;  X: ci = a^nb^m and stack = a^n
;;  Y: ci = a^nb^ma^(n - number of as in stack) and stack = number of as between 0 and n, final state
;;  Z: ci = a^n and stack = n number of as - even number of as between 0 and n, final state
;; Condition strong enough?
;;  If stack = ε -> number of as in stack = 0 -> ci = a^nb^ma^(n - 0) -> ci = a^nb^na^n
;;  If stack = ε -> stack = n number of as - even number of as between 0 and n -> n = even number of as between 0 and n -> a^n is accepted as input
(define a^nb^ma^n (make-ndpda '(S X Y Z)
                              '(a b)
                              '(a)
                              'S
                              '(Y Z)
                              '(((S a ε) (S (a)))
                                ((S b ε) (X ε))
                                ((S ε ε) (Z ε))
                                ((X b ε) (X ε))
                                ((X ε ε) (Y ε))
                                ((Y a (a)) (Y ε))
                                ((Z ε (a a)) (Z ε)))))

(sm-graph a^nb^ma^n)

;; L = {a^m b^n| m, n ≥ 0 ∧ m ̸= n}
;; Σ = {a b}
;; States:
;;  S: ci = a^m, stack = b^m, start state
;;  X: ci = a^mb^n, number of as in ci < (number of bs in ci + number of bs in stack), final state
;;  Y: ci = a^mb^n, number of as in ci > (number of bs in ci + number of bs in stack), final state
;; Strong enough?
;;  If stack = ε -> number of as in ci < number of bs in ci
;;  If stack = ε -> number of as in ci > number of bs in ci
(define a^mb^n (make-ndpda '(S X Y)
                           '(a b)
                           '(b)
                           'S
                           '(X Y)
                           '(((S a ε) (S (b)))
                             ((S ε ε) (X (b)))
                             ((X b (b)) (X ε))
                             ((X b ε) (X ε))
                             ((S ε (b)) (Y ε))
                             ((Y b (b)) (Y ε))
                             ((Y ε (b)) (Y ε)))))

(sm-graph a^mb^n)





(define p (make-ndpda '(S)
                      '(a)
                      '()
                      'S
                      '(S)
                      '(((S ε ε) (S ε)))))

(sm-graph p)






                      