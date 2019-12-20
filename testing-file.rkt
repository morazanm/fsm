#lang racket

(require fsm)

;; TEST MACHINES BELOW
(define INIT-STATES '(A B C D))
(define INIT-START 'A)
(define INIT-FINALS '(C D))
(define INIT-RULES (list `(A ,EMP B) '(B b A) '(A c C) '(C b D)))
(define INIT-SIGMA '(a b c b))
(define INIT-CURRENT 'A)
(define INIT-ALPHA `(a b c))

;; NDFA
(define M1 (make-ndfa INIT-STATES INIT-ALPHA INIT-START INIT-FINALS INIT-RULES))

;; DFA
(define M2 (make-dfa '(A B C)
                     '(a b c)
                     'A
                     '(B C)
                     (list '(A b C)
                           '(A a B)
                           '(B c A))))

(define M3 (make-dfa '(A B C D F)
                     '(a b)
                     'A
                     '(F)
                     '((A a A) (A b B) (B a C) (B b B) (F a F)
                               (C a A) (C b D) (D a F) (D b B)
                               (F b F))))

;; PDA
;; (sm-showtransitions M4 '(a a a b b b))
;;(state '(input) '(stack)) (
(define M4 (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M ,EMP ,EMP) (F ,EMP))
                                    ((M a ,EMP) (M (a)))
                                    ((M b ,EMP) (M (b)))
                                    ((M a (b)) (M ,EMP))
                                    ((M b (a)) (M ,EMP)))))
