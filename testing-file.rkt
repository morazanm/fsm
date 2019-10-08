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

(define M1 (make-ndfa INIT-STATES INIT-ALPHA INIT-START INIT-FINALS INIT-RULES))

;; This machine works
(define M2 (make-dfa '(A B C)
                     '(a b c)
                     'A
                     '(B C)
                     (list '(A b C)
                           '(A a B)
                           '(B c A))))
(define M3 (make-dfa '(A B C)
                     '(a b c)
                     'A
                     '(B C)
                     (list '(B b C)
                           '(C a B)
                           '(B c B))))
