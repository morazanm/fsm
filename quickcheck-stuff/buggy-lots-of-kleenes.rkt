#lang racket
(require "quickcheck-invariants.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-apply.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/sm-getters.rkt"
         "../sm-graph.rkt"
         racket/list
         rackunit)

;; (a*Ub*)(c*Ud*)U(c*Ud*)a*

;; States:
;;  S: ci = empty
;;  A: ci = c*
;;  B: ci = d*
;;  C: ci = a*
;;  D: ci = b*
;;  E: ci = b*c* or a*c*
;;  F: ci = a*d* or b*d*
;;  G: ci = c*a* or d*a*
(define lots-of-kleenes (make-unchecked-ndfa '(S A B C D E F G)
                                             '(a b c d)
                                             'S
                                             '(S E F G)
                                             `((S ,EMP A) (S ,EMP B) (S ,EMP C) (S ,EMP D)
                                               (A c A)    (A ,EMP G) (B d B)    (B ,EMP G)
                                               (C a C)    (C ,EMP E) (C ,EMP F) (D b D)
                                               (D ,EMP E) (D ,EMP F) (E c E)    (F d F)  (F ,EMP G) ;<-- added buggy rule
                                               (G a G))))

;; (a*Ub*)(c*Ud*)U(c*Ud*)a*
;; tests for lots-of-kleenes
(check-equal? (sm-apply lots-of-kleenes '()) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(a)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(b)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(c)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(d)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(a a a a)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(a a c)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(a a d d)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(c c c c a)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(d d d d a a a a)) 'accept)
(check-equal? (sm-apply lots-of-kleenes '(a b)) 'reject)
(check-equal? (sm-apply lots-of-kleenes '(c d)) 'reject)
(check-equal? (sm-apply lots-of-kleenes '(c c c c d)) 'reject)
(check-equal? (sm-apply lots-of-kleenes '(a a a a b b b)) 'reject)
(check-equal? (sm-apply lots-of-kleenes '(b b b a a a)) 'reject)
(check-equal? (sm-apply lots-of-kleenes '(d d d d d d c c)) 'reject)



;; word -. Boolean
;; Purpose: To determine if the given ci belongs in S
(define (S-INV-lots-of-kleenes ci)
  (empty? ci))

;; tests for S-INV-lots-of-kleenes
(check-equal? (S-INV-lots-of-kleenes '()) #t)
(check-equal? (S-INV-lots-of-kleenes '(a)) #f)
(check-equal? (S-INV-lots-of-kleenes '(b)) #f)
(check-equal? (S-INV-lots-of-kleenes '(c)) #f)
(check-equal? (S-INV-lots-of-kleenes '(a a)) #f)


;; word -> boolean
;; Purpose: To determine if the given word belong in A
(define (A-INV-lots-of-kleenes ci)
  (andmap (λ (x) (eq? 'c x)) ci))

;; tests for A-INV-lots-of-kleenes
(check-equal? (A-INV-lots-of-kleenes '()) #t)
(check-equal? (A-INV-lots-of-kleenes '(c)) #t)
(check-equal? (A-INV-lots-of-kleenes '(c c c c)) #t)
(check-equal? (A-INV-lots-of-kleenes '(a)) #f)
(check-equal? (A-INV-lots-of-kleenes '(b)) #f)
(check-equal? (A-INV-lots-of-kleenes '(d)) #f)
(check-equal? (A-INV-lots-of-kleenes '(a b)) #f)


;; word -> Boolean
;; Purpose: Determine if the give word belongs in B
(define (B-INV-lots-of-kleenes ci)
  (andmap (λ (x) (eq? 'd x)) ci))

;; tests B-INV-lots-of-kleenes
(check-equal? (B-INV-lots-of-kleenes '()) #t)
(check-equal? (B-INV-lots-of-kleenes '(d)) #t)
(check-equal? (B-INV-lots-of-kleenes '(d d)) #t)
(check-equal? (B-INV-lots-of-kleenes '(d d d)) #t)
(check-equal? (B-INV-lots-of-kleenes '(a)) #f)
(check-equal? (B-INV-lots-of-kleenes '(b)) #f)
(check-equal? (B-INV-lots-of-kleenes '(c)) #f)


;; word -> Boolean
;; Purpose: Determine if the given word belongs in C
(define (C-INV-lots-of-kleenes ci)
  (andmap (λ (x) (eq? 'a x)) ci))

;; tests for C-INV-lots-of-kleenes
(check-equal? (C-INV-lots-of-kleenes '()) #t)
(check-equal? (C-INV-lots-of-kleenes '(a)) #t)
(check-equal? (C-INV-lots-of-kleenes '(a a a a)) #t)
(check-equal? (C-INV-lots-of-kleenes '(a a)) #t)
(check-equal? (C-INV-lots-of-kleenes '(b)) #f)
(check-equal? (C-INV-lots-of-kleenes '(c)) #f)
(check-equal? (C-INV-lots-of-kleenes '(d)) #f)


;; word -> Boolean
;; Purpose: Determine if the give word belongs in D
(define (D-INV-lots-of-kleenes ci)
  (andmap (λ (x) (eq? 'b x)) ci))

;; tests for D-INV-lots-of-kleenes
(check-equal? (D-INV-lots-of-kleenes '()) #t)
(check-equal? (D-INV-lots-of-kleenes '(b)) #t)
(check-equal? (D-INV-lots-of-kleenes '(b b b b)) #t)
(check-equal? (D-INV-lots-of-kleenes '(a)) #f)
(check-equal? (D-INV-lots-of-kleenes '(c)) #f)
(check-equal? (D-INV-lots-of-kleenes '(d)) #f)


;; word -> Boolean
;; Purpose: Determine if the given word belongs in E
(define (E-INV-lots-of-kleenes ci)
  (let* [(AorBs (takef ci
                       (λ (x) (or (eq? 'a x)(eq? 'b x)))))
         (Cs (drop ci (length AorBs)))]
    (and (or (andmap (λ (s) (eq? s 'a)) AorBs)
             (andmap (λ (s) (eq? s 'b)) AorBs))
         (andmap (λ (s) (eq? s 'c)) Cs))))

#;(define (F-INV-lots-of-kleenes ci)
  (let* [(AorBs (takef ci
                       (λ (x) (or (eq? 'a x)(eq? 'b x)))))   ;<-- correct one
         (Ds (drop ci (length AorBs)))]
    (and (or (andmap (λ (s) (eq? s 'a)) AorBs)
             (andmap (λ (s) (eq? s 'b)) AorBs))
         (andmap (λ (s) (eq? s 'd)) Ds))))


(define (F-INV-lots-of-kleenes ci)
  (let* [(AorBs (takef ci
                       (λ (x) (or (eq? 'a x)(eq? 'b x)))))
         (Ds (drop ci (length AorBs)))]
    (and (or (andmap (λ (s) (eq? s 'a)) AorBs)
             (andmap (λ (s) (eq? s 'b)) AorBs))
         (andmap (λ (s) (eq? s 'c)) Ds))))


(define (G-INV-lots-of-kleenes ci)
  (let* [(CorDs (takef ci
                       (λ (x) (or (eq? 'c x)(eq? 'd x)))))
         (As (drop ci (length CorDs)))]
    (and (or (andmap (λ (s) (eq? s 'c)) CorDs)
             (andmap (λ (s) (eq? s 'd)) CorDs))
         (andmap (λ (s) (eq? s 'a)) As))))


#;(quickcheck-invs lots-of-kleenes (list (list 'S S-INV-lots-of-kleenes)
                                                        (list 'A A-INV-lots-of-kleenes)
                                                        (list 'B B-INV-lots-of-kleenes)
                                                        (list 'C C-INV-lots-of-kleenes)
                                                        (list 'D D-INV-lots-of-kleenes)
                                                        (list 'E E-INV-lots-of-kleenes)
                                                        (list 'F F-INV-lots-of-kleenes)
                                                        (list 'G G-INV-lots-of-kleenes)))
