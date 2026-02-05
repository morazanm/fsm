#lang fsm
(require  "quickcheck-invariants.rkt"
          "new-new-regexp.rkt")


(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))



;; word word --> Boolean
;; Purpose: Determine if second word ends with first word
(define (end-with? suffix w)
  (and (>= (length w) (length suffix))
       (equal? suffix (take-right w (length suffix)))))








;; Let Σ = {a b}. Design and implement a dfa for the following language:
;; L = {w | w does not have two consecutive a

;; L = {w | w does not contain aa}
;; States
;; S: nothing detected, start and final state
;; A: a has been detected, final state
;; B: b has been detected, final state
;; R: aa has been detected

(define NO-AA
  (make-dfa
   '(S A B R)
   '(a b)
   'S
   '(S A B)
   '((S a A) (S b B)
             (B a A) (B b B)
             (A a R) (A b B)
             (R a R) (R b R))
   'no-dead))

(define PROHIBITED-PATTERN '(a a))



;; defining the state invarients


;; word -> Boolean
;; Purpose: Determine if the consumed input contains PROHIBITED PATTERN
;; Assume: |ci| >= 2
(define (R-INV ci)
  (contains? ci PROHIBITED-PATTERN))


;; word -> Boolean
;; Purpose: To determine if the consumed input ends with a
;;          and does not contain the prohibited input
(define (A-INV ci)
  (and (end-with? '(a) ci)
       (contains? ci '(a a))))



;; word -> Boolean
;; Purpose: Determine if NO-AA shoule be in B
(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(b))
       (not (contains? ci PROHIBITED-PATTERN)))) 



;; word -> Boolean
;; Purpose: Determine if NO-AA should be in S
(define (S-INV ci)
  (or (= (length ci) 0)
      (and (not (contains? ci PROHIBITED-PATTERN))
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (not (equal? (drop ci (- (length ci) 2))
                            '(a a)))))))


(define LOI1 (list (list 'S S-INV)
                   (list 'A A-INV)
                   (list 'B B-INV)
                   (list 'R R-INV)))

#;(quickcheck-invs NO-AA (list (list 'S S-INV)
                               (list 'A A-INV)
                               (list 'B B-INV)
                               (list 'R R-INV)))