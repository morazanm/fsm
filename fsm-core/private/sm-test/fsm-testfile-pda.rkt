#lang fsm

(require racket/list
         rackunit
         "sm-test-invs-pda.rkt"
        
         )


(define a^nb^n* (make-ndpda '(S A B)
                            '(a b)
                            '(a z)
                            'S
                            '(S)
                            `(((S ,EMP ,EMP) (A (z)))
                              ((A a ,EMP) (A (a)))
                              ((A ,EMP ,EMP) (B ,EMP))
                              ((B b (a)) (B ,EMP))
                              ((B ,EMP (z)) (S ,EMP)))))


(define a^nb^n*-old (make-ndpda '(S A B C D E)
                            '(a b)
                            '(a b)
                            'S
                            '(C)
                            `(((S ,EMP ,EMP) (A ,EMP))
                              ((A a ,EMP) (A (a)))
                              ((A ,EMP ,EMP) (B ,EMP))
                              ((B b (a)) (B ,EMP))
                              ((B a ,EMP) (D (b)))
                              ((B ,EMP ,EMP) (C ,EMP))
                              ((D a ,EMP) (D (b)))
                              ((D b (b)) (E ,EMP))
                              ((E b (b)) (E ,EMP))
                              ((E ,EMP ,EMP) (A ,EMP)))))

(check-equal? (sm-apply a^nb^n* '()) 'accept)
(check-equal? (sm-apply a^nb^n* '(a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a b a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a b a b a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a a b b a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a a b b a a a b b b a b a a b b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a)) 'reject)
(check-equal? (sm-apply a^nb^n* '(b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a a)) 'reject)
(check-equal? (sm-apply a^nb^n* '(b b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a b b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a a b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a a a a a b b a a a b b b a b b b b)) 'reject)

(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))


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

;; S: nothing detected, start and final state
;; A: a has been detected, final state
;; B: b has been detected, final state
;; R: aa has been detected


;; word -> Boolean
;; Purpose: Determine if the consumed input contains PROHIBITED PATTERN
;; Assume: |ci| >= 2
#;(define (R-INV ci)
  (not(contains? ci PROHIBITED-PATTERN)))            ;<-- purposely broken for testing




(define (R-INV ci)
  (contains? ci PROHIBITED-PATTERN))  ;<- correct version


;; tests for R-INV
;(check-equal? (R-INV '(a)) #f)
;(check-equal? (R-INV '(a a)) #t)
;(check-equal? (R-INV '(a a b)) #t)


;; word -> Boolean
;; Purpose: To determine if the consumed input ends with a
;;          and does not contain the prohibited input
(define (A-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(a))    ;<-- broken 
       (not (contains? ci PROHIBITED-PATTERN))))

#;(define (A-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(a))    ;<-- not broken 
       (not (contains? ci PROHIBITED-PATTERN))))


;;tests for A-INV
#;(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(b)) #f)
#;(check-equal? (A-INV '(a b a)) #t)


;; word -> Boolean
;; Purpose: Determine if NO-AA shoule be in B
(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(b))
       (not (contains? ci PROHIBITED-PATTERN))))    ;<-- not broken

#;(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(b))
       (contains? ci PROHIBITED-PATTERN)))          ; <- broken


#|
;;tests for B-INV
(check-equal? (B-INV '(b)) #t)
(check-equal? (B-INV '(a b)) #t)
(check-equal? (B-INV '(a b b)) #t)
(check-equal? (B-INV '(a a b b)) #f)
(check-equal? (B-INV '(a a a b b a a)) #f)      
(check-equal? (B-INV '(a a b a b b a b b)) #f)
|#


;; word -> Boolean
;; Purpose: Determine if NO-AA should be in S
(define (S-INV ci)
  (or (= (length ci) 0)
      (and (not (contains? ci PROHIBITED-PATTERN))
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (not (equal? (drop ci (- (length ci) 2))    ; <- not broken
                            '(a a)))))))


#;(define (S-INV ci)
  (or (= (length ci) 0)
      (and (contains? ci PROHIBITED-PATTERN)
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (equal? (drop ci (- (length ci) 2)    ; <- broken
                            '(a a)))))))

#|
(check-equal? (S-INV '(b)) #t)
(check-equal? (S-INV '(a b)) #t)
(check-equal? (S-INV '(a b a b)) #t)
(check-equal? (S-INV '(a a b b)) #f)
(check-equal? (S-INV '(a)) #f)

        

(check-equal? (sm-apply NO-AA '()) 'accept)
(check-equal? (sm-apply NO-AA '(a)) 'accept)
(check-equal? (sm-apply NO-AA '(a b)) 'accept)
(check-equal? (sm-apply NO-AA '(a b a)) 'accept)
(check-equal? (sm-apply NO-AA '(b b)) 'accept)
(check-equal? (sm-apply NO-AA '(a a)) 'reject)
(check-equal? (sm-apply NO-AA '(a b a a)) 'reject)
(check-equal? (sm-apply NO-AA '(a b b a a b)) 'reject)
|#
(define (DEAD-INV ci)
  #true)


(define LOI1 (list (list 'S S-INV)
                   (list 'A A-INV)
                   (list 'B B-INV)
                   (list 'R R-INV)))
