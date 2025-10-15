#lang fsm

(require "sm-test-invs-wip.rkt")

;; word word -> Boolean
;; Purpose: Determine if the second given word appears in
;;          the first given word
(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))


;; L = (a*Ub*)c*

;; States
;; S: nothing consumed, start and final state
;; A: a* has been detected
;; B: b* has been detected
;; C: (a*Ub*)c has been detected, final state

(define EX-NDFA (make-ndfa '(S A B C)
                           '(a b c)
                           'S
                           '(C)
                           `((S ,EMP A) (S ,EMP B)
                                        (A a A) (A c C)
                                        (B b B) (B c C))))

;; word -> Boolean
;; Purpose: Determine if ci should be in S
(define (S-INV-EX-NDFA ci)
  (empty? ci))

;; tests for S-INV-EX-NDFA
(check-equal? (S-INV-EX-NDFA '()) #t)
(check-equal? (S-INV-EX-NDFA '(a a b a c)) #f)
(check-equal? (S-INV-EX-NDFA '(a c b)) #f)
(check-equal? (S-INV-EX-NDFA '(a c a b c)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in A
(define (A-INV-EX-NDFA ci)
  (andmap (λ (x) (eq? 'a x)) ci))

;; tests for A-INV-EX-NDFA
;(check-inv-holds? A-INV-EX-NDFA '() '(a) '(a a a a a))
;(check-inv-fails? A-INV-EX-NDFA '(a a b a c) '(a c b) '(a c a b c))

;; word -> Boolean
;; Purpose: Determine if ci should be in B
(define (B-INV-EX-NDFA ci)
  (andmap (λ (x) (eq? 'b x)) ci))

;; tests for B-INV-EX-NDFA
(check-equal? (B-INV-EX-NDFA '()) #t)
(check-equal? (B-INV-EX-NDFA '(b)) #t)
(check-equal? (B-INV-EX-NDFA '(b b b b b b)) #t)
(check-equal? (B-INV-EX-NDFA '(a a b a c)) #f)
(check-equal? (B-INV-EX-NDFA '(a c b)) #f)
(check-equal? (B-INV-EX-NDFA '(a c a b c)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in C
(define (C-INV-EX-NDFA ci)
  (and (not (empty? ci))
       (eq? (last ci) 'c)
       (<= 1 (length ci))
       (or (andmap (λ (x) (eq? 'a x)) (take ci (- (length ci) 1)))
           (andmap (λ (x) (eq? 'b x)) (take ci (- (length ci) 1))))))
  

;; tests for C-INV-EX-NDFA
(check-equal? (C-INV-EX-NDFA '(c)) #t)
(check-equal? (C-INV-EX-NDFA '(b c)) #t)
(check-equal? (C-INV-EX-NDFA '(b b b b b b c)) #t)
(check-equal? (C-INV-EX-NDFA '(a c)) #t)
(check-equal? (C-INV-EX-NDFA '(a a a a a a c)) #t)
(check-equal? (C-INV-EX-NDFA '()) #f)
(check-equal? (C-INV-EX-NDFA '(a a b a c)) #f)
(check-equal? (C-INV-EX-NDFA '(a c b)) #f)
(check-equal? (C-INV-EX-NDFA '(a c a b c)) #f)
(check-equal? (C-INV-EX-NDFA '(c c)) #f)
(check-equal? (C-INV-EX-NDFA '(a c c b)) #f)




;; word → Boolean
;; Purpose: Determine if word contains aabab
(define (contains-aabab? w)
  (and (>= (length w) 5)
       (or (equal? (take w 5) '(a a b a b))
           (contains-aabab? (rest w)))))
;; word word → Boolean
;; Purpose: Determine if second word ends with first word
(define (end-with? suffix w)
  (and
   (>= (length w) (length suffix))
   (equal? suffix (take-right w (length suffix)))))



;; Let Σ = {a b}
;; L = {w | w does contains aabab 

;; State Documentation
;; S: none of the pattern detected, starting state
;; A: a detected and no other prefix detected 
;; B: aa detected and no other prefix detected 
;; C: aab detected and no other prefix detected 
;; D: aaba detected and no other prefix detected 
;; E: pattern detected, final state

(define CONTAINS-aabab (make-dfa '(S A B C D E)
                                 '(a b)
                                 'S
                                 '(E)
                                 `((S a A) (S b S) (A a B) (A b S)
                                           (B a B) (B b C) (C a D) (C b S)
                                           (D a A) (D b E) (E a E) (E b E))
                                 'no-dead))

(check-accept? CONTAINS-aabab '(a a b a b) '(b b a a a b a b b))
(check-reject? CONTAINS-aabab '() '(b b a a a b a))

;; invariants for CONTAINS-aabab

;; word → Boolean
;; Purpose: Determine that none of aabab is detected
(define (S-INV-CONTAINS-aabab ci)
  (and (not (end-with? '(a) ci))
       (not (end-with? '(a a) ci))
       (not (end-with? '(a a b) ci))
       (not (end-with? '(a a b a) ci))
       (not (contains-aabab? ci))))

;; tests for S-INV-CONTAINS-aabab
(check-equal? (S-INV-CONTAINS-aabab '()) #t)
(check-equal? (S-INV-CONTAINS-aabab '(b)) #t)
(check-equal? (S-INV-CONTAINS-aabab '(b b b)) #t)
(check-equal? (S-INV-CONTAINS-aabab '(a b)) #t)
(check-equal? (S-INV-CONTAINS-aabab '(a b b)) #t)
(check-equal? (S-INV-CONTAINS-aabab '(b b b b a b a b)) #t)
(check-equal? (S-INV-CONTAINS-aabab '(a)) #f)
(check-equal? (S-INV-CONTAINS-aabab '(a a a)) #f)
(check-equal? (S-INV-CONTAINS-aabab '(b a)) #f)
(check-equal? (S-INV-CONTAINS-aabab '(a a)) #f)
(check-equal? (S-INV-CONTAINS-aabab '(a a b a b)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in A
(define (A-INV-CONTAINS-aabab ci)
  (and (end-with? '(a) ci)
       (not (end-with? '(a a) ci))
       (not (end-with? '(a a b a) ci))
       (not (contains-aabab? ci))))


;; tests for A-INV-CONTAINS-aabab
(check-equal? (A-INV-CONTAINS-aabab '(a)) #t)
(check-equal? (A-INV-CONTAINS-aabab '(b b a)) #t)
(check-equal? (A-INV-CONTAINS-aabab '(b b a b a)) #t)
(check-equal? (A-INV-CONTAINS-aabab '()) #f)
(check-equal? (A-INV-CONTAINS-aabab '(a b)) #f)
(check-equal? (A-INV-CONTAINS-aabab '(a a b a b)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in B
(define (B-INV-CONTAINS-aabab ci)
  (and (end-with? '(a a) ci)
       (not (contains-aabab? ci))))

;; tests for B-INV-CONTAINS-aabab
(check-equal? (B-INV-CONTAINS-aabab '(a a)) #t)
(check-equal? (B-INV-CONTAINS-aabab '(b b a b b a a)) #t)
(check-equal? (B-INV-CONTAINS-aabab '(a b b a b b a a)) #t)
(check-equal? (B-INV-CONTAINS-aabab '()) #f)
(check-equal? (B-INV-CONTAINS-aabab '(a b b b b b)) #f)
(check-equal? (B-INV-CONTAINS-aabab '(a a b a b)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in C
(define (C-INV-CONTAINS-aabab ci)
  (and (end-with? '(a a b) ci)
       (not (contains-aabab? ci))))
;; tests for C-INV-CONTAINS-aabab
(check-equal? (C-INV-CONTAINS-aabab '(a a b)) #t)
(check-equal? (C-INV-CONTAINS-aabab '(a b b b a a b)) #t)
(check-equal? (C-INV-CONTAINS-aabab '(b b b a b b a a b)) #t)
(check-equal? (C-INV-CONTAINS-aabab '()) #f)
(check-equal? (C-INV-CONTAINS-aabab '(a a)) #f)
(check-equal? (C-INV-CONTAINS-aabab '(b b)) #f)
(check-equal? (C-INV-CONTAINS-aabab '(a a b a b)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in D
(define (D-INV-CONTAINS-aabab ci)
  (and (end-with? '(a a b a) ci)
       (not (contains-aabab? ci))))
    
;; tests for D-INV-CONTAINS-aabab
(check-equal? (D-INV-CONTAINS-aabab '(a a b a)) #t)
(check-equal? (D-INV-CONTAINS-aabab '(a b b b a a b a)) #t)
(check-equal? (D-INV-CONTAINS-aabab '(b b a b b b a a b a)) #t)
(check-equal? (D-INV-CONTAINS-aabab '()) #f)
(check-equal? (D-INV-CONTAINS-aabab '(a b b b a a b)) #f)
(check-equal? (D-INV-CONTAINS-aabab '(a b b b a a b a b)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in E
(define E-INV-CONTAINS-aabab contains-aabab?)


;; tests for E-INV-CONTAINS-aabab
(check-equal? (E-INV-CONTAINS-aabab '(a a b a b)) #t)
(check-equal? (E-INV-CONTAINS-aabab '(a b b b a a b a b)) #t)
(check-equal? (E-INV-CONTAINS-aabab '(b b a b b b a a b a b b b b a b)) #t)
(check-equal? (E-INV-CONTAINS-aabab '()) #f)
(check-equal? (E-INV-CONTAINS-aabab '(a b b b a a b)) #f)
(check-equal? (E-INV-CONTAINS-aabab '(a b b b a a b a a)) #f)


#;(sm-test-invs CONTAINS-aabab
              (list 'S S-INV-CONTAINS-aabab)
              (list 'A A-INV-CONTAINS-aabab)
              (list 'B B-INV-CONTAINS-aabab)
              (list 'C C-INV-CONTAINS-aabab)
              (list 'D D-INV-CONTAINS-aabab)
              (list 'E E-INV-CONTAINS-aabab))

#;(sm-all-possible-words CONTAINS-aabab)
#;(sm-viz CONTAINS-aabab '(a a b a a))
#;(sm-graph CONTAINS-aabab)


