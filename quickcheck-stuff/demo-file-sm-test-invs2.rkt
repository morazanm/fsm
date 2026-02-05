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
(check-equal? (A-INV-EX-NDFA '()) #t)
(check-equal? (A-INV-EX-NDFA '(a)) #t)
(check-equal? (A-INV-EX-NDFA '(a a a a a)) #t)
(check-equal? (A-INV-EX-NDFA '(a a b a c)) #f)
(check-equal? (A-INV-EX-NDFA '(a c b)) #f)
(check-equal? (A-INV-EX-NDFA '(a c a b c)) #f)

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



;; Let Σ = {a b c}
;; L = {w | w is an odd amount of bs or a*, then has a multiple of 3 cs
#|
   L = (b(bb)*Ua*)(ccc)+
|#
;; States
;; S: nothing consumed, start and final state
;; A: a* has been detected
;; B: even amount of bs detected and no cs or as
;; C: odd amount of bs detected and no cs or as
;; D: odd amount of bs or a*, and remainder of amount of cs divided by 3 is 1
;; E: odd amount of bs or a*, and remainder of amount of cs divided by 3 is 2
;; F: odd amount of bs or a*, and remainder of amount of cs divided by 3 is 0, final state

(define EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
  (make-ndfa '(S A B C D E F)
             '(a b c)
             'S
             '(F)
             `((S ,EMP B) (S ,EMP A) (A a A) (A c D)
                          (B b C) (C b B) (C c D) (D c E)
                          (E c F) (F c D))))

;; invariants for EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C

;; word -> Boolean
;; Purpose: Determine if ci should be in S
(define (S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (empty? ci))

;; tests for S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '()) #t)
(check-equal? (S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a)) #f)
(check-equal? (S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a)) #f)
(check-equal? (S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(c b)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in A
(define (A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (andmap (λ (x) (eq? 'a x)) ci))

;; tests for A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '()) #t)
(check-equal? (A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a)) #t)
(check-equal? (A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a a a a a a a)) #t)
(check-equal? (A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b)) #f)
(check-equal? (A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a c)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in B
(define (B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (and (not (contains? ci '(a)))
       (not (contains? ci '(a)))
       (even? (length (filter (λ (x) (eq? 'b x)) ci)))))

;; tests for B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '()) #t)
(check-equal? (B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b)) #t)
(check-equal? (B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b b b b b b)) #t)
(check-equal? (B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b)) #f)
(check-equal? (B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in C
(define (C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (and (not (contains? ci '(a)))
       (not (contains? ci '(a)))
       (odd? (length (filter (λ (x) (eq? 'b x)) ci)))))

;; tests for C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b)) #t)
(check-equal? (C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b)) #t)
(check-equal? (C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b b b b b b b)) #t)
(check-equal? (C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b b)) #f)
(check-equal? (C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in D
(define (D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (and (= (remainder (length (filter (λ (x) (eq? 'c x)) ci)) 3) 1)
       (or (andmap (λ (x) (eq? 'a x)) (filter (λ (x) (not (eq? 'c x))) ci))
           (odd? (length (filter (λ (x) (eq? 'b x)) ci))))))

;; tests for D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b c)) #t)
(check-equal? (D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(c)) #t)
(check-equal? (D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a a a a a c)) #t)
(check-equal? (D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b c c)) #f)
(check-equal? (D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a c c)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in E
(define (E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (and (= (remainder (length (filter (λ (x) (eq? 'c x)) ci)) 3) 2)
       (or (andmap (λ (x) (eq? 'a x)) (filter (λ (x) (not (eq? 'c x))) ci))
           (odd? (length (filter (λ (x) (eq? 'b x)) ci))))))

;; tests for E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b c c)) #t)
(check-equal? (E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(c c c c c)) #t)
(check-equal? (E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a a a a a c c c c c c c c)) #t)
(check-equal? (E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b c)) #f)
(check-equal? (E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a c c c)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in F
(define (F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C ci)
  (and (= (remainder (length (filter (λ (x) (eq? 'c x)) ci)) 3) 0)
       (or (andmap (λ (x) (eq? 'a x)) (filter (λ (x) (not (eq? 'c x))) ci))
           (odd? (length (filter (λ (x) (eq? 'b x)) ci))))))

;; tests for F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C
(check-equal? (F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b c c c)) #t)
(check-equal? (F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(c c c c c c)) #t)
(check-equal? (F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a a a a a a c c c c c c)) #t)
(check-equal? (F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(b b b c)) #f)
(check-equal? (F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C '(a c c)) #f)



;; Let Σ = {a b}
;; L = {w | w does contains aabab 

;; States
;; S: b*, ab is detected, aabb is detected, or empty
;; A: last of the ci is a and no aabab is detected
;; B: last of ci is aa and no aabab is detected
;; C: last of ci is aab and no aabab is detected
;; D: last of ci is aaba and no aabab is detected
;; E: aabab has been detected, final state

(define CONTAINS-aabab (make-dfa '(S A B C D E)
                                 '(a b)
                                 'S
                                 '(E)
                                 `((A a B) (A b S)
                                           (B a B) (B b C) (C a D) (C b S)
                                           (D a B) (D b E) (E a E) (E b E) (S a A) (S b S) )
                                 'no-dead))
                                  

;; invariants for CONTAINS-aabab

;; word -> Boolean 
;; Purpose: Determine if ci should be in S
(define (S-INV-CONTAINS-aabab ci)
  (and (not (contains? ci '(a a b a b)))
       (or (empty? ci)
           (eq? (last ci) 'b)
           (and (or (and (<= 2 (length ci)) (equal? (drop ci (- (length ci) 2)) '(a b)))
                    (and (<= 4 (length ci)) (equal? (drop ci (- (length ci) 4)) '(a a b b))))))))

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
  (and (<= 1 (length ci))
       (eq? (last ci) 'a)
       (not (contains? ci '(a a b a b)))))

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
  (and (<= 2 (length ci))
       (equal? (drop ci (- (length ci) 2)) '(a a))
       (not (contains? ci '(a a b a b)))))

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
  (and (<= 3 (length ci))
       (equal? (drop ci (- (length ci) 3)) '(a a b))
       (not (contains? ci '(a a b a b)))))

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
  (and (<= 4 (length ci))
       (equal? (drop ci (- (length ci) 4)) '(a a b a))
       (not (contains? ci '(a a b a b)))))

;; tests for D-INV-CONTAINS-aabab
(check-equal? (D-INV-CONTAINS-aabab '(a a b a)) #t)
(check-equal? (D-INV-CONTAINS-aabab '(a b b b a a b a)) #t)
(check-equal? (D-INV-CONTAINS-aabab '(b b a b b b a a b a)) #t)
(check-equal? (D-INV-CONTAINS-aabab '()) #f)
(check-equal? (D-INV-CONTAINS-aabab '(a b b b a a b)) #f)
(check-equal? (D-INV-CONTAINS-aabab '(a b b b a a b a b)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in E
(define (E-INV-CONTAINS-aabab ci)
  (and (<= 5 (length ci))
       (contains? ci '(a a b a b))))

;; tests for E-INV-CONTAINS-aabab
(check-equal? (E-INV-CONTAINS-aabab '(a a b a b)) #t)
(check-equal? (E-INV-CONTAINS-aabab '(a b b b a a b a b)) #t)
(check-equal? (E-INV-CONTAINS-aabab '(b b a b b b a a b a b b b b a b)) #t)
(check-equal? (E-INV-CONTAINS-aabab '()) #f)
(check-equal? (E-INV-CONTAINS-aabab '(a b b b a a b)) #f)
(check-equal? (E-INV-CONTAINS-aabab '(a b b b a a b a a)) #f)

