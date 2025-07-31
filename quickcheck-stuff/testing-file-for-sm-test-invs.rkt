#lang fsm
(require "sm-test-invs-wip.rkt")
(provide (all-defined-out))

;; USEFUL FUNCTIONS

;; word word -> Boolean
;; Purpose: Determine if the second given word appears in
;;          the first given word
(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))


;; X loX -> Boolean
;; Purpose: To determine if the given item is a member of the given list
(define (member? x lox)
  (ormap (λ (y) (equal? x y)) lox))


;                                      
;                                      
;                                      
;                                      
;   ;;;;;;   ;;;;;;;    ;;;            
;    ;    ;   ;    ;     ;;            
;    ;     ;  ;    ;    ;  ;    ;;;; ; 
;    ;     ;  ;  ;      ;  ;   ;    ;; 
;    ;     ;  ;;;;      ;  ;   ;       
;    ;     ;  ;  ;     ;;;;;;   ;;;;;  
;    ;     ;  ;        ;    ;        ; 
;    ;    ;   ;       ;      ; ;     ; 
;   ;;;;;;   ;;;;;   ;;;    ;;;;;;;;;  
;                                      
;                                      
;                                      
;                                      
;                                      


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
(define (R-INV ci)
  (not(contains? ci PROHIBITED-PATTERN)))            ;<-- purposely broken for testing


;; tests for R-INV
;(check-equal? (R-INV '(a)) #f)
;(check-equal? (R-INV '(a a)) #t)
;(check-equal? (R-INV '(a a b)) #t)


;; word -> Boolean
;; Purpose: To determine if the consumed input ends with a
;;          and does not contain the prohibited input
(define (A-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(a))
       (contains? ci PROHIBITED-PATTERN)))

;;tests for A-INV
#;(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(b)) #f)
#;(check-equal? (A-INV '(a b a)) #t)


;; word -> Boolean
;; Purpose: Determine if NO-AA shoule be in B
(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(b))
       (not (contains? ci PROHIBITED-PATTERN))))

;;tests for B-INV
(check-equal? (B-INV '(b)) #t)
(check-equal? (B-INV '(a b)) #t)
(check-equal? (B-INV '(a b b)) #t)
(check-equal? (B-INV '(a a b b)) #f)
(check-equal? (B-INV '(a a a b b a a)) #f)      
(check-equal? (B-INV '(a a b a b b a b b)) #f)

;; word -> Boolean
;; Purpose: Determine if NO-AA should be in S
(define (S-INV ci)
  (or (= (length ci) 0)
      (and (not (contains? ci PROHIBITED-PATTERN))
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (not (equal? (drop ci (- (length ci) 2))
                            '(a a)))))))

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

(define (DEAD-INV ci)
  #true)


(define LOI1 (list (list 'S S-INV) (list 'A A-INV) (list 'B B-INV) (list 'R R-INV)))



;; making a dfa with unreachable states
;; C & D are unreachable from the starting config

(define NO-AA-WITH-UNREACHABLE-STATES (make-dfa
                                       '(S A B R C D)
                                       '(a b)
                                       'S
                                       '(S A B)
                                       '((S a A) (S b B)
                                                 (B a A) (B b B)
                                                 (A a R) (A b B)
                                                 (R a R) (R b R)
                                                 (C a C) (C b D)
                                                 (D a C) (D b D))
                                       'no-dead))



(define DNA-SEQUENCE (make-dfa '(K H F M I D B S R)
                               '(a t c g)
                               'K
                               '(K F I B R)
                               `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                         (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                         (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                         (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (DNA-K-INV a-word)
  (not (empty? a-word)))

;;word -> boolean
;;Purpose: Determines if the given word has more a's than t's
(define (DNA-H-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (< num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of a's and t's
(define (DNA-F-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more t's than a's
(define (DNA-M-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-t num-a)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of t's and a's
(define (DNA-I-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more c's than g's
(define (DNA-D-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-c num-g)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of c's and g's
(define (DNA-B-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has more g's than c's
(define (DNA-S-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of g's and c's
(define (DNA-R-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

(define DNA-INVS (list (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
                       (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)
                       (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV)))




;; Let Σ = {a b}. Design and implement a dfa for the following language:
;; L = {w | w has an even number of b}

;; L = {w | w does not contain aa}
;; States
;; S: even number of bs, start and final state
;; F: odd number of bs

(define EVEN-NUM-Bs
  (make-dfa '(S F)
            '(a b)
            'S
            '(S)
            `((S a S) (S b F)
                      (F a F) (F b S))
            'no-dead))



;;word -> boolean
;;Purpose: Determine if the given word has an even number of Bs
(define (EVEN-NUM-Bs-S-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))

;;word -> boolean
;;Purpose: Determine if the given word has an odd number of Bs
(define (EVEN-NUM-Bs-F-INV a-word)
  (odd? (length (filter (λ (w) (equal? w 'b)) a-word))))



(define LOI-EVEN-Bs (list (list 'S EVEN-NUM-Bs-S-INV) (list 'F EVEN-NUM-Bs-F-INV)))


#| Sophia's version of CONTAINS-aabab


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

(define LOI-CONTAINS-aabab (list (list 'S S-INV-CONTAINS-aabab) (list 'A A-INV-CONTAINS-aabab)
                                 (list 'B B-INV-CONTAINS-aabab) (list 'C C-INV-CONTAINS-aabab)
                                 (list 'D D-INV-CONTAINS-aabab) (list 'E E-INV-CONTAINS-aabab)))

|#

;;marco's version of Contains-aabab

(define CONTAINS-aabab 
  (make-dfa '(S A B C D E)
            '(a b)
            'S
            '(E)
            '((S a A) (S b S) (A a B) (A b S)
                      (B a B) (B b C) (C a D) (C b S)
                      (D a B) (D b E) (E a E) (E b E))
            'no-dead))

(check-accept? CONTAINS-aabab  '(a a b a b) '(b b a a a b a b b))
(check-reject? CONTAINS-aabab  '() '(b b a a a b a) '(a a b a a))

;; word --> Boolean
;; Purpose: Determine of word contains aabab
(define (contains-aabab? w)
  (and (>= (length w) 5)
       (or (equal? (take w 5) '(a a b a b))
           (contains-aabab? (rest w)))))

;; word word --> Boolean
;; Purpose: Determine if second word ends with first word
(define (end-with? suffix w)
  (and (>= (length w) (length suffix))
       (equal? suffix (take-right w (length suffix)))))

;; word -> Boolean
;; Purpose: Determine that none of aabab is detected
(define (S2-INV ci)
  (and (not (end-with? '(a) ci))
       (not (end-with? '(a a) ci))
       (not (end-with? '(a a b) ci))
       (not (end-with? '(a a b a) ci))
       (not (contains-aabab? ci))))

;; word -> Boolean
;; Purpose: Determine that only a is detected
(define (A2-INV ci)
  (and (end-with? '(a) ci)
       (not (end-with? '(a a) ci))
       (not (end-with? '(a a b a) ci))
       (not (contains-aabab? ci))))

;; word -> Boolean
;; Purpose: Determine that only aa is detected
(define (B2-INV ci)
  (and (end-with? '(a a) ci)
       (not (contains-aabab? ci))))

;; word -> Boolean
;; Purpose: Determine that only aab is detected
(define (C2-INV ci)
  (and (end-with? '(a a b) ci)
       (not (contains-aabab? ci))))

;; word -> Boolean
;; Purpose: Determine that only aaba is detected
(define (D2-INV ci)
  (and (end-with? '(a a b a) ci)
       (not (contains-aabab? ci))))

;; word -> Boolean
;; Purpose: Determine that only aabab is detected
(define E2-INV contains-aabab?)

(define RES (sm-test-invs CONTAINS-aabab
                          (list 'S S2-INV)
                          (list 'A A2-INV)
                          (list 'B B2-INV)
                          (list 'C C2-INV)
                          (list 'D D2-INV)
                          (list 'E E2-INV)))

(define RES-WORDS (sm-all-possible-words CONTAINS-aabab
                                         (list (list 'S S2-INV)
                                               (list 'A A2-INV)
                                               (list 'B B2-INV)
                                               (list 'C C2-INV)
                                               (list 'D D2-INV)
                                               (list 'E E2-INV))))

(define TOTAL-WORDS (foldl (λ (pair acc) (+ (length (second pair)) acc))
                           0
                           RES-WORDS))


;; L = {w | w does not contain bababa}
;; States
;; S: a*, baa is detected, or babaa is detected , start and final state
;; A: last of ci is b and no bababa is detected, final state
;; B: last of ci is ba and no bababa is detected, final state
;; C: last of ci is bab and no bababa is detected, final state
;; D: last of ci is baba and no bababa is detected, final state
;; E: last of ci is babab and no bababa is detected, final state
;; F: bababa has been detected, final state
(define no-contain-bababa (make-dfa '(S A B C D E F)
                                    '(a b)
                                    'S
                                    '(S A B C D E)
                                    '((S a S) (S b A)
                                              (A a B) (A b A)
                                              (B a S) (B b C)
                                              (C a D) (C b A)
                                              (D a S) (D b E)
                                              (E a F) (E b A)
                                              (F a F) (F b F))
                                    'no-dead
                                    ))

;;tests for no-contain-bababa
(check-equal? (sm-apply no-contain-bababa '()) 'accept)
(check-equal? (sm-apply no-contain-bababa '(a)) 'accept)
(check-equal? (sm-apply no-contain-bababa '(b)) 'accept)
(check-equal? (sm-apply no-contain-bababa '(a a a)) 'accept)
(check-equal? (sm-apply no-contain-bababa '(a b b)) 'accept)
(check-equal? (sm-apply no-contain-bababa '(a b b a b a b)) 'accept)
(check-equal? (sm-apply no-contain-bababa '(b a b a b a)) 'reject)
(check-equal? (sm-apply no-contain-bababa '(a b a b a b a b b a)) 'reject)
(check-equal? (sm-apply no-contain-bababa '(b b a b a b a a a b a b a b a)) 'reject)
(check-equal? (sm-apply no-contain-bababa '(a b a b a b a b a b a a b b a)) 'reject)

;; invariants for no-contains-bababa

;; note: using auxilaries that were made for CONTAINS-aabab

;; word -> Boolean
;; Purpose: Determine if ci should be in S
(define (S-INV-no-contain-bababa ci)
  (and (not (contains? ci '(b a b a b a)))
       (not (end-with? '(b) ci))
       (not (end-with? '(b a) ci))
       (not (end-with? '(b a b) ci))
       (not (end-with? '(b a b a) ci))
       (not (end-with? '(b a b a b) ci))))

;; tests for S-INV-no-contain-bababa
(check-equal? (S-INV-no-contain-bababa '()) #t)
(check-equal? (S-INV-no-contain-bababa '(a)) #t)
(check-equal? (S-INV-no-contain-bababa '(a a)) #t)
(check-equal? (S-INV-no-contain-bababa '(b a a)) #t)
(check-equal? (S-INV-no-contain-bababa '(b a b a a)) #t)
(check-equal? (S-INV-no-contain-bababa '(b b a a b a a a a)) #t)
(check-equal? (S-INV-no-contain-bababa '(b)) #f)
(check-equal? (S-INV-no-contain-bababa '(b a b a a b a b)) #f)
(check-equal? (S-INV-no-contain-bababa '(b a b a b a a a)) #f)
(check-equal? (S-INV-no-contain-bababa '(b b a b a b a b b b a a b)) #f)
(check-equal? (S-INV-no-contain-bababa '(a b a b a b a b a b)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in A
(define (A-INV-no-contain-bababa ci)
  (and (not (contains? ci '(b a b a b a)))
       (end-with? '(b) ci)
       (not (end-with? '(b a) ci))
       (not (end-with? '(b a b) ci))
       (not (end-with? '(b a b a) ci))
       (not (end-with? '(b a b a b) ci))))

;; tests for A-INV-no-contains-bababa
(check-equal? (A-INV-no-contain-bababa '(b)) #t)
(check-equal? (A-INV-no-contain-bababa '(a a b a a b)) #t)
(check-equal? (A-INV-no-contain-bababa '(b a a b)) #t)
(check-equal? (A-INV-no-contain-bababa '(b b b b b b)) #t)
(check-equal? (A-INV-no-contain-bababa '()) #f)
(check-equal? (A-INV-no-contain-bababa '(a)) #f)
(check-equal? (A-INV-no-contain-bababa '(b a)) #f)
(check-equal? (A-INV-no-contain-bababa '(b a b)) #f)
(check-equal? (A-INV-no-contain-bababa '(b a b a)) #f)
(check-equal? (A-INV-no-contain-bababa '(b a b a b)) #f)
(check-equal? (A-INV-no-contain-bababa '(b a b a b a)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in B
(define (B-INV-no-contain-bababa ci)
  (and (not (contains? ci '(b a b a b a)))
       (end-with? '(b a) ci)
       (not (end-with? '(b a b) ci))
       (not (end-with? '(b a b a) ci))
       (not (end-with? '(b a b a b) ci))))

;; tests for B-INV-no-contains-bababa
(check-equal? (B-INV-no-contain-bababa '(b a)) #t)
(check-equal? (B-INV-no-contain-bababa '(a a b a a b a)) #t)
(check-equal? (B-INV-no-contain-bababa '(a a b a)) #t)
(check-equal? (B-INV-no-contain-bababa '(b b b b b a)) #t)
(check-equal? (B-INV-no-contain-bababa '(a b a b a a b a)) #t)
(check-equal? (B-INV-no-contain-bababa '(a)) #f)
(check-equal? (B-INV-no-contain-bababa '(b)) #f)
(check-equal? (B-INV-no-contain-bababa '(b a b)) #f)
(check-equal? (B-INV-no-contain-bababa '(b a b a)) #f)
(check-equal? (B-INV-no-contain-bababa '(b a b a b)) #f)
(check-equal? (B-INV-no-contain-bababa '(b a b a b a)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in C
(define (C-INV-no-contain-bababa ci)
  (and (not (contains? ci '(b a b a b a)))
       (end-with? '(b a b) ci)
       (not (end-with? '(b a b a) ci))
       (not (end-with? '(b a b a b) ci))))

;; tests for C-INV-no-contains-bababa
(check-equal? (C-INV-no-contain-bababa '(b a b)) #t)
(check-equal? (C-INV-no-contain-bababa '(a a b a a b a b)) #t)
(check-equal? (C-INV-no-contain-bababa '(a a b a b)) #t)
(check-equal? (C-INV-no-contain-bababa '(b b b b b a b)) #t)
(check-equal? (C-INV-no-contain-bababa '(a b a b a a b a b)) #t)
(check-equal? (C-INV-no-contain-bababa '(a)) #f)
(check-equal? (C-INV-no-contain-bababa '(b)) #f)
(check-equal? (C-INV-no-contain-bababa '(b a)) #f)
(check-equal? (C-INV-no-contain-bababa '(b a b a)) #f)
(check-equal? (C-INV-no-contain-bababa '(b a b a b)) #f)
(check-equal? (C-INV-no-contain-bababa '(b a b a b a)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in D
(define (D-INV-no-contain-bababa ci)
  (and (not (contains? ci '(b a b a b a)))
       (end-with? '(b a b a) ci)
       (not (end-with? '(b a b a b) ci))))

;; tests for D-INV-no-contains-bababa
(check-equal? (D-INV-no-contain-bababa '(b a b a)) #t)
(check-equal? (D-INV-no-contain-bababa '(a a b a a b a b a)) #t)
(check-equal? (D-INV-no-contain-bababa '(a a b a b a)) #t)
(check-equal? (D-INV-no-contain-bababa '(b b b b b a b a)) #t)
(check-equal? (D-INV-no-contain-bababa '(a b a b a a b a b a)) #t)
(check-equal? (D-INV-no-contain-bababa '(a)) #f)
(check-equal? (D-INV-no-contain-bababa '(b)) #f)
(check-equal? (D-INV-no-contain-bababa '(b a)) #f)
(check-equal? (D-INV-no-contain-bababa '(b a b)) #f)
(check-equal? (D-INV-no-contain-bababa '(b a b a b)) #f)
(check-equal? (D-INV-no-contain-bababa '(b a b a b a)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in E
(define (E-INV-no-contain-bababa ci)
  (and (not (contains? ci '(b a b a b a)))
       (end-with? '(b a b a b) ci)))

;; tests for E-INV-no-contains-bababa
(check-equal? (E-INV-no-contain-bababa '(b a b a b)) #t)
(check-equal? (E-INV-no-contain-bababa '(a a b a a b a b a b)) #t)
(check-equal? (E-INV-no-contain-bababa '(a a b a b a b)) #t)
(check-equal? (E-INV-no-contain-bababa '(b b b b b a b a b)) #t)
(check-equal? (E-INV-no-contain-bababa '(a b a b a a b a b a b)) #t)
(check-equal? (E-INV-no-contain-bababa '(a)) #f)
(check-equal? (E-INV-no-contain-bababa '(b)) #f)
(check-equal? (E-INV-no-contain-bababa '(b a)) #f)
(check-equal? (E-INV-no-contain-bababa '(b a b)) #f)
(check-equal? (E-INV-no-contain-bababa '(b a b a)) #f)
(check-equal? (E-INV-no-contain-bababa '(b a b a b a)) #f)


;; word -> Boolean
;; Purpose: Determine if ci should be in F
(define (F-INV-no-contain-bababa ci)
  (contains? ci '(b a b a b a)))

;; tests for F-INV-no-contains-bababa
(check-equal? (F-INV-no-contain-bababa '(b a b a b a)) #t)
(check-equal? (F-INV-no-contain-bababa '(a a b a a b a b a b a)) #t)
(check-equal? (F-INV-no-contain-bababa '(a a b a b a b a)) #t)
(check-equal? (F-INV-no-contain-bababa '(b b b b b a b a b a)) #t)
(check-equal? (F-INV-no-contain-bababa '(a b a b a a b a b a b a)) #t)
(check-equal? (F-INV-no-contain-bababa '(a)) #f)
(check-equal? (F-INV-no-contain-bababa '(b)) #f)
(check-equal? (F-INV-no-contain-bababa '(b a)) #f)
(check-equal? (F-INV-no-contain-bababa '(b a b)) #f)
(check-equal? (F-INV-no-contain-bababa '(b a b a)) #f)
(check-equal? (F-INV-no-contain-bababa '(b a b a b)) #f)














;                                               
;                                               
;                                               
;                                               
;  ;;;   ;;; ;;;;;;   ;;;;;;;    ;;;            
;   ;;    ;   ;    ;   ;    ;     ;;            
;   ; ;   ;   ;     ;  ;    ;    ;  ;    ;;;; ; 
;   ; ;   ;   ;     ;  ;  ;      ;  ;   ;    ;; 
;   ;  ;  ;   ;     ;  ;;;;      ;  ;   ;       
;   ;   ; ;   ;     ;  ;  ;     ;;;;;;   ;;;;;  
;   ;   ; ;   ;     ;  ;        ;    ;        ; 
;   ;    ;;   ;    ;   ;       ;      ; ;     ; 
;  ;;;   ;;  ;;;;;;   ;;;;;   ;;;    ;;;;;;;;;  
;                                               
;                                               
;                                               
;                                               
;                                               





;Let Σ ={a b c}. Design and implement an ndfa for:
;L = {w | w is missing exactly 1 of the elements in Σ

;; States
;; S: nothing consumed, start state
;; A: a* has been consumed
;; B: at least one a and at least one b has been consumed, and no c's have been consumed, final state
;; C: at least one a and at least one c has been consumed, an no b's have been consumed, final state
;; D: b* has been consumed
;; E: at least one b and at least one c has been consumed, and no a's have been consumed, final state
;; F: c* has been consumed


(define ONE-LETTER-MISSING (make-ndfa '(S A B C D E F)
                                      '(a b c)
                                      'S
                                      '(B C E)
                                      `((S a A) (S b D) (S c F)
                                                (A a A) (A b B) (A c C)
                                                (B a B) (B b B)
                                                (C a C) (C c C)
                                                (D b D) (D c E) (D a B)
                                                (E b E) (E c E)
                                                (F c F) (F b E) (F a C))
                                      #:accepts (list '(a c) '(b a) '(c a)
                                                      '(a b) '(a b b) '(a b a a)
                                                      '(b a b b) '(c a c a))
                                      #:rejects (list '(a) '(b) '(c)
                                                      '(a b c) '(a a b b c c)
                                                      '(a b c a b c) '(b a c a) '())))

;; word -> Boolean
;; Purpose: To determine if ci should be in S
(define (S-INV-1-MISSING ci)
  (empty? ci))

;; tests for S-INV-1-MISSING
(check-equal? (S-INV-1-MISSING '()) #t)
(check-equal? (S-INV-1-MISSING '(a b c)) #f)
(check-equal? (S-INV-1-MISSING '(a b a c)) #f)

;; word -> Boolean
;; Purpose: To determine if ci should be in A
(define (A-INV-1-MISSING ci)
  (and (<= 1 (length ci))
       (andmap (λ (x) (equal? x 'a)) ci)))

;; tests for A-INV-1-MISSING
(check-equal? (A-INV-1-MISSING '(a)) #t)
(check-equal? (A-INV-1-MISSING '(a a a a a)) #t)
(check-equal? (A-INV-1-MISSING '()) #f)
(check-equal? (A-INV-1-MISSING '(a b c)) #f)
(check-equal? (A-INV-1-MISSING '(a b)) #f)
(check-equal? (A-INV-1-MISSING '(c b)) #f)
(check-equal? (A-INV-1-MISSING '(a c)) #f)

;; word -> Boolean
;; Purpose: To determine if ci should be in B
(define (B-INV-1-MISSING ci)
  (and (<= 1 (length ci))
       (contains? ci '(a))
       (contains? ci '(b))
       (not (contains? ci '(c)))))

;; tests for B-INV-1-MISSING
(check-equal? (B-INV-1-MISSING '(a b)) #t)
(check-equal? (B-INV-1-MISSING '(b a)) #t)
(check-equal? (B-INV-1-MISSING '(b a b a b a a)) #t)
(check-equal? (B-INV-1-MISSING '(a a a a b)) #t)
(check-equal? (B-INV-1-MISSING '(a b b b b)) #t)
(check-equal? (B-INV-1-MISSING '(a c a c c b)) #f)
(check-equal? (B-INV-1-MISSING '(b a a c c b)) #f)
(check-equal? (B-INV-1-MISSING '(a c a b c a c)) #f)
(check-equal? (B-INV-1-MISSING '(c b a a b)) #f)


;; word -> Boolean
;; Purpose: To determine if ci should be in C
(define (C-INV-1-MISSING ci)
  (and (<= 1 (length ci))
       (contains? ci '(a))
       (contains? ci '(c))
       (not (contains? ci '(b)))))

;; tests for C-INV-1-MISSING
(check-equal? (C-INV-1-MISSING '(a c)) #t)
(check-equal? (C-INV-1-MISSING '(c a)) #t)
(check-equal? (C-INV-1-MISSING '(a a a c c c a a c)) #t)
(check-equal? (C-INV-1-MISSING '(c a c a)) #t)
(check-equal? (C-INV-1-MISSING '(a c b)) #f)
(check-equal? (C-INV-1-MISSING '(b a c a c b)) #f)
(check-equal? (C-INV-1-MISSING '(b a a c a a b)) #f)


;; word -> Boolean
;; Purpose: To determine if ci should be in D
(define (D-INV-1-MISSING ci)
  (and (<= 1 (length ci))
       (andmap (λ (x) (equal? x 'b)) ci)))

;; tests for D-INV-1-MISSING
(check-equal? (D-INV-1-MISSING '(b)) #t)
(check-equal? (D-INV-1-MISSING '(a)) #f)
(check-equal? (D-INV-1-MISSING '(c)) #f)
(check-equal? (D-INV-1-MISSING '(a b c)) #f)
(check-equal? (D-INV-1-MISSING '(b c a b a c)) #f)


;; word -> Boolean
;; Purpose: To determine if ci should be in E
(define (E-INV-1-MISSING ci)
  (and (<= 1 (length ci))
       (contains? ci '(b))
       (contains? ci '(c))
       (not (contains? ci '(a)))))

;; tests for E-INV-1-MISSING
(check-equal? (E-INV-1-MISSING '(b c)) #t)
(check-equal? (E-INV-1-MISSING '(c c b b)) #t)
(check-equal? (E-INV-1-MISSING '(b c b c)) #t)
(check-equal? (E-INV-1-MISSING '(a b c)) #f)
(check-equal? (E-INV-1-MISSING '(a b a)) #f)
(check-equal? (E-INV-1-MISSING '(a c b a c)) #f)

;; word -> Boolean
;; Purpose: To determine if ci should be in F
(define (F-INV-1-MISSING ci)
  (and (<= 1 (length ci))
       (andmap (λ (x) (equal? x 'c)) ci)))

;; tests for E-INV-1-MISSING
(check-equal? (F-INV-1-MISSING '(c)) #t)
(check-equal? (F-INV-1-MISSING '(c c c c c c)) #t)
(check-equal? (F-INV-1-MISSING '(b)) #f)
(check-equal? (F-INV-1-MISSING '(a)) #f)
(check-equal? (F-INV-1-MISSING '(a b c)) #f)
(check-equal? (F-INV-1-MISSING '(a b)) #f)
(check-equal? (F-INV-1-MISSING '(c a b c)) #f)
(check-equal? (F-INV-1-MISSING '(a b a b c a c b)) #f)


(define LOI-ONE-MISSING (list (list 'A A-INV-1-MISSING) (list 'B B-INV-1-MISSING)
                              (list 'C C-INV-1-MISSING) (list 'D D-INV-1-MISSING)
                              (list 'E E-INV-1-MISSING) (list 'F F-INV-1-MISSING)))




; Design and implement an ndfa for:
; (ab)∗b∗ ∪ ab∗

;;(ab)*b*Uab*


;; L = (ab)*b*Uab*

;; States
;; S: nothing consumed, start and final state
;; A: nothing has been consumed
;; B: ab* has been detected, final state
;; C: (ab)* has been detected, final state
;; D: (ab)*a has been detected, final state
;; E: b* has been detected, final state 

(define ab*b*Uab* (make-ndfa '(S A B C D E)
                             '(a b)
                             'S
                             '(S B C E)
                             `((S ,EMP A) (S ,EMP C) (A a B) (B b B)
                                          (C a D) (C b E) (D b C) (E b E))
                             #:accepts (list '(a b a b a b) '(a) '(a b) '(b) '(a b a b b b) '(a b b b b)
                                             )
                             #:rejects (list '(a a) '(a b a) '(a b a a)
                                             )))


;; word -> Boolean
;; Purpose: Determine if ci should be in S
(define (S-INV-ab*b*Uab* ci)
  (not (= (length ci) 0)))           ;<-- purposely broken for testing

;; tests for S-INV-L
;(check-equal? (S-INV-ab*b*Uab* '()) #t)
;(check-equal? (S-INV-ab*b*Uab* '(a)) #f)
;(check-equal? (S-INV-ab*b*Uab* '(b)) #f)
;(check-equal? (S-INV-ab*b*Uab* '(a b a b a b b)) #f)

;; word -> Boolean
;; Purpose: Determine if ci should be in A
(define (A-INV-ab*b*Uab* ci)          
  (not (= (length ci) 0)))           ;<-- purposely broken for testing

;; tests for A-INV-ab*b*Uab*
;(check-equal? (A-INV-ab*b*Uab* '()) #t)
;(check-equal? (A-INV-ab*b*Uab* '(a)) #f)
;(check-equal? (A-INV-ab*b*Uab* '(b)) #f)
;(check-equal? (A-INV-ab*b*Uab* '(a b a b b a)) #f)
              

;; word -> Boolean
;; Purpose: Determine if ci should be in B
(define (B-INV-ab*b*Uab* ci)
  (or (= (length ci) 1)
      (and (eq? 'a (first ci))
           (andmap (λ (x) (equal? x 'b)) (rest ci)))))

;; tests for B-INV-ab*b*Uab*
(check-equal? (B-INV-ab*b*Uab* '(a)) #t)
(check-equal? (B-INV-ab*b*Uab* '(a b)) #t)
(check-equal? (B-INV-ab*b*Uab* '(a b b b b b b b b)) #t)
(check-equal? (B-INV-ab*b*Uab* '(a b a)) #f)
(check-equal? (B-INV-ab*b*Uab* '(a a)) #f)




(define (filter-abs ci)
  (if (or (not (<= 2 (length ci)))
          (empty? ci)
          (not (equal? (take ci 2) '(a b))))
      ci
      (filter-abs (drop ci 2))))


;; word -> Boolean
;; Purpose: Determine if ci should be in C
(define (C-INV-ab*b*Uab* ci)
  (or (empty? ci)
      (and (<= 2 (length ci))
           (eq? (first ci) 'a)
           (eq? (second ci) 'b)
           (C-INV-ab*b*Uab* (rest (rest ci))))))

;; tests for C-INV-ab*b*Uab*
(check-equal? (C-INV-ab*b*Uab* '()) #t)
(check-equal? (C-INV-ab*b*Uab* '(a b)) #t)
(check-equal? (C-INV-ab*b*Uab* '(a b a b a b)) #t)
(check-equal? (C-INV-ab*b*Uab* '(b)) #f)
(check-equal? (C-INV-ab*b*Uab* '(a)) #f)
(check-equal? (C-INV-ab*b*Uab* '(a a)) #f)
(check-equal? (C-INV-ab*b*Uab* '(a b a b a b a)) #f)



;; word -> Boolean
;; Purpose: Determine if ci should be in D
(define (D-INV-ab*b*Uab* ci)
  (and (<= 1 (length ci))
       (eq? (last ci) 'a)
       (empty? (filter-abs (take ci (- (length ci) 1))))))


(check-equal? (D-INV-ab*b*Uab* '(a)) #t)
(check-equal? (D-INV-ab*b*Uab* '(a b a)) #t)
(check-equal? (D-INV-ab*b*Uab* '(a b a b a)) #t)
(check-equal? (D-INV-ab*b*Uab* '(b)) #f)
(check-equal? (D-INV-ab*b*Uab* '(a b a b b)) #f)
(check-equal? (D-INV-ab*b*Uab* '(a b a b a b)) #f)
(check-equal? (D-INV-ab*b*Uab* '(b a b b b b)) #f)


  
;; word -> Boolean
;; Purpose: Determine if ci should be in E
(define (E-INV-ab*b*Uab* ci)
  (let ([no-abs (filter-abs ci)])
    (and (<= 1 (length ci))
         (eq? (last ci) 'b)
         (<= 1 (length no-abs))
         (andmap (λ (x) (eq? 'b x)) no-abs))))

;; tests for E-INV-ab*b*Uab*
(check-equal? (E-INV-ab*b*Uab* '(b)) #t)
(check-equal? (E-INV-ab*b*Uab* '(b b)) #t)
(check-equal? (E-INV-ab*b*Uab* '(b b b b b b b b)) #t)
(check-equal? (E-INV-ab*b*Uab* '(a b)) #f)
(check-equal? (E-INV-ab*b*Uab* '(a b b b b b)) #t)
(check-equal? (E-INV-ab*b*Uab* '(a b b a b)) #f)
(check-equal? (E-INV-ab*b*Uab* '(b b a b b a b)) #f)



(define LOI-ab*b*Uab* (list (list 'A A-INV-ab*b*Uab*) (list 'B B-INV-ab*b*Uab*) (list 'C C-INV-ab*b*Uab*)
                            (list 'D D-INV-ab*b*Uab*) (list 'E E-INV-ab*b*Uab*) (list 'S S-INV-ab*b*Uab*)))



;;L = aa* U ab*
(define aa*Uab* (make-ndfa '(K B D)
                           '(a b)
                           'K
                           '(B D)
                           `((K a D) (K a B)
                                     (B a B)
                                     (D b D))))


;; word -> Boolean
;; Purpose: To determine whether ci = emp
(define (aa-ab-K-INV ci)
  (empty? ci))

;; word -> Boolean
;; Purpose: To determine whether ci = aa*
(define (aa-ab-B-INV ci)
  (and (not (empty? ci))
       (eq? (first ci) 'a)
       (andmap (λ (w) (eq? w 'a)) ci)))

;; word -> Boolean
;; Purpose: To determine whether ci = ab*
(define (aa-ab-D-INV ci)               ;; <-- this one is purposely broken
  (and (not (empty? ci))
       (eq? (first ci) 'a)
       (andmap (λ (el) (eq? el 'b)) ci)))


(define LOI-aa*-ab* (list (list 'K  aa-ab-K-INV) (list 'B aa-ab-B-INV) (list 'D aa-ab-D-INV)))



(define rnd-ndfa (make-ndfa '(S A B C D E F G H I)
                            '(a b)
                            'S
                            '(A C)
                            `((S ,EMP A) (S ,EMP B)
                                         (A ,EMP E) (A a D) (A ,EMP H) (A ,EMP G)
                                         (B a C)
                                         (D b A) (D a F)
                                         (H a I))))


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

#| Sophia's version of EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C


;; Let Σ = {a b c}
;; L = {w | w is an odd amount of bs or a*, then has a multiple of 3 cs

;; States
;; S: nothing consumed, start and final state
;; A: a* has been detected
;; B: even amount of bs detected and no cs or as
;; C: odd amount of bs detected and no cs or as
;; D: odd amount of bs or a*, and remainder of amount of cs divided by 3 is 1
;; E: odd amount of bs or a*, and remainder of amount of cs divided by 3 is 2
;; F: odd amount of bs or a*, and remainder of amount of cs divided by 3 is 0, final state

(define EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C (make-ndfa '(S A B C D E F)
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


(define LOI-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C (list (list 'S S-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C) (list 'A A-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C)
                                                    (list 'B B-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C) (list 'C C-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C)
                                                    (list 'D D-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C) (list 'E E-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C)
                                                    (list 'F F-INV-EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C)))
|#

;; marco's version of EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C:


(define M3
  (make-ndfa '(S A B C D E F)
             '(a b c)
             'S
             '(F)
             `((S ,EMP B) (S ,EMP A) (A a A) (A c D)
                          (B b C) (C b B) (C c D) (D c E)
                          (E c F) (F c D))))

(define S3-INV empty?)

(define (A3-INV ci)
  (andmap (λ (s) (eq? s 'a)) ci))

(define (B3-INV ci)
  (and (even? (length ci))
       (andmap (λ (s) (eq? s 'b)) ci)))

(define (C3-INV ci)
  (and (odd? (length ci))
       (andmap (λ (s) (eq? s 'b)) ci)))

(define (D3-INV ci)
  (let* [(bcs (takef ci (λ (s) (not (eq? s 'c)))))
         (cs (drop ci (length bcs)))]
    (and (or (C3-INV bcs) (A3-INV bcs))
         (andmap (λ (s) (eq? s 'c)) cs)
         (= (remainder (length cs) 3) 1))))

(define (E3-INV ci)
  (let* [(bcs (takef ci (λ (s) (not (eq? s 'c)))))
         (cs (drop ci (length bcs)))]
    (and (or (C3-INV bcs) (A3-INV bcs))
         (andmap (λ (s) (eq? s 'c)) cs)
         (= (remainder (length cs) 3) 2))))

(define (F3-INV ci)
  (let* [(bcs (takef ci (λ (s) (not (eq? s 'c)))))
         (cs (drop ci (length bcs)))]
    (and (or (C3-INV bcs) (A3-INV bcs))
         (andmap (λ (s) (eq? s 'c)) cs)
         (= (remainder (length cs) 3) 0))))

(define LOI-M3 (list (list 'S S3-INV)
                     (list 'A A3-INV)
                     (list 'B B3-INV)
                     (list 'C C3-INV)
                     (list 'D D3-INV)
                     (list 'E E3-INV)
                     (list 'F F3-INV)))

(define RES2 (sm-test-invs M3
                           (list 'S S3-INV)
                           (list 'A A3-INV)
                           (list 'B B3-INV)
                           (list 'C C3-INV)
                           (list 'D D3-INV)
                           (list 'E E3-INV)
                           (list 'F F3-INV)))

(define RES2-WORDS (sm-all-possible-words M3
                                          (list (list 'S S3-INV)
                                                (list 'A A3-INV)
                                                (list 'B B3-INV)
                                                (list 'C C3-INV)
                                                (list 'D D3-INV)
                                                (list 'E E3-INV)
                                                (list 'F F3-INV))))

(define TOTAL-WORDS2 (foldl (λ (pair acc) (+ (length (second pair)) acc))
                            0
                            RES2-WORDS))








;                                      
;                                      
;                                      
;                                      
;    ;;;;;;  ;;;;;;     ;;;            
;     ;    ;  ;    ;     ;;            
;     ;    ;  ;     ;   ;  ;    ;;;; ; 
;     ;    ;  ;     ;   ;  ;   ;    ;; 
;     ;    ;  ;     ;   ;  ;   ;       
;     ;;;;;   ;     ;  ;;;;;;   ;;;;;  
;     ;       ;     ;  ;    ;        ; 
;     ;       ;    ;  ;      ; ;     ; 
;    ;;;;;   ;;;;;;  ;;;    ;;;;;;;;;  
;                                      
;                                      
;                                      
;                                      
;                                      



;; L = {aˆnbˆn | n >= 0}
;; States
;; S ci = (listof a) = stack, start state
;; M ci = (append (listof a) (listof b)) AND
;;        (length ci as) = (length stack) + (length ci bs)
;; F ci = (append (listof a) (listof b)) and all as and bs matched,
;; final state
;; The stack is a (listof a)
(define aˆnbˆn (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))
;; Tests for aˆnbˆn
(check-equal? (sm-apply aˆnbˆn '(a)) 'reject)
(check-equal? (sm-apply aˆnbˆn '(b b)) 'reject)
(check-equal? (sm-apply aˆnbˆn '(a b b)) 'reject)
(check-equal? (sm-apply aˆnbˆn '(a b a a b b)) 'reject)
(check-equal? (sm-apply aˆnbˆn '()) 'accept)
(check-equal? (sm-apply aˆnbˆn '(a a b b)) 'accept)


;; Invariants for aˆnbˆn

;; word stack → Boolean
;; Purpose: Determine if the given ci and stack are the
;;          same (listof a)
(define (S-INV-aˆnbˆn ci stck)
  (and (= (length ci) (length stck))
       (andmap (λ (i g) (and (eq? i 'a) (eq? g 'a))) ci stck)))

;; Tests for S-INV-aˆnbˆn
(check-equal? (S-INV-aˆnbˆn '() '(a a)) #f)
(check-equal? (S-INV-aˆnbˆn '(a) '()) #f)
(check-equal? (S-INV-aˆnbˆn '(b b b) '(b b b)) #f)
(check-equal? (S-INV-aˆnbˆn '() '()) #t)
(check-equal? (S-INV-aˆnbˆn '(a a a) '(a a a)) #t)


;; word stack → Boolean
;; Purpose: Determine if ci = EMP or a+b+ AND the stack
;;          only contains a AND |ci as| = |stack| + |ci bs|
(define (M-INV-aˆnbˆn ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (equal? (append as bs) ci)
         (andmap (λ (s) (eq? s 'a)) stck)
         (= (length as) (+ (length bs) (length stck))))))

;; Tests for M-INV-aˆnbˆn
(check-equal? (M-INV-aˆnbˆn '(a a b) '(a a)) #f)
(check-equal? (M-INV-aˆnbˆn '(a) '()) #f)
(check-equal? (M-INV-aˆnbˆn '(a a a b) '(a a a)) #f)
(check-equal? (M-INV-aˆnbˆn '(a a a b) '(a)) #f)
(check-equal? (M-INV-aˆnbˆn '() '()) #t)
(check-equal? (M-INV-aˆnbˆn '(a) '(a)) #t)
(check-equal? (M-INV-aˆnbˆn '(a b) '()) #t)
(check-equal? (M-INV-aˆnbˆn '(a a a b b) '(a)) #t)



;; word stack → Boolean
;; Purpose: Determine if ci = a^nb^n and stack is empty
(define (F-INV-aˆnbˆn ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (empty? stck)
         (equal? (append as bs) ci)
         (= (length as) (length bs)))))

;; Tests for F-INV-aˆnbˆn
(check-equal? (F-INV-aˆnbˆn '(a a b) '()) #f)
(check-equal? (F-INV-aˆnbˆn '(a) '()) #f)
(check-equal? (F-INV-aˆnbˆn '(a a a b) '(a a a)) #f)
(check-equal? (F-INV-aˆnbˆn '() '()) #t)
(check-equal? (F-INV-aˆnbˆn '(a b) '()) #t)
(check-equal? (F-INV-aˆnbˆn '(a a b b) '()) #t)



;; L = wcwˆR | w in (a b)*
;; States
;; S ci is empty and stack is empty
;; P ci = stackˆR AND c not in ci
;; Q ci = (append w (list c) v) AND
;; w = stackˆR vˆR
;; F stack = () AND ci = (append w (list c) wˆR)
(define wcwˆr (make-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP) (P ,EMP))
                            ((P a ,EMP) (P (a)))
                            ((P b ,EMP) (P (b)))
                            ((P c ,EMP) (Q ,EMP))
                            ((Q a (a)) (Q ,EMP))
                            ((Q b (b)) (Q ,EMP))
                            ((Q ,EMP ,EMP) (F ,EMP)))))

;; Tests for wcwˆr
(check-equal? (sm-apply wcwˆr '(a)) 'reject)
(check-equal? (sm-apply wcwˆr '(a c)) 'reject)
(check-equal? (sm-apply wcwˆr '(b c a)) 'reject)
(check-equal? (sm-apply wcwˆr '(a a b c b a b)) 'reject)
(check-equal? (sm-apply wcwˆr '(c)) 'accept)
(check-equal? (sm-apply wcwˆr '(a c a)) 'accept)
(check-equal? (sm-apply wcwˆr '(a b b b c b b b a)) 'accept)


;; invariants for wcwˆr

;; word stack → Boolean
;; Purpose: Determine in the given word and stack are empty
(define (S-INV-wcwˆr ci s) (and (empty? ci) (empty? s)))

;; Tests for S-INV-wcwˆr
(check-equal? (S-INV-wcwˆr '() '(a a)) #f)
(check-equal? (S-INV-wcwˆr '(a c a) '()) #f)
(check-equal? (S-INV-wcwˆr '(a c a) '(b b)) #f)
(check-equal? (S-INV-wcwˆr '() '()) #t)

;; word stack → Boolean
;; Purpose: Determine if the given ci is the reverse of
;; the given stack AND c is not in ci
(define (P-INV-wcwˆr ci s)
  (and (equal? ci (reverse s)) (not (member 'c ci))))

;; Tests for P-INV-wcwˆr
(check-equal? (P-INV-wcwˆr '(a c a) '(a c a)) #f)
(check-equal? (P-INV-wcwˆr '(a a) '(a b)) #f)
(check-equal? (P-INV-wcwˆr '() '()) #t)
(check-equal? (P-INV-wcwˆr '(a b) '(b a)) #t)
(check-equal? (P-INV-wcwˆr '(a b a a) '(a a b a)) #t)


;; word stack → Boolean
;; Purpose: Determine if ci=s^Rv^Rcv
(define (Q-INV-wcwˆr ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))
         (v (if (member 'c ci)
                (drop ci (add1 (length w)))
                '()))]
    (and (equal? ci (append w (list 'c) v))
         (equal? w (append (reverse s) (reverse v))))))

;; Tests for Q-INV-wcwˆr
(check-equal? (Q-INV-wcwˆr '(a a) '()) #f)
(check-equal? (Q-INV-wcwˆr '(b b c a) '(b a)) #f)
(check-equal? (Q-INV-wcwˆr '(c) '()) #t)
(check-equal? (Q-INV-wcwˆr '(b a c) '(a b)) #t)
(check-equal? (Q-INV-wcwˆr '(a b c b) '(a)) #t)
(check-equal? (Q-INV-wcwˆr '(a b b c b) '(b a)) #t)


;; word stack → Boolean
;; Purpose: Determine if ci=s^Rv^Rcv AND stack is empty
(define (F-INV-wcwˆr ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))]
    (and (empty? s)
         (equal? ci (append w (list 'c) (reverse w))))))

;; Tests for F-INV-wcwˆr
(check-equal? (F-INV-wcwˆr '() '()) #f)
(check-equal? (F-INV-wcwˆr '(b b) '()) #f)
(check-equal? (F-INV-wcwˆr '(b a c) '(b a)) #f)
(check-equal? (F-INV-wcwˆr '(c) '()) #t)
(check-equal? (F-INV-wcwˆr '(b a c a b) '()) #t)
(check-equal? (F-INV-wcwˆr '(a b b c b b a) '()) #t)




;; Let Σ = {a b}. Design and implement a pda for L = {w | w has an
;; equal number of as and bs}. Follow all the steps of the design recipe.

;; L = {w | w has an equal number of as and bs}
;; States
;;    S: stack contains the number of bs that have been read - the number of as that have been read
;;       OR stack containst the number of as that have been read - the number of bs that have been read
;; The stack is a (listof (a*b*))


(define equal-as-bs (make-ndpda '(S)
                                '(a b)
                                '(a b)
                                'S
                                '(S)
                                `(((S a ,EMP) (S (a)))
                                  ((S b ,EMP) (S (b)))
                                  ((S a (b)) (S ,EMP))
                                  ((S b (a)) (S ,EMP)))))



;; Let Σ = {a b}. Design and implement a pda for L = {w | w is a
;; palindrome}. Follow all the steps of the design recipe.
;; (a palindrome is a word that can be spelled the same forward and backwards

;; basically ww^R

;; States
;;   S: stack is empty AND ci is empty ,start state and final state
;;   A: ci = the stack reversed (w^R
;;   B: ci = (append w m v) AND w = stack^R m v^R
;;   C: stack = empty AND ci is a palindrome (append w w^R), final state 
(define palindrome-pda (make-ndpda '(S A B C)
                                   '(a b)
                                   '(a b)
                                   'S
                                   '(C)
                                   `(((S ,EMP ,EMP) (A ,EMP))
                                     ((A a ,EMP) (A (a)))
                                     ((A b ,EMP) (A (b)))
                                     ((A a ,EMP) (B,EMP))
                                     ((A b ,EMP) (B ,EMP))
                                     ((A ,EMP ,EMP) (B ,EMP))
                                     ((B a (a)) (B ,EMP))
                                     ((B b (b)) (B ,EMP))
                                     ((B ,EMP ,EMP) (C ,EMP)))))

;; tests for palindrome-pda
(check-equal? (sm-apply palindrome-pda '()) 'accept)
(check-equal? (sm-apply palindrome-pda '(a)) 'accept)
(check-equal? (sm-apply palindrome-pda '(a b a)) 'accept)
(check-equal? (sm-apply palindrome-pda '(b a b)) 'accept)
(check-equal? (sm-apply palindrome-pda '(a a b b a a)) 'accept)
(check-equal? (sm-apply palindrome-pda '(a b)) 'reject)
(check-equal? (sm-apply palindrome-pda '(b a)) 'reject)
(check-equal? (sm-apply palindrome-pda '(a b a a)) 'reject)

;; invariants for palindrome-pda

;; invariants also take in the stack now

;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in S
;;          (empty stack AND empty word)
(define (S-INV-PALINDROME-PDA ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for S-INV-PALINDROME-PDA
(check-equal? (S-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (S-INV-PALINDROME-PDA '(a) '()) #f)
(check-equal? (S-INV-PALINDROME-PDA '() '(b)) #f)
(check-equal? (S-INV-PALINDROME-PDA '(a) '(a)) #f)
(check-equal? (S-INV-PALINDROME-PDA '(b b) '(b b)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in A
;;          (ci = stack reversed)
(define (A-INV-PALINDROME-PDA ci stack)
  (equal? ci (reverse stack)))

;; tests for A-INV-PALINDROME-PDA
(check-equal? (A-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (A-INV-PALINDROME-PDA '(a) '(a)) #t)
(check-equal? (A-INV-PALINDROME-PDA '(a b b a) '(a b b a)) #t)
(check-equal? (A-INV-PALINDROME-PDA '(b b) '(b b)) #t)
(check-equal? (A-INV-PALINDROME-PDA '(b a) '(b a)) #f)
(check-equal? (A-INV-PALINDROME-PDA '(a b) '(a b)) #f)
(check-equal? (A-INV-PALINDROME-PDA '(a a a a) '(b b)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;          (ci = (append w v) AND w = stack^R v^R)
(define (B-INV-PALINDROME-PDA ci stack)    
  (or (empty? ci)
      (and (<= (length stack) (length ci))
           (local [(define w (if (even? (length (append ci stack)))
                                 (take ci (/ (length (append ci stack)) 2))         
                                 (take ci (floor (/ (length (append ci stack)) 2)))))
                   
                   (define v (if (even? (length (append ci stack)))
                                 (drop ci (length w))
                                 (drop ci (add1 (length w)))))]
             (equal? w (append (reverse stack) (reverse v)))))))

;; tests for B-INV-PALINDROME-PDA
(check-equal? (B-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a b) '(a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a b b) '(a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(b b a) '(b b)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a b b a b) '(b a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a a a) '(a a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(b) '(b a)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(a a) '(b b)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(b a) '(b b)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(a a) '(b a)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(b) '(b b)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(b b) '(b b b)) #f)


               
;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in C
;;          (stack = empty AND ci = ww^R)
(define (C-INV-PALINDROME-PDA ci stack)
  (and (empty? stack)
       (or (empty? ci)
           (equal? ci (reverse ci)))))

;; tests for C-INV-PALINDROME-PDA
(check-equal? (C-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a a b a a) '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a a a a a a) '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a a a a a) '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a b a b) '()) #f)
(check-equal? (C-INV-PALINDROME-PDA '(a a a b) '(a a b)) #f)


;; PROOF for palindrome-pda

;; The state invariants hold when M accepts w
#|

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. This
guarentees that ci = '() and stack = '(). After using the transition rule,
ci = '() and stack = '(). A-INV holds since the consumed input is the reverse
of the stack. Thus, A-INV holds after the transition rule.

((A a ,EMP) (A (a))): By inductive hypothesis, A-INV holds. This
guarentees that ci is the reverse of the stack. After using the transition rule,
we read an a and push an a onto the stack, making the stack the
reverse of the consumed input, and with this, A-INV holds.
Thus, A-INV holds after the transition rule is used. 

((A b ,EMP) (A (b))): By inductive hypothesis, A-INV holds. This guareentees
that the ci is the reverse of the stack. After using the transition rule, we read
a b and push a b onto the stack. With this, the stack is the reverse of the
consumed input, making A-INV hold. Thus, A-INV holds after the transition
rule is used. 

((A a ,EMP) (B,EMP)): By inductive hypothesis, A-INV holds. This
guarentees that the ci is the reverse of the stack. After using the
transition rule, an a is read and nothing is pushed and popped onto
the stack. B-INV holds since ci = (append w m v) AND w = stack^R m v^R.
In this case, w is the reverse of the stack and v = '(), and m = a, making the
B-INV hold. Thus, B-INV holds after the transition rule. 

((A b ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. This
guarentees that the ci is the reverse of the stack. After using the
transition rule, a b is read and nothing is pushed and popped onto
the stack. B-INV holds since ci = (append w m v) AND w = stack^R m v^R.
ci = (stack^R v) is the reverse of the stack and v = '(), with v being
the letters that were matched with the stack and m = b. Thus, B-INV holds
after the transition rule.

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis , A-INV holds. This
guarentees that the ci = stack^R. After using this transition rule,
the ci = stack^R and the stack remains the same since nothing is pushed
or popped. w = stack^R m v^R and ci = (append w m v), with m = '() and v = '(). 
Thus, B-INV holds after the transition rule is used. 

((B a (a)) (B ,EMP)): By inductive hypothesis, B-INV holds. This
guareentees that ci = (append w m v) AND w = stack^R m v^R. After
using this transition rule, ci = (append w m v) AND w = stack^R m v^R.
Thus, B-INV holds after the transition rule. 

((B b (b)) (B ,EMP)): By inductive hypothesis, B-INV holds. This
guareentees that ci = (append w m v) AND w = stack^R m v^R. After
this transition rule, ci = (append w m v) AND w = stack^R m v^R. Thus,
B-INV holds after the transition rule. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. This
guareentees that ci = (append w m v) AND w = stack^R m v^R. After
this transition rule, ci = (append w m v) AND w = stack^R m v^R. Since
C is a final state, the stack must be empty for the word to be accepted,
and for the word to be accepted, it must be a palindrone. Thus,
C-INV holds after the transition rule. 


|#



;; Let Σ = {a b}. Design and implement a pda for
;; L = {aibj |i≤j≤2i}. Follow all the steps of the design recipe

;; States: 
;;   S: ci = empty AND stack = empty, start and final state
;;   A: ci = a^i AND stack = a^k AND i <= k <= 2i
;;   B: ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i
;;   C: ci = a^ib^j where i≤j≤2i AND stack = empty, final state

(define AiBj (make-ndpda '(S A B C)
                         '(a b)
                         '(a)
                         'S
                         '(C)
                         `(((S a ,EMP) (A (a)))
                           ((S a ,EMP) (A (a a)))
                           ((S ,EMP ,EMP) (A ,EMP))
                           ((A a ,EMP) (A (a)))
                           ((A a ,EMP) (A (a a)))
                           ((A b (a)) (B ,EMP))
                           ((A ,EMP ,EMP) (B ,EMP))
                           ((B b (a)) (B ,EMP))
                           ((B ,EMP ,EMP) (C ,EMP))
                           )))
                         

;; Tests for AiBj
(check-equal? (sm-apply AiBj '(a b)) 'accept)
(check-equal? (sm-apply AiBj '(a a b b b)) 'accept)
(check-equal? (sm-apply AiBj '()) 'accept)
(check-equal? (sm-apply AiBj '(a a a b b b)) 'accept)
(check-equal? (sm-apply AiBj '(a b b)) 'accept)
(check-equal? (sm-apply AiBj '(a a a a a a b)) 'reject)
(check-equal? (sm-apply AiBj '(a b b b)) 'reject)
(check-equal? (sm-apply AiBj '(a)) 'reject)
(check-equal? (sm-apply AiBj '(b)) 'reject)
(check-equal? (sm-apply AiBj '(a a)) 'reject)
(check-equal? (sm-apply AiBj '(a a b)) 'reject)


;; invariants for AiBj

;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in S
;;          (empty stack AND empty word)
(define (S-INV-AiBj ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for S-INV-AiBj
(check-equal? (S-INV-AiBj '() '()) #t)
(check-equal? (S-INV-AiBj '(a) '(a a)) #f)
(check-equal? (S-INV-AiBj '(b) '()) #f)
(check-equal? (S-INV-AiBj '(a a a a) '(a a a a a a a a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in A
;;          ci = a^i AND stack = a^k AND i <= k <= 2i
(define (A-INV-AiBj ci stack)
  (and (andmap (λ (x) (eq? 'a x)) ci)
       (<= (length ci) (length stack) (* 2 (length ci)))))

;; tests for A-INV-AiBj
(check-equal? (A-INV-AiBj '(a) '(a a)) #t)
(check-equal? (A-INV-AiBj '(a a) '(a a a a)) #t)
(check-equal? (A-INV-AiBj '(a a a) '(a a a a a a)) #t)
(check-equal? (A-INV-AiBj '(a a a a) '(a a a a a a a a)) #t)
(check-equal? (A-INV-AiBj '() '()) #t)
(check-equal? (A-INV-AiBj '() '(a)) #f)
(check-equal? (A-INV-AiBj '(a b) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;          (ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i)
(define (B-INV-AiBj ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (equal? (append As Bs) ci)
         (<= (length As) (+ (length Bs) (length stack)) (* 2 (length As)))
         )))

;; tests for B-INV-AiBj
(check-equal? (B-INV-AiBj '(a a b b) '()) #t)
(check-equal? (B-INV-AiBj '(a b) '(a)) #t)
(check-equal? (B-INV-AiBj '(a a a b b b) '(a a a)) #t)
(check-equal? (B-INV-AiBj '(a a b b b) '(a)) #t)
(check-equal? (B-INV-AiBj '(a a b b) '(a a)) #t)
(check-equal? (B-INV-AiBj '(a a b) '(a a a a)) #f)
(check-equal? (B-INV-AiBj '(a a b b b b b b) '()) #f)
(check-equal? (B-INV-AiBj '(a b b b b b b b b b) '()) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in C
;;          (ci = a^ib^j where i≤j≤2i AND stack = empty)
(define (C-INV-AiBj ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (<= (length As) (length Bs) (* 2 (length As)))
         (equal? ci (append As Bs))
         (empty? stack))))

;; tests for C-INV-AiBj
(check-equal? (C-INV-AiBj '(a b) '()) #t)
(check-equal? (C-INV-AiBj '(a b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b b) '()) #t)
(check-equal? (C-INV-AiBj '(a b b b) '()) #f)
(check-equal? (C-INV-AiBj '() '()) #t)
(check-equal? (C-INV-AiBj '(a b b) '(a)) #f)
(check-equal? (C-INV-AiBj '(a b b) '(a a)) #f)


;; PROOF

;; The state invariants hold when M accepts w
#|

When M starts, S-INV holds because ci = '() and s '().
This establishes the base case.
 

Proof invariants hold after each transition that consumes input:

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. This
guareentees that ci = '() and s = '(). After using this rule, ci = '() = a^i
and stack = '() = a^k. A-holds, because i <= k <= 2i.

((S a ,EMP) (A (a))): By inductive hypothesis, S-INV holds.
This guarantees that ci = '() and s = '(). After using this
rule, ci = '(a) = a^i and s = '(a) = a^k. A-INV holds, because ci = a^i
AND stack = '() = a^k. Observe that i <= k <= 2i. Thus, A-INV holds. 

((S a ,EMP) (A (a a))): By inductive hypothesis, S-INV holds. S-INV
guarantees that ci = '() and s = '(). After using this rule, ci = '(a) = a^i
and stack = '(a a) = a^k. A-INV holds, because ci = a^i AND stack = a^k.
Observe that i <= k <= 2i. Thus, A-INV holds. 

((A a ,EMP) (A (a))): By inductive hypothesis, A-INV holds. A-INV
guarantees that ci = a^i AND stack = a^k AND i <= k <= 2i. By reading
an a and pushing an a to the stack, we have ci = '(a^i a) = a^i+1 AND
stack = '(a^k a) = a^k+1. Observe that i+1 <= k+1 <= 2(i+1). 

((A a ,EMP) (A (a a))): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^i AND stack = a^k AND i <= k <= 2i. By reading
an a and pushing 2 as to the stack, we have ci = '(a^i a) AND stack = '(a^k a a)\
= a^k+2. Observe that i+1 <= k+2 <= 2(i+1).

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^i AND stack = a^k AND i <= k <= 2i. Observe that
ci = a^ib^0 = a^ib^j. Since i <= 0 + k <= 2i, we have that i <= j+k <= 2i.
Thus, B-INV holds.

((A b (a)) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that  ci = a^i = a^ib^0 AND stack = a^k AND i <= k <= 2i. By reading
a b and popping an a from the stack, we have ci = a^ib^1 = a^ib^j+1 AND
stack = a^k-1. Since i <= 1 + k-1 <= 2i, we have that i <= j+k <= 2i.
Thus, B-INV holds.

((B b (a)) (B ,EMP)): By inductive hypothesis, B-INV holds. B-INV guarentees that
ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i. By reading a b and popping an a
from the stack, we have ci = a^ib^j+1 AND stack = a^k-1. Since i <= j+1 + k-1 <= 2i,
we have that i <= j+k <= 2i. Thus B-INV holds. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. B-INV guarentees that
ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i. Reading nothing and not changing the
stack means that ci = a^ib^j after the transition. Recall that M is nondetermintistic
and uses such transtion only to move to C (the final state) and accept. This means
that s must be empty and, therefore ci = a^ib^j, where i <= j <= 2i. Thus, C-INV holds. 



PROVING L = L(M)

w∈L ⇔ w∈L(M)

(⇒) Assume w∈L. This means that w = a^ib^j, where i <= j <= 2i. Given that state
invariantsalways hold, the following computation takes place:
(S a^ib^j EMP) -|ˆ∗ (A b^j a^j) -| (B b^k a^k) -|ˆ∗ (C EMP EMP)
Therefore, w∈L(M).

(⇐) Assume w∈L(M). This means that M halts in C, a final state,
with an empty stack having consumed w. Given that the state invariants
always hold, we may conclude that w = a^ib^j, where i <= j <= 2i. Therefore, w∈L


w∈/L ⇔ w∈/L(M)
Proof

(⇒) Assume w∈/L. This means w ≠ aibj, where i <= j <= 2i. Given that
the state invariant predicates always hold, there is no computation
that has M consume w and end in C with an empty stack. Therefore, w∈/L(M).

(⇐) Assume w∈/L(M). This means that M cannot transition into C with an
empty stack having consumed w. Given that the state invariants always hold,
this means that w ≠ aibj, where i <= j <= 2i. Thus, w∈/L.

|#




;; Let Σ = {a b}. Design and implement a pda for L = {a^nb^ma^n |n,m≥0}
;; States:
;;   S: ci = a* = a^n AND stack = a^n, start state
;;   A: ci = a^nb* = a^nb^m AND stack = a^n
;;   B: ci = a^nb^ma^k AND stack = a^n-k, final state
(define A^nB^mA^n (make-ndpda '(S A B)
                              '(a b)
                              '(a)
                              'S
                              '(B)
                              `(((S a ,EMP) (S (a)))
                                ((S ,EMP ,EMP) (A ,EMP))
                                ((S b ,EMP) (A ,EMP))
                                ((A b ,EMP) (A ,EMP))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((A a (a)) (B ,EMP))
                                ((B a (a)) (B ,EMP)))))

;; tests for A^nB^mA^n
(check-equal? (sm-apply A^nB^mA^n '()) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a b a)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(b)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a a b b a a)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a b b b b b b b a)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(b b b b b b b b b)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a a a a b b a a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a a a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(b a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a b)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a a a a a a a b a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a b a b)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a a b a a a)) 'reject)

;; invariants for A^nB^mA^n

;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in S
;;   S: ci = a* = a^n AND stack = a^n, start state
(define (S-INV-A^nB^mA^n ci stack)
  (and (andmap (λ (x) (eq? 'a x)) ci)
       (andmap (λ (x) (eq? 'a x)) stack)
       (= (length stack) (length ci))))

;; tests for S-INV-A^nB^mA^n
(check-equal? (S-INV-A^nB^mA^n '() '()) #t)
(check-equal? (S-INV-A^nB^mA^n '(a) '(a)) #t)
(check-equal? (S-INV-A^nB^mA^n '(a a a) '(a a a)) #t)
(check-equal? (S-INV-A^nB^mA^n '(a a a a) '(a a a a)) #t)
(check-equal? (S-INV-A^nB^mA^n '(a) '()) #f)
(check-equal? (S-INV-A^nB^mA^n '(b) '(a)) #f)
(check-equal? (S-INV-A^nB^mA^n '(a) '(a a)) #f)
(check-equal? (S-INV-A^nB^mA^n '(a b a) '()) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in A
;;   A: ci = a^nb* = a^nb^m AND stack = a^n
(define (A-INV-A^nB^mA^n ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (equal? As stack)
         (equal? (append As Bs) ci))))

;; tests for A-INV-A^nB^mA^n
(check-equal? (A-INV-A^nB^mA^n '(b) '()) #t)
(check-equal? (A-INV-A^nB^mA^n '(a) '(a)) #t)
(check-equal? (A-INV-A^nB^mA^n '(a b b) '(a)) #t)
(check-equal? (A-INV-A^nB^mA^n '(a a a b b b b b) '(a a a)) #t)
(check-equal? (A-INV-A^nB^mA^n '(b b b b b b b b) '()) #t)
(check-equal? (A-INV-A^nB^mA^n '(a a a a a a a b) '(a a a a a a a)) #t)
(check-equal? (A-INV-A^nB^mA^n '() '()) #t)
(check-equal? (A-INV-A^nB^mA^n '(a b a) '(a a)) #f)
(check-equal? (A-INV-A^nB^mA^n '(a b a b) '()) #f)
(check-equal? (A-INV-A^nB^mA^n '(a) '(a a)) #f)
(check-equal? (A-INV-A^nB^mA^n '(a b b a a a) '(a)) #f)
(check-equal? (A-INV-A^nB^mA^n '(a a b b b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;    B: ci = a^nb^ma^k AND stack = a^n-k
(define (B-INV-A^nB^mA^n ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (As-after-Bs (takef (drop ci (length (append As Bs))) (λ (x) (eq? x 'a))))
         (stack-all-as? (andmap (λ (x) (eq? 'a x)) stack))]
    (or (and (equal? ci As)
             (even? (length As))
             stack-all-as?) 
        (and (= (length stack) (- (length As) (length As-after-Bs)))
             stack-all-as?
             (equal? (append As Bs As-after-Bs) ci)))))

  ;; tests for B-INV-A^nB^mA^n
  (check-equal? (B-INV-A^nB^mA^n '() '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(b) '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(a b a) '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(a a) '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(a a a a) '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(a a b b a a) '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(a a a a a b b b a a a a a) '()) #t)
  (check-equal? (B-INV-A^nB^mA^n '(a) '()) #f)
  (check-equal? (B-INV-A^nB^mA^n '(a a a) '()) #f)
  (check-equal? (B-INV-A^nB^mA^n '(b b a) '()) #f)
  (check-equal? (B-INV-A^nB^mA^n '(a a b) '()) #f)
  (check-equal? (B-INV-A^nB^mA^n '(a a a a a) '()) #f)
  (check-equal? (B-INV-A^nB^mA^n '(b a b a) '()) #f)


;; PROOF FOR A^nB^mA^n


;; PROOF

;; The state invariants hold when M accepts w
#|

Proof invariants hold after each transition that consumes input:

When M starts, S-INV holds because ci = '() and s = '().
This establishes the base case.

((S a ,EMP) (S (a))): By inductive hypothesis, S-INV holds.
S-INV guareentees that ci = a* = a^n AND stack = a^n. After
using this rule, ci = a* = a^n+1 AND stack = a^n+1. Observe that
ci = a* = a^n+1 AND stack = a^n+1. Thus, S-INV holds. 

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds.
S-INV guarentees that ci = a* = a^n AND stack = a^n. After using this
rule, ci = a* = a^n AND stack = a^n. A-INV holds since ci = a^nb^0
AND stack = a^n.

((S b ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. S-INV
guarentees that ci = a* = a^n AND stack = a^n. After using this rule,
ci = a^nb^1 and stack = a^n. Observe that ci = a^nb^1 = a^nb* = a^nb^m
and stack = a^n. Thus, A-INV holds. 

((A b ,EMP) (A ,EMP)): By inductive hypothesis A-INV holds. A-INV
guarentees that ci = a^nb* = a^nb^m AND stack = a^n. After using this rule,
ci = a^nb^m+1 and stack = a^n. Observe that we have ci = a^nb^m+1 =
a^nb* and stack = a^n. Thus, A-INV holds. 

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^nb* = a^nb^m and stack = a^n. After using this
rule, ci = a^nb^m AND stack = a^n. Observe that ci = a^nb^ma^0 = a^nb^ma^k
AND stack = a^n-k = a^n-0. Thus, B-INV holds. 

((A a (a)) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^nb* = a^nb^m AND stack = a^n. After using this
rule, ci = a^nb^ma^1 = a^nb^ma^k AND stack = a^n-k. Thus, B-INV holds. 

((B a (a)) (B ,EMP))))): By inductive hypothesis, B-INV holds. B-INV
guarentees that ci = a^nb^ma^k AND stack = a^n-k. After using this rule,
ci = a^nb^ma^k+1 AND stack = a^n-(k+1). Thus, B-INV holds. 


PROVING L = L(M)

w∈L ⇔ w∈L(M)

(⇒) Assume w∈L. This means that w = a^nb^ma^n, where n, m => 0.
Given that state invariants always hold, the following computation
takes place:
(S a^nb^ma^n '()) |-* (S b^ma^n a^n) |-* (A a^n a^n) |-* (B ,EMP ,EMP)
Therefore, w∈L(M).

(⇐) Assume w∈L(M). This means that M halts in B, a final state,
with an empty stack having consumed w. Given that the state invariants
always hold, we may conclude that w = a^nb^ma^n, where n, m => 0.
Therefore, w∈L


w∈/L ⇔ w∈/L(M)
Proof

(⇒) Assume w∈/L. This means w ≠ a^nb^ma^n, where n,m => 0. Given that
the state invariant predicates always hold, there is no computation
that has M consume w and end in B with an empty stack. Therefore, w∈/L(M).

(⇐) Assume w∈/L(M). This means that M cannot transition into B with an
empty stack having consumed w. Given that the state invariants always hold,
this means that w ≠ a^nb^ma^n, where n,m => 0. nThus, w∈/L.

|#



;; Let Σ = {a b}. Design and implement a pda for L = {}.
;; States:
;;   S: (a*b*)*, start state
(define accept-nothing (make-ndpda '(S)
                                   '(a b)
                                   '()
                                   'S
                                   '()
                                   `(((S a ,EMP) (S ,EMP))
                                     ((S b ,EMP) (S ,EMP)))))

;; tests for accept-nothing
(check-equal? (sm-apply accept-nothing '()) 'reject)
(check-equal? (sm-apply accept-nothing '(a)) 'reject)
(check-equal? (sm-apply accept-nothing '(b)) 'reject)
(check-equal? (sm-apply accept-nothing '(a b a)) 'reject)
(check-equal? (sm-apply accept-nothing '(b b b)) 'reject)
(check-equal? (sm-apply accept-nothing '(a a a)) 'reject)
(check-equal? (sm-apply accept-nothing '(a b a b a b a b a b)) 'reject)
(check-equal? (sm-apply accept-nothing '(a a b b a a b b)) 'reject)
(check-equal? (sm-apply accept-nothing '(a a b a a b)) 'reject)
(check-equal? (sm-apply accept-nothing '(a b a b b b a a a b)) 'reject)
(check-equal? (sm-apply accept-nothing '(a b a b b b b b a a a a b)) 'reject)

;; invariants for accept-nothing

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
(define (S-INV-accept-nothing ci stack)
  #t)

;; tests for S-INV-accept-nothing
(check-equal? (S-INV-accept-nothing '() '()) #t)
(check-equal? (S-INV-accept-nothing '(a) '()) #t)
(check-equal? (S-INV-accept-nothing '(b) '()) #t)
(check-equal? (S-INV-accept-nothing '(a a) '()) #t)
(check-equal? (S-INV-accept-nothing '(b b) '()) #t)
(check-equal? (S-INV-accept-nothing '(a b) '()) #t)
(check-equal? (S-INV-accept-nothing '(b a) '()) #t)
(check-equal? (S-INV-accept-nothing '(a a a b b b) '()) #t)
(check-equal? (S-INV-accept-nothing '(a b a b a b a) '()) #t)
(check-equal? (S-INV-accept-nothing '(a a b b a a b b) '()) #t)
(check-equal? (S-INV-accept-nothing '(a a b b b b b a a a) '()) #t)


;; Proof for accept-nothing

#|
;; the state invariants hold when M accepts w

IT DOESN'T ACCEPT ANYTHING THO??????????????



|#



;; Let Σ = {a b c d}. Design and implement a pda for
;;   L={a^mb^nc^pd^q:m,n,p,q≥0 ∧ m+n=p+q}. Follow all the steps of
;;   the design recipe
;; States:
;;  S: ci = a* = a^m AND stack = a^m, start state
;;  A: ci = a^mb* = a^mb^n AND stack = a^m+n
;;  B: ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p
;;  C: ci = a^mb^nc^pd* = a^mb^nc^pd^q AND stack = a^m+n-p-q, final state
(define a^mb^nc^pd^q (make-ndpda '(S A B C)
                                 '(a b c d)
                                 '(a)
                                 'S
                                 '(C)
                                 `(((S a ,EMP) (S (a)))
                                   ((S ,EMP ,EMP) (A ,EMP))
                                   ((A b ,EMP) (A (a)))
                                   ((A ,EMP ,EMP) (B ,EMP))
                                   ((B c (a)) (B ,EMP))
                                   ((B ,EMP ,EMP) (C ,EMP))
                                   ((C d (a)) (C ,EMP)))))

;; tests for a^mb^nc^pd^q
(check-equal? (sm-apply a^mb^nc^pd^q '()) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a b c d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(b b d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(b b c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a b c d d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a b b d d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(b b b c d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(b)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(c)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(d)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a a c)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a a b b c d)) 'reject)

;; invariants for a^mb^nc^pd^q                                   

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
;;  (ci = a* = a^m AND stack = a^m)
(define (S-INV-a^mb^nc^pd^q ci stack)
  (and (andmap (λ (x) (eq? x 'a)) ci)
       (andmap (λ (x) (eq? x 'a)) stack)
       (equal? ci stack)))

;; tests for S-INV-a^mb^nc^pd^q
(check-equal? (S-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a) '(a a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a a a) '(a a a a a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a a a a a a) '(a a a a a a a a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a) '()) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '() '(a)) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a) '(a)) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a) '(a a)) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a) '()) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a a) '(a a a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
;;  (ci = a^mb* = a^mb^n AND stack = a^m+n)
(define (A-INV-a^mb^nc^pd^q ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (equal? (append As Bs) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= (length stack) (length ci)))))
  
  
;; tests for A-INV-a^mb^nc^pd^q
(check-equal? (A-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(b) '(a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a b) '(a a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a a a b b) '(a a a a a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a) '()) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(b) '()) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(a a) '(a)) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(b b) '()) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(a b a) '(a a a)) #f)

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
;;  (ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p)
(define (B-INV-a^mb^nc^pd^q ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? x 'c))))]
    (and (equal? (append As Bs Cs) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= (length stack) (- (+ (length As) (length Bs)) (length Cs))))))
  
  
;; tests for B-INV-a^mb^nc^pd^q
(check-equal? (B-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(b) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a c) '()) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(b c) '()) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a b c) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a b b) '(a a a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(c) '(a)) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(a d) '()) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(b d) '()) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(a c c) '()) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(b b c) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
;;  (ci = a^mb^nc^pd* = a^mb^nc^pd^q AND stack = a^m+n-p-q)
(define (C-INV-a^mb^nc^pd^q ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? x 'c))))
         (Ds (takef (drop ci (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? x 'd))))]
    (and (equal? (append As Bs Cs Ds) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= (length stack) (- (+ (length As) (length Bs)) (length Cs) (length Ds))))))
  
  
;; tests for C-INV-a^mb^nc^pd^q
(check-equal? (C-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a c) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a d) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(b c) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(b d) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a a a d) '(a a)) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a a b c c d) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a) '(a a)) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(b b b) '(a a a a)) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(a b c c c) '()) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(a a a a d) '(a)) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(d c b a) '()) #f)

;; proof for a^mb^nc^pd^q

;; PROOF

;; The state invariants hold when M accepts w
#|

Proof invariants hold after each transition:

When M starts, S-INV holds because ci = '() and s = '().
This establishes the base case.

((S a ,EMP) (S (a))): By inductive hypothesis, S-INV holds. S-INV
guarentees that ci = a* = a^m AND stack = a^m. After using this rule,
ci = a^m+1 = a* AND stack = a^m+1. Thus, S-INV holds.

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. S-INV
guarentees that ci = a* = a^m AND stack = a^m. After using this rule,
ci = a^m = a^mb^0 = a^mb^n AND stack = a^m = a^m+n. Thus, A-INV holds.

((A b ,EMP) (A (a))): By inductive hypothesis, A-INV holds. A-inv
guarentees that ci = a^mb* = a^mb^n AND stack = a^m+n. After using
this rule, ci = a^mb^n+1 = a^mb* AND stack = a^m+n+1. Thus,
A-INV holds.

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^mb* = a^mb^n AND stack = a^m+n. After using this
rule, ci = a^mb^n = a^mb^nc^0 = a^mb^nc^p AND stack = a^m+n-p.
Thus, B-INV holds. 

((B c (a)) (B ,EMP)): By inductive hypothesis, B-INV holds. B-INV
guarentees that ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p. After
using this rule, ci = a^mb^nc^p+1 AND stack = a^m+n-(p+1). Thus, B-INV
holds. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. B-INV
guarentees that ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p. After using
this rule, ci = a^mb^nc^pd^0 = a^mb^nc^pd^q AND stack = a^m+n-p-q. Thus,
C-INV holds. 

((C d (a)) (C ,EMP)): By inductive hypothesis, C-INV holds. C-INV guarentees
that ci = a^mb^nc^pd* = a^mb^nc^pd^q AND stack = a^m+n-p-q. After using
this rule, ci = a^mb^nc^pd^q+1 AND stack = a^m+n-p-(q+1). Thus, C-INV holds. 

|#


#|
 word in language & word is in language of the machine

w∈L -> w∈L(M):

Assume that w∈L. This means that w = a^mb^nc^pd^q, where m+n = p+q.
Given that the state invariants always hold, the following computation
takes place:

(S a^mb^nc^pd^q '()) |-* (S b^nc^pd^q a^m) |- (A c^pd^q a^m+n)
|-* (B d^q a^m+n-p) |-* (C '() a^m+n-p-q)

Therefore, w∈L(M).

w∈L(M) -> w∈L:

Assume w∈L(M). This means that M halts in C, a final state, with an empty
stack and the word being entirely consumed. Given that the state invariants
always hold, we can conclude that w = a^mb^nc^pd^q, where m+n= p+q. Thus,
can can conclude w∈L.


|#






;; Let Σ = {a b}. Design and implement a pda for L = {w | w has 3
;;   times as many as than b}. Follow all the steps of the design recipe

;; States:
;;   S: ci = amount of as read divided by 3 is 0
;;   A: amount of as read divided by 3 is 1
;;   B: read a b and as read, when divided by 3, is 0
;;   C: amount of as read divided by 3 is 2
(define 3xa-b (make-ndpda '(S A B C)
                          '(a b)
                          '(a b)
                          'S
                          '(S)
                          `(((S a ,EMP) (A ,EMP))
                            ((S a (a)) (A ,EMP))
                            ((S b (b)) (S ,EMP))
                            ((S b ,EMP) (B (a a a)))
                            ((A b (b)) (A ,EMP))
                            ((A b ,EMP) (A (a a a)))
                            ((A a (a)) (C ,EMP))
                            ((A a ,EMP) (C ,EMP))
                            ((B b (b)) (B ,EMP))
                            ((B b ,EMP) (B (a a a)))
                            ((B a (a)) (A ,EMP))
                            ((C b (b)) (C ,EMP))
                            ((C b ,EMP) (C (a a a)))
                            ((C a ,EMP) (S (b)))
                            ((C a (a)) (S ,EMP)))))
#|

;; tests for 3xa-b
(check-equal? (sm-apply 3xa-b '() '()) 'accept)
(check-equal? (sm-apply 3xa-b '(b a a a) '()) 'accept)
(check-equal? (sm-apply 3xa-b '(a a a b) '()) 'accept)
(check-equal? (sm-apply 3xa-b '(b a a b a a a a) '()) 'accept)
(check-equal? (sm-apply 3xa-b '(a b a a) '()) 'accept)
(check-equal? (sm-apply 3xa-b '(a a b b a a b a a a a a) '()) 'accept)
(check-equal? (sm-apply 3xa-b '(a) '()) 'reject)
(check-equal? (sm-apply 3xa-b '(b) '()) 'reject)


HAVE TO COME BACK TO THIS!!!!!!


|#



;; Let Σ={abc}. Design and implement a pda for
;; L = {a^mb^nc^p : m,n,p≥0 ∧ (m = n ∨ n = p)}
;; Follow all the steps of the design recipe

;; States:
;;  S: ci = '() AND stack = '(), start state
;;  A: ci = a^m AND stack = a^m
;;  B: ci = a^mb^n AND stack a^m-n
;;  C: ci = a^mb^nc^p AND stack = a^m-n, final state
;;  D: ci = a^m AND stack = '()
;;  E: ci = a^mb^n AND stack = a^n
;;  F: ci = a^mb^nc^p AND stack = a^n-p, final state
(define a^mb^nc^p (make-ndpda '(S A B C D E F)
                              '(a b c)
                              '(a)
                              'S
                              '(C F)
                              `(((S ,EMP ,EMP) (A ,EMP))
                                ((S ,EMP ,EMP) (D ,EMP))
                                ((A a ,EMP) (A (a)))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((B b (a)) (B ,EMP))
                                ((B ,EMP ,EMP) (C ,EMP))
                                ((C c ,EMP) (C ,EMP))
                                ((D a ,EMP) (D ,EMP))
                                ((D ,EMP ,EMP) (E ,EMP))
                                ((E b ,EMP) (E (a)))
                                ((E ,EMP ,EMP) (F ,EMP))
                                ((F c (a)) (F ,EMP)))))

;; tests for a^mb^nc^p
(check-equal? (sm-apply a^mb^nc^p '()) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a b)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(b c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a b c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a a b b c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a b b c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a a a b b b)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(b b b c c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a c)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(b)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(b b b)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(a a b c c)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(a b b b c c)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(a a a a a a b)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(b b b b b b c)) 'reject)


;; invariants for a^mb^nc^p

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
;;  (ci = '() AND stack = '())
(define (S-INV-a^mb^nc^p ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for S-INV-a^mb^nc^p
(check-equal? (S-INV-a^mb^nc^p '() '()) #t)
(check-equal? (S-INV-a^mb^nc^p '(a a) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(a a a) '(a)) #f)
(check-equal? (S-INV-a^mb^nc^p '(a b c) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(b) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(c) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(a b) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(b c) '()) #f)
 

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
;;  (ci = a^m AND stack = a^m)
(define (A-INV-a^mb^nc^p ci stack)
  (and (andmap (λ (x) (eq? x 'a)) ci)
       (andmap (λ (x) (eq? x 'a)) stack)
       (= (length ci) (length stack))))

;; tests for A-INV-a^mb^nc^p
(check-equal? (A-INV-a^mb^nc^p '() '()) #t)
(check-equal? (A-INV-a^mb^nc^p '(a) '(a)) #t)
(check-equal? (A-INV-a^mb^nc^p '(a a a a) '(a a a a)) #t)
(check-equal? (A-INV-a^mb^nc^p '(a a a a a) '(a a a a a)) #t)
(check-equal? (A-INV-a^mb^nc^p '(b) '()) #f)
(check-equal? (A-INV-a^mb^nc^p '(c) '()) #f)
(check-equal? (A-INV-a^mb^nc^p '(a b) '(a)) #f)
(check-equal? (A-INV-a^mb^nc^p '(a b b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
;;  (ci = a^mb^n AND stack a^m-n)
(define (B-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))]
  (and (andmap (λ (x) (eq? x 'a)) stack)
       (equal? ci (append As Bs))
       (= (length stack) (- (length As) (length Bs))))))

;; tests for B-INV-a^mb^nc^p
(check-equal? (B-INV-a^mb^nc^p '() '()) #t)
(check-equal? (B-INV-a^mb^nc^p '(a) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^p '(a b) '()) #t)
(check-equal? (B-INV-a^mb^nc^p '(a a b) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^p '(a a b b) '()) #t)
(check-equal? (B-INV-a^mb^nc^p '(b) '()) #f)
(check-equal? (B-INV-a^mb^nc^p '(a) '(a a)) #f)
(check-equal? (B-INV-a^mb^nc^p '(b) '(a)) #f)
(check-equal? (B-INV-a^mb^nc^p '(a a b b b) '()) #f)
(check-equal? (B-INV-a^mb^nc^p '(b b b a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
;;  (ci = a^mb^nc^p AND stack = a^m-n)
(define (C-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? 'c x))))]
  (and (andmap (λ (x) (eq? x 'a)) stack)
       (equal? ci (append As Bs Cs))
       (= (length stack) (- (length As) (length Bs))))))

;; tests for C-INV-a^mb^nc^p
(check-equal? (C-INV-a^mb^nc^p '() '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(c) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a b) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a b c) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a a b b c c) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a c) '()) #f)
(check-equal? (C-INV-a^mb^nc^p '(b c) '()) #f)
(check-equal? (C-INV-a^mb^nc^p '(b a) '()) #f)
(check-equal? (C-INV-a^mb^nc^p '(a b) '(a)) #f)
(check-equal? (C-INV-a^mb^nc^p '(a a a a a b b b b b c c) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in D
;;  (ci = a^m AND stack = '())
(define (D-INV-a^mb^nc^p ci stack)
  (and (andmap (λ (x) (eq? x 'a)) ci)
       (empty? stack)))

;; tests for D-INV-a^mb^nc^p
(check-equal? (D-INV-a^mb^nc^p '() '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a) '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a a a a) '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a a a) '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a b c) '()) #f)
(check-equal? (D-INV-a^mb^nc^p '(a b) '()) #f)
(check-equal? (D-INV-a^mb^nc^p '(a a a a b) '()) #f)
(check-equal? (D-INV-a^mb^nc^p '(a) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
;;  (ci = a^mb^n AND stack = a^n)
(define (E-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))]
  (and (andmap (λ (x) (eq? x 'a)) stack)
       (equal? ci (append As Bs))
       (= (length stack) (length Bs)))))

;; tests for E-INV-a^mb^nc^p
(check-equal? (E-INV-a^mb^nc^p '() '()) #t)
(check-equal? (E-INV-a^mb^nc^p '(b) '(a)) #t)
(check-equal? (E-INV-a^mb^nc^p '(a b) '(a)) #t)
(check-equal? (E-INV-a^mb^nc^p '(b b b b) '(a a a a)) #t)
(check-equal? (E-INV-a^mb^nc^p '(a) '()) #t)
(check-equal? (E-INV-a^mb^nc^p '(a) '(a)) #f)
(check-equal? (E-INV-a^mb^nc^p '(b b) '(a)) #f)
(check-equal? (E-INV-a^mb^nc^p '(a b c) '()) #f)
(check-equal? (E-INV-a^mb^nc^p '(b b b c) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
;;  (ci = a^mb^nc^p AND stack = a^n-p)
(define (F-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? 'c x))))]
  (and (andmap (λ (x) (eq? x 'a)) stack)
       (equal? ci (append As Bs Cs))
       (= (length stack) (- (length Bs) (length Cs))))))

;; tests for F-INV-a^mb^nc^p
(check-equal? (F-INV-a^mb^nc^p '() '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(a) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(b c) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(b) '(a)) #t)
(check-equal? (F-INV-a^mb^nc^p '(a b c) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(a b b c c) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(a a a) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(b b b b c c) '(a a)) #t)
(check-equal? (F-INV-a^mb^nc^p '(a b c c) '()) #f)
(check-equal? (F-INV-a^mb^nc^p '(a a b b c) '()) #f)
(check-equal? (F-INV-a^mb^nc^p '(a a a b c) '(a)) #f)
(check-equal? (F-INV-a^mb^nc^p '(b b b b b c) '(a a a a a a a a)) #f)
(check-equal? (F-INV-a^mb^nc^p '(a b b b b) '()) #f)


;; PROOF




;; PROOF

;; The state invariants hold when M accepts w
#|

Proof invariants hold after each transition that consumes input:

When M starts, S-INV holds because ci = '() and s = '().
This establishes the base case.

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds.
S-INV guareentees that ci = '() AND stack = '(). After using this rule,
ci = '() AND stack = '(). Observe that ci = a^0 = a^m AND stack = a^m.
Thus, A-INV holds. 

((S ,EMP ,EMP) (D ,EMP)): By inductive hypothesis, S-INV holds.
S-INV guareentees that ci = '() AND stack = '(). After using this rule,
ci = '() AND stack = '(). Observe that ci = a^0 = a^m AND stack = '().
Thus, D-INV holds. 

((A a ,EMP) (A (a))): By inductive hypothesis, A-INV holds.
A-INV guareentees that ci = a^m AND stack = a^m. After using this
rule, ci = a^m+1 AND stack = a^m+1. Thus, A-INV holds.

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds.
A-INV guareentees that ci = a^m AND stack = a^m. After using this
rule, ci = a^m = a^mb^0 = a^mb^n AND stack = a^m = a^m-n.
Thus, B-INV holds. 

((B b (a)) (B ,EMP)): By inductive hypothesis, B-INV holds.
B-INV guareentees that ci = a^mb^n AND stack = a^m-n. After using
this rule, ci = a^mb^n+1 AND stack = a^m-(n+1). Thus, B-INV holds. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. 
B-INV guareentees that ci = a^mb^n AND stack = a^m-n. After using
this rule, ci = a^mb^nc^0 = a^mb^nc^p AND stack = a^m-n. Thus,
C-INV holds. 

((C c ,EMP) (C ,EMP)): By inductive hypothesis, C-INV holds.
C-INV guareentees that ci = a^mb^nc^p AND stack = a^m-n. After
using this rule, ci = a^mb^nc^p+1 AND stack = a^m-n. Thus,
C-INV holds.

((D a ,EMP) (D ,EMP)): By inductive hypothesis, D-INV holds.
D-INV guareentees that ci = a^m AND stack = '(). After using this
rule, ci = a^m+1 AND stack = '(). Thus, D-INV holds. 

((D ,EMP ,EMP) (E ,EMP)): By inductive hypothesis, D-INV holds.
D-INV guareentees that ci = a^m AND stack = '(). After using this
rule, ci = a^mb^0 = a^mb^n AND stack = a^n. Thus, E-INV holds. 

((E b ,EMP) (E (a))): By inductive hypothesis, E-INV holds.
E-INV guareentees that ci = a^mb^n AND stack = a^n. After using
this rule, ci = a^mb^n+1 AND stack = a^n+1. Thus, E-INV holds.

((E ,EMP ,EMP) (F ,EMP)): By inductive hypothesis, E-INV holds.
E-INV guareentees that ci = a^mb^n AND stack = a^n. After using
this rule, ci = a^mb^nc^0 = a^mb^nc^p AND stack = a^n-p.
Thus F-INV holds.

((F c (a)) (F ,EMP)): BY inductive hypothesis, F-INV holds.
F-INV guareentees that ci = a^mb^nc^p AND stack = a^n-p.
After using this rule, ci= a^mb^nc^p+1 AND stack = a^n-(p+1).
Thus F-INV holds. 


PROVING L = L(M)

w∈L ⇔ w∈L(M)

(⇒) Assume w∈L. This means that w = a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p).
Given that state invariants always hold, this means that M consumes all its input in
either C or F. Since C and F are final states, w∈L(M).

(⇐) Assume w∈L(M). This means that M halts in C or F, final states,
with an empty stack having consumed w. Given that the state invariants
always hold, we may conclude that w = a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p).
Therefore, w∈L


w∈/L ⇔ w∈/L(M)
Proof

(⇒) Assume w∈/L. This means w ≠ a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p).
Given that the state invariant predicates always hold, there is no computation
that has M consume w and end in C or F with an empty stack. Therefore, w∈/L(M).

(⇐) Assume w∈/L(M). This means that M cannot transition into C or F with an
empty stack having consumed w. Given that the state invariants always hold,
this means that w ≠ a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p). Thus, w∈/L.

|#




