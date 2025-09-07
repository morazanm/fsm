#lang racket
(require racket/list
         rackunit
         "sm-test-invs-wip.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-apply.rkt"
         "../fsm-core/private/constants.rkt"
         "../sm-graph.rkt"
         "../fsm-core/private/sm-getters.rkt")

(struct test-case (name num-tests thunk))



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
  (make-unchecked-dfa
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

;(check-equal? (S-INV '(b)) #t)
;(check-equal? (S-INV '(a b)) #t)
;(check-equal? (S-INV '(a b a b)) #t)
;(check-equal? (S-INV '(a a b b)) #f)
;(check-equal? (S-INV '(a)) #f)
#;(sm-test-invs NO-AA (list 'S S-INV)
                (list 'A A-INV)
                (list 'B B-INV)
                (list 'R R-INV))
#;(check-equal? (length (first (sm-test-invs NO-AA (list 'S S-INV)
                                             (list 'A A-INV)
                                             (list 'B B-INV)
                                             (list 'R R-INV)))) 19)
        
;(check-equal? (sm-apply NO-AA '()) 'accept)
(check-equal? (sm-apply NO-AA '(a)) 'accept)
(check-equal? (sm-apply NO-AA '(a b)) 'accept)
(check-equal? (sm-apply NO-AA '(a b a)) 'accept)
(check-equal? (sm-apply NO-AA '(b b)) 'accept)
(check-equal? (sm-apply NO-AA '(a a)) 'reject)
(check-equal? (sm-apply NO-AA '(a b a a)) 'reject)
(check-equal? (sm-apply NO-AA '(a b b a a b)) 'reject)

(define (DEAD-INV ci)
  #true)


(define LOI1 (list (list 'S S-INV)
                   (list 'A A-INV)
                   (list 'B B-INV)
                   (list 'R R-INV)))



;; making a dfa with unreachable states
;; C & D are unreachable from the starting config

(define NO-AA-WITH-UNREACHABLE-STATES (make-unchecked-dfa
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



(define DNA-SEQUENCE (make-unchecked-dfa '(K H F M I D B S R)
                                         '(a t c g)
                                         'K
                                         '(K F I B R)
                                         `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                                   (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                                   (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                                   (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

(define EVIL-dna-sequence (complement-fsa DNA-SEQUENCE))


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

(define DNA-INVS (list (list 'K DNA-K-INV)
                       (list 'H DNA-H-INV)
                       (list 'F DNA-F-INV)
                       (list 'M DNA-M-INV)
                       (list 'I DNA-I-INV)
                       (list 'D DNA-D-INV)
                       (list 'B DNA-B-INV)
                       (list 'S DNA-S-INV)
                       (list 'R DNA-R-INV)))



#;(time (sm-test-invs EVIL-dna-sequence (list 'K DNA-K-INV)
                      (list 'H DNA-H-INV)
                      (list 'F DNA-F-INV)
                      (list 'M DNA-M-INV)
                      (list 'I DNA-I-INV)
                      (list 'D DNA-D-INV)
                      (list 'B DNA-B-INV)
                      (list 'S DNA-S-INV)
                      (list 'R DNA-R-INV)))


;; Let Σ = {a b}. Design and implement a dfa for the following language:
;; L = {w | w has an even number of b}

;; L = {w | w does not contain aa}
;; States
;; S: even number of bs, start and final state
;; F: odd number of bs

(define EVEN-NUM-Bs
  (make-unchecked-dfa '(S F)
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

(define CONTAINS-aabab (make-unchecked-dfa '(S A B C D E)
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
  (make-unchecked-dfa '(S A B C D E)
                      '(a b)
                      'S
                      '(E)
                      '((S a A) (S b S) (A a B) (A b S)
                                (B a B) (B b C) (C a D) (C b S)
                                (D a B) (D b E) (E a E) (E b E))
                      'no-dead))

(define BUGGY-CONTAINS-aabab 
  (make-unchecked-dfa '(S A B C D E)
                      '(a b)
                      'S
                      '(E)
                      '((S a A) (S b S) (A a B) (A b S)
                                (B a B) (B b C) (C a D) (C b S)
                                (D a A) #;(D a B) (D b E) (E a E) (E b E))
                      'no-dead))

(check-equal? (sm-apply CONTAINS-aabab  '(a a b a b)) 'accept)
(check-equal? (sm-apply CONTAINS-aabab  '(b b a a a b a b b)) 'accept)
(check-equal? (sm-apply CONTAINS-aabab  '()) 'reject)
(check-equal? (sm-apply CONTAINS-aabab  '(b b a a a b a)) 'reject)
(check-equal? (sm-apply CONTAINS-aabab  '(a a b a a)) 'reject)
       
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

(define LOI-aabab-marco (list (list 'S S2-INV) (list 'A A2-INV)
                              (list 'B B2-INV) (list 'C C2-INV)
                              (list 'D D2-INV) (list 'E E2-INV)))


#;(define RES (sm-test-invs CONTAINS-aabab
                            (list 'S S2-INV)
                            (list 'A A2-INV)
                            (list 'B B2-INV)
                            (list 'C C2-INV)
                            (list 'D D2-INV)
                            (list 'E E2-INV)))

#;(define RES-WORDS (sm-all-possible-words CONTAINS-aabab
                                           (list (list 'S S2-INV)
                                                 (list 'A A2-INV)
                                                 (list 'B B2-INV)
                                                 (list 'C C2-INV)
                                                 (list 'D D2-INV)
                                                 (list 'E E2-INV))))

#;(define TOTAL-WORDS (foldl (λ (pair acc) (+ (length (second pair)) acc))
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
(define no-contain-bababa (make-unchecked-dfa '(S A B C D E F)
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


(define LOI-no-bababa (list (list 'S S-INV-no-contain-bababa)
                            (list 'A A-INV-no-contain-bababa)
                            (list 'B B-INV-no-contain-bababa)
                            (list 'C C-INV-no-contain-bababa)
                            (list 'D D-INV-no-contain-bababa)
                            (list 'E E-INV-no-contain-bababa)
                            (list 'F F-INV-no-contain-bababa)))






; L = a+b+c+a+b+c+

(define a+b+c+a+b+ (make-unchecked-dfa '(S A B C D E)
                                       '(a b c)
                                       'S
                                       '(E)
                                       '((S a A)
                                         (A a A)
                                         (A b B)
                                         (B b B)
                                         (B c C)
                                         (C c C)
                                         (C a D)
                                         (D a D)
                                         (D b E)
                                         (E b E))))

(define (INVS=T ci)
  #true)

(define LOI-a+b+c+a+b+ (list (list 'S INVS=T)
                             (list 'A INVS=T)
                             (list 'B INVS=T)
                             (list 'C INVS=T)
                             (list 'D INVS=T)
                             (list 'E INVS=T)))



; L = contains a+b+c+a+

(define contains-abca (make-unchecked-dfa '(S A B C D)
                                          '(a b c)
                                          'S
                                          '(D)
                                          '((S a A)
                                            (S b S)
                                            (S c S)
                                            (A a A)
                                            (A b B)
                                            (A c S)
                                            (B b S)
                                            (B a A)
                                            (B c C)
                                            (C a D)
                                            (C b S)
                                            (C c S)
                                            (D a D)
                                            (D b D)
                                            (D c D))
                                          'no-dead))


(define LOI-contains-a+b+c+a+b+ (list (list 'S INVS=T)
                                      (list 'A INVS=T)
                                      (list 'B INVS=T)
                                      (list 'C INVS=T)
                                      (list 'D INVS=T)
                                      (list 'E INVS=T)))
                                      

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


(define ONE-LETTER-MISSING (make-unchecked-ndfa '(S A B C D E F)
                                                '(a b c)
                                                'S
                                                '(B C E)
                                                `((S a A) (S b D) (S c F)
                                                          (A a A) (A b B) (A c C)
                                                          (B a B) (B b B)
                                                          (C a C) (C c C)
                                                          (D b D) (D c E) (D a B)
                                                          (E b E) (E c E)
                                                          (F c F) (F b E) (F a C))))

#|
#:accepts (list '(a c) '(b a) '(c a) '(a b) '(a b b) '(a b a a)
                '(b a b b) '(c a c a))
 #:rejects (list '(a) '(b) '(c) '(a b c) '(a a b b c c)
                 '(a b c a b c) '(b a c a) '())
|#

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


(define LOI-ONE-MISSING (list (list 'S S-INV-1-MISSING)
                              (list 'A A-INV-1-MISSING)
                              (list 'B B-INV-1-MISSING)
                              (list 'C C-INV-1-MISSING)
                              (list 'D D-INV-1-MISSING)
                              (list 'E E-INV-1-MISSING)
                              (list 'F F-INV-1-MISSING)))




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

(define ab*b*Uab* (make-unchecked-ndfa '(S A B C D E)
                                       '(a b)
                                       'S
                                       '(S B C E)
                                       `((S ,EMP A) (S ,EMP C) (A a B) (B b B)
                                                    (C a D) (C b E) (D b C) (E b E))))

#|

#:accepts (list '(a b a b a b) '(a) '(a b) '(b) '(a b a b b b) '(a b b b b))
#:rejects (list '(a a) '(a b a) '(a b a a))

|#


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
(define aa*Uab* (make-unchecked-ndfa '(K B D)
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


(define LOI-aa*-ab* (list (list 'K aa-ab-K-INV)
                          (list 'B aa-ab-B-INV)
                          (list 'D aa-ab-D-INV)))



(define rnd-ndfa (make-unchecked-ndfa '(S A B C D E F G H I)
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

(define EX-NDFA (make-unchecked-ndfa '(S A B C)
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


(define EX-NDFA-LOI (list (list 'S S-INV-EX-NDFA)
                          (list 'A A-INV-EX-NDFA)
                          (list 'B B-INV-EX-NDFA)
                          (list 'C C-INV-EX-NDFA)))

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

(define EVEN-B-OR-A*-THEN-MULTIPLE-OF-3-C (make-unchecked-ndfa '(S A B C D E F)
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
  (make-unchecked-ndfa '(S A B C D E F)
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

#;(define RES2 (sm-test-invs M3
                             (list 'S S3-INV)
                             (list 'A A3-INV)
                             (list 'B B3-INV)
                             (list 'C C3-INV)
                             (list 'D D3-INV)
                             (list 'E E3-INV)
                             (list 'F F3-INV)))

#;(define RES2-WORDS (sm-all-possible-words M3
                                            (list (list 'S S3-INV)
                                                  (list 'A A3-INV)
                                                  (list 'B B3-INV)
                                                  (list 'C C3-INV)
                                                  (list 'D D3-INV)
                                                  (list 'E E3-INV)
                                                  (list 'F F3-INV))))

#;(define TOTAL-WORDS2 (foldl (λ (pair acc) (+ (length (second pair)) acc))
                              0
                              RES2-WORDS))



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
                                             `((S ,EMP A)
                                               (S ,EMP B)
                                               (S ,EMP C)
                                               (S ,EMP D)
                                               (A c A)
                                               (A ,EMP G)
                                               (B d B)
                                               (B ,EMP G)
                                               (C a C)
                                               (C ,EMP E)
                                               (C ,EMP F)
                                               (D b D)
                                               (D ,EMP E)
                                               (D ,EMP F)
                                               (E c E)
                                               (F d F)
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
  (let [(Bs (takef ci (λ (x) (eq? 'b x))))
        (As (takef ci (λ (x) (eq? 'a x))))]
    (or (andmap (λ (x) (eq? 'a x)) ci))))


(define (F-INV-lots-of-kleenes ci)
  (let [(Bs (takef ci (λ (x) (eq? 'b x))))
        (As (takef  ci(λ (x) (eq? 'a x))))]
    (or (andmap (λ (x) (eq? 'a x)) ci))))


(define (G-INV-lots-of-kleenes ci)
  (let [(Bs (takef ci (λ (x) (eq? 'b x))))
        (As (takef ci (λ (x) (eq? 'a x))))]
    (or (andmap (λ (x) (eq? 'a x)) ci))))

(define LOI-lots-of-kleenes (list (list 'S S-INV-lots-of-kleenes)
                                  (list 'A A-INV-lots-of-kleenes)
                                  (list 'B B-INV-lots-of-kleenes)
                                  (list 'C C-INV-lots-of-kleenes)
                                  (list 'D D-INV-lots-of-kleenes)
                                  (list 'E E-INV-lots-of-kleenes)
                                  (list 'F F-INV-lots-of-kleenes)
                                  (list 'G G-INV-lots-of-kleenes)))

;  (a*Ub*)(c*Ud*)U(c*Ud*)a*

(define AT-LEAST-ONE-MISSING (make-unchecked-ndfa '(S A B C)
                                                  '(a b c)
                                                  'S
                                                  '(A B C)
                                                  `((S ,EMP A)
                                                    (S ,EMP B)
                                                    (S ,EMP C)
                                                    (A b A)
                                                    (A c A)
                                                    (B a B)
                                                    (B c B)
                                                    (C a C)
                                                    (C b C))))


;; word → Boolean
;; Purpose: Determine if the given word is empty
(define (S-INV-AT-LEAST-ONE-MISSING ci) (empty? ci))

;; Test for S-INV-AT-LEAST-ONE-MISSING
(check-equal? (S-INV-AT-LEAST-ONE-MISSING '()) #t)
(check-equal? (S-INV-AT-LEAST-ONE-MISSING '(a b)) #f)


;; word → Boolean
;; Purpose: Determine if the given word does not contain a
(define (A-INV-AT-LEAST-ONE-MISSING ci) (empty? (filter (λ (a) (eq? a 'a)) ci)))

;; Test for A-INV-AT-LEAST-ONE-MISSING
(check-equal? (A-INV-AT-LEAST-ONE-MISSING '(a)) #f)
(check-equal? (A-INV-AT-LEAST-ONE-MISSING '(a c b)) #f)
(check-equal? (A-INV-AT-LEAST-ONE-MISSING '(c c b a b)) #f)
(check-equal? (A-INV-AT-LEAST-ONE-MISSING '(b)) #t)
(check-equal? (A-INV-AT-LEAST-ONE-MISSING '(c c b c b)) #t)
(check-equal? (A-INV-AT-LEAST-ONE-MISSING '()) #t)


;; word → Boolean
;; Purpose: Determine if the given word does not contain b
(define (B-INV-AT-LEAST-ONE-MISSING ci) (empty? (filter (λ (a) (eq? a 'b)) ci)))
;; Test for B-INV-AT-LEAST-ONE-MISSING
(check-equal? (B-INV-AT-LEAST-ONE-MISSING '(b)) #f)
(check-equal? (B-INV-AT-LEAST-ONE-MISSING '(a c b)) #f)
(check-equal? (B-INV-AT-LEAST-ONE-MISSING '(a a b a b)) #f)
(check-equal? (B-INV-AT-LEAST-ONE-MISSING '(c)) #t)
(check-equal? (B-INV-AT-LEAST-ONE-MISSING '(c c a c c a a a)) #t)
(check-equal? (B-INV-AT-LEAST-ONE-MISSING '()) #t)


;; word → Boolean
;; Purpose: Determine if the given word does not contain c
(define (C-INV-AT-LEAST-ONE-MISSING ci) (empty? (filter (λ (a) (eq? a 'c)) ci)))

;; Test for C-INV-AT-LEAST-ONE-MISSING
(check-equal? (C-INV-AT-LEAST-ONE-MISSING '(c)) #f)
(check-equal? (C-INV-AT-LEAST-ONE-MISSING '(a b c b)) #f)
(check-equal? (C-INV-AT-LEAST-ONE-MISSING '(c c b a b)) #f)
(check-equal? (C-INV-AT-LEAST-ONE-MISSING '(b)) #t)
(check-equal? (C-INV-AT-LEAST-ONE-MISSING '(b b a a b a a a)) #t)
(check-equal? (C-INV-AT-LEAST-ONE-MISSING '()) #t)


(define LOI-AT-LEAST-ONE-MISSING (list (list 'S S-INV-AT-LEAST-ONE-MISSING)
                                       (list 'A A-INV-AT-LEAST-ONE-MISSING)
                                       (list 'B B-INV-AT-LEAST-ONE-MISSING)
                                       (list 'C C-INV-AT-LEAST-ONE-MISSING)))




(define big-container (make-unchecked-ndfa '(S A B C D E F H I)
                                           '(u n e a r
                                               ;c l d i g o
                                               )
                                           'S
                                           '(C F I)
                                           '((S u A) (A n B) (S e H) (H r I) (D e E) (E a F)    
                                                     (C u C) (C n C) (C e C) (C a C) (C r C)
                                                     (D u D)
                                                     (D n D) (D e D) (D a D) (D r D)
                                                     (F u F) (F n F)
                                                     (F e F) (F a F) (F r F)
                                                     (S u D) (S n D)
                                                     (S a D) (S r D)
                                                     (B u C) (B n C) (B e C) (B a C)
                                                     (B r C)
                                                     (S e E) (B e H) (B e E) (F e H) (C e H))))

(define LOI-big-container (list (list 'S INVS=T)
                                (list 'A INVS=T)
                                (list 'B INVS=T)
                                (list 'C INVS=T)
                                (list 'D INVS=T)
                                (list 'E INVS=T)
                                (list 'F INVS=T)
                                (list 'G INVS=T)
                                (list 'H INVS=T)
                                (list 'I INVS=T))) 







(define mini-monster-kaboom (make-unchecked-ndfa '(S A B J K L M)
                                                 '(k a
                                                     ;l
                                                     b
                                                     ;u g c h
                                                     o
                                                     ;i n
                                                     m)
                                                 'S
                                                 '(M)
                                                 `((S a S) (S b S)
                                                           (S o S)
                                                           (S m S) (S k A)
                                                                        
                                                           (A a B) (A k S) (A b S)
                                                           (A o S)
                                                           (A w S) (A m S)
                                                                        
                                                           (B b J) (B k S) (B a S)
                                                           (B o S)
                                                           (B m S)


                                                           (J k S) (J a S)
                                                           (J b S)
                                                           (J m S) (J o K)

                                                           (K k S) (K a S) (K b S)
                                                          
                                                           (K m S) (K o L)

                                                           (L m M) (L k S) (L a S) (L b S)
                                                           (L o S)
                                                          

                                                           (M k M) (M a M) (M b M)
                                                           (M o M)
                                                           (M m M)

                                                           )))


(define LOI-mini-monster-kaboom (list (list 'S INVS=T)
                                      (list 'A INVS=T)
                                      (list 'B INVS=T)
                                      (list 'J INVS=T)
                                      (list 'K INVS=T)
                                      (list 'L INVS=T)
                                      (list 'M INVS=T)
                                      ))



(define loM (list big-container
                  DNA-SEQUENCE
                  EVIL-dna-sequence
                  CONTAINS-aabab
                  no-contain-bababa
                  M3
                  lots-of-kleenes
                  ONE-LETTER-MISSING
                  ab*b*Uab*
                  a+b+c+a+b+))








(define tests (list #;(test-case 'big-container
                                 50
                                 (lambda () (sm-test-invs big-container
                                                          (list 'S INVS=T)
                                                          (list 'A INVS=T)
                                                          (list 'B INVS=T)
                                                          (list 'C INVS=T)
                                                          (list 'D INVS=T)
                                                          (list 'E INVS=T)
                                                          (list 'F INVS=T)
                                                          (list 'G INVS=T)
                                                          (list 'H INVS=T)
                                                          (list 'I INVS=T))))
                    #;(test-case 'mini-monster-kaboom
                               50
                               (lambda () (sm-test-invs mini-monster-kaboom
                                                        (list 'S INVS=T)
                                                        (list 'A INVS=T)
                                                        (list 'B INVS=T)
                                                        (list 'J INVS=T)
                                                        (list 'K INVS=T)
                                                        (list 'L INVS=T)
                                                        (list 'M INVS=T))))

                    #;(test-case 'evil-dna-sequence
                               50
                               (lambda () (sm-test-invs EVIL-dna-sequence
                                                        (list 'K DNA-K-INV)
                                                        (list 'H DNA-H-INV)
                                                        (list 'F DNA-F-INV)
                                                        (list 'M DNA-M-INV)
                                                        (list 'I DNA-I-INV)
                                                        (list 'D DNA-D-INV)
                                                        (list 'B DNA-B-INV)
                                                        (list 'S DNA-S-INV)
                                                        (list 'R DNA-R-INV))))


                    #;(test-case 'dna-sequence
                               50
                               (lambda () (sm-test-invs DNA-SEQUENCE
                                                        (list 'K DNA-K-INV)
                                                        (list 'H DNA-H-INV)
                                                        (list 'F DNA-F-INV)
                                                        (list 'M DNA-M-INV)
                                                        (list 'I DNA-I-INV)
                                                        (list 'D DNA-D-INV)
                                                        (list 'B DNA-B-INV)
                                                        (list 'S DNA-S-INV)
                                                        (list 'R DNA-R-INV))))
                    (test-case 'no-contain-bababa
                               50
                               (lambda () (sm-test-invs no-contain-bababa (list 'S S-INV-no-contain-bababa)
                                                        (list 'A A-INV-no-contain-bababa)
                                                        (list 'B B-INV-no-contain-bababa)
                                                        (list 'C C-INV-no-contain-bababa)
                                                        (list 'D D-INV-no-contain-bababa)
                                                        (list 'E E-INV-no-contain-bababa)
                                                        (list 'F F-INV-no-contain-bababa))))
                    (test-case 'AT-LEAST-ONE-MISSING
                               50
                               (lambda () (sm-test-invs AT-LEAST-ONE-MISSING  (list 'S S-INV-AT-LEAST-ONE-MISSING)
                                                        (list 'A A-INV-AT-LEAST-ONE-MISSING)
                                                        (list 'B B-INV-AT-LEAST-ONE-MISSING)
                                                        (list 'C C-INV-AT-LEAST-ONE-MISSING))))
                    (test-case 'lots-of-kleenes
                               50
                               (lambda () (sm-test-invs lots-of-kleenes (list 'S S-INV-lots-of-kleenes)
                                                        (list 'A A-INV-lots-of-kleenes)
                                                        (list 'B B-INV-lots-of-kleenes)
                                                        (list 'C C-INV-lots-of-kleenes)
                                                        (list 'D D-INV-lots-of-kleenes)
                                                        (list 'E E-INV-lots-of-kleenes)
                                                        (list 'F F-INV-lots-of-kleenes)
                                                        (list 'G G-INV-lots-of-kleenes))))
                    (test-case 'ONE-LETTER-MISSING
                               50
                               (lambda () (sm-test-invs ONE-LETTER-MISSING (list 'S S-INV-1-MISSING)
                                                        (list 'A A-INV-1-MISSING)
                                                        (list 'B B-INV-1-MISSING)
                                                        (list 'C C-INV-1-MISSING)
                                                        (list 'D D-INV-1-MISSING)
                                                        (list 'E E-INV-1-MISSING)
                                                        (list 'F F-INV-1-MISSING))))
                    (test-case 'ab*b*Uab*
                               50
                               (lambda () (sm-test-invs ab*b*Uab* (list 'A A-INV-ab*b*Uab*) (list 'B B-INV-ab*b*Uab*) (list 'C C-INV-ab*b*Uab*)
                                                        (list 'D D-INV-ab*b*Uab*) (list 'E E-INV-ab*b*Uab*) (list 'S S-INV-ab*b*Uab*))))
                    (test-case 'M3
                               50
                               (lambda () (sm-test-invs M3 (list 'S S3-INV)
                                                        (list 'A A3-INV)
                                                        (list 'B B3-INV)
                                                        (list 'C C3-INV)
                                                        (list 'D D3-INV)
                                                        (list 'E E3-INV)
                                                        (list 'F F3-INV))))
                    (test-case 'a+b+c+a+b+
                               50
                               (lambda () (sm-test-invs a+b+c+a+b+ (list 'S INVS=T)
                                                        (list 'A INVS=T)
                                                        (list 'B INVS=T)
                                                        (list 'C INVS=T)
                                                        (list 'D INVS=T)
                                                        (list 'E INVS=T))))
                    (test-case 'CONTAINS-aabab
                               50
                               (lambda () (sm-test-invs CONTAINS-aabab (list 'S S2-INV) (list 'A A2-INV)
                                                        (list 'B B2-INV) (list 'C C2-INV)
                                                        (list 'D D2-INV) (list 'E E2-INV))))
                    (test-case 'a+b+c+a+b+ndfa
                               50
                               (lambda () (sm-test-invs (make-unchecked-ndfa '(S A B C D E)
                                                                             '(a b c)
                                                                             'S
                                                                             '(E)
                                                                             '((S a A)
                                                                               (A a A)
                                                                               (A b B)
                                                                               (B b B)
                                                                               (B c C)
                                                                               (C c C)
                                                                               (C a D)
                                                                               (D a D)
                                                                               (D b E)
                                                                               (E b E))) (list 'S INVS=T)
                                                                                         (list 'A INVS=T)
                                                                                         (list 'B INVS=T)
                                                                                         (list 'C INVS=T)
                                                                                         (list 'D INVS=T)
                                                                                         (list 'E INVS=T))))
                    (test-case 'NO-AA
                               50
                               (lambda () (sm-test-invs NO-AA
                                                        (list 'S S-INV)
                                                        (list 'A A-INV)
                                                        (list 'B B-INV)
                                                        (list 'R R-INV))))
                    (test-case 'EVEN-NUM-Bs
                               50
                               (lambda () (sm-test-invs EVEN-NUM-Bs
                                                        (list 'S EVEN-NUM-Bs-S-INV) (list 'F EVEN-NUM-Bs-F-INV))))
                    (test-case 'aa*Uab*
                               50
                               (lambda () (sm-test-invs aa*Uab*
                                                        (list 'K  aa-ab-K-INV)
                                                        (list 'B aa-ab-B-INV)
                                                        (list 'D aa-ab-D-INV))))
                    (test-case 'EX-NDFA
                               50
                               (lambda () (sm-test-invs EX-NDFA
                                                        (list 'S S-INV-EX-NDFA)
                                                        (list 'A A-INV-EX-NDFA)
                                                        (list 'B B-INV-EX-NDFA)
                                                        (list 'C C-INV-EX-NDFA))))
                    ))
(define res
  (for/list ([test (in-list tests)])
    (displayln (test-case-name test))
    (let ([result (for/vector #:length (test-case-num-tests test)
                    ([test-num (in-range (test-case-num-tests test))])
                    (define-values (results-lst cpu-time real-time gc-time)
                      (time-apply (test-case-thunk test) '()))
                    (collect-garbage 'major)
                    (collect-garbage 'major)
                    real-time)])
      (println result)
      (list (test-case-name test)
            result))))
#;(println res)



#|
"DNA-SEQUENCE"
(for ([x (in-naturals)]
      #:break (> x 50))
 
 (time (sm-test-invs DNA-SEQUENCE
                     (list 'K DNA-K-INV)
                       (list 'H DNA-H-INV)
                       (list 'F DNA-F-INV)
                       (list 'M DNA-M-INV)
                       (list 'I DNA-I-INV)
                       (list 'D DNA-D-INV)
                       (list 'B DNA-B-INV)
                       (list 'S DNA-S-INV)
                       (list 'R DNA-R-INV))))
"no-contain-bababa"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs no-contain-bababa (list 'S S-INV-no-contain-bababa)
                            (list 'A A-INV-no-contain-bababa)
                            (list 'B B-INV-no-contain-bababa)
                            (list 'C C-INV-no-contain-bababa)
                            (list 'D D-INV-no-contain-bababa)
                            (list 'E E-INV-no-contain-bababa)
                            (list 'F F-INV-no-contain-bababa))))
"AT-LEAST-ONE-MISSING"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs AT-LEAST-ONE-MISSING  (list 'S S-INV-AT-LEAST-ONE-MISSING)
                                       (list 'A A-INV-AT-LEAST-ONE-MISSING)
                                       (list 'B B-INV-AT-LEAST-ONE-MISSING)
                                       (list 'C C-INV-AT-LEAST-ONE-MISSING))))
"lots-of-kleenes"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs lots-of-kleenes (list 'S S-INV-lots-of-kleenes)
                             (list 'A A-INV-lots-of-kleenes)
                             (list 'B B-INV-lots-of-kleenes)
                             (list 'C C-INV-lots-of-kleenes)
                             (list 'D D-INV-lots-of-kleenes)
                             (list 'E E-INV-lots-of-kleenes)
                             (list 'F F-INV-lots-of-kleenes)
                             (list 'G G-INV-lots-of-kleenes))))
"ONE-LETTER-MISSING"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs ONE-LETTER-MISSING (list 'S S-INV-1-MISSING)
                              (list 'A A-INV-1-MISSING)
                              (list 'B B-INV-1-MISSING)
                              (list 'C C-INV-1-MISSING)
                              (list 'D D-INV-1-MISSING)
                              (list 'E E-INV-1-MISSING)
                              (list 'F F-INV-1-MISSING))))
"ab*b*Uab*"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs ab*b*Uab* (list 'A A-INV-ab*b*Uab*) (list 'B B-INV-ab*b*Uab*) (list 'C C-INV-ab*b*Uab*)
                            (list 'D D-INV-ab*b*Uab*) (list 'E E-INV-ab*b*Uab*) (list 'S S-INV-ab*b*Uab*))))
"M3"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs M3 (list 'S S3-INV)
                     (list 'A A3-INV)
                     (list 'B B3-INV)
                     (list 'C C3-INV)
                     (list 'D D3-INV)
                     (list 'E E3-INV)
                     (list 'F F3-INV))))
"a+b+c+a+b+(dfa)"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs a+b+c+a+b+ (list 'S INVS=T)
                             (list 'A INVS=T)
                             (list 'B INVS=T)
                             (list 'C INVS=T)
                             (list 'D INVS=T)
                             (list 'E INVS=T))))
"CONTAINS-aabab"

(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs CONTAINS-aabab (list 'S S2-INV) (list 'A A2-INV)
                              (list 'B B2-INV) (list 'C C2-INV)
                              (list 'D D2-INV) (list 'E E2-INV))))
"a+b+c+a+b+(ndfa)"
(for ([x (in-naturals)]
      #:break (= x 51))
 
 (time (sm-test-invs (make-unchecked-ndfa '(S A B C D E)
                             '(a b c)
                             'S
                             '(E)
                             '((S a A)
                               (A a A)
                               (A b B)
                               (B b B)
                               (B c C)
                               (C c C)
                               (C a D)
                               (D a D)
                               (D b E)
                               (E b E))) (list 'S INVS=T)
                             (list 'A INVS=T)
                             (list 'B INVS=T)
                             (list 'C INVS=T)
                             (list 'D INVS=T)
                             (list 'E INVS=T))))
"NO-AA"
(for ([x (in-naturals)]
      #:break (> x 50))

 (time (sm-test-invs NO-AA
                     (list 'S S-INV)
                   (list 'A A-INV)
                   (list 'B B-INV)
                   (list 'R R-INV))))
"EVEN-NUM-Bs"
(for ([x (in-naturals)]
      #:break (> x 50))
 
 (time (sm-test-invs EVEN-NUM-Bs
                     (list 'S EVEN-NUM-Bs-S-INV) (list 'F EVEN-NUM-Bs-F-INV))))
"aa*Uab*"
(for ([x (in-naturals)]
      #:break (> x 50))
 
 (time (sm-test-invs aa*Uab*
                     (list 'K  aa-ab-K-INV) (list 'B aa-ab-B-INV) (list 'D aa-ab-D-INV))))
"EX-NDFA"
(for ([x (in-naturals)]
      #:break (> x 50))
 
 (time (sm-test-invs EX-NDFA
                     (list 'S S-INV-EX-NDFA)
                          (list 'A A-INV-EX-NDFA)
                          (list 'B B-INV-EX-NDFA)
                          (list 'C C-INV-EX-NDFA))))


|#






;; contains-kaboom-kaching-kalabunga

(define monster-machine (make-unchecked-ndfa '(S A B C D E F G H I J K L M N O P Q R)
                                             '(k a l b u g c h o i n m)
                                             'S
                                             '(I M R)
                                             `((S a S) (S l S) (S b S) (S u S) (S g S)
                                                       (S c S) (S h S) (S o S) (S i S)
                                                       (S n S) (S m S) (S k A)
                                                                        
                                                       (A a B) (A k S) (A l S) (A b S) (A u S)
                                                       (A g S) (A c S) (A h S) (A o S)
                                                       (A w S) (A i S) (A n S) (A m S)
                                                                        
                                                       (B l C) (B b J) (B c N) (B k S) (B a S)
                                                       (B u S) (B g S) (B h S) (B o S)
                                                       (B i S) (B n S) (B m S)

                                                       (C k S) (C l S) (C b S) (C u S) (C g S)
                                                       (C c S) (C h S) (C o S) (C i S)
                                                       (C n S) (C m S) (C a D)

                                                       (D b E) (D k S) (D a S) (D l S) (D u S)
                                                       (D g S) (D c S) (D h S) (D o S)
                                                       (D i S) (D n S) (D m S)

                                                       (E u F) (E k S) (E a S) (E l S) (E b S)
                                                       (E g S) (E c S) (E h S) (E o S)
                                                       (E i S) (E n S) (E m S)

                                                       (F n G) (F k S) (F a S) (F l S) (F b S)
                                                       (F u S) (F g S) (F c S) (F h S) (F o S)
                                                       (F i S) (F m S)

                                                       (G g H) (G k S) (G a S) (G l S) (G b S)
                                                       (G u S) (G c S) (G h S) (G o S)
                                                       (G i S) (G m S) (G n S)

                                                       (H a I) (H k S) (H l S) (H b S) (H u S)
                                                       (H g S) (H c S) (H h S) (H o S)
                                                       (H i S) (H n S) (H m S)

                                                       (I k I) (I a I) (I l I) (I b I) (I u I)
                                                       (I g I) (I c I) (I h I) (I o I)
                                                       (I i I) (I n I) (I m I)

                                                       (J k S) (J a S) (J l S) (J u S) (J g S)
                                                       (J c S) (J h S) (J b S) (J i S)
                                                       (J n S) (J m S) (J o K)

                                                       (K o L) (K k S) (K a S) (K l S) (K b S)
                                                       (K u S) (K g S) (K c S) (K h S)
                                                       (K i S) (K n S) (K m S)

                                                       (L m M) (L k S) (L a S) (L l S) (L b S)
                                                       (L u S) (L g S) (L c S) (L h S) (L o S)
                                                       (L i S) (L n S)

                                                       (M k M) (M a M) (M l M) (M b M) (M u M)
                                                       (M g M) (M c M) (M h M) (M o M)
                                                       (M i M) (M n M) (M m M)

                                                       (N h O) (N k S) (N a S) (N l S) (N b S)
                                                       (N u S) (N g S) (N c S) (N o S)
                                                       (N i S) (N n S) (N m S)

                                                       (O i P) (O k S) (O a S) (O l S) (O b S)
                                                       (O u S) (O g S) (O c S) (O h S) (O o S)
                                                       (O n S) (O m S)

                                                       (P n Q) (P k S) (P a S) (P l S) (P b S)
                                                       (P u S) (P g S) (P c S) (P h S) (P o S)
                                                       (P i S) (P m S)

                                                       (Q g R) (Q k S) (Q a S) (Q l S) (Q b S)
                                                       (Q u S) (Q c S) (Q h S) (Q o S)
                                                       (Q i S) (Q n S) (Q m S)

                                                       (R k R) (R a R) (R l R) (R b R) (R u R)
                                                       (R g R) (R c R) (R h R) (R o R)
                                                       (R i R) (R n R) (R m R))))



(define LOI-monster-machine (list (list 'S INVS=T)
                                  (list 'A INVS=T)
                                  (list 'B INVS=T)
                                  (list 'C INVS=T)
                                  (list 'D INVS=T)
                                  (list 'E INVS=T)
                                  (list 'F INVS=T)
                                  (list 'G INVS=T)
                                  (list 'H INVS=T)
                                  (list 'I INVS=T)
                                  (list 'J INVS=T)
                                  (list 'K INVS=T)
                                  (list 'L INVS=T)
                                  (list 'M INVS=T)
                                  (list 'N INVS=T)
                                  (list 'O INVS=T)
                                  (list 'P INVS=T)
                                  (list 'Q INVS=T)
                                  (list 'R INVS=T)))


