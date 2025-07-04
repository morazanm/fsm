#lang fsm
(require "sm-test-invs-wip.rkt")

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
(define (aa-ab-D-INV ci)
  (and (not (empty? ci))
       (eq? (first ci) 'a)
       (andmap (λ (el) (eq? el 'b)) ci)))


(define LOI-aa*-bb* (list (list 'K  aa-ab-K-INV) (list 'B aa-ab-B-INV) (list 'D aa-ab-D-INV)))



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
;;   A: ci = the stack reversed
;;   B: ci = (append w v) AND w = stack^R v^R
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



;; Let Σ = {a b}. Design and implement a pda for
;; L = {aibj |i≤j≤2i}. Follow all the steps of the design recipe

;; States: 
;;   S: ci = empty AND stack = empty, start and final state
;;   A: ci = a+ AND stack = amount of as to be matched with a b AND (length stack) => 1
;;   B: ci = a+b+ AND stack = amount of as to be matched with a b
;;   C: ci = aibj where i≤j≤2i AND stack = empty, final state

(define AiBj (make-ndpda '(S A B C)
                         '(a b)
                         '(a)
                         'S
                         '(S C)
                         `(((S a ,EMP) (A (a)))
                           ((S a ,EMP) (A (a a)))
                           ((A a ,EMP) (A (a)))
                           ((A a ,EMP) (A (a a)))
                           ((A b (a)) (B ,EMP))
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
;;          ci = a+ AND stack = amount of as to be matched with a b AND (length stack) => 1
(define (A-INV-AiBj ci stack)
  (and (andmap (λ (x) (eq? 'a x)) ci)
       (< 0 (length ci))))

;; tests for A-INV-AiBj
(check-equal? (A-INV-AiBj '(a) '(a a)) #t)
(check-equal? (A-INV-AiBj '(a a) '(a a a a)) #t)
(check-equal? (A-INV-AiBj '(a a a) '(a a a a a a)) #t)
(check-equal? (A-INV-AiBj '(a a a a) '(a a a a a a a a)) #t)
(check-equal? (A-INV-AiBj '() '()) #f)
(check-equal? (A-INV-AiBj '() '(a)) #f)
(check-equal? (A-INV-AiBj '(a b) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;          (ci = a+b+ AND stack = amount of as to be matched with a b)
(define (B-INV-AiBj ci stack)
  (local [(define num-as (length (takef ci (λ (x) (eq? x 'a)))))
          (define num-bs (length (drop ci num-as)))]
  (and (< 0 num-as)
       (< 0 num-bs)
       (< 1 (length ci))
       (<= num-as num-bs (* 2 num-as)))))

;; tests for B-INV-AiBj
(check-equal? (B-INV-AiBj '(a a b b) '()) #t)
(check-equal? (B-INV-AiBj '(a b) '(a)) #t)
(check-equal? (B-INV-AiBj '(a a a b b b) '(a a a a)) #t)
(check-equal? (B-INV-AiBj '(a a b b b) '(a)) #t)
(check-equal? (B-INV-AiBj '(a a b b) '(a a a)) #t)
(check-equal? (B-INV-AiBj '(a a b) '(a a a)) #f)
(check-equal? (B-INV-AiBj '(a a b b b b b b) '()) #f)
(check-equal? (B-INV-AiBj '(a b b b b b b b b b) '()) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in C
;;          (ci = aibj where i≤j≤2i AND stack = empty)
(define (C-INV-AiBj ci stack)
  (local [(define num-as (length (takef ci (λ (x) (eq? x 'a)))))
          (define num-bs (length (drop ci num-as)))]
    (and (< 0 num-as)
         (<= num-as num-bs)
         (<= num-bs (* 2 num-as))
         (empty? stack))))

;; tests for C-INV-AiBj
(check-equal? (C-INV-AiBj '(a b) '()) #t)
(check-equal? (C-INV-AiBj '(a b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b b) '()) #t)
(check-equal? (C-INV-AiBj '(a b b b) '()) #f)
(check-equal? (C-INV-AiBj '() '()) #f)
(check-equal? (C-INV-AiBj '(a b b) '(a)) #f)
(check-equal? (C-INV-AiBj '(a b b) '(a a)) #f)
       

;; Let Σ = {a b}. Design and implement a pda for L = {w | w has 3
;;   times as many as than b}. Follow all the steps of the design recipe










