#lang fsm

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

;; A queue of X, (qof X), is eithe(r:
;;  1. empty
;;  2. (cons X (qof X))


(define E-QUEUE '())

(define qempty? empty?)

;; Tests for qempty?
(check-equal? (qempty? '())      #true)
(check-equal? (qempty? '(a b c)) #false)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (append a-qox a-lox))

;; Tests for enqueue
(check-equal? (enqueue '(8 d) '()) '(8 d))
(check-equal? (enqueue '(d) '(a b c)) '(a b c d))
(check-equal? (enqueue '(6 5 4) '(7)) '(7 6 5 4))

;; (qof X) --> X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; Tests for qfirst
;(check-error  (qfirst '()) "qfirst applied to an empty queue")
(check-equal? (qfirst '(a b c)) 'a)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))

;; Tests for qfirst
;(check-error  (dequeue '()) "dequeue applied to an empty queue")
(check-equal? (dequeue '(a b c)) '(b c))



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







;                                                                                                     
;                                                                                                     
;                                                                                                     
;                             ;                                                                       
;                             ;                                                                       
;  ;;;    ;;;                                              ;;;;;                                      
;   ;;    ;;                                                  ;;                                      
;   ; ;  ; ;     ;;;;       ;;;      ;; ;;;                  ;  ;      ;;  ;;      ;;;;       ;;;;    
;   ; ;  ; ;    ;    ;        ;       ;;   ;                 ;  ;       ;;;  ;    ;    ;     ;    ;   
;   ; ;  ; ;         ;        ;       ;    ;                 ;  ;       ;        ;      ;         ;   
;   ;  ;;  ;    ;;;;;;        ;       ;    ;                ;    ;      ;        ;;;;;;;;    ;;;;;;   
;   ;  ;;  ;   ;     ;        ;       ;    ;                ;;;;;;      ;        ;          ;     ;   
;   ;      ;   ;     ;        ;       ;    ;                ;    ;      ;        ;          ;     ;   
;   ;      ;   ;    ;;        ;       ;    ;               ;      ;     ;         ;     ;   ;    ;;   
;  ;;;    ;;;   ;;;; ;;    ;;;;;;;   ;;;  ;;;             ;;;    ;;;   ;;;;;;      ;;;;;     ;;;; ;;  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     





(struct rule (source element-read destination) #:transparent)

(define (rule->struct a-rule)
  (rule (first a-rule) (second a-rule) (third a-rule)))


;; (listof (list state (word -> boolean))) (listof symbol) -> Boolean
;; Purpose: Determine if the given invariant holds 
(define (invariant-holds? a-loi a-config)
  (or (empty? a-loi)
      ((second (first a-loi)) (first a-config))))



;; rule (listof rule) -> (listof rule)
;; Purpose: To return the next rules that can be used from the given rule
(define (get-next-rules a-rule rules)
  (filter (λ (rule) (eq? (third a-rule) (first rule))) rules))

;; (listof rule) (listof rule) -> (listof (listof rule))
;; Purpose: To return a list of paths that comes from the given path and rules
(define (new-paths a-path rules)
  (local [;; (listof rule) (listof (listof rule)) -> (listof (listofrule))
          ;; Purpose: To return a list of paths that come from the given paths and rules
          ;; Accumulator invariant: accum = list of paths
          (define (new-paths-helper next-rules accum)
            (if (empty? next-rules) accum
                (new-paths-helper (rest next-rules)
                                  (append (list (append a-path (list (first next-rules)))) accum))))]
    (new-paths-helper rules '())))


;; machine -> (listof rules)
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine)
  (local[(define rules (sm-rules a-machine))
         ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
         ;; Purpose: To return all the paths of the given machine
         ;; Accumulator invarient: paths = list of current paths
         (define (find-paths-helper a-qop paths)
           (if (qempty? a-qop) paths
               (local [(define next-rules-first-path (get-next-rules (last (qfirst a-qop))
                                                                     (filter
                                                                      (λ (x)
                                                                        (not (member? x (qfirst a-qop))))
                                                                      rules)))
                       (define paths-with-qfirst (cons (qfirst a-qop) paths))]
                 (if (empty? next-rules-first-path)
                     (find-paths-helper (dequeue a-qop)
                                        paths-with-qfirst)
                     (find-paths-helper (enqueue (new-paths (qfirst a-qop) next-rules-first-path)
                                                 (dequeue a-qop))
                                        paths-with-qfirst))
                 )))]
    (find-paths-helper (enqueue (map (λ (x) (list x))
                                     (filter
                                      (λ (rule) (eq? (first rule) (sm-start a-machine)))
                                      (sm-rules a-machine)))
                                '())
                       '())))



;; ndfa -> ndfa
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
(define (remove-states-that-cannot-reach-finals a-ndfa)
  (local [(define paths-that-end-in-finals (filter (λ (x) (member? (third (last x)) (sm-finals a-ndfa)))
                                                   (find-paths a-ndfa)))
          (define new-rules (remove-duplicates (apply append paths-that-end-in-finals)))
          (define new-states (remove-duplicates (append-map (λ (x) (list (first x) (third x))) new-rules)))]
    (make-ndfa new-states (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules)))


;; (listof rules) -> word
;; Purpose: To return a word that is made from the given list of rules
(define (word-of-path a-lor)
  (filter (λ (x) (not (eq? 'ε x))) (append-map (λ (x) (list (second x))) a-lor)))


;; (listof (listof symbol)) (listof (list state (word -> boolean))) -> Boolean
;; Purpose: To determine if a the invariant for a given path holds
(define (path-inv-not-hold? a-path a-loi)
  (not (invariant-holds? a-loi (list (word-of-path a-path) (third (last a-path))))))


;; machine (listof (list state (word -> boolean))) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs a-machine a-loi)
  (local [;; the given machine without the states and rules of states that cannot reach a final state
          (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                                  a-machine
                                  (remove-states-that-cannot-reach-finals a-machine)))
          ;; list of invariants that are reachable from the starting configuration
          (define reachable-inv (filter (λ (x) (member? (first x) (sm-states new-machine))) a-loi))
          ;; all paths of new-machine
          (define all-paths-new-machine (find-paths new-machine))

          ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
          ;; Purpose: To return a list of the invarients and the word that causes them not to hold
          ;; Accumulator Invarient: accum = list of lists of words that cause the invarient not to hold
          ;;                                & the state that it doesn't hold for
          (define (sm-test-invs-helper all-paths accum)
            (if (empty? all-paths)
                accum
                (sm-test-invs-helper (rest all-paths)
                                     (if (path-inv-not-hold? (first all-paths)
                                                             (filter
                                                              (λ (x) (eq? (third (last (first all-paths)))
                                                                          (first x)))
                                                              reachable-inv))
                                         (cons (list (word-of-path (first all-paths))
                                                     (third (last (first all-paths)))) accum)
                                         accum))))
          ]
    (sm-test-invs-helper all-paths-new-machine
                         (if ((second (assoc (sm-start a-machine) a-loi)) '())
                             '()
                             (list (list '() (sm-start a-machine)))))))
