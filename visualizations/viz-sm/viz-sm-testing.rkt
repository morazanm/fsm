#lang racket

(require "testing-parameter.rkt"
         "../../fsm-core/interface.rkt"
         "sm-viz.rkt")

(define (ndfa->pda M)
  (let [(states (sm-states M))
        (sigma (sm-sigma M))
        (start (sm-start M))
        (finals (sm-finals M))
        (rules (sm-rules M))]
    (make-ndpda states
                sigma
                '()
                start
                finals
                (map (λ (r) (list (list (first r) (second r) EMP)
                                  (list (third r) EMP)))
                     rules))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = ab* U (ab)*
(define M (make-ndfa '(S A B C D E F G H I)
                     '(a b)
                     'S
                     '(A B C D)
                     `((S ,EMP A)
                       (S ,EMP D)
                       (A a B)
                       (B ,EMP C)
                       (C b I)
                       (I ,EMP B)
                       (D ,EMP E)
                       (E a F)
                       (F ,EMP G)
                       (G b H)
                       (H ,EMP D))))

(define L (make-ndfa '(S A B C D E)
                               '(a b)
                               'S
                               '(S B C E)
                               `((S ,EMP A) (S ,EMP C) (A a B) (B b B)
                                 (C a D) (C b E) (D b C) (E b E))))


(define aa*Uab* (make-ndfa '(K B D)
                           '(a b)
                           'K
                           '(B D)
                           `((K a D) (K a B)
                                     (B a B)
                                     (D b D))))

(define AT-LEAST-ONE-MISSING
  (make-ndfa '(S A B C)
             '(a b c)
             'S
             '(A B C)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                          (A b A) (A c A)
                          (B a B) (B c B)
                          (C a C) (C b C))))

(define p2-ndfa
  (make-ndfa '(S A B C D E)
             '(a b)
             'S
             '(C E)
             `((S ,EMP A) (S ,EMP D)
                          (A a B) (A ,EMP C)
                          (B b A)
                          (C b C)
                          (D a E)
                          (E b E))))

(define AB*B*UAB*
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))

(define AB*B*UAB*2
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H) (H a S))))

(define aa-ab
  (make-ndfa `(S A B F)
             '(a b)
             'S
             '(A B F)
             `((S a A) (S a B) (S ,EMP F)
                       (A a A)
                       (B b B))))

(define ends-with-two-bs
  (make-ndfa `(S A B)
             '(a b)
             'S
             '(B)
             `((S a S) (S b S) (S b A)
                       (A b B)
                       (B b B))))
(define ENDS-WITH-TWO-Bs
  (make-ndfa `(S A B)
             '(a b)
             'S
             '(B)
             `((S a S) (S b A)
                       (A b B) (A a S)
                       (B b B) (B a S))))

(define nd-a* (make-ndfa '(K H)
                         '(a b)
                         'K
                         '(H)
                         `((K ,EMP H)
                           (H a H))))

(define missing-exactly-one
  (make-ndfa '(S A B C D E F G H I J K L M N O P)
             '(a b c)
             'S
             '(E G I K M O)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                          (A b D) (A c F)
                          (B a H) (B b J)
                          (C a L) (C c N)
                          (D b D) (D c E)
                          (F c F) (F b G)
                          (H a H) (H b I)
                          (J b J) (J a K)
                          (L a L) (L c M)
                          (N c N) (N a O)
                          (E c E) (E b E) (E a P)
                          (G b G) (G c G) (G a P)
                          (I b I) (I a I) (I c P)
                          (K a K) (K b K) (K c P)
                          (M a M) (M c M) (M b P)
                          (O a O) (O c O) (O b P)
                          (P a P) (P b P) (P c P))))

(define nd (make-ndfa '(S Z Y A B)
                      '(a b)
                      'S
                      '(B)
                      `((S b Z)
                        (S b Y)
                        (Y a A)
                        (Z a A)
                        (A a B))))

(define n (make-ndfa '(K H F M I)
                     '(a b)
                     'K
                     '(I)
                     `((K b H)
                       (H ,EMP F)
                       (H ,EMP M)
                       (F a I)
                       (M a I))))

(define nk (make-ndfa '(K H F M I)
                      '(a b)
                      'K
                      '(I)
                      `((K b H)
                        (H ,EMP F)
                        (F ,EMP M)
                        (M ,EMP I)
                        (I ,EMP H))))


(define PROP-BI (make-dfa '(S M N)
                          '(0 1)
                          'S
                          '(N M)
                          '((S 1 M)
                            (S 0 N)
                            (M 0 M)
                            (M 1 M))))

(define DNA-SEQUENCE (make-dfa '(K H F M I D B S R) ;C)
                               '(a t c g)
                               'K
                               '(K F I B R)
                               `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                         (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                         (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                         (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

;; L = (aba)* U (ab)*
(define ND
  (make-ndfa '(S A B C D E)
             '(a b)
             'S
             '(S)
             `((S a A) (S a B)
                       (A b C)
                       (B b D)
                       (C a E)
                       (D ,EMP S)
                       (E ,EMP S))))

(define ND2
  (make-ndfa
   '(S A B C D E F)
   '(a b)
   'S
   '(D E)
   `((S ,EMP A) (S ,EMP B)
                (A ,EMP D)
                (D b D) (D ,EMP F)
                (B a E) (B b B)
                (E a E) (E b E) (E ,EMP C))))

(define ND3
  (make-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) (S ,EMP B)
                          (A a A) (A ,EMP C)
                          (C ,EMP D)
                          (D ,EMP B)
                          (B b B))))

(define ND4 (make-ndfa '(S ds)
                       '(a b)
                       'S
                       '(ds)
                       `((S a ds)
                         (ds a ds))))

(define ND5
  (make-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) 
               (A ,EMP B)
               (B ,EMP C)
               (C ,EMP D)
               (D ,EMP S))))

(define EVEN-NUM-Bs
  (make-dfa '(S F)
            '(a b)
            'S
            '(S)
            `((S a S) (S b F)
                      (F a F) (F b S))
            'no-dead))

(define M2 (make-dfa `(S A F ,DEAD)
                     '(a b)
                     'S
                     '(F)
                     `((S a A) (S b ,DEAD) (,DEAD a ,DEAD) (,DEAD b ,DEAD)
                               (A a ,DEAD) (A b F)
                               (F a ,DEAD) (F b F))
                     'no-dead))

;;word -> boolean
;;Purpose: Determines if the given word is missing an a
(define (ALON-A-INV a-word)
  (empty? (filter (λ (w) (equal? w 'a))
                  a-word)))

;;word -> boolean
;;Purpose: Determines if the given word is missing an b
(define (ALON-B-INV a-word)
  (empty? (filter (λ (w) (equal? w 'b))
                  a-word)))
;;word -> boolean
;;Purpose: Determines if the given word is missing an c
(define (ALON-C-INV a-word)
  (empty? (filter (λ (w) (equal? w 'c))
                  a-word)))

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (S-INV a-word)
  (empty? a-word))

;;word -> boolean
;;Purpose: Determines if the last letter in the given word is an b
(define (ND-K-INV a-word)
  (or (empty? a-word) (equal? (last a-word) 'b)))

;;word -> boolean
;;Purpose: Determines if the last letter in the given word is an a
(define (B-INV a-word)
  (and (not (empty? a-word)) (not
       (equal? (last a-word) 'a))))
;)

;;word -> boolean
;;Purpose: Determines if the given word has one a
(define (C-INV a-word)
  (= (length (filter (λ (w) (equal? w 'a)) a-word)) 1))

;;word -> boolean
;;Purpose: Determines if the given word is empty or if the last letter is an a or b
(define (ND-H-INV a-word)
  ;(not
  (or (empty? a-word) (equal? (last a-word) 'a) (equal? (last a-word) 'b)))
;)

;;word -> boolean
;;Purpose: Determine if the given word has an even number of Bs
(define (EVEN-NUM-Bs-S-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))

;;word -> boolean
;;Purpose: Determine if the given word has an odd number of Bs
(define (EVEN-NUM-Bs-F-INV a-word)
  (odd? (length (filter (λ (w) (equal? w 'b)) a-word))))


(define (s-inv a-word)
  (empty? a-word))

(define (a-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (= num-a num-b)))

(define (b-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (> num-a num-b)))

(define (c-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (= num-b num-a)))

(define (d-inv a-word)
  (local [(define num-a (length (filter (λ (w) (equal? w 'a)) a-word)))
          (define num-b (length (filter (λ (w) (equal? w 'b)) a-word)))]
    (= num-a num-b)))

(define (e-inv a-word)
  (andmap (λ (w) (equal? w 'b)) (rest a-word)))

(define (S-INV1 ci)
  (not (empty? ci)))

;; word -> Boolean
;; Purpose: To determine whether ci = aa*
(define (A-INV1 ci)
  #f #;(and (not (empty? ci))
            (andmap (λ (w) (eq? w 'a)) ci)))

;; word -> Boolean
;; Purpose: To determine whether ci = ab*
(define (B-INV1 ci)
  (and (not (empty? ci))
       (eq? (first ci) 'a)
       (andmap (λ (el) (eq? el 'b)) ci)))

;;word -> Boolean
;;Purpose: To determine whether ci = emp
(define (F-INV1 ci)
  (empty? ci))

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (DNA-K-INV a-word)
  (empty? a-word))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
(define SCHIZO-SAME-NUM-AB (make-ndpda '(K H I)
                                       '(a b)
                                       '(a b)
                                       'K
                                       '(I)
                                       `(((K ,EMP ,EMP)(H ,EMP))
                                         ((K a ,EMP)(K (b)))
                                         ((K b ,EMP)(K (a)))
                                         ((H ,EMP ,EMP)(K ,EMP))
                                         ((H b (b))(H ,EMP))
                                         ((H a (a))(H ,EMP))
                                         ((H ,EMP ,EMP)(I ,EMP)))))


(define P2 (make-ndpda '(S H)
                       '(a b)
                       '(b)
                       'S
                       '(H)
                       `(((S ε ε)(H ε))     ((S a ε)(S (b b)))
                                            ((H b (b b))(H ε)) ((H ε (b))(H ε)))))

(define P3 (make-ndpda '(S H)
                       '(a b)
                       '(b)
                       'S
                       '(H)
                       `(((S ε ε)(H ε))     ((S a ε)(S (b b)))
                                            ((H b (b b))(H ε)) ((H b (b))(H ε)))))


(define wcw^r (make-ndpda '(S P Q F)
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

(define PUW (make-ndpda '(S P Q F  A B X)
                        '(a b c)
                        '(a b)
                        'S
                        '(F X)
                        `(((S ,EMP ,EMP) (P ,EMP))
                          ((P a ,EMP) (P (a)))
                          ((P b ,EMP) (P (b)))
                          ((P c ,EMP) (Q ,EMP))
                          ((Q a (a)) (Q ,EMP))
                          ((Q b (b)) (Q ,EMP))
                          ((Q ,EMP ,EMP) (F ,EMP))
                          ((S ε ε)(A ε)) ((S ε ε)(X ε)) ((S a ε)(S (b b)))
                          ((A b ε)(A ε)) ((A a ε)(A ε)) ((A ε ε)(B ε))
                          ((B ε ε)(A ε))
                          ((X b (b b))(X ε)) ((X b (b))(X ε)))))

(define BUGGY-SAME-NUM-AB (make-ndpda '(K H F M)
                                      '(a b)
                                      '(a b)
                                      'K
                                      '(K)
                                      `(;((K ,EMP ,EMP) (K ,EMP))
                                        ((K a    ,EMP) (H (b)))
                                        ((K b    ,EMP) (F (a)))                                  
                                        ((H a    ,EMP) (H (b)))
                                        ((H ,EMP  (b)) (M ,EMP))                                  
                                        ((F ,EMP  (a)) (M ,EMP))
                                        ((F b    ,EMP) (F (a)))                                  
                                        ;((M a    ,EMP) (H ,EMP))
                                        ((M a    ,EMP) (K ,EMP))
                                        ;((M b    ,EMP) (F ,EMP))
                                        ((M b    ,EMP) (K ,EMP))
                                        )))

(define SAME-NUM-AB (make-ndpda '(K H F M)
                                '(a b)
                                '(a b)
                                'K
                                '(M)
                                `(((K ,EMP ,EMP) (H ,EMP))
                                  ((K ,EMP ,EMP) (F ,EMP))
                                  ((H a ,EMP) (H (b)))
                                  ((H b (b)) (F ,EMP))
                                  ((H ,EMP ,EMP) (M ,EMP))
                                  ((F b ,EMP) (F (a)))
                                  ((F a (a)) (H ,EMP))
                                  ((F ,EMP ,EMP) (M ,EMP)))))

(define P (make-ndpda '(S A B X)
                      '(a b)
                      '(b)
                      'S
                      '(X)
                      `(((S ε ε)(A ε)) ((S ε ε)(X ε)) ((S a ε)(S (b b)))
                                       ((A b ε)(A ε)) ((A a ε)(A ε)) ((A ε ε)(B ε))
                                       ((B ε ε)(A ε))
                                       ((X b (b b))(X ε)) ((X b (b))(X ε)))))

(define aˆnbˆn (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

(define p (make-ndpda '(K H F M I)
                      '(a b)
                      '(a b)
                      'K
                      '(I)
                      `(((K b ,EMP)(H ,EMP))
                        ((H ,EMP ,EMP)(F ,EMP))
                        ((H ,EMP ,EMP)(M ,EMP))
                        ((F a ,EMP)(I ,EMP))
                        ((M a ,EMP)(I ,EMP)))))

(define pk (make-ndpda '(K H F M I)
                       '(a b)
                       '(a b)
                       'K
                       '(I)
                       `(((K b ,EMP)(H ,EMP))
                         ((H ,EMP ,EMP)(F ,EMP))
                         ((F ,EMP ,EMP)(M ,EMP))
                         ((M ,EMP ,EMP)(I ,EMP))
                         ((I ,EMP ,EMP)(H ,EMP)))))

(define a* (make-ndpda '(K H)
                       '(a b)
                       '(a)
                       'K
                       '(H)
                       `(((K ,EMP ,EMP)(H ,EMP))
                         ((H a ,EMP)(H ,EMP)))))



(define aa* (make-ndpda '(K H)
                        '(a b)
                        '(a)
                        'K
                        '(H)
                        `(((K a ,EMP)(H ,EMP))
                          ((H a ,EMP)(H ,EMP)))))

(define inf-a (make-ndpda '(K)
                          '(a)
                          '(a)
                          'K
                          '(K)
                          `(((K ,EMP ,EMP) (K, '(a))))))

(define more-a-than-b (make-ndpda '(S A)
                                  '(a b)
                                  '(a)
                                  'S
                                  '(A)
                                  `(((S a ,EMP) (S (a)))
                                    ((S ,EMP ,EMP) (A ,EMP))
                                    ((A b (a)) (A ,EMP))
                                    ((A ,EMP (a)) (A ,EMP)))))

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(define pd (make-ndpda '(S A)
                       '(a b)
                       '(a b)
                       'S
                       '(A)
                       `(((S a ,EMP) (A (a)))
                         ((S a ,EMP) (A (b))))))

(define pd-numb>numa (grammar->sm numb>numa))

  
;;word stack-> boolean
;;purpose: Determine if the given word has an equal number of a's and b's
;;         and that the stack is empty
(define (K-INV a-word stck)
  (and (empty? stck)
       (= (length (filter (λ (w) (equal? w 'a)) a-word))
          (length (filter (λ (w) (equal? w 'b)) a-word)))))

;;word stack-> boolean
;;purpose: Determine if the given word has >= a's than b's with an a 
;;         and that the stack has only b's
(define (H-INV a-word stck)
  (and (andmap (λ (w) (not (equal? w 'a))) stck)
       (> (length (filter (λ (s) (equal? s 'a)) a-word))
          (length (filter (λ (s) (equal? s 'b)) a-word)))))

;;word stack-> boolean
;;purpose: Determine if the given word >= b's than a's than
;;         and that the stack has only a's
(define (F-INV a-word stck)
  (and (andmap (λ (w) (not (equal? w 'b))) stck))   
  (> (length (filter (λ (s) (equal? s 'b)) a-word))
     (length (filter (λ (s) (equal? s 'a)) a-word))))

;;word stack-> boolean
;;purpose: Determine if the given word has a differing amount of b's and a's
;;         and that the stack has the same amount of a's and b's 
(define (M-INV a-word stck)
  (not (and (= (length (filter (λ (w) (equal? w 'a)) stck))
               (length (filter (λ (w) (equal? w 'b)) stck)))
            (not (= (length (filter (λ (w) (equal? w 'b)) a-word))
                    (length (filter (λ (w) (equal? w 'a)) a-word)))))))

;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
#;(define (P-S-INV a-word stck)
    (let ([num-as (length (filter (λ (w) (eq? w 'a)) a-word))]
          [num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
      (and (or (<= num-as num-bs)
               (<= num-as (/ num-bs 2)))
           (andmap (λ (w) (eq? w 'b)) stck))))
;;Purpose: to determine if the number of b's in the stack is greater than or equal to the number of b's in the ci
(define (P-X-INV a-word stck)
  (let ([word-num-bs (length (filter (λ (w) (eq? w 'b)) a-word))]
        [stck-num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (>= stck-num-bs word-num-bs)))
;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-A-INV a-word stck)
  (let ([num-as (length (filter (λ (w) (eq? w 'a)) a-word))]
        [num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (and (or (<= num-as num-bs)
             (<= num-as (/ num-bs 2)))
         (andmap (λ (w) (eq? w 'b)) stck))))
;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-B-INV a-word stck)
  (let ([num-as (length (filter (λ (w) (eq? w 'a)) a-word))]
        [num-bs (length (filter (λ (w) (eq? w 'b)) stck))])
    (and (or (<= num-as num-bs)
             (<= num-as (/ num-bs 2)))
         (andmap (λ (w) (eq? w 'b)) stck))))

;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-S-INV a-word stck)
  (and (andmap (λ (w) (eq? w 'b)) stck)
       (andmap (λ (w) (eq? w 'a)) a-word)
       (= (* 2 (length a-word)) (length stck))))
;;Purpose: to determine if the number of b's in the stack is greater than or equal to the number of b's in the ci
(define (P-H-INV ci stck)
  (let ([ci-as (filter (λ (w) (eq? w 'a)) ci)]
        [ci-bs (filter (λ (w) (eq? w 'b)) ci)])
    (and (equal? ci (append ci-as ci-bs))
         (andmap (λ (w) (eq? w 'b)) stck)
         (<= (length ci-as) (length (append ci-bs stck)) (* 2 (length ci-as))))))

(define (P-H1-INV ci stck)
  (let ([ci-as (filter (λ (w) (eq? w 'a)) ci)]
        [ci-bs (filter (λ (w) (eq? w 'b)) ci)])
    (and (equal? ci (append ci-as ci-bs))
         (<= (length ci-as) (length ci-bs) (* 2 (length ci-as)))
         (andmap (λ (w) (eq? w 'b)) stck)
         (<= (length ci-as) (length (append ci-bs stck)) (* 2 (length ci-as))))))


;;"note to self:"
;;"edit A and D to scroll thru word, not jump to end"


(define (pd-A-INV a-wrd a-stck)
  (andmap (λ (s) (eq? s 'a)) a-stck))


(define (p-xds-inv wrd stck)
  (and (empty? wrd)
       (empty? stck)))

(define (pd-a-inv wrd stck)
  (or (not (= (length (filter (λ (w) (equal? w 'a)) wrd)) 4))
      (not (= (length (filter (λ (w) (equal? w 'b)) wrd)) 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;States (i = head's position)
;; K - tape[1..i-1] contains an even amount of a's and even bs
;; H - tape[1..i-1] contains an odd amount of a's and even bs
;; I - tape[1..i-1] contains an odd amount of b's and even as
;; B - tape[1..i-1] contains an odd amount of a's and odd bs
;; S - tape[i] = blank AND tape[1..i-1] contains an even amount of a's and even bs, final state

;;Pre-condition = tape = LMw_ AND i = 0 
(define EVEN-AS-&-BS (make-unchecked-tm '(K H I B S)
                                        '(a b)
                                        `(((K ,BLANK) (S ,BLANK))
                                          ((K a) (H ,RIGHT)) ((H a) (K ,RIGHT)) ((H b) (B ,RIGHT)) ((B b) (H ,RIGHT))
                                          ((K b) (I ,RIGHT)) ((I b) (K ,RIGHT)) ((I a) (B ,RIGHT)) ((B a) (I ,RIGHT)))
                                        'K
                                        '(S)
                                        'S))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an even # of a's and b's
(define (EVEN-K-INV tape headpos)
  (or (= headpos 0)
      (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
            (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
        (and (even? num-as)
             (even? num-bs)))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an odd # of a's and even # of b's
(define (EVEN-H-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (odd? num-as)
         (even? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an odd # of a's and even # of b's
(define (BRK-EVEN-H-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (even? num-as)
         (even? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an even # of a's and odd # of b's
(define (EVEN-I-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (even? num-as)
         (odd? num-bs))))

(define (BRK-EVEN-I-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (not (even? num-as))
         (odd? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an odd # of a's and odd # of b's
(define (EVEN-B-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (odd? num-as)
         (odd? num-bs))))


(define (BRK-EVEN-B-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (not (odd? num-as))
         (odd? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an even # of a's and b's and tape[i] = BLANK
(define (EVEN-S-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (even? num-as)
         (even? num-bs)
         (eq? (list-ref tape headpos) BLANK))))

(define tm-a* (make-unchecked-tm '(S Y N)
                              '(a b)
                              `(((S a) (S ,RIGHT))
                                ((S b) (N b))
                                ((S ,BLANK) (Y ,BLANK)))
                              'S
                              '(Y N)
                              'Y))

;; States (i is the position of the head)
;; S: no tape elements read, starting sate
;; A: tape[1..i-1] has only a
;; B: tape[1..i-1] has only a
;; C: tape[1..i-2] has only a and tape[i-1] = b
;; Y: tape[i] = BLANK and tape[1..i-1] = a* or a*b,
;; final accepting state
;; N: tape[1..i-1] != a* or a*b, final state
;; L = a* U a*b
;; PRE: tape = LMw ANDi=1
(define a*Ua*b (make-unchecked-tm '(S A B C Y N)
                                  '(a b)
                                  `(((S ,BLANK) (Y ,BLANK))
                                    ((S a) (A ,RIGHT))
                                    ((S a) (B ,RIGHT))
                                    ((S b) (C ,RIGHT))
                                    ((A a) (A ,RIGHT))
                                    ((A ,BLANK) (Y ,BLANK))
                                    ((B a) (B ,RIGHT))
                                    ((B b) (C ,RIGHT))
                                    ((C a) (N ,RIGHT))
                                    ((C b) (N ,RIGHT))
                                    ((C ,BLANK) (Y ,BLANK)))
                                  'S
                                  '(Y N)
                                  'Y))

;; tape natnum → Boolean
;; Purpose: Determine that no tape elements read
(define (tm-S-INV t i) (= i 1))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-1] only has a
(define (A-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-1] only has a
(define (tm-B-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-2] has only a and
;; tape[i-1] = b
(define (tm-C-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (- i 2)))
       (eq? (list-ref t (sub1 i)) 'b)))

;; tape natnum → Boolean
;; Purpose: Determine that tape[i] = BLANK and
;; tape[1..i-1] = a* or tape[1..i-1] = a*b
(define (Y-INV t i)
  (or (and (= i 2) (eq? (list-ref t (sub1 i)) BLANK))
      (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))
      (let* [(front (takef (rest t) (λ (s) (eq? s 'a))))
             (back (takef (drop t (add1 (length front)))
                          (λ (s) (not (eq? s BLANK)))))]
        (equal? back '(b)))))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-1] != a* or a*b
(define (N-INV t i)
  (and (not (andmap (λ (s) (eq? s 'a))
                    (take (rest t) (sub1 i))))
       (let* [(front (takef (rest t) (λ (s) (eq? s 'a))))
              (back (takef (drop t (add1 (length front)))
                           (λ (s) (not (eq? s BLANK)))))]
         (not (equal? back '(b))))))

(define anbncn (make-tm
                '(S A B C D E F G H I J K L Y)
                '(a b c x)
                `(((S ,BLANK) (J ,RIGHT))
                  ((J ,BLANK) (Y ,BLANK))
                  ((J a) (A ,RIGHT))
                  ((A a) (A ,RIGHT))
                  ((A b) (B ,RIGHT))
                  ((B b) (B ,RIGHT))
                  ((B c) (C ,RIGHT))
                  ((C c) (C ,RIGHT))
                  ((C ,BLANK) (D ,LEFT))
                  ((D a) (D ,LEFT))
                  ((D b) (D ,LEFT))
                  ((D c) (D ,LEFT))
                  ((D x) (D ,LEFT))
                  ((D ,BLANK) (E ,RIGHT))
                  ((E x) (E ,RIGHT))
                  ((E a) (F x))
                  ((E a) (H x))
                  ((F a) (F ,RIGHT))
                  ((F b) (G x))
                  ((F x) (F ,RIGHT))
                  ((G b) (G ,RIGHT))
                  ((G x) (G ,RIGHT))
                  ((G c) (D x))
                  ((H x) (H ,RIGHT))
                  ((H b) (I x))
                  ((I x) (I ,RIGHT))
                  ((I c) (K x))
                  ((K x) (L ,RIGHT))
                  ((L ,BLANK) (Y ,BLANK)))
                'S
                '(Y)
                'Y))

(define FBR (make-tm '(S A F)
                     '(a b)
                     `(((S a) (A ,RIGHT))
                       ((S b) (A ,RIGHT))
                       ((S ,BLANK) (A ,RIGHT))
                       ((A a) (A ,RIGHT))
                       ((A b) (A ,RIGHT))
                       ((A ,BLANK) (F ,BLANK)))
                     'S
                     '(F)))

(define ADD (make-tm '(S A B C D E F G)
                     '(d)
                     `(((S ,BLANK) (A ,RIGHT))
                       ((A d) (A ,RIGHT))
                       ((A ,BLANK) (B d))
                       ((B d) (B ,RIGHT))
                       ((B ,BLANK) (C ,LEFT))
                       ((C d) (D ,BLANK))
                       ((D ,BLANK) (E ,LEFT))
                       ((E d) (F ,RIGHT))
                       ((E ,BLANK) (G ,RIGHT))
                       ((G ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))

(define PR^N (make-tm '(S A B D E F G H I J K M N Q Y)
                      '(a b h i r)
                      `(((S ,BLANK) (A ,RIGHT)) ;; start the simulation
                        ((A i) (Q ,RIGHT)) ;; move to next instruction if any
                        ((Q ,BLANK) (Y ,BLANK)) ;; check if done
                        ((Q r) (M i)) ;; Otherwise swap i and r in previous position
                        ((M i) (N ,LEFT)) 
                        ((N i) (B r))
                        ((B r) (B ,RIGHT)) ;; find h
                        ((B i) (B ,RIGHT))
                        ((B ,BLANK) (B ,RIGHT))
                        ((B a) (B ,RIGHT))
                        ((B b) (B ,RIGHT))
                        ((B h) (D ,RIGHT))
                        ((D a) (E h)) ;; read a and move simultaed head right
                        ((D b) (G h)) ;; read b and move simultaed head right
                        ((D ,BLANK) (I h)) ;; read blank and move simultaed head right
                        ((E h) (F ,LEFT)) ;; move simulated head left after a read
                        ((F h) (K a))     ;; substitute h with a
                        ((G h) (H ,LEFT)) ;; move simulated head left after b read
                        ((H h) (K b))     ;; substitute h with b
                        ((I h) (J ,LEFT)) ;; move simulated head left after blank read
                        ((J h) (K ,BLANK)) ;; substitute h with blank
                        ((K a) (K ,LEFT)) ;; find i
                        ((K b) (K ,LEFT))
                        ((K ,BLANK) (K ,LEFT))
                        ((K r) (K ,LEFT))
                        ((K i) (Q ,RIGHT))) 
                      'S
                      '(Y)))

;; word symbol → word
;; Purpose: Return the subword at the front of the
;; given word that only contains the given
;; symbol
(define (front-symbs w s)
  (takef w (λ (a) (eq? a s))))

;; tape natnum → Boolean
;; Purpose: Determine that head is in position 1 and
;; tape[i] = BLANK
(define (tm-S-INV1 t i) (and (= i 1) (eq? (list-ref t i) BLANK)))

(define (tm-A-INV1 t i)
  (and (> i 2)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))]
         (equal? as w))))


;; tape natnum → Boolean
;; Purpose: Determine head in position > 2 and
;; tape[2..i-1] = a+b+
(define (tm-B-INV1 t i)
  (and (> i 3)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))
              (w-as (drop w (length as)))
              (bs (front-symbs w-as 'b))]
         (and (equal? w (append as bs))
              (not (empty? as))
              (not (empty? bs))))))

;; tape natnum → Boolean
;; Purpose: Determine head in position > 2 and
;; tape[2..i-1] = a+b+
(define (BRK-B-INV1 t i)
  (not (and (> i 3)
            (let* [(w (drop (take t i) 2))
                   (as (front-symbs w 'a))
                   (w-as (drop w (length as)))
                   (bs (front-symbs w-as 'b))]
              (and (equal? w (append as bs))
                   (not (empty? as))
                   (not (empty? bs)))))))

;; tape natnum → Boolean
;; Purpose: Determine head in position > 3 and
;; tape[2..i-1]=a+b+c+
(define (C-INV1 t i)
  (and (> i 4)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))
              (w-as (drop w (length as)))
              (bs (front-symbs w-as 'b))
              (w-asbs (drop w-as (length bs)))
              (cs (front-symbs w-asbs 'c))]
         (and (equal? w (append as bs cs))
              (not (empty? as))
              (not (empty? bs))
              (not (empty? cs))))))


;; tape natnum → Boolean
;; Purpose: Determine that head position is >= 1 and that
;; tape[i]=xna+xnb+xnc+
(define (D-INV t i)
  (and (>= i 1)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 0)
              (= (length xs1) (length xs2) (length xs3))))))


;; tape natnum → Boolean
;; Purpose: Determine head in position > 1 and
;; input word = xna+xnb+xnc+
(define (E-INV t i)
  (and (> i 1)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 0)
              (= (length xs1) (length xs2) (length xs3))))))


(define (BRK-E-INV t i)
  (not (and (> i 1)
            (let* [(w (takef (drop t 2)
                             (λ (s) (not (eq? s BLANK)))))
                   (xs1 (front-symbs w 'x))
                   (w-xs1 (drop w (length xs1)))
                   (as (front-symbs w-xs1 'a))
                   (w-xs1as (drop w-xs1 (length as)))
                   (xs2 (front-symbs w-xs1as 'x))
                   (w-xs1asxs2 (drop w-xs1as (length xs2)))
                   (bs (front-symbs w-xs1asxs2 'b))
                   (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
                   (xs3 (front-symbs w-xs1asxs2bs 'x))
                   (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
                   (cs (front-symbs w-xs1asbsxs3 'c))]
              (and (equal? w (append xs1 as xs2 bs xs3 cs))
                   (> (length as) 0)
                   (> (length bs) 0)
                   (> (length cs) 0)
                   (= (length xs1) (length xs2) (length xs3)))))))


;; tape natnum → Boolean
;; Purpose: Determine head in position > 1 and
;; input word = xn+1a+xnbb+xncc+
(define (sm-F-INV t i)
  (and (> i 1)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 1)
              (> (length cs) 1)
              (= (sub1 (length xs1))
                 (length xs2)
                 (length xs3))))))

;; tape natnum → Boolean
;; Purpose: Determine head in position > 2 and
;; tape=xn+1a+xn+1b+xncc+
(define (G-INV t i)
  (and (> i 3)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 1)
              (= (sub1 (length xs1))
                 (sub1 (length xs2))
                 (length xs3))))))


;; tape natnum → Boolean
;; Purpose: Determine tape[i]=x^+bx^+c and |xs|%3 = 1 and
;; |x^+b| = 2*|x^+c|
(define (tm-H-INV t i)
  (and (> i 1)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (b (front-symbs w-xs1 'b))
              (w-xs1b (drop w-xs1 (length b)))
              (xs2 (front-symbs w-xs1b 'x))
              (w-xs1bxs2 (drop w-xs1b (length xs2)))
              (c (front-symbs w-xs1bxs2 'c))]
         (and (equal? w (append xs1 b xs2 c))
              (= (add1 (length xs1)) (* 2 (add1 (length xs2))))
              (= (length b) 1)
              (= (length c) 1)
              (= (remainder (length (append xs1 xs2)) 3) 1)))))

;; tape natnum → Boolean
;; Purpose: Determine tape[i]=x*c and |xs|%3 = 2
(define (I-INV t i)
  (and (> i 2)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (c (front-symbs w-xs1 'c))]
         (and (equal? w (append xs1 c))
              (= (length c) 1)
              (= (remainder (length xs1) 3) 2)))))

;; tape natnum → Boolean
;; Purpose: Determine that head’s position is 2 and
;; tape[1] = BLANK
(define (J-INV tape i)
  (and (= i 2) (eq? (list-ref tape (sub1 i)) BLANK)))


;; tape natnum → Boolean
;; Purpose: Determine if w = xxxx* and |xs|%3 = 0
;; and tape[i] = x
(define (tm-K-INV t i)
  (let [(w (drop-right (drop t 2) 1))]
    (and (eq? (list-ref t i) 'x) ;;;
         (andmap (λ (s) (eq? s 'x)) w)
         (>= (length w) 3)
         (= (remainder (length w) 3) 0)
         (= i (add1 (length w))))))

;; tape natnum → Boolean
;; Purpose: Determine that w = xxxx* and |xs|%3 = 0 and
;; i = |w| + 2
(define (L-INV t i)
  (let [(w (drop-right (drop t 2) 1))]
    (and (andmap (λ (s) (eq? s 'x)) w)
         (>= (length w) 3)
         (= (remainder (length w) 3) 0)
         (= i (+ (length w) 2)))))

;; tape natnum → Boolean
;; Purpose: Determine input word = x* and |xs|%3 = 0
(define (Y-INV1 t i)
  (let* [(w (drop-right (drop t 2) 1))]
    (and (andmap (λ (s) (eq? s 'x)) w)
         (= (remainder (length w) 3) 0))))



;(sm-viz EVEN-AS-&-BS '(@ a b a b) 0)
;(sm-viz anbncn `(,LM ,BLANK a b c) #:head-pos 1 #:cut-off 15)

#;(sm-viz EVEN-AS-&-BS `(,LM b a b a) 0 (list 'K EVEN-K-INV)
          (list 'H EVEN-H-INV)
          (list 'I BRK-EVEN-I-INV)
          (list 'B EVEN-B-INV)
          (list 'S EVEN-S-INV))
;(sm-viz ADD `(,LM ,BLANK d d ,BLANK d d d) 1)
#;(sm-viz anbncn `(,LM ,BLANK a b c) 1 (list 'S S-INV)
          (list 'A A-INV)
          (list 'B B-INV)
          (list 'C C-INV)
          (list 'Y Y-INV)
          (list 'N N-INV))

#;(parameterize ([testing? #t])
  
  (sm-viz pd-numb>numa '(a b) #:cut-off 5)
  (sm-viz pd-numb>numa '(a b) #:cut-off 1)
  (sm-viz pd-numb>numa '(a b) #:cut-off 10) ;;needs to be looked into
  (sm-viz pd-numb>numa '(a b a) #:cut-off 5)
  ;(sm-viz pd-numb>numa '(a b) #:cut-off 0) ;;needs to be looked into 
  ;(sm-viz pd-numb>numa '(a b) #:cut-off -3)
  (sm-viz more-a-than-b '(a a a a a b b))
  (sm-viz a* '(a a a a a)
          (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (= (length w) 3)) (empty? s)))))
  (sm-viz aa* '(a a)
          (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (empty? w) (empty? s)))))
  (sm-viz a* '(a a)
          (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s)))))
  (sm-viz P2 '(a a a b b) (list 'S P-S-INV) (list 'H P-H-INV))
  (sm-viz a* '(a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                 a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                 a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a)
          (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s))))) ;; look into
  (sm-viz P2 '(a a a b b) (list 'S P-S-INV) (list 'H P-H-INV)) ;;needs to be looked into

  (sm-viz P2 '(a a a b b))

  (sm-viz P2 '(a a a b b) (list 'S P-S-INV))
  (sm-viz AB*B*UAB* '(a b b))

  ;;accept examples
  (sm-viz AB*B*UAB* '(a b b))
  (sm-viz p2-ndfa '(a b b))
  (sm-viz missing-exactly-one '(a a a a b b b b a a b b a a))
  (sm-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b))
  (sm-viz aa-ab '(a a a a))
  (sm-viz ends-with-two-bs '(a b a b a b b b))
  (sm-viz ends-with-two-bs '(a b a b a b b a a a b b))
  ;;reject examples
  (sm-viz AB*B*UAB* '(a b b a))
  (sm-viz p2-ndfa '(a b b a))
  (sm-viz missing-exactly-one '(a a a a b b b b a a b b a a c))
  (sm-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b a))
  (sm-viz aa-ab '(a b b b a))
  (sm-viz AT-LEAST-ONE-MISSING '(a b c))
  (sm-viz p2-ndfa '(a b a b))
  (sm-viz AB*B*UAB* '(a b a b))

  ;;Invariant examples
  (sm-viz AT-LEAST-ONE-MISSING '(a b c)
          (list 'S S-INV)
          (list 'A ALON-A-INV)
          (list 'B ALON-B-INV)
          (list 'C ALON-C-INV)) 
  (sm-viz EVEN-NUM-Bs '(a b b b a b b) 
          (list 'S EVEN-NUM-Bs-S-INV)
          (list 'F EVEN-NUM-Bs-F-INV))

  (sm-viz AB*B*UAB* '(a b b b b)
          (list 'S S-INV)
          (list 'K ND-K-INV)
          (list 'B B-INV)
          (list 'C C-INV)
          (list 'H ND-H-INV))


  (sm-viz n '(b a a))
  (sm-viz nk '(b a a))
  (sm-viz aa-ab '(a a a a b a))
  (sm-viz aa-ab '(a a a a b a) #:add-dead #t)
  (sm-viz aa-ab '(a a a a b a) #:add-dead 3)

  (sm-viz aa-ab '(a a a a a a a))
  (sm-viz ends-with-two-bs '(a a a a b b a b b b))
  (sm-viz aa-ab '(a a a a a a a) (list 'S S-INV) (list 'A A-INV1) (list 'B B-INV1) (list 'F F-INV1) #:add-dead #t)
  (sm-viz DNA-SEQUENCE '(a t c g t a c) (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV))
  (sm-viz DNA-SEQUENCE '(c g c g a t a t g c t a g c a t)  (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV))

  (sm-viz ND4 '(a b b b) #:add-dead #t)

  (sm-viz M2 '(a a b b b b) #:add-dead #t)

  (sm-viz DNA-SEQUENCE '(c g c g a t a t g c t a g c a t)  (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV))

  (sm-viz DNA-SEQUENCE '(c g c g a t a t g c t a g c a t)  (list 'K DNA-K-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV))

  (sm-viz DNA-SEQUENCE '(c g c g a t a t g c t a g c a t)))
