#lang racket

(require "sm-viz.rkt"
         "../../fsm-core/interface.rkt")


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
  (and (not (empty? a-word)) ;(not
                              (equal? (last a-word) 'a)))
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
  (and (not (empty? ci))
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

(define nd-a* (make-ndfa '(K H)
                         '(a b)
                         'K
                         '(H)
                         `((K ,EMP H)
                           (H a H))))

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


;;"note to self:"
;;"edit A and D to scroll thru word, not jump to end"

(define pd (make-ndpda '(S A)
                       '(a b)
                       '(a b)
                       'S
                       '(A)
                       `(((S a ,EMP) (A (a)))
                         ((S a ,EMP) (A (b))))))

(define (pd-A-INV a-wrd a-stck)
  (andmap (λ (s) (eq? s 'a)) a-stck))


(define (p-xds-inv wrd stck)
  (and (empty? wrd)
       (empty? stck)))

(define (pd-a-inv wrd stck)
  (or (not (= (length (filter (λ (w) (equal? w 'a)) wrd)) 4))
      (not (= (length (filter (λ (w) (equal? w 'b)) wrd)) 2))))

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(define pd-numb>numa (grammar->sm numb>numa))
;(sm-viz pd-numb>numa '(a b) #:cut-off 5)
;(sm-viz pd-numb>numa '(a b) #:cut-off 1)
;(sm-viz pd-numb>numa '(a b) #:cut-off 10) ;;needs to be looked into
;(sm-viz pd-numb>numa '(a b a) #:cut-off 5)
;(sm-viz pd-numb>numa '(a b) #:cut-off 0) ;;needs to be looked into 
;(sm-viz pd-numb>numa '(a b) #:cut-off -3)
;(sm-viz more-a-than-b '(a a a a a b b))
#;(sm-viz a* '(a a a a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (= (length w) 3)) (empty? s)))))
#;(sm-viz aa* '(a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (empty? w) (empty? s)))))
#;(sm-viz a* '(a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s)))))
;(sm-viz P2 '(a a a b b) (list 'S P-S-INV) (list 'H P-H-INV))
#;(sm-viz a* '(a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                 a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a
                 a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a)
           (list 'K (λ (w s) (and (empty? w) (empty? s)))) (list 'H (λ (w s) (and (not (empty? w)) (empty? s))))) ;; look into
;(sm-viz P2 '(a a a b b) (list 'S P-S-INV) (list 'H P-H-INV)) ;;needs to be looked into

;(sm-viz AB*B*UAB* '(a b b))
#|
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
|#
#;(sm-viz DNA-SEQUENCE '(c g c g a t a t g c t a g c a t)  (list 'K DNA-K-INV) (list 'H DNA-H-INV) (list 'F DNA-F-INV)
          (list 'M DNA-M-INV) (list 'I DNA-I-INV) (list 'D DNA-D-INV)  (list 'B DNA-B-INV) (list 'S DNA-S-INV) (list 'R DNA-R-INV))