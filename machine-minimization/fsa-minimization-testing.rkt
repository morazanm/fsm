#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/fsa.rkt"
         "fsa-minimization.rkt"
         "../sm-graph.rkt"
         "../visualizations/viz-sm-constructors/viz-minimization.rkt")

#;(define (remove-states-that-cannot-reach-finals a-ndfa))
  

(define EX1 (make-unchecked-dfa '(A B C D E)
                                '(0 1)
                                'A
                                '(E)
                                '((A 0 B) (A 1 C)
                                          (B 0 B) (B 1 D)
                                          (C 0 B) (C 1 C)
                                          (D 1 E) (D 0 B)
                                          (E 0 B) (E 1 C))
                                'no-dead))
; Q 0 1 2 3 4 5 6 7
(define EX2-trans (make-unchecked-dfa '(A B C D E F G H)
                                      '(0 1)
                                      'A
                                      '(C)
                                      '((A 0 B) (A 1 F)
                                                (B 0 G) (B 1 C)
                                                (C 0 A) (C 1 C)
                                                (D 0 C) (D 1 G)
                                                (E 0 H) (E 1 F)
                                                (F 0 C) (F 1 G)
                                                (G 0 G) (G 1 E)
                                                (H 0 G) (H 1 C))
                                      'no-dead))

;A  B  C  D  E  F  G  H
(define EX2-vid (make-unchecked-dfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                                    '(0 1)
                                    'Q0
                                    '(Q2)
                                    '((Q0 0 Q1) (Q0 1 Q5)
                                                (Q1 0 Q6) (Q1 1 Q2)
                                                (Q2 0 Q0) (Q2 1 Q2)
                                                (Q3 0 Q2) (Q3 1 Q6)
                                                (Q4 0 Q7) (Q4 1 Q5)
                                                (Q5 0 Q2) (Q5 1 Q6)
                                                (Q6 0 Q6) (Q6 1 Q4)
                                                (Q7 0 Q6) (Q7 1 Q2))
                                    'no-dead))

(define EX3-vid (make-unchecked-dfa '(A B C D E F)
                                    '(0 1)
                                    'A
                                    '(C D E)
                                    '((A 0 B) (A 1 C)
                                              (B 0 A) (B 1 D)
                                              (C 0 E) (C 1 F)
                                              (D 0 E) (D 1 F)
                                              (E 0 E) (E 1 F)
                                              (F 0 F) (F 1 F))
                                    'no-dead))

(define EX4-vid-unreachable-final (make-unchecked-dfa '(A B C D E F G)
                                                      '(0 1)
                                                      'A
                                                      '(F) ;;'(B C G)
                                                      '((A 0 B) (A 1 C)
                                                                (B 0 D) (B 1 E)
                                                                (C 0 E) (C 1 D)
                                                                (D 0 G) (D 1 G)
                                                                (E 0 G) (E 1 G)
                                                                (F 0 D) (F 1 E)
                                                                (G 0 G) (G 1 G))
                                                      'no-dead))

(define EX4-vid (make-unchecked-dfa '(A B C D E F G)
                                    '(0 1)
                                    'A
                                    '(B C G)
                                    '((A 0 B) (A 1 C)
                                              (B 0 D) (B 1 E)
                                              (C 0 E) (C 1 D)
                                              (D 0 G) (D 1 G)
                                              (E 0 G) (E 1 G)
                                              (F 0 D) (F 1 E)
                                              (G 0 G) (G 1 G))
                                    'no-dead))

(define EX5 (make-unchecked-dfa '(A B C D E F G H I)
                                '(0 1)
                                'A
                                '(B C G)
                                '((A 0 B) (A 1 C)
                                          (B 0 D) (B 1 E)
                                          (C 0 E) (C 1 D)
                                          (D 0 G) (D 1 G)
                                          (E 0 G) (E 1 G)
                                          (F 0 D) (F 1 E)
                                          (H 0 F) (H 1 I)
                                          (I 0 H) (I 1 F)
                                          (G 0 G) (G 1 G))
                                'no-dead))

(define EX5-vid (make-unchecked-dfa '(A B C D E F)
                                    '(0 1)
                                    'A
                                    '(D C E)
                                    '((A 0 B) (A 1 C)
                                              (B 0 A) (B 1 D)
                                              (C 0 E) (C 1 F)
                                              (D 0 E) (D 1 F)
                                              (E 0 E) (E 1 F)
                                              (F 0 F) (F 1 F))
                                    'no-dead))

(define EX6-vid (make-unchecked-dfa '(A B C D E)
                                    '(0 1)
                                    'A
                                    '(E)
                                    '((A 0 B) (A 1 C)
                                              (B 0 B) (B 1 D)
                                              (C 0 B) (C 1 C)
                                              (D 0 B) (D 1 E)
                                              (E 0 B) (E 1 C))
                                    'no-dead))

;; L = ab* U (ab)*
(define M (make-unchecked-ndfa '(S A B C D E F G H I)
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


(define L (make-unchecked-ndfa '(S A B C D E)
                               '(a b)
                               'S
                               '(S B C E)
                               `((S ,EMP A) (S ,EMP C) (A a B) (B b B)
                                            (C a D) (C b E) (D b C) (E b E))))


(define aa*Uab* (make-unchecked-ndfa '(K B D)
                                     '(a b)
                                     'K
                                     '(B D)
                                     `((K a D) (K a B)
                                               (B a B)
                                               (D b D))))

(define AT-LEAST-ONE-MISSING
  (make-unchecked-ndfa '(S A B C)
                       '(a b c)
                       'S
                       '(A B C)
                       `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                                    (A b A) (A c A)
                                    (B a B) (B c B)
                                    (C a C) (C b C))))

(define d (ndfa->dfa AT-LEAST-ONE-MISSING))

(define p2-ndfa
  (make-unchecked-ndfa '(S A B C D E)
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
  (make-unchecked-ndfa '(S K B C H)
                       '(a b)
                       'S
                       '(H)
                       `((S ,EMP K) (S a C)
                                    (K a B) (K ,EMP H)
                                    (B b K)
                                    (C ,EMP H)
                                    (H b H))))

(define AB*B*UAB*2
  (make-unchecked-ndfa '(S K B C H)
                       '(a b)
                       'S
                       '(H)
                       `((S ,EMP K) (S a C)
                                    (K a B) (K ,EMP H)
                                    (B b K)
                                    (C ,EMP H)
                                    (H b H) (H a S))))

(define aa-ab
  (make-unchecked-ndfa `(S A B F)
                       '(a b)
                       'S
                       '(A B F)
                       `((S a A) (S a B) (S ,EMP F)
                                 (A a A)
                                 (B b B))))

(define ends-with-two-bs
  (make-unchecked-ndfa `(S A B)
                       '(a b)
                       'S
                       '(B)
                       `((S a S) (S b S) (S b A)
                                 (A b B)
                                 (B b B))))
(define ENDS-WITH-TWO-Bs
  (make-unchecked-ndfa `(S A B)
                       '(a b)
                       'S
                       '(B)
                       `((S a S) (S b A)
                                 (A b B) (A a S)
                                 (B b B) (B a S))))

(define nd-a* (make-unchecked-ndfa '(K H)
                                   '(a b)
                                   'K
                                   '(H)
                                   `((K ,EMP H)
                                     (H a H))))

(define missing-exactly-one
  (make-unchecked-ndfa '(S A B C D E F G H I J K L M N O P)
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

(define nd (make-unchecked-ndfa '(S Z Y A B)
                                '(a b)
                                'S
                                '(B)
                                `((S b Z)
                                  (S b Y)
                                  (Y a A)
                                  (Z a A)
                                  (A a B))))

(define n (make-unchecked-ndfa '(K H F M I)
                               '(a b)
                               'K
                               '(I)
                               `((K b H)
                                 (H ,EMP F)
                                 (H ,EMP M)
                                 (F a I)
                                 (M a I))))

(define nk (make-unchecked-ndfa '(K H F M I)
                                '(a b)
                                'K
                                '(I)
                                `((K b H)
                                  (H ,EMP F)
                                  (F ,EMP M)
                                  (M ,EMP I)
                                  (I ,EMP H))))

(define ab*-U-ab*b*-ndfa 
  (make-unchecked-ndfa '(S A B C D E)
                       '(a b)
                       'S
                       '(C E)
                       `((S ,EMP A) (S ,EMP D) (A a B) (A ,EMP C)
                                    (B b A) (C b C) (D a E) (E b E))))


(define PROP-BI (make-unchecked-dfa '(S M N)
                                    '(0 1)
                                    'S
                                    '(N M)
                                    '((S 1 M)
                                      (S 0 N)
                                      (M 0 M)
                                      (M 1 M))))

;;failed min4 ;; PASSED min 5
(define DNA-SEQUENCE (make-unchecked-dfa '(K H F M I D B S R) ;C)
                                         '(a t c g)
                                         'K
                                         '(K F I B R)
                                         `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                                   (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                                   (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                                   (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

;; L = (aba)* U (ab)*
(define ND
  (make-unchecked-ndfa '(S A B C D E)
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
  (make-unchecked-ndfa
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
  (make-unchecked-ndfa '(S A B C D)
                       '(a b)
                       'S
                       '(B)
                       `((S ,EMP A) (S ,EMP B)
                                    (A a A) (A ,EMP C)
                                    (C ,EMP D)
                                    (D ,EMP B)
                                    (B b B))))

(define ND4 (make-unchecked-ndfa '(S ds)
                                 '(a b)
                                 'S
                                 '(ds)
                                 `((S a ds)
                                   (ds a ds))))

(define ND5
  (make-unchecked-ndfa '(S A B C D)
                       '(a b)
                       'S
                       '(B)
                       `((S ,EMP A) 
                         (A ,EMP B)
                         (B ,EMP C)
                         (C ,EMP D)
                         (D ,EMP S))))

(define EVEN-NUM-Bs
  (make-unchecked-dfa '(S F)
                      '(a b)
                      'S
                      '(S)
                      `((S a S) (S b F)
                                (F a F) (F b S))
                      'no-dead))

(define M2 (make-unchecked-dfa `(S A F ,DEAD)
                               '(a b)
                               'S
                               '(F)
                               `((S a A) (S b ,DEAD) (,DEAD a ,DEAD) (,DEAD b ,DEAD)
                                         (A a ,DEAD) (A b F)
                                         (F a ,DEAD) (F b F))
                               'no-dead))


(define for-david (make-unchecked-ndfa
                   '(S A B C D E F G H I J K L M N O P Q R S-0 T U V W X Y Z A-0 B-0 C-0 D-0 E-0 F-0 G-0 H-0 I-0 J-0)
                   '(a b c d e f g h i h k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9)
                   'S
                   '(J-0)
                   '((S a A) (A b B) (B c C) (C d D) (D e E)
                             (E f F) (F g G) (G h H) (H i I)
                             (I j J) (J k K) (K l L) (L m M)
                             (M n N) (N o O) (O p P) (P q Q)
                             (Q r R) (R s S-0) (S-0 t T) (T u U)
                             (U v V) (V w W) (W x X) (X y Y)
                             (Y z Z) (Z 0 A-0) (A-0 1 B-0)
                             (B-0 2 C-0) (C-0 3 D-0) (D-0 4 E-0)
                             (E-0 5 F-0) (F-0 6 G-0) (G-0 7 H-0)
                             (H-0 8 I-0) (I-0 9 J-0))))

;; L = {w | w in (a b)* ^ |w| is odd}
(define ODDL (make-unchecked-dfa '(S I J)
                                 '(a b)
                                 'S
                                 '(I J)
                                 '((S a I) (S b J)
                                           (I a S) (I b S)
                                           (J a S) (J b S))
                                 'no-dead))

(define listofmachines
  (list EX1 EX2-trans EX3-vid EX4-vid EX5 EX5-vid EX6-vid ODDL M L aa*Uab* AT-LEAST-ONE-MISSING p2-ndfa AB*B*UAB* AB*B*UAB*2 aa-ab ends-with-two-bs
        nd n nk ab*-U-ab*b*-ndfa PROP-BI DNA-SEQUENCE ND ND2 ND3 ND4 ND5 ENDS-WITH-TWO-Bs nd-a* missing-exactly-one EVEN-NUM-Bs M2))
#|
(struct status (M result) #:transparent)

"minimize 5 - myhill-nerode"
(define minimize5-test (map (λ (M) (status (M 'whatami) (boolean? (test-equiv-fsa (ndfa->dfa M) (minimize-dfa M))))) listofmachines))
"all passed?"
(andmap (λ (s) (status-result s)) minimize5-test)

"total"
(length listofmachines)
"pass"
(length (filter (λ (s) (status-result s)) minimize5-test))
"fail"
(- (length listofmachines) (length (filter (λ (s) (status-result s)) minimize5-test)))
"success rate"
(* 100 (/ (length (filter (λ (s) (status-result s)) minimize5-test)) (length listofmachines)))
|#
#;(map (λ (M)
         (time (minimization-viz M)))
       listofmachines)

;(minimization-viz EX3-vid)
;(minimization-viz AB*B*UAB*2)
;(minimization-viz AT-LEAST-ONE-MISSING)

;(time (minimization-viz EX3-vid))


"bug is in losp the order, isnt fixed so things are being shown out of order, probably show rewrite file."