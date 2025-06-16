#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/misc.rkt"
         #;(only-in "../fsm-core/interface.rkt"
                  ndfa->dfa
                  EMP
                  DEAD
                  make-uncheckeddfa
                  make-uncheckedndfa)
         "../sm-graph.rkt")


(struct equivalence-class (non-final final) #:transparent)

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






(define (minimize-dfa M)
  (let* ([dfa (ndfa->dfa M)]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [equivalence-class (make-equivalence-classes (filter (λ (s) (not (member s finals))) (fsa-getstates dfa)) finals transition-table)])
    (equivalence-class->dfa (fsa-getalphabet dfa) (fsa-getstart dfa) finals equivalence-class transition-table)))


(define (equivalence-class->dfa alphabet start finals EC transition-table)
  (let* ([merged-states (cond  [(set-member? (equivalence-class-non-final EC) start) start]
                               [(not (set-empty? (equivalence-class-non-final EC))) (first (set->list (equivalence-class-non-final EC)))]
                               ['()])]
        [other-states (append-map set->list (equivalence-class-final EC))]
        [new-states (if (empty? merged-states)
                        other-states
                        (cons merged-states other-states))]
        [table->rules (filter (λ (rule)
                                (and (list? (member (first rule) new-states))
                                     (list? (member (third rule) new-states))))
                              (foldl (λ (row acc)
                                       (if (list? (member (first row) new-states))
                                           (append (map (λ (r) (if (list? (member (second r) new-states))
                                                                   (cons (first row) r)
                                                                   (list (first row) (first r) merged-states)))
                                                                    (second row)) acc)
                                           acc))
                                     '()
                                     transition-table))])
  (make-unchecked-dfa new-states
                      alphabet
                      start
                      finals
                      table->rules
                      'no-dead)))

(define (make-transition-table dfa)
  (let ([states (fsa-getstates dfa)]
        [rules (fsa-getrules dfa)])
    (for/list ([state states]
               #:do [(define applicable-rules (filter-map (λ (rule) (and (eq? state (first rule))
                                                                         (list (second rule) (third rule))))
                                                          rules))])
      (list state applicable-rules))))
    


;;establish equivalence classes where

(define (make-equivalence-classes states finals transition-table)
  (let ([first-equivalence-class (equivalence-class (list->set states) (list (list->set finals)))])
    
    (define (equivalence? matchee matcher last-ec-non-finals)
      (define (same-transitions? matchee-transitions matcher-transitions)
        (list? (andmap (λ (transition) (member transition matchee-transitions)) matcher-transitions)))

      (define (partial-match? matchee-transitions matcher-transitions)
        (list? (ormap (λ (t) (member t matchee-transitions)) matcher-transitions)))
      (let ([matchee-transitions (first (filter-map (λ (row) (and (eq? matchee (first row))
                                                           (second row)))
                                             transition-table))]
            [matcher-transitions (first (filter-map (λ (row) (and (eq? matcher (first row))
                                                           (second row)))
                                             transition-table))])
        (or (same-transitions? matchee-transitions matcher-transitions)
            (and (partial-match? matchee-transitions matcher-transitions)
                 (andmap (λ (t) (set-member? last-ec-non-finals (second t))) matchee-transitions)
                 (andmap (λ (t) (set-member? last-ec-non-finals (second t))) matcher-transitions)))))
                 
           
    (define (make-equivalence matchee potential-matches new-ec last-ec-non-finals)
      (cond [(set-empty? potential-matches)
             (struct-copy equivalence-class
                          new-ec
                          [final (cons (set matchee) (equivalence-class-final new-ec))])]
            [(equivalence? matchee (set-first potential-matches) last-ec-non-finals)
             (struct-copy equivalence-class
                          new-ec
                          [non-final (if (set-empty? (equivalence-class-non-final new-ec))
                                          (set-add (set-add (equivalence-class-non-final new-ec) matchee) (set-first potential-matches))
                                          (set-add (equivalence-class-non-final new-ec) matchee))])]
            [else (make-equivalence matchee (set-rest potential-matches) new-ec last-ec-non-finals)]))

    
    (define (make-next-equivalence-class last-ec matches-to-make new-ec)
      (let ([set-second (compose1 set-first set-rest)])
        (if (set-empty? matches-to-make)
            new-ec
            (make-next-equivalence-class last-ec
                                         (set-rest matches-to-make)
                                         (make-equivalence (set-first matches-to-make)
                                                           (set-union (set-rest matches-to-make) (equivalence-class-non-final new-ec))
                                                           new-ec
                                                           (equivalence-class-non-final last-ec))))))
    
    (define (make-equivalence-classes-helper LoEC)
      (define (same-equivalence-class? EC1 EC2)
        (and (list? (andmap (λ (t) (member t (equivalence-class-final EC1))) (equivalence-class-final EC2)))
             (set=? (equivalence-class-non-final EC1) (equivalence-class-non-final EC2))))
      (if (and (>= (length LoEC) 2)
               (same-equivalence-class? (first LoEC) (second LoEC)))
          (first LoEC)
          (make-equivalence-classes-helper (cons (make-next-equivalence-class (first LoEC)
                                                                              (equivalence-class-non-final (first LoEC))
                                                                              (equivalence-class (set) (equivalence-class-final (first LoEC))))
                                                 LoEC)))) 
        (make-equivalence-classes-helper (list first-equivalence-class))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
