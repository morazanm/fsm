#lang fsm

(require "./mtape-tm.rkt")

;; tm --> mttm
;; Purpose: Convert given tm into an mttm
(define (tm->mttm M)
  (let* [(sts (sm-states M))
         (sigma (sm-sigma M))
         (start (sm-start M))
         (finals (sm-finals M))
         (rules (sm-rules M))
         (accept (sm-accept M))]
    (if (not (null? accept))
        (make-mttm sts
                   sigma
                   start
                   finals
                   (map (λ (r) (list (list (caar r) (list (cadar r)))
                                     (list (caadr r) (list (cadadr r)))))
                        rules)
                   1
                   accept)
        (make-mttm sts
                   sigma
                   start
                   finals
                   (map (λ (r) (list (list (caar r) (list (cadar r)))
                                     (list (caadr r) (list (cadadr r)))))
                        rules)
                   1))))

;; L = a*
;; PRE: tape = LMw_ AND i = 0
(define a* (make-tm '(S Y N)
                    `(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y))

;; Tests for a*
(check-equal? (sm-apply a* `(,LM a a a b a a)) 'reject)
(check-equal? (sm-apply a* `(,LM b a a)) 'reject)
(check-equal? (sm-apply a* `(,LM)) 'accept)
(check-equal? (sm-apply a* `(,LM a a a)) 'accept)


(define a*-mt (tm->mttm a*))

;; Tests for a*
(check-equal? (sm-apply a*-mt `(,LM a a a b a a)) 'reject)
(check-equal? (sm-apply a*-mt `(,LM b a a)) 'reject)
(check-equal? (sm-apply a*-mt `(,LM)) 'accept)
(check-equal? (sm-apply a*-mt `(,LM a a a)) 'accept)


;; L = a* U a*b
;; PRE: tape = LMw AND i = 1
(define a*Ua*b (make-tm '(S A B C Y N)
                        `(a b)
                        `(((S a) (A ,RIGHT))
                          ((S a) (B ,RIGHT))
                          ((S b) (C ,RIGHT))
                          ((S ,BLANK) (Y ,BLANK))
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

;; Tests for a*Ua*b
(check-equal? (sm-apply a*Ua*b `(,LM b b) 1) 'reject)
(check-equal? (sm-apply a*Ua*b `(,LM a a b a) 1) 'reject)
(check-equal? (sm-apply a*Ua*b `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM b) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM a b) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM a a a) 1) 'accept)
(check-equal? (sm-apply a*Ua*b `(,LM a a a b) 1) 'accept)

(define a*Ua*b-mt (tm->mttm a*Ua*b))

;; Tests for a*Ua*b-mt
(check-equal? (sm-apply a*Ua*b-mt `(,LM b b) 1) 'reject)
(check-equal? (sm-apply a*Ua*b-mt `(,LM a a b a) 1) 'reject)
(check-equal? (sm-apply a*Ua*b-mt `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply a*Ua*b-mt `(,LM b) 1) 'accept)
(check-equal? (sm-apply a*Ua*b-mt `(,LM a b) 1) 'accept)
(check-equal? (sm-apply a*Ua*b-mt `(,LM a a a) 1) 'accept)
(check-equal? (sm-apply a*Ua*b-mt `(,LM a a a b) 1) 'accept)



;; L = a^n b^n c^n
;; PRE: tape = `(,LM ,BLANK w) ∧ i = 1, where w∈{a b c}∗
;; Σ = {a b c x}
(define anbncn (make-tm '(S A B C D E F G H I J K L Y)
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

(check-equal? (sm-apply anbncn `(,LM ,BLANK a a) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK b b b) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK b a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a c b) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b b b c c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a b c c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b b c c a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a b c) 1) 'accept)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b b c c) 1) 'accept)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a a b b b c c c) 1) 'accept)

(define anbncn-mt (tm->mttm anbncn))

;; Tests for anbncn-mt
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a a) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK b b b) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK c) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK b a b c) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a c b) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a a b c) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a a b b b c c) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a b c c) 1) 'reject)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a a b b c c a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a b c) 1) 'accept)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a a b b c c) 1) 'accept)
(check-equal? (sm-apply anbncn-mt `(,LM ,BLANK a a a b b b c c c) 1) 'accept)

;; ------------- Sanity test for mttm -------------

;;; POST: A+B (on T1)
;;; HOW: Copy A to T2, Copy B to T3, COPY T2 and T3 to T1
(define ADD (make-mttm '(S A T U V)
                       `(I)
                       'S
                       '(H)
                       `(((S (,BLANK ,BLANK ,BLANK)) (A (R R R)))
                         ((A (I ,BLANK ,BLANK)) (A (,BLANK I ,BLANK)))
                         ((A (,BLANK I ,BLANK)) (A (R R ,BLANK)))
                         ((A (,BLANK ,BLANK ,BLANK)) (T (R ,BLANK ,BLANK)))
                         ((T (I ,BLANK ,BLANK)) (T (,BLANK ,BLANK I)))
                         ((T (,BLANK ,BLANK I)) (T (R ,BLANK R)))
                         ((T (,BLANK ,BLANK ,BLANK)) (Q (L ,BLANK ,BLANK)))
                         ((Q (,BLANK ,BLANK ,BLANK)) (U (L L L)))
                         ((U (,BLANK I I)) (U (I I ,BLANK)))
                         ((U (I I ,BLANK)) (U (L I L)))
                         ((U (,BLANK I ,BLANK)) (V (,BLANK I ,BLANK)))
                         ((V (,BLANK I ,BLANK)) (V (I ,BLANK ,BLANK)))
                         ((V (I ,BLANK ,BLANK)) (V (L L ,BLANK)))
                         ((V (,BLANK ,BLANK ,BLANK)) (H (,BLANK ,BLANK ,BLANK))))
                       3))

(check-equal? (mttm-apply ADD `(,BLANK I I I ,BLANK I I))
              '(H (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _))))

(check-equal? (mttm-show-transitions ADD `(,BLANK I I I ,BLANK I I))
              '((S (0 (_ I I I _ I I)) (0 (_)) (0 (_)))
                (A (1 (_ I I I _ I I)) (1 (_ _)) (1 (_ _)))
                (A (1 (_ _ I I _ I I)) (1 (_ I)) (1 (_ _)))
                (A (2 (_ _ I I _ I I)) (2 (_ I _)) (1 (_ _)))
                (A (2 (_ _ _ I _ I I)) (2 (_ I I)) (1 (_ _)))
                (A (3 (_ _ _ I _ I I)) (3 (_ I I _)) (1 (_ _)))
                (A (3 (_ _ _ _ _ I I)) (3 (_ I I I)) (1 (_ _)))
                (A (4 (_ _ _ _ _ I I)) (4 (_ I I I _)) (1 (_ _)))
                (T (5 (_ _ _ _ _ I I)) (4 (_ I I I _)) (1 (_ _)))
                (T (5 (_ _ _ _ _ _ I)) (4 (_ I I I _)) (1 (_ I)))
                (T (6 (_ _ _ _ _ _ I)) (4 (_ I I I _)) (2 (_ I _)))
                (T (6 (_ _ _ _ _ _ _)) (4 (_ I I I _)) (2 (_ I I)))
                (T (7 (_ _ _ _ _ _ _ _)) (4 (_ I I I _)) (3 (_ I I _)))
                (Q (6 (_ _ _ _ _ _ _ _)) (4 (_ I I I _)) (3 (_ I I _)))
                (U (5 (_ _ _ _ _ _ _ _)) (3 (_ I I I _)) (2 (_ I I _)))
                (U (5 (_ _ _ _ _ I _ _)) (3 (_ I I I _)) (2 (_ I _ _)))
                (U (4 (_ _ _ _ _ I _ _)) (3 (_ I I I _)) (1 (_ I _ _)))
                (U (4 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (1 (_ _ _ _)))
                (U (3 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (0 (_ _ _ _)))
                (V (3 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (0 (_ _ _ _)))
                (V (3 (_ _ _ I I I _ _)) (3 (_ I I _ _)) (0 (_ _ _ _)))
                (V (2 (_ _ _ I I I _ _)) (2 (_ I I _ _)) (0 (_ _ _ _)))
                (V (2 (_ _ I I I I _ _)) (2 (_ I _ _ _)) (0 (_ _ _ _)))
                (V (1 (_ _ I I I I _ _)) (1 (_ I _ _ _)) (0 (_ _ _ _)))
                (V (1 (_ I I I I I _ _)) (1 (_ _ _ _ _)) (0 (_ _ _ _)))
                (V (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _)))
                (H (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _)))))