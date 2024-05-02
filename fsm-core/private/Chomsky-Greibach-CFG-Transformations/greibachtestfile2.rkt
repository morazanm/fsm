#lang fsm
(require "pda2cfg-v2-simple-pda.rkt" #;"greibach.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> pda
;; Purpose: Transform the given cfg into a pda
(define (cfg2pda G)
  (let [(nts (grammar-nts G))
        (sigma (grammar-sigma G))
        (start (grammar-start G))
        (rules (grammar-rules G))]
    (make-ndpda '(S Q)
                sigma
                (append nts sigma)
                'S
                (list 'Q)
                (append
                 (list (list (list 'S EMP EMP) (list 'Q (list start))))
                 (map (λ (r)
                        (list (list 'Q EMP (list (first r)))
                              (list 'Q
                                    (if (eq? (third r) EMP)
                                        EMP
                                        (symbol->fsmlos (third r))))))
                      rules)
                 (map (λ (a) (list (list 'Q a (list a)) (list 'Q EMP)))
                      sigma)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define a2nb2n2 (make-ndpda '(P Q)
                            '(a b)
                            '(S a b c)
                            'P
                            '(Q)
                            '(((P ε ε) (Q (c S)))
                              ((Q ε (c S)) (Q ε))
                              ((Q ε (c S)) (Q (a c S b)))
                              ((Q a (a)) (Q ε))
                              ((Q b (b)) (Q ε)))))

(define p5 (make-ndpda '(S P Q F A B C D E G H I J K L M N O R)
                       '(a b)
                       '(a b C S Z)
                       'S
                       '(F)
                       '(((S ε ε) (P (Z)))

                       
                         ((P ε (C)) (A (C C)))
                         ((A ε (C)) (B (C S)))
                         ((B ε (C)) (Q (C)))
                         
                         ((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (C)))
                         
                         ((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (C)))
                         
                         ((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (C)))
                         
                         ((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (C)))  


                         ((Q b (b)) (Q ε))
                         ((Q a (a)) (Q ε))

                         ((Q ε (Z)) (F ε))

                         ((Q ε (C)) (L ε))
                         ((L ε (S)) (Q ε))

                         ((Q ε (C)) (M ε))
                         ((M ε (S)) (N (S b)))
                         ((N ε (S)) (O (S S)))
                         ((O ε (S)) (R (S C)))
                         ((R ε (S)) (Q (a)))
                         
                         )))

(sm-graph a2nb2n2)
;(sm-graph (cfg2pda (pda2cfg anbn)))
;(sm-graph (cfg2pda (greibach (pda2cfg anbn))))
;(sm-graph (pda2spda a2nb2n2))
(sm-graph p5)

;.................................................

(define p52 (make-ndpda '(S P Q F L M)
                        '(a b)
                        '(a b C S Z)
                        'S
                        '(F)
                        '(((S ε ε) (P (Z)))
                       
                          #|((P ε (c)) (A (c c)))
                         ((A ε (c)) (B (c S)))
                         ((B ε (c)) (Q (c)))|#
                         
                          #|((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (c)))|#
                         
                          #|((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (c)))|#
                          ;((P ε (a)) (Q (a a)))
                         
                          #|((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (c)))|#
                          ;((P ε (S)) (Q (c S S)))
                         
                          #|((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (c)))|#  
                          ;((P ε (Z)) (Q (c S Z)))

                          ((Q b (b)) (Q ε))
                          ((Q a (a)) (Q ε))

                          ((Q ε (Z)) (F ε))

                          ((Q ε (C)) (L ε))
                          ((L ε (S)) (Q ε))
                         
                          ((P ε (Z)) (Q (a C S b Z)))

                          ((Q ε (C)) (M ε))
                          ((M ε (S)) (Q (a C S b)))

                          ;((Q ε (c)) (M ε))
                          ;((M ε (S)) (N (S b)))
                          ;((N ε (S)) (O (S S)))
                          ;((O ε (S)) (R (S c)))
                          ;((R ε (S)) (Q (a)))
                         
                          )))

(sm-graph p52)
;(sm-showtransitions p52 '(a a b b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = {w | w in (a b)* AND  w has more b than a}
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(define numb>numa-pda (cfg2pda numb>numa))

(sm-graph numb>numa-pda)
(sm-graph (pda2spda numb>numa-pda))

;.................................................

(define nanb (make-ndpda '(S F Q R)
                         '(a b)
                         '(a b S Z A)
                         'S
                         '(F)
                         '(((S ε ε) (R (Z)))
                           ((Q ε (Z)) (F ε))
                           ((Q a (a)) (Q ε))
                           ((Q b (b)) (Q ε))
                           ((Q ε (A)) (Q (b A)))
                           ((Q ε (A)) (Q ε))
                           ((Q ε (S)) (Q (b)))
                                          
                           ((R ε (Z)) (Q (b Z)))
                           
                           ;((S ε (Z)) (Q (A b A Z)))
                           ((R ε (Z)) (Q (b A b A Z)))
                           ((R ε (Z)) (Q (b A Z)))
                           
                           ((R ε (Z)) (Q (a A b A b A Z)))
                           ((R ε (Z)) (Q (b A a A b A Z)))

                           ((Q ε (A)) (Q (a A b A)))
                           ((Q ε (A)) (Q (b A a A))))))

(sm-graph nanb)
;(sm-showtransitions nanb '(b a b b))

;(sm-showtransitions nanb '(b a))
;(sm-showtransitions nanb '(a a a a))
;(sm-showtransitions numb>numa-pda '(a a a a))
;(sm-showtransitions (pda2spda numb>numa-pda) '(a a a a))                         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(sm-graph (pda2spda wcw^r))

;.................................................

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = {w | w in (a b)* AND  w has more b than a}
#;(define b>a (make-ndpda '(S Q)
                          '(a b)
                          '(a b A S)
                          'S
                          '(Q)
                          '(((S ε ε) (Q (S)))
                            ((Q b (b)) (Q ε))
                            ((Q a (a)) (Q ε))
                            ((Q ε (A)) (Q (b A)))
                            ((Q ε (A)) (Q ε))
                            ((Q ε (A)) (Q (A b A a A)))
                            ((Q ε (A)) (Q (A a A b A)))
                            ((Q ε (S)) (Q (A b A)))
                            ((Q ε (S)) (Q (b))))))
(define b>a (make-ndpda '(S Q)
                        '(a b)
                        '(a b A S)
                        'S
                        '(Q)
                        '(((S ε ε) (Q (S)))
                          ((Q b (b)) (Q ε))
                          ((Q a (a)) (Q ε))
                          ((Q ε (A)) (Q (b A)))
                          ((Q ε (A)) (Q ε))
                          ((Q ε (A)) (Q (A b A a A)))
                          ((Q ε (A)) (Q (A a A b A)))
                          ;((Q ε (A)) (Q (A b)))
                          ((Q ε (S)) (Q (A b A)))
                          ((Q ε (S)) (Q (b))))))
                          
(sm-graph b>a)
(sm-graph (pda2spda b>a))

(define sb>a (make-ndpda '(S P F Q A B C D E G H I J K)
                         '(a b)
                         '(a b S A Z)
                         'S
                         '(F)
                         '(((S ε ε) (P (Z)))
                           ((P ε (Z)) (Q (S Z)))

                           ((Q a (a)) (Q ε))
                           ((Q b (b)) (Q ε))
                           ((Q ε (A)) (Q (b A)))
                           ((Q ε (A)) (Q ε))
                           ((Q ε (A)) (Q (A b)))
                           ((Q ε (S)) (Q (b)))

                           ((Q ε (S)) (A (S A)))
                           ((A ε (S)) (B (S b)))
                           ((B ε (S)) (Q (A)))

                           ((Q ε (A)) (C (A A)))
                           ((C ε (A)) (D (A a)))
                           ((D ε (A)) (E (A A)))
                           ((E ε (A)) (G (A b)))
                           ((G ε (A)) (Q (A)))

                           ((Q ε (A)) (H (A A)))
                           ((H ε (A)) (I (A a)))
                           ((I ε (A)) (J (A A)))
                           ((J ε (A)) (K (A b)))
                           ((K ε (A)) (Q (A)))

                           ((Q ε (Z)) (F ε)))))

(sm-graph sb>a)

#;'(((S ε ε) (P (Z)))
  ((P ε (Z)) (Q (S Z)))

  ((Q a (a)) (Q ε))
  ((Q b (b)) (Q ε))
  ((Q ε (A)) (Q (b A)))
  ((Q ε (A)) (Q ε))
  ((Q ε (A)) (Q (A b)))
  ((Q ε (S)) (Q (b)))

  ((Q ε (S)) (A (S A)))
  ((A ε (S)) (B (S b)))
  ((B ε (S)) (Q (A)))

  ((Q ε (A)) (C (A A)))
  ((C ε (A)) (D (A a)))
  ((D ε (A)) (E (A A)))
  ((E ε (A)) (G (A b)))
  ((G ε (A)) (Q (A)))

  ((Q ε (A)) (H (A A)))
  ((H ε (A)) (I (A a)))
  ((I ε (A)) (J (A A)))
  ((J ε (A)) (K (A b)))
  ((K ε (A)) (Q (A))) 

  ((Q ε (Z)) (F ε)))

;; Z S A

;; B -> b A a A 
;; B -> b A a A B

'(((S ε ε) (P (Z))) 
  ((P ε (Z)) (Q (S Z))) ;; Z -> S 

  ((Q a (a)) (Q ε))
  ((Q b (b)) (Q ε))
  
  ((Q ε (A)) (Q (b A)))
  ((Q ε (A)) (Q ε))
  ((Q ε (A)) (Q (A b))) ;; A -> A
  ((Q ε (S)) (Q (b)))

  ((Q ε (S)) (A (S A))) ;; S -> A 
  ((A ε (S)) (B (S b)))
  ((B ε (S)) (Q (A)))

  ((Q ε (A)) (C (A A))) ;; A -> A b A a A
  ((C ε (A)) (D (A a)))
  ((D ε (A)) (E (A A)))
  ((E ε (A)) (G (A b)))
  ((G ε (A)) (Q (A)))

  ((Q ε (A)) (H (A A))) ;; A -> A a A b A
  ((H ε (A)) (I (A a)))
  ((I ε (A)) (J (A A)))
  ((J ε (A)) (K (A b)))
  ((K ε (A)) (Q (A))) 

  ((Q ε (Z)) (F ε)))



'(((S ε ε) (P (Z))) 

  ((P ε (Z)) (Q (S Z))) ;; Z -> S 

  ((Q a (a)) (Q ε))
  ((Q b (b)) (Q ε))
  
  ((Q ε (A)) (Q (b A)))
  ((Q ε (A)) (Q ε))
  


  ((Q ε (S)) (Q (b)))

  ((Q ε (S)) (Q (A b A)))


  ((Q ε (A)) (Q (b D))) ;; A -> A
  ((Q ε (A)) (Q (b A a A C))) ;; A -> A b A a A
  ((Q ε (A)) (Q (a A b A B))) ;; A -> A a A b A

  ((Q ε (A)) (Q (A b B))) ;; A -> A
  ((Q ε (A)) (Q (A b A a A B))) ;; A -> A b A a A


  ((Q ε (B)) (Q (a A b A)))
  ((Q ε (B)) (Q (a A b A B)))
  ((Q ε (C)) (Q (b A a A)))
  ((Q ε (C)) (Q (b A a A C)))
  ((Q ε (D)) (Q (b)))
  ((Q ε (D)) (Q (b D)))
  

  ((Q ε (Z)) (F ε)))

;.................................................

(define new-b>a (make-ndpda '(S F Q R)
                            '(a b)
                            '(a b S Z A)
                            'S
                            '(F)
                            '(((S ε ε) (R (Z)))
                              ((Q ε (Z)) (F ε))
                              ((Q a (a)) (Q ε))
                              ((Q b (b)) (Q ε))
                              ((Q ε (A)) (Q (b A)))
                              ((Q ε (A)) (Q ε))
                              ((Q ε (S)) (Q (b)))
                                          
                              ((R ε (Z)) (Q (b Z)))
                           
                              ;((S ε (Z)) (Q (A b A Z)))
                              ((R ε (Z)) (Q (b A b A Z)))
                              ((R ε (Z)) (Q (b A Z)))
                           
                              ((R ε (Z)) (Q (a A b A b A Z)))
                              ((R ε (Z)) (Q (b A a A b A Z)))

                              ((Q ε (A)) (Q (a A b A)))
                              ((Q ε (A)) (Q (b A a A))))))

(sm-graph new-b>a)                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define G (make-cfg '(S)
                    '(a b)
                    `((S ,ARROW ,EMP)
                      (S ,ARROW SaSaSbS)
                      (S ,ARROW SaSbSaS)
                      (S ,ARROW SbSaSaS))
                    'S))

(define PG (grammar->sm G))

;(sm-apply PG '())
;(sm-apply PG '(a a b))
;(sm-apply PG '(a a b a b a b a a)) ;; takes too long
;(sm-apply PG '(a a)) ;; infinite derivation
(sm-graph PG)


;; L = a^nb^n
(define M (make-ndpda '(S A C D B F)
                      '(a b)
                      '(a b C)
                      'S
                      '(F)
                      `(((S ,EMP ,EMP) (A ,EMP))
                        ((A a ,EMP) (A (a)))
                        ((A ,EMP ,EMP) (C ,EMP))
                        ((C ,EMP ,EMP) (C (C)))
                        ((C ,EMP ,EMP) (D ,EMP))
                        ((D ,EMP (C)) (D ,EMP))
                        ((D ,EMP ,EMP) (C ,EMP))
                        ((D ,EMP ,EMP) (B ,EMP))
                        ((B b (a)) (B ,EMP))
                        ((B ,EMP ,EMP) (F ,EMP)))))

(sm-graph M)
(sm-graph (pda2spda M))


;; L = {a^m b^n| m, n ≥ 0 ∧ m ̸= n}
;; Σ = {a b}
;; States:
;;  S: ci = a^m, stack = b^m, start state
;;  X: ci = a^mb^n, number of as in ci < (number of bs in ci + number of bs in stack), final state
;;  Y: ci = a^mb^n, number of as in ci > (number of bs in ci + number of bs in stack), final state
;; Strong enough?
;;  If stack = ε -> number of as in ci < number of bs in ci
;;  If stack = ε -> number of as in ci > number of bs in ci
(define a^mb^n (make-ndpda '(S X Y)
                           '(a b)
                           '(b)
                           'S
                           '(X Y)
                           '(((S a ε) (S (b)))
                             ((S ε ε) (X (b)))
                             ((X b (b)) (X ε))
                             ((X b ε) (X ε))
                             ((S ε (b)) (Y ε))
                             ((Y b (b)) (Y ε))
                             ((Y ε (b)) (Y ε)))))

(sm-graph a^mb^n)
(sm-graph (pda2spda a^mb^n))
;(sm-graph (cfg2pda (greibach (pda2cfg a^mb^n))))
(sm-graph (cfg2pda (pda2cfg a^mb^n)))