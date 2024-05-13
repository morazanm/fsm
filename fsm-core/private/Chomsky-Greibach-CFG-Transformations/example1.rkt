#lang fsm
(require "pda2cfg-v2-simple-pda.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample pda

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphs 

(sm-graph a^mb^n)
(sm-graph (pda2spda a^mb^n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pda converted 

(define a^mb^n2 (make-ndpda '(S Q)
                            '(a b)
                            '(a b Z S X Y F)
                            'S
                            '(Q)
                            '(((S ε ε) (Q (S Z)))

                              ((Q a (a)) (Q ε))
                              ((Q b (b)) (Q ε))
                              
                              ((Q ε (S b)) (Q (a S b b)))
                              ((Q ε (S Z)) (Q (a S b Z)))

                              ((Q ε (S b)) (Q (Y)))
                              ((Q ε (Y b)) (Q (Y)))
                              ((Q ε (Y b)) (Q (b Y)))
                              ((Q ε (Y Z)) (Q (F)))
                        
                              ((Q ε (S b)) (Q (X b b)))
                              ((Q ε (S Z)) (Q (X b Z)))
                              ((Q ε (X b)) (Q (b X b)))
                              ((Q ε (X Z)) (Q (b X Z)))
                              ((Q ε (X b)) (Q (b X)))
                              ((Q ε (X Z)) (Q (F)))

                              ((Q ε (F)) (Q ε))

                              )))

;(sm-graph a^mb^n2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; converted pda -> greibach form

(define a^mb^n3 (make-ndpda '(S Q)
                            '(a b)
                            '(a b Z S X Y F)
                            'S
                            '(Q)
                            '(((S ε ε) (Q (S Z)))

                              ((Q a (a)) (Q ε))
                              ((Q b (b)) (Q ε))
                              
                              ((Q ε (S b)) (Q (a S b b)))
                              ((Q ε (S Z)) (Q (a S b Z)))

                              ((Q ε (S b)) (Q (Y))) ;; Sb -> Y
                              ((Q ε (Y b)) (Q (Y)))
                              ((Q ε (Y b)) (Q (b Y)))
                              ((Q ε (Y Z)) (Q (F)))
                        
                              ((Q ε (S b)) (Q (X b b)))
                              ((Q ε (S Z)) (Q (X b Z)))
                              ((Q ε (X b)) (Q (b X b)))
                              ((Q ε (X Z)) (Q (b X Z)))
                              ((Q ε (X b)) (Q (b X)))
                              ((Q ε (X Z)) (Q (F)))

                              ((Q ε (F)) (Q ε))

                              )))

(sm-graph a^mb^n3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = {w | w in (a b)* AND  w has more b than a}
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
                          ((Q ε (S)) (Q (A b A)))
                          ((Q ε (S)) (Q (b))))))
                          
;(sm-graph b>a)

;; simple pdas
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
                           ((H ε (A)) (I (A b)))
                           ((I ε (A)) (J (A A)))
                           ((J ε (A)) (K (A a)))
                           ((K ε (A)) (Q (A)))

                           ((Q ε (Z)) (F ε)))))

;(sm-graph sb>a)

(define sb>a2 (make-ndpda '(S P F Q)
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
                            ((Q ε (S)) (Q (b)))

                            ((Q ε (S)) (Q (A b A)))

                            ((Q ε (A)) (Q (A b A a A)))
                            ((Q ε (A)) (Q (A a A b A)))

                            ((Q ε (Z)) (F ε)))))

;(sm-graph sb>a2)

;; simple pda transformed
(define sb>a3 (make-ndpda '(S Q)
                          '(a b)
                          '(a b Z A S P Q F)
                          'S
                          '(Q)
                          '(((S ε ε) (Q (S)))
                            ((Q ε (S)) (Q (P Z)))
                            ((Q ε (P Z)) (Q (Q S Z)))
                            ((Q ε (Q A)) (Q (Q A a A b A)))
                            ((Q ε (Q A)) (Q (Q A b A a A)))
                            ((Q ε (Q A)) (Q (Q b A)))
                            ((Q ε (Q S)) (Q (Q A b A)))
                            ((Q ε (Q S)) (Q (Q b)))
                            ((Q ε (Q A)) (Q (Q)))
                            ((Q ε (Q b)) (Q (b Q)))
                            ((Q ε (Q a)) (Q (a Q)))

                            ((Q ε (Q Z)) (Q (F)))
                            ((Q ε (F)) (Q ε))

                            ((Q a (a)) (Q ε))
                            ((Q b (b)) (Q ε))

                            )))

;(sm-graph sb>a3)

;; greibach 
(define sb>a4 (make-ndpda '(S P F Q)
                          '(a b)
                          '(a b S A Z B)
                          'S
                          '(F)
                          '(((S ε ε) (P (Z)))
                            ;((P ε (Z)) (Q (S Z)))

                            ((Q a (a)) (Q ε))
                            ((Q b (b)) (Q ε))
                            ((Q ε (A)) (Q (b A)))
                            ((Q ε (A)) (Q ε))
                            ((Q ε (S)) (Q (b)))

                            ;((Q ε (S)) (Q (A b A)))
                            ((Q ε (S)) (Q (b A)))
                            ((Q ε (S)) (Q (b A b A)))
                            ((Q ε (S)) (Q (b A B b A)))
                            ((Q ε (S)) (Q (b A a A b A)))
                            ((Q ε (S)) (Q (b A a A B b A)))
                            ((Q ε (S)) (Q (a A b A b A)))
                            ((Q ε (S)) (Q (a A b A B b A)))

                            ;((Q ε (A)) (Q (A a A b A)))
                            ;((Q ε (A)) (Q (A b A a A)))

                            ((Q ε (A)) (Q (b A B)))

                            ((Q ε (B)) (Q (b A a A)))
                            ((Q ε (B)) (Q (b A a A B)))
                            ((Q ε (B)) (Q (a A b A)))
                            ((Q ε (B)) (Q (a A b A B)))

                            ;((Q ε (A)) (Q (B)))
                            ((Q ε (A)) (Q (b A a A)))
                            ((Q ε (A)) (Q (b A a A B)))
                            ((Q ε (A)) (Q (a A b A)))
                            ((Q ε (A)) (Q (a A b A B)))

                            ((Q ε (Z)) (F ε))

                            ;; ((P ε (Z)) (Q (S Z)))
                            ((P ε (Z)) (Q (b Z)))
                            ((P ε (Z)) (Q (b A Z)))
                            ((P ε (Z)) (Q (b A b A Z)))
                            ((P ε (Z)) (Q (b A B b A Z)))
                            ((P ε (Z)) (Q (b A a A b A Z)))
                            ((P ε (Z)) (Q (b A a A B b A Z)))
                            ((P ε (Z)) (Q (a A b A b A Z)))
                            ((P ε (Z)) (Q (a A b A B b A Z)))
                           
                  


                            )))

;(sm-graph sb>a4)

;; simple transformed greibach
(define sb>a5 (make-ndpda '(S Q)
                          '(a b)
                          '(a b Z A S B P Q F)
                          'S
                          '(Q)
                          '(((S ε ε) (Q (S)))
                            ((Q ε (S)) (Q (P Z)))
                            ((Q ε (P Z)) (Q (Q S Z)))

                            ((Q ε (Q a)) (Q (a Q)))
                            ((Q ε (Q b)) (Q (b Q)))
                            
                            ((Q ε (Q A)) (Q (Q b A)))
                            ((Q ε (Q A)) (Q (Q)))
                            ((Q ε (Q S)) (Q (Q b)))

                            ;((Q ε (S)) (Q (A b A)))
                            ((Q ε (Q S)) (Q (Q b A)))
                            ((Q ε (Q S)) (Q (Q b A b A)))
                            ((Q ε (Q S)) (Q (Q b A B b A)))
                            ((Q ε (Q S)) (Q (Q b A a A b A)))
                            ((Q ε (Q S)) (Q (Q b A a A B b A)))
                            ((Q ε (Q S)) (Q (Q a A b A b A)))
                            ((Q ε (Q S)) (Q (Q a A b A B b A)))

                            ;((Q ε (A)) (Q (A a A b A)))
                            ;((Q ε (A)) (Q (A b A a A)))

                            ((Q ε (Q A)) (Q (Q b A B)))

                            ((Q ε (Q B)) (Q (Q b A a A)))
                            ((Q ε (Q B)) (Q (Q b A a A B)))
                            ((Q ε (Q B)) (Q (Q a A b A)))
                            ((Q ε (Q B)) (Q (Q a A b A B)))

                            ;((Q ε (A)) (Q (B)))
                            ((Q ε (Q A)) (Q (Q b A a A)))
                            ((Q ε (Q A)) (Q (Q b A a A B)))
                            ((Q ε (Q A)) (Q (Q a A b A)))
                            ((Q ε (Q A)) (Q (Q a A b A B)))

                           
                            ((Q ε (Q Z)) (Q (F)))
                            ((Q ε (F)) (Q ε))

                            ((Q a (a)) (Q ε))
                            ((Q b (b)) (Q ε))

                            )))

;(sm-graph sb>a5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = a^nb^n
(define M (make-ndpda '(S A C D B F)
                      '(a b)
                      '(a b C)
                      'S
                      '(F)
                      '(((S ε ε) (A ε))
                        ((A a ε) (A (a)))
                        ((A ε ε) (C ε))
                        ((C ε ε) (C (C)))
                        ((C ε ε) (D ε))
                        ((D ε (C)) (D ε))
                        ((D ε ε) (C ε))
                        ((D ε ε) (B ε))
                        ((B b (a)) (B ε))
                        ((B ε ε) (F ε)))))

(sm-graph M)
;(sm-graph (pda2spda M))

;; simple pda
(define M2 (make-ndpda '(S A C D B F X Y)
                       '(a b)
                       '(a b C Z)
                       'X
                       '(Y)
                       '(((X ε ε) (S (Z)))
                         
                         ((S ε (C)) (A (C)))
                         ((S ε (b)) (A (b)))
                         ((S ε (a)) (A (a)))
                         ((S ε (Z)) (A (Z)))
                         
                         ((A a (C)) (A (a C)))
                         ((A a (b)) (A (a b)))
                         ((A a (a)) (A (a a)))
                         ((A a (Z)) (A (a Z)))
                         
                         ((A ε (C)) (C (C)))
                         ((A ε (b)) (C (b)))
                         ((A ε (a)) (C (a)))
                         ((A ε (Z)) (C (Z)))

                         ((C ε (C)) (C (C C)))
                         ((C ε (b)) (C (C b)))
                         ((C ε (a)) (C (C a)))
                         ((C ε (Z)) (C (C Z)))

                         ((C ε (C)) (D (C)))
                         ((C ε (b)) (D (b)))
                         ((C ε (a)) (D (a)))
                         ((C ε (Z)) (D (Z)))

                         ((D ε (C)) (C (C)))
                         ((D ε (b)) (C (b)))
                         ((D ε (a)) (C (a)))
                         ((D ε (Z)) (C (Z)))

                         ((D ε (C)) (D ε))

                         ((D ε (C)) (B (C)))
                         ((D ε (b)) (B (b)))
                         ((D ε (a)) (B (a)))
                         ((D ε (Z)) (B (Z)))

                         ((B b (a)) (B ε))

                         ((B ε (C)) (F (C)))
                         ((B ε (b)) (F (b)))
                         ((B ε (a)) (F (a)))
                         ((B ε (Z)) (F (Z)))

                         ((F ε (Z)) (Y ε))              

                         )))

(sm-graph M2)

;; simple pda transformed
(define M3 (make-ndpda '(S Q)
                       '(a b)
                       '(a b C Z S X A D B Y F)
                       'S
                       '(Q)
                       '(((S ε ε) (Q (X)))
                         
                         ((Q ε (X)) (Q (S Z)))
                         
                         ((Q ε (S Z)) (Q (A Z)))

                         ((Q ε (A C)) (Q (a A a C)))
                         ((Q ε (A b)) (Q (a A a b)))
                         ((Q ε (A a)) (Q (a A a a)))
                         ((Q ε (A Z)) (Q (a A a Z)))

                         ((Q ε (A C)) (Q (C C)))
                         ((Q ε (A b)) (Q (C b)))
                         ((Q ε (A a)) (Q (C a)))
                         ((Q ε (A Z)) (Q (C Z)))

                         ((Q ε (C C)) (Q (C C C)))
                         ((Q ε (C b)) (Q (C C b)))
                         ((Q ε (C a)) (Q (C C a)))
                         ((Q ε (C Z)) (Q (C C Z)))

                         ((Q ε (C C)) (Q (D C)))
                         ((Q ε (C b)) (Q (D b)))
                         ((Q ε (C a)) (Q (D a)))
                         ((Q ε (C Z)) (Q (D Z)))

                         ((Q ε (D C)) (Q (C C)))
                         ((Q ε (D b)) (Q (C b)))
                         ((Q ε (D a)) (Q (C a)))
                         ((Q ε (D Z)) (Q (C Z)))

                         ((Q ε (D C)) (Q (D)))

                         ((Q ε (D C)) (Q (B C)))
                         ((Q ε (D b)) (Q (B b)))
                         ((Q ε (D a)) (Q (B a)))
                         ((Q ε (D Z)) (Q (B Z)))

                         ((Q ε (B a)) (Q (b B)))

                         ((Q ε (B C)) (Q (F C)))
                         ((Q ε (B b)) (Q (F b)))
                         ((Q ε (B a)) (Q (F a)))
                         ((Q ε (B Z)) (Q (F Z)))

                         ((Q ε (F Z)) (Q (Y)))

                         ((Q ε (Y)) (Q ε))

                         ((Q a (a)) (Q ε))
                         ((Q b (b)) (Q ε))
                        
                         )))

;(sm-graph M3)

(define M4 (make-ndpda '(S Q)
                       '(a b)
                       '(a b C Z S X A D B Y F)
                       'S
                       '(Q)
                       '(((S ε ε) (Q (X))) ;; DONE
                         
                         ((Q ε (X)) (Q (S Z))) ;; DONE
                         
                         ((Q ε (S Z)) (Q (A Z))) ;; DONE

                         ((Q ε (A C)) (Q (a A a C))) ;; DONE
                         ((Q ε (A b)) (Q (a A a b))) ;; DONE
                         ((Q ε (A a)) (Q (a A a a))) ;; DONE
                         ((Q ε (A Z)) (Q (a A a Z))) ;; DONE

                         ((Q ε (A C)) (Q (C C))) ;; DONE
                         ((Q ε (A b)) (Q (C b))) ;; DONE
                         ((Q ε (A a)) (Q (C a))) ;; DONE
                         ((Q ε (A Z)) (Q (C Z))) ;; DONE

                         ((Q ε (C C)) (Q (C C C))) ;; C -> H    H -> CH | C    I -> 
                         ((Q ε (C b)) (Q (C C b))) ;; b -> Cb
                         ((Q ε (C a)) (Q (C C a))) ;; a -> Ca
                         ((Q ε (C Z)) (Q (C C Z))) ;; Z -> CZ

                         ((Q ε (C C)) (Q (D C))) ;; C -> C
                         ((Q ε (C b)) (Q (D b))) ;; DONE
                         ((Q ε (C a)) (Q (D a))) ;; DONE
                         ((Q ε (C Z)) (Q (D Z))) ;; Z -> Z

                         ((Q ε (D C)) (Q (D))) ;; C -> emp 
                         
                         ((Q ε (D C)) (Q (C C))) 
                         ((Q ε (D b)) (Q (C b)))
                         ((Q ε (D a)) (Q (C a)))
                         ((Q ε (D Z)) (Q (C Z)))

                         ((Q ε (D C)) (Q (B C)))
                         ((Q ε (D b)) (Q (B b)))
                         ((Q ε (D a)) (Q (B a)))
                         ((Q ε (D Z)) (Q (B Z)))

                         ((Q ε (B a)) (Q (b B)))

                         ((Q ε (B C)) (Q (F C)))
                         ((Q ε (B b)) (Q (F b)))
                         ((Q ε (B a)) (Q (F a)))
                         ((Q ε (B Z)) (Q (F Z)))

                         ((Q ε (F Z)) (Q (Y))) ;; DONE

                         ((Q ε (Y)) (Q ε)) ;; DONE

                         ((Q a (a)) (Q ε)) ;; DONE
                         ((Q b (b)) (Q ε)) ;; DONE
                        
                         )))

;(sm-graph M4)

(define M5 (make-ndpda '(S Q)
                       '(a b)
                       '(a b C Z S X A D B Y F)
                       'S
                       '(Q)
                       '(((S ε ε) (Q (X))) ;; DONE
                         
                         ((Q ε (X)) (Q (S Z))) ;; DONE
                         
                         ((Q ε (S Z)) (Q (A Z))) ;; DONE

                         ((Q ε (A C)) (Q (a A a C))) ;; DONE
                         ((Q ε (A b)) (Q (a A a b))) ;; DONE
                         ((Q ε (A a)) (Q (a A a a))) ;; DONE
                         ((Q ε (A Z)) (Q (a A a Z))) ;; DONE

                         ((Q ε (A)) (Q (C))) ;; DONE

                         ((Q ε (C C)) (Q (C))) ;; C -> H    H -> CH | C    I -> 
                         ((Q ε (C b)) (Q (C b))) ;; b -> Cb
                         ((Q ε (C a)) (Q (C a))) ;; a -> Ca
                         ((Q ε (C Z)) (Q (C))) ;; Z -> CZ

                         ((Q ε (C)) (Q (D))) ;; C -> C

                         ((Q ε (D C)) (Q (D))) ;; C -> emp 
                         
                         ((Q ε (D)) (Q (C))) ;; DONE

                         ((Q ε (D)) (Q (B))) ;; DONE

                         ((Q ε (B a)) (Q (b B))) ;; DONE

                         ((Q ε (B)) (Q (F))) ;; DONE

                         ((Q ε (F Z)) (Q (Y))) ;; DONE

                         ((Q ε (Y)) (Q ε)) ;; DONE

                         ((Q a (a)) (Q ε)) ;; DONE
                         ((Q b (b)) (Q ε)) ;; DONE
                        
                         )))

(sm-graph M5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = a^nb^n
(define newM (make-ndpda '(S A C D B F)
                         '(a b)
                         '(a b C)
                         'S
                         '(F)
                         '(((S ε ε) (A ε))
                           ((A a ε) (A (a)))
                           ((A ε ε) (C ε))
                           ((C ε ε) (C (C)))
                           ((C ε ε) (D ε))
                           ((D ε (C)) (D ε))
                           ((D ε ε) (C ε))
                           ((D ε ε) (B ε))
                           ((B b (a)) (B ε))
                           ;((B b (C)) (B ε))
                           ((B ε ε) (F ε)))))

(sm-graph newM)
(sm-graph (pda2spda newM))
;(sm-graph (pda2spda newM))

(define newM5 (make-ndpda '(S Q)
                          '(a b)
                          '(a b C Z S X A D B Y F)
                          'S
                          '(Q)
                          '(((S ε ε) (Q (X))) ;; DONE
                         
                            ((Q ε (X)) (Q (S Z))) ;; DONE
                         
                            ((Q ε (S Z)) (Q (A Z))) ;; DONE

                            ((Q ε (A C)) (Q (a A a C))) ;; DONE
                            ((Q ε (A b)) (Q (a A a b))) ;; DONE
                            ((Q ε (A a)) (Q (a A a a))) ;; DONE
                            ((Q ε (A Z)) (Q (a A a Z))) ;; DONE

                            ((Q ε (A)) (Q (C))) ;; DONE

                            ((Q ε (C C)) (Q (C))) ;; C -> H    H -> CH | C    I -> 
                            ((Q ε (C b)) (Q (C b))) ;; b -> Cb
                            ((Q ε (C a)) (Q (C a))) ;; a -> Ca
                            ((Q ε (C Z)) (Q (C))) ;; Z -> CZ

                            ((Q ε (C)) (Q (D))) ;; C -> C

                            ((Q ε (D C)) (Q (D))) ;; C -> emp 
                         
                            ((Q ε (D)) (Q (C))) ;; DONE

                            ((Q ε (D)) (Q (B))) ;; DONE

                            ((Q ε (B a)) (Q (b B))) ;; DONE

                            ((Q ε (B)) (Q (F))) ;; DONE

                            ((Q ε (F Z)) (Q (Y))) ;; DONE

                            ((Q ε (Y)) (Q ε)) ;; DONE

                            ((Q a (a)) (Q ε)) ;; DONE
                            ((Q b (b)) (Q ε)) ;; DONE

                            ;((Q ε (C)) (Q (C C)))
                            ;((Q b (B C)) (Q (b B)))
                        
                            )))

(sm-graph newM5)