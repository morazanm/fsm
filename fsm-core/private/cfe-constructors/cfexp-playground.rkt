#lang racket/base

(require  "../constants.rkt"
          "context-free-expressions-constructors.rkt"
          "../cfg-struct.rkt"
          "../pda.rkt"
          ;"../visualizations/viz-grammar-constructors/cfg-derive-leftmost.rkt"
          "../../../sm-graph.rkt"          
          "construct-cfe-macro.rkt"
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PDA->CFE & CFE->PDA Transformations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;PDA->CFE


(define anbkckdn (make-unchecked-ndpda '(S D M C)
                                       '(a b c d)
                                       '(n k)
                                       'S
                                       '(C)
                                       `(((S ,EMP ,EMP)(D ,EMP))
                                         ((D ,EMP ,EMP)(M ,EMP))
                                         ((S a ,EMP)(S (n)))
                                         ((D b ,EMP)(D (k)))
                                         ((M c (k))(M ,EMP))
                                         ((C d (n))(C ,EMP))
                                         ((M ,EMP ,EMP)(C ,EMP)))))
                                       

;;w = a*
(define A* (make-unchecked-ndpda '(S)
                                '(a b)
                                '(a)
                                'S
                                '(S)
                                `(((S a ,EMP) (S ,EMP)))))


(define Gina-aˆnbˆn (make-unchecked-ndpda '(S M F)
                                          '(a b)
                                          '(a)
                                          'S
                                          '(F)
                                          `(((S ,EMP ,EMP) (M ,EMP))
                                            ((S a ,EMP) (S (a)))
                                            ((M b (a)) (M ,EMP))
                                            ((M ,EMP ,EMP) (F ,EMP)))))

(define Gina-wcwˆr (make-unchecked-ndpda '(S P Q F)
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

(define Gina-palindrome-pda (make-unchecked-ndpda '(S A B C)
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

;;L = {a^ib^j | i ≤ j ≤ 2i}
(define Gina-AiBj (make-unchecked-ndpda '(S A B C)
                         '(a b)
                         '(a)
                         'S
                         '(C)
                         `(((S a ,EMP) (A (a)))
                           ((S a ,EMP) (A (a a)))
                           ((S ,EMP ,EMP) (A ,EMP))
                           ((A a ,EMP) (A (a)))
                           ((A a ,EMP) (A (a a)))
                           ((A b (a)) (B ,EMP))
                           ((A ,EMP ,EMP) (B ,EMP))
                           ((B b (a)) (B ,EMP))
                           ((B ,EMP ,EMP) (C ,EMP))
                           )))
(define Gina-AiBj2 (make-unchecked-ndpda '(S A B C)
                         '(a b)
                         '(a)
                         'S
                         '(C)
                         `(((S a ,EMP) (A (a)))
                           ((S a ,EMP) (A (a a)))
                           ((S ,EMP ,EMP) (A ,EMP))
                           ((A a ,EMP) (A (a)))
                           ((A a ,EMP) (A (a a)))
                           ((A a ,EMP) (S ,EMP))
                           ((A b (a)) (B ,EMP))
                           ((A ,EMP ,EMP) (B ,EMP))
                           ((B b (a)) (B ,EMP))
                           ((B ,EMP ,EMP) (C ,EMP))
                           )))

;;L = {a^nb^ma^n | n,m ≥ 0 }
(define Gina-A^nB^mA^n (make-unchecked-ndpda '(S A B)
                              '(a b)
                              '(a)
                              'S
                              '(B)
                              `(((S a ,EMP) (S (a)))
                                ((S ,EMP ,EMP) (A ,EMP))
                                ((S b ,EMP) (A ,EMP))
                                ((A b ,EMP) (A ,EMP))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((A a (a)) (B ,EMP))
                                ((B a (a)) (B ,EMP)))))

;;L = {a^mb^nc^pd^q | m,n,p,q ≥ 0 ∧ m + n = p + q}
(define Gina-a^mb^nc^pd^q (make-unchecked-ndpda '(S A B C)
                                 '(a b c d)
                                 '(a)
                                 'S
                                 '(C)
                                 `(((S a ,EMP) (S (a)))
                                   ((S ,EMP ,EMP) (A ,EMP))
                                   ((A b ,EMP) (A (a)))
                                   ((A ,EMP ,EMP) (B ,EMP))
                                   ((B c (a)) (B ,EMP))
                                   ((B ,EMP ,EMP) (C ,EMP))
                                   ((C d (a)) (C ,EMP)))))


;;L = {a^mb^nc^p | m,n,p≥0 ∧ (m = n ∨ n = p)}
(define Gina-a^mb^nc^p (make-unchecked-ndpda '(S A B C D E F)
                              '(a b c)
                              '(a)
                              'S
                              '(C F)
                              `(((S ,EMP ,EMP) (A ,EMP))
                                ((S ,EMP ,EMP) (D ,EMP))
                                ((A a ,EMP) (A (a)))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((B b (a)) (B ,EMP))
                                ((B ,EMP ,EMP) (C ,EMP))
                                ((C c ,EMP) (C ,EMP))
                                ((D a ,EMP) (D ,EMP))
                                ((D ,EMP ,EMP) (E ,EMP))
                                ((E b ,EMP) (E (a)))
                                ((E ,EMP ,EMP) (F ,EMP))
                                ((F c (a)) (F ,EMP)))))


(define marco-anbncndn
  (make-unchecked-ndpda '(P S H U R)
                        '(a b c d)
                        '(z Z)
                        'P
                        '(R)
                        `(
                          ((P ,EMP ,EMP) (S (Z)))
                          ((S a ,EMP) (S (z)))
                          ((S ,EMP ,EMP) (H ,EMP))
                          ((H b (z)) (H ,EMP))
                          ((H ,EMP (Z)) (U (Z)))
                          ((U c ,EMP) (U (z)))
                          ((U ,EMP ,EMP) (R ,EMP))
                          ((R ,EMP (Z)) (R ,EMP))
                          ((R d (z)) (R ,EMP))
                          )))

(define sample-P
  (make-unchecked-ndpda
   '(S F)
   '(a b)
   '(Z)
   'S
   '(F)
   `(((S a ,EMP) (F ,EMP))
     ((F b ,EMP) (S ,EMP)))))

(define sample-P2
  (make-unchecked-ndpda
   '(S F X)
   '(a b)
   '(Z)
   'S
   '(F)
   `(((S a ,EMP) (F ,EMP))
     ((F b ,EMP) (S ,EMP))
     ((F a ,EMP) (X ,EMP))))) 

(define LOPDA (list Gina-aˆnbˆn A* Gina-wcwˆr Gina-palindrome-pda Gina-AiBj Gina-A^nB^mA^n Gina-a^mb^nc^pd^q Gina-a^mb^nc^p))

#;(sm-graph Gina-aˆnbˆn)

#;(pda->cfe Gina-aˆnbˆn)


#;(sm-graph Gina-AiBj)
;(pda->cfe Gina-AiBj2)



;;w = a*
#|
(define A*-cfe (pda->cfe A*))

(define Gina-aˆnbˆn-cfe (pda->cfe Gina-aˆnbˆn))

(define Gina-wcwˆr-cfe (pda->cfe Gina-wcwˆr))


(define Gina-palindrome-pda-cfe (pda->cfe Gina-palindrome-pda))

(define Gina-AiBj-cfe (pda->cfe Gina-AiBj))

(define Gina-A^nB^mA^n-cfe (pda->cfe Gina-A^nB^mA^n))

(define Gina-a^mb^nc^pd^q-cfe (pda->cfe Gina-a^mb^nc^pd^q))

(define Gina-a^mb^nc^p-cfe (pda->cfe Gina-a^mb^nc^p))

;;w = a^nb^n
(define converted-ANBN (pda->cfe (cfe->pda ANBN)))
|#
