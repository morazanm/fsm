 #lang racket/base

(require  "../constants.rkt"
          "context-free-expressions-constructors.rkt"
          "../cfg-struct.rkt"
          "../pda.rkt"
          "../../../sm-graph.rkt"          
          "construct-cfe-macro.rkt"
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PDA->CFE & CFE->PDA Transformations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ABCD
  (make-cfe ([A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [D (singleton-cfexp "d")])
            (concat-cfexp A B C D)))

(define ABCD2
  (make-cfe ([A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [D (singleton-cfexp "d")])
            (union-cfexp A B C D)))

(define AnCkBn
  (make-cfe ([EMPTY (empty-cfexp)]
             [A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [ASB (union-cfexp (concat-cfexp A ASB B) Ck)]
             [Ck (union-cfexp (concat-cfexp C Ck) EMPTY)])
    ASB))

(define AnCkBn2
  (make-cfe ([EMPTY (empty-cfexp)]
                          [A (singleton-cfexp "a")]
                          [B (singleton-cfexp "b")]
                          [C (singleton-cfexp "c")]
                          [CK (kleenestar-cfexp C)]
                          [AnCKbn (union-cfexp (concat-cfexp A AnCKbn B) CK)])
                         AnCKbn))

(define AiBj (make-cfe ([EMPTY (empty-cfexp)]
                        [A (singleton-cfexp "a")]
                        [B (singleton-cfexp "b")]
                        [AiBj (union-cfexp EMPTY
                                           (concat-cfexp A AiBj B)
                                           (concat-cfexp A AiBj B B))])
                       AiBj))

(define AnBn (make-cfe ([EMPTY (empty-cfexp)]
                        [A (singleton-cfexp "a")]
                        [B (singleton-cfexp "b")]
                        [AnBn (union-cfexp EMPTY (concat-cfexp A AnBn B))])
                       AnBn))

(define AnBncKUAkBnCn (make-cfe ([EMPTY (empty-cfexp)]
                                 [A (singleton-cfexp "a")]
                                 [B (singleton-cfexp "b")]
                                 [C (singleton-cfexp "c")]
                                 [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
                                 [CF (union-cfexp (concat-cfexp C CF) EMPTY)]
                                 [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
                                 [AZ (union-cfexp (concat-cfexp A AZ) EMPTY)]
                                 [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
                                AiBjCk))

(define AnBncKUAkBnCn2 (make-cfe ([EMPTY (empty-cfexp)]
                                  [A (singleton-cfexp "a")]
                                  [B (singleton-cfexp "b")]
                                  [C (singleton-cfexp "c")]
                                  [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
                                  [CF (kleenestar-cfexp C)]
                                  [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
                                  [AZ (kleenestar-cfexp A)]
                                  [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
                                 AiBjCk))


;;L = wcw^r
(define WcWr (make-cfe ([A (singleton-cfexp "a")]
                        [B (singleton-cfexp "b")]
                        [C (singleton-cfexp "c")]
                        [WcWr (union-cfexp (concat-cfexp A WcWr A)
                                           (concat-cfexp B WcWr B)
                                           C)])
                       WcWr))

(define Gina-aˆnbˆn (make-unchecked-ndpda '(S M F)
                                          '(a b)
                                          '(a)
                                          'S
                                          '(F)
                                          `(((S ,EMP ,EMP) (M ,EMP))
                                            ((S a ,EMP) (S (a)))
                                            ((M b (a)) (M ,EMP))
                                            ((M ,EMP ,EMP) (F ,EMP)))))
;;L = {wcw^r | sig = {a b}}
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
                                          ((B ,EMP ,EMP) (C ,EMP)))))

;;L = {a^nb^ma^n | n,m ≥ 0}
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

(define Gina-aˆnbˆn* (make-unchecked-ndpda '(S M F)
                                          '(a b)
                                          '(a Z)
                                          'S
                                          '(S)
                                          `(((S ,EMP ,EMP) (M (Z)))
                                            ((M a ,EMP) (M (a)))
                                            ((F b (a)) (F ,EMP))
                                            ((M ,EMP ,EMP) (F ,EMP))
                                            ((F ,EMP (Z)) (S ,EMP)))))


(define Gina-aˆnbˆn-cfe (pda->cfe Gina-aˆnbˆn))

(define Gina-wcwˆr-cfe (pda->cfe Gina-wcwˆr))

(define Gina-palindrome-pda-cfe (pda->cfe Gina-palindrome-pda))

(define Gina-AiBj-cfe (pda->cfe Gina-AiBj))

(define Gina-A^nB^mA^n-cfe (pda->cfe Gina-A^nB^mA^n))

(define Gina-a^mb^nc^pd^q-cfe (pda->cfe Gina-a^mb^nc^pd^q))

(define Gina-a^mb^nc^p-cfe (pda->cfe Gina-a^mb^nc^p))

(define anbkckdn-cfe (pda->cfe anbkckdn))

(define Gina-aˆnbˆn*-cfe (pda->cfe Gina-aˆnbˆn*))