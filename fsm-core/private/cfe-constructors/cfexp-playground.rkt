#lang racket

(require  "../fsm-core/private/constants.rkt"
          "context-free-expressions-constructors.rkt"
          "../fsm-core/private/cfg-struct.rkt"
          "../fsm-core/private/pda.rkt"
          ;"../visualizations/viz-grammar-constructors/cfg-derive-leftmost.rkt"
          "../sm-graph.rkt"
          racket/syntax-srcloc
          (for-syntax racket/base
                      syntax/parse
                      racket/set
                      syntax/parse/experimental/template
                      racket/contract/combinator
                      )
          "construct-cfe-macro.rkt"
          )

(provide (all-defined-out))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CFEXP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY (empty-cfexp))

(define A (singleton-cfexp 'a))

#;(construct-cfe [(A (singleton 'a))]
                 A)

(define B (singleton-cfexp 'b))

(define C (singleton-cfexp 'c))

;; w = ww^r
(define WWR
  (let* [(WWR (var-cfexp 'S))
         (AHA (concat-cfexp A WWR A))
         (BHB (concat-cfexp B WWR B))]
    (begin
      (update-binding! WWR 'S (union-cfexp EMPTY AHA BHB))
      WWR)))

;;w = a^nb^n
(define ANBN
  (let* [(ANBN (var-cfexp 'S))
         (ASB (concat-cfexp A ANBN B))]
    (begin
      (update-binding! ANBN 'S (union-cfexp EMPTY ASB))
      ANBN)))

(define WWRUANBN (union-cfexp WWR ANBN))

;;w = a^2ib^i
(define A2iBi
  (let* [(A2iBi (var-cfexp 'S))
         (EUAAKB (union-cfexp EMPTY (concat-cfexp A A A2iBi B)))]
    (begin
      (update-binding! A2iBi 'S EUAAKB)
      A2iBi)))

;;w = A^iB^j | i <= j <= 2i
(define AiBj
  (let* [(AiBj (var-cfexp 'A))
         (AIB (concat-cfexp A AiBj B))
         (AIBB (concat-cfexp A AiBj B B))
         (EUAIBUAIBB (union-cfexp EMPTY AIB AIBB))]
    (begin
      (update-binding! AiBj 'A EUAIBUAIBB)
      AiBj)))

(define AiBj-new
  (construct-cfe [(AiBj (var EUAIBUAIBB))
                  (EUAIBUAIBB (union EMPTY AIB AIBB))
                  (AIB (concat A AiBj B))
                  (AIBB (concat A AiBj B B))]
                 AiBj))

AiBj-new
;;w = b^na^n
(define BNAN
  (let* [(BNAN (var-cfexp 'S))
         (BSA (concat-cfexp B BNAN A))]
    (begin
      (update-binding! BNAN 'S (union-cfexp EMPTY BSA))
      BNAN)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CFG->CFE & CFE->CFG Transformations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;w = a^nb^n
(define ANBN-cfg (make-unchecked-cfg '(S)
                                     '(a b)
                                     `((S ,ARROW ,EMP) (S ,ARROW aSb))
                                     'S))

;;w = a^nb^n
(define transformed-anbn (cfg->cfe ANBN-cfg))

;;w = a*
(define thesis-cfg (make-unchecked-cfg '(S T U)
                                       '(a)
                                       `((S ,ARROW aST) (S ,ARROW U)
                                                        (T ,ARROW TU) (T ,ARROW S)
                                                        (U ,ARROW ,EMP))
                                       'S))

;;w = a*
(define transformed-thesis (cfg->cfe thesis-cfg))


;;cfe->cfg

;;w = (ab)*c
(define AB^NC
  (let* [(AB^NC (var-cfexp 'S))
         (ABX (concat-cfexp A B AB^NC))]
    (begin
      (update-binding! AB^NC 'S (union-cfexp C ABX))
      AB^NC)))

;;w = (ab)*c
(define abnc (make-unchecked-cfg '(X)
                                 '(a b c)
                                 `((X ,ARROW c) (X ,ARROW abX))
                                 'X))
;;w = (abc)^na^n
(define thesis-cfe
  (let* [(X (var-cfexp 'X))
         (Y (var-cfexp 'Y))
         (ABY (concat-cfexp A B Y))
         (CXA (concat-cfexp C X A))]
    (begin
      (update-binding! X 'X (union-cfexp EMPTY ABY))
      (update-binding! Y 'Y CXA)
      X)))

;;w = (abc)^na^n
(define thesis-cfe2
  (let* [(X (var-cfexp 'X))
         (Y (var-cfexp 'Y))
         (ABY (concat-cfexp A B Y))
         (CXA (concat-cfexp C X A))]
    (begin
      (update-binding! X 'X EMPTY)
      (update-binding! X 'X ABY)
      (update-binding! Y 'Y CXA)
      X)))

;;w = (abc)^na^n
(define thesis-cfg1 (make-unchecked-cfg '(X Y)
                                        '(a b c)
                                        `((X ,ARROW abY) (X ,ARROW ,EMP)
                                                         (Y ,ARROW cXa))
                                        'X))

;;CFE->CFG

;;w = (abc)^na^n
(define thesis-cfg-converted (cfg->cfe thesis-cfg1))

;;w = (abc)^na^n
(define thesis-cfe-converted (cfe->cfg thesis-cfe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PDA->CFE & CFE->PDA Transformations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;PDA->CFE

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

;;w = a*

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
