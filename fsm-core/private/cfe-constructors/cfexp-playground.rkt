#lang racket/base

(require  "../constants.rkt"
          "context-free-expressions-constructors.rkt"
          "../cfg-struct.rkt"
          "../pda.rkt"
          ;"../visualizations/viz-grammar-constructors/cfg-derive-leftmost.rkt"
          "../../../sm-graph.rkt"          
          "construct-cfe-macro.rkt"
          )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CFEXP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY (make-cfe [(EMPTY (empty-cfexp))]
                 EMPTY
                 #;(empty))
  #;(empty-cfexp))

(define NULL (make-cfe [(NULL (null-cfexp))]
                 NULL))

(define ONEorTWO (make-cfe ([one (singleton-cfexp "1")]
                             [two (singleton-cfexp "2")])
                      (union-cfexp one two)))


(define A (make-cfe [(A (singleton-cfexp "a"))]
                 A)
  #;(singleton-cfexp 'a))

#;(make-cfe [(A (singleton-cfexp 'a))]
                 A)

(define B (make-cfe [(B (singleton-cfexp "b"))]
                 B)
  #;(singleton-cfexp 'b))

(define C (make-cfe [(C (singleton-cfexp "c"))]
                 C)
  #;(singleton-cfexp 'c))

(define D (make-cfe [(D (singleton-cfexp "d"))]
                 D))

#;(singleton-cfexp 'a)
(define G (make-cfe [(B (singleton-cfexp "g"))
                     (test (kleenestar-cfexp B))]
                 test))


;; w = ww^r
(define WWR
  (make-cfe [(WWr (union-cfexp EMPTY
                         (concat-cfexp A WWr A)
                         (concat-cfexp B WWr B)))]
            WWr)
  #;(let* [(WWR (var-cfexp 'S))
           (AHA (concat-cfexp A WWR A))
           (BHB (concat-cfexp B WWR B))]
      (begin
        (update-binding! WWR 'S (union-cfexp EMPTY AHA BHB))
        WWR)))

;;w = a^nb^n
(define ANBN
  (make-cfe ([ASB (union-cfexp EMPTY (concat-cfexp A ASB B))])
            ASB)
  #;(let* [(ANBN (var-cfexp 'S))
         (ASB (concat-cfexp A ANBN B))]
    (begin
      (update-binding! ANBN 'S (union-cfexp EMPTY ASB))
      ANBN)))

(define WWRUANBN (make-cfe ([WWrUAnBn (union-cfexp WWR ANBN)])
                           WWrUAnBn))

(define AUB (make-cfe ([AUB (union-cfexp A B)])
                      AUB))
(define cAUB (make-cfe ([AUB (union-cfexp A B)]
                        [aAUB (concat-cfexp C AUB)])
                      aAUB))

(define cAUB2 (make-cfe ([aAUB (concat-cfexp C (union-cfexp A B))]) ;; now boxes the union-cfexp 
                      aAUB))

(define CUAUB (make-cfe ([CUAUB (union-cfexp C (union-cfexp A B))]) ;;now lifts nested unions
                      CUAUB))

(define CUAUBUD (make-cfe ([CUAUBUD (union-cfexp C (union-cfexp A (union-cfexp D B)))]) ;;now lifts nested unions
                      CUAUBUD))


;;w = a^2ib^i
(define A2iBi
  (make-cfe ([A2iBi (union-cfexp EMPTY (concat-cfexp A A A2iBi B))])
            A2iBi)
            #;(let* [(A2iBi (var-cfexp 'S))
         (EUAAKB (union-cfexp EMPTY (concat-cfexp A A A2iBi B)))]
    (begin
      (update-binding! A2iBi 'S EUAAKB)
      A2iBi)))

;;w = A^iB^j | i <= j <= 2i
(define AiBj
  (make-cfe ([AiBj (union-cfexp EMPTY
                          (concat-cfexp A AiBj B)
                          (concat-cfexp A AiBj B B))])
            AiBj)
            #;(let* [(AiBj (var-cfexp 'A))
         (AIB (concat-cfexp A AiBj B))
         (AIBB (concat-cfexp A AiBj B B))
         (EUAIBUAIBB (union-cfexp EMPTY AIB AIBB))]
    (begin
      (update-binding! AiBj 'A EUAIBUAIBB)
      AiBj)))

(define AiBj-new
  (make-cfe ([AiBj (union-cfexp EMPTY
                          (concat-cfexp A AiBj B)
                          (concat-cfexp A AiBj B B))
                  #;(EUAIBUAIBB (union-cfexp EMPTY AIB AIBB))
                  #;(AIB (concat-cfexp A AiBj B))
                  #;(AIBB (concat-cfexp A AiBj B B))])
                 AiBj))

;AiBj-new

;;w = b^na^n
(define BNAN
  (make-cfe ([BNAN (union-cfexp EMPTY (concat-cfexp B BNAN A))])
            BNAN)
  #;(let* [(BNAN (var-cfexp 'S))
         (BSA (concat-cfexp B BNAN A))]
    (begin
      (update-binding! BNAN 'S (union-cfexp EMPTY BSA))
      BNAN)))

;;w = a^ib^jc^k, i=j or j=k

;;AiBjCk =>  a^ib^ic^k or a^ib^kc^k, i=j or j=k
;;EF => a^ib^ic^k 
;;E => a^ib^i
;;F => c^k
;;ZW => a^ib^kc^k
;;Z => a^i
;;W => b^kc^k
;;A -> 'a
;;B -> 'b
;;C -> 'c

(define AiBjCk3
  (let ([EMPTY (empty-cfexp)]
        [A (singleton-cfexp "a")]
        [B (singleton-cfexp "b")]
        [C (singleton-cfexp "c")]
        [AEB (box (void))] ;; AEB = A^iB^j, i=j
        [CF (box (void))]
        [BWC (box (void))] ;;BWC = B^jC^k, j=k
        [AZ (box (void))])
    (begin
      (set-box! AEB (union-cfexp EMPTY (concat-cfexp A AEB B)))
      (set-box! CF (union-cfexp (concat-cfexp C CF) EMPTY))
      (set-box! BWC (union-cfexp (concat-cfexp B BWC C) EMPTY))
      (set-box! AZ (union-cfexp (concat-cfexp A AZ) EMPTY))      
      (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC)))))

;;w = a^nc^kb^n
(define AnCkBn
  (let ([EMPTY (empty-cfexp)]
        [A (singleton-cfexp "a")]
        [B (singleton-cfexp "b")]
        [C (singleton-cfexp "c")]
        [ASB (box (void))]
        [Ck (box (void))])
    (begin
      (set-box! ASB (union-cfexp (concat-cfexp A ASB B) Ck))
      (set-box! Ck (union-cfexp (concat-cfexp C Ck) EMPTY))
      ASB)))

;;w = a^nb^n
(define ANBN-1
  (let [(ASB (box (void)))]
    (begin
      (set-box! ASB (union-cfexp EMPTY (concat-cfexp A ASB B)))
      ASB)))

(define ANBN-2
  (make-cfe ([ASB (union-cfexp EMPTY (concat-cfexp A ASB B))])
            ASB)
  #;(let [(ASB (box (void)))]
    (begin
      (set-box! ASB (union-cfexp EMPTY (concat-cfexp A ASB B)))
      ASB)))

(define ANBN* (kleenestar-cfexp ANBN-2))

(define A-STAR
  #;(let ([EMPTY (empty-cfexp)]
        [A (singleton-cfexp "a")]
        [A* (box (void))])
    (begin
      (set-box! A* (union-cfexp EMPTY (concat-cfexp A A*)))
      A*))
  (make-cfe ([EMPTY (empty-cfexp)]
             [A (singleton-cfexp "a")]
             [A* (union-cfexp EMPTY (concat-cfexp A A*))])
             A*))

#;(define AiBjCk
  (make-cfe ([A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
             [CF (union-cfexp (concat-cfexp C CF) EMPTY)]
             [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
             [AZ (union-cfexp (concat-cfexp A AZ) EMPTY)])
            (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC)))
  #;(make-cfe ([A (singleton-cfexp "a")]
               [B (singleton-cfexp "b")]
               [C (singleton-cfexp "c")]
               [AEB #;(union-cfexp EMPTY #(concat-cfexp A AEB B)) (concat-cfexp A E B)] ;; AEB = A^iB^j, i=j
               [CF (concat-cfexp C F)] ;;c^k
               [AEBUEMP (union-cfexp AEB EMPTY)] ;;AEB U EMP
               [CFUEMP (union-cfexp CF EMPTY)] ;;CF U EMP
               [E (var AEBUEMP)]
               [F (var CFUEMP)]
               [EF (concat-cfexp E F)] ;;a^ib^jc^k, i=j
               [BWC (concat-cfexp B W C)] ;;BWC = B^jC^k, j=k
               [AZ (concat-cfexp A Z)] ;;a^i
               [BWCUEMP (union-cfexp BWC EMPTY)]
               [AZUEMP (union-cfexp AZ EMPTY)]
               [W (var BWCUEMP)]
               [Z (var AZUEMP)]
               [ZW (concat-cfexp Z W)] ;;a^ib^jc^k, j=k
               [EFUWZ (union-cfexp EF ZW)]
               [AiBjCk (var EFUWZ)])
              AiBjCk))

(define AiBjCk2
  (make-cfe ([A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
             [CF (union-cfexp (concat-cfexp C CF) EMPTY)]
             [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
             [AZ (union-cfexp (concat-cfexp A AZ) EMPTY)]
             [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
            AiBjCk)
  #;(make-cfe ([A (singleton-cfexp "a")]
                  [B (singleton-cfexp "b")]
                  [C (singleton-cfexp "c")]
                  [AEB (concat-cfexp A E B)] ;; AEB = A^iB^j, i=j
                  [CF (concat-cfexp C F)] ;;c^k
                  [AEBUEMP (union-cfexp AEB EMPTY)] ;;AEB U EMP
                  [CFUEMP (union-cfexp CF EMPTY)] ;;CF U EMP
                  [E (var AEBUEMP)]
                  [F (var CFUEMP)]
                  [EF (concat-cfexp E F)] ;;a^ib^jc^k, i=j
                  [BWC (concat-cfexp B W C)] ;;BWC = B^jC^k, j=k
                  [AZ (concat-cfexp A Z)] ;;a^i
                  [BWCUEMP (union-cfexp BWC EMPTY)]
                  [AZUEMP (union-cfexp AZ EMPTY)]
                  [W (var BWCUEMP)]
                  [Z (var AZUEMP)]
                  [ZW (concat-cfexp Z W)] ;;a^ib^jc^k, j=k
                  [AiBjCk (union-cfexp EF ZW)])
                 AiBjCk))

(define AiBjCk4
  (make-cfe ([A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
             [CF (kleenestar-cfexp C)]
             [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
             [AZ (kleenestar-cfexp A)]
             [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
            AiBjCk))


;;L = wcw^r
(define WcWr (make-cfe ([WcWr (union-cfexp (concat-cfexp A WcWr A)
                                     (concat-cfexp B WcWr B)
                                     C)])
                       WcWr))

(define G2 (cfe->cfg AiBjCk2))

;;We do NOT need kleene because the variable binding is functionally equivalent.
;;   L*    = L U EMP
;; kleene^    var^
;;We *could* use a kleene BUT it would be harder to understand and read. Especially since the implementation
;;of CFEs are closely related to CFGs. Consider the cfg for L = a*, the rules would be S -> aS and S -> EMP
;;the way of using a var (L U EMP) is directly reflects how the cfg rules would look AND captures the same
;;(if not better) nature of kleene. Also, it makes converting easier since there is no need for the kleene
;;as theres one less expression to check for AND everything I need would be found in the environment of the

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
  (make-cfe ([ABnC (union-cfexp (concat-cfexp A B ABnC) C)])
            ABnC)
  #;(let* [(AB^NC (var-cfexp 'S))
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
  #;(let ([ABY (box (void))]
        [CXA (box (void))])
    (begin
      (set-box! ABY (union-cfexp (concat-cfexp A B CXA) EMPTY))
      (set-box! CXA (concat-cfexp C ABY A))
      ABY))
  (make-cfe ([ABY (union-cfexp (concat-cfexp A B CXA) EMPTY)]
             [CXA (concat-cfexp C ABY A)])
            ABY)
  #;(let* [(X (var-cfexp 'X))
         (Y (var-cfexp 'Y))
         (ABY (concat-cfexp A B Y))
         (CXA (concat-cfexp C X A))]
    (begin
      (update-binding! X 'X (union-cfexp EMPTY ABY))
      (update-binding! Y 'Y CXA)
      X)))

(define thesis-cfe21
  (let ([ABY (box (void))]
        [CXA (box (void))])
    (begin
      (set-box! ABY (union-cfexp (concat-cfexp A B CXA) EMPTY))
      (set-box! CXA (concat-cfexp C ABY A))
      ABY))
    #;(make-cfe ([ABY (union-cfexp (concat-cfexp A B CXA) EMPTY)]
             [CXA (concat-cfexp C ABY A)])
            ABY)
  #;(let* [(X (var-cfexp 'X))
         (Y (var-cfexp 'Y))
         (ABY (concat-cfexp A B Y))
         (CXA (concat-cfexp C X A))]
    (begin
      (update-binding! X 'X (union-cfexp EMPTY ABY))
      (update-binding! Y 'Y CXA)
      X)))

;;w = (abc)^na^n
#;(define thesis-cfe2
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
