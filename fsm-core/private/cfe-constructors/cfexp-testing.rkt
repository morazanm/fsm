#lang racket/base

(require "./cfexp-structs.rkt"
         "../pda.rkt"
         "../cfg.rkt"
         "../constants.rkt"
         "../../../sm-graph.rkt"          
          "construct-cfe-macro.rkt"
          "../cfg-struct.rkt"
         "./context-free-expressions-constructors.rkt"
         rackunit
         rackunit/text-ui
         racket/set
         racket/list
         )

(define WORD-AMOUNT 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CFEXP;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY (make-cfe [(EMPTY (empty-cfexp))]
                 EMPTY))

(define NULL (make-cfe [(NULL (null-cfexp))]
                 NULL))

(define ONEorTWO (make-cfe ([one (singleton-cfexp "1")]
                             [two (singleton-cfexp "2")])
                      (union-cfexp one two)))


(define A (make-cfe [(A (singleton-cfexp "a"))]
                 A))



(define B (make-cfe [(B (singleton-cfexp "b"))]
                 B))

(define C (make-cfe [(C (singleton-cfexp "c"))]
                 C))

(define D (make-cfe [(D (singleton-cfexp "d"))]
                 D))

(define G (make-cfe [(B (singleton-cfexp "g"))
                     (test (kleenestar-cfexp B))]
                 test))


;; w = ww^r
(define WWR (make-cfe [(WWr (union-cfexp EMPTY
                                         (concat-cfexp A WWr A)
                                         (concat-cfexp B WWr B)))]
                      WWr))

;;w = a^nb^n
(define ANBN (make-cfe ([ASB (union-cfexp EMPTY (concat-cfexp A ASB B))])
                       ASB))

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
(define A2iBi (make-cfe ([A2iBi (union-cfexp EMPTY (concat-cfexp A A A2iBi B))])
                        A2iBi))

;;w = A^iB^j | i <= j <= 2i
(define AiBj (make-cfe ([AiBj (union-cfexp EMPTY
                          (concat-cfexp A AiBj B)
                          (concat-cfexp A AiBj B B))])
            AiBj))


;;w = b^na^n
(define BNAN (make-cfe ([BNAN (union-cfexp EMPTY (concat-cfexp B BNAN A))])
            BNAN) )

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

(define ANCKBN (make-cfe ([EMPTY (empty-cfexp)]
                          [A (singleton-cfexp "a")]
                          [B (singleton-cfexp "b")]
                          [C (singleton-cfexp "c")]
                          [CK (kleenestar-cfexp C)]
                          [AnCKbn (union-cfexp (concat-cfexp A AnCKbn B) CK)])
                         AnCKbn))
                         

;;w = a^nb^n
(define ANBN-1
  (let [(ASB (box (void)))]
    (begin
      (set-box! ASB (union-cfexp EMPTY (concat-cfexp A ASB B)))
      ASB)))

(define ANBN-2 (make-cfe ([ASB (union-cfexp EMPTY (concat-cfexp A ASB B))])
            ASB))

(define ANBN* (kleenestar-cfexp ANBN-2))

(define A-STAR (make-cfe ([EMPTY (empty-cfexp)]
             [A (singleton-cfexp "a")]
             [A* (union-cfexp EMPTY (concat-cfexp A A*))])
             A*))


(define AiBjCk2 (make-cfe ([A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
             [CF (union-cfexp (concat-cfexp C CF) EMPTY)]
             [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
             [AZ (union-cfexp (concat-cfexp A AZ) EMPTY)]
             [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
            AiBjCk))

(define AiBjCk4 (make-cfe ([A (singleton-cfexp "a")]
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
(define AB^NC (make-cfe ([ABnC (union-cfexp (concat-cfexp A B ABnC) C)])
                        ABnC))

;;w = (ab)*c
(define abnc (make-unchecked-cfg '(X)
                                 '(a b c)
                                 `((X ,ARROW c) (X ,ARROW abX))
                                 'X))
;;w = (abc)^na^n
(define thesis-cfe (make-cfe ([ABY (union-cfexp (concat-cfexp A B CXA) EMPTY)]
             [CXA (concat-cfexp C ABY A)])
            ABY))

(define thesis-cfe21
  (let ([ABY (box (void))]
        [CXA (box (void))])
    (begin
      (set-box! ABY (union-cfexp (concat-cfexp A B CXA) EMPTY))
      (set-box! CXA (concat-cfexp C ABY A))
      ABY))
    #;(make-cfe ([ABY (union-cfexp (concat-cfexp A B CXA) EMPTY)]
             [CXA (concat-cfexp C ABY A)])
            ABY))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Unit Tests;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-test-suite
  CFE-UNIT-TESTING
  (test-case
   "Concat Constructor Makes Correct CFE"
   (check-equal? (concat-cfexp) (mk-null-cfexp))
   (check-equal? (concat-cfexp NULL) (mk-null-cfexp))
   (check-equal? (concat-cfexp NULL NULL NULL NULL NULL) (mk-null-cfexp))
   (check-equal? (concat-cfexp NULL EMPTY A) (mk-null-cfexp))
   (check-equal? (concat-cfexp EMPTY) (mk-empty-cfexp))
   (check-equal? (concat-cfexp EMPTY EMPTY EMPTY EMPTY EMPTY) (mk-empty-cfexp))
   (check-equal? (concat-cfexp A) (mk-singleton-cfexp "a"))
   (check-equal? (concat-cfexp A A) (mk-concat-cfexp (vector (mk-singleton-cfexp "a") (mk-singleton-cfexp "a"))))
   (check-equal? (concat-cfexp A (union-cfexp B D) C) (mk-concat-cfexp (vector (mk-singleton-cfexp "a")
                                                                               (box (mk-union-cfexp (vector (mk-singleton-cfexp "b")
                                                                                                            (mk-singleton-cfexp "d"))))
                                                                               (mk-singleton-cfexp "c"))))
   )

  (test-case
   "Observers for singleton, union, concat, and kleenestar"
   (check-equal? (concat-cfexp-cfes (concat-cfexp A A)) (vector A A))
   (check-equal? (union-cfexp-cfes (union-cfexp C (union-cfexp A B) D)) (vector C D A B))
   (check-equal? (kleenestar-cfexp-c1 (kleenestar-cfexp A)) A)
   (check-equal? (singleton-cfexp-a C) "c")
   )
   

  (test-case
   "Union Constructor Makes Correct CFE"
   (check-equal? (union-cfexp) (mk-null-cfexp))
   (check-equal? (union-cfexp NULL EMPTY) (mk-union-cfexp (vector (mk-null-cfexp) (mk-empty-cfexp))))
   (check-equal? (union-cfexp EMPTY EMPTY EMPTY EMPTY) (mk-empty-cfexp))
   (check-equal? (union-cfexp B) (mk-singleton-cfexp "b"))
   (check-equal? (union-cfexp B D) (mk-union-cfexp (vector (mk-singleton-cfexp "b") (mk-singleton-cfexp "d"))))
   (check-equal? (union-cfexp C (union-cfexp A B) D) (mk-union-cfexp (vector (mk-singleton-cfexp "c")
                                                                             (mk-singleton-cfexp "d")
                                                                             (mk-singleton-cfexp "a")
                                                                             (mk-singleton-cfexp "b"))))

   )

  (test-case
   "Kleene Constructor Makes Correct CFE"
   (check-equal? (kleenestar-cfexp  A) (mk-kleene-cfexp (mk-singleton-cfexp "a")))
   (check-equal? (kleenestar-cfexp  EMPTY) EMPTY)
   (check-equal? (kleenestar-cfexp  NULL) NULL)
   )


  (test-case
   "CFE Predicates"
   (check-true (null-cfexp? NULL))
   (check-false (null-cfexp? EMPTY))
   (check-true (null-cfexp? (union-cfexp NULL NULL NULL)))
   (check-true (null-cfexp? (concat-cfexp A NULL NULL B NULL)))
 
   (check-true (empty-cfexp? EMPTY))
   (check-false (empty-cfexp? NULL))
   (check-true (empty-cfexp? (concat-cfexp EMPTY EMPTY EMPTY EMPTY)))
   (check-true (empty-cfexp? (union-cfexp EMPTY EMPTY EMPTY EMPTY)))
 
   (check-true (singleton-cfexp? C))
   (check-false (singleton-cfexp? (concat-cfexp A C)))
   (check-true (singleton-cfexp? (concat-cfexp A)))
   (check-true (singleton-cfexp? (union-cfexp D)))
 
   (check-true (concat-cfexp? (concat-cfexp D C)))
   (check-false (concat-cfexp? (concat-cfexp EMPTY)))
   (check-false (concat-cfexp? (concat-cfexp EMPTY EMPTY EMPTY EMPTY)))
   (check-true (concat-cfexp? (concat-cfexp EMPTY A EMPTY EMPTY EMPTY)))
   (check-false (concat-cfexp? (concat-cfexp NULL)))
   (check-false (concat-cfexp? (concat-cfexp NULL NULL NULL)))
   (check-false (concat-cfexp? (concat-cfexp A NULL NULL B NULL)))
   (check-false (concat-cfexp? (concat-cfexp A)))
 
   (check-true (union-cfexp? (union-cfexp B A)))
   (check-false (union-cfexp? (union-cfexp EMPTY)))
   (check-false (union-cfexp? (union-cfexp NULL)))
   (check-true (union-cfexp? (union-cfexp A NULL NULL C NULL)))
   (check-true (union-cfexp? (union-cfexp D NULL NULL B NULL)))
   (check-false (union-cfexp? (union-cfexp EMPTY EMPTY EMPTY EMPTY)))
   (check-false (union-cfexp? (union-cfexp NULL NULL NULL)))

   (check-true (kleenestar-cfexp? (kleenestar-cfexp  A)))
   (check-false (kleenestar-cfexp? (kleenestar-cfexp  EMPTY)))
   (check-false (kleenestar-cfexp? (kleenestar-cfexp  NULL)))
   )

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Generating Words;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;cfe natnum -> (listof word)
;;Purpose: Generates at MOST natnum amount of words generated by the given cfe
(define (gen-cfe-words cfe)
  ;; (setof word) -> (setof word)
  ;;Purpose: Generates a natnum amount of generated words
  (define (loopinator-helper acc)
    (if (= (set-count acc) WORD-AMOUNT)
        acc
        (loopinator-helper (set-add acc (gen-cfexp-word cfe)))))
  (set->list (loopinator-helper (set))))


;;cfg cfexp -> boolean
;;Purpose: Determines if the given cfe can generate a natnum amount of words that the grammar can derive
(define (grammar-checker g words)
  (for/and ([w (in-list words)])
    (list? (cfg-derive g (if (eq? w EMP) '() w)))))

;;pda cfexp -> boolean
;;Purpose: Determines if the given cfe can generate a natnum amount of words that the pda can accept
(define (pda-checker p words)
  (for/and ([w (in-list words)])
    (eq? (apply-pda p (if (eq? w EMP) '() w)) 'accept)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Langauge Predicates;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = ww^r
(define (valid-wwr-word? w)
  (or (eq? w EMP)
      (let* ([w-length (length w)]
             [half-w (take w (/ w-length 2))]
             [w^r (drop w (/ w-length 2))])
        (and (even? w-length)
             (equal? w (append half-w w^r))
             (equal? (reverse half-w) w^r)
             (equal? half-w (reverse w^r))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = a^nb^n
(define (valid-anbn-word? w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (even? (length w))
             (equal? w (append as bs))
             (= (length as) (length bs))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = b^na^n
(define (valid-bnan-word? w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (even? (length w))
             (equal? w (append bs as))
             (= (length as) (length bs))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = a^2ib^i
(define (valid-a2ibi-word? w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (equal? w (append as bs))
             (= (length as) (* 2 (length bs)))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = A^iB^j | i <= j <= 2i
(define (valid-aibj-word? w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (equal? w (append as bs))
             (<= (length as) (length bs) (* 2 (length as)))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for A^iB^jC^k | i = j V j = k
(define (valid-aibjck-word? w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)]
            [cs (filter (λ (s) (eq? s 'c)) w)])
        (and (equal? w (append as bs cs))
             (or (= (length as) (length bs))
                 (= (length bs) (length cs)))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = a*
(define (valid-A*-word? a-word)
  (or (eq? a-word EMP)
      (andmap (λ (w) (eq? w 'a)) a-word)))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = aˆnbˆn
(define (valid-Gina-aˆnbˆn-word? ci)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (equal? (append as bs) ci)
         (= (length as) (length bs)))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = wcwˆr
(define (valid-Gina-wcwˆr-word? ci)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))]
    (equal? ci (append w (list 'c) (reverse w)))))


;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w is a palindrome
(define (valid-Gina-palindrome-pda-word? ci)
  (or (empty? ci)
      (equal? ci (reverse ci))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = AiBj where i<= j <= 2i
(define (valid-Gina-AiBj-word? ci)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (<= (length As) (length Bs) (* 2 (length As)))
         (equal? ci (append As Bs)))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = A^nB^mA^n
(define (valid-Gina-A^nB^mA^n-word? ci)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (As-after-Bs (takef (drop ci (length (append As Bs))) (λ (x) (eq? x 'a))))]
    (or (and (equal? ci As)
             (even? (length As))) 
        (and (= (- (length As) (length As-after-Bs)) 0) 
             (equal? (append As Bs As-after-Bs) ci)))))


(define (valid-Gina-a^mb^nc^pd^q-word? ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? x 'c))))
         (Ds (takef (drop ci (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? x 'd))))]
    (and (equal? (append As Bs Cs Ds) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= 0 (- (+ (length As) (length Bs)) (length Cs) (length Ds))))))

;;word -> boolean
;;Purpose: Determines if the given word is a valid word for w = a^mb^nc^p
(define (valid-Gina-a^mb^nc^p-word? ci)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? 'c x))))]
    (and (equal? ci (append As Bs Cs))
         (or (= 0 (- (length Bs) (length Cs)))
             (= 0 (- (length As) (length Bs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LANGUAGE BANK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define WWR-WORDS (gen-cfe-words WWR))

(define ANBN-WORDS (gen-cfe-words ANBN))

(define BNAN-WORDS (gen-cfe-words BNAN))

(define A2iBi-WORDS (gen-cfe-words A2iBi))

(define AiBj-WORDS (gen-cfe-words AiBj))

(define TRANSFORMED-ANBN-WORDS (gen-cfe-words transformed-anbn))

(define TRANSFORMED-BNAN-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg BNAN))))

(define TRANSFORMED-WWR-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg WWR))))

(define TRANSFORMED-AiBj-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg AiBj))))

(define TRANSFORMED-A2iBi-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg A2iBi))))

(define thesis-cfg-converted-WORDS (gen-cfe-words thesis-cfg-converted))

(define thesis-cfe-WORDS (gen-cfe-words thesis-cfe))

(define A*-WORDS (gen-cfe-words A*-cfe))

(define Gina-aˆnbˆn-WORDS (gen-cfe-words Gina-aˆnbˆn-cfe))

(define Gina-wcwˆr-WORDS (gen-cfe-words Gina-wcwˆr-cfe))

(define Gina-palindrome-pda-WORDS (gen-cfe-words Gina-palindrome-pda-cfe))

(define Gina-AiBj-WORDS (gen-cfe-words Gina-AiBj-cfe))

(define Gina-A^nB^mA^n-WORDS (gen-cfe-words Gina-A^nB^mA^n-cfe))

(define Gina-a^mb^nc^pd^q-WORDS (gen-cfe-words Gina-a^mb^nc^pd^q-cfe))

(define Gina-a^mb^nc^p-WORDS (gen-cfe-words Gina-a^mb^nc^p-cfe))

(define converted-ANBN-WORDS (gen-cfe-words converted-ANBN))

(define converted-BNAN-WORDS (gen-cfe-words (pda->cfe (cfe->pda BNAN))))

(define converted-WWR-WORDS (gen-cfe-words (pda->cfe (cfe->pda WWR))))

(define converted-A2iBi-WORDS (gen-cfe-words (pda->cfe (cfe->pda A2iBi))))

(define converted-AiBj-WORDS (gen-cfe-words (pda->cfe (cfe->pda AiBj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TESTING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case
 "PDA->CFE Word Generation"
 (check-pred (λ (low) (andmap valid-A*-word? low)) A*-WORDS)

 (check-pred (λ (low) (andmap valid-anbn-word? low)) converted-ANBN-WORDS)

 (check-pred (λ (low) (andmap valid-bnan-word? low)) converted-BNAN-WORDS)

 (check-pred (λ (low) (andmap valid-wwr-word? low)) converted-WWR-WORDS)

 (check-pred (λ (low) (andmap valid-a2ibi-word? low)) converted-A2iBi-WORDS)

 (check-pred (λ (low) (andmap valid-aibj-word? low)) converted-AiBj-WORDS)

 (check-pred (λ (low) (andmap valid-Gina-aˆnbˆn-word? low)) Gina-aˆnbˆn-WORDS) 

 (check-pred (λ (low) (andmap valid-Gina-wcwˆr-word? low)) Gina-wcwˆr-WORDS)

 (check-pred (λ (low) (andmap valid-Gina-palindrome-pda-word? low)) Gina-palindrome-pda-WORDS)

 (check-pred (λ (low) (andmap valid-Gina-AiBj-word? low)) Gina-AiBj-WORDS)

 (check-pred (λ (low) (andmap valid-Gina-A^nB^mA^n-word? low)) Gina-A^nB^mA^n-WORDS)

 (check-pred (λ (low) (andmap valid-Gina-a^mb^nc^pd^q-word? low)) Gina-a^mb^nc^pd^q-WORDS)

 (check-pred (λ (low) (andmap valid-Gina-a^mb^nc^p-word? low)) Gina-a^mb^nc^p-WORDS)
 )

(test-case "CFE->PDA Word Membership"

           (check-true (pda-checker (cfe->pda ANBN) converted-ANBN-WORDS))

           (check-true (pda-checker (cfe->pda BNAN) converted-BNAN-WORDS))

           (check-true (pda-checker (cfe->pda AiBj) converted-AiBj-WORDS))

           (check-true (pda-checker (cfe->pda A2iBi) converted-A2iBi-WORDS))

           (check-true (pda-checker (cfe->pda WWR) converted-WWR-WORDS))
           )

(test-case
 "PDA->CFE Word Membership" 

 (check-true (pda-checker Gina-aˆnbˆn Gina-aˆnbˆn-WORDS))

 (check-true (pda-checker Gina-wcwˆr Gina-wcwˆr-WORDS))

 (check-true (pda-checker Gina-palindrome-pda Gina-palindrome-pda-WORDS))

 (check-true (pda-checker Gina-AiBj Gina-AiBj-WORDS))

 (check-true (pda-checker Gina-A^nB^mA^n Gina-A^nB^mA^n-WORDS))

 (check-true (pda-checker Gina-a^mb^nc^pd^q Gina-a^mb^nc^pd^q-WORDS))

 (check-true (pda-checker Gina-a^mb^nc^p Gina-a^mb^nc^p-WORDS))

 (check-true (pda-checker (cfe->pda Gina-aˆnbˆn-cfe) Gina-aˆnbˆn-WORDS))

 (check-true (pda-checker (cfe->pda Gina-wcwˆr-cfe) Gina-wcwˆr-WORDS))

 (check-true (pda-checker (cfe->pda Gina-palindrome-pda-cfe) Gina-palindrome-pda-WORDS))

 (check-true (pda-checker (cfe->pda Gina-AiBj-cfe) Gina-AiBj-WORDS))

 (check-true (pda-checker (cfe->pda Gina-A^nB^mA^n-cfe) Gina-A^nB^mA^n-WORDS))

 (check-true (pda-checker (cfe->pda Gina-a^mb^nc^pd^q-cfe) Gina-a^mb^nc^pd^q-WORDS))

 (check-true (pda-checker (cfe->pda Gina-a^mb^nc^p-cfe) Gina-a^mb^nc^p-WORDS))
 )
|#

(define CFE-WORD-TESTS
  (let [(WWR-WORDS (gen-cfe-words WWR))

        (ANBN-WORDS (gen-cfe-words ANBN))

        (BNAN-WORDS (gen-cfe-words BNAN))

        (A2iBi-WORDS (gen-cfe-words A2iBi))

        (AiBj-WORDS (gen-cfe-words AiBj))

        (TRANSFORMED-ANBN-WORDS (gen-cfe-words transformed-anbn))

        (TRANSFORMED-BNAN-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg BNAN))))

        (TRANSFORMED-WWR-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg WWR))))

        (TRANSFORMED-AiBj-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg AiBj))))

        (TRANSFORMED-A2iBi-WORDS (gen-cfe-words (cfg->cfe (cfe->cfg A2iBi))))

        (thesis-cfg-converted-WORDS (gen-cfe-words thesis-cfg-converted))

        (thesis-cfe-WORDS (gen-cfe-words thesis-cfe))]
    (test-suite
     "Word-Generation-&-Membership"
     (test-case
      "CFE Word Generation"

      (check-pred (λ (low) (andmap valid-wwr-word? low)) WWR-WORDS)

      (check-pred (λ (low) (andmap valid-anbn-word? low)) ANBN-WORDS)

      (check-pred (λ (low) (andmap valid-bnan-word? low)) BNAN-WORDS)

      (check-pred (λ (low) (andmap valid-a2ibi-word? low)) A2iBi-WORDS)

      (check-pred (λ (low) (andmap valid-aibj-word? low)) AiBj-WORDS)
      )



     (test-case
      "CFG->CFE Word Membership"

      (check-pred (λ (low) (andmap valid-anbn-word? low)) TRANSFORMED-ANBN-WORDS)

      (check-pred (λ (low) (andmap valid-bnan-word? low)) TRANSFORMED-BNAN-WORDS)

      (check-pred (λ (low) (andmap valid-wwr-word? low)) TRANSFORMED-WWR-WORDS)

      (check-pred (λ (low) (andmap valid-a2ibi-word? low)) TRANSFORMED-A2iBi-WORDS)

      (check-pred (λ (low) (andmap valid-aibj-word? low)) TRANSFORMED-AiBj-WORDS)
      )

     (test-case
      "CFE->CFG Word Membership" 

      (check-true (grammar-checker thesis-cfg1 thesis-cfg-converted-WORDS))

      (check-true (grammar-checker thesis-cfe-converted thesis-cfe-WORDS))

      (check-true (grammar-checker (cfe->cfg ANBN) TRANSFORMED-ANBN-WORDS))

      (check-true (grammar-checker (cfe->cfg BNAN) TRANSFORMED-BNAN-WORDS))

      (check-true (grammar-checker (cfe->cfg AiBj) TRANSFORMED-AiBj-WORDS))

      (check-true (grammar-checker (cfe->cfg A2iBi) TRANSFORMED-A2iBi-WORDS))

      (check-true (grammar-checker (cfe->cfg WWR) TRANSFORMED-WWR-WORDS))
      )

     )))
(define (run-testing)
  (begin
    (run-tests CFE-UNIT-TESTING)
    (run-tests CFE-WORD-TESTS)
    (void)))