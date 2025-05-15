#lang racket/base

(require
  (for-syntax syntax/parse
              racket/base
              "fsm-syntax-classes.rkt")
  "../tm.rkt"
  "../constants.rkt"
  racket/contract
  "../sm-getters.rkt"
  racket/list
  syntax/parse
  "fsm-syntax-classes.rkt"
  racket/syntax
  racket/function
  racket/syntax-srcloc
  "../sm-apply.rkt"
  "check-accept-reject-failure-strings.rkt"
  "check-exn.rkt"
  "check-utils.rkt")

(provide check-turing-machine)

(define (map-to-val invalid-exprs all-exprs map-to-vals)
  (let ([idxs (map (lambda (x) (index-of all-exprs x)) invalid-exprs)])
    (map (lambda (x) (list-ref map-to-vals x)) idxs)))

(define (build-contract lst-deps)
  (map (lambda (x) (lambda (y) (> (length x) y))) lst-deps))

(struct syntax-value-obj (stx val))


;; use curry and apply to use dependencies provided
;; error-id should probably be in str deps or hidden behind constructor (actually perhaps both?)

(define (accept M M-stx orig-val-lst orig-stx-lst unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst head-pos-stx-lst)
  (check-syntax (list (fsm-error (macro-contract correct-tm-word-arity/c) M-stx 'machine-invalid-arity
                                  #:highlight-stx-fmt (lambda (stxs) (list (first stxs))))
                      orig-val-lst
                      orig-stx-lst)
                (list (fsm-error (macro-contract list?) M-stx 'machine-invalid-word
                                 #:highlight-stx-fmt (lambda (stxs) (list (first stxs))))
                      unprocessed-word-lst
                      unprocessed-word-stx-lst)
                (list (fsm-error (macro-contract-dep (map (lambda (x) (valid-head-pos/c (length x))) unprocessed-word-lst)) M-stx 'machine-invalid-head-pos
                                                          #:str-fmt (lambda (vals stxs) (map-to-val
                                                                                         stxs
                                                                                         head-pos-stx-lst
                                                                                         unprocessed-word-lst)))
                      head-pos-lst
                      head-pos-stx-lst)
                (list (fsm-error (macro-contract-dep (build-contract unprocessed-word-lst)) M-stx 'machine-invalid-head-pos-index
                                 #:str-fmt (lambda (vals stxs)
                                             (map-to-val stxs
                                                         head-pos-stx-lst
                                                         unprocessed-word-lst)))
                       head-pos-lst
                       head-pos-stx-lst)
                (list (fsm-error (macro-contract valid-tm-word/c) M-stx 'machine-no-left-hand-marker)
                      unprocessed-word-lst unprocessed-word-stx-lst)
                (list (fsm-error (macro-contract (listof (apply or/c (cons '@ (cons '_ (sm-sigma M))))))
                                 M-stx
                                 'machine-invalid-nonterminal)
                      unprocessed-word-lst unprocessed-word-stx-lst)
                (list (fsm-error (macro-contract-final-check (lambda (val head-pos) (equal? (sm-apply M val head-pos) 'accept)))
                                 M-stx
                                 'machine-accept)
                     unprocessed-word-lst
                     unprocessed-word-stx-lst
                     head-pos-lst))
  
  #;(let* ([wrong-arities (accumulate-invalid-words correct-tm-word-arity/c orig-val-lst orig-stx-lst)]
         [wrong-arity-words (map first wrong-arities)]
         [wrong-arity-word-stx (map second wrong-arities)])
    (if (empty? wrong-arities)
        (let* ([invalid-word (accumulate-invalid-words list? unprocessed-word-lst unprocessed-word-stx-lst)]
               [invalid-word-lst (map first invalid-word)]
               [invalid-word-stx (map second invalid-word)])
          (if (empty? invalid-word)
          (let* ([non-natural-head-pos (accumulate-invalid-words valid-head-pos/c head-pos-lst head-pos-stx-lst)]
                 [non-natural-head-pos-vals (map first non-natural-head-pos)]
                 [non-natural-head-pos-stx (map second non-natural-head-pos)])
            (if (empty? non-natural-head-pos)
                (let* ([invalid-head-pos-index (accumulate-invalid-words-dep build-contract head-pos-lst head-pos-stx-lst unprocessed-word-lst)]
                       [invalid-head-pos-index-vals (map first invalid-head-pos-index)]
                       [invalid-head-pos-index-stx (map second invalid-head-pos-index)])
                  (if (empty? invalid-head-pos-index)
                      (let* ([no-left-hand-marker (accumulate-invalid-words valid-tm-word/c unprocessed-word-lst unprocessed-word-stx-lst)]
                             [no-left-hand-marker-vals (map first no-left-hand-marker)]
                             [no-left-hand-marker-stx (map second no-left-hand-marker)])
                        (if (empty? no-left-hand-marker)
                            (let* ([new-contract (listof (apply or/c (cons '@ (cons '_ (sm-sigma M)))))]
                                   [invalids (accumulate-invalid-words new-contract unprocessed-word-lst unprocessed-word-stx-lst)]
                                   [invalid-words (map first invalids)]
                                   [invalid-word-stx (map second invalids)])
                              (if (empty? invalids)
                                  (let* ([res (foldr (lambda (val word-val head-pos accum)
                                                       (if (equal? (sm-apply M word-val head-pos) 'accept)
                                                           accum
                                                           (cons (list val word-val) accum)))
                                                     '()
                                                     unprocessed-word-stx-lst
                                                     unprocessed-word-lst
                                                     head-pos-lst)]
                                         [word-lst (map second res)]
                                         [word-stx-lst (map first res)])
                                    (unless (empty? res)
                                      (let ([failure-str (create-failure-str machine-accept M-stx word-lst)])
                                        (display-exception failure-str (exn:fail:check-failed
                                                                          failure-str
                                                                          (current-continuation-marks)
                                                                          (map syntax-srcloc word-stx-lst))))))
                                  (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
                                    (display-exception failure-str (exn:fail:check-failed
                                                                      failure-str
                                                                      (current-continuation-marks)
                                                                      (map syntax-srcloc invalid-word-stx))))))
                            (let ([failure-str (create-failure-str machine-no-left-hand-marker M-stx no-left-hand-marker-vals)])
                              (display-exception failure-str (exn:fail:check-failed
                                                                failure-str
                                                                (current-continuation-marks)
                                                                (map syntax-srcloc no-left-hand-marker-stx))))))
                      (let ([failure-str (create-failure-str machine-invalid-head-pos-index
                                                             M-stx
                                                             (map-to-val invalid-head-pos-index-stx
                                                                         head-pos-stx-lst
                                                                         unprocessed-word-lst))])
                        (display-exception failure-str (exn:fail:check-failed
                                                          failure-str
                                                          (current-continuation-marks)
                                                          (map syntax-srcloc invalid-head-pos-index-stx))))))
                (let ([failure-str (create-failure-str machine-invalid-head-pos M-stx (map-to-val
                                                                                       non-natural-head-pos-stx
                                                                                       head-pos-stx-lst
                                                                                       unprocessed-word-lst))])
                  (display-exception failure-str (exn:fail:check-failed
                                                    failure-str
                                                    (current-continuation-marks)
                                                    (map syntax-srcloc non-natural-head-pos-stx))))))
          (let ([failure-str (create-failure-str machine-invalid-word M-stx invalid-word-lst)])
            (display-exception failure-str (exn:fail:check-failed
                                              failure-str
                                              (current-continuation-marks)
                                              (map syntax-srcloc unprocessed-word-stx-lst))))))
        (let ([failure-str (create-failure-str machine-invalid-arity M-stx (list (first wrong-arity-words)))])
          (display-exception failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (list ( syntax-srcloc (first wrong-arity-word-stx)))))))))

(define (zip . lists)
  (apply map list lists))

;; Syntax -> Syntax
;; Purpose: Checks turing machines
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M tests tests-stx)
     #`(let* ([results (foldr (lambda (val accum)
                                (let ([res (syntax-parse val
                                             [(~var list-two-exprs list-tm-word)
                                              (list #'list-two-exprs #'list-two-exprs.word #'list-two-exprs.head-pos)]
                                             [(~var quoted-two-exprs quoted-tm-word)
                                              (list #'quoted-two-exprs #'quoted-two-exprs.word #'quoted-two-exprs.head-pos)]
                                             [(~var quasiquoted-two-exprs quasiquoted-tm-word)
                                              (list #'quasiquoted-two-exprs #'quasiquoted-two-exprs.word #'quasiquoted-two-exprs.head-pos)]
                                             [(~var else-clause)
                                              '()])])
                                  (if (empty? res)
                                      accum
                                      (cons res accum))))
                              '()
                              tests-stx)]
              [two-exprs-stx-lst (map first results)]
              [two-exprs-word-stx-lst (map second results)]
              [two-exprs-head-pos-stx-lst (map third results)]
              [head-pos-stx-lst (map (lambda (orig-lst-val orig-lst-stx)
                                       (let ([res-idx (index-of (map syntax-srcloc two-exprs-stx-lst) (syntax-srcloc orig-lst-stx))])
                                         (if res-idx
                                             (list orig-lst-val
                                                   orig-lst-stx
                                                   (first orig-lst-val)
                                                   (list-ref two-exprs-word-stx-lst res-idx)
                                                   (second orig-lst-val)
                                                   (list-ref two-exprs-head-pos-stx-lst res-idx))
                                             (list orig-lst-val
                                                   orig-lst-stx
                                                   ;; Do this because the error reporting is going to catch this later on
                                                   (if (list? orig-lst-val)
                                                       (if (empty? orig-lst-val)
                                                       '()
                                                       (first orig-lst-val))
                                                       orig-lst-val)
                                                   orig-lst-stx
                                                   ;; Do this because the error reporting is going to catch this later on
                                                   (if (list? orig-lst-val)
                                                       (if (> (length orig-lst-val) 1)
                                                       (second orig-lst-val)
                                                       '())
                                                       orig-lst-val)
                                                   orig-lst-stx))))
                                     tests
                                     tests-stx)])
         (accept M
                 #'M
                 (map first head-pos-stx-lst)
                 (map second head-pos-stx-lst)
                 (map third head-pos-stx-lst)
                 (map fourth head-pos-stx-lst)
                 (map fifth head-pos-stx-lst)
                 (map sixth head-pos-stx-lst)))]))

(define (reject M M-stx orig-val-lst orig-stx-lst unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst head-pos-stx-lst)
  (void)
  #;(let* ([wrong-arities (accumulate-invalid-words correct-tm-word-arity/c orig-val-lst orig-stx-lst)]
         [wrong-arity-words (map first wrong-arities)]
         [wrong-arity-word-stx (map second wrong-arities)])
    (if (empty? wrong-arities)
        (let* ([non-natural-head-pos (accumulate-invalid-words valid-head-pos/c head-pos-lst head-pos-stx-lst)]
               [non-natural-head-pos-vals (map first non-natural-head-pos)]
               [non-natural-head-pos-stx (map second non-natural-head-pos)])
          (if (empty? non-natural-head-pos)
              (let* ([invalid-head-pos-index (accumulate-invalid-words-dep build-contract head-pos-lst head-pos-stx-lst unprocessed-word-lst)]
                     [invalid-head-pos-index-vals (map first invalid-head-pos-index)]
                     [invalid-head-pos-index-stx (map second invalid-head-pos-index)])
                (if (empty? invalid-head-pos-index)
                    (let* ([no-left-hand-marker (accumulate-invalid-words valid-tm-word/c unprocessed-word-lst unprocessed-word-stx-lst)]
                           [no-left-hand-marker-vals (map first no-left-hand-marker)]
                           [no-left-hand-marker-stx (map second no-left-hand-marker)])
                      (if (empty? no-left-hand-marker)
                          (let* ([new-contract (listof (apply or/c (cons '@ (cons '_ (sm-sigma M)))))]
                                 [invalids (accumulate-invalid-words new-contract unprocessed-word-lst unprocessed-word-stx-lst)]
                                 [invalid-words (map first invalids)]
                                 [invalid-word-stx (map second invalids)])
                            (if (empty? invalids)
                                (let* ([res (foldr (lambda (val word-val head-pos accum)
                                                     (if (equal? (sm-apply M word-val head-pos) 'reject)
                                                         accum
                                                         (cons (list val word-val) accum)))
                                                   '()
                                                   unprocessed-word-stx-lst
                                                   unprocessed-word-lst
                                                   head-pos-lst)]
                                       [word-lst (map second res)]
                                       [word-stx-lst (map first res)])
                                  (unless (empty? res)
                                    (let ([failure-str (create-failure-str machine-reject M-stx word-lst)])
                                      (display-exception failure-str (exn:fail:check-failed
                                                                        failure-str
                                                                        (current-continuation-marks)
                                                                        (map syntax-srcloc word-stx-lst))))))
                                (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
                                  (display-exception failure-str (exn:fail:check-failed
                                                                    failure-str
                                                                    (current-continuation-marks)
                                                                    (map syntax-srcloc invalid-word-stx))))))
                          (let ([failure-str (create-failure-str machine-no-left-hand-marker M-stx no-left-hand-marker-vals)])
                            (display-exception failure-str (exn:fail:check-failed
                                                              failure-str
                                                              (current-continuation-marks)
                                                              (map syntax-srcloc no-left-hand-marker-stx))))))
                    (let ([failure-str (create-failure-str machine-invalid-head-pos-index
                                                           M-stx
                                                           (map-to-val invalid-head-pos-index-stx
                                                                       head-pos-stx-lst
                                                                       unprocessed-word-lst))])
                      (display-exception failure-str (exn:fail:check-failed
                                                        failure-str
                                                        (current-continuation-marks)
                                                        (map syntax-srcloc invalid-head-pos-index-stx))))))
              (let ([failure-str (create-failure-str machine-invalid-head-pos M-stx (map-to-val
                                                                                     non-natural-head-pos-stx
                                                                                     head-pos-stx-lst
                                                                                     unprocessed-word-lst))])
                (display-exception failure-str (exn:fail:check-failed
                                                  failure-str
                                                  (current-continuation-marks)
                                                  (map syntax-srcloc non-natural-head-pos-stx))))))
        (let ([failure-str (create-failure-str machine-invalid-arity M-stx (list (first wrong-arity-words)))])
          (display-exception failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (list ( syntax-srcloc (first wrong-arity-word-stx)))))))))

(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M tests tests-stx)
     #`(let* ([results (foldr (lambda (val accum)
                                (let ([res (syntax-parse val
                                             [(~var list-two-exprs list-tm-word)
                                              (list #'list-two-exprs #'list-two-exprs.word #'list-two-exprs.head-pos)]
                                             [(~var quoted-two-exprs quoted-tm-word)
                                              (list #'quoted-two-exprs #'quoted-two-exprs.word #'quoted-two-exprs.head-pos)]
                                             [(~var quasiquoted-two-exprs quasiquoted-tm-word)
                                              (list #'quasiquoted-two-exprs #'quasiquoted-two-exprs.word #'quasiquoted-two-exprs.head-pos)]
                                             [(~var else-clause)
                                              '()])])
                                  (if (empty? res)
                                      accum
                                      (cons res accum))))
                              '()
                              tests-stx)]
              [two-exprs-stx-lst (map first results)]
              [two-exprs-word-stx-lst (map second results)]
              [two-exprs-head-pos-stx-lst (map third results)]
              [head-pos-stx-lst (map (lambda (orig-lst-val orig-lst-stx)
                                       (let ([res-idx (index-of (map syntax-srcloc two-exprs-stx-lst) (syntax-srcloc orig-lst-stx))])
                                         (if res-idx
                                             (list orig-lst-val
                                                   orig-lst-stx
                                                   (first orig-lst-val)
                                                   (list-ref two-exprs-word-stx-lst res-idx)
                                                   (second orig-lst-val)
                                                   (list-ref two-exprs-head-pos-stx-lst res-idx))
                                             (list orig-lst-val
                                                   orig-lst-stx
                                                   ;; Do this because the error reporting is going to catch this later on
                                                   (if (empty? orig-lst-val)
                                                       '()
                                                       (first orig-lst-val))
                                                   orig-lst-stx
                                                   ;; Do this because the error reporting is going to catch this later on
                                                   (if (> (length orig-lst-val) 1)
                                                       (second orig-lst-val)
                                                       '()) orig-lst-stx))))
                                     tests
                                     tests-stx)])
         (reject M
                 #'M
                 (map first head-pos-stx-lst)
                 (map second head-pos-stx-lst)
                 (map third head-pos-stx-lst)
                 (map fourth head-pos-stx-lst)
                 (map fifth head-pos-stx-lst)
                 (map sixth head-pos-stx-lst)))]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-turing-machine stx)
  (syntax-parse stx
    [(_ #t M (~var first-pairs) (~var first-pairs-stx))
     #'(check-accept M first-pairs first-pairs-stx)]
    [(_ #f M (~var first-pairs) (~var first-pairs-stx))
     #'(check-reject M first-pairs first-pairs-stx)]))

(define-syntax (check-in-lang? stx)
  (syntax-parse stx
    [(_ unknown-expr x ...)
     #'(check-turing-machine #t unknown-expr (list x ...) (list #'x ...))]))


(define equal-a-b-c (make-unchecked-tm '(S X A B C D E F G H I P Y)
                             '(a b c x)
                             `(((S ,BLANK) (X ,RIGHT))
                               ((X x) (X ,RIGHT))
                               ((X a) (A x))
                               ((X b) (B x))
                               ((X c) (C x))
                               ((X ,BLANK) (Y ,BLANK))
                               ((A x) (A ,RIGHT))
                               ((A b) (D x))
                               ((A c) (E x))
                               ((A a) (A ,RIGHT))
                               ((B x) (B ,RIGHT))
                               ((B a) (F x))
                               ((B c) (G x))
                               ((B b) (B ,RIGHT))
                               ((C x) (C ,RIGHT))
                               ((C a) (H x))
                               ((C b) (I x))
                               ((C c) (C ,RIGHT))
                               ((D x) (D ,RIGHT))
                               ((D c) (P x))
                               ((D a) (D ,RIGHT))
                               ((D b) (D ,RIGHT))
                               ((E a) (E ,RIGHT))
                               ((E c) (E ,RIGHT))
                               ((E x) (E ,RIGHT))
                               ((E b) (P x))
                               ((F x) (F ,RIGHT))
                               ((F c) (P x))
                               ((F a) (F ,RIGHT))
                               ((F b) (F ,RIGHT))
                               ((G x) (G ,RIGHT))
                               ((G a) (P x))
                               ((G b) (G ,RIGHT))
                               ((G c) (G ,RIGHT))
                               ((H a) (H ,RIGHT))
                               ((H c) (H ,RIGHT))
                               ((H x) (H ,RIGHT))
                               ((H b) (P x))
                               ((I x) (I ,RIGHT))
                               ((I a) (P x))
                               ((I b) (I ,RIGHT))
                               ((I c) (I ,RIGHT))
                               ((P x) (P ,LEFT))
                               ((P c) (P ,LEFT))
                               ((P b) (P ,LEFT))
                               ((P a) (P ,LEFT))
                               ((P ,BLANK) (X ,RIGHT)))
                             'S '(Y) 'Y))



;; need to check why this specifically has a syntax error
;(check-not-in-lang? equal-a-b-c `(,LM ,BLANK) 1)


#;(check-not-in-lang? equal-a-b-c `((,LM ,BLANK a) 11))
#;(check-not-in-lang? equal-a-b-c
              `((,LM ,BLANK a) 11)
              `((,LM ,BLANK a) 11))

;(check-in-lang? equal-a-b-c `((,LM ,BLANK a) -1))
#;(check-in-lang? equal-a-b-c
              `((,LM ,BLANK a b) 1)
              #;`((,LM ,BLANK a) 11))


(define-syntax (check-inv-holds? stx)
  (syntax-parse stx
    [(_ inv words ...)
     #'(let ([word-vals (list words ...)]
             [word-stxs (list #'words ...)])
         (check-syntax (list (fsm-error (macro-contract inv) #'inv 'inv-holds)
                             word-vals
                             word-stxs)))]))

(define-syntax (check-inv-fails? stx)
  (syntax-parse stx
    [(_ inv words ...)
     #'(let ([word-vals (list words ...)]
             [word-stxs (list #'words ...)])
         (check-syntax (list (fsm-error (macro-contract (lambda (x) (not (inv x)))) #'inv 'inv-fails)
                             word-vals
                             word-stxs)))]))

;; L = a(ab)*aa+
(define (A-INV ci)
  (and (eq? (first ci) 'a)
       (even? (length (rest ci)))
       (andmap (λ (p) (equal? p '(a b)))
               (foldl (λ (s acc)
                        (if (or (empty? acc) (= (length (first acc)) 2))
                            (cons (list s) acc)
                            (cons (list (first (first acc)) s) (rest acc))))
                      '()
                      (rest ci)))))

(check-inv-holds? A-INV '(a a a) '(a a b a b) '(a b a a))
(check-inv-fails? A-INV '(b b) '(a a b) '(b a))