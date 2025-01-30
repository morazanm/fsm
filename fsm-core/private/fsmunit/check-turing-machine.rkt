#lang racket/base

(require
  (for-syntax syntax/parse
              racket/base
              "fsm-syntax-classes.rkt")
  racket/list
  syntax/parse
  "fsm-syntax-classes.rkt"
  racket/syntax
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

(define (accept c M M-stx orig-val-lst orig-stx-lst unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst head-pos-stx-lst)
  (let* ([wrong-arities (accumulate-invalid-words correct-tm-word-arity/c orig-val-lst orig-stx-lst)]
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
                          (let* ([invalids (accumulate-invalid-words c unprocessed-word-lst unprocessed-word-stx-lst)]
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
                                      (display-failed-test failure-str (exn:fail:check-failed
                                                                        failure-str
                                                                        (current-continuation-marks)
                                                                        (map syntax-srcloc word-stx-lst))))))
                                (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
                                  (display-failed-test failure-str (exn:fail:check-failed
                                                                    failure-str
                                                                    (current-continuation-marks)
                                                                    (map syntax-srcloc invalid-word-stx))))))
                          (let ([failure-str (create-failure-str machine-no-left-hand-marker M-stx no-left-hand-marker-vals)])
                            (display-failed-test failure-str (exn:fail:check-failed
                                                              failure-str
                                                              (current-continuation-marks)
                                                              (map syntax-srcloc no-left-hand-marker-stx))))))
                    (let ([failure-str (create-failure-str machine-invalid-head-pos-index
                                                           M-stx
                                                           (map-to-val invalid-head-pos-index-stx
                                                                       head-pos-stx-lst
                                                                       unprocessed-word-lst))])
                      (display-failed-test failure-str (exn:fail:check-failed
                                                        failure-str
                                                        (current-continuation-marks)
                                                        (map syntax-srcloc invalid-head-pos-index-stx))))))
              (let ([failure-str (create-failure-str machine-invalid-head-pos M-stx (map-to-val
                                                                                     non-natural-head-pos-stx
                                                                                     head-pos-stx-lst
                                                                                     unprocessed-word-lst))])
                (display-failed-test failure-str (exn:fail:check-failed
                                                  failure-str
                                                  (current-continuation-marks)
                                                  (map syntax-srcloc non-natural-head-pos-stx))))))
        (let ([failure-str (create-failure-str machine-invalid-arity M-stx (list (first wrong-arity-words)))])
          (display-failed-test failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (list ( syntax-srcloc (first wrong-arity-word-stx)))))))))

(define (zip . lists)
  (apply map list lists))

;; Syntax -> Syntax
;; Purpose: Checks turing machines
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ C:id M tests tests-stx)
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
         (accept C
                 M
                 #'M
                 (map first head-pos-stx-lst)
                 (map second head-pos-stx-lst)
                 (map third head-pos-stx-lst)
                 (map fourth head-pos-stx-lst)
                 (map fifth head-pos-stx-lst)
                 (map sixth head-pos-stx-lst)))]))

(define (reject c M M-stx  orig-val-lst orig-stx-lst unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst head-pos-stx-lst)
  (let* ([wrong-arities (accumulate-invalid-words correct-tm-word-arity/c orig-val-lst orig-stx-lst)]
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
                          (let* ([invalids (accumulate-invalid-words c unprocessed-word-lst unprocessed-word-stx-lst)]
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
                                      (display-failed-test failure-str (exn:fail:check-failed
                                                                        failure-str
                                                                        (current-continuation-marks)
                                                                        (map syntax-srcloc word-stx-lst))))))
                                (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
                                  (display-failed-test failure-str (exn:fail:check-failed
                                                                    failure-str
                                                                    (current-continuation-marks)
                                                                    (map syntax-srcloc invalid-word-stx))))))
                          (let ([failure-str (create-failure-str machine-no-left-hand-marker M-stx no-left-hand-marker-vals)])
                            (display-failed-test failure-str (exn:fail:check-failed
                                                              failure-str
                                                              (current-continuation-marks)
                                                              (map syntax-srcloc no-left-hand-marker-stx))))))
                    (let ([failure-str (create-failure-str machine-invalid-head-pos-index
                                                           M-stx
                                                           (map-to-val invalid-head-pos-index-stx
                                                                       head-pos-stx-lst
                                                                       unprocessed-word-lst))])
                      (display-failed-test failure-str (exn:fail:check-failed
                                                        failure-str
                                                        (current-continuation-marks)
                                                        (map syntax-srcloc invalid-head-pos-index-stx))))))
              (let ([failure-str (create-failure-str machine-invalid-head-pos M-stx (map-to-val
                                                                                     non-natural-head-pos-stx
                                                                                     head-pos-stx-lst
                                                                                     unprocessed-word-lst))])
                (display-failed-test failure-str (exn:fail:check-failed
                                                  failure-str
                                                  (current-continuation-marks)
                                                  (map syntax-srcloc non-natural-head-pos-stx))))))
        (let ([failure-str (create-failure-str machine-invalid-arity M-stx (list (first wrong-arity-words)))])
          (display-failed-test failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (list ( syntax-srcloc (first wrong-arity-word-stx)))))))))

(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ C:id M tests tests-stx)
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
         (reject C
                 M
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
    [(_ #t C:id M (~var first-pairs) (~var first-pairs-stx))
     #'(check-accept C M first-pairs first-pairs-stx)]
    [(_ #f C:id M (~var first-pairs) (~var first-pairs-stx))
     #'(check-reject C M first-pairs first-pairs-stx)]))
