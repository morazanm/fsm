#lang racket/base

(require
  (for-syntax syntax/parse
              racket/base)
  racket/list
  syntax/parse
  "fsm-syntax-classes.rkt"
  racket/syntax-srcloc
  "../sm-apply.rkt"
  "check-accept-reject-failure-strings.rkt"
  "check-exn.rkt"
  "check-utils.rkt")

(provide check-turing-machine)

;; (Listof Syntax) (Listof Syntax) (Listof Any) -> (Listof Any)
;; Used to map values to another value associated with them
;; for the sake of creating more useful error string
(define (map-to-val invalid-exprs all-exprs map-to-vals)
  (let ([idxs (map (lambda (x) (index-of all-exprs x)) invalid-exprs)])
    (map (lambda (x) (list-ref map-to-vals x)) idxs)))

;; (Listof Any) -> (Listof flat-contract)
;; Used to create flat contracts for invalid head-position indices
(define (build-contract lst-deps)
  (map (lambda (x) (lambda (y) (> (length x) y))) lst-deps))


;; tm tm-syntax (Listof word-expression) (Listof word-expression-syntax) (Listof head-position-expression)
;; Checks if the turing machine accepts the words inside of the test cases
(define (accept M M-stx unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst)
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
                                          (map syntax-srcloc word-stx-lst)))))))

;; tm tm-syntax (Listof word-expression) (Listof word-expression-syntax) (Listof head-position-expression)
;; Checks if the turing machine rejects the words inside of the test cases
(define (reject M M-stx unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst)
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
                                          (map syntax-srcloc word-stx-lst)))))))

;; boolean flat-contract tm syntax-of-tm (Listof original-expression) (Listof original-expression-syntax) (Listof word-expression) (Listof word-expression-syntax) (Listof head-position-expression) (Listof head-position-expression-syntax)
;; Checks validity of all the arguments given to the original check expression, runs the actual check itself
(define (check-validity-of-arguments accept? c M M-stx orig-val-lst orig-stx-lst unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst head-pos-stx-lst)
  (let* ([wrong-arities (accumulate-invalid-words correct-tm-word-arity/c orig-val-lst orig-stx-lst)]
         [wrong-arity-words (map first wrong-arities)]
         [wrong-arity-word-stx (map second wrong-arities)])
    (if (empty? wrong-arities)
        (let* ([invalid-word (accumulate-invalid-words list? unprocessed-word-lst unprocessed-word-stx-lst)]
               [invalid-word-lst (map first invalid-word)]
               [invalid-word-stx (map second invalid-word)]
               )
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
                                (let* ([invalids (accumulate-invalid-words c unprocessed-word-lst unprocessed-word-stx-lst)]
                                       [invalid-words (map first invalids)]
                                       [invalid-word-stx (map second invalids)])
                                  (if (empty? invalids)
                                      (if accept?
                                          (accept M M-stx unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst)
                                          (reject M M-stx unprocessed-word-lst unprocessed-word-stx-lst head-pos-lst))
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
              (let ([failure-str (create-failure-str machine-invalid-word M-stx invalid-word-lst)])
                (display-failed-test failure-str (exn:fail:check-failed
                                                  failure-str
                                                  (current-continuation-marks)
                                                  (map syntax-srcloc unprocessed-word-stx-lst))))))
        (let ([failure-str (create-failure-str machine-invalid-arity M-stx (list (first wrong-arity-words)))])
          (display-failed-test failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (list (syntax-srcloc (first wrong-arity-word-stx)))))))))

;; flat-contract tm tm-syntax test-cases test-cases-syntax
;; Parses the test cases' syntax into their word and head-positions
(define (parse-test-cases accept? C M M-stx tests tests-stx)
  (let* ([results (foldr (lambda (val accum)
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
    (check-validity-of-arguments C
                                 M
                                 M-stx
                                 (map first head-pos-stx-lst)
                                 (map second head-pos-stx-lst)
                                 (map third head-pos-stx-lst)
                                 (map fourth head-pos-stx-lst)
                                 (map fifth head-pos-stx-lst)
                                 (map sixth head-pos-stx-lst))))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-turing-machine stx)
  (syntax-parse stx
    [(_ accept? C:id M (~var first-pairs) (~var first-pairs-stx))
     #'(parse-test-cases accept? C M #'M first-pairs first-pairs-stx)]))
