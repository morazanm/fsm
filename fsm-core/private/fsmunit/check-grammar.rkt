#lang racket/base

(require (for-syntax syntax/parse
                     
                     racket/base)
         racket/syntax-srcloc
         racket/list
         "../csg.rkt"
         "../cfg.rkt"
         "../regular-grammar.rkt"
         "check-accept-reject-failure-strings.rkt"
         "check-exn.rkt"
         "check-utils.rkt")

(provide check-grammar)

(define (accept C G G-stx unprocessed-word-lst unprocessed-word-stx-lst)
  (let* ([not-a-words (accumulate-invalid-words list? unprocessed-word-lst unprocessed-word-stx-lst)]
         [not-a-words-lst (map first not-a-words)]
         [not-a-words-lst-stx (map second not-a-words)])
    (if (empty? not-a-words)
        (let* ([invalids (accumulate-invalid-words C unprocessed-word-lst unprocessed-word-stx-lst)]
               [invalid-words (map first invalids)]
               [invalid-word-stx (map second invalids)])
          (if (empty? invalids)
              (let* ([res (foldr (lambda (word word-stx accum)
                                   (if (not (string? (cond [(rg? G) (rg-derive G word)]
                                                           [(cfg? G) (cfg-derive G word)]
                                                           [(csg? G) (csg-derive G word)]
                                                           [else (raise (exn:fail:check-failed
                                                                         "Unknown grammar type"
                                                                         (current-continuation-marks)
                                                                         (list (syntax-srcloc #'G)))
                                                                        #t)])))
                                       accum
                                       (cons (list word word-stx) accum)))
                                 '()
                                 unprocessed-word-lst
                                 unprocessed-word-stx-lst)]
                     [word-lst (map first res)]
                     [word-stx-lst (map second res)])
                (unless (empty? res)
                  (let ([failure-str (create-failure-str grammar-accept G-stx word-lst)])
                    (display-failed-test failure-str (exn:fail:check-failed
                                                      failure-str
                                                      (current-continuation-marks)
                                                      (map syntax-srcloc word-stx-lst))
                                         ))))
              (let ([failure-str (create-failure-str grammar-invalid-nonterminal G-stx invalid-words)])
                (display-failed-test failure-str (exn:fail:check-failed
                                                  failure-str
                                                  (current-continuation-marks)
                                                  (map syntax-srcloc invalid-word-stx))))))
        (let ([failure-str (create-failure-str grammar-invalid-expression G-stx not-a-words)])
          (display-failed-test failure-str (exn:fail:check-failed failure-str
                                                                  (current-continuation-marks)
                                                                  (map syntax-srcloc not-a-words-lst-stx)))))
    )
    )

(define (reject C G G-stx unprocessed-word-lst unprocessed-word-stx-lst)
  (let* ([invalids (accumulate-invalid-words C unprocessed-word-lst unprocessed-word-stx-lst)]
         [invalid-words (map first invalids)]
         [invalid-word-stx (map second invalids)])
    (if (empty? invalids)
        (let* ([res (foldr (lambda (word word-stx accum)
                             (if (string? (cond [(rg? G) (rg-derive G word)]
                                                [(cfg? G) (cfg-derive G word)]
                                                [(csg? G) (csg-derive G word)]
                                                [else (raise (exn:fail:check-failed
                                                              "Unknown grammar type"
                                                              (current-continuation-marks)
                                                              (list (syntax-srcloc #'G)))
                                                             #t)]))
                                 accum
                                 (cons (list word word-stx) accum)))
                           '()
                           unprocessed-word-lst
                           unprocessed-word-stx-lst)]
               [word-lst (map first res)]
               [word-stx-lst (map second res)])
          (unless (empty? res)
            (let ([failure-str (create-failure-str grammar-reject G-stx word-lst)])
              (display-failed-test failure-str (exn:fail:check-failed
                                                failure-str
                                                (current-continuation-marks)
                                                (map syntax-srcloc word-stx-lst))
                                   ))))
        (let ([failure-str (create-failure-str grammar-invalid-nonterminal G-stx invalid-words)])
          (display-failed-test failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (map syntax-srcloc invalid-word-stx)))))))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-grammar stx)
  (syntax-parse stx
    [(_ #t C:id M (~var x) ...)
     #'(accept C M #'M (list x ...) (list #'x ...))]
    [(_ #f C:id M (~var x) ...)
     #'(reject C M #'M (list x ...) (list #'x ...))]))
