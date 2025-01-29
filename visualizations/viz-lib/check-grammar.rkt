#lang racket/base

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base)
         racket/syntax-srcloc
         racket/list
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "check-accept-reject-failure-strings.rkt"
         "check-exn.rkt"
         "check-utils.rkt")

(provide check-grammar)

(define (accept C G G-stx unprocessed-word-lst unprocessed-word-stx-lst)
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
                                            (map syntax-srcloc invalid-word-stx)))))))

;; stx -> stx
;; Purpose: Tests grammars
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ C:id G (~var w ) ...)
     #'(accept C G #'G (list w ...) (list #'w ...))]))

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

(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ C:id G (~var w ) ...)
     #`(reject C G #'G (list w ...) (list #'w ...))]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-grammar stx)
  #;(define-syntax-class
    valid-word
    (pattern (quote w)))
  
  #;(define-syntax-class
    invalid-word
    (pattern (~not (quote w))))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var x #;valid-word) ...)
     #'(if accept?
           (check-accept C M x ...)
           (check-reject C M x ...))]
    
    #;[(_ accept?:boolean C:id M (~or (~var valids valid-word)
                                    (~var invalids invalid-word)) ...)
     #'(raise (exn:fail:check-failed
               (create-failure-str grammar-invalid-expression M (list invalids ...))
               (current-continuation-marks)
               (map syntax-srcloc (list #'invalids ...)))
              #t)]))