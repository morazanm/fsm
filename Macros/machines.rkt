#lang racket
(require (for-syntax
          racket/syntax
          syntax/stx
          syntax/parse
          racket/list
          racket/string
          syntax/to-string
          racket/match))

(begin-for-syntax
  (define (is-valid-state sym)
    (define (vaild? c) (or (char-upper-case? c)
                           (char-numeric? c)
                           (eq? c #\-)))
    (and (symbol? sym)
         (empty? (foldr (lambda (v a) (and (vaild? v) a))
                        #f
                        (string->list (symbol->string sym)))))
  ))


(define-syntax (make-dfa stx)
  (define-syntax-class states
    #:description "The states that the machine can transition to"
    (pattern (state:id ...)
             #:fail-when (check-duplicate-identifier
                          (is-valid-state (syntax->list #'(state ...))))
             "Duplicate state name"))
  (syntax-parse stx
    [(_ s:states) #`(void)]))