#lang racket/base

(require "../fsm-error-types/regexp-error-types.rkt"
         "../macro-subexpr-contract.rkt"
         "../../fsa.rkt"
         "../../sm-apply.rkt")

(provide check-regexp)

(define (check-regexp accept? regex words)
  (define regexp-machine (regexp->fsa regex))
  (check-syntax (property-check list?
                                words
                                warn:fsm:app:regexp:invalid-words
                                regex)
                (if accept?
                    (property-check (lambda (word) (eq? 'accept (sm-apply regexp-machine word)))
                                    words
                                    warn:fsm:app:regexp:accept
                                    regex)
                    (property-check (lambda (word) (eq? 'reject (sm-apply regexp-machine word)))
                                    words
                                    warn:fsm:app:regexp:reject
                                    regex))))