#lang racket/base

(require "../fsm-error-types/fsa-error-types.rkt"
         "../../sm-apply.rkt"
         "../../sm-getters.rkt"
         "../macro-subexpr-contract.rkt"
         "../syntax-value-struct.rkt"
         racket/contract/base)

(provide check-machine)

(define (valid-machine-word M)
  (listof (apply or/c (sm-sigma M))))

(define (check-machine accept? M unprocessed-words)
  (check-syntax (fsm-contract list?
                              unprocessed-words
                              warn:fsm:app:sm:invalid-word
                              M)
                (fsm-contract (valid-machine-word (val-stx-pair-val M))
                              unprocessed-words
                              warn:fsm:app:sm:invalid-nt
                              M)
                (if accept?
                    (fsm-contract (lambda (word) (equal? (sm-apply (val-stx-pair-val M) word) 'accept))
                                  unprocessed-words
                                  warn:fsm:app:sm:accept
                                  M)
                    (fsm-contract (lambda (word) (equal? (sm-apply (val-stx-pair-val M) word) 'reject))
                                  unprocessed-words
                                  warn:fsm:app:sm:reject
                                  M))))