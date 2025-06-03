#lang racket/base

(require "../../csg.rkt"
         "../../cfg.rkt"
         "../../regular-grammar.rkt"
         "../../grammar-getters.rkt"
         "../fsm-error-types/grammar-error-types.rkt"
         "../syntax-value-struct.rkt"
         "../macro-subexpr-contract.rkt"
         racket/contract/base)

(provide check-grammar)

(define (grammar-derive G word)
  (cond [(rg? G) (rg-derive G word)]
        [(cfg? G) (cfg-derive G word)]
        [(csg? G) (csg-derive G word)]
        [else (raise-user-error "Invalid grammar type. Please contact FSM devs if you see this error.")]))

(define (valid-grammar-word G)
  (listof (apply or/c (grammar-sigma G))))

(define (check-grammar accept? G unprocessed-words)
  (check-syntax (fsm-contract list?
                              unprocessed-words
                              warn:fsm:app:gmr:invalid-word
                              G)
                (fsm-contract (valid-grammar-word (val-stx-pair-val G))
                              unprocessed-words
                              warn:fsm:app:gmr:invalid-nt
                              G)
                (if accept?
                    (fsm-contract (lambda (val) (not (string? (grammar-derive (val-stx-pair-val G) val))))
                                  unprocessed-words
                                  warn:fsm:app:gmr:accept
                                  G)
                    (fsm-contract (lambda (val) (string? (grammar-derive (val-stx-pair-val G) val)))
                                  unprocessed-words
                                  warn:fsm:app:gmr:reject
                                  G))))