#lang racket/base

(require "../fsm-error-types/invariant-error-types.rkt"
         "../macro-subexpr-contract.rkt")

(provide check-invariant)

(define (valid-predicate? proc)
  (bitwise-bit-set? (procedure-arity-mask proc) 1))

(define (check-invariant accept? inv words)
  (check-syntax (fsm-contract valid-predicate?
                              (list inv)
                              warn:fsm:app:inv:invalid-pred
                              inv)
                (fsm-contract list?
                              words
                              warn:fsm:app:inv:invalid-words
                              inv)
                (if accept?
                    (fsm-contract inv
                                  words
                                  warn:fsm:app:inv:accept
                                  inv)
                    (fsm-contract (compose1 not inv)
                                  words
                                  warn:fsm:app:inv:reject
                                  inv))))