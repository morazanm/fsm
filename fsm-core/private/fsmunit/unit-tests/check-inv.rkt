#lang racket/base

(require "../fsm-error-types/invariant-error-types.rkt"
         "../macro-subexpr-contract.rkt")

(provide check-invariant)

(define (valid-predicate? proc)
  (bitwise-bit-set? (procedure-arity-mask proc) 1))

(define (check-invariant accept? inv words)
  (check-syntax (property-check valid-predicate?
                                (list inv)
                                warn:fsm:app:inv:invalid-pred
                                inv)
                (property-check list?
                                words
                                warn:fsm:app:inv:invalid-words
                                inv)
                (if accept?
                    (property-check inv
                                    words
                                    warn:fsm:app:inv:accept
                                    inv)
                    (property-check (compose1 not inv)
                                    words
                                    warn:fsm:app:inv:reject
                                    inv))))