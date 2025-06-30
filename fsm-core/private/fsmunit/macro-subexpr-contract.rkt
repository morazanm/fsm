#lang racket/base

(require "fsm-error-types/base-fsm-error-types.rkt"
         "syntax-value-struct.rkt")

(provide check-syntax
         (struct-out property-check))

(define (accumulate-invalid-words contract word-lst)
  (filter (lambda (w) (not (contract (val-stx-pair-val w)))) word-lst))

(define (accumulate-invalid-words-dep contracts word-lst)
  (foldr (lambda (w contract accum)
           (if (contract (val-stx-pair-val w))
               accum
               (cons w accum)))
         '()
         word-lst
         contracts))

(struct property-check (contract testables err-type-constructor fsm-expr))

(define (check-syntax . fsm-contracts)
  (for ([a-contract (in-list fsm-contracts)]
        #:do [(define err-vals (if (list? (property-check-contract a-contract))
                                   (accumulate-invalid-words-dep (property-check-contract a-contract) (property-check-testables a-contract))
                                   (accumulate-invalid-words (property-check-contract a-contract) (property-check-testables a-contract))))]
        #:final (not (null? err-vals)))
    (when (not (null? err-vals))
      (display-fsm-err ((property-check-err-type-constructor a-contract) (property-check-fsm-expr a-contract) err-vals)))))