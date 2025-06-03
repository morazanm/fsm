#lang racket/base

(require "fsm-error-types/base-fsm-error-types.rkt"
         "syntax-value-struct.rkt")

(provide check-syntax
         (struct-out fsm-contract))

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

(struct fsm-contract (c testables err fsm-expr))

(define (check-syntax . fsm-contracts)
  (for ([a-contract (in-list fsm-contracts)]
        #:do [(define err-vals (if (list? (fsm-contract-c a-contract))
                                   (accumulate-invalid-words-dep (fsm-contract-c a-contract) (fsm-contract-testables a-contract))
                                   (accumulate-invalid-words (fsm-contract-c a-contract) (fsm-contract-testables a-contract))))]
        #:final (not (null? err-vals)))
    (when (not (null? err-vals))
      (display-fsm-err ((fsm-contract-err a-contract) (fsm-contract-fsm-expr a-contract) err-vals)))))