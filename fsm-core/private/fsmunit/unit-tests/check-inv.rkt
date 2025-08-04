#lang racket/base

(require "../fsm-error-types/invariant-error-types.rkt"
         "../macro-subexpr-contract.rkt"
         "../syntax-value-struct.rkt")

(provide check-invariant)

(define (valid-predicate? proc)
  (define bit-mask (procedure-arity-mask proc))
  (or (bitwise-bit-set? bit-mask 1)
      (bitwise-bit-set? bit-mask 2)))

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
                    (property-check (if (and (not (null? words))
                                             (not (null? (car words)))
                                             (list? (car (val-stx-pair-val (car words)))))
                                        (map (lambda (word-pair)
                                               (lambda (x) ((val-stx-pair-val inv) x (cadr (val-stx-pair-val word-pair)))))
                                             words)
                                        (val-stx-pair-val inv))
                                    words 
                                    warn:fsm:app:inv:accept
                                    inv)
                    (property-check (if (and (not (null? words))
                                             (not (null? (car words)))
                                             (list? (car (val-stx-pair-val (car words)))))
                                        (map (lambda (word-pair)
                                               (lambda (x) ((compose1 not (val-stx-pair-val inv)) x (cadr (val-stx-pair-val word-pair)))))
                                             words)
                                        (compose1 not (val-stx-pair-val inv)))
                                    words
                                    warn:fsm:app:inv:reject
                                    inv))))