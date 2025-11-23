#lang racket/base

(require "../fsm-error-types/cfexp-error-types.rkt"
         "../macro-subexpr-contract.rkt"
         "../../chomsky.rkt"
         "../../cyk.rkt"
         "../../cfe-constructors/context-free-expressions-constructors.rkt"
         "../syntax-value-struct.rkt")

(provide check-cfexp)

(define (check-cfexp accept? cfe words)
  (define converted-cfg (chomsky (cfe->cfg (val-stx-pair-val cfe))))
  
  (check-syntax #;(property-check list?
                                words
                                warn:fsm:app:regexp:invalid-words
                                cfe)
                (if accept?
                    (property-check (lambda (word) (cyk converted-cfg word))
                                    words
                                    warn:fsm:app:cfexp:accept
                                    cfe)
                    (property-check (lambda (word) (not (cyk converted-cfg word)))
                                    words
                                    warn:fsm:app:cfexp:reject
                                    cfe))))