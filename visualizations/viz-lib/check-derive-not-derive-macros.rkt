#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/misc.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "viz-state.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")

(provide check-derive check-not-derive)


;; grammar word -> Boolean
;; Purpose: To check if the given grammar can derive a given word
(define-syntax (check-derive stx)
  (syntax-parse stx
    [(_ g w)
     #'(check-equal? (last (grammar-derive g w))
                     (los->symbol w))]
    ))

;; grammar word -> Boolean
;; Purpose: To check if the given grammar cannot derive a given word
(define-syntax (check-not-derive stx)
  (syntax-parse stx
    [(_ g w)
     #'(check-equal? (string? (grammar-derive g w)) #t)]
    ))
