#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "viz-state.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")

(provide check-derive check-not-derive)


(define-syntax (check-derive stx)
  (syntax-parse stx
    [(_ g w)
     #'(if (string? (grammar-derive g w))
           #f
           #t)]
    ))


(define-syntax (check-not-derive stx)
  (syntax-parse stx
    [(_ g w)
     #'(if (string? (grammar-derive g w))
           #t
           #f)]
    ))
