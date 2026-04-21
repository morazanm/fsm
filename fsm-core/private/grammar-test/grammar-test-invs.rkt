#lang racket/base

(require "cfg-test-invs.rkt"
         "../grammar-getters.rkt"
         "rg-test-invs.rkt")

(provide grammar-test-invs)

(define (grammar-test-invs a-grammar #:cutoff-length [cutoff-length 15] . inv-pairs)
  (define g-type (grammar-type a-grammar))
  (cond [(eq? g-type 'rg) (rg-test-invs a-grammar inv-pairs)]
        [(eq? g-type 'cfg) (cfg-test-invs a-grammar cutoff-length inv-pairs)]
        [(eq? g-type 'csg) (error "Not implemented for unrestricted grammars yet")]
        [else (error "This shouldn't be possible")]))