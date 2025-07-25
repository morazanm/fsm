#lang racket/base
(require "cfg-struct.rkt" "csg.rkt" "regular-grammar.rkt")
(provide grammar-nts
         grammar-sigma
         grammar-rules
         grammar-start
         grammar-type)


(define (grammar-nts g)
  (cond [(rg? g) (rg-getnts g)]
        [(cfg? g) (cfg-get-v g)]
        [(csg? g) (csg-getv g)]))
  
(define (grammar-sigma g)
  (cond [(rg? g) (rg-getalphabet g)]
        [(cfg? g) (cfg-get-alphabet g)]
        [(csg? g) (csg-getsigma g)]))
  
(define (grammar-rules g)
  (cond [(rg? g) (rg-getunparsedrules g)]
        [(cfg? g) (cfg-get-rules g)]
        [(csg? g) (csg-get-unparsed-rules g)]))
  
(define (grammar-start g)
  (cond [(rg? g) (rg-getstart g)]
        [(cfg? g) (cfg-get-start g)]
        [(csg? g) (csg-getstart g)]))
  
(define (grammar-type g)
  (cond [(rg? g) 'rg]
        [(cfg? g) 'cfg]
        [(csg? g) 'csg]
        [else (error (format "In grammar-gettype: Unknown grammar type. Given: ~a" g))]))
