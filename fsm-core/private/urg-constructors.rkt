#lang racket/base

(require "csg.rkt"
         racket/contract/region
         "macros/grammar-constructors.rkt")

(provide make-csg make-grammar)

;;make-csg V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof csg-rule), and S is a symbol
(define/contract (make-csg nts sigma delta state
                           #:accepts [accepts '()]
                           #:rejects [rejects '()])
  make-csg/c
  (begin
    (displayln "Warning: This constructor is deprecated. Use make-grammar.")
    (make-unchecked-csg nts sigma delta state)))

;;make-grammar V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof csg-rule), and S is a symbol
;; This is just a renaming for make-csg.
(define/contract (make-grammar nts sigma delta state
                               #:accepts [accepts '()]
                               #:rejects [rejects '()])
  make-csg/c
  (make-unchecked-csg nts sigma delta state))