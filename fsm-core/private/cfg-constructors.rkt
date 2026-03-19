#lang racket/base

(require racket/contract/region
         "cfg-struct.rkt"
         "macros/grammar-constructors.rkt")

(provide make-cfg)

;;(make-cfg V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof cfg-rule), and S is a symbol
(define/contract (make-cfg nts sigma delta state
                           #:accepts [accepts '()]
                           #:rejects [rejects '()])
  make-cfg/c
  (make-unchecked-cfg nts sigma delta state)
  )