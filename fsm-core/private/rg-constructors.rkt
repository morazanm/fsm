#lang racket/base

(require racket/contract/region
         "regular-grammar.rkt"
         "macros/grammar-constructors.rkt")
(provide make-rg)
;;(make-rg N A R S), such that
;; N is a (listof symbol) (the non-terminals), A is a (listof symbol) (the
;; alphabet), R is a (listof rrule), and S is a symbol (starting symbol)
(define/contract (make-rg nts sigma delta state
                          #:accepts [accepts '()]
                          #:rejects [rejects '()])
  make-rg/c
  (make-unchecked-rg nts sigma delta state)
  )