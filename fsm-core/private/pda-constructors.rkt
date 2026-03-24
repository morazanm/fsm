#lang racket/base
(require "pda.rkt"
         "macros/constructors.rkt"
         racket/contract/region)

(provide make-ndpda)

;; Purpose: Constructs an ndpda given a set of states, a machine alphabet,
;; set of stack symbols, a start state, a list of final states, and a list
;; of ndpda rules. The function checks that all fields are valid before
;; constructing the ndpda.
(define/contract (make-ndpda states sigma gamma start finals rules
                             #:accepts [accepts '()]
                             #:rejects [rejects '()])
  make-ndpda/c
  (make-unchecked-ndpda states sigma gamma start finals rules)
  )