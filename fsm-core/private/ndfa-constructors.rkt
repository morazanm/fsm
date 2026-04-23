#lang racket/base
(require "macros/constructors.rkt"
         "fsa.rkt"
         racket/contract/region)
(provide make-ndfa make-dfa)

(define/contract (make-ndfa states sigma start finals rules
                            #:accepts [accepts '()]
                            #:rejects [rejects '()])
  make-ndfa/c
  (make-unchecked-ndfa states sigma start finals rules)
  )

;; make-dfa: states alphabet state states rules (boolean) -> machine
;; Purpose: Eventually, will construct a multi-tape turing-machine from the given
;; DFA inputs, but for now just parses inputs and constructs an unchecked-dfa.
(define/contract (make-dfa states sigma start finals rules
                           [add-dead '()]
                           #:accepts [accepts '()]
                           #:rejects [rejects '()])
  make-dfa/c
  (if (null? add-dead)
      (make-unchecked-dfa states sigma start finals rules)
      (make-unchecked-dfa states sigma start finals rules add-dead)
      )
  )