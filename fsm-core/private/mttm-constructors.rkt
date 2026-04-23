#lang racket/base
(require "mtape-tm.rkt"
         "macros/constructors.rkt"
         racket/contract/region)
(provide make-mttm)

(define/contract (make-mttm states sigma start finals rules num-tapes
                            [accept 'null]
                            #:accepts [accepts '()]
                            #:rejects [rejects '()])
  make-mttm/c
  (if (equal? accept 'null)
      (make-unchecked-mttm states sigma start finals rules num-tapes)
      (make-unchecked-mttm states sigma start finals rules num-tapes accept))
  )