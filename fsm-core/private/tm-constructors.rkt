#lang racket/base

(require "macros/constructors.rkt"
         "tm.rkt"
         racket/contract/region)
(provide make-tm)
   
(define/contract (make-tm states sigma rules start finals
                          [accept 'null]
                          #:accepts [accepts '()]
                          #:rejects [rejects '()]
                          )
  make-tm/c
  (if (equal? accept 'null)
      (make-unchecked-tm states sigma rules start finals)
      (make-unchecked-tm states sigma rules start finals accept))
  )