#lang racket/base
(provide call-with-values-chained)
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (call-with-values-chained stx)
  (syntax-parse stx
    [(_ last-elem)
     #'last-elem]
    [(_ first-elem (~seq rest-lst ...))
     #'(call-with-values (lambda () (call-with-values-chained rest-lst ...)) first-elem)]))