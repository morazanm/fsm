#lang racket
(require (for-syntax racket/syntax syntax/parse syntax/stx))
(provide keyword-builder)



(define-syntax (keyword-builder stx)
  (define-syntax-class distinct-fields
    #:description "sequence of distinct field names"
    (pattern (normal-id:id ... [with-default-id:id default-val:expr] ...)
             #:fail-when (or (check-duplicate-identifier (syntax->list #'(normal-id ...)))
                             (check-duplicate-identifier (syntax->list #`(with-default-id ...))))            
             "duplicate field name"))
  (syntax-parse stx
    [(_ struct-name:id fields:distinct-fields)
     #:with (default-keyword ...)
     (stx-map
      (compose string->keyword symbol->string syntax->datum)
      #`(fields.with-default-id ...))
     #:with make-id (format-id #`struct-name "make-~a" #`struct-name)
     #`(begin
         (struct struct-name [fields.normal-id ... fields.with-default-id ...] #:transparent)
         (define (make-id fields.normal-id ...
                          (~@ default-keyword [fields.with-default-id fields.default-val]) ...)
           (struct-name fields.normal-id ... fields.with-default-id ...)))]))