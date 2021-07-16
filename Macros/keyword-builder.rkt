#lang racket
(require (for-syntax racket/syntax syntax/parse syntax/stx))
#|  Keyword-builder pattern Macro. Written by Joshua Schappel 5/13/21
See Macro readme for exact transformation and basic usage.
|#
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
     #:with make-id (format-id #`struct-name "make-~a" #`struct-name) ;; function name
     #`(begin
         (struct struct-name [fields.normal-id ... fields.with-default-id ...] #:transparent) ;; create the struct
         (define (make-id fields.normal-id ... ;; create functiona and the keyword args for the function
                          (~@ default-keyword [fields.with-default-id fields.default-val]) ...)
           (struct-name fields.normal-id ... fields.with-default-id ...)))]))