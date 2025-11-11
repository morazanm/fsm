#lang racket/base
(require syntax/parse)
(provide concat-expr-internal
         concat-expr
         singleton-expr
         union-expr
         empty-expr
         null-expr)
         
(define-syntax-class concat-expr-internal
  #:attributes ((vals 1))
  (pattern ((~literal concat) vals:expr ...)))

(define-syntax-class singleton-expr-internal
  #:attributes (val)
  (pattern ((~literal singleton) val:expr)))
    
(define-syntax-class singleton-expr
  #:attributes (id val)
  (pattern (id:id ((~literal singleton) val:expr))))

(define-syntax-class union-expr-internal
  #:attributes ((vals 1))
  (pattern ((~literal concat) vals:expr ...)))

(define-syntax-class empty-expr-internal
  (pattern ((~datum empty))))
  
(define-syntax-class empty-expr
  #:attributes (id)
  (pattern (id:id ((~datum empty)))))
  
(define-syntax-class null-expr-internal
  (pattern ((~datum null))))

(define-syntax-class null-expr
  #:attributes (id)
  (pattern (id:id ((~datum null)))))
  
(define-syntax-class union-expr
  #:attributes (id (empty-expr 1)
                   (singleton-expr.val 1)
                   (concat-expr.vals 2)
                   (union-expr.vals 2)
                   (null-expr 1)
                   (iden 1))
  (pattern (id:id ((~literal union) (~or* empty-expr:empty-expr-internal
                                          singleton-expr:singleton-expr-internal
                                          concat-expr:concat-expr-internal
                                          union-expr:union-expr-internal
                                          null-expr:null-expr-internal
                                          iden:id) ...))))

(define-syntax-class concat-expr
  #:attributes (id (empty-expr 1)
                   (singleton-expr.val 1)
                   (concat-expr.vals 2)
                   (union-expr.vals 2)
                   (null-expr 1)
                   (iden 1))
  (pattern (id:id ((~literal concat) (~or* empty-expr:empty-expr-internal
                                          singleton-expr:singleton-expr-internal
                                          concat-expr:concat-expr-internal
                                          union-expr:union-expr-internal
                                          null-expr:null-expr-internal
                                          iden:id) ...))))