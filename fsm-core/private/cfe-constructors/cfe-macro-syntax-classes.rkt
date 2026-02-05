#lang racket/base
(require syntax/parse)
(provide concat-expr-internal
         concat-expr
         singleton-expr
         singleton-expr-internal
         union-expr
         union-expr-internal
         empty-expr
         empty-expr-internal
         null-expr
         null-expr-internal)
         
(define-syntax-class concat-expr-internal
  #:attributes ((vals 1))
  #:description "concat cfexp"
  (pattern ((~literal concat) vals:expr ...)))

(define-syntax-class singleton-expr-internal
  #:attributes (val)
  #:description "singleton cfexp"
  (pattern ((~literal singleton) val:expr)))
    
(define-syntax-class singleton-expr
  #:attributes (id val)
  #:description "singleton cfexp"
  (pattern (id:id ((~literal singleton) val:expr))))

(define-syntax-class union-expr-internal
  #:attributes ((vals 1))
  #:description "union cfexp"
  (pattern ((~literal union) vals:expr ...)))

(define-syntax-class empty-expr-internal
  #:description "empty cfexp"
  (pattern ((~datum empty))))
  
(define-syntax-class empty-expr
  #:attributes (id)
  #:description "empty cfexp"
  (pattern (id:id ((~datum empty)))))
  
(define-syntax-class null-expr-internal
  #:description "null cfexp"
  (pattern ((~datum null))))

(define-syntax-class null-expr
  #:attributes (id)
  #:description "null cfexp"
  (pattern (id:id ((~datum null)))))
  
(define-syntax-class union-expr
   #:attributes (id (vals 1))
   #:description "union cfexp"
  (pattern (id:id ((~literal union) vals ...))))

(define-syntax-class concat-expr
  #:attributes (id (vals 1))
  #:description "concat cfexp"
  (pattern (id:id ((~literal concat) vals ...))))