#lang racket/base

(require "context-free-expressions-constructors.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/match
                     racket/syntax-srcloc
                     racket/syntax))

(provide make-cfe)



(begin-for-syntax
  (define-syntax-class concat-cfexp-expr
    #:attributes ((vals 1))
    #:description "concat cfexp"
    (pattern ((~datum concat-cfexp) vals:expr ...)))

  (define-syntax-class concat-cfexp-binding
    #:attributes (id (expr.vals 1))
    #:description "concat cfexp"
    (pattern (id:id expr:concat-cfexp-expr)))

  (define-syntax-class singleton-cfexp-expr
    #:attributes (val)
    #:description "singleton cfexp"
    (pattern ((~datum singleton-cfexp) val:expr)))
    
  (define-syntax-class singleton-cfexp-binding
    #:attributes (id expr.val)
    #:description "singleton cfexp"
    (pattern (id:id expr:singleton-cfexp-expr)))

  (define-syntax-class union-cfexp-expr
    #:attributes ((vals 1))
    #:description "union cfexp"
    (pattern ((~datum union-cfexp) vals:expr ...)))
  
  (define-syntax-class union-cfexp-binding
    #:attributes (id (expr.vals 1))
    #:description "union cfexp"
    (pattern (id:id expr:union-cfexp-expr)))

  (define-syntax-class kleene-cfexp-expr
    #:attributes (val)
    #:description "kleene cfexp"
    (pattern ((~datum kleenestar-cfexp) val:expr)))
  
  (define-syntax-class kleene-cfexp-binding
    #:attributes (id expr.val)
    #:description "kleene cfexp"
    (pattern (id:id expr:kleene-cfexp-expr)))

  (define-syntax-class empty-cfexp-expr
    #:description "empty cfexp"
    (pattern ((~datum empty-cfexp))))
  
  (define-syntax-class empty-cfexp-binding
    #:attributes (id)
    #:description "empty cfexp"
    (pattern (id:id expr:empty-cfexp-expr)))

  (define-syntax-class null-cfexp-expr
    #:description "null cfexp"
    (pattern ((~datum null-cfexp))))

  (define-syntax-class null-cfexp-binding
    #:attributes (id)
    #:description "null cfexp"
    (pattern (id:id expr:null-cfexp-expr)))
  
  (struct exn:fail:bad-circular-ref exn:fail
    (a-srcloc)
    #:property prop:exn:srclocs
    (lambda (a-struct)
      (match a-struct
        [(exn:fail:bad-circular-ref msg marks (list a-srcloc ...))
         a-srcloc])))

  (define circular-binding-message "Bad circular reference with binding identifier in")
  
  (define (check-for-bad-circular-ref id-stx-obj val-stx-obj cfexp-type)
    (when (and (identifier? val-stx-obj)
               (free-identifier=? id-stx-obj val-stx-obj))
      (raise (exn:fail:bad-circular-ref (if cfexp-type
                                            (format "~a: ~a: ~a: ~a"
                                                    (srcloc->string (syntax-srcloc val-stx-obj))
                                                    cfexp-type
                                                    circular-binding-message
                                                    (syntax->datum val-stx-obj))
                                            (format "~s: ~a: ~a"
                                                    (srcloc->string (syntax-srcloc val-stx-obj))
                                                    circular-binding-message
                                                    (syntax->datum val-stx-obj)))
                                        (current-continuation-marks)
                                        (list (syntax-srcloc id-stx-obj) (syntax-srcloc val-stx-obj)))))))

(define-syntax (process-cfe-syntax stx)
  (syntax-parse stx
    [(_ a-null-expr:null-cfexp-expr)
     #'(null-cfexp)]
    
    [(_ a-empty-expr:empty-cfexp-expr)
     #'(empty-cfexp)]
    
    [(_ a-singleton-expr:singleton-cfexp-expr (~optional iden #:defaults ([iden #'#f])))
     (when (syntax-e #'iden)
       (check-for-bad-circular-ref #'iden #'a-singleton-expr.val 'singleton-cfexp))
     (syntax/loc #'a-singleton-expr
       (singleton-cfexp a-singleton-expr.val))]

    [(_ a-kleene-expr:kleene-cfexp-expr (~optional iden #:defaults ([iden #'#f])))
     (when (syntax-e #'iden)
       (check-for-bad-circular-ref #'iden #'a-kleene-expr.val 'kleene-cfexp))
     (syntax/loc #'a-kleene-expr
       (kleene-cfexp a-kleene-expr.val))]
    
    [(_ a-concat-expr:concat-cfexp-expr (~optional iden #:defaults ([iden #'#f])))
     (when (syntax-e #'iden)
       (for ([val-stx (in-list (syntax-e #'(a-concat-expr.vals ...)))])
         (check-for-bad-circular-ref #'iden val-stx 'concat-cfexp)))
     (syntax/loc #'a-concat-expr
       (concat-cfexp (process-cfe-syntax a-concat-expr.vals iden)
                     ...))]
    
    [(_ a-union-expr:union-cfexp-expr (~optional iden #:defaults ([iden #'#f])))
     (when (syntax-e #'iden)
       (for ([val-stx (in-list (syntax-e #'(a-union-expr.vals ...)))])
         (check-for-bad-circular-ref #'iden val-stx 'union-cfexp)))
     (syntax/loc #'a-union-expr
       (union-cfexp (process-cfe-syntax a-union-expr.vals iden)
                    ...))]
    
    [(_ iden-expr:id (~optional iden #:defaults ([iden #'#f])))
     (when (syntax-e #'iden)
       (check-for-bad-circular-ref #'iden #'iden-expr #f))
     #`(if (procedure? iden-expr)
           #,(syntax/loc #'iden-expr
               (iden-expr))
           iden-expr)]
    
    [(_ express:expr)
     (define temp-id (generate-temporary))
     #`(let ([#,temp-id #,(syntax/loc #'express (express))])
         (if (procedure? #,temp-id)
             #,(quasisyntax/loc #'express
                 (#,temp-id))
             #,temp-id))]))

(define-syntax (define-singleton-cfe stx)
  (syntax-parse stx
    [(_ a-singleton-clause:singleton-cfexp-binding)
     (check-for-bad-circular-ref #'a-singleton-clause.id #'a-singleton-clause.expr.val 'singleton-cfexp)
     #`(define a-singleton-clause.id #,(syntax/loc #'a-singleton-clause
                                         (singleton-cfexp a-singleton-clause.expr.val)))]))

(define-syntax (define-concat-cfe stx)
  (syntax-parse stx
    [(_ a-concat-clause:concat-cfexp-binding)
     #`(define a-concat-clause.id
         (lambda ()
           (set! a-concat-clause.id
                 #,(syntax/loc #'a-concat-clause
                     (concat-cfexp (process-cfe-syntax a-concat-clause.expr.vals a-concat-clause.id)
                                   ...)))))]))

(define-syntax (define-kleene-cfe stx)
  (syntax-parse stx
    [(_ a-kleene-clause:kleene-cfexp-binding)
     #`(define a-kleene-clause.id
         (lambda ()
           (set! a-kleene-clause.id
                 #,(syntax/loc #'a-kleene-clause
                     (kleenestar-cfexp (process-cfe-syntax a-kleene-clause.expr.val a-kleene-clause.id))))))]))

(define-syntax (union-cfe-set-box stx)
  (syntax-parse stx
    [(_ a-union-clause:union-cfexp-binding)
     #`(set-box! a-union-clause.id
                 #,(syntax/loc #'a-union-clause
                     (union-cfexp (process-cfe-syntax a-union-clause.expr.vals)
                                  ...)))]))

(define-syntax (make-cfe stx)
  (syntax-parse stx
    [(_ [(~describe #:role "auxiliary cfexps"
                    "cfexp-clause"
                    (~or* a-empty-expr:empty-cfexp-binding
                          a-singleton-expr:singleton-cfexp-binding
                          a-concat-expr:concat-cfexp-binding
                          a-union-expr:union-cfexp-binding
                          a-kleene-clause:kleene-cfexp-binding
                          a-null-expr:null-cfexp-binding)) ...]
        result-expr)
     
     #`(let ()
         ;; Empty cfes
         (~? (define a-empty-expr.id (empty-cfexp)))
         ...
         ;; Null cfes
         (~? (define a-null-expr.id (null-cfexp)))
         ...
         ;; Singleton cfes
         (~? (define-singleton-cfe a-singleton-expr)) ;;a-singleton-expr.id a-singleton-expr a-singleton-expr.expr.val))
         ...
         ;; Concat cfes
         (~? (define-concat-cfe a-concat-expr)) ;a-concat-expr.id a-concat-expr a-concat-expr.expr.vals ...))
         ...
         (~? (define-kleene-cfe a-kleene-clause))
         ...
         ;; Union cfes
         (~? (define a-union-expr.id (box (void))))
         ...
         (~? (a-concat-expr.id))
         ...
         (~? (a-kleene-clause.id))
         ...
         (~? (union-cfe-set-box a-union-expr)) ;; a-union-expr.id a-union-expr a-union-expr.vals ...))
         ...
         (process-cfe-syntax result-expr))]))