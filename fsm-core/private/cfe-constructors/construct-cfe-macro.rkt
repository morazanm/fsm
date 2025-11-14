#lang racket/base

(require "context-free-expressions-constructors.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/match
                     racket/syntax-srcloc
                     "cfe-macro-syntax-classes.rkt"
                     racket/syntax))

(provide make-cfe)

(define-for-syntax circular-binding-message "Bad circular reference with binding identifier")

(define-syntax (process-cfe-syntax stx)
  (syntax-parse stx
    [(_ a-empty-expr:empty-expr-internal)
     #'(empty-cfexp)]
    [(_ a-singleton-expr:singleton-expr-internal)
     (syntax/loc #'a-singleton-expr
       (singleton-cfexp a-singleton-expr.val))]
    [(_ a-concat-expr:concat-expr-internal)
     (quasisyntax/loc #'a-concat-expr
       (concat-cfexp #,@(for/list ([val (in-list (syntax-e #'(a-concat-expr.vals ...)))])
                          (quasisyntax/loc val
                            (process-cfe-syntax #,val)))))]
    [(_ a-union-expr:union-expr-internal)
     (quasisyntax/loc #'a-union-expr
       (union-cfexp #,@(for/list ([val (in-list (syntax-e #'(a-union-expr.vals ...)))])
                         (quasisyntax/loc val
                           (process-cfe-syntax #,val)))))]
    [(_ a-null-expr:null-expr-internal)
     #'(null-cfexp)]
    [(_ iden:id)
     #`(if (procedure? iden)
           #,(syntax/loc #'iden
               (iden))
           iden)]
    [(_ express:expr)
     (define temp-id (generate-temporary))
     #`(let ([#,temp-id (syntax/loc #'express (express))])
         (if (procedure? #,temp-id)
             #,(quasisyntax/loc #'express
                 (#,temp-id))
             #,temp-id))]))

(begin-for-syntax
  (struct exn:fail:bad-circular-ref exn:fail
    (a-srcloc)
    #:property prop:exn:srclocs
    (lambda (a-struct)
      (match a-struct
        [(exn:fail:bad-circular-ref msg marks (list a-srcloc ...))
         a-srcloc]))))

(define-syntax (process-nr-cfe-syntax stx)
  (syntax-parse stx
    [(_ iden:id empty-expr:empty-expr-internal)
     #'(empty-cfexp)]
           
    [(_ iden:id a-singleton-expr:singleton-expr-internal)
     (when (and (identifier? #'a-singleton-expr.val)
                (free-identifier=? #'iden #'a-singleton-expr.val))
       (raise (exn:fail:bad-circular-ref circular-binding-message
                                         (current-continuation-marks)
                                         (list (syntax-srcloc #'iden) (syntax-srcloc #'a-singleton-expr.val)))))
     (syntax/loc #'a-singleton-expr
       (singleton-cfexp a-singleton-expr.val))]
           
    [(_ iden:id a-concat-expr:concat-expr-internal)
     (for ([val-stx (in-list (syntax-e #'(a-concat-expr.vals ...)))])
       (when (and (identifier? val-stx)
                  (free-identifier=? #'iden val-stx))
         (raise (exn:fail:bad-circular-ref circular-binding-message
                                           (current-continuation-marks)
                                           (list (syntax-srcloc #'iden) (syntax-srcloc val-stx))))))
     (quasisyntax/loc #'a-concat-expr
       (concat-cfexp (process-nr-cfe-syntax iden a-concat-expr.vals)
                     ...))]
           
    [(_ iden:id a-union-expr:union-expr-internal)
     (for ([val-stx (in-list (syntax-e #'(a-union-expr.vals ...)))])
       (when (and (identifier? val-stx)
                  (free-identifier=? #'iden val-stx))
         (raise-syntax-error 'union-cfexp circular-binding-message stx val-stx '() "")))
     (quasisyntax/loc #'a-union-expr
       (union-cfexp (process-nr-cfe-syntax iden a-union-expr.vals)
                    ...))]
           
    [(_ iden:id a-null-expr:null-expr-internal)
     #'(null-cfexp)]
           
    [(_ iden:id iden-expr:id)
     (when (and (identifier? #'iden-expr)
                (free-identifier=? #'iden #'iden-expr))
       (raise (exn:fail:bad-circular-ref circular-binding-message
                                         (current-continuation-marks)
                                         (list (syntax-srcloc #'iden) (syntax-srcloc #'iden-expr))))
       #;(raise-syntax-error 'cfexp circular-binding-message stx #'iden-expr '() ""))
     #`(if (procedure? iden)
           #,(syntax/loc #'iden
               (iden))
           iden)]
             
    [(_ iden:id express:expr)
     (define temp-id (generate-temporary))
     #`(let ([#,temp-id (syntax/loc #'express (express))])
         (if (procedure? #,temp-id)
             #,(quasisyntax/loc #'express
                 (#,temp-id))
             #,temp-id))]))

(define-syntax (define-singleton-cfe stx)
  (syntax-parse stx
    [(_ id-stx whole-expr val-stx)
     (when (and (identifier? #'val-stx)
                (free-identifier=? #'id-stx #'val-stx))
                (raise-syntax-error 'singleton-cfexp circular-binding-message #'whole-expr #'val-stx '() ""))
     #`(define id-stx (lambda ()
                          (set! id-stx
                                #,(quasisyntax/loc #'whole-expr
                                    (singleton-cfexp val-stx)))
                          id-stx))]))

(define-syntax (define-concat-cfe stx)
  (syntax-parse stx
    [(_ id-stx whole-expr vals ...)
     #`(define id-stx
         (lambda ()
           (set! id-stx
                 #,(quasisyntax/loc #'whole-expr
                     (concat-cfexp #,@(for/list ([val (in-list (syntax-e #'(vals ...)))])
                                        (quasisyntax/loc val
                                          (process-nr-cfe-syntax id-stx #,val))))))))]))

(define-syntax (union-cfe-set-box stx)
  (syntax-parse stx
    [(_ id-stx whole-expr vals ...)
     #`(set-box! id-stx
                 #,(quasisyntax/loc #'whole-expr
                     (union-cfexp #,@(for/list ([val (in-list (syntax-e #'(vals ...)))])
                                       (quasisyntax/loc val
                                         (process-cfe-syntax #,val))))))]))

(define-syntax (make-cfe stx)
  (syntax-parse stx
    [(_ [(~describe #:role "auxiliary cfexps"
                    "cfexp-clause"
                    (~or* a-empty-expr:empty-expr
                          a-singleton-expr:singleton-expr
                          a-concat-expr:concat-expr
                          a-union-expr:union-expr
                          a-null-expr:null-expr)) ...]
        result-expr)
     
     #`(let ()
         ;; Empty cfes
         (~? (define a-empty-expr.id (empty-cfexp)))
         ...
         ;; Null cfes
         (~? (define a-null-expr.id (null-cfexp)))
         ...
         ;; Singleton cfes
         (~? (define-singleton-cfe a-singleton-expr.id a-singleton-expr a-singleton-expr.val))
         ...
         ;; Concat cfes
         (~? (define-concat-cfe a-concat-expr.id a-concat-expr a-concat-expr.vals ...))
         ...
         ;; Union cfes
         (~? (define a-union-expr.id (box (void))))
         ...
         (~? (when (procedure? a-singleton-expr.id)
               (a-singleton-expr.id)))
         ...
         (~? (when (procedure? a-concat-expr.id)
               (a-concat-expr.id)))
         ...
         (~? (union-cfe-set-box a-union-expr.id a-union-expr a-union-expr.vals ...))
         ...
         (process-cfe-syntax result-expr))]))