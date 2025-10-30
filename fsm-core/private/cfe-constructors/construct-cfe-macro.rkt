#lang racket/base

(require "context-free-expressions-constructors.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide construct-cfe)
(define-for-syntax circular-binding-message "Bad circular reference with binding identifier")
(define-syntax (construct-cfe stx)
  (define-syntax-class concat-expr
    #:attributes (id (vals 1))
    (pattern (id:id ((~literal concat) vals:expr ...))))

  (define-syntax-class singleton-expr
    #:attributes (id val)
    (pattern (id:id ((~literal singleton) val:expr))))

  (define-syntax-class var-expr
    #:attributes (id binding)
    (pattern (id:id ((~literal var) binding:expr))))

  (define-syntax-class union-expr
    #:attributes (id (vals 1))
    (pattern (id:id ((~literal union) vals:expr ...))))

  (define-syntax-class empty-expr
    #:attributes (id)
    (pattern (id:id ((~datum empty)))))

  (define-syntax-class null-expr
    #:attributes (id)
    (pattern (id:id ((~datum null)))))

  (syntax-parse stx
    [(_ [(~or* empty-expr:empty-expr
               var-expr:var-expr
               singleton-expr:singleton-expr
               concat-expr:concat-expr
               union-expr:union-expr
               null-expr:null-expr) ...]
        res-cfe-id:id)
     
     #:with var-cfe-lst #`(list (~? (syntax->datum #'var-expr.id) #f) ...)
     #`(let ()
         (define symb-lookup
           (for/hash ([cfe-id (in-list var-cfe-lst)]
                      #:when cfe-id)
             (values cfe-id (gensym 'A-))))
         ;; Empty cfes
         (~? (define empty-expr.id (lambda ()
                                     (set! empty-expr.id (empty-cfexp))
                                     empty-expr.id)))
         ...
         ;; Null cfes
         (~? (define null-expr.id (lambda ()
                                    (set! null-expr.id (null-cfexp))
                                    null-expr.id)))
         ...
         ;; Singleton cfes
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? singleton-expr.id) ...)))]
                       [val-stx (in-list (syntax-e #'((~? singleton-expr.val) ...)))]
                       [stx (in-list (syntax-e #'((~? singleton-expr) ...)))])
              (when (and (identifier? val-stx)
                         (free-identifier=? id-stx val-stx))
                (raise-syntax-error 'singleton-cfexp circular-binding-message stx val-stx '() ""))
              #`(define #,id-stx (lambda ()
                                   (set! #,id-stx
                                         #,(quasisyntax/loc stx
                                             (singleton-cfexp #,val-stx)))
                                   #,id-stx)))
         ;; Var cfes
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? var-expr.id) ...)))]
                       [val-stx (in-list (syntax-e #'((~? var-expr.binding) ...)))]
                       [stx (in-list (syntax-e #'((~? var-expr) ...)))])
              (when (and (identifier? val-stx)
                         (free-identifier=? id-stx val-stx))
                (raise-syntax-error 'var-cfexp circular-binding-message stx val-stx '() ""))
              #`(define #,id-stx (lambda ()
                                   (set! #,id-stx
                                         #,(quasisyntax/loc stx
                                             (var-cfexp (hash-ref symb-lookup '#,(syntax->datum id-stx)))))
                                   #,id-stx)))
         ;; Union cfes
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? union-expr.id) ...)))]
                       [vals-stx (in-list (syntax-e #'((~? (union-expr.vals ...)) ...)))]
                       [stx (in-list (syntax-e #'((~? union-expr) ...)))])
              (define vals-stx-lst (syntax->list vals-stx))
              (for ([val-stx (in-list vals-stx-lst)])
                (when (and (identifier? val-stx)
                         (free-identifier=? id-stx val-stx))
                (raise-syntax-error 'union-cfexp circular-binding-message stx val-stx '() "")))
              #`(define #,id-stx (lambda ()
                                   #,@(for/list ([vals (in-list vals-stx-lst)])
                                        #`(when (procedure? #,vals)
                                            (#,vals)))
                                   (set! #,id-stx
                                         #,(quasisyntax/loc stx
                                             (union-cfexp #,@vals-stx-lst)))
                                   #,id-stx)))
         ;; Concat cfes
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? concat-expr.id) ...)))]
                       [vals-stx (in-list (syntax-e #'((~? (concat-expr.vals ...)) ...)))]
                       [stx (in-list (syntax-e #'((~? concat-expr) ...)))])
              (define vals-stx-lst (syntax->list vals-stx))
              (for ([val-stx (in-list vals-stx-lst)])
                (when (and (identifier? val-stx)
                         (free-identifier=? id-stx val-stx))
                (raise-syntax-error 'concat-cfexp circular-binding-message stx val-stx '() "")))
              #`(define #,id-stx (lambda ()
                                   #,@(for/list ([vals vals-stx-lst])
                                        #`(when (procedure? #,vals)
                                            (#,vals)))
                                   (set! #,id-stx
                                         #,(quasisyntax/loc stx
                                             (concat-cfexp #,@vals-stx-lst)))
                                   #,id-stx)))
         
         ;; Update bindings for var cfes
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? var-expr.id) ...)))]
                       [binding-stx (in-list (syntax-e #'((~? var-expr.binding) ...)))]
                       [stx (in-list (syntax-e #'((~? var-expr) ...)))])
              #`(begin
                  (when (procedure? #,binding-stx)
                    (#,binding-stx))
                  (when (procedure? #,id-stx)
                    (#,id-stx))
                  #,(quasisyntax/loc stx
                      (update-binding! #,id-stx
                                       (hash-ref symb-lookup '#,(syntax->datum id-stx))
                                       #,binding-stx))))
         (if (procedure? res-cfe-id)
             (res-cfe-id)
             res-cfe-id))]))