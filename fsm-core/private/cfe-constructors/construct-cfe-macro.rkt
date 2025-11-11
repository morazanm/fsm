#lang racket/base

(require "context-free-expressions-constructors.rkt"
         (for-syntax racket/base
                     syntax/parse
                     "cfe-macro-syntax-classes.rkt"))

(provide make-cfe)

(define-for-syntax circular-binding-message "Bad circular reference with binding identifier")

(define-syntax (make-cfe stx)
  (syntax-parse stx
    [(_ [(~or* empty-expr:empty-expr
               singleton-expr:singleton-expr
               concat-expr:concat-expr
               union-expr:union-expr
               null-expr:null-expr) ...]
        res-cfe-id:id)
     
     #`(let ()
         ;; Empty cfes
         (~? (define empty-expr.id (empty-cfexp)))
         ...
         ;; Null cfes
         (~? (define null-expr.id (null-cfexp)))
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
         ;; Concat cfes
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? concat-expr.id) ...)))]
                       [stx (in-list (syntax-e #'((~? concat-expr) ...)))]
                       [empty-stx (syntax-e #'((~? ((~? concat-expr.empty-expr) ...)) ...))]
                       [singleton-stx (syntax-e #'((~? ((~? concat-expr.singleton-expr.val) ...)) ...))]
                       [concat-stx (syntax-e #'((~? ((~? ((~? concat-expr.concat-expr.vals) ...)) ...)) ...))]
                       [union-stx (syntax-e #'((~? ((~? ((~? concat-expr.union-expr.vals) ...)) ...)) ...))]
                       [iden-stx (syntax-e #'((~? ((~? concat-expr.iden) ...)) ...))])
              (for ([val-stx (in-list iden-stx)])
                (when (and (identifier? val-stx)
                           (free-identifier=? id-stx val-stx))
                  (raise-syntax-error 'concat-cfexp circular-binding-message stx val-stx '() "")))
              #`(define #,id-stx
                  (lambda ()
                    #,@(for/list ([vals (in-list iden-stx)])
                         #`(when (procedure? #,vals)
                             (#,vals)))
                    (set! #,id-stx
                          #,(quasisyntax/loc stx
                              (concat-cfexp #,@(for/list ([a-empty-stx (in-list (syntax->list empty-stx))])
                                                 #'(empty-cfexp))
                                            #,@(for/list ([a-singleton-stx (in-list (syntax->list singleton-stx))])
                                                 (when (and (identifier? a-singleton-stx)
                                                            (free-identifier=? id-stx a-singleton-stx))
                                                   (raise-syntax-error 'concat-cfexp circular-binding-message stx a-singleton-stx '() ""))
                                                 (quasisyntax/loc singleton-stx
                                                   (singleton-cfexp #,a-singleton-stx)))
                                            #,@(for/list ([a-concat-stx (in-list (syntax->list concat-stx))])
                                                 (for ([val-stx (in-list a-concat-stx)])
                                                   (when (and (identifier? val-stx)
                                                              (free-identifier=? id-stx val-stx))
                                                     (raise-syntax-error 'concat-cfexp circular-binding-message stx val-stx '() "")))
                                                 (quasisyntax/loc concat-stx
                                                   (concat-cfexp #,@a-concat-stx)))
                                            #,@(for/list ([a-union-stx (in-list (syntax->list union-stx))])
                                                 (for ([val-stx (in-list a-union-stx)])
                                                   (when (and (identifier? val-stx)
                                                              (free-identifier=? id-stx val-stx))
                                                     (raise-syntax-error 'concat-cfexp circular-binding-message stx val-stx '() "")))
                                                 (quasisyntax/loc union-stx
                                                   (union-cfexp #,@a-union-stx)))
                                            #,@(syntax->list iden-stx))))
                    #,id-stx)))
         ;; Union cfes
         (~? (define union-expr.id (box (void))))
         ...
         (~? (when (procedure? singleton-expr.id)
               (singleton-expr.id)))
         ...
         (~? (when (procedure? concat-expr.id)
               (concat-expr.id)))
         ...
         (~? (when (procedure? null-expr.id)
               (null-expr.id)))
         ...
         (~? (when (procedure? empty-expr.id)
               (empty-expr.id)))
         ...
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? union-expr.id) ...)))]
                       [empty-stx (syntax-e #'((~? ((~? union-expr.empty-expr) ...)) ...))]
                       [singleton-stx (syntax-e #'((~? ((~? union-expr.singleton-expr.val) ...)) ...))]
                       [concat-stx (syntax-e #'((~? ((~? ((~? union-expr.concat-expr.vals) ...)) ...)) ...))]
                       [union-stx (syntax-e #'((~? ((~? ((~? union-expr.union-expr.vals) ...)) ...)) ...))]
                       [iden-stx (syntax-e #'((~? ((~? union-expr.iden) ...)) ...))]
                       [stx (in-list (syntax-e #'((~? union-expr) ...)))])
              #`(set-box! #,id-stx #,(quasisyntax/loc stx
                                       (union-cfexp #,@(for/list ([a-empty-stx (in-list (syntax->list empty-stx))])
                                                         #'(empty-cfexp))
                                                    #,@(for/list ([a-singleton-stx (in-list (syntax->list singleton-stx))])
                                                         (quasisyntax/loc singleton-stx
                                                           (singleton-cfexp #,a-singleton-stx)))
                                                    #,@(for/list ([a-concat-stx (in-list (syntax->list concat-stx))])
                                                         (quasisyntax/loc concat-stx
                                                           (concat-cfexp #,@a-concat-stx)))
                                                    #,@(for/list ([a-union-stx (in-list (syntax->list union-stx))])
                                                         (quasisyntax/loc union-stx
                                                           (union-cfexp #,@a-union-stx)))
                                                    #,@(syntax->list iden-stx)))))
         (if (procedure? res-cfe-id)
             (res-cfe-id)
             res-cfe-id))]))