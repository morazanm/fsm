#lang racket/base

(require  #;"../constants.rkt"
          "context-free-expressions-constructors.rkt"
          "cfexp-contracts.rkt"
          #;"../cfg-struct.rkt"
          #;"../cfg.rkt"
          #;"../pda.rkt"
          racket/syntax-srcloc
          (for-syntax racket/base
                      syntax/parse
                      racket/set
                      syntax/parse/experimental/template
                      racket/contract/combinator
                      racket/syntax
                      )
          
          racket/match
          )

(provide construct-cfe)

(define-struct (exn:fail:srcloc exn:fail) (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:srcloc msg marks (list a-srcloc))
       (list a-srcloc)])))

(define-syntax (construct-cfe stx)
  (define-syntax-class concat-expr
    #:attributes (id (vals 1))
    (pattern (id:id ((~literal concat) vals:expr ...))))

  (define-syntax-class singleton-expr
    #:attributes (id val)
    (pattern (id:id ((~literal singleton) val:expr))))

  (define-syntax-class empty-var-expr
    #:attributes (id)
    (pattern (id:id ((~literal var)))))

  (define-syntax-class var-expr
    #:attributes (id binding)
    (pattern (id:id ((~literal var) binding:expr))))

  (define-syntax-class union-expr
    #:attributes (id (vals 1))
    (pattern (id:id ((~literal union) vals:expr ...))))

  (define-syntax-class empty-expr
    #:attributes (id)
    (pattern (id:id ((~literal empty)))))

  (define-syntax-class null-expr
    #:attributes (id)
    (pattern (id:id ((~literal null)))))
  
  (syntax-parse stx
    [(_ [(~or* empty-var-expr:empty-var-expr
               var-expr:var-expr
               singleton-expr:singleton-expr
               concat-expr:concat-expr
               union-expr:union-expr
               empty-expr:empty-expr
               null-expr:null-expr) ...]
        res-cfe-id:id)
     
     #:with var-cfe-lst #`(list (~? (syntax->datum #'var-expr.id) #f) ...)
     #`(let ()
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? empty-expr.id) ...)))]
                       [stx (in-list (syntax-e #'((~? empty-expr) ...)))])
              (quasisyntax/loc stx
                (define #,id-stx (lambda ()
                              (set! #,id-stx (empty-cfexp))
                              #,id-stx))))
         #;(~? (define empty-expr.id (lambda ()
                                          (set! empty-expr.id (empty-cfexp))
                                          empty-expr.id)))
         #;...

         #,@(for/list ([id-stx (in-list (syntax-e #'((~? null-expr.id) ...)))]
                       [stx (in-list (syntax-e #'((~? null-expr) ...)))])
              (quasisyntax/loc stx
                (define #,id-stx (lambda ()
                                   (set! #,id-stx (null-cfexp))
                                   #,id-stx))))
         #;(~? (define null-expr.id (lambda ()
                                     (set! null-expr.id (null-cfexp))
                                     null-expr.id)))
         #;...

         
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? singleton-expr.id) ...)))]
                       [val-stx (in-list (syntax-e #'((~? singleton-expr.val) ...)))]
                       [stx (in-list (syntax-e #'((~? singleton-expr) ...)))])
              (quasisyntax/loc stx
                (begin
                  (unless (singleton-cfexp-input-pred #,val-stx)
                    (raise (exn:fail:srcloc (format "singleton-cfexp expected a symbol [a-Z] or number [0-9] as input, given: ~s" #,val-stx)
                                            (current-continuation-marks)
                                            (list (srcloc '#,(syntax-source stx)
                                                          '#,(syntax-line stx)
                                                          '#,(syntax-column stx)
                                                          '#,(syntax-position stx)
                                                          '#,(syntax-span stx))))))
                  (define #,id-stx (lambda ()
                                     (set! #,id-stx
                                           (singleton-cfexp #,val-stx))
                                     #,id-stx))))

              
              #;(let ([stx-loc (syntax stx)])
                  (quasisyntax/loc stx
                    (begin
                      (unless (singleton-cfexp-input-pred #,val-stx)
                        (raise (exn:fail:srcloc (format "singleton-cfexp expected a symbol [a-Z] or number [0-9] as input, given: ~a" #,val-stx)
                                                (current-continuation-marks)
                                                (list (syntax-srcloc #,stx)))))
                      (define #,id-stx (lambda ()
                                         (set! #,id-stx
                                               (singleton-cfexp #,val-stx))
                                         #,id-stx))))))
         #;(~? (define singleton-expr.id (lambda ()
                                        (set! singleton-expr.id
                                              (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'singleton-expr)))))])
                                             (singleton-cfexp singleton-expr.val)))
                                        singleton-expr.id)))
         #;...
         
         #;(~? (define empty-var-expr.id (lambda ()
                                        (set! empty-var-expr.id (var-cfexp #f))
                                        empty-var-expr.id)))
         #;...
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? var-expr.id) ...)))]
                       [binding-stx (in-list (syntax-e #'((~? var-expr.binding) ...)))]
                       [stx (in-list (syntax-e #'((~? var-expr) ...)))])
              (quasisyntax/loc stx
                (begin
                  (unless (some-pred #,binding-stx)
                    (raise (exn:fail:srcloc (format "some error msg: ~s" #,(syntax->datum binding-stx))
                                            (current-continuation-marks)
                                            (list (srcloc '#,(syntax-source stx)
                                                          '#,(syntax-line stx)
                                                          '#,(syntax-column stx)
                                                          '#,(syntax-position stx)
                                                          '#,(syntax-span stx))))))
                  (define #,id-stx (lambda ()
                                     (set! #,id-stx (var-cfexp (hash-ref symb-lookup (syntax->datum #,id-stx))))
                                     #,id-stx)))))
         #;(~? (define var-expr.id (lambda ()
                                  (set! var-expr.id
                                        (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'var-expr)))))])
                                             (var-cfexp (hash-ref symb-lookup (syntax->datum #'var-expr.id)))))
                                  var-expr.id)))
         #;...
         #,@(for/list ([id-stx (in-list (syntax-e #'((~? union-expr.id) ...)))]
                       [vals-stx (in-list (syntax-e #'((~? (union-expr.vals ...)) ...)))]
                       [stx (in-list (syntax-e #'((~? union-expr) ...)))])
              (quasisyntax/loc stx
                (begin
                  (unless (some-pred #,vals-stx)
                    (raise (exn:fail:srcloc (format "some error msg: ~s" #,(syntax->datum vals-stx))
                                            (current-continuation-marks)
                                            (list (srcloc '#,(syntax-source stx)
                                                          '#,(syntax-line stx)
                                                          '#,(syntax-column stx)
                                                          '#,(syntax-position stx)
                                                          '#,(syntax-span stx))))))
                  (define #,id-stx (lambda ()
                                     (set! #,id-stx (union-cfexp (if (procedure? #,vals-stx)
                                                                     (#,vals-stx)
                                                                     #,vals-stx)...))
                                     #,id-stx)))))
         #;(~? (define union-expr.id (lambda ()
                              (set! union-expr.id
                                    (with-handlers
                                        ([exn:fail:contract?
                                          (lambda (e)
                                            (raise (exn:fail:contract:srcloc
                                                    (exn-message e)
                                                    (current-continuation-marks)
                                                    (list (syntax-srcloc #'union-expr)))))])
                                      (union-cfexp (if (procedure? union-expr.vals)
                                                       (union-expr.vals)
                                                       union-expr.vals)...)))
                              union-expr.id)))
         #;...
         (~? (define concat-expr.id (lambda ()
                                     (set! concat-expr.id
                                           (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'concat-expr)))))])
                                             (concat-cfexp (if (procedure? concat-expr.vals)
                                                             (concat-expr.vals)
                                                             concat-expr.vals) ...)))
                                     concat-expr.id)))
         ...
         (~? (with-handlers
                 ([exn:fail:contract?
                   (lambda (e)
                     (raise (exn:fail:contract:srcloc (exn-message e)
                                                      (current-continuation-marks)
                                                      (list (syntax-srcloc #'var-expr)))))])
               (update-binding! (if (procedure? var-expr.id)
                                    (var-expr.id)
                                    var-expr.id)
                                (hash-ref symb-lookup (syntax->datum #'var-expr.id))
                                (if (procedure? var-expr.binding)
                                    (var-expr.binding)
                                    var-expr.binding))))
         ...
         (if (procedure? res-cfe-id)
             (res-cfe-id)
             res-cfe-id)
         )
     #;#`(letrec ([symb-lookup (for/hash ([cfe-id (in-list var-cfe-lst)]
                                        #:when cfe-id)
                               (values cfe-id (gensym 'A-)))]
                (~? [empty-expr.id (lambda ()
                                    (set! empty-expr.id (empty-cfexp))
                                    empty-expr.id)])
                ...
                (~? [null-expr.id (lambda ()
                                     (set! null-expr.id (null-cfexp))
                                     null-expr.id)])
                ...
                (~? [singleton-expr.id (lambda ()
                                        (set! singleton-expr.id
                                              (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'singleton-expr)))))])
                                             (singleton-cfexp singleton-expr.val)))
                                        singleton-expr.id)])
                ...
                (~? [empty-var-expr.id (lambda ()
                                        (set! empty-var-expr.id (var-cfexp #f))
                                        empty-var-expr.id)])
                ...
                (~? [var-expr.id (lambda ()
                                  (set! var-expr.id
                                        (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'var-expr)))))])
                                             (var-cfexp (hash-ref symb-lookup (syntax->datum #'var-expr.id)))))
                                  var-expr.id)])
                ...
                (~? [union-expr.id (lambda ()
                                     (set! union-expr.id
                                           (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'union-expr)))))])
                                             (union-cfexp (if (procedure? union-expr.vals)
                                                              (union-expr.vals)
                                                              union-expr.vals)...)))
                                     union-expr.id)])
                ...
                (~? [concat-expr.id (lambda ()
                                     (set! concat-expr.id
                                           (with-handlers
                                               ([exn:fail:contract?
                                                 (lambda (e)
                                                   (raise (exn:fail:contract:srcloc
                                                           (exn-message e)
                                                           (current-continuation-marks)
                                                           (list (syntax-srcloc #'concat-expr)))))])
                                             (concat-cfexp (if (procedure? concat-expr.vals)
                                                             (concat-expr.vals)
                                                             concat-expr.vals) ...)))
                                     concat-expr.id)])
                ...)
         (~? (with-handlers
                 ([exn:fail:contract?
                   (lambda (e)
                     (raise (exn:fail:contract:srcloc (exn-message e)
                                                      (current-continuation-marks)
                                                      (list (syntax-srcloc #'var-expr)))))])
               (update-binding! (if (procedure? var-expr.id)
                                    (var-expr.id)
                                    var-expr.id)
                                (hash-ref symb-lookup (syntax->datum #'var-expr.id))
                                (if (procedure? var-expr.binding)
                                    (var-expr.binding)
                                    var-expr.binding))))
         ...
         (if (procedure? res-cfe-id)
             (res-cfe-id)
             res-cfe-id))]))