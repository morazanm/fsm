#lang racket
(require (for-syntax racket/syntax syntax/stx syntax/parse))
(provide builder)

(define-syntax (builder stx)
  (define-syntax-class distinct-fields
    #:description "builder structure field names"
    #:datum-literals (:)
    (pattern ((~or* (field:id : val:expr) field:id) ...)
             #:with (id ...) #'(field ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(field ...)))
             "Duplicate field name"))
  (syntax-parse stx
    [(_ defname:id (sname:id fnames:distinct-fields))
     #:with bname (format-id #'sname "~a-builder-s" #'sname) ;; name of the builder struct
     #:with bname-func (format-id #'sname "~a-builder" #'sname) ;; name of the builder function
     #:with cname (format-id #'defname "~a-control" #'sname) ;; name of the msg-control function
     ;; name of the build funcion (builds the struct from the builder)
     #:with builder-fn-name (format-id #'defname "~a.build" #'defname)
     ;; creates the mutable fields for the internal builder
     #:with (mut-fields ...) (stx-map (lambda (v) (syntax-parse v
                                                    [n #`[n #:mutable]])) #`(fnames.field ...))
     ;; condition branches for the control struct
     #:with (control-conds ...) (stx-map (lambda (v) (syntax-parse v
                                                         [n
                                                          #:with id (format-id #'n "add-~a" #'n)
                                                          #`[(eq? 'id msg) id]]))
                                           #`(fnames.field ...))
     ;; default values for the builder struct
     #:with (default-vals ...) (stx-map (lambda (v) (syntax-parse v
                                                      [(_ : val) #`val]
                                                      [_ #`'NONE])) #`fnames)
     ;; local set! funcs for the builder
     #:with (hidden-setters ...) (stx-map (lambda (v) (syntax-parse v
                                                         [n
                                                          #:with fname (format-id #'n "add-~a" #'n)
                                                          #:with set (format-id #'n "set-~a-~a!" #'bname #'n)
                                                          #'(define (fname val)
                                                              (set temp val))]))
                                           #`(fnames.field ...))
     ;; makes sure that all fields are properly set
     #:with (check-fields ...) (stx-map (lambda (v) (with-syntax* ([n v] ;; TODO: REMOVE?
                                                                   [actual ((compose symbol->string syntax->datum) #'n)]
                                                                   [id (format-id #'n "~a-~a" #'bname #'n)])
                                                      #`(if (eq? 'NONE (id temp))
                                                            (error (format "Field ~a has not been set for the builder" actual))
                                                            (id temp))))
                                        #`(fnames.field ...))
     ;; setters that the user interacts with
     #:with (public-setters ...) (stx-map (lambda (v) (syntax-parse v
                                                    [field
                                                     #:with fname (format-id #'defname "~a.add-~a" #'defname #'field)
                                                     #:with msg (format-id #'n "add-~a" #'field)
                                                     #`(define (fname val)
                                                         ((defname 'msg) val))]))
                                      #`(fnames.field ...))                                              
     #`(begin
         (struct sname (fnames.field ...) #:transparent)
         (define (bname-func)
           (struct bname (mut-fields ...))
           (define temp (bname default-vals ...))
           hidden-setters ...
           (define (builder->struct)
             (define temp-struct (sname check-fields ...))
             (set! temp (bname default-vals ...))
             temp-struct)
           (define (cname msg)
           (cond
             control-conds ...
             [(eq? 'gen-struct msg) (builder->struct)]))
           cname)
         (define defname (bname-func))
         public-setters ...
         (define (builder-fn-name)
           (defname 'gen-struct)))]))