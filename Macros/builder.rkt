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
     #:with bname (format-id #'sname "~a-builder-s" #'sname) ;; name of the builder structure
     #:with bname-func (format-id #'sname "~a-builder" #'sname) ;; name of the builder function
     #:with msg-handler (format-id #'defname "~a-msg-handler" #'sname) ;; name of the msg-control function
     ;; name of the build funcion (builds the struct from the builder)
     #:with builder-fn-name (format-id #'defname "~a.build" #'defname)
     ;; creates the mutable fields for the internal builder
     #:with (mut-field ...) (stx-map (λ (f) #`(#,f #:mutable)) #'(fnames.field ...))
     #:with (hidden-name ...) (stx-map (λ (f) (format-id f "add-~a" f)) #`(fnames.field ...))
     ;; set! procedures
     #:with (set-proc ...) (stx-map (λ (f) #`(#,(format-id f "set-~a-~a!" #'bname f) temp val))
                                    #`(fnames.field ...))
   
     ;; default values for the builder struct
     #:with (default-vals ...) (stx-map (λ (v) (syntax-parse v
                                                      [(_ : val) #`val]
                                                      [_ #`'NONE])) #`fnames)
     
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
           (struct bname (mut-field ...))
           (define temp (bname default-vals ...))
           (~@ (define (hidden-name val) set-proc) ...)

           (define (builder->struct)
             (define temp-struct (sname check-fields ...))
             (set! temp (bname default-vals ...))
             temp-struct)
           (define (msg-handler msg)
             (match msg
               (~@ ('hidden-name hidden-name)...)
               ('gen-struct (builder->struct))
               (else (error (format "Invalid pattern supplied. Given ~s" (string msg))))))
           msg-handler)
         (define defname (bname-func))
         public-setters ...
         (define (builder-fn-name)
           (defname 'gen-struct)))]))