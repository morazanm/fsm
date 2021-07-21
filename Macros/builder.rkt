#lang racket
(require (for-syntax racket/syntax syntax/stx syntax/parse))
(provide builder)


(define-syntax (builder stx)
  (define-syntax-class distinct-fields
    #:description "builder structure field names and values"
    #:datum-literals (:)
    (pattern ((field:id (~optional (~seq : val:expr) #:defaults ([val #`'NONE]))) ...)
             #:with (id ...) #'(field ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(field ...)))
             "Duplicate field name"))
  (syntax-parse stx
    [(_ defname:id (sname:id fnames:distinct-fields))
     #:with bname-func (format-id #'sname "~a-builder" #'sname) ;; name of the builder function
     #:with msg-handler (format-id #'defname "~a-msg-handler" #'sname) ;; name of the msg-control function
     #:with (hidden-fn-names ...) (stx-map (λ (f) (format-id f "add-~a" f)) #`(fnames.field ...)) ;; names of the msg passing functions
     #:with (field-checks ...) (stx-map (λ (f) (with-syntax [(id (format-id f "~a-~a" #'sname f))] ;; if statements used to check that there are no 'NONE's left in the builder struct
                                                 #`(if (eq? 'NONE (id temp))
                                                       (error (format "Field ~a has not been set for the builder" #,((compose symbol->string syntax->datum) f)))
                                                       (id temp))))
                                        #`(fnames.field ...))
     
     #`(begin
         (struct sname ([fnames.field  #:mutable] ...) #:transparent)
         (define (bname-func)
           (define temp (sname fnames.val ...))

           #,@(stx-map (λ (f) #`(define (#,(format-id f "add-~a" f) val)
                                  (#,(format-id f "set-~a-~a!" #'sname f) temp val)))
                       #`(fnames.field ...))

           (define (builder->struct)
             (define temp-struct (sname field-checks ...))
             (set! temp (sname fnames.val ...))
             temp-struct)

           (define (msg-handler msg)
             (match msg
               (~@ ('hidden-fn-names hidden-fn-names)...)
               ('gen-struct (builder->struct))
               (else (error (format "Invalid pattern supplied. Given ~s" (string msg))))))
           msg-handler)
           
         (define defname (bname-func))
         
         #,@(stx-map (λ (f) #`(define (#,(format-id #'defname "~a.add-~a" #'defname f) val)
                                ((defname '#,(format-id #'n "add-~a" f)) val)))
                     #`(fnames.field ...))
         
         (define (#,(format-id #'defname "~a.build" #'defname))
           (defname 'gen-struct)))

     ]))