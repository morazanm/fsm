#lang racket
(provide define-invariant define-invariants-for sm-visualize!!! sm-visualize!! sm-visualize!)

(require
  (for-syntax syntax/parse racket/syntax syntax/stx racket/list racket/match)
  syntax/to-string "electron-backend/server.rkt")

(begin-for-syntax
  (define ID-STR "~a-$INV$")
  (define common-ctx #'common-context-for-hidden-ids)


  (define-syntax-class invariant
    #:datum-literals (->)
    #:description "Invariant associated with a state in the machine"
    (pattern (s-name:expr -> func:id)
      #:fail-when (not (identifier-binding #`func))
      (format "Function \"~s\" does not exist" (syntax->datum #`func))
      #:fail-when ((compose1 not symbol? syntax-e) #`s-name)
      "State name must be an identifier")))

(define-syntax (define-invariant stx)
  (syntax-parse stx
    [(~and func (_ (f-name:id args:expr ...) body:expr ...))
     #:with str-f-name (syntax-local-introduce (format-id common-ctx ID-STR (syntax-e #'f-name)))
     #`(begin
         (define f-name (lambda (args ...) body ...))
         ;; syntax->string uses the loction of the syntax to add spaces and new-lines. This
         ;; causes multiple \n to be added to the beginning so we need to remove them
         (define str-f-name (regexp-replace #px"define-invariant"
                                            (syntax->string #`(func))
                                            "define")))]))

(define-syntax (sm-visualize! stx)
  (syntax-parse stx
    [(_ fsa:id invs:invariant ...)
     #:with (inv-name ...) #`(invs.s-name ...)
     #:with (inv-func ...) #`(invs.func ...)
     #:with (str-inv-name ...) (stx-map (lambda (s) (format-id common-ctx ID-STR (syntax-e s)))
                                        #`(invs.func ...))
     #`(run-with-prebuilt fsa (list (list 'inv-name str-inv-name inv-func) ...))]))

(define-syntax (sm-visualize!! stx)
  (syntax-parse stx
    [(_ fsa:id invs:invariant ...)
     #:with (inv-name ...) #`(invs.s-name ...)
     #:with (str-inv-name ...) (stx-map (lambda (s) (format-id common-ctx ID-STR (syntax-e s)))
                                        #`(invs.func ...))
     #`(run-with-prebuilt-hotload fsa (list (cons 'inv-name str-inv-name) ...))]))


; ------------------------------------------------------
(begin-for-syntax
  ;; check-duplicates* :: [syntax] -> syntax | boolean
  ;; if a duplicate identifier is found then the syntax is returned
  (define (check-duplicates* stx-list)
    (define/match (helper lst acc)
      [(`() _) #f]
      [(`(,x ,xs ...) a) (if (member (syntax-e x) a)
                             x
                             (helper xs (cons (syntax-e x) a)))])
    (helper stx-list '()))
  
  
  (define-syntax-class invariant-func
    #:datum-literals (define-invariant-for)
    (pattern (~and (define-invariant-for state:id (args:id ...) body:expr ...+)
                   stx)
      #:attr str-value #`(regexp-replace #px"define-invariant-for *[^\\s]* *"
                                         (syntax->string #`(stx))
                                         "define "))))


(define-syntax (define-invariants-for stx)
  (syntax-parse stx
    [(_ m-name func:invariant-func ...+)
     #:fail-when (check-duplicates* (syntax->list #`(func.state ...)))
     "Duplicate invariant for state found"
     #:with (inv-names ...) (stx-map (lambda (n)
                                       (syntax-local-introduce (format-id common-ctx "~a-inv-~a" #'m-name n)))
                                     #`(func.state ...))
     #:with (str-inv-names ...) (stx-map (lambda (n)
                                           (syntax-local-introduce (format-id common-ctx "~a-inv-~a-str" #`m-name n)))
                                         #`(func.state ...))
     #`(begin
         ;; Add the invariant functions
         (define (inv-names func.args ...) func.body ...) ...

         ;; Add the string representation of the inv funcs for the GUI
         #;(define str-inv-names func.str-value) ;...

         ;; Add both values to a list for the gui 
         (define #,(syntax-local-introduce (format-id common-ctx "~a-inv-gui-list" #`m-name))
           (list #,@(stx-map (lambda (n f s) #`(list '#,n #,s #,f))
                             #`(func.state ...)
                             #`(inv-names ...)
                             #`(func.str-value ...)))))
     ]
    [(_ _) (raise-syntax-error #f "Expected at least one 'define-invariant-for' clause" stx)]))


(define-syntax (sm-visualize!!! stx)
  (syntax-parse stx
    [(_ fsa:id)
     #:with inv-list (format-id common-ctx "~a-inv-gui-list" #'fsa)
     #`(run-with-prebuilt fsa inv-list)]))



(module+ test
  (require rackunit syntax/macro-testing)
  (define a-aUb* 'dummy)

  
  (check-exn #rx"State name must be an identifier"
             (lambda ()
               (define S-INV (lambda (v) #t))
               (convert-compile-time-error (sm-visualize!! a-aUb* ('S -> S-INV)))))

  (check-exn #rx"Function \"S-INV\" does not exist"
             (lambda ()
               (convert-compile-time-error (sm-visualize!! a-aUb* (S -> S-INV) (F -> F-INV)))))

  ) ;;end module+ test
