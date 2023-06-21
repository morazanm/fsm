#lang racket
(provide define-invariant sm-visualize!! sm-visualize!)

(require
 (for-syntax syntax/parse racket/syntax syntax/stx)
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
     #`(run-with-prebuilt fsa #,@(stx-map (lambda (state func)
                                            `(cons ',state ,func))
                                          #`(invs.s-name ...)
                                          #`(invs.func ...)))]))

(define-syntax (sm-visualize!! stx)
  (syntax-parse stx
    [(_ fsa:id invs:invariant ...)
     #:with (inv-name ...) #`(invs.s-name ...)
     #:with (str-inv-name ...) (stx-map (lambda (s) (format-id common-ctx ID-STR (syntax-e s)))
                                   #`(invs.func ...))
     #`(run-with-prebuilt-hotload fsa
                                  (list (cons 'inv-name str-inv-name) ...))]))


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
