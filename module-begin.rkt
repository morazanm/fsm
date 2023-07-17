#lang racket
(require (for-syntax syntax/parse racket/syntax syntax/stx racket/list racket/match)
         syntax/parse/define 
         racket/splicing
         syntax/to-string
         "fsm-gui/interface.rkt")

(provide module-begin
         define-invariants-for-dfa
         define-invariants-for-ndfa
         define-invariants-for-pda
         define-invariants-for-tm
         define-invariants-for-mttm
         sm-visualize!
         $MODULE-NAMESPACE$)

;; We need to attach a namespace anchor to the top of the module so we can
;; access all functions defined when calling eval
(define $MODULE-NAMESPACE$ (make-parameter #f))
(define-syntax-parser module-begin
  [(_ . xs)
   #'(#%module-begin
      (define-namespace-anchor anchor)
      (splicing-parameterize ([$MODULE-NAMESPACE$ (namespace-anchor->namespace anchor)])
        . xs))])

(begin-for-syntax
  (define ID-INV "~a-$inv$-~a")
  ;; This hash table holds the list of invarinats that a machine is associated with
  ;; The key is the machine name and value is a listof(cons symbol string)
  (define shared-invariant-data (make-hash))
  
  ;; A common scope for storing variables 
  (define common-ctx #'common-context-for-hidden-ids)

  ;; check-duplicates* :: [syntax] -> syntax | boolean
  ;; if a duplicate identifier is found then the syntax is returned
  (define (check-duplicates* stx-list)
    (define/match (helper lst acc)
      [(`() _) #f]
      [(`(,x ,xs ...) a)
       (if (member (syntax-e x) a) x (helper xs (cons (syntax-e x) a)))])
    (helper stx-list '()))

  ;; Syntax class for racket define keyword
  (define-syntax-class racket-define
    #:literals (define)
    (pattern (define n:id e:expr))
    (pattern (define (n:id params:id ...) body:expr ...+)))
      
  ;; Syntax for associating a invariant with a state in the machine
  (define-syntax-class (invariant-func machine-name arity-num)
    #:datum-literals (define-invariant)
    (pattern (~and (define-invariant state:id (arg:id ...) body:expr ...+)
                   stx)
      #:with id (syntax-local-introduce (format-id common-ctx ID-INV machine-name #'state))
      #:with str-value #`(regexp-replace #px"define-invariant *[^\\s]* *\\("
                                         (syntax->string #`(stx))
                                         (format "define (~a-inv " (syntax-e #'state)))
      #:with arg-len (length (syntax->list #'(arg ...)))
      #:fail-when (not (equal? (syntax-e #'arg-len) arity-num))
      (format "Arity mismatch. Expected ~s, but was given ~s" arity-num (syntax-e #'arg-len)))))


;; Macro for starting GUI 2.0. if invariants are defined for the machine they are
;; sent to the viztool otherwise just the machine is sent.
(define-syntax (sm-visualize! stx)
  (syntax-parse stx
    [(_ fsa:id)
     #:with invariants (hash-ref shared-invariant-data (syntax-e #'fsa) #''())
     #`(run-with-prebuilt fsa invariants ($MODULE-NAMESPACE$))]))


; This macro generates a macro for each for each of the `define-invariants-for-<machine-type>`
;; forms. The first arg is the name of the generated macro and the second arg is the arity that the
;; `invariant-func` should be associated with.
(define-syntax-parse-rule (generate-invariant-macro macro-name:id num:expr)
  (...
   (define-syntax (macro-name stx)
     (syntax-parse stx
       [(_ m-name:id (~or func:racket-define (~var func (invariant-func #'m-name num))) ...+)
        #:fail-when (empty? (syntax->list #'((~? func.state) ...)))
        "Expected at least one 'define-invariant' clause"
        #:fail-when (check-duplicates* (syntax->list #`((~? func.state) ...)))
        "Duplicate invariant for state found"
        (begin 
          (hash-set! shared-invariant-data
                     (syntax-e #'m-name)
                     #'(list (~? (cons 'func.state func.str-value)) ...))
          #`(begin
              ;; Add the invariant functions
              (~? (define (func.id func.arg ...) func.body ...) func) ...))]
       [(_ _) (raise-syntax-error #f "Expected at least one 'define-invariant' clause" stx)]))))

(generate-invariant-macro define-invariants-for-dfa  1)
(generate-invariant-macro define-invariants-for-ndfa 1)
(generate-invariant-macro define-invariants-for-pda  2)
(generate-invariant-macro define-invariants-for-tm   2)
(generate-invariant-macro define-invariants-for-mttm 2)


(module+ test
  (require rackunit rackunit/text-ui syntax/macro-testing)
  (define a^nb^nc^n2 'dummy)

  (define macro-tests
    (test-suite
     "Macro tests"
     (test-case "dfa"
                (check-exn #rx"Duplicate invariant for state found"
                           (lambda ()
                             (define a* 'dymmy)
                             (convert-compile-time-error
                              (define-invariants-for-dfa a*
                                (define-invariant S (ci) #t)
                                (define-invariant A (ci) #t)
                                (define-invariant S (ci) #f)))))

                (check-exn #rx"Arity mismatch. Expected 1, but was given 2"
                           (lambda ()
                             (define a* 'dymmy)
                             (convert-compile-time-error
                              (define-invariants-for-dfa a*
                                (define-invariant S (ci o) #t)
                                (define-invariant A (ci o) #t)
                                (define-invariant S (ci o) #f))))))

     (test-case "ndfa"
                (check-exn #rx"Duplicate invariant for state found"
                           (lambda ()
                             (define a* 'dymmy)
                             (convert-compile-time-error
                              (define-invariants-for-ndfa a*
                                (define-invariant S (ci) #t)
                                (define-invariant A (ci) #t)
                                (define-invariant S (ci) #f)))))

                (check-exn #rx"Arity mismatch. Expected 1, but was given 2"
                           (lambda ()
                             (define a* 'dymmy)
                             (convert-compile-time-error
                              (define-invariants-for-ndfa a*
                                (define-invariant S (ci o) #t)
                                (define-invariant A (ci o) #t)
                                (define-invariant S (ci o) #f))))))

     (test-case "pda"
                (check-exn #rx"Duplicate invariant for state found"
                           (lambda ()
                             (define a* 'dymmy)
                             (convert-compile-time-error
                              (define-invariants-for-pda a*
                                (define-invariant S (ci stack) #t)
                                (define-invariant A (ci stack) #t)
                                (define-invariant S (ci stack) #f)))))

                (check-exn #rx"Arity mismatch. Expected 2, but was given 1"
                           (lambda ()
                             (define a* 'dymmy)
                             (convert-compile-time-error
                              (define-invariants-for-pda a*
                                (define-invariant S (ci) #t)
                                (define-invariant A (ci) #t)
                                (define-invariant S (ci) #f))))))

     (test-case "tm"
                (check-exn #rx"Duplicate invariant for state found"
                           (lambda ()
                             (define S-INV (lambda (tape posn) #t))
                             (convert-compile-time-error
                              (define-invariants-for-tm a^nb^nc^n2
                                (define get-num-of-x (lambda (tape symbol)
                                                       (length (filter (lambda (ele)
                                                                         (eq? ele symbol)) tape))))

                                (define-invariant B (tape posn)
                                  (let ((num-z (get-num-of-x tape 'z)))
                                    (and (> num-z (get-num-of-x tape 'x))
                                         (> num-z (get-num-of-x tape 'y)))))

                                ;; the number of x's before the first b is one more then the number of x's after the first b
                                (define-invariant C (tape posn)
                                  (let ((num-x (get-num-of-x tape 'x)))
                                    (and (= num-x (get-num-of-x tape 'z))
                                         (> num-x (get-num-of-x tape 'y)))))
                  

                                ;; the number of z's is one bigger then number of x's and number of y's
                                (define-invariant B (tape posn)
                                  (let ((num-z (get-num-of-x tape 'z)))
                                    (and (> num-z (get-num-of-x tape 'x))
                                         (> num-z (get-num-of-x tape 'y))))))))))
     (test-case "general errors"
                (check-exn #rx"Expected at least one 'define-invariant' clause"
                           (lambda ()
                             (convert-compile-time-error (define-invariants-for-tm a^nb^nc^n2 ))))
  

                (check-exn #rx"Expected at least one 'define-invariant' clause"
                           (lambda ()
                             (convert-compile-time-error (define-invariants-for-pda a^nb^nc^n2
                                                           (define x 10))))))))

  (run-tests macro-tests)
  ) ;;end module+ test