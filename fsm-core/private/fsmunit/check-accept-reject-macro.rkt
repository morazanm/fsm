#lang racket/base

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base
                     )
         racket/syntax-srcloc
         racket/contract/base
         racket/list
         "fsm-type-predicates.rkt"
         "check-turing-machine.rkt"
         "check-machine.rkt"
         "check-grammar.rkt"
         "check-exn.rkt"
         "../sm-getters.rkt"
         "../grammar-getters.rkt"
         "check-accept-reject-failure-strings.rkt"
         )

(provide check-in-lang? check-not-in-lang?
         check-derive?)

(define (sm-word-lst/c sigma)
  (listof (apply or/c sigma)))

;; (Listof Symbol) -> Contract
;; Creates contract based off of language of turing machine provided in sigma
(define (new-tm-word/c sigma)
  (define tm-word-char/c (apply or/c sigma))
  (define tm-word/c (or/c null?
                          (cons/c tm-word-char/c (recursive-contract tm-word/c #:flat))))
  tm-word/c)

;; (Listof Symbol) -> Contract
;; Creates contract based off of language of grammar provided in sigma
(define (new-grammar-word/c sigma)
  (define grammar-word-char/c (apply or/c sigma))
  (define grammar-word/c (listof grammar-word-char/c))
  grammar-word/c)

;; Syntax -> Syntax
;; Purpose: To determine whether a given machine/grammar can accept/process a given word
(define-syntax (check-in-lang? stx)
  (syntax-parse stx
    [(_)
     (quasisyntax/loc stx
       (let ([failure-str "Step 2 of the design recipe has not been successfully completed. This unit-test does not contain any cases to test, nor any FSM value to test with."])
         (display-failed-test failure-str (exn:fail:check-failed
                                           failure-str
                                           (current-continuation-marks)
                                           (list #,(syntax-srcloc stx)))
                              )))]
    [(_ unknown-expr)
     (quasisyntax/loc #'unknown-expr
       (if (eq? 'notanfsmval (whatami? unknown-expr))
           (let ([failure-str "Step 2 of the design recipe has not been successfully completed. This test does not contain any FSM value to test with."])
             (display-failed-test failure-str (exn:fail:check-failed
                                               failure-str
                                               (current-continuation-marks)
                                               (list #,(syntax-srcloc stx))))
                  )
           (let ([failure-str (create-failure-str no-test-cases unknown-expr)])
             (display-failed-test failure-str (exn:fail:check-failed
                                               failure-str
                                               (current-continuation-marks)
                                               (list #,(syntax-srcloc stx)))
                                  ))))]
    [(_ unknown-expr x ...)
     #`(cond [(eq? (whatami? unknown-expr) 'turing-machine)
              (let ([tm-word-contract (new-tm-word/c (remove-duplicates (cons '@ (cons '_ (sm-sigma unknown-expr)))))])
                #,(syntax/loc stx (check-turing-machine #t tm-word-contract unknown-expr x ...)))]
             [(eq? (whatami? unknown-expr) 'machine)
              (let [(sm-word-contract (sm-word-lst/c (sm-sigma unknown-expr)))]
                #,(syntax/loc stx (check-machine #t sm-word-contract unknown-expr x ...)))]
             [(eq? (whatami? unknown-expr) 'grammar)
              (let [(grammar-word-contract (new-grammar-word/c (grammar-sigma unknown-expr)))]
                #,(syntax/loc stx (check-grammar #t grammar-word-contract unknown-expr x ...)))]
             #,(quasisyntax/loc #'unknown-expr
                 [else (let ([failure-str (format "Step 2 of the design recipe has not been successfully completed. ~s is not a valid FSM value that can be tested." (syntax->datum #'unknown-expr))])
                     (display-failed-test failure-str (exn:fail:check-failed
                                                       failure-str
                                                       (current-continuation-marks)
                                                       (list (syntax-srcloc #'unknown-expr)))
                                          ))]))]))

;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can reject a given word
(define-syntax (check-not-in-lang? stx)
  (syntax-parse stx
    [(_)
     (quasisyntax/loc stx
       (let ([failure-str "Step 2 of the design recipe has not been successfully completed. This unit-test does not contain any cases to test, nor any FSM value to test with."])
         (display-failed-test failure-str (exn:fail:check-failed
                                           failure-str
                                           (current-continuation-marks)
                                           (list #,(syntax-srcloc stx)))
                              )))]
    ;; Need to check if this is fsm val or test case, then return error string based on that
    [(_ unknown-expr)
     (quasisyntax/loc #'unknown-expr
       (if (eq? 'notanfsmval (whatami? unknown-expr))
           (let ([failure-str "Step 2 of the design recipe has not been successfully completed. This test does not contain any FSM value to test with."])
             (display-failed-test failure-str (exn:fail:check-failed
                                               failure-str
                                               (current-continuation-marks)
                                               (list #,(syntax-srcloc stx))))
                  )
           (let ([failure-str (create-failure-str no-test-cases unknown-expr)])
             (display-failed-test failure-str (exn:fail:check-failed
                                               failure-str
                                               (current-continuation-marks)
                                               (list #,(syntax-srcloc stx)))
                                  ))))]
    [(_ unknown-expr x ...)
     #`(cond [(eq? (whatami? unknown-expr) 'turing-machine)
              (let ([tm-word-contract (new-tm-word/c (cons '_ (sm-sigma unknown-expr)))])
                #,(syntax/loc stx (check-turing-machine #f tm-word-contract unknown-expr x ...)))]
             [(eq? (whatami? unknown-expr) 'machine)
              (let [(sm-word-contract (sm-word-lst/c (sm-sigma unknown-expr)))]
                #,(syntax/loc stx (check-machine #f sm-word-contract unknown-expr x ...)))]
             [(eq? (whatami? unknown-expr) 'grammar)
              (let [(grammar-word-contract (new-grammar-word/c (grammar-sigma unknown-expr)))]
                #,(syntax/loc stx (check-grammar #f grammar-word-contract unknown-expr x ...)))]
             #,(quasisyntax/loc #'unknown-expr
                 [else (let ([failure-str (format "Step 2 of the design recipe has not been successfully completed. ~s is not a valid FSM value that can be tested." (syntax->datum #'unknown-expr))])
                     (display-failed-test failure-str (exn:fail:check-failed
                                                       failure-str
                                                       (current-continuation-marks)
                                                       (list (syntax-srcloc #'unknown-expr)))
                                          ))])
             )]))

(define-syntax check-derive? (make-rename-transformer #'check-in-lang?))