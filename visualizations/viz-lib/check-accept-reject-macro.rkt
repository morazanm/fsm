#lang racket/base

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base
                     )
         racket/syntax-srcloc
         racket/contract/base
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/grammar-getters.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "check-turing-machine.rkt"
         "check-machine.rkt"
         "check-grammar.rkt"
         "check-exn.rkt"
         )

(provide check-accept check-reject)



;; Any -> Boolean
;; Purpose: Checks if m is a machine (not tm)
(define (is-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'ndfa m-type)
        (eq? 'dfa m-type)
        (eq? 'pda m-type))))

;; Any -> Boolean
;; Purpose: Checks if m is a turing machine
(define (is-turing-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'tm m-type)
        (eq? 'tm-language-recognizer m-type))))


;; Any -> Boolean
;; Purpose: Checks is g is a grammar
(define (is-grammar? g)
  (or (rg? g)
      (cfg? g)
      (csg? g)))

;; Syntax -> Syntax
;; Changes srcloc data associated with syntax so as to raise errors up to caller code
(define-syntax (reattribute-syntax-loc stx)
    (syntax-parse stx
      [(_ x xc)
       (syntax/loc #'x (identity xc))]))

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
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_)
     #`(raise (exn:fail:check-failed
               "This unit-test does not contain any cases to test, nor any value to test with"
               (current-continuation-marks)
               (list #,(syntax-srcloc stx)))
              #t)]
    [(_ unknown-expr)
     #`(raise (exn:fail:check-failed
               "This unit-test does not contain any cases to test"
               (current-continuation-marks)
               (list #,(syntax-srcloc stx)))
              #t)]
    [(_ unknown-expr x ...)
     #`(cond [(is-turing-machine? unknown-expr)
              (let ([tm-word-contract (new-tm-word/c (cons '_ (sm-sigma unknown-expr)))])
                #,(syntax/loc stx (check-turing-machine #t tm-word-contract unknown-expr x ...)))]
             [(is-machine? unknown-expr)
              (let [(sm-word-contract (sm-word-lst/c (sm-sigma unknown-expr)))]
                #,(syntax/loc stx (check-machine #t sm-word-contract unknown-expr x ...)))]
             [(is-grammar? unknown-expr)
              (let [(grammar-word-contract (new-grammar-word/c (grammar-sigma unknown-expr)))]
                #,(syntax/loc stx (check-grammar #t grammar-word-contract unknown-expr x ...)))]
             [else (raise (exn:fail:check-failed
                           (format "~s is not a valid FSM value that can be tested" (syntax->datum #'unknown-expr))
                           (current-continuation-marks)
                           (list (syntax-srcloc #'unknown-expr)))
                          #t)])]))

;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can reject a given word
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_)
     #`(raise (exn:fail:check-failed
               "This unit-test does not contain any cases to test, nor any value to test with"
               (current-continuation-marks)
               (list #,(syntax-srcloc stx)))
              #t)]
    [(_ unknown-expr)
     #`(raise (exn:fail:check-failed
               "This unit-test does not contain any cases to test"
               (current-continuation-marks)
               (list #,(syntax-srcloc stx)))
              #t)]
    [(_ unknown-expr x ...)
     #`(cond [(is-turing-machine? unknown-expr)
              (let ([tm-word-contract (new-tm-word/c (cons '_ (sm-sigma unknown-expr)))])
                #,(syntax/loc stx (check-turing-machine #f tm-word-contract unknown-expr x ...)))]
             [(is-machine? unknown-expr)
              (let [(sm-word-contract (sm-word-lst/c (sm-sigma unknown-expr)))]
                #,(syntax/loc stx (check-machine #f sm-word-contract unknown-expr x ...)))]
             [(is-grammar? unknown-expr)
              (let [(grammar-word-contract (new-grammar-word/c (grammar-sigma unknown-expr)))]
                #,(syntax/loc stx (check-grammar #f grammar-word-contract unknown-expr x ...)))]
             [else (raise (exn:fail:check-failed
                           (format "~s is not a valid FSM value that can be tested" (syntax->datum #'unknown-expr))
                           (current-continuation-marks)
                           (list (syntax-srcloc #'unknown-expr)))
                          #t)])]))