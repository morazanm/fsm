#lang racket/base
(require "chomsky.rkt"
         "Chomsky-Greibach-CFG-Transformations/greibach.rkt"
         "grammar-getters.rkt"
         "regular-grammar.rkt"
         "cfg.rkt"
         "cfg-struct.rkt"
         "cyk.rkt"
         "csg.rkt"
         "fsa.rkt"
         "pda.rkt"
         "sm-operations.rkt"
         "constants.rkt"
         "word.rkt"
         racket/list
         "misc.rkt"
         )

(provide cfg->chomsky
         cfg->greibach
         grammar-rename-nts
         grammar->sm
         grammar-union
         grammar-concat
         grammar-kleenestar
         grammar-derive
         grammar-derive?
         grammar-both-derive
         grammar-test
         grammar-testequiv)
         
         
; cfg --> cfg
(define cfg->chomsky chomsky)

; cfg --> cfg
(define cfg->greibach greibach)
  
; (listof nonterminals) grammar --> grammar
(define (grammar-rename-nts nts g)
  (define type (grammar-type g))
  (cond [(eq? type 'rg) (rg-rename-nts nts g)]
        [(eq? type 'cfg) (cfg-rename-nts nts g)]
        [(eq? type 'csg) (csg-rename-nts nts g)]
        [else (error (format "In grammar-rename-nts: Unknown grammar type"))]))

; grammar --> sm 
(define (grammar->sm g)
  (let ((t1 (grammar-type g)))
    (cond [(eq? t1 'rg) (rg->fsa g)]
          [(eq? t1 'cfg) (cfg->pda g)]
          [(eq? t1 'csg) (error (format "Converting a Context-Sensitive Grammar to a Turing machine is not yet implemented....stay tuned!"))]
          [else (error "Unknown grammar type")])))
  
; grammar grammar --> grammar
(define (grammar-union g1 g2)
  (let ((same-type (eq? (grammar-type g1) (grammar-type g2))))
    (cond [(not same-type) (error (format "grammar-union: the input grammars are not the same type."))]
          [(rg? g1)
           (let* ((m1 (grammar->sm g1))
                  (m2 (grammar->sm g2))
                  (newm (ndfa->dfa (sm-union m1 m2))))
             (sm->grammar newm))]
          [(cfg? g1) (cfg-union g1 g2)]
          [(csg? g1) (csg-union g1 g2)]
          [else (error (format "Unknown grammar type"))])))
  
; grammar grammar --> grammar
(define (grammar-concat g1 g2)
  (let ((same-type (eq? (grammar-type g1) (grammar-type g2))))
    (cond [(not same-type) (error (format "grammar-concat: the input grammars are not the same type."))]
          [(rg? g1)
           (let* ((m1 (grammar->sm g1))
                  (m2 (grammar->sm g2))
                  (newm (ndfa->dfa (sm-concat m1 m2))))
             (sm->grammar newm))]
          [(cfg? g1) (cfg-concat g1 g2)]
          [(csg? g1) (csg-concat g1 g2)]
          [else (error (format "Unknown grammar type ~s given to" (grammar-type g1) 'grammar-concat))])))

; grammar --> grammar
(define (grammar-kleenestar g1)
  (let ((gtype (grammar-type g1)))
    (cond [(eq? gtype 'rg) (sm->grammar (sm-kleenestar (grammar->sm g1)))]
          [(eq? gtype 'cfg) (cfg-star g1)]
          [(eq? gtype 'csg) (error (format "Stay tuned! The Kleene star of a csg is not yet implemented"))]
          [else (error (format "Error in grammar-kleenestar: unknown grammar type ~s" gtype))])))
          
  
; grammar word -> derivation or "Not a member"
(define (grammar-derive g w)
  (cond [(rg? g) (rg-derive g w)]
        [(cfg? g) (cfg-derive g w)]
        [(csg? g) (csg-derive g w)]
        [else (error (format "Unknown grammar type"))]))

(define (grammar-derive? g w)
  (cond [(rg? g) (list? (rg-derive g w))]
        [(cfg? g) (if (null? w)
                      (list? (cfg-derive g w))
                      (cyk (chomsky g) w))]
        [(csg? g) (list? (csg-derive g w))]
        [else (error (format "Error in grammar-derive: unknown grammar type"))]))
                  
  
; grammar grammar word --> boolean
(define (grammar-both-derive g1 g2 w)
  (let ((r1 (grammar-derive g1 w))
        (r2 (grammar-derive g2 w)))
    (or (and (string? r1) (string? r2))
        (and (list? r1) (list? r2)))))
  
; grammar word -> derivation or "Not a member"
(define (grammar-test g . l)
  (let ((numtests (if (null? l) NUM-TESTS (car l))))
    (cond [(rg? g) (test-rg g numtests)]
          [(cfg? g) (test-cfg g numtests)]
          [(csg? g) (error (format "test-grammar: A context-sensitive grammar must be tested manually."))]
          [else (error (format "Unknown grammar type"))])))
  
  
; grammar word -> (or true (listof word))
(define (grammar-testequiv g1 g2 . l)
  (let* ((numtests (if (null? l) NUM-TESTS (car l)))
         (sigma1 (cond [(rg? g1) (rg-getalphabet g1)]
                       [(cfg? g1) (cfg-get-alphabet g1)]
                       [(csg? g1) (csg-getsigma g1)]))
         (sigma2 (cond [(rg? g2) (rg-getalphabet g2)]
                       [(cfg? g2) (cfg-get-alphabet g2)]
                       [(csg? g2) (csg-getsigma g2)]))
         (testlist (append (generate-words (floor (/ numtests 2)) sigma1 '())
                           (generate-words (ceiling (/ numtests 2)) sigma2 '())))
         (res1 (map (lambda (w) 
                      (let ((r (grammar-derive g1 w)))
                        (if (string? r) r (last r))))
                    testlist))
         (res2 (map (lambda (w) 
                      (let ((r (grammar-derive g2 w)))
                        (if (string? r) r (last r))))
                    testlist))
         (diffs (get-differences res1 res2 testlist)))
    (if (null? diffs) #t diffs)))