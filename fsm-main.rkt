; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

#lang racket

(require "fsa.rkt" "cfg.rkt"  "pda.rkt" 
         "regular-grammar.rkt" "csg.rkt" "tm.rkt" "transducer.rkt"
         "regexp.rkt" "constants.rkt" "word.rkt" "misc.rkt"
         "state.rkt" "sm-getters.rkt" "grammar-getters.rkt" 
         "regexp-predicate.rkt" "abstract-predicate.rkt")
  
(provide
 check-machine
 empties

 ; sm constructors
 make-dfa make-ndfa make-ndpda make-tm
 regexp->fsa ndfa->dfa fsa->regexp
 sm-rename-states 
 sm-union sm-concat sm-kleenestar sm-complement sm-intersection grammar->sm
 make-unchecked-dfa
 make-unchecked-ndfa
 make-unchecked-tm
 make-unchecked-ndpda


 ; sm observers
 sm-apply sm-showtransitions sm-type
 sm-getstates sm-getalphabet sm-getrules sm-getfinals sm-getstart sm-getstackalphabet
 sm-getaccept

 ; sm testers
 sm-sameresult? sm-testequiv? sm-test

 ; ctm constructor and observer
 combine-tms ctm-run

 ; grammar constructors
 make-rg make-cfg make-csg 
 sm->grammar grammar-rename-nts 
 grammar-union grammar-concat
   
 ; grammar observers
 grammar-derive grammar-gettype
 grammar-getnts grammar-getalphabet grammar-getrules grammar-getstart 

 ;grammar testers
 grammar-both-derive grammar-testequiv grammar-test

 ; regexp constructors
 empty-regexp singleton-regexp union-regexp concat-regexp kleenestar-regexp

 ; regexp observers
 simplify-regexp printable-regexp
 regexp? singleton-regexp? concat-regexp? union-regexp? kleenestar-regexp? null-regexp? empty-regexp?

 ; regexp transformers
 fsa->regexp

 ; some helpful functions
 los->symbol symbol->list generate-symbol symbol->fsmlos symbol-upcase

 ; constants
 EMP DEAD RIGHT LEFT LM BLANK BRANCH GOTO ARROW VAR
 
 ; transducers
 make-dfst)
; Primitive constructors imported from other modules
  
; (listof state) fsm --> fsm
(define (sm-rename-states sts m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (rename-states-fsa sts m)]
          [(eq? t1 'pda) (rename-states-pda sts m)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-rename-states sts m)]
          [else (error "Incorrect input to sm-rename-states")])))
  
; fsm fsm --> fsm
(define (sm-union m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(not (eq? t1 t2)) 
           (error "Machines have different types: union-fsm")]
          [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (union-fsa m1 m2)]
          [(eq? t1 'pda) (union-pda m1 m2)]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-union m1 m2)]
          [else (error (format "Unknown/Invalid machine types as input to sm-union: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
  
; fsm fsm --> fsm
(define (sm-concat m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(not (eq? t1 t2)) 
           (error "Machines have different types: concat-fsm")]
          [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (concat-fsa m1 m2)]
          [(eq? t1 'pda) (concat-pda m1 m2)]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2))
           (error (format "Stay tuned: sm-concat for tm language recognizers is not yet implemented"))]
          [else (error (format "Unknown/Invalid machine types as input to sm-concat: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
; fsm --> fsm
(define (sm-kleenestar m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa)) 
           (kleenestar-fsa m)]
          [(eq? t1 'pda) (kleenestar-pda m )]
          [(eq? t1 'tm-language-recognizer)
           (error (format "Stay tuned: sm-kleenestar for tm language recognizers is not yet implemented"))]
          [else (error (format "Unknown/Invalid machine type as input to sm-kleenestar: input is of type ~s" t1))])))
  
; fsm --> fsm or error
(define (sm-complement m)
  (let (( t1 (sm-type m)))
    (cond [(eq? t1 'pda)
           (error "Cannot complement a pda")]
          [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (complement-fsa m)]
          [(eq? t1 'tm-language-recognizer) (tm-complement m )]
          [else (error (format "Unknown/Invalid machine type as input to sm-complement: input is of type ~s" t1))])))
  
  
; sm sm --> sm or error
(define (sm-intersection m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(not (eq? t1 t2)) 
           (error "Machines have different types: intersection-fsm")]
          [(eq? t1 'pda)
           (error "Cannot intersect two pdas")]
          [(or (eq? t1 'dfa)
               (eq? t2 'ndfa))
           (intersection-fsa m1 m2)]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-intersection m1 m2)]
          [else (error (format "Unknown/Invalid machine types as input to sm-intersection: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
; grammar --> sm 
(define (grammar->sm g)
  (let ((t1 (grammar-gettype g)))
    (cond [(eq? t1 'rg) (rg->fsa g)]
          [(eq? t1 'cfg) (cfg->pda g)]
          [(eq? t1 'csg) (error (format "Converting a Context-Sensitive Grammar to a Turing machine is not yet implemented....stay tuned!"))]
          [else (error "Unknown grammar type")])))
  
; fsm word [natnum] --> 'accept or 'reject
(define (sm-apply M w . l)
  (let ((head (if (null? l) 0 (car l)))
        (t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (apply-fsa M w)]
          [(eq? t1 'pda) (apply-pda M w)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-apply M w head)]
          [else (error "Incorrect input to apply-fsm")])))
  
; ctm word [natnum] --> (list state natnum tape)
(define (ctm-run M w . l)
  (let ((res (ctm-apply M w (if (null? l) 0 (car l)))))
    (list (tmconfig-state res) (tmconfig-index res) (tmconfig-tape res))))
  
; fsm word [natnum] --> path
(define (sm-showtransitions M w . l)  
  (let ((head (if (null? l) 0 (car l)))
        (t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (show-transitions-fsa M w)]
          [(eq? t1 'pda) (show-transitions-pda M w)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-showtransitions M w head)]
          [(eq? t1 'dfst) (M 'show-transitions)]
          [else (error "Incorrect input to show-transitions")])))
  
; fsm fsm word --> boolean
(define (sm-sameresult? M1 M2 w)
  (eq? (sm-apply M1 w) (sm-apply M2 w)))
  
; fsm fsm [natnum] --> true or (listof words)
(define (sm-testequiv?  M1 M2 . l)
  (define number-tests (if (null? l) NUM-TESTS (car l)))
  (let* ((test-m1 (generate-words number-tests
                                  (remove* `(,LM) (sm-getalphabet M1))
                                  null))
         (test-m2 (generate-words number-tests
                                  (remove* `(,LM) (sm-getalphabet M2))
                                  null))
         (test-words (append test-m1 test-m2))
         (res-m1 (map (lambda (w) (list w (sm-apply M1 w)))
                      (if (or (eq? (sm-type M1) 'tm)
                              (eq? (sm-type M1) 'tm-language-recognizer))
                          (map (λ (w) (cons LM w)) test-words)
                          test-words)))
         (res-m2 (map (lambda (w) (list w (sm-apply M2 w)))
                      (if (or (eq? (sm-type M2) 'tm)
                              (eq? (sm-type M2) 'tm-language-recognizer))
                          (map (λ (w) (cons LM w)) test-words)
                          test-words))))
    (cond [(or (eq? (sm-type M1) 'tm)
               (eq? (sm-type M2) 'tm))
           (error "Random testing of Turing Machines is not possible.")]
          [(equal? res-m1 res-m2) #t]
          [else (get-differences res-m1 res-m2 test-words)])))
  
; sm [natnum] --> (listof (list word symbol))
(define (sm-test M . l)
  (let ((numtests (if (null? l) NUM-TESTS (car l)))
        (t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (test-fsa M numtests)]
          [(eq? t1 'pda) (test-pda M numtests)]
          [(eq? t1 'tm-language-recognizer) (tm-test M numtests)]
          [(eq? t1 'tm) (error "Random testing of Turing Machines is not possible.")]
          [else (error "Incorrect input to test-fsm")])))

  
  
;;; GRAMMARS
  
; grammar --> fsm
(define (sm->grammar m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (fsa->rg m)]
          [(eq? t1 'pda) (error (format "Converting a PDA to a Context-Free Grammar is not yet implemented....stay tuned!"))]
          [(eq? t1 'tm) (error (format "Converting a Turing machine to a Context-Sensitive Grammar is not yet implemented....stay tuned!"))]
          [else (error "Input is not a valid grammar.")])))
  
; (listof nonterminals) grammar --> grammar
(define (grammar-rename-nts nts g)
  (define type (grammar-gettype g))
  (cond [(eq? type 'rg) (rg-rename-nts nts g)]
        [(eq? type 'cfg) (cfg-rename-nts nts g)]
        [(eq? type 'csg) (csg-rename-nts nts g)]
        [else (error (format "In grammar-rename-nts: Unknown grammar type"))]))
  
; grammar grammar --> grammar
(define (grammar-union g1 g2)
  (let ((same-type (eq? (grammar-gettype g1) (grammar-gettype g2))))
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
  (let ((same-type (eq? (grammar-gettype g1) (grammar-gettype g2))))
    (cond [(not same-type) (error (format "grammar-concat: the input grammars are not the same type."))]
          [(rg? g1)
           (let* ((m1 (grammar->sm g1))
                  (m2 (grammar->sm g2))
                  (newm (ndfa->dfa (sm-concat m1 m2))))
             (sm->grammar newm))]
          [(cfg? g1) (cfg-concat g1 g2)]
          [(csg? g1) (csg-concat g1 g2)]
          [else (error (format "Unknown grammar type ~s given to" (grammar-gettype g1) 'grammar-concat))])))

; grammar --> grammar
(define (grammar-kleenestar g1)
  (let ((gtype (grammar-gettype g1)))
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
    (if (null? diffs) true diffs)))


  

;new constructors with contracts

; make-dfa: (listof state) alphabet state (listof state) (listof rule)) [symbol] --> dfa
(define (make-dfa states sigma start finals deltas . adddead)
  (cond [(equal? true (check-machine states sigma finals deltas start 'dfa))
         (if (null? adddead)
             (make-unchecked-dfa states
                                 sigma
                                 start
                                 finals
                                 deltas)
             (make-unchecked-dfa states
                                 sigma
                                 start
                                 finals
                                 deltas
                                 adddead))]
        [else (begin (newline) (error"Check above message for error"))])
  )

; make-ndfa: (listof states) alphabet state (listof state) (listof rule)
;            --> ndfa
(define (make-ndfa states sigma start finals deltas . adddead)
  (cond [(equal? true (check-machine states sigma finals deltas start 'ndfa))
         (if (null? adddead)
             (make-unchecked-ndfa states
                                  sigma
                                  start
                                  finals
                                  deltas)
             (make-unchecked-ndfa states
                                  sigma
                                  start
                                  finals
                                  deltas
                                  adddead))]
        [else (begin (newline) (error"Check above message for error"))])
  )

; make-ndpda: (listof states) alphabet alphabet state (listof states) (listof pdarules) --> ndpda
(define (make-ndpda states sigma gamma start finals deltas . adddead)
  (cond [(check-machine states sigma finals deltas start 'pda gamma)
         (if (null? adddead)
             (make-unchecked-ndpda states
                                   sigma
                                   gamma
                                   start
                                   finals
                                   deltas)
             (make-unchecked-ndpda states
                                   sigma
                                   gamma
                                   start
                                   finals
                                   deltas
                                   adddead))]
        [else (begin (newline) (error "Check above message for error"))])
  )
  
;make-tm (listof state) (listof symbol) (listof (list state symbol) (list state symbol)) (listof state) state --> tm
(define (make-tm states sigma delta start finals . accept)
  (cond [(equal? (check-machine states
                                sigma
                                finals
                                delta
                                start
                                'tm) #t)
         (if (null? accept) (make-unchecked-tm states
                                               sigma
                                               delta
                                               start
                                               finals)
             (if (member (car accept) finals)
                 (make-unchecked-tm
                  states
                  sigma
                  delta
                  start
                  finals
                  (car accept))
                 (begin (newline) (error (format "accept state: ~s, not in final states" accept)))))]
        [else (begin (newline) (error"Check above message for error"))])) 


;(make-cfg V sigma R S), where V and sigma are a (listof symbol), R
; is a (listof cfg-rule), and S is a symbol
(define (make-cfg nts sigma delta state)
  (cond [(equal? true (check-grammar  nts sigma delta state 'cfg)) (make-unchecked-cfg nts sigma delta state)]
        [else (begin (newline) (error"Check above message for error"))])
  )

;make-csg V sigma R S), where V and sigma are a (listof symbol), R
; is a (listof csg-rule), and S is a symbol
(define (make-csg nts sigma delta state)
  (cond [(equal? true(check-grammar nts sigma delta state 'csg)) (make-unchecked-csg nts sigma delta state)]
        [else (begin (newline) (error"Check above message for error"))])               
  )

;(make-rg N A R S), such that
; N is a (listof symbol) (the non-terminals), A is a (listof symbol) (the
; alphabet), R is a (listof rrule), and S is a symbol (starting symbol)
(define (make-rg nts sigma delta state)
  (cond [(equal? true (check-grammar nts sigma delta state 'rg)) (make-unchecked-rg nts sigma delta state)]
        [else (begin (newline) (error"Check above message for error"))])
  )


(define (singleton-regexp a)
  (local [(define tentative (make-unchecked-singleton a))
          (define final (valid-regexp? tentative))]
    (if (string? final) (error final)
        tentative)
    )
  )
  
(define (concat-regexp a b)
  (local [(define tentative (if (and (regexp? a) (regexp? b)) (make-unchecked-concat a b)
                                (if (regexp? a) (format "~s must be a regexp to be a valid rhs for the concat-regexp ~s ~s" b a b)
                                    (format "~s must be a regexp to be a valid rhs for the concat-regexp ~s ~s" a a b))))
          (define final (if (string? tentative) tentative
                            (valid-regexp? tentative)))]
    (if (string? final) (error final)
        tentative)
    )
  )
  
(define (union-regexp a b)
  (local [(define tentative (if (and (regexp? a) (regexp? b)) (make-unchecked-union a b)
                                (if (regexp? a) (format "~s must be a regexp to be a valid rhs for the union-regexp ~s ~s" b a b)
                                    (format "~s must be a regexp to be a valid rhs for the union-regexp ~s ~s" a a b))))
          (define final (if (string? tentative) tentative
                            (valid-regexp? tentative)))]
    (if (string? final) (error final)
        tentative)
    )
  )
  
(define (kleenestar-regexp a)
  (local [(define tentative (if (regexp? a) (make-unchecked-kleenestar a)
                                (format "~s must be a regexp to be a valid input to the kleenestar-regexp" a)))
          (define final (if (string? tentative) tentative
                            (valid-regexp? tentative)))]
    (if (string? final) (error final)
        tentative)
    )
  )

