; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

#lang racket/base

(require
  "private/fsa.rkt"
  "private/cfg.rkt"
  "private/pda.rkt" 
  "private/regular-grammar.rkt"
  "private/csg.rkt"
  "private/tm.rkt" 
  "private/regexp.rkt"
  "private/constants.rkt"
  "private/word.rkt"
  "private/misc.rkt"
  "private/state.rkt"
  "private/sm-getters.rkt"
  "private/grammar-getters.rkt" 
  "private/regexp-predicate.rkt"
  "private/abstract-predicate.rkt"
  "private/mtape-tm.rkt"
  "private/macros/regexp-contracts.rkt"
  "private/macros/constructors.rkt"
  "private/macros/grammar-constructors.rkt"
  "private/sm-apply.rkt"
  "private/sm-apply.rkt"
  "private/callgraphs/callgraphs-ndfa.rkt"
  "private/callgraphs/callgraphs-pda.rkt"
  "private/callgraphs/callgraphs-tm.rkt"
  "private/callgraphs/callgraphs-mttm.rkt"
  "private/callgraphs/transdiagram-mttm.rkt"
  "../visualizations/viz-sm/viz-ctm.rkt"
  "../visualizations/viz-sm/sm-viz.rkt"
  "../visualizations/viz-grammar-constructors/rg-viz.rkt"
  "../visualizations/viz-grammar-constructors/cfg-viz.rkt"
  "../visualizations/viz-grammar-constructors/csg-viz.rkt"
  "private/Chomsky-Greibach-CFG-Transformations/chomsky.rkt"
  "private/Chomsky-Greibach-CFG-Transformations/greibach.rkt"
  "private/fsmunit/check-accept-reject-macro.rkt"
  racket/list
  racket/bool
  racket/contract)
  
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
 make-mttm


 ; sm observers
 sm-apply sm-showtransitions sm-type
 sm-states sm-sigma sm-rules sm-finals sm-start sm-gamma
 sm-accept sm-numtapes

 ; sm testers
 sm-sameresult? sm-testequiv? sm-test

 ; ctm constructor and observer
 combine-tms ctm-run

 ; grammar constructors
 make-rg make-cfg make-csg 
 sm->grammar grammar-rename-nts 
 grammar-union grammar-concat grammar-kleenestar
   
 ; grammar observers
 grammar-derive grammar-type
 grammar-nts grammar-sigma grammar-rules grammar-start 

 ; grammar testers
 grammar-both-derive grammar-testequiv grammar-test

 ; grammar transformations
 cfg->chomsky cfg->greibach
 
 ; regexp constructors
 empty-regexp singleton-regexp union-regexp concat-regexp kleenestar-regexp null-regexp

 ; regexp observers
 simplify-regexp printable-regexp
 regexp? singleton-regexp? concat-regexp? union-regexp? kleenestar-regexp? null-regexp? empty-regexp?
 singleton-regexp-a
 concat-regexp-r1 concat-regexp-r2
 union-regexp-r1 union-regexp-r2
 kleenestar-regexp-r1
 extract-concat-regexps
 convert-singleton
 pick-regexp pick-reps
 extract-union-regexps
 gen-regexp-word gen-concat-word gen-ks-word 

 ; regexp transformers
 fsa->regexp

 ; some helpful functions
 los->symbol symbol->list generate-symbol symbol->fsmlos symbol-upcase
 gen-state gen-nt

 ; constants
 EMP DEAD RIGHT LEFT LM BLANK BRANCH GOTO ARROW VAR

 ; sm-apply
 ; sm-apply

 ; ctm-viz
 ctm-viz

 ; computation graphs
 sm-cmpgraph

 ;; FSM Unit Testing
 check-derive? check-not-derive?
 ;check-gen? check-not-gen?
 check-accept? check-reject?
 ;check-in-lang? check-not-in-lang?
 
 ;; Grammar visualizations
 grammar-viz

 sm-viz
 )
; Primitive constructors imported from other modules

(define (grammar-viz G w #:derv-type [derv-type 'left] #:cpu-cores [cpu-cores #f] . invariants)
  (cond [(rg? G) (apply rg-viz G w #:cpu-cores cpu-cores invariants)]
        [(cfg? G) (apply cfg-viz G w #:cpu-cores cpu-cores #:derv-type derv-type invariants)]
        [(csg? G) (apply csg-viz G w #:cpu-cores cpu-cores invariants)]
        [else (error "Unknown grammar type given to grammar-viz.")]))
  
; sm word [natnum] --> image
(define (sm-cmpgraph M w #:palette [p 'default] #:cutoff [c 100] . headpos)
  (let ((t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (computation-diagram-fsa M w p)]
          [(eq? t1 'pda)
           (computation-diagram-pda M w c p)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer))
           (computation-diagram-tm M w (if (empty? headpos) 0 (first headpos)) c p)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (computation-diagram-mttm M w (if (empty? headpos) 0 (first headpos)) c p)]
          [else (error "Unknown machine type given to sm-cmpgraph.")])))
  
; (listof state) fsm --> fsm
(define (sm-rename-states sts m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (rename-states-fsa sts m)]
          [(eq? t1 'pda) (rename-states-pda sts m)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-rename-states sts m)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "State renaming not supported for multitape Turing machines")]
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
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Union not supported for multitape Turing machines")]
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
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Concatenation not supported for multitape Turing machines")]
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
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Kleene star not supported for multitape Turing machines")]
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
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Complement not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine type as input to sm-complement: input is of type ~s" t1))])))
  
  
; sm sm --> sm or error
(define (sm-intersection m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(and (or (eq? t1 'dfa) (eq? t1 'ndfa))
                (or (eq? t2 'dfa) (eq? t2 'ndfa)))
           (intersection-fsa m1 m2)]
          [(eq? t1 'pda)
           (error "Cannot intersect two pdas")]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-intersection m1 m2)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Intersection not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine types as input to sm-intersection: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
; grammar --> sm 
(define (grammar->sm g)
  (let ((t1 (grammar-type g)))
    (cond [(eq? t1 'rg) (rg->fsa g)]
          [(eq? t1 'cfg) (cfg->pda g)]
          [(eq? t1 'csg) (error (format "Converting a Context-Sensitive Grammar to a Turing machine is not yet implemented....stay tuned!"))]
          [else (error "Unknown grammar type")])))
  
;; fsm word [natnum] --> 'accept or 'reject
;(define (sm-apply M w . l)
;  (let ((head (if (null? l) 0 (car l)))
;        (t1 (sm-type M)))
;    (cond [(or (eq? t1 'dfa)
;               (eq? t1 'ndfa))
;           (apply-fsa M w)]
;          [(eq? t1 'pda) (apply-pda M w)]
;          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-apply M w head)]
;          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (mttm-apply M w head)]
;          [else (error "Incorrect input to apply-fsm")])))
;  
;; fsm word [natnum] --> path
;(define (sm-showtransitions M w . l)  
;  (let ((head (if (null? l) 0 (car l)))
;        (t1 (sm-type M)))
;    (cond [(or (eq? t1 'dfa)
;               (eq? t1 'ndfa))
;           (show-transitions-fsa M w)]
;          [(eq? t1 'pda) (show-transitions-pda M w)]
;          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-showtransitions M w head)]
;          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (mttm-show-transitions M w head)]
;          ;[(eq? t1 'dfst) (M 'show-transitions)]
;          [else (error "Incorrect input to show-transitions")])))

; ctm word [trace Boolean] [natnum] --> (list state natnum tape)
(define (ctm-run M w #:trace [trace #f] . l)
  (let ((res (ctm-apply M w (if (null? l) 0 (car l)) trace)))
    (if trace
        res
        (list (tmconfig-state res) (tmconfig-index res) (tmconfig-tape res)))))

;; ctm word [natnum] --> (list state natnum tape)
;(define (ctm-run M w . l)
;  (let ((res (ctm-apply M w (if (null? l) 0 (car l)))))
;    (list (tmconfig-state res) (tmconfig-index res) (tmconfig-tape res))))
  
; fsm fsm word --> boolean
(define (sm-sameresult? M1 M2 w)
  (let ((s1 (sm-sigma M1))
        (s2 (sm-sigma M2)))
    (if (equal? s1 s2)
        (equal? (sm-apply M1 w) (sm-apply M2 w))
        (error (format "The alphabets of the given machines are different: ~s ~s" s1 s2)))))
  
; fsm fsm [natnum] --> true or (listof words)
(define (sm-testequiv?  M1 M2 . l)
  (define number-tests (if (null? l) NUM-TESTS (car l)))
  (if (or (eq? (sm-type M1) 'mttm)
          (eq? (sm-type M1) 'mttm-language-recognizer)
          (eq? (sm-type M2) 'mttm)
          (eq? (sm-type M2) 'mttm-language-recognizer))
      (error "Random testing of Multitape Turing Machines is not possible.")
      (let* ((test-m1 (generate-words number-tests
                                      (remove* `(,LM) (sm-sigma M1))
                                      null))
             (test-m2 (generate-words number-tests
                                      (remove* `(,LM) (sm-sigma M2))
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
              [else (remove-duplicates (get-differences res-m1 res-m2 test-words))]))))
  
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
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Random testing of Multitape Turing Machines is not possible.")]
          [else (error "Incorrect input to test-fsm")])))

  
  
;;; GRAMMARS

; cfg --> cfg
(define cfg->chomsky chomsky)

; cfg --> cfg
(define cfg->greibach greibach)
  
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
  (define type (grammar-type g))
  (cond [(eq? type 'rg) (rg-rename-nts nts g)]
        [(eq? type 'cfg) (cfg-rename-nts nts g)]
        [(eq? type 'csg) (csg-rename-nts nts g)]
        [else (error (format "In grammar-rename-nts: Unknown grammar type"))]))
  
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

;; make-dfa: states alphabet state states rules (boolean) -> machine
;; Purpose: Eventually, will construct a multi-tape turing-machine from the given
;; DFA inputs, but for now just parses inputs and constructs an unchecked-dfa.
(define/contract (make-dfa states sigma start finals rules
                           [add-dead '()]
                           #:accepts [accepts '()]
                           #:rejects [rejects '()])
  make-dfa/c
  (if (null? add-dead)
      (make-unchecked-dfa states sigma start finals rules)
      (make-unchecked-dfa states sigma start finals rules add-dead)
      )
  )

  

(define/contract (make-ndfa states sigma start finals rules
                            #:accepts [accepts '()]
                            #:rejects [rejects '()])
  make-ndfa/c
  (make-unchecked-ndfa states sigma start finals rules)
  )

;; Purpose: Constructs an ndpda given a set of states, a machine alphabet,
;; set of stack symbols, a start state, a list of final states, and a list
;; of ndpda rules. The function checks that all fields are valid before
;; constructing the ndpda.
(define/contract (make-ndpda states sigma gamma start finals rules
                             #:accepts [accepts '()]
                             #:rejects [rejects '()])
  make-ndpda/c
  (make-unchecked-ndpda states sigma gamma start finals rules)
  )

   
(define/contract (make-tm states sigma rules start finals
                          [accept 'null]
                          #:accepts [accepts '()]
                          #:rejects [rejects '()]
                          )
  make-tm/c
  (if (equal? accept 'null)
      (make-unchecked-tm states sigma rules start finals)
      (make-unchecked-tm states sigma rules start finals accept))
  )

(define/contract (make-mttm states sigma start finals rules num-tapes
                            [accept 'null]
                            #:accepts [accepts '()]
                            #:rejects [rejects '()])
  make-mttm/c
  (if (equal? accept 'null)
      (make-unchecked-mttm states sigma start finals rules num-tapes)
      (make-unchecked-mttm states sigma start finals rules num-tapes accept))
  )


;;(make-cfg V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof cfg-rule), and S is a symbol
;(define (make-cfg nts sigma delta state)
;  (cond [(equal? true (check-grammar  nts sigma delta state 'cfg)) (make-unchecked-cfg nts sigma delta state)]
;        [else (begin (newline) (error"Check above message for error"))])
;  )
;
;;make-csg V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof csg-rule), and S is a symbol
;(define (make-csg nts sigma delta state)
;  (cond [(equal? true(check-grammar nts sigma delta state 'csg)) (make-unchecked-csg nts sigma delta state)]
;        [else (begin (newline) (error"Check above message for error"))])               
;  )
;
;;(make-rg N A R S), such that
;; N is a (listof symbol) (the non-terminals), A is a (listof symbol) (the
;; alphabet), R is a (listof rrule), and S is a symbol (starting symbol)
;(define (make-rg nts sigma delta state)
;  (cond [(equal? true (check-grammar nts sigma delta state 'rg)) (make-unchecked-rg nts sigma delta state)]
;        [else (begin (newline) (error"Check above message for error"))])
;  )

;;(make-cfg V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof cfg-rule), and S is a symbol
(define/contract (make-cfg nts sigma delta state
                           #:accepts [accepts '()]
                           #:rejects [rejects '()])
  make-cfg/c
  (make-unchecked-cfg nts sigma delta state)
  )


;;make-csg V sigma R S), where V and sigma are a (listof symbol), R
;; is a (listof csg-rule), and S is a symbol
(define/contract (make-csg nts sigma delta state
                           #:accepts [accepts '()]
                           #:rejects [rejects '()])
  make-csg/c
  (make-unchecked-csg nts sigma delta state)
  )
  
;;(make-rg N A R S), such that
;; N is a (listof symbol) (the non-terminals), A is a (listof symbol) (the
;; alphabet), R is a (listof rrule), and S is a symbol (starting symbol)
(define/contract (make-rg nts sigma delta state
                          #:accepts [accepts '()]
                          #:rejects [rejects '()])
  make-rg/c
  (make-unchecked-rg nts sigma delta state)
  )

;; singleton-regexp: string --> singleton-regexp
;; purpose: Given a lowercase Roman alphabet character, constructs a singleton
;;          regular expression for that letter.
(define/contract (singleton-regexp a)
  singleton-regexp/c
  (make-unchecked-singleton a))

;; concat-regexp: regexp regexp --> concat-regexp
;; purpose: Constructs the regular expression whose language contains words
;;          that are the concatenation of one word from L(a) and a word from L(b).
(define/contract (concat-regexp
                  a
                  b
                  #:sigma [sigma '()]
                  #:pred [pred (lambda (x) #t)]
                  #:gen-cases [gen-cases 10]
                  #:in-lang [in-lang '()]
                  #:not-in-lang [not-in-lang '()]
                  )
  concat-regexp/c
  (make-unchecked-concat a b))

;; union-regexp: regexp regexp --> union-regexp
;; purpose: Constructs a regular expression whose language contains all the words
;;          from L(a) and L(b).
(define/contract (union-regexp
                  a
                  b
                  #:sigma [sigma '()]
                  #:pred [pred (lambda (x) #true)]
                  #:gen-cases [gen-cases 10]
                  #:in-lang [in-lang '()]
                  #:not-in-lang [not-in-lang '()]
                  )
  union-regexp/c
  (make-unchecked-union a b)
  )

;; kleenestar-regexp: regexp --> kleenestar-regexp
;; purpose: Constructs a regular expression who language contains any word
;;          that is constructed by concatenating zero or more words from L(a).
(define/contract (kleenestar-regexp
                  a
                  #:sigma [sigma '()]
                  #:pred [pred (lambda (x) #true)]
                  #:gen-cases [gen-cases 10]
                  #:in-lang [in-lang '()]
                  #:not-in-lang [not-in-lang '()]
                  )
  kleenestar-regexp/c
  (make-unchecked-kleenestar a))
