; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

#lang racket/base

(require
  "private/fsa.rkt"
  "private/ndfa-constructors.rkt"
  "private/grammar-operations.rkt"
  "private/sm-operations.rkt"
  "private/cfg-struct.rkt"
  "private/cfg-constructors.rkt"
  "private/pda.rkt"
  "private/pda-constructors.rkt"
  "private/regular-grammar.rkt"
  "private/rg-constructors.rkt"
  "private/csg.rkt"
  "private/urg-constructors.rkt"
  "private/tm.rkt"
  "private/tm-constructors.rkt"
  "private/mttm-constructors.rkt"
  "private/regexp.rkt"
  "private/regexp-constructors.rkt"
  "private/constants.rkt"
  "private/misc.rkt"
  "private/state.rkt"
  "private/sm-getters.rkt"
  "private/grammar-getters.rkt"
  "private/abstract-predicate.rkt"
  "private/sm-apply.rkt"
  "private/sm-test/sm-quickcheck.rkt"
  "private/sm-test/sm-test-invs.rkt"
  "private/callgraphs/callgraphs-ndfa.rkt"
  "private/callgraphs/callgraphs-pda.rkt"
  "private/callgraphs/callgraphs-tm.rkt"
  "private/callgraphs/callgraphs-mttm.rkt"
  "../visualizations/viz-sm/viz-ctm.rkt"
  "../visualizations/viz-sm/sm-viz.rkt"
  "../visualizations/viz-grammar-constructors/rg-viz.rkt"
  "../visualizations/viz-grammar-constructors/cfg-viz.rkt"
  "../visualizations/viz-grammar-constructors/csg-viz.rkt"
  "private/fsmunit/interface.rkt"
  "private/cfe-constructors/construct-cfe-macro.rkt"
  "private/cfe-constructors/context-free-expressions-constructors.rkt"
  (rename-in "private/machine-minimization/fsa-minimization.rkt"
             (minimize-dfa fsa-minimize))
  "../visualizations/viz-sm-constructors/viz-minimization.rkt"
  )
  
(provide
 sm-test-invs
 sm-quickcheck
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
 make-grammar ;; a rename for make-csg
 sm->grammar grammar-rename-nts 
 grammar-union grammar-concat grammar-kleenestar
   
 ; grammar observers
 grammar-derive grammar-derive? grammar-type
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
 check-gen? check-not-gen?
 check-accept? check-reject?
 check-inv-holds? check-inv-fails?
 ;check-in-lang? check-not-in-lang?
 
 ;; Grammar visualizations
 grammar-viz

 ;;sm-viz
 sm-viz

 ;;fsa minimization
 fsa-minimize minimization-viz

 ;;cfexp constructors
 make-cfe null-cfexp empty-cfexp singleton-cfexp
 concat-cfexp union-cfexp kleenestar-cfexp

 ; cfexp observers
 cfg->cfe cfe->cfg
 cfexp? null-cfexp? empty-cfexp? singleton-cfexp?
 concat-cfexp? union-cfexp? kleenestar-cfexp?
 gen-cfexp-word pick-cfexp singleton-cfexp-a
 union-cfexp-cfes concat-cfexp-cfes kleenestar-cfexp-c1
 #;pda->cfe #;cfe->pda #;printable-cfexp
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
           (computation-diagram-tm M w (if (null? headpos) 0 (car headpos)) c p)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (computation-diagram-mttm M w (if (null? headpos) 0 (car headpos)) c p)]
          [else (error "Unknown machine type given to sm-cmpgraph.")])))