; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

(module main racket

  (require rackunit
           "fsm-core/interface.rkt"
           "fsm-gui/interface.rkt"
           "visualizations/viz-sm-constructors/viz-complement.rkt"
           "visualizations/viz-sm-constructors/viz-concat.rkt"
           "visualizations/viz-sm-constructors/viz-intersection.rkt"
           "visualizations/viz-sm-constructors/viz-kleenestar.rkt"
           "visualizations/viz-sm-constructors/viz-ndfa2dfa.rkt"
           "visualizations/viz-sm-constructors/viz-ndfa2regexp.rkt"
           "visualizations/viz-sm-constructors/viz-regexp2ndfa.rkt"
           "visualizations/viz-sm-constructors/viz-union.rkt"
           "sm-graph.rkt")
  
  (provide
   (all-from-out racket)
   (all-from-out rackunit)
   (all-from-out "fsm-gui/interface.rkt")
   sm-visualize
   check-machine
   empties


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

   ;;fsa minimization
   minimization-viz
   
   ; sm constructors
   sm-test-invs
   sm-quickcheck
   make-dfa make-ndfa make-ndpda make-tm
   regexp->fsa ndfa->dfa fsa->regexp
   sm-rename-states 
   sm-union sm-concat sm-kleenestar sm-complement sm-intersection grammar->sm
   make-mttm
   fsa-minimize

   ; sm observers
   sm-apply sm-showtransitions sm-type
   sm-states sm-sigma sm-rules sm-finals sm-start sm-gamma
   sm-accept sm-numtapes

   ;; sm graph
   sm-graph ctm-graph
    
   ; sm testers
   sm-sameresult? sm-testequiv? sm-test

   ; ctm constructor and observer
   combine-tms ctm-run

   ; grammar constructors
   make-rg make-cfg make-csg
   make-grammar ;; new name for make-csg
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

   ; viz constructors
   complement-viz concat-viz intersection-viz kleenestar-viz
   ndfa2dfa-viz ndfa2regexp-viz regexp2ndfa-viz union-viz

   ; computation graphs
   sm-cmpgraph

   grammar-viz

   ; ctm-viz
   ctm-viz

   ;; FSM Unit Testing
   check-derive? check-not-derive?
   check-gen? check-not-gen?
   check-accept? check-reject?
   check-inv-holds? check-inv-fails?
   ;check-in-lang? check-not-in-lang?

   ; some helpful functions
   los->symbol symbol->list generate-symbol symbol->fsmlos symbol-upcase
   gen-state gen-nt

   ; constants
   EMP DEAD RIGHT LEFT LM BLANK BRANCH GOTO ARROW VAR

   sm-viz

   ; invariant testing
   sm-test-invs
   )
  ) ; close module
