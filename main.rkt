


(module fsm racket

  (require "fsm-main.rkt" "./FSM-Visualization/visualize.rkt" "./GraphViz/render-graph.rkt")

  (provide
   check-machine
   empties

   ; sm constructors
   make-dfa make-ndfa make-ndpda make-tm
   regexp->fsa ndfa->dfa fsa->regexp
   sm-rename-states 
   sm-union sm-concat sm-kleenestar sm-complement sm-intersection grammar->sm
   make-mttm

   ; sm observers
   sm-apply sm-showtransitions sm-type
   sm-states sm-sigma sm-rules sm-finals sm-start sm-gamma
   sm-accept sm-numtapes

   ;; sm graph
   sm-graph
   
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
   make-dfst

   ; visualization
   sm-visualize
   sm-marco)

  (define sm-visualize visualize)
  (define sm-marco marco)

  ) ; close module