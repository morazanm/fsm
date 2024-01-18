; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

(module main racket

  (require rackunit
           "fsm-core/interface.rkt"
           "fsm-gviz/interface.rkt"
           "fsm-gui/interface.rkt")
  
  (provide
   (all-from-out racket)
   (all-from-out rackunit)
   (all-from-out "fsm-gui/interface.rkt")
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
   grammar-union grammar-concat grammar-kleenestar
   
   ; grammar observers
   grammar-derive grammar-type
   grammar-nts grammar-sigma grammar-rules grammar-start 

   ;grammar testers
   grammar-both-derive grammar-testequiv grammar-test

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

   ; constructor visualizations
   fsa-complement-viz fsa-union-viz fsa-ks-viz fsa-intersect-viz
   fsa-concat-viz ndfa->dfa-viz regexp->ndfa-viz ndfa->regexp-viz

   ; call graphs
   sm-callgraph

   ; some helpful functions
   los->symbol symbol->list generate-symbol symbol->fsmlos symbol-upcase

   ; constants
   EMP DEAD RIGHT LEFT LM BLANK BRANCH GOTO ARROW VAR)


  ;; sm-graph :: fsa optional(number) -> bitmap
  ;; draws a graph of the given machine and returns the bitmap so it
  ;; can be displayed in the DrRacket Terminal
  (define (sm-graph fsa #:color [color-blind-mode 0])
    (when (or (< color-blind-mode 0) (> color-blind-mode 2))
      (error 'sm-graph "Invalid color option. Must be either 0, 1, or 2. Given ~a" color-blind-mode))
    (fsa->bitmap fsa color-blind-mode))

  (define R (make-tm '(S F)
                     '(d)
                     `(((S d) (F ,RIGHT))
                       ((S ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))

  (define FBR (combine-tms (list 0
                                 R
                                 (cons BRANCH
                                       (list (list 'd (list GOTO 0))
                                             (list BLANK (list GOTO 10))))
                                 10)
                           (list 'd)))

 
  ) ; close module
