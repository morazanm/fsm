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
   check-machine
   empties

   ; cfexp
   make-cfe cfg->cfe cfe->cfg pda->cfe cfe->pda #;printable-cfexp

   ; sm constructors
   sm-test-invs
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

   ;;sm-viz
   sm-viz)


  ;; sm-graph :: fsa optional(number) -> bitmap
  ;; draws a graph of the given machine and returns the bitmap so it
  ;; can be displayed in the DrRacket Terminal
  #;(define (sm-graph fsa #:color [color-blind-mode 0])
      (when (or (< color-blind-mode 0) (> color-blind-mode 2))
        (error 'sm-graph "Invalid color option. Must be either 0, 1, or 2. Given ~a" color-blind-mode))
      (fsa->bitmap fsa color-blind-mode))

  (define aab* (make-unchecked-ndfa '(W X Y)
                                    '(a b)
                                    'W
                                    '(Y)
                                    '((W a X)
                                      (X a Y)
                                      (Y b Y))))

  (define EQABC2
    (make-mttm
     '(S Y C D E F G)
     '(a b c)
     'S
     '(Y)
     (list
      (list (list 'S (list BLANK BLANK BLANK BLANK))
            (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
      (list (list 'C (list 'a BLANK BLANK BLANK))
            (list 'D (list 'a 'a BLANK BLANK)))
      (list (list 'D (list 'a 'a BLANK BLANK))
            (list 'C (list RIGHT RIGHT BLANK BLANK)))
      (list (list 'C (list 'b BLANK BLANK BLANK))
            (list 'E (list 'b BLANK 'b BLANK)))
      (list (list 'E (list 'b BLANK 'b BLANK))
            (list 'C (list RIGHT BLANK RIGHT BLANK)))
      (list (list 'C (list 'c BLANK BLANK BLANK))
            (list 'F (list 'c BLANK BLANK 'c)))
      (list (list 'F (list 'c BLANK BLANK 'c))
            (list 'C (list RIGHT BLANK BLANK RIGHT)))
      (list (list 'C (list BLANK BLANK BLANK BLANK))
            (list 'G (list BLANK LEFT LEFT LEFT)))
      (list (list 'G (list BLANK BLANK BLANK BLANK))
            (list 'Y (list BLANK BLANK BLANK BLANK)))
      (list (list 'G (list BLANK 'a 'b 'c))
            (list 'G (list BLANK LEFT LEFT LEFT))))
     4
     'Y))

  (define M (make-dfa 
	'(B A S) 
	'(a b) 
	'S 
	'(B) 
	'((B a S) (A b A) (B b B) (A a B) (S b A) (S a B))))

  ;(sm-graph EQABC2)
  ;(sm-cmpgraph EQABC2 `(,LM ,BLANK a a b c c b) 1)
  ;(sm-cmpgraph EQABC2 `(,LM ,BLANK a a b c c) 1)
 
  ) ; close module
