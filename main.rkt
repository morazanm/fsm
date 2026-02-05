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
           "sm-graph.rkt"
           "quickcheck-stuff/quickcheck-invariants.rkt"
           "quickcheck-stuff/sm-test-invs.rkt"
           "quickcheck-stuff/sm-test-invs-pda.rkt")
  
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
   ;check-gen? check-not-gen?
   check-accept? check-reject?
   ;check-in-lang? check-not-in-lang?

   ; some helpful functions
   los->symbol symbol->list generate-symbol symbol->fsmlos symbol-upcase
   gen-state gen-nt

   ; constants
   EMP DEAD RIGHT LEFT LM BLANK BRANCH GOTO ARROW VAR

   sm-viz

   ;machine testing
   sm-test-invs sm-quickcheck)


  ;; sm-graph :: fsa optional(number) -> bitmap
  ;; draws a graph of the given machine and returns the bitmap so it
  ;; can be displayed in the DrRacket Terminal
  #;(define (sm-graph fsa #:color [color-blind-mode 0])
      (when (or (< color-blind-mode 0) (> color-blind-mode 2))
        (error 'sm-graph "Invalid color option. Must be either 0, 1, or 2. Given ~a" color-blind-mode))
      (fsa->bitmap fsa color-blind-mode))

 ;; fsa . (list state (word -> boolean)) -> (listof (listof symbol))
 ;; Purpose: To return a list of the invariants that don't hold and the words that cause it not to hold
  (define (sm-test-invs machine #:rep-limit [rep-limit 1] #:max-path-length [max-path-length 12] . loi)
    (cond [(or (eq? 'dfa (sm-type machine))
               (eq? 'ndfa (sm-type machine)))
           (sm-test-invs-fsa machine rep-limit loi)]
          [(eq? 'pda (sm-type machine))
           (sm-test-invs-pda machine max-path-length loi)]
          [else (error "Turing machines and mttm testing is not implemented yet, stay tuned :)" )]))


  ;; machine [natnum] [natnum] . (listof (state invariant)) -> void throws error 
  ;; Purpose: To quickcheck the invariants of the states of the given machine
  (define (sm-quickcheck machine #:num-tests [tests 300] #:max-length-kleenes [num-kleenes 20] . state-invs-pairs)
    (cond [(or (eq? (sm-type machine) 'dfa)
               (eq? (sm-type machine) 'ndfa))
           (fsa-quickcheck-invs machine state-invs-pairs tests num-kleenes)]
          [else (error "quickcheck testing not yet implemented for pdas, Turing machines, and mttms, stay tuned :)")]))


                                
(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))

  
(define NO-AA
  (make-unchecked-dfa
   '(S A B R)
   '(a b)
   'S
   '(S A B)
   '((S a A) (S b B)
             (B a A) (B b B)
             (A a R) (A b B)
             (R a R) (R b R))
   'no-dead))

(define PROHIBITED-PATTERN '(a a))



;; defining the state invarients

;; S: nothing detected, start and final state
;; A: a has been detected, final state
;; B: b has been detected, final state
;; R: aa has been detected


;; word -> Boolean
;; Purpose: Determine if the consumed input contains PROHIBITED PATTERN
;; Assume: |ci| >= 2
(define (R-INV ci)
  (not(contains? ci PROHIBITED-PATTERN)))            ;<-- purposely broken for testing




#;(define (R-INV ci)
  (contains? ci PROHIBITED-PATTERN))  ;<- correct version


;; tests for R-INV
;(check-equal? (R-INV '(a)) #f)
;(check-equal? (R-INV '(a a)) #t)
;(check-equal? (R-INV '(a a b)) #t)


;; word -> Boolean
;; Purpose: To determine if the consumed input ends with a
;;          and does not contain the prohibited input
#;(define (A-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(a))    ;<-- broken 
       (contains? ci PROHIBITED-PATTERN)))

(define (A-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(a))    ;<-- not broken 
       (not (contains? ci PROHIBITED-PATTERN))))


;;tests for A-INV
#;(check-equal? (A-INV '(a)) #t)
(check-equal? (A-INV '(b)) #f)
#;(check-equal? (A-INV '(a b a)) #t)


;; word -> Boolean
;; Purpose: Determine if NO-AA shoule be in B
#;(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(b))
       (not (contains? ci PROHIBITED-PATTERN))))    ;<-- not broken

(define (B-INV ci)
  (and (equal? (drop ci (- (length ci) 1)) '(b))
       (contains? ci PROHIBITED-PATTERN)))          ; <- broken


#|
;;tests for B-INV
(check-equal? (B-INV '(b)) #t)
(check-equal? (B-INV '(a b)) #t)
(check-equal? (B-INV '(a b b)) #t)
(check-equal? (B-INV '(a a b b)) #f)
(check-equal? (B-INV '(a a a b b a a)) #f)      
(check-equal? (B-INV '(a a b a b b a b b)) #f)
|#


;; word -> Boolean
;; Purpose: Determine if NO-AA should be in S
(define (S-INV ci)
  (or (= (length ci) 0)
      (and (not (contains? ci PROHIBITED-PATTERN))
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (not (equal? (drop ci (- (length ci) 2))    ; <- not broken
                            '(a a)))))))


#;(define (S-INV ci)
  (or (= (length ci) 0)
      (and (contains? ci PROHIBITED-PATTERN)
           (eq? (last ci) 'b)
           (or (= (length ci) 1)
               (equal? (drop ci (- (length ci) 2)    ; <- broken
                            '(a a)))))))



  
  
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
