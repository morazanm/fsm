#lang racket
(require   "../../fsm-core/private/fsa.rkt"
           "../../main.rkt")



(module+ test
  (require rackunit)
  
  ; L = a*
  (define dfa-a* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q a Q))))
  
  ;; ---- DFA TESTS ---
  (define dfa-renamests-a* (sm-rename-states (sm-states dfa-a*) dfa-a*))
  )