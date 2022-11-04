#lang racket

(require
 rackunit
 "../test-machine.rkt"
 "../../fsm-main.rkt"
 "../../sm-getters.rkt"
 "../../GraphViz/interface.rkt"
 "../../FSM-Visualization/globals.rkt"
 "../../FSM-Visualization/structs/state.rkt"
 "../../FSM-Visualization/structs/machine.rkt")

;; check-eq-snapshot-graph :: graph -> string -> bool (optional) -> bool
;; checks if the graph is equal to the given file. If rebuild is passed with true
;; then the file is updated to have the current graph
;; All files are saved in <current directory>/dotfiles
(define (check-eq-snapshot-graph graph filename #:rebuild [rebuild #f])
  (define compare-dir (build-path (current-directory) "dotfiles"))
  (if rebuild
      (displayln (format "Rebuilt: ~a" (graph->dot graph compare-dir filename)))
      (let [(actual-graph (string-split (graph->str graph) "\n"))]
        (call-with-input-file (build-path compare-dir (format "~a.dot" filename))
          (λ (port)
            (define lines (sequence->list (in-lines port)))
            (for ([expected-line lines]
                  [actual-line actual-graph])
              (check-equal? (string-trim actual-line)
                            (string-trim expected-line)))
            (check-equal? (length lines)
                          (length actual-graph)))))))

;; Converts a fsa to a viztool machine
(define (fsa->machine fsa)
  (match (sm-type fsa)
    [(or 'dfa 'ndfa) (machine (map (lambda (s) (fsm-state s #f #f)) (sm-states fsa))
                              (sm-start fsa)
                              (sm-finals fsa)
                              (sm-rules fsa)
                              '()
                              (sm-sigma fsa)
                              (sm-type fsa))]
    ['pda (pda-machine (map (lambda (s) (fsm-state s #f #f)) (sm-states fsa))
                       (sm-start fsa)
                       (sm-finals fsa)
                       (sm-rules fsa)
                       '()
                       (sm-sigma fsa)
                       (sm-type fsa)
                       (sm-gamma fsa))]
    ['tm (tm-machine (map (lambda (s) (fsm-state s #f #f)) (sm-states fsa))
                     (sm-start fsa)
                     (sm-finals fsa)
                     (sm-rules fsa)
                     `(,LM)
                     (sm-sigma fsa)
                     (sm-type fsa)
                     0)]
    [_ 'tm-language-recognizer (lang-rec-machine (map (lambda (s) (fsm-state s #f #f)) (sm-states fsa))
                                                 (sm-start fsa)
                                                 (sm-finals fsa)
                                                 (sm-rules fsa)
                                                 `(,LM)
                                                 (sm-sigma fsa)
                                                 (sm-type fsa)
                                                 0
                                                 (sm-accept fsa))]))


(module+ test
  
  (check-eq-snapshot-graph
   (fsa->graph a*a 0)
   "a_star_a")

  (check-eq-snapshot-graph
   (fsa->graph pda-numa=numb 0)
   "pda_numa_numb")
  
  (check-eq-snapshot-graph
   (fsa->graph FSM 0)
   "FSM")
  
  (check-eq-snapshot-graph
   (fsa->graph a^nb^nc^n 0)
   "a_nb_nc_n")

  (check-eq-snapshot-graph
   (machine->graph (fsa->machine a*a) 0 '(A a F) 'A 'pass)
   "a_star_a_vizTool_pass")

  (check-eq-snapshot-graph
   (machine->graph (fsa->machine a*a) 1 '(A a F) 'A 'pass)
   "a_star_a_vizTool_pass_cb1")

  (check-eq-snapshot-graph
   (machine->graph (fsa->machine pda-numa=numb) 0 '((S ε ε) (M ε)) 'S 'none)
   "pda_numa_numb_vizTool_none")
  
  (check-eq-snapshot-graph
   (machine->graph (fsa->machine pda-numa=numb) 0 '((S ε ε) (M ε)) 'S 'pass)
   "pda_numa_numb_vizTool_pass")

  (check-eq-snapshot-graph
   (machine->graph (fsa->machine pda-numa=numb) 0 '((S ε ε) (M ε)) 'S 'fail)
   "pda_numa_numb_vizTool_fail")

  (check-eq-snapshot-graph
   (machine->graph (fsa->machine a^nb^nc^n) 0 '((E @) (E R)) 'E 'pass)
   "a_nb_nc_n_vizTool")
  ); end module+ test
