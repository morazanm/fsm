#lang racket

(require
  rackunit
  "../test-helpers.rkt"
  "../test-machine.rkt"
  "../../fsm-main.rkt"
  "../../GraphViz/lib.rkt"
  "../../GraphViz/render-graph.rkt")

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
(module+ test
  
  (check-eq-snapshot-graph
   (fsa->graph a* 0)
   "a_star")

  (check-eq-snapshot-graph
   (fsa->graph pda-numa=numb 0)
   "pda_numa_numb")
  
  (check-eq-snapshot-graph
   (fsa->graph FSM 0)
   "FSM")
  
  (check-eq-snapshot-graph
   (fsa->graph a^nb^nc^n 0)
   "a_nb_nc_n")

  ); end module+ test