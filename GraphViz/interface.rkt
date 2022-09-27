#lang racket
(require "private/adapter.rkt"
         "private/lib.rkt"
         "../sm-getters.rkt"
         "../FSM-Visualization/structs/machine.rkt"
         "../FSM-Visualization/structs/state.rkt")

(provide
 (all-from-out  "private/lib.rkt")
 (contract-out
  [fsa->graph (-> any/c colorblind-opt? graph?)]
  [machine->graph (-> machine?
                      colorblind-opt?
                      symbol?
                      symbol?
                      symbol?
                      graph?)]))


(define (is-tm-lang-rec? type) (equal? type 'tm-language-recognizer))

;; fsa->graph :: fsa -> graph
;; converts the fsa to a graphviz graph
(define (fsa->graph fsa color-blind-mode)
  (define adapter (fsa-adapter (sm-states fsa)
                               (sm-start fsa)
                               (sm-finals fsa)
                               (sm-rules fsa)
                               (sm-type fsa)
                               (if (is-tm-lang-rec? (sm-type fsa)) (sm-accept fsa) #f)
                               #f
                               #f
                               "transparent"))     
  (fsa-adapter->graph adapter color-blind-mode))


;; machine->graph :: machine -> symbol -> symbol -> symbol -> graph
;; converts the machine to a graphviz graph
(define (machine->graph machine color-blind-mode cur-rule cur-state cur-state-color)
  (define adapter (fsa-adapter
                   (map (lambda (s) (fsm-state-name s)) (machine-state-list machine))
                   (machine-start-state machine)
                   (machine-final-state-list machine)
                   (machine-rule-list machine)
                   (machine-type machine)
                   (if (is-tm-lang-rec? (machine-type machine)) (lang-rec-machine-accept-state machine) #f)
                   cur-state
                   cur-rule
                   cur-state-color))
  (fsa-adapter->graph adapter color-blind-mode))