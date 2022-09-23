#lang racket
(require "lib.rkt" "../sm-getters.rkt" "../FSM-Visualization/globals.rkt")
#| render-graph.rkt
Written by: Joshua Schappel, Sachin Mahashabde Sena Karsavran, and Isabella Felix on 4/15/20

This file contains the sm-graph function
|#

(provide
 sm-graph
 fsa->graph)

;; state-type->node-type: symbol -> machine -> symbol
;; determins the state type for the given state
(define (state-type->node-type state fsa)
  (define is-tm? (or (equal? (sm-type fsa) 'tm)
                     (equal? (sm-type fsa) 'tm-language-recognizer)))
  (cond
    [(and is-tm? (equal? state (sm-accept fsa))) 'accept]
    [(and (equal? state (sm-start fsa))
          (member state (sm-finals fsa))) 'startfinal]
    [(equal? state (sm-start fsa)) 'start]
    [(member state (sm-finals fsa)) 'final]
    [else 'none]))

;; fsa-states->nodes: machine -> boolean -> graph -> graph
;; Adds all the machine states to the graph
(define (fsa-states->nodes fsa is-cur-state? graph)
  (define (fsa-state->node state graph)
    (if (and is-cur-state?)
        (add-node graph state (state-type->node-type state fsa)) ;; #:atb color-atb ;;TODO Jschappel add color blind back in 
        (add-node graph state (state-type->node-type state fsa))))
  (foldl fsa-state->node graph (sm-states fsa)))



;; fsa-rules->edges :: machine -> symbol | boolean -> symbol | boolean -> graph -> graph
;; Adds all the machines rules to the graph
(define (fsa-rules->edges fsa start-state end-state graph)
  (define type (sm-type fsa))
  (define (fsa-rule->edge rule graph)
    (match-define `(,start ,end) (match type
                                   [(or 'dfa 'ndfa) (list (first rule) (last rule))]
                                   [_ (list (caar rule) (caadr rule))]))
    (define rule-label (if (or (equal? type 'dfa) (equal? type 'ndfa))
                           (cadr rule)
                           rule))
    (if (and (equal? start start-state) (equal? end end-state))
        (add-edge graph rule-label start end #:atb HIGHLIGHT-EDGE)
        (add-edge graph rule-label start end)))
  (foldl fsa-rule->edge graph (sm-rules fsa)))


;; fsa->graph :: machine -> graph
(define (fsa->graph machine color-blind-mode cur-rule cur-state)
  (fsa-rules->edges machine
                    #f
                    #f
                    (fsa-states->nodes machine
                                       #f
                                       (create-graph 'G #:color color-blind-mode))))




;; sm-graph: machine -> Image
;; Purpose: draws the Graphviz graph of the geven machine
(define (sm-graph machine #:color [color-blind-mode 0])
  (graph->bitmap (fsa->graph machine color-blind-mode #f #f) (current-directory) "vizTool"))