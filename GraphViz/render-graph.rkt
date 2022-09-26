#lang racket
(require "lib.rkt" "../sm-getters.rkt" "../FSM-Visualization/globals.rkt")
#| render-graph.rkt
Written by: Joshua Schappel, Sachin Mahashabde Sena Karsavran, and Isabella Felix on 4/15/20

This file contains the sm-graph function
|#

(provide
 sm-graph
 fsa->graph)


(struct fsa-adapter (states start finals rules type accept cur-state cur-rule) #:transparent)
(define (is-tm? type) (or (equal? type 'tm)
                          (equal? type 'tm-language-recognizer)))


;; state-type->node-type: symbol -> fsa-adapter -> symbol
;; determins the state type for the given state
(define (state-type->node-type state fsa)
  (cond
    [(and (is-tm? (fsa-adapter-type fsa))
          (equal? state (fsa-adapter-accept fsa))) 'accept]
    [(and (equal? state (fsa-adapter-start fsa))
          (member state (fsa-adapter-finals fsa))) 'startfinal]
    [(equal? state (fsa-adapter-start fsa)) 'start]
    [(member state (fsa-adapter-finals fsa)) 'final]
    [else 'none]))

;; fsa-states->nodes: fsa-adapter -> graph -> graph
;; Adds all the machine states to the graph
(define (fsa-states->nodes fsa graph)
  (define (fsa-state->node state graph)
    (if (equal? state (fsa-adapter-cur-state fsa))
        (add-node graph state (state-type->node-type state fsa)) ;; #:atb color-atb ;;TODO Jschappel add color blind back in 
        (add-node graph state (state-type->node-type state fsa))))
  (foldl fsa-state->node graph (fsa-adapter-states fsa)))


;; fsa-rules->edges :: fsa-adapter -> graph -> graph
;; Adds all the machines rules to the graph
(define (fsa-rules->edges fsa graph)
  (define (parse-rule rule)
    (if (not rule)
        (list #f #f)
        (match (fsa-adapter-type fsa)
          [(or 'dfa 'ndfa) (list (first rule) (last rule))]
          [_ (list (caar rule) (caadr rule))])))
  (match-define `(,start-state ,end-state) (parse-rule (fsa-adapter-cur-rule fsa)))
  (define (fsa-rule->edge rule graph)
    (match-define `(,start ,end) (parse-rule rule))
    (define rule-label (if (or (equal? (fsa-adapter-type fsa) 'dfa) (equal? (fsa-adapter-type fsa) 'ndfa))
                           (cadr rule)
                           rule))
    (if (and (equal? start start-state) (equal? end end-state))
        (add-edge graph rule-label start end #:atb HIGHLIGHT-EDGE)
        (add-edge graph rule-label start end)))
  (foldl fsa-rule->edge graph (fsa-adapter-rules fsa)))


;; fsa->graph :: machine -> graph
(define (fsa->graph fsa color-blind-mode)
  (define adapter (fsa-adapter (sm-states fsa)
                               (sm-start fsa)
                               (sm-finals fsa)
                               (sm-rules fsa)
                               (sm-type fsa)
                               (if (is-tm? (sm-type fsa)) (sm-accept fsa) #f)
                               #f
                               #f))     
  (fsa-rules->edges adapter
                    (fsa-states->nodes adapter
                                       (create-graph 'G #:color color-blind-mode))))




;; sm-graph: machine -> Image
;; Purpose: draws the Graphviz graph of the geven machine
(define (sm-graph machine #:color [color-blind-mode 0])
  (graph->bitmap (fsa->graph machine color-blind-mode) (current-directory) "vizTool"))