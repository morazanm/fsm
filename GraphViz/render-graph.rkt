#lang racket
(require "lib.rkt" "../fsm-main.rkt")
#| render-graph.rkt
Written by: Joshua Schappel, Sachin Mahashabde Sena Karsavran, and Isabella Felix on 4/15/20

This file contains the sm-graph function
|#

(provide
 states->nodes
 rules->edges
 sm-graph)



; states->nodes: (listof symbols) symbol (listof symbol) graph -> (listof nodes) 
; Purpose: Create a list of node structures given a list of state symbols
(define (states->nodes los S lof G)
  (map (lambda (x) (add-node
                    G
                    x
                    (cond [(and (equal? x S)
                                (member x lof)) 'startfinal]
                          [(equal? x S) 'start]
                          [(member x lof) 'final]
                          [else 'none]))) los))


;; rules->edges: (listof rules) graph -> (listof edges)
;; Purpose: Creates a list of edges when given a list of rules
(define (rules->edges lor G)
  (map (lambda (rule) (add-edge G
                                (cadr rule)
                                (car rule)
                                (last rule))) lor))


;; sm-graph: machine -> Image
;; Purpose: draws the Graphviz graph of the geven machine
(define (sm-graph machine)
  (let ((g (create-graph 'G)))
    (begin
      (states->nodes (sm-getstates machine)
                     (sm-getstart machine)
                     (sm-getfinals machine)
                     g)
      (rules->edges (sm-getrules machine)
                    g)
      (graph->bitmap g "graph.dot" "graph"))))