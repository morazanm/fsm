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


#|
; states->nodes: (listof symbols) symbol (listof symbol) graph -> (listof nodes) 
; Purpose: Create a list of node structures given a list of state symbols
(define (states->nodes los S lof G)
  (apply (lambda (x) (add-node
                      G
                      x
                      (cond [(and (equal? x S)
                                  (member x lof)) 'startfinal]
                            [(equal? x S) 'start]
                            [(member x lof) 'final]
                            [else 'none]))) los))
|#

; states->nodes: (listof symbols) symbol (listof symbol) graph -> NONE 
; Purpose: for every state in the los a node is add to the graph
(define (states->nodes los S lof G)
  (let ((type (lambda (x)
                (cond [(and (equal? x S)(member x lof))
                       'startfinal]
                      [(equal? x S) 'start]
                      [(member x lof) 'final]
                      [else 'none]))))
    (cond
      [(empty? los) (void)]
      [else (begin
              (add-node G (car los) (type (car los)))
              (states->nodes (cdr los) S lof G))])))

;; rules->edges: (listof rules) symbol graph -> (listof edges)
;; Purpose: Creates a list of edges when given a list of rules
(define (rules->edges lor m-type G)
  (letrec ((get-start (lambda (rule)
                        (case m-type
                          [(dfa) (car rule)]
                          [else (caar rule)])))
                          
           (get-end (lambda (rule)
                      (case m-type
                        [(dfa) (last rule)]
                        [else (caadr rule)]))))
                       
    (cond
      [(empty? lor) (void)]
      [else
       (let ((rule (car lor)))
         (begin
           (add-edge G
                     (if (equal? m-type 'dfa)
                         (cadr rule)
                         rule)
                     (get-start rule)
                     (get-end rule))
                     (rules->edges (cdr lor) m-type G)))])))



;; sm-graph: machine -> Image
;; Purpose: draws the Graphviz graph of the geven machine
(define (sm-graph machine #:color [color-blind 0])
  (let ((g (create-graph 'G #:color color-blind)))
    (begin
      (states->nodes (sm-getstates machine)
                     (sm-getstart machine)
                     (sm-getfinals machine)
                     g)
      (rules->edges (sm-getrules machine) (sm-type machine) g)
      (graph->bitmap g "graph.dot" "graph"))))