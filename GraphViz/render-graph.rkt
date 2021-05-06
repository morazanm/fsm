#lang racket
(require "lib.rkt" "../sm-getters.rkt" "../FSM-Visualization/globals.rkt")
#| render-graph.rkt
Written by: Joshua Schappel, Sachin Mahashabde Sena Karsavran, and Isabella Felix on 4/15/20

This file contains the sm-graph function
|#

(provide
 sm-graph
 states->nodes
 rules->edges
 fsa->graph)




; states->nodes: (listof symbols) symbol (listof symbol) graph -> NONE 
; Purpose: for every state in the los a node is add to the graph
(define (states->nodes los S lof G #:cur-state[cur-state #f] #:color[color #f])
  (let ((type (lambda (x)
                (cond [(and (equal? x S)(member x lof))
                       'startfinal]
                      [(equal? x S) 'start]
                      [(member x lof) 'final]
                      [else 'none]))))
    (cond
      [(empty? los) (void)]
      [else (begin 
              (if (and color (equal? (car los) cur-state))
                  (add-node G (car los) (type (car los)) #:atb color)
                  (add-node G (car los) (type (car los))))
              (states->nodes (cdr los) S lof G #:cur-state cur-state #:color color))])))

;; rules->edges: (listof rules) symbol graph symbol symbol string -> (listof edges)
;; Purpose: Creates a list of edges when given a list of rules
(define (rules->edges lor m-type G cur-start cur-end color)
  (letrec ((get-start (lambda (rule)
                        (case m-type
                          [(dfa) (car rule)]
                          [(ndfa) (car rule)]
                          [else (caar rule)])))
                          
           (get-end (lambda (rule)
                      (case m-type
                        [(dfa) (last rule)]
                        [(ndfa) (last rule)]
                        [else (caadr rule)])))
           ;; determins if a esge is highlighted or not
           (make-edge (lambda (rule)
                        (if (and (equal? (get-start rule) cur-start) (equal? (get-end rule) cur-end))
                            (add-edge G
                                      (if (or (equal? m-type 'dfa) (equal? m-type 'ndfa))
                                          (cadr rule)
                                          rule)
                                      (get-start rule)
                                      (get-end rule)
                                      #:atb HIGHLIGHT-EDGE)
                            (add-edge G
                                      (if (or (equal? m-type 'dfa) (equal? m-type 'ndfa))
                                          (cadr rule)
                                          rule)
                                      (get-start rule)
                                      (get-end rule))))))                
    (cond
      [(empty? lor) (void)]
      [else
       (let ((rule (car lor)))
         (begin
           (make-edge rule)
           (rules->edges (cdr lor) m-type G cur-start cur-end color)))])))


;; fsa->graph :: machine -> graph
(define (fsa->graph machine color-blind)
  (let ((g (create-graph 'G #:color color-blind)))
    (begin
      (states->nodes (sm-getstates machine)
                     (sm-getstart machine)
                     (sm-getfinals machine)
                     g)
      (rules->edges (sm-getrules machine) (sm-type machine) g "$NULL" "$NULL" "black")
      g)))


;; sm-graph: machine -> Image
;; Purpose: draws the Graphviz graph of the geven machine
(define (sm-graph machine #:color [color-blind 0])
  (graph->bitmap (fsa->graph machine color-blind)))