#lang racket

#| lib.rkt
Written by: Joshua Schappel, Sena Karsavran, and Isabella Felix on 4/15/20

This file contains the fsm-graphviz library used to render the graph
  representation onto the Visualization tool.
|#

(provide (struct-out graph)
         (struct-out node)
         (struct-out edge)
         create-graph
         add-node
         add-edge
         create-node
         create-edge
         render-graph) 
         

; A graph is represented as a structure with three elements:
; name is a symbol used to represent the name of the graph
; node-list is a list of node structures
; edge-list is a list of edge structures 
(struct graph ([name]
               [node-list #:mutable]
               [edge-list #:mutable]))

; A node is a structure with four elements that represents a single state in FSA
; name is a symbol used to represent the name of a graphviz node
; value is symbol used to represent the name of a state
; color is a symbol used to represent a graphviz color
;   https://www.graphviz.org/doc/info/colors.html
; type is a symbol used to represent if a state is a starting, final, or accepting state 
(struct node ([name]
              [value]
              [color]
              [type]))

; An edge is a structure that represents a transition from one node to another
; name is a symbol used to represent the name of the edge
; color is a symbol used to represent a graphviz color
;   https://www.graphviz.org/doc/info/colors.html
; start-node is a node-name (symbol)
; end-node is a node-name (symbol) 
(struct edge ([name]
              [color]
              [start-node #:mutable]
              [end-node #:mutable]))



;; create-graph: symbol list-of-node list-of-edge -> graph
;; Purpose: Creates a Graph
(define (create-graph name #:nodes [nodes '()] #:edges [edges '()])
  (graph name nodes edges))


;; add-node: graph node -> NONE
;; Purpose: adds a node to the given graph
(define (add-node graph node)
  (set-graph-node-list! graph
                        (cons node (graph-node-list graph))))


;; add-edge: graph edge -> NONE
;; Purpose: adds an edge to the graph
;; IMPORTANT: This function assumes that the node exists in the graph structure
(define (add-edge graph edge)
  (set-graph-edge-list! graph
                        (cons edge (graph-edge-list graph))))






;; create-node: symbol symbol symbol symbol -> node
;; Purpose: Creates a node
;; Acceptable types:
;;    start           => A start state
;;    startfinal      => A start and final state
;;    final           => A final state
;;    accept          => An accepting state (lang recs)
;;    none (default)  => just a plain old state 
(define (create-node name value color #:type [type 'none])
  (if (or (equal? 'start type)
          (equal? 'none type)
          (equal? 'final type)
          (equal? 'accept type)
          (equal? 'startfinal type))
      (node name value color type)
      (error "Invalid node type. A node must be one of the following symbols:\n'start\n'startfinal\n'final\n'accept\n'none")))


;; create-edge symbol double(<=1) symbol symbol -> edge
;; Purpose: Creates an edge
(define (create-edge val color start-node end-node)
  (edge val color start-node end-node))




;; render-graph: graph string -> NONE
;; Purpose: writes graph to the specified file
(define (render-graph graph path)
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (displayln (format "digraph ~s {" (graph-name graph)) out)
      (displayln "    rankdir=\"LR\";" out)
      (render-nodes (graph-node-list graph) out)
      (render-edges (graph-edge-list graph) out)
      (displayln "}" out))))


;; render-nodes list-of-nodes write-buffer -> NONE
(define (render-nodes lon port)
  (cond
    [(empty? lon) (void)]
    [else
     (letrec ((node (car lon))
              ;; determin-shape: node -> symbol
              ;; Purpose: determins the shape of the node
              (determin-shape (lambda (node)
                                (case (node-type node)
                                  [(final) 'doublecircle]
                                  [(startfinal) 'doublecircle]
                                  [(accept) 'doubleoctagon]
                                  [else 'circle])))

              ;; determin-color: node -> symbol
              ;; Purpose: determins the color of the node
              (determin-color (lambda (node)
                                (case (node-type node)
                                  [(start) 'green]
                                  [(startfinal) 'green]
                                  [else 'black]))))                        
       (begin
         (displayln (format "    ~s [label=\"~s\", shape=\"~s\", color=\"~s\"];"
                            (node-name node)
                            (node-value node)
                            (determin-shape node)
                            (determin-color node))
                    port)
         (render-nodes (cdr lon) port)))]))


;; render-edges list-of-edges write-buffer -> NONE
(define (render-edges loe port)
  (cond
    [(empty? loe) (void)]
    [else
     (let ((edge (car loe)))
       (begin
         (displayln (format "    ~s -> ~s [label=\"~s\", color=\"~s\"];"
                            (edge-start-node edge)
                            (edge-end-node edge)
                            (edge-name edge)
                            (edge-color edge))
                    port)
         (render-edges (cdr loe) port)))]))
  


#|

; L(KLEENESTAR-abUaba) = (abUaba)*
(define testGraph (create-graph 'test))
(add-node testGraph (create-node 'Q0 'Q-0 'green #:type 'startfinal))
(add-node testGraph (create-node 'Q1 'Q-1 'black))
(add-node testGraph (create-node 'Q2 'Q-2 'black #:type 'accept))
(add-node testGraph (create-node 'Q3 'Q-3 'black))
(add-node testGraph (create-node 'Q4 'Q-4 'black))
(add-node testGraph (create-node 'Q5 'Q-5 'black #:type 'final))

(add-edge testGraph (create-edge 'a 'black 'Q0 'Q1))
(add-edge testGraph (create-edge 'b 'red 'Q1 'Q2))
(add-edge testGraph (create-edge 'a 'black 'Q2 'Q3))
(add-edge testGraph (create-edge EMP 'black 'Q3 'Q0))
(add-edge testGraph (create-edge 'a 'black 'Q0 'Q4))
(add-edge testGraph (create-edge 'b 'black 'Q4 'Q5))
(add-edge testGraph (create-edge EMP 'black 'Q5 'Q0))
(add-edge testGraph (create-edge 'b 'black 'Q5 'Q4))
(add-edge testGraph (create-edge 'c 'black 'Q5 'Q4))


(render-graph testGraph "testGraph.dot")|#
