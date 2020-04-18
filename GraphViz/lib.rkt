#lang racket
(require 2htdp/image)
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
         render-graph
         dot->png
         graph->bitmap)


;; Constants 
(define START-STATE-COLOR "forestgreen")
(define DEFAULT-STATE-COLOR "black")
(define FINAL-STATE-SHAPE "doublecircle")
(define ACCEPT-STATE-SHAPE "doublehexagon")
(define GRAPH-WIDTH 600)
(define GRAPH-HEIGHT 600)
(define DEFAULT-EDGE (hash
                      'fontsize 10))

(define DEFAULT-NODE (hash
                      'color "black"
                      'shape "circle"))
                      
         

; A graph is represented as a structure with three elements:
; name is a symbol used to represent the name of the graph
; node-list is a list of node structures
; edge-list is a list of edge structures 
(struct graph ([name]
               [node-list #:mutable]
               [edge-list #:mutable]) #:transparent)

; A node is a structure with four elements that represents a single state in FSA
; name is a symbol used to represent the name of a graphviz node
; value is symbol used to represent the name of a state
; abt is a hashmap used to represent a graphviz node attributes
;   https://www.graphviz.org/doc/info/colors.html
; type is a symbol used to represent if a state is a starting, final, or accepting state 
(struct node ([name]
              [value]
              [atb #:mutable]
              [type]) #:transparent)

; An edge is a structure that represents a transition from one node to another
; abt is a hashmap used to represent a graphviz edge attributes
;   https://www.graphviz.org/doc/info/colors.html
; start-node is a node-name (symbol)
; end-node is a node-name (symbol) 
(struct edge ([atb #:mutable]
              [start-node #:mutable]
              [end-node #:mutable]) #:transparent)





;; create-graph: symbol -> graph
;; Purpose: Creates a Graph wiht the given name
(define (create-graph name)
  (graph name '() '()))


;; add-node: symbol symbol symbol symbol -> node
;; Purpose: Creates a node
;; Acceptable types:
;;    start           => A start state
;;    startfinal      => A start and final state
;;    final           => A final state
;;    accept          => An accepting state (lang recs)
;;    none            => just a plain old state 
;; Purpose: adds a node to the given graph
(define (add-node graph name type  #:atb [atb DEFAULT-NODE])
  (set-graph-node-list! graph
                        (cons (create-node name type #:atb atb)
                              (graph-node-list graph))))





;; add-edge: graph symbol symbol symbol hash-map
;; Purpose: adds an edge to the graph
;; IMPORTANT: This function assumes that the node exists in the graph structure
(define (add-edge graph val start-node end-node #:atb [atb DEFAULT-EDGE])
  (letrec (
           (extractor (lambda (list accum)
                        (cond
                          [(empty? list) #f]
                          [(and (equal? start-node (edge-end-node (car list)))
                                (equal? (edge-start-node (car list)) (edge-end-node (car list)))
                                (equal? end-node (edge-end-node (car list))))accum]
                          [else (extractor (cdr list) (+ accum 1))])))

           (index (extractor (graph-edge-list graph) 0))
           )
    (cond
      [(equal? #f index) (set-graph-edge-list! graph
                                               (cons
                                                (create-edge val start-node end-node #:atb atb)
                                                (graph-edge-list graph)))]
      [else
       (let ((x (edge-atb (list-ref (graph-edge-list graph) index))))
         (set-edge-atb! (list-ref (graph-edge-list graph) index)
                        (hash-set x 'label (cons val (hash-ref x 'label)))))])))
#|       
       (set-edge-names! (list-ref (graph-edge-list graph) index)
                        (cons val (edge-names (list-ref (graph-edge-list graph) index))))])))
       
|#





;; create-node: symbol symbol symbol hash-map -> node
;; Purpose: Creates a node
;; Acceptable types:
;;    start           => A start state
;;    startfinal      => A start and final state
;;    final           => A final state
;;    accept          => An accepting state (lang recs)
;;    startaccept     => A starting and accepting state
;;    none (default)  => just a plain old state 
(define (create-node name type #:atb [atb DEFAULT-NODE])
  (if (or (equal? 'start type)
          (equal? 'none type)
          (equal? 'final type)
          (equal? 'accept type)
          (equal? 'startaccept type)
          (equal? 'startfinal type))
      (node (remove-dashes name) name (set-node-color-shape type atb) type)
      (error "Invalid node type. A node must be one of the following symbols:\n'start\n'startfinal\n'final\n'accept\n'startaccept\n'none")))


;; set-node-color-shape: type hash-map -> hash-map
(define (set-node-color-shape type hash-map)
  (case type
    [(start) (hash-set hash-map 'color START-STATE-COLOR)]
    [(startfinal)(hash-set (hash-set hash-map 'shape FINAL-STATE-SHAPE)
                           'color START-STATE-COLOR)]
    [(startaccept)(hash-set (hash-set hash-map 'shape ACCEPT-STATE-SHAPE)
                            'color START-STATE-COLOR)]
    [(final) (hash-set hash-map 'shape FINAL-STATE-SHAPE)]
    [else hash-map]))


;; create-edge: symbol symbol symbol symbol -> edge
;; Purpose: Creates an edge
(define (create-edge val start-node end-node #:atb [atb DEFAULT-EDGE])
  (edge (hash-set atb 'label (list val))  (remove-dashes start-node) (remove-dashes end-node)))






;; render-graph: graph string -> NONE
;; Purpose: writes graph to the specified file
(define (render-graph graph path #:scale [scale #f])
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (displayln (format "digraph ~s {" (graph-name graph)) out)
      (displayln "    rankdir=\"LR\";" out)
      (if scale
          (displayln (format "    size=\"~s, ~s!\";" GRAPH-WIDTH GRAPH-HEIGHT) out)
          (void))
      (render-nodes (graph-node-list graph) out)
      (render-edges (graph-edge-list graph) out)
      (displayln "}" out))))


;; render-nodes list-of-nodes write-buffer -> NONE
(define (render-nodes lon port)
  (cond
    [(empty? lon) (void)]
    [else
     (let ((node (car lon)))                    
       (begin
         (displayln (string-append (format "    ~s [label=\"~s\", "
                                           (node-name node)
                                           (node-value node))
                                   (hash->graphvizString (node-atb node)) "];")
                    port)
         (render-nodes (cdr lon) port)))]))


;list->str: (listof symbols) -> string
;Purpose: to convert the symbols in the list to a string
(define (list->str los accum)
  (cond [(empty? los) (string-trim accum)]
        [else (list->str
               (cdr los)
               (string-append accum (symbol->string (car los)) " "))]))
                             

;; render-edges list-of-edges write-buffer -> NONE
(define (render-edges loe port)
  (cond
    [(empty? loe) (void)]
    [else
     (let ((edge (car loe)))
       (begin
         (displayln (string-append (format "    ~s -> ~s ["
                                           (edge-start-node edge)
                                           (edge-end-node edge))
                                   (hash->graphvizString (edge-atb edge)) "];")
                    port)
         (render-edges (cdr loe) port)))]))
  
; remove-dashes: symbol -> symbol
; Purpose: Remove dashes
(define (remove-dashes s)
  (string->symbol (string-replace (symbol->string s) "-" "")))


;; dot->png: stirng string-> NONE
;; Purpose: converts a dot file to a png. The png files is saved in
;;   this directory
(define (dot->png path png-name)
  (if (system "dot -V")
      (begin
        (system (format "dot -Tpng ~s -o ~s.png" path png-name))
        (void))
      (error "\nError:\nPlease add graphviz as an enviroment variable. Instructions can be found at:\n   https://github.com/morazanm/fsm\n\n")))


;; graph->bitmap: graph string string -> image
;; Converts a graph to an image
(define (graph->bitmap g path png-name #:scale [scale #f])
  (begin
    (render-graph g path #:scale scale)
    (dot->png path png-name)
    (bitmap "graph.png")))

;; hash->graphvizString: hash-map -> string
;; Purpose: conversts all elemts of the hashmap to a string that can
;;  be used as a node or graph property
(define (hash->graphvizString hash)
  (letrec ((first-posn (hash-iterate-first hash))

           (key-val->string (lambda (pair)
                              (cond
                                [(and (list? (cdr pair)) (equal? (car pair) 'label))
                                 (format "~s=~s" (car pair) (convet-trans-to-string (cdr pair) "" ))]
                                [else
                                 (format "~s=~s" (car pair) (cdr pair))])))

           (iterate-hash (lambda (posn accum)
                           (cond
                             [(eq? #f posn) (string-trim accum)]
                                
                             [else
                              (iterate-hash (hash-iterate-next hash posn)
                                            (string-append (key-val->string
                                                            (hash-iterate-pair hash posn))
                                                           (if (eq? "" accum)
                                                               ""
                                                               ", ")
                                                           accum))]))))
    (iterate-hash first-posn "")))


;; convet-trans-to-string: (listOf transitions) string -> string
;; Purpose: Converts a list of transitions into a graphviz string
(define (convet-trans-to-string lot accum)
  (cond
    [(empty? lot) (string-trim accum)]
    [else
     (convet-trans-to-string (cdr lot)
                             (string-append accum " " (determine-list-type (car lot))))]))


;; determine-list-type: transition -> string
;; Purpose: Converts the list to its string representation
(define (determine-list-type aList)
  (match aList
    [val #:when (symbol? val) (symbol->string val)]
    ;; dfa/ndfa
    ;; pda
    [(list
      (list start push pop1)
      (list end pop2))
     (string-append "((" (symbol->string start)  " " (symbol->string push) " "
                    (if (list? pop1) (list->str pop1 "") (symbol->string pop1)) ") "
                    "(" (symbol->string end) " ("
                    (if (list? pop2) (list->str pop2 "") (symbol->string pop2)) ")))"
                    "\n")]
    ;; tm and lang rec
    [(list
      (list a b)
      (list c d)) (string-append "((" (symbol->string a) " " (symbol->string b) ") "
                                 "(" (symbol->string c) " " (symbol->string d) "))\n")]))



