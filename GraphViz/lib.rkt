#lang racket
(require 2htdp/image "../constants.rkt")
#| lib.rkt
This file contains the fsm-graphviz library used to render the graph
  representation onto the Visualization tool.

Written by: Joshua Schappel
|#

; *Color Blind States*
; 0 -> default colors
; 1 -> Deuteranopia (Red-Green colorblindness)
; 2 -> Deuteranopia (alt colors)
; 3 -> Blue-Red
(define (colorblind-opt? n)
  (and (>= n 0) (<= n 2)))

(define (state-type? s)
  (or (equal? 'start s)     ; A start state
      (equal? 'startfinal s); A start and final state
      (equal? 'final s)     ; A final state
      (equal? 'accept s)    ; An accepting state (lang recs)
      (equal? 'none s)))    ; Just a plain old state 


(provide
 (contract-out
  [struct node ((name symbol?)
                (value symbol?)
                (atb hash?)
                (type state-type?))]
  [struct edge ((atb hash?)
                (start-node symbol?)
                (end-node symbol?))]
  [struct graph ((name symbol?)
                 (node-list (listof node?))
                 (edge-list (listof edge?))
                 (color-blind colorblind-opt?))]
  [create-graph (->* (symbol?)
                     (#:color colorblind-opt?)
                     graph?)]
  [add-node (->* (graph? symbol? state-type?)
                 (#:atb hash?)
                 graph?)]
  [add-edge (->* (graph? (or/c list? symbol?) symbol? symbol?)
                 (#:atb hash?)
                 graph?)]
  [graph->bitmap (-> graph? path? string? image?)]
  [graph->dot (-> graph? path? string? path?)]
  [graph->str (-> graph? string?)]))


;; Constants 
(define START-STATE-COLOR-0 "forestgreen")
(define START-STATE-COLOR-1 "#ede209") ;; Yellow
(define START-STATE-COLOR-2 "#d48217");; Orange
(define START-STATE-COLOR-3 "#002afc") ;; Blue

(define DEFAULT-STATE-COLOR "black")
(define FINAL-STATE-SHAPE "doublecircle")
(define ACCEPT-STATE-SHAPE "doubleoctagon")
(define GRAPH-WIDTH 600)
(define GRAPH-HEIGHT 600)
(define DEFAULT-EDGE (hash
                      'fontsize 15))
(define DEFAULT-NODE (hash
                      'color "black"
                      'shape "circle"))
(define RULE-LIMIT 5)

; A graph is represented as a structure with three elements:
; name is a symbol used to represent the name of the graph
; node-list is a list of node structures
; edge-list is a list of edge structures
; color-blind is a interger from 1-2 that represents the color bind state (see color-blind state above)
(struct graph ([name]
               [node-list #:mutable]
               [edge-list #:mutable]
               [color-blind #:mutable]) #:transparent)

; A node is a structure with four elements that represents a single state in FSA
; - name is a symbol used to represent the name of a graphviz node
; - value is symbol used to represent the name of a state
; - abt is a hashmap used to represent a graphviz node attributes
;      For all colors see: https://www.graphviz.org/doc/info/colors.html
; - type is a symbol used to represent if a state is a starting, final, or accepting state 
(struct node ([name]
              [value]
              [atb #:mutable]
              [type]) #:transparent)

; An edge is a structure that represents a transition from one node to another
; - abt is a hashmap used to represent a graphviz edge attributes
;      For all colors see: https://www.graphviz.org/doc/info/colors.html
; - start-node is a node-name (symbol)
; - end-node is a node-name (symbol) 
(struct edge ([atb #:mutable]
              [start-node #:mutable]
              [end-node #:mutable]) #:transparent)

;; node->str: node -> string
;; returns the graphviz representation of a node as a string
(define (node->str node)
  (string-append (format "    ~s [label=\"~s\", "
                         (node-name node)
                         (node-value node))
                 (hash->str (node-atb node)) "];\n"))

;; edge->str: edge -> string
;; returns the graphviz representation of a edge as a string
(define (edge->str edge)
  (string-append (format "    ~s -> ~s ["
                         (edge-start-node edge)
                         (edge-end-node edge))
                 (hash->str (edge-atb edge)) "];\n"))

;; graph->str: graph -> string
;; returns the graphviz representation of a graph as a string
(define (graph->str graph)
  (define base-str (string-append
                    (format "digraph ~s {" (graph-name graph))
                    "\n    rankdir=\"LR\";\n"))
  #;(if scale
        (format "\n    size=\"~s, ~s!\";\n" GRAPH-WIDTH GRAPH-HEIGHT) "\n")

  (string-append
   base-str
   (foldl (lambda (n a) (string-append a (node->str n)))
          ""
          (graph-node-list graph))
   (foldl (lambda (e a) (string-append a (edge->str e)))
          ""
          (graph-edge-list graph))
   "}"))


;; create-graph: symbol -> graph
;; name: The name of the graph
;; color-blind: An unsignded integer value that represents the color-blind state
;; Purpose: Creates a Graph wiht the given name
(define (create-graph name #:color[color-blind 0])
  (if (> color-blind 3)
      (error "The color blind option must be one of the following:\n0 -> default\n1 -> Deuteranopia option1\n2 -> Deuteranopia option2\n3 -> Red-Blue color blindness")
      (graph name '() '() color-blind)))


;; add-node: symbol symbol symbol hash-map -> graph
;; Purpose: Creates a node
;; Acceptable types (3rd parameter):
;;    start           => A start state
;;    startfinal      => A start and final state
;;    final           => A final state
;;    accept          => An accepting state (lang recs)
;;    none            => just a plain old state 
;; Purpose: adds a node to the given graph
(define (add-node graph name type  #:atb [atb DEFAULT-NODE])
  (set-graph-node-list! graph
                        (cons (create-node name type (graph-color-blind graph) #:atb atb)
                              (graph-node-list graph)))
  graph)





;; add-edge: graph -> symbol -> symbol -> symbol -> Optional(hash-map) -> graph
;; Purpose: adds an edge to the graph
;; IMPORTANT: This function assumes that the node exists in the graph structure
(define (add-edge g val start-node end-node #:atb [atb DEFAULT-EDGE])
  (define start (remove-dashes start-node))
  (define end (remove-dashes end-node))
  (define (edge-eq? e) (and (equal? start (edge-start-node e))
                            (equal? end (edge-end-node e))))      
  (define edge-index (index-where (graph-edge-list g) edge-eq?))
  (graph (graph-name g)
         (graph-node-list g)
         (if (equal? #f edge-index)
             (cons (create-edge val start-node end-node #:atb atb)
                   (graph-edge-list g))
             (list-update (graph-edge-list g)
                          edge-index
                          (lambda (e)
                            (edge
                             (hash-set (edge-atb e) 'label (cons val (hash-ref (edge-atb e) 'label)))
                             (edge-start-node e)
                             (edge-end-node e)))))  
         (graph-color-blind g)))



;; create-node: symbol symbol symbol hash-map -> node
;; Purpose: Creates a node
;; Acceptable types:
;;    start           => A start state
;;    startfinal      => A start and final state
;;    final           => A final state
;;    accept          => An accepting state (lang recs)
;;    startaccept     => A starting and accepting state
;;    none (default)  => just a plain old state 
(define (create-node name type color-blind #:atb [atb DEFAULT-NODE])
  (if (or (equal? 'start type)
          (equal? 'none type)
          (equal? 'final type)
          (equal? 'accept type)
          (equal? 'startaccept type)
          (equal? 'startfinal type))
      (node (remove-dashes name) name (set-node-color-shape type  color-blind atb) type)
      (error "Invalid node type. A node must be one of the following symbols:\n'start\n'startfinal\n'final\n'accept\n'startaccept\n'none")))


;; set-node-color-shape: type hash-map -> hash-map
(define (set-node-color-shape type color-blind hash-map)
  (let ((color (case color-blind
                 [(0) START-STATE-COLOR-0]
                 [(1) START-STATE-COLOR-1]
                 [(2) START-STATE-COLOR-2]
                 [else START-STATE-COLOR-3])))
    (case type
      [(start) (hash-set hash-map 'color color)]
      [(startfinal)(hash-set (hash-set hash-map 'shape FINAL-STATE-SHAPE)
                             'color color)]
      [(startaccept)(hash-set (hash-set hash-map 'shape ACCEPT-STATE-SHAPE)
                              'color color)]
      [(final) (hash-set hash-map 'shape FINAL-STATE-SHAPE)]
      [else hash-map])))


;; create-edge: symbol symbol symbol symbol -> edge
;; Purpose: Creates an edge
(define (create-edge val start-node end-node #:atb [atb DEFAULT-EDGE])
  (edge (hash-set atb 'label (list val))  (remove-dashes start-node) (remove-dashes end-node)))


;list->str: (listof symbols) -> string
;Purpose: to convert the symbols in the list to a string
(define (list->str los accum)
  (cond [(empty? los) (string-append (string-trim accum) ")")]
        [else (list->str
               (cdr los)
               (string-append accum (stringify-value (car los)) " "))]))


; remove-dashes: symbol -> symbol
; Purpose: Remove dashes
(define (remove-dashes s)
  (string->symbol (string-replace (stringify-value s) "-" "")))

;; graph->dot: graph -> path -> string -> path
;; Purpose: writes graph to the specified file
(define (graph->dot graph save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (call-with-output-file dot-path
    #:exists 'replace
    (lambda (out)
      (displayln (graph->str graph) out)))
  dot-path)

;; dot->png: path -> path
;; Purpose: converts a dot file to a png. The png files in the current directory
(define (dot->png dot-path)
  (define png-path (path-replace-extension dot-path ".png"))
  (if (system (format "dot -Tpng ~s -o ~s"  (path->string dot-path) (path->string png-path)))
      png-path
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")))

;; png->bitmap: path -> string
(define png->bitmap bitmap/file)

;; graph->bitmap: graph string string boolean -> image
;; Converts a graph to an image
(define (graph->bitmap graph save-dir filename)
  ((compose1 png->bitmap dot->png graph->dot) graph save-dir filename))
 

;; hash->str: hash -> string
;; Purpose: converts the hash to a graphviz string
(define (hash->str hash)
  (define (key-val->string key value)
    (if (and (list? value) (equal? key 'label))
        (format "~s=~s" key (rule-label->str value))
        (format "~s=~s" key value)))
  (string-join (hash-map hash key-val->string) ", "))


;; rule-label->str: listof(rules) -> string
;; Purpose: Converts a list of rules to a graphviz label
(define (rule-label->str rules)
  (define (format-line l acc count)
    (match l
      ['() (cons acc '())]
      [`(,x ,xs ...)
       (if (and (not (empty? acc))
                (> (+ 2 count (string-length x)) RULE-LIMIT))
           (cons acc l)
           (format-line xs (append acc (list x)) (+ count (string-length x))))]))
  (define (format-lines lines)
    (match-define (cons line xs) (format-line lines '() 0))
    (if (empty? lines)
        '()
        (cons (string-join line ", ") (format-lines xs))))
  (string-join (format-lines (map fsa-rule->label rules)) ",\n"))


;; fsa-rule->label: transition -> string
;; Purpose: Converts the list to its string representation
(define (fsa-rule->label aList)
  (match aList
    ;; dfa/ndfa
    [(list _ input _) (stringify-value input)]
    ;; pda
    [(list (list _ read pop) (list _ push))
     (format "[~a ~a ~a]" (stringify-value read)
             (if (list? pop) (list->str pop "(") (stringify-value pop))
             (if (list? push) (list->str push "(") (stringify-value push)))]
    ;; tm and lang rec
    [(list (list _ b) (list _ d))
     (format "[~a ~a]" (stringify-value b) (stringify-value d))]
    ;; dfa/ndfa legacy way
    [val (stringify-value val)]))


;; Helper function to convert a value to a string
(define (stringify-value input)
  (cond [(number? input) (number->string input)]
        [(string? input) input]
        [[symbol? input] (symbol->string input)]
        [else (error "Graphviz internal error: Unable to convert to string")]))