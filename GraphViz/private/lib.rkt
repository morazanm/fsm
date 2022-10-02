#lang racket
(require 2htdp/image "../../constants.rkt")

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
 colorblind-opt?
 state-type?
 (contract-out
  [struct node ((name symbol?)
                (value symbol?)
                (atb hash?)
                (type state-type?))]
  [struct edge ((start-node symbol?)
                (end-node symbol?)
                (atb hash?))]
  [struct graph ((name symbol?)
                 (node-list (listof node?))
                 (edge-list (listof edge?))
                 (color-blind colorblind-opt?)
                 (atb hash?))]
  [create-graph (->* (symbol?)
                     (#:color colorblind-opt?
                      #:atb hash?)
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

(define DEFAULT-GRAPH (hash 'rankdir "LR"))
(define DEFAULT-EDGE (hash 'fontsize 15))
(define DEFAULT-NODE (hash 'color "black" 'shape "circle"))
(define RULE-LIMIT 5)

; A graph is represented as a structure with three elements:
; name is a symbol used to represent the name of the graph
; node-list is a list of node structures
; edge-list is a list of edge structures
; color-blind is a interger from 1-2 that represents the color bind state (see color-blind state above)
(struct graph ([name]
               [node-list]
               [edge-list]
               [color-blind]
               [atb #:mutable]) #:transparent)

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
; - start-node is a node-name (symbol)
; - end-node is a node-name (symbol) 
; - abt is a hashmap used to represent a graphviz edge attributes
;      For all colors see: https://www.graphviz.org/doc/info/colors.html
(struct edge ([start-node]
              [end-node]
              [atb #:mutable]) #:transparent)

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
  (define name (format "digraph ~s {\n" (graph-name graph)))
  (string-append
   name
   (format "    ~a;\n" (hash->str (graph-atb graph) ";\n    "))
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
(define (create-graph name #:color[color-blind 0] #:atb [atb DEFAULT-GRAPH])
      (graph name '() '() color-blind atb))


;; add-node: graph -> string -> symbol -> Optional(hash-map) -> graph
;; Purpose: adds a node to the given graph
(define (add-node g name type  #:atb [atb DEFAULT-NODE])
  (graph
   (graph-name g)
   (cons (node (remove-dashes name)
               name
               (set-node-color-shape type (graph-color-blind g) atb)
               type)
         (graph-node-list g))
   (graph-edge-list g)
   (graph-color-blind g)
   (graph-atb g)))

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
             (cons (edge (remove-dashes start-node)
                         (remove-dashes end-node)
                         (hash-set atb 'label (list val)))
                   (graph-edge-list g))
             (list-update (graph-edge-list g)
                          edge-index
                          (lambda (e)
                            (edge
                             (edge-start-node e)
                             (edge-end-node e)
                             (hash-set (edge-atb e)
                                       'label
                                       (cons val (hash-ref (edge-atb e) 'label)))))))
         (graph-color-blind g)
         (graph-atb g)))


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
      [(accept) (hash-set hash-map 'shape ACCEPT-STATE-SHAPE)]
      [else hash-map])))


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

;; graph->bitmap: graph string string -> image
;; Converts a graph to an image
(define (graph->bitmap graph save-dir filename)
  ((compose1 png->bitmap dot->png graph->dot) graph save-dir filename))
 

;; hash->str: hash -> Optional(string) -> string
;; Purpose: converts the hash to a graphviz string
(define (hash->str hash (spacer ", "))
  (define (key-val->string key value)
    (if (and (list? value) (equal? key 'label))
        (format "~s=~s" key (rule-label->str value))
        (format "~s=~s" key value)))
  (string-join (hash-map hash key-val->string) spacer))


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
