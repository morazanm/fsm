#lang racket
(require 2htdp/image "../constants.rkt")
#| lib.rkt
This file contains the fsm-graphviz library used to render the graph
  representation onto the Visualization tool.

Written by: Joshua Schappel
|#

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


#| ----IMPORTANT VALUES----

*Color Blind States*
0 -> default colors
1 -> Deuteranopia (Red-Green colorblindness)
2 -> Deuteranopia (alt colors)
3 -> Blue-Red
                      
         
|#
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
                 (hash->graphvizString (node-atb node)) "];\n"))

;; edge->str: edge -> string
;; returns the graphviz representation of a edge as a string
(define (edge->str edge)
  (string-append (format "    ~s -> ~s ["
                         (edge-start-node edge)
                         (edge-end-node edge))
                 (hash->graphvizString (edge-atb edge)) "];\n"))

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





;; add-edge: graph symbol symbol symbol hash-map
;; Purpose: adds an edge to the graph
;; IMPORTANT: This function assumes that the node exists in the graph structure
(define (add-edge graph val start-node end-node #:atb [atb DEFAULT-EDGE])
  (letrec ((extractor (lambda (list accum)
                        (cond
                          [(empty? list) #f]
                          [(and (equal? (remove-dashes start-node) (edge-start-node (car list)))
                                (equal? (remove-dashes end-node) (edge-end-node (car list)))) accum]
                          [else
                           (extractor (cdr list) (+ accum 1))])))
          
           (index (extractor (graph-edge-list graph) 0)))
    (cond
      [(equal? #f index) (begin
                           (set-graph-edge-list! graph
                                                 (cons
                                                  (create-edge val start-node end-node #:atb atb)
                                                  (graph-edge-list graph)))
                           graph)]
      [else
       (let ((x (edge-atb (list-ref (graph-edge-list graph) index))))
         (begin
           (set-edge-atb! (list-ref (graph-edge-list graph) index)
                          (hash-set x 'label (cons val (hash-ref x 'label))))
           graph))])))





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
 

;; hash->graphvizString: hash-map -> string
;; Purpose: conversts all elemts of the hashmap to a string that can
;;  be used as a node or graph property
(define (hash->graphvizString hash)
  (letrec ((first-posn (hash-iterate-first hash))

           (key-val->string (lambda (pair)
                              (cond
                                [(and (list? (cdr pair)) (equal? (car pair) 'label))
                                 (format "~s=~s" (car pair) (convet-trans-to-string (cdr pair) "" 0))]
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


;; convet-trans-to-string: (listOf transitions) string int -> string
;; Purpose: Converts a list of transitions into a graphviz string
(define (convet-trans-to-string lot accum len)
  (cond
    [(empty? lot) (string-trim accum)]
    [else
     (let* ([val (determine-list-type (car lot))]
            [str (cond
                   [(and (not (equal? "" accum))
                         (empty? (cdr lot))) (string-append accum ", " val)]
                   [(or (equal? "" accum)
                        (empty? (cdr lot))) (string-append accum " " val)]
                   [else (string-append accum ", " val)])])
       ;(println val)
       (cond
         [(string-contains? val "\n") (convet-trans-to-string (cdr lot)
                                                              (string-append accum " " val)
                                                              (+ len (string-length val)))]
         [(> (string-length val) RULE-LIMIT) (convet-trans-to-string (cdr lot)
                                                                     (string-append accum "\n" val "\n")
                                                                     0)]
         [(> len RULE-LIMIT) (convet-trans-to-string (cdr lot)
                                                     (string-append accum "," "\n" val)
                                                     (string-length (string-trim val)))]
         [(convet-trans-to-string (cdr lot)
                                  str
                                  (+ len (string-length val)))]))]))


;; determine-list-type: transition -> string
;; Purpose: Converts the list to its string representation
(define (determine-list-type aList)
  (letrec (;; convertEMP: symbol -> string
           ;; Purpose: converts 'e to ε
           (convertEMP (lambda (x)
                         (if (equal? EMP x)
                             "ε"
                             (stringify-value x)))))
    (match aList
      ;; dfa/ndfa legacy way
      [val #:when (or (number? val) (symbol? val)) (convertEMP val)]
      ;; string check for regexp
      [val #:when (string? val) (convertEMP (or (string->number val) (string->symbol val)))]
      ;; dfa/ndfa
      [(list _ input _)(stringify-value input)]
      ;; pda
      [(list
        (list _ read pop)
        (list _ push))
       (string-append "["
                      (convertEMP read)
                      " "
                      (if (list? pop) (list->str pop "(") (convertEMP pop))
                      " "
                      (if (list? push) (list->str push "(") (convertEMP push))
                      "],"
                      "\n")]
      ;; tm and lang rec
      [(list
        (list _ b)
        (list _ d)) (string-append "["
                                   (convertEMP b)
                                   " "
                                   (convertEMP d)
                                   "],\n")])))


;; Helper function to convert a value to a string
(define (stringify-value input)
  (cond [(number? input) (number->string input)]
        [(string? input) input]
        [[symbol? input] (symbol->string input)]
        [else (error "Graphviz internal error: Unable to convert to string")]))