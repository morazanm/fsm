#lang racket
(require 2htdp/image "../constants.rkt")
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
         render-graph
         dot->png
         graph->bitmap
         graph->png)


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





;; create-graph: symbol -> graph
;; name: The name of the graph
;; color-blind: An unsignded integer value that represents the color-blind state
;; Purpose: Creates a Graph wiht the given name
(define (create-graph name #:color[color-blind 0])
  (if (> color-blind 3)
      (error "The color blind option must be one of the following:\n0 -> default\n1 -> Deuteranopia option1\n2 -> Deuteranopia option2\n3 -> Red-Blue color blindness")
      (graph name '() '() color-blind)))


;; add-node: symbol symbol symbol hash-map -> node
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
                              (graph-node-list graph))))





;; add-edge: graph symbol symbol symbol hash-map
;; Purpose: adds an edge to the graph
;; IMPORTANT: This function assumes that the node exists in the graph structure
(define (add-edge graph val start-node end-node #:atb [atb DEFAULT-EDGE])
  (letrec (
           (extractor (lambda (list accum)
                        (cond
                          [(empty? list) #f]
                          [(and (equal? (remove-dashes start-node) (edge-start-node (car list)))
                                (equal? (remove-dashes end-node) (edge-end-node (car list)))) accum]
                          [else
                           (extractor (cdr list) (+ accum 1))])))
          
           (index (extractor (graph-edge-list graph) 0)))
    (cond
      [(equal? #f index) (set-graph-edge-list! graph
                                               (cons
                                                (create-edge val start-node end-node #:atb atb)
                                                (graph-edge-list graph)))]
      [else
       (let ((x (edge-atb (list-ref (graph-edge-list graph) index))))
         (set-edge-atb! (list-ref (graph-edge-list graph) index)
                        (hash-set x 'label (cons val (hash-ref x 'label)))))])))





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


;; render-graph: graph string -> NONE
;; Purpose: writes graph to the specified file
(define (render-graph graph path #:scale [scale #f] #:rule [rule #f])
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


;; NO LONGER USED!!!
;; render-header rule out-port
;; Purpose: renders the current rule in the top right
(define (render-header cur-rule stdout)
  (displayln "labelloc=\"t\";" stdout)
  (displayln "labeljust=\"r\";" stdout)
  (displayln (format "label=\"Rule: ~s\";" cur-rule) stdout)
  (displayln "fontcolor=\"blue\";" stdout))
  


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
  (cond [(empty? los) (string-append (string-trim accum) ")")]
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


;; dot->png: stirng string boolean-> NONE
;; Purpose: converts a dot file to a png. The png files is saved in
;;   this directory
(define (dot->png path png-name check)
  (if check
      (if (system "dot -V")
          (begin
            (system (format "dot -Tpng ~s -o ~s" path png-name))
            (void))
          (error "\nError:\nPlease add graphviz as an enviroment variable. Instructions can be found at:\n   https://github.com/morazanm/fsm/tree/master/GraphViz\n\n"))
      (begin
        (system (format "dot -Tpng ~s -o ~s" path png-name))
        (void))))


;; graph->bitmap: graph string string boolean -> image
;; Converts a graph to an image
(define (graph->bitmap g #:scale [scale #f])
  (let ((rel-path (build-path (current-directory) "graph.png")))
    (begin
      (render-graph g "graph.dot" #:scale scale)
      (dot->png "graph.dot" "graph.png" #t)
      (bitmap/file rel-path))))

;; graph->png: graph number rule -> NONE
;; Converts a graph to a png file stored at the root directory of fsm
(define (graph->png g #:scale [scale #f] #:rule [rule #f])
  (let ((rel-path (build-path (current-directory) "vizTool.png")))
    (begin
      (render-graph g "vizTool.dot" #:scale scale #:rule rule)
      (dot->png "vizTool.dot" "vizTool.png" #f)
      (bitmap/file rel-path))))

  

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
                             (symbol->string x)))))
    (match aList
      ;; dfa/ndfa legacy way
      [val #:when (symbol? val) (convertEMP val)]
      ;; string check for regexp
      [val #:when (string? val) (convertEMP (string->symbol val))]
      ;; dfa/ndfa
      [(list _ input _)(symbol->string input)]
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
