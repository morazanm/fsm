#lang racket
(require 2htdp/image racket/hash "dot.rkt")
#| This file handles converting graphs to the dot file equivalent |#

(provide
 stringify-value
 image?
 (contract-out
  (struct formatters ((graph (hash/c symbol? (-> any/c string?)))
                      (node (hash/c symbol? (-> any/c string?)))
                      (edge (hash/c symbol? (-> any/c string?)))))
  [struct node ((name symbol?)
                (atb (hash/c symbol? any/c)))]
  [struct edge ((start-node symbol?)
                (end-node symbol?)
                ;; NOTE: a edges label atb is a list since we squash all edges
                ;; between the same nodes into a single edge
                (atb (hash/c symbol? any/c)))]
  [struct graph ((name symbol?)
                 (node-list (listof node?))
                 (edge-list (listof edge?))
                 (fmtrs formatters?)
                 (atb (hash/c symbol? any/c)))]
  [create-graph (->* (symbol?)
                     (#:fmtrs formatters?
                      #:atb (hash/c symbol? any/c))
                     graph?)]
  [add-node (->* (graph? symbol?)
                 (#:atb (hash/c symbol? any/c))
                 graph?)]
  [add-nodes (->* (graph? (listof symbol?))
                 (#:atb (hash/c symbol? any/c))
                 graph?)]
  [add-edge (->* (graph? (or/c list? any/c) symbol? symbol?)
                 (#:atb (hash/c symbol? any/c))
                 graph?)]

  [add-edges (->* (graph? (list/c symbol? symbol? (or/c list? any/c)))
                  (#:atb (hash/c symbol? any/c))
                  graph?)]
  [graph->bitmap (-> graph? path? string? image?)]
  [graph->svg (-> graph? path? string? path?)]
  [graph->dot (-> graph? path? string? path?)]
  [graph->str (-> graph? string?)]))


;; Constants
(define DEFAULT-GRAPH (hash 'rankdir "LR"))
(define DEFAULT-EDGE (hash 'fontsize 15))
(define DEFAULT-NODE (hash 'color "black" 'shape "circle"))
(define (DEFAULT-EDGE-LABEL-FMTR lst)
  (string-join (map (lambda (v) (format "~a" v)) (reverse lst)) ", "))

;; formatters contain custom formatting functions for attributes
(struct formatters (graph node edge) #:transparent)

(define DEFAULT-FORMATTERS (formatters
                            (hash)
                            (hash)
                            (hash 'label DEFAULT-EDGE-LABEL-FMTR)))

; A structure the represents a digraph in the dot language
(struct graph ([name]
               [node-list]
               [edge-list]
               [fmtrs]
               [atb #:mutable]) #:transparent)

; A structure the represents a node in the dot language
(struct node ([name]
              [atb #:mutable]) #:transparent)

; A structure the represents a edgs in the dot language
(struct edge ([start-node]
              [end-node]
              [atb #:mutable]) #:transparent)

;; node->str: node hash -> string
;; returns the graphviz representation of a node as a string
(define (node->str node fmtr)
  (format "    ~s [~a];\n"
          (node-name node)
          (hash->str (node-atb node) fmtr)))

;; edge->str: edge hash -> string
;; returns the graphviz representation of a edge as a string
(define (edge->str edge fmtr)
  (format "    ~s -> ~s [~a];\n"
          (edge-start-node edge)
          (edge-end-node edge)
          (hash->str (edge-atb edge) fmtr)))

;; graph->str: graph -> string
;; returns the graphviz representation of a graph as a string
(define (graph->str g)
  (define name (format "digraph ~s {\n" (graph-name g)))
  (define fmtrs (graph-fmtrs g))
  (string-append
   name
   (format "    ~a;\n" (hash->str (graph-atb g) (formatters-graph fmtrs) ";\n    "))
   (foldl (lambda (n a) (string-append a (node->str n (formatters-node fmtrs))))
          ""
          (graph-node-list g))
   (foldl (lambda (e a) (string-append a (edge->str e (formatters-edge fmtrs))))
          ""
          (graph-edge-list g))
   "}"))


;; create-graph: symbol -> graph
;; name: The name of the graph
;; Purpose: Creates a Graph with the given name
(define (create-graph name #:fmtrs(fmtrs DEFAULT-FORMATTERS) #:atb [atb DEFAULT-GRAPH])
  (define (combine v1 v2) (hash-union v1 v2 #:combine/key (lambda (_k v1 _v2) v1)))
  (graph name
         '()
         '()
         (formatters
          (combine (formatters-graph fmtrs)
                   (formatters-graph DEFAULT-FORMATTERS))
          (combine (formatters-node fmtrs)
                   (formatters-node DEFAULT-FORMATTERS))
          (combine (formatters-edge fmtrs)
                   (formatters-edge DEFAULT-FORMATTERS)))
         atb))


;; add-node: graph string Optional(hash-map) -> graph
;; Purpose: adds a node to the given graph
(define (add-node g name #:atb [atb DEFAULT-NODE])
  (graph
   (graph-name g)
   (cons (node (clean-string name)
               (hash-set atb 'label (stringify-value name)))
         (graph-node-list g))
   (graph-edge-list g)
   (graph-fmtrs g)
   (graph-atb g)))


;; add-nodes: graph listof(symbol) Optional(hash-map) -> graph
;; Purpose: adds the list of nodes to the given graph
(define (add-nodes g names #:atb [atb DEFAULT-NODE])
  (define nodes-to-add (map (lambda (n) (node (clean-string n)
                                              (hash-set atb 'label (stringify-value n))))
                            names))
  (graph
   (graph-name g)
   (append nodes-to-add (graph-node-list g))
   (graph-edge-list g)
   (graph-fmtrs g)
   (graph-atb g)))

;; add-edge: graph symbol symbol symbol Optional(hash-map) -> graph
;; Purpose: adds an edge to the graph
;; NOTE: This function assumes that the node exists in the graph structure
;; NOTE: a edges label is a list since we squash all edges between the same nodes
;; into a single edge
(define (add-edge g val start-node end-node #:atb [atb DEFAULT-EDGE])
  (define start (clean-string start-node))
  (define end (clean-string end-node))
  (define (edge-eq? e) (and (equal? start (edge-start-node e))
                            (equal? end (edge-end-node e))))      
  (define edge-index (index-where (graph-edge-list g) edge-eq?))
  (graph (graph-name g)
         (graph-node-list g)
         (if (equal? #f edge-index)
             (cons (edge (clean-string start-node)
                         (clean-string end-node)
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
         (graph-fmtrs g)
         (graph-atb g)))


;; add-edges: graph listof(symbol symbol any/c) Optional(hash-map) -> graph
;; Purpose: adds this list of edges to the graph
;; NOTE: This function assumes that the node exists in the graph structure
;; NOTE: a edges label is a list since we squash all edges between the same nodes
;; into a single edge
(define (add-edges g edgs #:atb [atb DEFAULT-EDGE])
  (foldl (lambda (e a)
           (match-define (list start end val) e)
           (add-edge a val start end #:atb atb)) g edgs))

; clean-string: symbol -> symbol
; Purpose: cleans the string to only have valid dot language id symbols
; https://graphviz.org/doc/info/lang.html
(define (clean-string s)
  (string->symbol (string-replace (string-replace (stringify-value s) "-" "") " " "__")))

;; graph->dot: graph path string -> path
;; Purpose: writes graph to the specified file
(define (graph->dot graph save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (call-with-output-file dot-path
    #:exists 'replace
    (lambda (out)
      (displayln (graph->str graph) out)))
  dot-path)

;; dot->output-fmt: symbolof(file-format) path -> path
;; Purpose: converts a dot file to the specified output. The new file is saved in directory
;; of the provided file. The name of the new file is the same as input with a different extension
;; NOTE: For possiable formats see: https://graphviz.org/docs/outputs/svg/
(define (dot->output-fmt fmt dot-path)
  (define png-path (path-replace-extension dot-path (format ".~s" fmt)))
  (define dot-executable-path (find-dot))
  (unless (path? dot-executable-path)
    (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path"))
  (system (format "~a -T~s ~s -o ~s"
                  ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
                  ;; using the absolute path to the executable. For unknown reasons this does not
                  ;; work on Windows so we will still use the PATH to call the dot executable
                  (if (equal? (system-type) 'windows)
                      "dot"
                      (path->string dot-executable-path))
                  fmt
                  (path->string dot-path)
                  (path->string png-path)))

  png-path)


;; dot->png: path -> path
;; Purpose: converts a dot file to a png. The png file is saved in the directory
;; of the provided path
(define dot->png (curry dot->output-fmt 'png))

;; dot->svg: path -> path
;; Purpose: converts a dot file to a svg. The svg file is saved in the directory
;; of the provided path
(define dot->svg (curry dot->output-fmt 'svg))


;; png->bitmap: path -> string
;; Function alias to htdp2-lib function
(define png->bitmap bitmap/file)

;; graph->bitmap: graph string string -> image
;; Converts a graph to an image
(define (graph->bitmap graph save-dir filename)
  ((compose1 png->bitmap dot->png graph->dot) graph save-dir filename))


;; graph->svg: graph string string -> path
;; Converts a graph to a svg and returns the path to the svg image
(define (graph->svg graph save-dir filename)
  ((compose1 dot->svg graph->dot) graph save-dir filename))
 
;; hash->str: hash hash Optional(string) -> string
;; Purpose: converts the hash to a graphviz string
(define (hash->str hash fmtr (spacer ", "))
  (define (key-val->string key value)
    (define fmtr-fun (hash-ref fmtr key #f))
    (if fmtr-fun
        (format "~s=~s" key (fmtr-fun value))
        (format "~s=~s" key (if (equal? key 'label) (format "~a" value) value))))
  (string-join (hash-map hash key-val->string) spacer))

;; Helper function to convert a value to a string
(define (stringify-value input)
  (cond [(number? input) (number->string input)]
        [(string? input) input]
        [[symbol? input] (symbol->string input)]
        [else (error "Graphviz internal error: Unable to convert to string")]))



(module+ test
  (require rackunit)

  (check-equal? (add-nodes (create-graph 'test) '(|A A| B C D E-1))
                (graph
                 'test
                 (list
                  (node 'A__A #hash((color . "black") (label . "A A") (shape . "circle")))
                  (node 'B #hash((color . "black") (label . "B") (shape . "circle")))
                  (node 'C #hash((color . "black") (label . "C") (shape . "circle")))
                  (node 'D #hash((color . "black") (label . "D") (shape . "circle")))
                  (node 'E1 #hash((color . "black") (label . "E-1") (shape . "circle"))))
                 '()
                 DEFAULT-FORMATTERS
                 DEFAULT-GRAPH))


  (check-equal? (add-edges (add-nodes (create-graph 'test) '(A B C D))
                           '((A B a) (B B b) (B D c-1)))
                (graph
                 'test
                 (list
                  (node 'A #hash((color . "black") (label . "A") (shape . "circle")))
                  (node 'B #hash((color . "black") (label . "B") (shape . "circle")))
                  (node 'C #hash((color . "black") (label . "C") (shape . "circle")))
                  (node 'D #hash((color . "black") (label . "D") (shape . "circle"))))
                 (list
                  (edge 'B 'D #hash((fontsize . 15) (label . (c-1))))
                  (edge 'B 'B #hash((fontsize . 15) (label . (b))))
                  (edge 'A 'B #hash((fontsize . 15) (label . (a)))))
                 DEFAULT-FORMATTERS
                 DEFAULT-GRAPH))






  ) ;; end module+ test
