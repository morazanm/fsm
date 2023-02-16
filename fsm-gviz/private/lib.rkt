#lang racket
(require 2htdp/image "dot.rkt")

;; NOTICE: For more info on the functions and structures of this library please
;; read the scribble file

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
  [add-edge (->* (graph? (or/c list? symbol?) symbol? symbol?)
                 (#:atb (hash/c symbol? any/c))
                 graph?)]
  [graph->bitmap (-> graph? path? string? image?)]
  [graph->dot (-> graph? path? string? path?)]
  [graph->str (-> graph? string?)]))


;; Constants
(define DEFAULT-GRAPH (hash 'rankdir "LR"))
(define DEFAULT-EDGE (hash 'fontsize 15))
(define DEFAULT-NODE (hash 'color "black" 'shape "circle"))

;; formatters contain custom formatting functions for attributes
(struct formatters (graph node edge))

(define DEFAULT-FORMATTERS (formatters (hash) (hash) (hash)))

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

;; node->str: node -> hash -> string
;; returns the graphviz representation of a node as a string
(define (node->str node fmtr)
  (format "    ~s [~a];\n"
          (node-name node)
          (hash->str (node-atb node) fmtr)))

;; edge->str: edge -> hash -> string
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
  (graph name '() '() fmtrs atb))


;; add-node: graph -> string -> Optional(hash-map) -> graph
;; Purpose: adds a node to the given graph
(define (add-node g name #:atb [atb DEFAULT-NODE])
  (graph
   (graph-name g)
   (cons (node (remove-dashes name)
               (hash-set atb 'label (stringify-value name)))
         (graph-node-list g))
   (graph-edge-list g)
   (graph-fmtrs g)
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
         (graph-fmtrs g)
         (graph-atb g)))

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
  (define dot-exe-path (find-dot))
  (if (path? dot-exe-path)
      (begin
        ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
        ;; using the absolute path to the executable. For unknown reasons this does not
        ;; work on Windows so we will still use the PATH to call the dot executable
        (if (eq? (system-type) 'windows)
            (system (format "dot -Tpng ~s -o ~s"
                            (path->string dot-path)
                            (path->string png-path)))
            (system (format "~a -Tpng ~s -o ~s"
                            (path->string dot-exe-path)
                            (path->string dot-path)
                            (path->string png-path))))
        png-path)
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")))

;; png->bitmap: path -> string
(define png->bitmap bitmap/file)

;; graph->bitmap: graph string string -> image
;; Converts a graph to an image
(define (graph->bitmap graph save-dir filename)
  ((compose1 png->bitmap dot->png graph->dot) graph save-dir filename))
 
;; hash->str: hash -> hash -> Optional(string) -> string
;; Purpose: converts the hash to a graphviz string
(define (hash->str hash fmtr (spacer ", "))
  (define (key-val->string key value)
    (define fmtr-fun (hash-ref fmtr key #f))
    (if fmtr-fun
        (format "~s=~s" key (fmtr-fun value))
        (format "~s=~s" key value)))
  (string-join (hash-map hash key-val->string) spacer))

;; Helper function to convert a value to a string
(define (stringify-value input)
  (cond [(number? input) (number->string input)]
        [(string? input) input]
        [[symbol? input] (symbol->string input)]
        [else (error "Graphviz internal error: Unable to convert to string")]))
