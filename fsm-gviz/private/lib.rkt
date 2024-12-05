#lang racket/base
(require 2htdp/image
         racket/hash
         "dot.rkt"
         racket/treelist
         racket/string
         racket/list
         racket/match
         racket/system
         racket/function
         racket/contract/base
         )

(provide hash->str)

#| This file handles converting graphs to the dot file equivalent |#

;; custom contract for a graph type. A graph type is either a 
;; graph or subgraph struct
(define (graph-type? a)
  (or (graph? a) (subgraph? a)))

(provide
 hash->str
 stringify-value
 image?
 find-dot
 find-tmp-dir
 special-graph->dot
 cfg-graph->dot
 test-cfg-graph->dot
 test-cfg-graph->str
 cfg-graph->str
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
  [struct subgraph ((name string?)
                    (node-list (listof node?))
                    (edge-list (listof edge?))
                    (subgraph-list (listof subgraph?))
                    (atb (hash/c symbol? any/c)))]
  [struct graph ((name symbol?)
                 (node-list (listof node?))
                 (edge-list (listof edge?))
                 (subgraph-list (listof subgraph?))
                 (fmtrs formatters?)
                 (atb (hash/c symbol? any/c))
                 (rank-node-lst (listof (listof symbol?)))
                 )]
  [create-graph (->* (symbol?)
                     (#:fmtrs formatters?
                      #:atb (hash/c symbol? any/c))
                     graph?)]

  [create-subgraph (->* ()
                        (#:name symbol?
                         #:atb (hash/c symbol? any/c))
                        subgraph?)]
  (create-formatters (->* () (#:graph (hash/c symbol? (-> any/c string?))
                              #:node (hash/c symbol? (-> any/c string?))
                              #:edge (hash/c symbol? (-> any/c string?)))
                          formatters?))
  [add-node (->* (graph-type? symbol?)
                 (#:atb (hash/c symbol? any/c))
                 graph-type?)]
  [add-nodes (->* (graph-type? (listof symbol?))
                  (#:atb (hash/c symbol? any/c))
                  graph-type?)]
  [add-edge (->* (graph-type? (or/c list? any/c) symbol? symbol?)
                 (#:atb (hash/c symbol? any/c))
                 graph-type?)]
  [add-subgraph (-> graph-type? subgraph? graph-type?)]
  [add-edges (->* (graph-type? (listof (list/c symbol? any/c symbol?)))
                  (#:atb (hash/c symbol? any/c))
                  graph-type?)]
  [graph->bitmap (->* (graph?)
                      (#:directory path?
                       #:filename string?
                       #:clean boolean?)
                      image?)]
  [graph->svg (->* (graph? path? string?) (#:clean boolean?) path?)]
  [graph->dot (-> graph? path? string? path?)]
  [graph->png (->* (graph? path? string?) (#:clean boolean?) path?)]
  [graph->str (-> graph? string?)]))


;; Constants
(define DEFAULT-GRAPH (hash 'rankdir "LR"))
(define DEFAULT-EDGE (hash 'fontsize 15))
(define DEFAULT-NODE (hash 'color "black" 'shape "circle"))
(define (DEFAULT-EDGE-LABEL-FMTR lst)
  (string-join (map (lambda (v) (format "~a" v)) (reverse lst)) ", "))
(define SAVE-DIR (find-tmp-dir))


;; formatters contain custom formatting functions for attributes
(struct formatters (graph node edge) #:transparent)

(define DEFAULT-FORMATTERS (formatters
                            (hash)
                            (hash)
                            (hash 'label DEFAULT-EDGE-LABEL-FMTR)))

; A structure the represents a digraph in the dot language
(struct graph (name
               node-list
               edge-list
               subgraph-list
               fmtrs
               [atb #:mutable]
               rank-node-lst) #:transparent)

; A structure the represents a subgraph in the dot language
(struct subgraph (name
                  node-list
                  edge-list
                  subgraph-list
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


;; subgraph->str: subgraph formatters -> string
;; returns the string representation of a subgraph
(define (subgraph->str sg fmtrs)
  (define name (if (null? (subgraph-name sg)) "" (subgraph-name sg)))
  (string-append (format "    subgraph ~s {\n" name) 
                 (format "        ~a;\n" (hash->str (subgraph-atb sg) (formatters-graph fmtrs) ";\n        "))
                 (foldl (lambda (n a) (string-append a "    " (node->str n (formatters-node fmtrs))))
                        ""
                        (subgraph-node-list sg))
                 (foldl (lambda (e a) (string-append a "    " (edge->str e (formatters-edge fmtrs))))
                        ""
                        (subgraph-edge-list sg))
                 (foldl (lambda (e a) (string-append a "    " (subgraph->str e fmtrs)))
                        ""
                        (subgraph-subgraph-list sg))
                 "    }\n"))

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
   (foldl (lambda (e a) (string-append a (subgraph->str e fmtrs)))
          ""
          (graph-subgraph-list g))
   "}"))


;; graph -> string
;; Returns graphviz representation of a graph containing a context sensitive graph as a string
(define (special-graph->str g rank-node-lst)
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
   (foldl (lambda (e a) (string-append a (subgraph->str e fmtrs)))
          ""
          (graph-subgraph-list g))
   (format "    {rank=same;~a};\n" (foldr (lambda (val accum) (string-append (symbol->string val) ";" accum)) "" rank-node-lst))
   "}")
  )

;; graph -> string
;; Returns graphviz representation of a graph containing a context free graph as a string
(define (cfg-graph->str g rank-node-lst)
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
   (foldl (lambda (e a) (string-append a (subgraph->str e fmtrs)))
          ""
          (graph-subgraph-list g))
   (foldr (lambda (lvl accum)
            (string-append (format "    {rank=same;~a};\n" (foldr (lambda (val accum) (string-append (symbol->string val) ";" accum)) "" lvl)) accum))
          ""
          rank-node-lst)
   "}")
  )



(define (test-cfg-graph->str g rank-node-lst)
  (define (hash->str-accum hash fmtr accum (spacer ", "))
    (define (fmt-val val)
      (if (boolean? val)
          (if val 'true 'false)
          val))
    #;(define (key-val->string key value)
      (define fmtr-fun (hash-ref fmtr key #f))
      (if fmtr-fun
          (cons "\"" (cons (fmtr-fun value) (cons "\"" (cons "=" (cons (stringify-value key) '())))))
          (cons (if (equal? key 'label)
                    (stringify-value (fmt-val value))
                    (stringify-value (fmt-val value)))
                (cons "=" (cons (symbol->string key) '())))
          ))
    (define (key-val->string key value)
      (define fmtr-fun (hash-ref fmtr key #f))
      (if fmtr-fun
          (format "~s=~s" key (fmtr-fun value))
          (format "~s=~s" key (if (equal? key 'label)
                                  (format "~a" (fmt-val value))
                                  (fmt-val value)))))
    (define (add-spacer lst accum)
      (if (empty? lst)
          accum
          (add-spacer (rest lst) (cons (first lst) (cons spacer accum)))))
    (let ([hash-vals (reverse (hash-map hash key-val->string))])
      (add-spacer (rest hash-vals) (cons (first hash-vals) accum))
      ))
  (define (node->str-accum node fmtr accum)
    (cons "];\n" (hash->str-accum (node-atb node) fmtr (cons "[" (cons (symbol->string (node-name node)) (cons "    " accum)))))
    )
  (define (edge->str-accum edge fmtr accum)
    (cons "];\n" (hash->str-accum (edge-atb edge) fmtr (cons " [" (cons (symbol->string (edge-end-node edge)) (cons " -> " (cons (symbol->string (edge-start-node edge)) (cons "    " accum)))))))
    )
  (define name (format "digraph ~s {\n" (graph-name g)))
  (define fmtrs (graph-fmtrs g))
  (apply string-append
   (reverse (cons "}" (foldr (lambda (lvl acc0)
                               (cons "};\n" (foldr (lambda (val acc1) (cons ";" (cons (symbol->string val) acc1))) (cons "    {rank=same;" acc0) lvl)
                                     )
                               )
                             (foldr (lambda (e a) (edge->str-accum e (formatters-edge fmtrs) a))
                                    (foldr (lambda (n a) (node->str-accum n (formatters-node fmtrs) a))
                                           (cons "\n" (hash->str-accum (graph-atb g)
                                                                       (formatters-graph fmtrs)
                                                                       (cons "    " (cons " {\n" (cons (symbol->string (graph-name g)) (cons "digraph " '())))) ";\n    "))
                                           (graph-node-list g))
                                    (graph-edge-list g))
                             rank-node-lst)))
   )
  )





;; combine :: hash hash -> hash
;; combines the two hashes together. If the hashs have the same key the first hash 
;; is used.
(define (combine v1 v2) (hash-union v1 v2 #:combine/key (lambda (_k v1 _v2) v1)))

;; create-graph: symbol -> graph
;; name: The name of the graph
;; Purpose: Creates a Graph with the given name
(define (create-graph name #:fmtrs(fmtrs DEFAULT-FORMATTERS) #:atb [atb (hash)])
  (graph name
         '()
         '()
         '()
         (formatters
          (combine (formatters-graph fmtrs)
                   (formatters-graph DEFAULT-FORMATTERS))
          (combine (formatters-node fmtrs)
                   (formatters-node DEFAULT-FORMATTERS))
          (combine (formatters-edge fmtrs)
                   (formatters-edge DEFAULT-FORMATTERS)))
         (combine atb DEFAULT-GRAPH)
         '()))


(define (create-formatters #:graph[graph (hash)] #:node[node (hash)] #:edge[edge (hash)])
  (formatters graph node edge))

;; create-subgraph :: subgraph
;; subgraphs can have a optional name, but it is not required
(define (create-subgraph #:name[name null] #:atb [atb (hash)])
  (subgraph (if (null? name) name (clean-string name))
            '()
            '()
            '()
            atb))

;; existing-name graph | subgraph string -> boolean
;; True when the name already exists
(define (existing-name? parent n)
  (define name (clean-string n))
  (define (check-subgraph n sg)
    (or (equal? sg n)
        (member n (subgraph-subgraph-list sg) check-subgraph)))
  (if (graph? parent)
      (or (member name (graph-node-list parent) (lambda (v n) (equal? (node-name n) v)))
          (member name (graph-subgraph-list parent) check-subgraph))
      (check-subgraph n parent)))




;; add-node: graph | subgraph string Optional(hash-map) -> graph | subgraph
;; Purpose: adds a node to the given graph
(define (add-node parent name #:atb [atb DEFAULT-NODE])
  (when (existing-name? parent name)
    (error "[Duplicate Name]: Node name already exists on the graph"))
  (define new-node (node (clean-string name) (hash-set atb 'label (stringify-value (hash-ref atb 'label name)))))
  (if (graph? parent)
      (struct-copy graph parent
                   [node-list (cons new-node (graph-node-list parent))])
      #;(graph
       (graph-name parent)
       (cons new-node (graph-node-list parent))
       (graph-edge-list parent)
       (graph-subgraph-list parent)
       (graph-fmtrs parent)
       (graph-atb parent))
      (struct-copy subgraph parent
                   [node-list (cons new-node (subgraph-node-list parent))])
      #;(subgraph 
       (subgraph-name parent)
       (cons new-node (subgraph-node-list parent))
       (subgraph-edge-list parent)
       (subgraph-subgraph-list parent)
       (subgraph-atb parent))))


;; add-nodes: graph | subgraph listof(symbol) Optional(hash-map) -> graph | subgraph
;; Purpose: adds the list of nodes to the given graph
(define (add-nodes parent names #:atb [atb DEFAULT-NODE])
  (define nodes-to-add 
    (map (lambda (n)
           (when (existing-name? parent n)
             (error "[Duplicate Name]: Edge name already exists on the graph"))
           (node (clean-string n) (hash-set atb 'label (stringify-value n))))
         names))
  (if (graph? parent)
      (struct-copy graph parent
                   [node-list (append nodes-to-add (graph-node-list parent))])
      #;(graph
       (graph-name parent)
       (append nodes-to-add (graph-node-list parent))
       (graph-edge-list parent)
       (graph-subgraph-list parent)
       (graph-fmtrs parent)
       (graph-atb parent))
      (struct-copy subgraph parent
                   [node-list (append nodes-to-add (subgraph-node-list parent))])
      #;(subgraph 
       (subgraph-name parent)
       (append nodes-to-add (subgraph-node-list parent))
       (subgraph-edge-list parent)
       (subgraph-subgraph-list parent)
       (subgraph-atb parent))))

;; add-edge: graph | subgraph  symbol symbol symbol Optional(hash-map) -> graph | subgraph
;; Purpose: adds an edge to the graph
;; NOTE: This function assumes that the node exists in the graph structure
;; NOTE: a edges label is a list since we squash all edges between the same nodes
;; into a single edge
(define (add-edge parent val start-node end-node #:atb [atb DEFAULT-EDGE])
  (define start (clean-string start-node))
  (define end (clean-string end-node))
  (define (is-subgraph? val parent)
    (cond
      [(graph? parent) (member val (graph-subgraph-list parent) is-subgraph?)]
      [(equal? val (subgraph-name parent)) #t]
      [else (member val (subgraph-subgraph-list parent) is-subgraph?)]))
  
  (define (edge-eq? e) (and (equal? start (edge-start-node e))
                            (equal? end (edge-end-node e))))
  (define (add/update-in-list lst)
    (define edge-index (index-where lst edge-eq?))
    (cond
      [(not edge-index)
       ;; If the node is a edge then we need to add lhead, or ltail attributes
       ;; and point it to a random node on the subgraph 
       (define is-subgraph-start? (is-subgraph? start parent))
       (define is-subgraph-end? (is-subgraph? end parent))
       (define h1 (hash-union (if is-subgraph-start? (hash 'ltail start) (hash))
                              (if is-subgraph-end? (hash 'lhead end) (hash))))
       (cons (edge (if is-subgraph-start?
                       (node-name (car (subgraph-node-list (car is-subgraph-start?))))
                       start)
                   (if is-subgraph-end?
                       (node-name (car (subgraph-node-list (car is-subgraph-end?))))
                       end)
                   (hash-union h1 (hash-set atb 'label (list val))))
             lst)]
      [else 
       (list-update lst
                    edge-index
                    (lambda (e)
                      (edge
                       (edge-start-node e)
                       (edge-end-node e)
                       (hash-set (edge-atb e)
                                 'label
                                 (cons val (hash-ref (edge-atb e) 'label))))))]))
    
  (if (graph? parent)
      (struct-copy graph parent
                   [edge-list (add/update-in-list (graph-edge-list parent))])
      #;(graph 
       (graph-name parent)
       (graph-node-list parent)
       (add/update-in-list (graph-edge-list parent))
       (graph-subgraph-list parent)
       (graph-fmtrs parent)
       (graph-atb parent))
      (struct-copy subgraph parent
                   [edge-list (add/update-in-list (subgraph-edge-list parent))])
      #;(subgraph 
       (subgraph-name parent)
       (subgraph-node-list parent)
       (add/update-in-list (subgraph-edge-list parent))
       (subgraph-atb parent))))


;; add-edges: graph | subgraph listof(symbol symbol any/c) Optional(hash-map) -> graph | subgraph
;; Purpose: adds this list of edges to the graph
;; NOTE: This function assumes that the node exists in the graph structure
;; NOTE: a edges label is a list since we squash all edges between the same nodes
;; into a single edge
(define (add-edges parent edgs #:atb [atb DEFAULT-EDGE])
  (foldl (lambda (e a)
           (match-define (list start-node edge-val end-node) e)
           (add-edge a edge-val start-node end-node #:atb atb)) parent edgs))


;; add-subgraph :: graph | subgraph subgraph -> graph | subgraph
;; adds a subgraph to either a graph or subgraph
(define (add-subgraph parent sg)
  (when (and (not (null? (subgraph-name sg)))
             (existing-name? parent (subgraph-name sg)))
    (error "[Duplicate Name]: Name of subgraph already exists on the graph"))
  (if (graph? parent)
      (struct-copy graph parent
                   [subgraph-list (cons sg (graph-subgraph-list parent))])
      #;(graph 
       (graph-name parent)
       (graph-node-list parent)
       (graph-edge-list parent)
       (cons sg (graph-subgraph-list parent))
       (graph-fmtrs parent)
       (graph-atb parent))
      (struct-copy subgraph parent
                   [subgraph-list (cons sg (subgraph-subgraph-list parent))])
      #;(subgraph 
       (subgraph-name parent)
       (subgraph-node-list parent)
       (subgraph-edge-list parent)
       (cons sg (subgraph-subgraph-list parent))
       (subgraph-atb parent))))

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

;; graph path string -> path
;; Writes graph containing context sensitive grammar to the specified file
(define (special-graph->dot graph rank-node-lst save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (call-with-output-file dot-path
    #:exists 'replace
    (lambda (out)
      (displayln (special-graph->str graph rank-node-lst) out)))
  dot-path)

;; graph path string -> path
;; Writes graph containing context sensitive grammar to the specified file
(define (cfg-graph->dot graph rank-node-lst save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (call-with-output-file dot-path
    #:exists 'replace
    (lambda (out)
      (displayln (cfg-graph->str graph rank-node-lst) out)))
  dot-path)



(define (test-cfg-graph->dot graph rank-node-lst save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (call-with-output-file dot-path
    #:exists 'replace
    (lambda (out)
      (displayln (test-cfg-graph->str graph rank-node-lst) out)))
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


;; png->bitmap: path -> values(img path)
;; returns the image and the path to the file that was used to create the image
(define (png->bitmap path)
  (values (bitmap/file path) path))

;; clean-up-files-by-extension :: filepath string/regex
;; Given a filepath will delete the files by the given extension. For example
;; if you have files test.png test.svg test.dot
;;    (clean-up-files-by-extension test-path #".dot" #".png")
;; will delete test.png and test.dot 
(define-syntax-rule (clean-up-files-by-extension file-path extra-extension ...)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (displayln (exn-message e)))])
    (delete-file (path-replace-extension file-path extra-extension))  ...))


;; graph->bitmap: graph optional(string) optional(string) optional(boolean) -> image
;; Converts a graph to an image
;; If clean is false then the dot and png files are not deleted
(define (graph->bitmap graph #:directory [save-dir SAVE-DIR] #:filename[filename "__tmp__"] #:clean[delete-files #t])
  (define-values (img path)
    ((compose1 png->bitmap dot->png graph->dot) graph save-dir filename))
  (when delete-files
    (clean-up-files-by-extension path #".png" #".dot"))
  img)

;; graph->svg: graph string string optional(boolean) -> path
;; Converts a graph to a svg and returns the path to the svg image
;; If clean is false then the dot file is not deleted
(define (graph->svg graph save-dir filename #:clean[delete-files #t])
  (define svg-path ((compose1 dot->svg graph->dot) graph save-dir filename))
  (when delete-files
    (clean-up-files-by-extension svg-path #".dot"))
  svg-path)

;; graph->svg: graph string string optional(boolean) -> path
;; Converts a graph to a png and returns the path to the png image
;; If clean is false then the dot file is not deleted
(define (graph->png graph save-dir filename #:clean[delete-files #t])
  (define png-path ((compose1 dot->png graph->dot) graph save-dir filename))
  (when delete-files
    (clean-up-files-by-extension png-path #".dot"))
  png-path)

;; hash->str: hash hash Optional(string) -> string
;; Purpose: converts the hash to a graphviz string
(define (hash->str hash fmtr (spacer ", "))
  (define (fmt-val val)
    (if (boolean? val)
        (if val 'true 'false)
        val))
  (define (key-val->string key value)
    (define fmtr-fun (hash-ref fmtr key #f))
    (if fmtr-fun
        (format "~s=~s" key (fmtr-fun value))
        (format "~s=~s" key (if (equal? key 'label)
                                (format "~a" (fmt-val value))
                                (fmt-val value)))))
  (string-join (hash-map hash key-val->string) spacer))

;; Helper function to convert a value to a string
(define (stringify-value input)
  (cond [(number? input) (number->string input)]
        [(string? input) input]
        [[symbol? input] (symbol->string input)]
        [else (error "Graphviz internal error: Unable to convert to string")]))



(module+ test
  (require rackunit)

  (test-equal? "Nodes with spaces in names"
               (add-nodes (create-graph 'test) '(|A A| B C D E-1))
               (graph
                'test
                (list
                 (node 'A__A #hash((color . "black") (label . "A A") (shape . "circle")))
                 (node 'B #hash((color . "black") (label . "B") (shape . "circle")))
                 (node 'C #hash((color . "black") (label . "C") (shape . "circle")))
                 (node 'D #hash((color . "black") (label . "D") (shape . "circle")))
                 (node 'E1 #hash((color . "black") (label . "E-1") (shape . "circle"))))
                '()
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))


  (test-equal? "Edges with dashes"
               (add-edges (add-nodes (create-graph 'test) '(A B C D))
                          '((A a B) (B b B) (B c-1 D)))
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
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))

  (test-equal? "Node with custom label"
               (add-node (create-graph 'test) 'A #:atb (hash 'label "AA"))
               (graph
                'test
                (list (node 'A #hash((label . "AA"))))
                '()
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))


  (test-equal? "Subgraphs with edges"
               (add-edges (add-subgraph (add-nodes (create-graph 'test) '(A B))
                                        (add-nodes (create-subgraph #:name 'cluster1) '(AA BB)))
                          '((A a B)
                            (AA a B)
                            (cluster1 a B)))
               (graph
                'test
                (list
                 (node 'A #hash((color . "black") (label . "A") (shape . "circle")))
                 (node 'B #hash((color . "black") (label . "B") (shape . "circle"))))
                (list
                 (edge 'AA 'B #hash((fontsize . 15) (label . (a)) (ltail . cluster1)))
                 (edge 'AA 'B #hash((fontsize . 15) (label . (a))))
                 (edge 'A 'B #hash((fontsize . 15) (label . (a)))))
                (list
                 (subgraph
                  'cluster1
                  (list
                   (node 'AA #hash((color . "black") (label . "AA") (shape . "circle")))
                   (node 'BB #hash((color . "black") (label . "BB") (shape . "circle"))))
                  '()
                  '()
                  #hash()))
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))

  ;; --- Exception Checks Below ---
  (check-exn
   exn:fail?
   (lambda () (add-nodes (add-nodes (create-graph 'test) '(A B C D)) '(A))))
  
  (check-exn
   exn:fail?
   (lambda ()
     (define sg (create-subgraph #:name 'A))
     (add-subgraph (add-nodes (create-graph 'test) '(A B C D)) sg)))



  #;(displayln
   (graph 'dgraph
          (list (node 'S0 (make-hash '(['color . 'violet]
                                       ['font . 'Sans]
                                       ['fontcolor . 'black]
                                       ['label . 'S]
                                       ['shape . 'hexagon]))))
          (list (edge 'A2 'a2 (make-hash '(['arrowhead . 'none]
                                           ['label . '()]
                                           ['style . 'invisible])))
                (edge 'S2 'A2 (make-hash '(['color . 'red]
                                           ['fontsize . 12]
                                           ['label . '()]
                                           ['style . 'solid]))))
          '()
          DEFAULT-FORMATTERS
          (hash))
   )

  ) ;; end module+ test
