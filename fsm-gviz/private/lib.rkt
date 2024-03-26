#lang typed/racket
;(require racket/draw)
(require typed/racket/unsafe)
(require/typed 2htdp/image
               [bitmap/file (-> Path-String Any)]
               [image? (-> Any Boolean)]
               )

;; Required at the moment since unfortunately racket contract system doesn't play nice with the contracts made with TR
 (unsafe-require/typed racket/hash
                [hash-union (All (a b) (->* ((Immutable-HashTable a b) (HashTable a b)) (#:combine (-> b b b) #:combine/key (-> a b b b)) (Immutable-HashTable a b)))]
                )
 (require/typed "dot.rkt"
                [find-tmp-dir (-> Path)]
                [find-dot (-> (U Path False))]
                )
#| This file handles converting graphs to the dot file equivalent |#

(provide
 formatters
 node
 edge
 subgraph
 graph
 graph?
 create-graph
 create-subgraph
 create-formatters
 add-node
 add-nodes
 add-edge
 add-subgraph
 add-edges
 graph->bitmap
 graph->svg
 graph->dot
 graph->dot-bytes
 graph->png
 graph->str
 stringify-value
 dot->png
 
 #;(contract-out
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
                 (atb (hash/c symbol? any/c)))]
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
(: DEFAULT-GRAPH (Immutable-HashTable Symbol Any))
(define DEFAULT-GRAPH (hash 'rankdir "LR"))


;

(: DEFAULT-EDGE (Immutable-HashTable Symbol Any))
(define DEFAULT-EDGE (hash 'fontsize 15))

(: DEFAULT-NODE (Immutable-HashTable Symbol Any))
(define DEFAULT-NODE (hash 'color "black" 'shape "circle"))

(: DEFAULT-EDGE-LABEL-FMTR (-> Any String))
(define (DEFAULT-EDGE-LABEL-FMTR lst)
  (if (list? lst)
      (string-join (map (lambda (v) (format "~a" v)) (reverse lst)) ", ")
      (format "~a" lst)
      )
  )

(: SAVE-DIR Path)
(define SAVE-DIR (find-tmp-dir))




; A structure the represents a node in the dot language
(struct node (
              [name : Symbol]
              [atb : (Immutable-HashTable Symbol Any)]
              )
  #:transparent
  #:mutable)

; A structure the represents a edgs in the dot language
(struct edge ([start-node : Symbol]
              [end-node : Symbol]
              [atb : (Immutable-HashTable Symbol Any)])
  #:mutable
  #:transparent)

; A structure the represents a subgraph in the dot language
(struct subgraph ([name : (U Null Symbol)]
                  [node-list : (Listof node)]
                  [edge-list : (Listof edge)]
                  [subgraph-list : (Listof subgraph)]
                  [atb : (Immutable-HashTable Symbol Any)])
  #:mutable
  #:transparent)

;; formatters contain custom formatting functions for attributes
(struct formatters ([graph-hash : (Immutable-HashTable Symbol (-> Any String))]
                    [node-hash : (Immutable-HashTable Symbol (-> Any String))]
                    [edge-hash : (Immutable-HashTable Symbol (-> Any String))]) #:transparent)

(: formatters-hash (-> Symbol (-> Any String) (Immutable-HashTable Symbol (-> Any String))))
(define (formatters-hash key frmt-fun) (hash key frmt-fun))

(: DEFAULT-FORMATTERS formatters)
(define DEFAULT-FORMATTERS (formatters
                            (hash)
                            (hash)
                            ;(hash 'label DEFAULT-EDGE-LABEL-FMTR)
                            (formatters-hash 'label DEFAULT-EDGE-LABEL-FMTR)
                            )
  )

; A structure the represents a digraph in the dot language
(struct graph ([name : Symbol]
               [node-list : (Listof node)]
               [edge-list : (Listof edge)]
               [subgraph-list : (Listof subgraph)]
               [fmtrs : formatters]
               [atb : (Immutable-HashTable Symbol Any)])
  #:mutable
  #:transparent)

;; custom contract for a graph type. A graph type is either a 
;; graph or subgraph struct
(: graph-type? (-> (U graph subgraph Any) Boolean))
(define (graph-type? a)
  (or (graph? a) (subgraph? a)))


;; hash->str: hash hash Optional(string) -> string
;; Purpose: converts the hash to a graphviz string
(: hash->str (->* ((Immutable-HashTable Symbol Any) (Immutable-HashTable Symbol (-> Any String))) (String) String))
(define (hash->str hash fmtr (spacer ", "))

  ;; Purpose: Formats a possibly boolean value to a symbol
  (: fmt-val (-> (U Boolean Any) Any))
  (define (fmt-val val)
    (if (boolean? val)
        (if val
            'true
            'false)
        val
        )
    )
  
  (: key-val->string (-> Symbol Any String))
  (define (key-val->string key value)
    ;(: fmtr-fun (U Procedure Boolean) : Procedure)
    (define fmtr-fun (hash-ref fmtr key #f))
    (if (procedure? fmtr-fun)
        (format "~s=~s" key (fmtr-fun value))
        (format "~s=~s" key (if (equal? key 'label)
                                (format "~a" (fmt-val value))
                                (fmt-val value)
                                )
                )
        )
    )
  (string-join (hash-map hash key-val->string) spacer)
  )


;; hash->str: hash hash Optional(string) -> string
;; Purpose: converts the hash to a graphviz string
(: hash->bytes (->* ((Immutable-HashTable Symbol Any) (Immutable-HashTable Symbol (-> Any String))) (String) Bytes))
(define (hash->bytes hash fmtr (spacer ", "))

  ;; Purpose: Formats a possibly boolean value to a symbol
  (: fmt-val (-> (U Boolean Any) Any))
  (define (fmt-val val)
    (if (boolean? val)
        (if val
            'true
            'false)
        val
        )
    )
  
  (: key-val->bytes (-> Symbol Any Bytes))
  (define (key-val->bytes key value)
    ;(: fmtr-fun (U Procedure Boolean) : Procedure)
    (define fmtr-fun (hash-ref fmtr key #f))
    (if (procedure? fmtr-fun)
        (let ([o (open-output-bytes)])
          (fprintf o "~s=~s" key (fmt-val value))
          (get-output-bytes o))
        ;(format "~s=~s" key (fmtr-fun value))
        (let ([o (open-output-bytes)])
          (fprintf o "~s=~s" key (if (equal? key 'label)
                                     (let ([o (open-output-string)])
                                       (fprintf o "~a" (fmt-val value))
                                       (get-output-string o))
                                     (fmt-val value))
                   )
          (get-output-bytes o))
        #;(format "~s=~s" key (if (equal? key 'label)
                                (let ([o (open-output-string)])
                                  (fprintf o "~a" (fmt-val value))
                                  (get-output-string o))
                                ;(format "~a" (fmt-val value))
                                (fmt-val value)
                                )
                )
        )
    )
  (bytes-join (hash-map hash key-val->bytes) (string->bytes/locale spacer))
  )



;; node->str: node node -> string
;; returns the graphviz representation of a node as a string

(: node->str (-> node (Immutable-HashTable Symbol (-> Any String)) String))
(define (node->str node fmtr-node)
  (format "    ~s [~a];\n"
          (node-name node)
          (hash->str (node-atb node) fmtr-node)))


(: node->bytes (-> node (Immutable-HashTable Symbol (-> Any String)) Bytes))
(define (node->bytes node fmtr-node)
  (let ([o (open-output-bytes)])
          (fprintf o "    ~s [~a];\n" (node-name node) (hash->str (node-atb node) fmtr-node))
          (get-output-bytes o))
  #;(format "    ~s [~a];\n"
          (node-name node)
          (hash->str (node-atb node) fmtr-node)))

;; edge->str: edge hash -> string
;; returns the graphviz representation of a edge as a string
(: edge->str (-> edge (Immutable-HashTable Symbol (-> Any String)) String))
(define (edge->str edge fmtr)
  (format "    ~s -> ~s [~a];\n"
          (edge-start-node edge)
          (edge-end-node edge)
          (hash->str (edge-atb edge) fmtr)))

(: edge->bytes (-> edge (Immutable-HashTable Symbol (-> Any String)) Bytes))
(define (edge->bytes edge fmtr)
  (let ([o (open-output-bytes)])
    (fprintf o "    ~s -> ~s [~a];\n" (edge-start-node edge) (edge-end-node edge) (hash->str (edge-atb edge) fmtr))
    (get-output-bytes o))
  #;(format "    ~s -> ~s [~a];\n"
          (edge-start-node edge)
          (edge-end-node edge)
          (hash->str (edge-atb edge) fmtr)
          )
  )


;; subgraph->str: subgraph formatters -> string
;; returns the string representation of a subgraph
(: subgraph->str (-> subgraph formatters String))
(define (subgraph->str sg fmtrs)
  (: name String)
  (define name (let [
                     (sg-name (subgraph-name sg))
                     ]
                 (if (null? sg-name)
                     ""
                     (symbol->string sg-name)
                     )
                 )
    )
  (string-append (format "    subgraph ~s {\n" name) 
                 (format "        ~a;\n" (hash->str (subgraph-atb sg) (formatters-graph-hash fmtrs) ";\n        "))
                 (foldl (lambda ([n : node] [a : String]) (string-append a "    " (node->str n (formatters-node-hash fmtrs))))
                        ""
                        (subgraph-node-list sg))
                 (foldl (lambda ([e : edge] [a : String]) (string-append a "    " (edge->str e (formatters-edge-hash fmtrs))))
                        ""
                        (subgraph-edge-list sg))
                 (foldl (lambda ([e : subgraph] [a : String]) (string-append a "    " (subgraph->str e fmtrs)))
                        ""
                        (subgraph-subgraph-list sg))
                 "    }\n"))


(: subgraph->bytes (-> subgraph formatters Bytes))
(define (subgraph->bytes sg fmtrs)
  (: name String)
  (define name (let [
                     (sg-name (subgraph-name sg))
                     ]
                 (if (null? sg-name)
                     ""
                     (symbol->string sg-name)
                     )
                 )
    )
  (let [
        (fourspaces (string->bytes/locale "    "))
        ]
    (bytes-append (let ([o (open-output-bytes)])
                  (fprintf o "    subgraph ~s {\n" name)
                  (get-output-bytes o))
                #;(format "    subgraph ~s {\n" name) 
                 (let ([o (open-output-bytes)])
                  (fprintf o  "        ~a;\n" (hash->bytes (subgraph-atb sg) (formatters-graph-hash fmtrs) ";\n        "))
                  (get-output-bytes o))
                 #;(format "        ~a;\n" (hash->str (subgraph-atb sg) (formatters-graph-hash fmtrs) ";\n        "))
                 (foldl (lambda ([n : node] [a : Bytes]) (bytes-append a fourspaces (node->bytes n (formatters-node-hash fmtrs))))
                        #""
                        (subgraph-node-list sg))
                 (foldl (lambda ([e : edge] [a : Bytes]) (bytes-append a fourspaces (edge->bytes e (formatters-edge-hash fmtrs))))
                        #""
                        (subgraph-edge-list sg))
                 (foldl (lambda ([e : subgraph] [a : Bytes]) (bytes-append a fourspaces (subgraph->bytes e fmtrs)))
                        #""
                        (subgraph-subgraph-list sg))
                 (string->bytes/locale "    }\n")
                 )
    )
  )

;; graph->str: graph -> string
;; returns the graphviz representation of a graph as a string
(: graph->str (-> graph String))
(define (graph->str g)
  (: name String)
  (define name (format "digraph ~s {\n" (graph-name g)))
  (: fmtrs formatters)
  (define fmtrs (graph-fmtrs g))
  (string-append
   name
   (format "    ~a;\n" (hash->str (graph-atb g) (formatters-graph-hash fmtrs) ";\n    "))
   (foldl (lambda ([n : node] [a : String]) (string-append a (node->str n (formatters-node-hash fmtrs))))
          ""
          (graph-node-list g))
   (foldl (lambda ([e : edge] [a : String]) (string-append a (edge->str e (formatters-edge-hash fmtrs))))
          ""
          (graph-edge-list g))
   (foldl (lambda ([e : subgraph] [a : String]) (string-append a (subgraph->str e fmtrs)))
          ""
          (graph-subgraph-list g))
   "}"))

(: graph->bytes (-> graph Bytes))
(define (graph->bytes g)
  (: name Bytes)
  (define name (let ([o (open-output-bytes)])
                  (fprintf o "digraph ~s {\n" (graph-name g))
                  (get-output-bytes o))
  #;(format "digraph ~s {\n" (graph-name g))
  )
  (: fmtrs formatters)
  (define fmtrs (graph-fmtrs g))
  (bytes-append
   name
   (let ([o (open-output-bytes)])
     (fprintf o "    ~a;\n" (hash->bytes (graph-atb g) (formatters-graph-hash fmtrs) ";\n    "))
     (get-output-bytes o))
   #;(format "    ~a;\n" (hash->bytes (graph-atb g) (formatters-graph-hash fmtrs) ";\n    "))
   (foldl (lambda ([n : node] [a : Bytes]) (bytes-append a (node->bytes n (formatters-node-hash fmtrs))))
          #""
          (graph-node-list g))
   (foldl (lambda ([e : edge] [a : Bytes]) (bytes-append a (edge->bytes e (formatters-edge-hash fmtrs))))
          #""
          (graph-edge-list g))
   (foldl (lambda ([e : subgraph] [a : Bytes]) (bytes-append a (subgraph->bytes e fmtrs)))
          #""
          (graph-subgraph-list g))
   (string->bytes/locale "}")
   )
  )


(: combine-key (All (a b) (-> a b b b)))
(define (combine-key key value1 value2) value1)




;; combine :: hash hash -> hash
;; combines the two hashes together. If the hashs have the same key the first hash 
;; is used.
(: combine-formatters (-> (Immutable-HashTable Symbol (-> Any String)) (Immutable-HashTable Symbol (-> Any String)) (Immutable-HashTable Symbol (-> Any String))))
(define (combine-formatters v1 v2) ((inst hash-union Symbol (-> Any String)) v1 v2 #:combine/key (lambda ([k : Symbol] [_v1 : (-> Any String)] [_v2 : (-> Any String)]) _v1)))

(: combine (-> (Immutable-HashTable Symbol Any) (Immutable-HashTable Symbol Any) (Immutable-HashTable Symbol Any)))
(define (combine v1 v2) ((inst hash-union Symbol Any) v1 v2 #:combine/key (lambda ([k : Symbol] [_v1 : Any] [_v2 : Any]) _v1)))







;; create-graph: symbol -> graph
;; name: The name of the graph
;; Purpose: Creates a Graph with the given name
(: create-graph (->* (Symbol) (#:fmtrs formatters #:atb (Immutable-HashTable Symbol Any)) graph))
(define (create-graph name #:fmtrs(fmtrs DEFAULT-FORMATTERS) #:atb [atb DEFAULT-GRAPH])
  (graph name
         '()
         '()
         '()
         (formatters
          (combine-formatters (formatters-graph-hash fmtrs)
                              (formatters-graph-hash DEFAULT-FORMATTERS))
          (combine-formatters (formatters-node-hash fmtrs)
                              (formatters-node-hash DEFAULT-FORMATTERS))
          (combine-formatters (formatters-edge-hash fmtrs)
                              (formatters-edge-hash DEFAULT-FORMATTERS)))
         (combine atb DEFAULT-GRAPH)
         )
  )

(: gviz-hash (-> (Immutable-HashTable Symbol Any)))
(define (gviz-hash) (hash))

(: formatter-hash (-> (Immutable-HashTable Symbol (-> Any String))))
(define (formatter-hash) (hash))

(: create-formatters (->* () (#:graph (Immutable-HashTable Symbol (-> Any String)) #:node (Immutable-HashTable Symbol (-> Any String)) #:edge (Immutable-HashTable Symbol (-> Any String))) formatters))
(define (create-formatters #:graph[graph (formatter-hash)]
                           #:node[node (formatter-hash)]
                           #:edge[edge (formatter-hash)]
                           )
  (formatters graph node edge))

;; Helper function to convert a value to a string
(: stringify-value (-> (U Number String Symbol Any) String))
(define (stringify-value input)
  (cond [(number? input) (number->string input)]
        [(string? input) input]
        [[symbol? input] (symbol->string input)]
        [else (error "Graphviz internal error: Unable to convert to string")]))

; clean-string: symbol -> symbol
; Purpose: cleans the string to only have valid dot language id symbols
; https://graphviz.org/doc/info/lang.html
(: clean-invalid-dot-lang (-> (U Symbol Null) Symbol))
(define (clean-invalid-dot-lang s)
  (string->symbol (string-replace (string-replace (stringify-value s) "-" "") " " "__")))

;; create-subgraph :: subgraph
;; subgraphs can have a optional name, but it is not required
(: create-subgraph (->* () (#:name (U Symbol Null) #:atb (Immutable-HashTable Symbol Any)) subgraph))
(define (create-subgraph #:name [name null] #:atb [atb (gviz-hash)])
  (subgraph (if (null? name)
                name
                (clean-invalid-dot-lang name)
                )
            '()
            '()
            '()
            atb))



;; existing-name graph | subgraph string -> boolean
;; True when the name already exists
(: existing-name? (-> (U graph subgraph) Symbol (U Boolean (Listof subgraph) (Listof node))))
(define (existing-name? parent name)
  (: cleaned-name Symbol)
  (define cleaned-name (clean-invalid-dot-lang name))




  
  (: check-subgraph (-> Symbol subgraph (U Boolean (Listof subgraph))))
  (define (check-subgraph n sg)
    (or (equal? (subgraph-name sg) n)
        (member n (subgraph-subgraph-list sg) check-subgraph
                )
        )
    )
  
  (if (graph? parent)
      (or (member cleaned-name (graph-node-list parent) (lambda ([v : Symbol] [n : node]) (equal? (node-name n) v)))
          (member cleaned-name (graph-subgraph-list parent) check-subgraph)
          )
      (check-subgraph name parent)
      )
  )

;; add-node: graph | subgraph string Optional(hash-map) -> graph | subgraph
;; Purpose: adds a node to the given graph
(: add-node (->* ((U graph subgraph) Symbol) (#:atb (Immutable-HashTable Symbol Any)) (U graph subgraph)))
(define (add-node parent name #:atb [atb DEFAULT-NODE])
  (if (existing-name? parent name)
      (error "[Duplicate Name]: Node name already exists on the graph")
      (local [
              (: name-lambda (-> Symbol))
              (define (name-lambda) name)
              (: new-node node)
              (define new-node (node (clean-invalid-dot-lang name) (hash-set atb 'label (stringify-value (hash-ref atb 'label name-lambda)))))
              ]
        (if (graph? parent)
            (graph
             (graph-name parent)
             (cons new-node (graph-node-list parent))
             (graph-edge-list parent)
             (graph-subgraph-list parent)
             (graph-fmtrs parent)
             (graph-atb parent)
             )
            (subgraph (subgraph-name parent)
                      (cons new-node (subgraph-node-list parent))
                      (subgraph-edge-list parent)
                      (subgraph-subgraph-list parent)
                      (subgraph-atb parent)
                      )
            )
        )
      )
  )


;; add-nodes: graph | subgraph listof(symbol) Optional(hash-map) -> graph | subgraph
;; Purpose: adds the list of nodes to the given graph
(: add-nodes (->* ((U graph subgraph) (Listof Symbol)) (#:atb (Immutable-HashTable Symbol Any)) (U graph subgraph)))
(define (add-nodes parent names #:atb [atb DEFAULT-NODE])
  (: nodes-to-add (Listof node))
  (define nodes-to-add 
    (map (lambda ([n : Symbol])
           (when (existing-name? parent n)
             (error "[Duplicate Name]: Edge name already exists on the graph"))
           (node (clean-invalid-dot-lang n) (hash-set atb 'label (stringify-value n))))
         names))
  (if (graph? parent)
      (graph
       (graph-name parent)
       (append nodes-to-add (graph-node-list parent))
       (graph-edge-list parent)
       (graph-subgraph-list parent)
       (graph-fmtrs parent)
       (graph-atb parent))
      (subgraph 
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
(: add-edge (->* ((U graph subgraph) Any Symbol Symbol) (#:atb (Immutable-HashTable Symbol Any)) (U graph subgraph)))
(define (add-edge parent val start-node end-node #:atb [atb DEFAULT-EDGE])
  (: start Symbol)
  (define start (clean-invalid-dot-lang start-node))
  
  (: end Symbol)
  (define end (clean-invalid-dot-lang end-node))

  (: is-subgraph? (-> Symbol (U graph subgraph) (U Boolean (Listof subgraph))))
  (define (is-subgraph? val parent)
    (cond
      [(graph? parent) (member val (graph-subgraph-list parent) is-subgraph?)]
      [(equal? val (subgraph-name parent)) #t]
      [else (member val (subgraph-subgraph-list parent) is-subgraph?)]
      )
    )
  
  (: edge-eq? (-> edge Boolean))
  (define (edge-eq? e) (and (equal? start (edge-start-node e))
                            (equal? end (edge-end-node e))))
  
  (: add/update-in-list (-> (Listof edge) (Listof edge)))
  (define (add/update-in-list lst)
    (: edge-index (U Exact-Nonnegative-Integer #f))
    (define edge-index (index-where lst edge-eq?))
    (cond
      [(not edge-index)
       ;; If the node is a edge then we need to add lhead, or ltail attributes
       ;; and point it to a random node on the subgraph 
       (define is-subgraph-start? (is-subgraph? start parent))
       (define is-subgraph-end? (is-subgraph? end parent))
       (: h1 (Immutable-HashTable Symbol Any))
       (define h1 (hash-union (if is-subgraph-start? (hash 'ltail start) (gviz-hash))
                              (if is-subgraph-end? (hash 'lhead end) (gviz-hash))))
       (cons (edge (if (and (cons? is-subgraph-start?)
                            (not (empty? is-subgraph-start?))
                            )
                       (let [
                             (sg-start-nodes (subgraph-node-list (car is-subgraph-start?)))
                             ]
                         (if (not (empty? sg-start-nodes))
                             (node-name (car sg-start-nodes))
                             (error "something has gone terribly wrong")
                             )
                         )
                       start)
                   (if (and (cons? is-subgraph-end?)
                            (not (empty? is-subgraph-end?))
                            )
                       (let [
                             (sg-end-nodes (subgraph-node-list (car is-subgraph-end?)))
                             ]
                         (if (not (empty? sg-end-nodes))
                             (node-name (car sg-end-nodes))
                             (error "something has gone terribly wrong")
                             )
                         )
                       end)
                   (hash-union h1 (hash-set atb 'label (list val))))
             lst)]
      [else 
       (list-update lst
                    edge-index
                    (lambda ([e : edge])
                      (edge
                       (edge-start-node e)
                       (edge-end-node e)
                       (hash-set (edge-atb e)
                                 'label
                                 (cons val (hash-ref (edge-atb e) 'label))))))]))
    
  (if (graph? parent)
      (graph 
       (graph-name parent)
       (graph-node-list parent)
       (add/update-in-list (graph-edge-list parent))
       (graph-subgraph-list parent)
       (graph-fmtrs parent)
       (graph-atb parent))
      ;; I think this path was never actually called????
      (subgraph 
       (subgraph-name parent)
       (subgraph-node-list parent)
       (add/update-in-list (subgraph-edge-list parent))
       (subgraph-subgraph-list parent) ;; this is entirely a guess on what is supposed to go here
       (subgraph-atb parent))))


;; add-edges: graph | subgraph listof(symbol symbol any/c) Optional(hash-map) -> graph | subgraph
;; Purpose: adds this list of edges to the graph
;; NOTE: This function assumes that the node exists in the graph structure
;; NOTE: a edges label is a list since we squash all edges between the same nodes
;; into a single edge
(: add-edges (->* ((U graph subgraph) (Listof (List Symbol Symbol Any))) (#:atb (Immutable-HashTable Symbol Any)) (U graph subgraph)))
(define (add-edges parent edgs #:atb [atb DEFAULT-EDGE])
  (foldl (lambda ([e : (List Symbol Symbol Any)] [a : (U graph subgraph)])
           (match-define (list start-node edge-val end-node) e)
           (add-edge a edge-val start-node end-node #:atb atb)) parent edgs)
  )


;; add-subgraph :: graph | subgraph subgraph -> graph | subgraph
;; adds a subgraph to either a graph or subgraph
(: add-subgraph (-> (U graph subgraph) subgraph (U graph subgraph)))
(define (add-subgraph parent sg)
  (let
      [
       (sg-name (subgraph-name sg))
       ]
  (if (and (not (null? sg-name))
           (existing-name? parent sg-name)
           )
      (error "[Duplicate Name]: Name of subgraph already exists on the graph")
      (if (graph? parent)
          (graph 
           (graph-name parent)
           (graph-node-list parent)
           (graph-edge-list parent)
           (cons sg (graph-subgraph-list parent))
           (graph-fmtrs parent)
           (graph-atb parent))
          (subgraph 
           (subgraph-name parent)
           (subgraph-node-list parent)
           (subgraph-edge-list parent)
           (cons sg (subgraph-subgraph-list parent))
           (subgraph-atb parent))
          )
      )
    )
  )



;; graph->dot: graph path string -> path
;; Purpose: writes graph to the specified file
(: graph->dot (-> graph Path String Path))
(define (graph->dot graph save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (call-with-output-file dot-path
    #:exists 'replace
    (lambda ([out : Output-Port])
      (displayln (graph->str graph) out))
    )
  dot-path)

#|
(: graph->bytes1 (-> graph Bytes))
(define (graph->bytes1 g)
  (: name String)
  (define name (format "digraph ~s {\n" (graph-name g)))
  (: fmtrs formatters)
  (define fmtrs (graph-fmtrs g))
  (string->bytes/locale
   (string-append
   name
   (format "    ~a;\n" (hash->str (graph-atb g) (formatters-graph-hash fmtrs) ";\n    "))
   (foldl (lambda ([n : node] [a : String]) (string-append a (node->str n (formatters-node-hash fmtrs))))
          ""
          (graph-node-list g))
   (foldl (lambda ([e : edge] [a : String]) (string-append a (edge->str e (formatters-edge-hash fmtrs))))
          ""
          (graph-edge-list g))
   (foldl (lambda ([e : subgraph] [a : String]) (string-append a (subgraph->str e fmtrs)))
          ""
          (graph-subgraph-list g))
   "}")
   )
  )
|#

(: graph->dot-bytes (-> graph Path String Path))
(define (graph->dot-bytes graph save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (define dotfile (open-output-file #:exists 'replace dot-path))
  (write-bytes (graph->bytes graph) dotfile)
  (close-output-port dotfile)
  dot-path
  )
  

;; dot->output-fmt: symbolof(file-format) path -> path
;; Purpose: converts a dot file to the specified output. The new file is saved in directory
;; of the provided file. The name of the new file is the same as input with a different extension
;; NOTE: For possiable formats see: https://graphviz.org/docs/outputs/svg/
(: dot->output-fmt (-> Symbol Path Path))
(define (dot->output-fmt fmt dot-path)
  (define png-path (path-replace-extension dot-path (format ".~s" fmt)))
  (define dot-executable-path (find-dot))
  (if (path? dot-executable-path)
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
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")
      )
  png-path)


;; dot->png: path -> path
;; Purpose: converts a dot file to a png. The png file is saved in the directory
;; of the provided path
(: dot->png (-> Path Path))
(define dot->png (curry dot->output-fmt 'png))

;; dot->svg: path -> path
;; Purpose: converts a dot file to a svg. The svg file is saved in the directory
;; of the provided path
(: dot->svg (-> Path Path))
(define dot->svg (curry dot->output-fmt 'svg))


;; png->bitmap: path -> values(img path)
;; returns the image and the path to the file that was used to create the image
(: png->bitmap (-> Path (Values Any Path)))
(define (png->bitmap path)
  (values (bitmap/file path) path))

;; clean-up-files-by-extension :: filepath string/regex
;; Given a filepath will delete the files by the given extension. For example
;; if you have files test.png test.svg test.dot
;;    (clean-up-files-by-extension test-path #".dot" #".png")
;; will delete test.png and test.dot 
(define-syntax-rule (clean-up-files-by-extension file-path extra-extension ...)
  (with-handlers ([exn:fail:filesystem? (lambda ([e : exn]) (displayln (exn-message e)))])
    (delete-file (path-replace-extension file-path extra-extension))  ...))


;; graph->bitmap: graph optional(string) optional(string) optional(boolean) -> image
;; Converts a graph to an image
;; If clean is false then the dot and png files are not deleted
(: graph->bitmap (->* (graph) (#:directory Path #:filename String #:clean Boolean) Any))
(define (graph->bitmap graph #:directory [save-dir SAVE-DIR] #:filename[filename "__tmp__"] #:clean[delete-files #t])
  (define-values (img path)
    ;((compose1 png->bitmap dot->png graph->dot) graph save-dir filename)
    (png->bitmap (dot->png (graph->dot graph save-dir filename)))
    )
  (when delete-files
    (clean-up-files-by-extension path #".png" #".dot"))
  img)

;; graph->svg: graph string string optional(boolean) -> path
;; Converts a graph to a svg and returns the path to the svg image
;; If clean is false then the dot file is not deleted
(: graph->svg (->* (graph Path String) (#:clean Boolean) Path))
(define (graph->svg graph save-dir filename #:clean[delete-files #t])
  ;(define svg-path ((compose1 dot->svg graph->dot) graph save-dir filename))
  (define svg-path (dot->svg (graph->dot graph save-dir filename)))
  (when delete-files
    (clean-up-files-by-extension svg-path #".dot"))
  svg-path)

;; graph->svg: graph string string optional(boolean) -> path
;; Converts a graph to a png and returns the path to the png image
;; If clean is false then the dot file is not deleted
(: graph->png (->* (graph Path String) (#:clean Boolean) Path))
(define (graph->png graph save-dir filename #:clean[delete-files #t])
  ;(define png-path ((compose1 dot->png graph->dot) graph save-dir filename))
  (define png-path (dot->png (graph->dot graph save-dir filename)))
  (when delete-files
    (clean-up-files-by-extension png-path #".dot"))
  png-path)


#|
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
                DEFAULT-GRAPH))


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
                DEFAULT-GRAPH))

  (test-equal? "Node with custom label"
               (add-node (create-graph 'test) 'A #:atb (hash 'label "AA"))
               (graph
                'test
                (list (node 'A #hash((label . "AA"))))
                '()
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH))


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
                DEFAULT-GRAPH))

  ;; --- Exception Checks Below ---
  (check-exn
   exn:fail?
   (lambda () (add-nodes (add-nodes (create-graph 'test) '(A B C D)) '(A))))
  
  (check-exn
   exn:fail?
   (lambda ()
     (define sg (create-subgraph #:name 'A))
     (add-subgraph (add-nodes (create-graph 'test) '(A B C D)) sg)))

  ) ;; end module+ test
|#