#lang scribble/manual
@(require (for-label racket 2htdp/image))


@title{FSM Graphviz Library}
@author["Joshua Schappel" "Marco T. Morazán"]

@defmodule["interface.rkt"]

A library for creating Graphviz graphs that can be converted to the Dot Language or supported image types.


@table-of-contents[]


@section[#:tag "design"]{Overall Design}
The graphviz library is designed to be its own entity. This means that both @emph{fsm-core} and @emph{fsm-gui}
can work without the graphviz library. Below is a diagram of how the library interfaces with the rest of fsm.

@centered{@image[#:suffixes @list[".png"]]{scribImgs/gvizInterface}}

@section[#:tag "functions"]{Library Functions}
Below are all the exported library functions from @racket["interface.rkt"].
@subsection{Creating Graphs}
@defproc[(create-graph [name string?]
                       [#:fmtrs custom-formatters formatters? DEFAULT-FORMATTERS]
                       [#:atb graph-attributes (hash/c symbol? any/c) (hash 'rankdir "LR")])
         graph?]{

 Creates a graph where the @racket[name] is the name of the generated @emph{dot language} graph.

 @racket[graph-attributes] are a hash where the key is a symbol representing a @hyperlink["https://graphviz.org/docs/graph/"]{graph attribute}
 and the value is the value for that attribute.

 @racket[custom-formatters] are attribute level transformer functions that can be subscribed to a graph
 in order to generate custom values for attributes. These are frequently used to format how many characters
 of a label can exist on a single line for a label. For more information on formatters see @secref{formatters}.
 The @racket[DEFAULT-FORMATTERS] are:
 @codeblock{
(define (DEFAULT-EDGE-LABEL-FMTR lst)
  (string-join (map (lambda (v) (format "~a" v)) (reverse lst)) ", "))

(formatters (hash)
            (hash)
            (hash 'label DEFAULT-EDGE-LABEL-FMTR))
}
}


@defproc[(add-node [graph graph?]
                   [name symbol?]
                   [#:atb node-attributes (hash/c symbol? any/c) DEFAULT-NODE])
                   graph?]{

 Adds a node to the provided @racket[graph] where the @racket[name] is the name of the generated @emph{DOT} node and sets
 the nodes label attribute to the name. Note: Since the @emph{DOT} language does not allow @racket{-} characters
 in node names the dashes are omitted but are still provided for the label.

 @racket[node-attributes] are a hash where the key is a symbol representing a @hyperlink["https://graphviz.org/docs/nodes/"]{node attribute}
 and the value is the value for that attribute. The @racket[DEFAULT-NODE] attributes are:
 @codeblock{
 (hash 'color "black" 'shape "circle")
}
}



@defproc[(add-nodes [graph graph?]
                    [names (listof symbol?)]
                    [#:atb node-attributes (hash/c symbol? any/c) DEFAULT-NODE])
                    graph?]{
 Adds the list of nodes to the provided @racket[graph] where each value in the @racket[names] is the name of the generated @emph{DOT} node and sets
 the nodes label attribute to the name. Note: Since the @emph{DOT} language does not allow @racket{-} characters
 in node names the dashes are omitted, but are still provided for the label.

 @racket[node-attributes] are a hash where the key is a symbol representing a @hyperlink["https://graphviz.org/docs/nodes/"]{node attribute}
 and the value is the value for that attribute. They are applied to each of the nodes in the list. The @racket[DEFAULT-NODE] attributes are:
 @codeblock{
 (hash 'color "black" 'shape "circle")
}
Example usage:
@codeblock{
(add-nodes (create-graph 'test) '(A B C D E-1))
}
}


@defproc[(add-edge [graph graph?]
                   [value any/c]
                   [start-node symbol?]
                   [end-node symbol?]
                   [#:atb edge-attributes (hash/c symbol? any/c) (hash 'fontsize 15)])
                   graph?]{

 Adds a edge to the provided @racket[graph] with a directional arrow from the @racket[start-node] to the @racket[end-node]. The
 label for the arrow is the @racket[value] that is supplied. The edge structure stores the @racket[value] as a list since we squash
 all edges between the same nodes into a single edge.
 @bold{Note}: Since the @emph{DOT} language does not allow @racket{-} characters in node names the dashes are omitted, but
 are still provided for the label.

 @racket[edge-attributes] are a hash where the key is a symbol representing a @hyperlink["https://graphviz.org/docs/edges/"]{edge attribute}
 and the value is the value for that attribute.
}


@defproc[(add-edges [graph graph?]
                    [edges (listof (list/c symbol? any/c symbol?))]
                    [#:atb edge-attributes (hash/c symbol? any/c) (hash 'fontsize 15)])
                    graph?]{

 Adds this list of edges to the provided @racket[graph] with a directional arrow from the @racket[start-node] to the @racket[end-node].
 @bold{Note}: Since the @emph{DOT} language does not allow @racket{-} characters in node names the dashes are omitted, but
 are still provided for the label.

 @racket[edges] is a list of triples with the structure @racket[(list start-node end-node value)]

 @racket[edge-attributes] are a hash where the key is a symbol representing a @hyperlink["https://graphviz.org/docs/edges/"]{edge attribute}
 and the value is the value for that attribute. It is applied to every value in the list.

Example usage:
@codeblock{
(add-edges (add-nodes (create-graph 'test) '(A B C D))
           '((A a B) (B b B) (B c-1 D)))
}
}



@defproc[(graph->bitmap [graph graph?]
                        [#:directory save-directory path? "system tmp directory"]
                        [#:filename filename string? "__tmp__"]
                        [#:clean delete-files boolean? #t])
                        image?]{
Converts the provided @racket[graph] to a bitmap using @emph{htdp2-lib}'s @hyperlink["https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._bitmap%2Ffile%29%29"]{bitmap/file} function. The file is saved in the provided
@racket[save-directory] using the provided @racket[filename].

When @racket[save-directory] is not specified then the systems tmp directory is used if read and write permissions exist, otherwise it defaults to the @racket[current-directory].

When @racket[delete-files] is false, then the generated ".dot" and ".png" files are not deleted.

@bold{Note}: In order for the function to work one must have the @emph{DOT Complier} downloaded on their machine and have a link to the @emph{DOT} executable
on there PATH or have the binary saved in one of the searched directories (see @secref{executable} for more details).
}
@codeblock{
(define my-graph (create-graph 'test))

;; generate and cleanup files in the systems tmp directory using the default name
(graph->bitmap my-graph)

;; generate and cleanup files in using specified directory and filename
(graph->bitmap my-graph #:directory (current-directory) #:filename "test")

;; test.dot and test.png are not deleted 
(graph->bitmap my-graph #:filename "test" #:clean #f)
}


@defproc[(graph->svg [graph graph?]
                     [save-directory path?]
                     [filename string?]
                     [#:clean delete-files boolean? #t])
                     path?]{
Converts the provided @racket[graph] to a svg file and returns the path to the newly created file.
The file is saved in the provided @racket[save-directory] using the provided @racket[filename].

When @racket[delete-files] is false the generated ".dot" file is deleted.

@bold{Note}: In order for the function to work one must have the @emph{DOT Complier} downloaded on their machine and have a link to the @emph{DOT} executable
on there PATH or have the binary saved in one of the searched directories (see @secref{executable} for more details).
}


@defproc[(graph->png [graph graph?]
                     [save-directory path?]
                     [filename string?]
                     [#:clean delete-files boolean? #t])
                     path?]{
Converts the provided @racket[graph] to a png file and returns the path to the newly created file.
The file is saved in the provided @racket[save-directory] using the provided @racket[filename].

When @racket[delete-files] is false the generated ".dot" file is deleted.

In order for the function to work one must have the @emph{DOT Complier} downloaded on their machine and have a link to the @emph{DOT} executable
on there PATH.
}


@defproc[(graph->dot [graph graph?]
                     [save-directory path?]
                     [filename string?])
                     path?]{
Converts the provided @racket[graph] to a @emph{DOT} language representation and returns the path to the newly created file.
The file is saved in the provided @racket[save-directory] using the provided @racket[filename].
}


@defproc[(fsa->graph [fsa fsa?]
                     [color-blind-mode (and/c (>= n 0) (<= n 2))])
                     image?]{
Converts the provided @racket[fsa] to a bitmap using @emph{htdp2-lib}'s @hyperlink["https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._bitmap%2Ffile%29%29"]{bitmap/file} function.
The file is saved in the users @emph{tmp} directory. If the that directory is not found then it is saved in the users @emph{current directory}.

@racket[color-blind-mode] is a integer between 0 and 2 that represents a different set of colors to be used for the image.
}


@defproc[(machine->graph [machine machine?]
                         [color-blind-mode (and/c (>= n 0) (<= n 2))]
                         [current-rule (or/c symbol? boolean?)]
                         [current-state (or/c symbol? boolean?)]
                         [invariant-state (or/c 'pass 'fail 'none)])
                     image?]{
Converts the provided GUI @racket[machine] to a bitmap using @emph{htdp2-lib}'s @hyperlink["https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._bitmap%2Ffile%29%29"]{bitmap/file} function.
The file is saved in the users @emph{tmp} directory. If the that directory is not found then it is saved in the users @emph{current directory}.

@racket[color-blind-mode] is a integer between 0 and 2 that represents a different set of colors to be used for the image.

@racket[current-rule] is the current rule to be highlighted. If @racket[#f] is supplied then no rule is highlighted.

@racket[current-state] is the current state that the machine is in. If @racket[#f] is supplied then no state is highlighted.

@racket[invariant-state] is a symbol representing is the invariant passed. If @racket['pass] is supplied then the state is highlighted
green. If @racket['fail] is supplied then the state is highlighted red. If @racket['none] is supplied then the state is not highlighted.
}


@subsection[#:tag "formatters"]{Custom Formatters}
Formatters are ways for implementers to customize how attribute data is generated to @emph{DOT} code.
If a formatter is provided for an attribute then it is used anytime @emph{DOT} code is generated for that
attribute.

@defstruct*[formatters ([graph (hash/c symbol? (-> any/c string?))]
                       [node (hash/c symbol? (-> any/c string?))]
                       [edge (hash/c symbol? (-> any/c string?))])]{
    A structure type for formatters with the following fields:
    @itemlist[@item{@racket[graph] formatters to be applied to graph level attributes.}
              @item{@@racket[node] formatters to be applied to node level attributes.}
              @item{@racket[edge] formatters to be applied to edge level attributes.}]




 For example one might add the following formatters to set one rule per line for a edge label.
 @codeblock{
  ;; one-rule-per-line :: listof(string) -> string
  ;; prints 1 rule per line
  (define (one-rule-per-line rules)
    (string-join rules "\n"))

  (define graph-formatters (formatters
                            (hash) ; graph level formatters
                            (hash) ; node level formatters
                            (hash 'label one-rule-per-line))) ; edge level formatters
 }
}

@defproc[(create-formatters [#:graph graph-fmtrs (hash/c symbol? (-> any/c string?)) (hash)]
                            [#:node node-fmtrs (hash/c symbol? (-> any/c string?)) (hash)]
                            [#:edge edge-fmtrs (hash/c symbol? (-> any/c string?)) (hash)])
                     formatters?]{
Creates a formatters struct with the given arguments. For examples the above could be simplified to:
@codeblock{
;; one-rule-per-line :: listof(string) -> string
;; prints 1 rule per line
(define (one-rule-per-line rules)
  (string-join rules "\n"))

(define fmtrs (create-formatters #:edge (hash 'label one-rule-per-line)))
}
}

@section[#:tag "executable"]{Dealing with the DOT executable}

@deftogether[(@defproc[(find-dot-executable) path?]
              @defproc[(has-dot-executable?) boolean?])]{
Looks for the @emph{DOT} executable on the system by looking at the PATH (Windows, MacOS, Linux) and specified
directories (MacOS, Linux). If the executable is found, then the path to the executable is returned. The specified
directories are:
@itemlist[@item{/bin}
          @item{/usr/bin}
          @item{/usr/local/bin}
          @item{/opt/local/bin}
          @item{/opt/homebrew/bin}]
}



@defproc[(find-tmp-dir) path?]{
Looks for the systems tmp directory, if it exists then returns the path to that directory.
If the tmp dir is not found then the current-directory from which the program is ran is used instead.
@bold{Note}: The directory that is returned must have read and write access.
}



@section[#:tag "examples"]{Examples}
Below are examples of how to use the library.

@subsection{Creating Basic Graphs}
The simplest way to create a graph is work in steps. First create the graph using @racket[create-graph]. Then
add the nodes to the graph using either @racket[add-node] or @racket[add-ndoes]. Then add the edges to the graph
using @racket[add-edges] or @racket[add-edge]. Last, convert it to the dot language.
@codeblock{
(define init-graph (create-graph 'cgraph #:atb (hash 'rankdir "LR")))

(define nodes '(A-1 A-2))

(graph->bitmap
 (add-edge (foldr (lambda (name graph) (add-node graph name)) init-graph nodes)
           'a-name
           'A-1
           'A-2)
 #:directory (current-directory)
 #:filename "test")
}
produces
@centered{@image[#:suffixes @list[".png"]]{scribImgs/simple_graph}}


@subsection{Adding Attributes}
Often you will want to add attributes to the graph. This is done using the #:atb keyword on the
graph, edge, and node functions. Below is an example of using attributes for a graph.
@codeblock{
(struct character (name mother father side))

;; character->node :: character -> graph
;; adds a character to the graph
(define (character->node c g) 
  (add-node g (character-name c) #:atb (hash 'color (match (character-side c)
                                                      ['light 'green]
                                                      ['dark 'red]
                                                      ['unknown ""])
                                             'shape 'box
                                             'style 'filled)))
;; character->edge :: character -> graph
;; adds an edge to the graph
(define (character->edge c g)
  (match (cons (character-father c) (character-mother c))
    [(cons 'unknown 'unknown) g]
    [(cons 'unknown mother) (add-edge g "" mother (character-name c))]
    [(cons father 'unknown) (add-edge g "" father (character-name c))]
    [(cons father mother) (add-edge (add-edge g "" father (character-name c)) "" mother (character-name c))]))
  

(define skywalkers (list (character 'Shmi 'unknown 'unknown 'unknown)
                         (character 'Anakin 'Shmi 'unknown 'dark)
                         (character 'Ruwee 'unknown 'unknown 'unknown)
                         (character 'Jobal 'unknown 'unknown 'unknown)
                         (character 'Padme 'Jobal 'Ruwee 'light)
                         (character 'Luke 'Padme 'Anakin 'light)
                         (character 'Leia 'Padme 'Anakin 'light)
                         (character 'Han 'unknown 'unknown 'light)
                         (character 'Ben 'Leia 'Han 'dark)))
                                


(define init-graph (create-graph 'test #:atb (hash 'label "Skywalker Family Tree")))
(define graph-with-nodes (foldl character->node init-graph skywalkers))
(define graph-with-edges (foldl character->edge graph-with-nodes skywalkers))
(graph->bitmap graph-with-edges)
}
produces
@centered{@image[#:suffixes @list[".png"]]{scribImgs/skywalker}}



@subsection{Creating Graphs with Formatters}
Formatters can be used to specify the output for an attribute value. For example we can use formatters to
format the edge labels on a graph so that only one rule is displayed per line.
@codeblock{
#lang racket
(require "interface.rkt")

;; one-rule-per-line :: listof(string) -> string
;; prints 1 rule per line
(define (one-rule-per-line rules)
  (string-join rules "\n"))

(define fmtrs (create-formatters #:edge (hash 'label one-rule-per-line)))

(graph->bitmap (add-edges (add-nodes (create-graph 'test  #:fmtrs fmtrs) '(A B C D))
                          '((A "(A a B)" B)
                            (A "(A b B)" B)
                            (B "(B a B)" C)
                            (B "(B b B)" C)
                            (B "(B c-1 B)" C)
                            (C "c-1" D)
                            (C "c-2" D))))
}
produces
@centered{@image[#:suffixes @list[".png"]]{scribImgs/simple_graph_with_formatter}}


@subsection{Using Complex Data for Edges}
Sometimes it can be more intuitive to use non string data for edge labels. Edges are allowed to store
any datatype for a edge label by defualt we just need to make sure that the label has a formatter to
convert it to a string. 
@codeblock{
#lang racket
(require "interface.rkt")

;; one-rule-per-line :: listof(rules) -> string
;; prints 1 rule per line
(define (one-rule-per-line rules)
  (define string-rules (map (curry format "~a") rules))
  (string-join rules "\n"))

(define fmtrs (create-formatters #:edge (hash 'label one-rule-per-line)))

(graph->bitmap (add-edges (add-nodes (create-graph 'test  #:fmtrs fmtrs) '(A B C D))
                          '((A (A a B) B)
                            (A (A b B) B)
                            (B (B a B) C)
                            (B (B b B) C)
                            (B (B c-1 B) C)
                            (C c-1 D)
                            (C c-2 D))))
}
produces
@centered{@image[#:suffixes @list[".png"]]{scribImgs/simple_graph_with_formatter}}
