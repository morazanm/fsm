#lang scribble/manual
@(require scribble/eval
          (for-label racket
                     "../private/lib.rkt"))
@title{FSM Graphviz Library}
@author[(author+email "Marco T. Morazán" "morazanm@shu.edu")
        "Joshua M. Schappel"]

The FSM Graphviz Library is a internal library for interfacing @(hyperlink "https://graphviz.org" "Grahpviz" )
which is a third party graphing library.@linebreak{}@linebreak{}@bold{Note: } FSM Library uses will only find @italic{@secref{download}}
useful. The rest of this documention page is ment to be a reference for FSM developers, however curiosity is always encouraged.


@section[#:tag "download"]{How to Download}

In order to use this library on your machine you first need to install Graphviz on your computer and add
it to your computers PATH. Instructions for installing graphviz on MacOS, Linux, and Windows can be found
@(hyperlink "https://graphviz.org/download/" "here" ). Once you have it installed you can test to make sure
it is on your PATH by enting the following command in your terminal:@linebreak{}@linebreak{}@tt{dot -V}.
@linebreak{} @linebreak{} if you get an output similar to @tt{dot - graphviz version 6.0.1}, then you have
installed it correctly.


@section[#:tag "architecture"]{Architecture Overview}



@section[#:tag "lib"]{Internal Library}
@defstruct*[graph ([name  string?]
                   [node-list (listof node?)]
                   [edge-list (listof edge?)]
                   [color-blind colorblind-opt?])]{
 The top level structure that the @tt{FSM Graphviz Library} works off of is a graph. A graph is a structre
 that holds all the information that is required in order to generate a graphviz @tt{dot} file.}

@racketblock[(graph 'Mygraph '() '() 0)]

@defstruct*[node ([name symbol?]
                  [value symbol?]
                  [atb hash?]
                  [type state-type?])]{
 A node is shape that is rendered on the graph and is used to connect edges to each other. Each node has both a
 name and a value field. The name field is the name for the node in the dot file and the value is the value that
 is rendered inside the node. A node can also contain attributes that are assigned as key value pairs inside the
 attr hash. The type fild is used for rendering fsa's as graphs and can be passes @racket['none] when dealing with
 other entities.}

@defstruct*[edge ([atb hash?]
                  [start-node symbol?]
                  [end-node symbol?])]{
 A edge is a connection between two nodes. Each edge contains a starting node, a ending node and attributes that
 are assigned as key value pairs inside the attr hash.
}



@section[#:tag "interface"]{Interfacing With FSM}