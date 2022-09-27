#lang scribble/manual

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



@section[#:tag "interface"]{Interfacing With FSM}