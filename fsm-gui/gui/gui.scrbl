#lang scribble/manual
@(require (for-label racket 2htdp/image))


@title{FSM GUI 2.0}
@author["Joshua Schappel" "Marco T. Morazán"]

@defmodule["../interface.rkt"]

A Graphical User Interface for constructing state machines, visualizing transitions, and debugging invariants.
 contains 1-1 parody with the @emph{Legacy GUI} while also adding new features such as a @emph{Map View},
@emph{State View}, and the ability to @emph{hot-reload invariants}.

@centered{@image[#:suffixes @list[".png"] #:scale 0.7]{scribImgs/gui2_tm}}

@table-of-contents[]

@section[#:tag "design_gui"]{Overall Design}
@emph{GUI 2.0} is split into a frontend and a backend. The frontend is written in @hyperlink["https://www.typescriptlang.org/"]{Typescript} using
@hyperlink["https://www.electronjs.org/"]{Electron} and @hyperlink["https://react.dev/"]{React}
while the backend is written in  @hyperlink["https://racket-lang.org/"]{Racket}. The benefit of using Electron is that it allows us to use
HTML and CSS to create a highly customizable desktop application. For more information on this see @secref{frontend}.

As a general overview the frontend just displays data for the user and the backend does all the heavily lifting when
it comes to constructing fsa's, images, and invariants. The frontend and backend communicate over TCP Sockets which allows which
is discussed in more detail in @secref{backend}.

@centered{@image[#:suffixes @list[".png"]]{scribImgs/gui2_diagram}}

@section[#:tag "backend"]{Backend Design}
As discussed in @secref{design_gui}, the backend handles the creation of fsa's, images, and invariants. It does this by starting
a TCP server that listens for incoming connections on a specified port. Once a connection is established the client and server
communicate by sending json data over the socket using a specified API (see @secref{api}). The connection between the client
and sever exists until the client is finished, at which time the server will terminate the connection. If the server has to shutdown
early it will send a @emph{EOF} message to all registered clients telling them to shut down.

Since TCP does not have a standard for telling the receiver when a TCP message has been fully sent. Because of this the backend uses
a custom protocol named @emph{FSM Protocol}. This is a simple protocol that just sends a EOF object after the initial message to signal to the
client that the message is finished. An example of its implementation is below:
@codeblock{
;; send-fsm-protocol :: jsexpr port -> ()
(define (send-fsm-protocol data out)
  (write-json data out)
  (write eof out)
  (flush-output out))
}

When generating machines for the GUI the backend computes all transitions and GraphViz images at once to minimize the traffic between the
client and the server. Computing everything at once also allows uses to view transitions in the GUI ever if the connection is severed.

@subsection[#:tag "api"]{Shared API}
The basic structure for a request and response is as follows.
@margin-note{All type definitions are in Typescript}
@verbatim|{
type Instruction = string; 
          
type SocketRequest<T> = {
  data: T;
  instr: Instruction;
};

type SocketResponse<T> = {
  data: T;
  responseType: Instruction;
  error: string | null;
};

}|

Both a request and response have a @emph{Instruction} that is passed back and forth. This tells the server what action needs to be
executed before sending back data to the client. An example of a instruction might be @racket{build_machine} which tells the server
that the client is requesting to build the given fsa with a specified input. A request also has a generic @emph{data} field that may
contain some data needed for the server to compute a response.

A response contains a generic @emph{data} field that contains the computed data from the server. It also contains the instruction and
a possible error message field that is populated if the server encountered an during its computation. See @secref{req_types} and
@secref{res_types} for a full list of types that the server accepts
@subsubsection[#:tag "req_types"]{Request Types}
@verbatim|{
type Filepath = string;

type FSMBuildMachineRequest = {
  states: State[];
  alphabet: FSMAlpha[];
  rules: FSMRule[];
  type: MachineType;
  input: FSMAlpha[];
  nodead: boolean;
  tapeIndex: number | undefined; // Only defined for TM's
  stackAlpha: FSMStackAlpha[] | undefined; // undefined when not a pda
};

type RedrawnGraphvizImageRequest = {
  states: State[];
  rules: FSMRule[];
  type: MachineType;
  currentFilepath: string;
};

type RecomputeInvariantRequest = FSMBuildMachineRequest & {
  targetState: string;
  invStatuses: {
    index: number;
    status: boolean; // pass or fail
    filepath: Filepath;
  }[];
};
}|
@subsubsection[#:tag "res_types"]{Response Types}
@verbatim|{
type BuildMachineResponse = {
  transitions: FSMTransition[];
  // Sometimes fsm-core will add the dead-state (ds). Because of this we
  // will return the result of sm-states. Any new states + rules should
  // be added to the gui.
  states: State[];
  rules: FSMRule[];
};

type PrebuiltMachineResponse = {
  states: State[];
  alpha: FSMAlpha[];
  rules: FSMRule[];
  type: MachineType;
  stackAlpha: FSMStackAlpha[] | undefined; // undefined when not a pda
  hotReload: boolean; //when set to true, invariants can be edited in the GUI

  // The absolute path to the graphViz image. If this field does not exist then
  // that means that graphViz is not on the users computer
  filepath: string | null;
};

type RedrawnGraphvizImageResponse = {
  // The absolute path to the graphViz image. If this field is null then
  // it means that graphViz is not on the users computer
  filepath: string | null;
};

type InvalidSyntaxError = string;

type Filepath = string;

type RecomputeInvariantResponse = {
  targetState: string;
  changedStatuses: {
    index: number;
    status: boolean | InvalidSyntaxError;
    filepath: Filepath;
  }[];
};
}|

@subsection[#:tag "comp_inv"]{Computing Invariants}
Since the GUI allows for hot-reloading invariants the backend and the frontend represent an invariant as string
containing the racket code. For example  @racket[(define (S-INV) #t)] would be represented as @racket{(define (S-INV) #t)}. In
order to run the string we use the racket functions @racket[read], @racket[eval], and @racket[apply] to convert the string to
Racket code and then run it: @racketblock[(eval (read (open-input-string invariant)))]

After loading the code into racket via @racket[read] we need to provide a @hyperlink["https://docs.racket-lang.org/guide/eval.html#%28tech._namespace%29"]{namespace}
to @racket[eval] so we can call racket defined functions and user defined functions.
For more information on how this is done see @secref{inv_macros}.

@subsection[#:tag "start_backend"]{Starting the Backend}
There are two exported functions that can be used for starting the backend. They are as follows
@defproc[(run-with-prebuilt [fsa fsa?]
                            [invariants (listof (cons symbol? string?))]
                            [namespace namespace?])
         void?]{
Starts the backend with a prebuilt @racket[fsa]. This @racket[fsa] is sent to the client when the connection
is first established.

@racket[invariants] is a list of paris containing a symbol denoting a state and a string representing the racket invariant code
associated with the state.

@racket[namespace] is a namespace that is used for computing the invariant.
}


@defproc[(run-without-prebuilt)
         void?]{
Starts the backend server to listen for incoming connections.
}


@section[#:tag "frontend"]{Frontend Design}
TODO
@subsection[#:tag "start_frontend"]{Starting and Testing the Frontend}
All frontend code lives in the directory @code|{fsm/fsm-gui/gui/frontend}|. In order to start the frontend you need to have
@hyperlink["https://nodejs.org/en"]{NodeJS} installed on your computer. Then you can run the following commands from the 
frontend directory:
@margin-note{
  If this is your first time running the frontend or a dependency has changed you first need to run @codeblock|{npm ci}|

  All commands must be ran from @codeblock|{fsm/fsm-gui/gui/frontend}|
}

@itemlist[
          @item{Start GUI @verbatim|{npm start}|}
          @item{Run Test Suite @verbatim|{npm test}|}
          @item{Run Typechecker and Linter @verbatim|{npm run typecheck; npm run lint}|}
          @item{Run Code Formatter @verbatim|{npm run format}|}]


@section[#:tag "inv_macros"]{Invariant Macros and Namespaces}
TODO
@section[#:tag "cicd"]{Deploying (CI/CD)}
Since the backend is written in racket and included in the build, any changes to the backend will automatically be deployed when the
code is merged into master.

The same goes for the scribble doc. Any changes to the scribble file will be picked up by the CI/CD Docs build step and be auto deployed
when the code is merged to master

The frontend is a bit more complicated. Since the frontend is built in TypeScript using Electron we have a CI/CD pipeline that builds a
new binary for MacOS, Linux(Debian), and Windows. These binaries are pushed to a draft Github release that needs to be manually released. In order to
have the CI Tool build a draft release the following steps must run in the terminal
@margin-note{The commands must be ran in the frontend directory: @codeblock|{fsm/fsm-gui/gui/frontend}|}
@codeblock|{
npm version minor
git push --follow-tags
}|
Once the above commands are executed one just needs to publish the draft release.
@margin-note{This process only needs be done when changes are made to frontend}
