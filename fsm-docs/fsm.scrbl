; FSM Version 1.0
; Copyright (C) 202- by Marco T. Morazan 

#lang scribble/manual

@(require (for-label racket 2htdp/image) setup/collects)

@title{FSM}
@author[(author+email "Marco T. Morazán" "morazanm@shu.edu")]
@defmodulelang[fsm #:use-sources (fsm)]


A DSL for the Automata Theory Classroom

FSM is a DSL designed to help students understand Automata Theory by
allowing them to construct and minipulate state machines, grammars, and regular expressions..
For instructions on how to download the DSL, view the patch notes, or just browse the code, please see either the offical
FSM readme on @(hyperlink "https://github.com/morazanm/fsm" "Github") or the
new @(hyperlink "https://morazanm.github.io/fsm/index.html" "FSM website").

@table-of-contents[]

@section{Constants}
@defidform[ARROW]
The symbol used to separate the lefthand side from the righthand side
of a grammar rule.

@defidform[BLANK]
In a Turing machine tape, this symbol denotes a blank space.

@defidform[BRANCH]
In a ctm description, this symbol denotes conditional branch.

@defidform[EMP]
The symbol denoting the empty string or character. It cannot be in 
the alphabet of a state machine or grammar.

@defidform[DEAD]
The symbol denoting the default dead state. This is the state 
reached when no transition rules applies to the current 
configuration of a state machine.

@defidform[GOTO]
In a ctm description, this symbol denotes an unconditional branch.

@defidform[LEFT]
In a Turing machine transition rule, this symbol denotes moving the 
head to the left.

@defidform[LM]
In a Turing machine tape, this symbol denotes the left end marker.


@defidform[RIGHT]
In a Turing machine transition rule, this symbol denotes moving the 
head to the right.

@defidform[START]
The seed symbol used to generate a new start state.

@defidform[VAR]
In a ctm description, this symbol denotes the introduction of a
variable to abstract over the currently read symbol.


@section{Data Definitions}



@defidform[alphabet] A list of symbols representing lowercase letters in the Roman alphabet.

@defidform[word]{
 A @italic{(listof symbol)}. Each symbol is a member of the same alphabet.}

@defidform[letter]{
 A string of length one representing a lowercase letter in the Roman alphabet.}

@defidform[state]  
An uppercase letter (e.g., A) or a symbol comprised of an uppercase 
letter, dash, and number (e.g., A-72431).

@defidform[dfa-rule] 
A (list state symbol state) representing a transition in a 
deterministic finite-state automaton. The symbol must be in the 
alphabet of the machine.

@defidform[ndfa-rule]{ 
 A @italic{(list state symbol state)} representing a transition in a 
 nondeterministic finite-state automaton. The symbol must either be 
 in the alphabet of the machine or be EMP.}

@defidform[pda-rule]{ 
 A @italic{(list (list state symbol pop) (list state push))} denoting a 
 transition in a pushdown automaton. The symbol must be in the 
 alphabet of the machine. The elements to remove from the 
 top of the stack are denoted by pop which is either EMP or
 a list of symbols where the leftmost is first element to pop. 
 The elements to place onto the top of the stack 
 are denoted by push which is either EMP or a list of symbols where 
 the leftmost symbol is the last element to push.}

@defidform[tm-action] 
If an alphabet symbol, it denotes the symbol written to the tape of a Turing
machine. Otherwise, it is the direction in which to move the head:
RIGHT or LEFT.

@defidform[tm-rule]{ 
 A @italic{(list (list state symbol) (list state tm-action))} representing a 
 transition in a nondeterministic Turing machine. The symbol must
 either be either EMP or an element of the machine's alphabet.}

@defidform[mttm-rule]{ 
 A @italic{(list (list state (listof symbol)) (list state (listof action)))} representing
 a transition in a nondeterministic multitape Turing machine. The symbols represent what
 is read on each of the tapes and must be either EMP or an element of the machine's
 alphabet.}


@defidform[dfa-configuration] 
A list containing a state and a word. The word is the unread part of 
the input and the state is the current state of the machine.

@defidform[ndfa-configuration] 
A list containing a state and a word. The word is the unread part of 
the input and the state is the current state of the machine.

@defidform[pda-configuration] 
A list containing a state, a word, and a list of symbols. The state 
is the current state of the machine. The word is the unread part of 
the input. The list of symbols is the contents of the stack where
the leftmost symbol is the top of the stack.

@defidform[tm-configuration] 
A list containing a state, a natural number, and a list of symbols. 
The state is the current state of the machine. The natural number is
the head's position. The list of symbols is the contents of the tape
up to the rightmost position reached, so far, by the machine.

@defidform[mttm-configuration] 
A list containing a state and one or more tape configurations. A tape
configuration is a list containing a natural number for the head's
position and a sublist representing the contents of the tape.

@defidform[regexp] A regular expression (see regular expression constructors below).

@defidform[terms] Lowercase letters.

@defidform[nts]
A set of nonterminal symbols. A nonterminal symbol is an upercase
letter in English: A..Z. That is, FSM programmers are limited to 26 nonterminals.
The internal representation in FSM may use symbols of the form nts-<digit>^+
(e.g., A-72431). Such nonterminals may not be directly used in an FSM program.

@defidform[rrule] A regular grammar rule is a list of 
the following form:
@itemlist[@item{(S ARROW EMP)}
          @item{(N ARROW a)}
          @item{(N ARROW aB)}]
S is the starting nonterminal, N and B are nonterminal symbols, and 
a is a terminal symbol.

@defidform[cfrule]{ A context-free grammar rule is a list of the
 form @italic{(A ARROW J)}, where A is a nonterminal symbol and J is either
 EMP or an aggregate symbol of terminals and nonterminals.}

@defidform[csrule]{A context-sensitive grammar rule is a list of the
 form @italic{(H ARROW K)}, where H is an aggregate symbol of terminals and
 at least one nonterminal and K is either
 EMP or a an aggregate symbol of terminals and nonterminals.}

@defidform[machine]  
A representation of a statemachine in FSM. A state machine is one of the following:
@itemlist[
 @item{Deterministic Finite Automaton (dfa)}
 @item{Nondeterministic Finite Automaton (ndfa)}
 @item{Pushdown Automaton (pda)}
 @item{Turing Machine (tm)}
 @item{Language Recognizer (tm-langauge-recognizer)}
 @item{Multitape Turing Machine (mttm)}
 @item{Multitape Turing Machine Language Recognizer (mttm-language-recognizer)}]

@defidform[smrule]  
A state machine rule is either a dfa-rule, an ndfa-rule, a 
ndpda-rule, or a tm-rule.

@defidform[grammar]
A representation of a grammar in FSM. A grammar is one of the following:
@itemlist[
 @item{Regular Grammar (rg)}
 @item{Context-Free Grammar (cfg)}
 @item{Context-Sensitive Grammar (csg)}]

@defidform[grule]  
A grammar rule is either a rrule, a cfrule, or csrule.

@defidform[Derivation]  
A derivation is either a (list nts ARROW nts) or 
a (append (list ARROW nts) Derivation).

@defidform[ctmd]  
A composed Turing machine description is either:
@itemlist[@item{empty}
          @item{(cons tm ctmd)}
          @item{(cons LABEL ctmd)}
          @item{(cons symbol ctmd}
          @item{(cons BRANCH (listof (list symbol ctmd)))}
          @item{(cons (GOTO LABEL) ctm)}
          @item{(cons ((VAR symbol) ctm) ctm)}]
A LABEL is a natural number.

@defidform[ctm]  
A composed Turing machine.

@section{State Machine Constructors}


@defproc[(make-dfa [sts (listof state)] 
                   [sigma alphabet] 
                   [start state] 
                   [finals (listof state)] 
                   [delta (listof dfa-rule)]
                   [add-dead (or/c 'no-dead (void))]
                   [#:rejects rejects (listof word) '()]
                   [#:accepts accepts (listof word) '()])
         dfa]{Builds a deterministic finite-state automaton.
              @italic{delta} is a transition function. The optional
              argument @italic{add-dead} is used to signal the
              constructor to add or not add a dead state and
              transitions into it for any transitions in delta (to
              make it a function). If the delta function is fully
              specified providing 'no-dead prevents the
              addition of such a dead state and associated
              transitions. The optional
              keyword parameters @italic{rejects} and @italic{accepts}
              are used to integrate testing. They are both lists of
              words using elements of @italic{sigma}. The first is a
              list of words that ought to be rejected and the second is
              a list of words that ought to be accepted. If any word
              produces the wrong result the constructor fails and a
              @italic{recipe-based} error is thrown.
             }


@defproc[(make-ndfa [sts (listof state)] 
                    [sigma alphabet] 
                    [start state] 
                    [finals (listof state)] 
                    [delta (listof ndfa-rule)]
                    [#:rejects rejects (listof word) '()]
                    [#:accepts accepts (listof word) '()])
         ndfa]{Builds a nondeterministic finite-state automaton.
               @italic{delta} is a transition relation.
               The optional
              keyword parameters @italic{rejects} and @italic{accepts}
              are used to integrate testing. They are both lists of
              words using elements of @italic{sigma}. The first is a
              list of words that ought to be rejected and the second is
              a list of words that ought to be accepted. If any word
              produces the wrong result the constructor fails and a
              @italic{recipe-based} error is thrown.
              }

@defproc[(make-ndpda [sts (listof state)] 
                     [sigma alphabet] 
                     [gamma (listof symbol)]
                     [start state] 
                     [finals (listof state)] 
                     [delta (listof pda-rule)]
                     [#:rejects rejects (listof word) '()]
                     [#:accepts accepts (listof word) '()])
         ndpda]{Builds a nondeterministic pushdown automaton from the
 given list of states, alphabet, list of stack symbols,
 start state, list of final states, and list of
 pda-rule. @italic{delta} is a transition relation.
 The optional keyword parameters @italic{rejects} and @italic{accepts}
 are used to integrate testing. They are both lists of
 words using elements of @italic{sigma}. The first is a
 list of words that ought to be rejected and the second is
 a list of words that ought to be accepted. If any word
 produces the wrong result the constructor fails and a
 @italic{recipe-based} error is thrown.

 Given that a nondeterministic pda may only semidecide a language,
 use @italic{rejects} with caution. Words not in the machine's
 language may cause the constructor to not terminate.
}

@defproc*[([(make-tm    [sts (listof state)] 
                        [sigma alphabet]
                        [delta (listof tm-rule)]
                        [start state] 
                        [finals (listof state)]
                        [#:rejects rejects (listof word) '()]
                        [#:accepts accepts (listof word) '()]) tm]
           [(make-tm    [sts (listof state)] 
                        [sigma alphabet]
                        [delta (listof tm-rule)]
                        [start state] 
                        [finals (listof state)]
                        (accept state)
                        [#:rejects rejects (listof word) '()]
                        [#:accepts accepts (listof word) '()]) tm])]{Builds a nondeterministic Turing machine. 
 @italic{delta} is a transition relation.
 @italic{LM} is automatically added to the machine's alphabet.
 Rules for moving off the @italic{LM} are automatically added
 to the machine's rules.
 If the optional @italic{accept} argument is given then the resulting
  Turing machine is a language recognizer. The optional
              keyword parameters @italic{rejects} and @italic{accepts}
              are used to integrate testing. They are both lists of
              words using elements of @italic{sigma}. The first is a
              list of words that ought to be rejected and the second is
              a list of words that ought to be accepted. If any word
              produces the wrong result the constructor fails and a
              @italic{recipe-based} error is thrown.

  Given that a tm may only semidecide a language,
  use @italic{rejects} with caution. Words not in the machine's
  language may cause the constructor to not terminate.
  }

@defproc*[([(make-mttm  [sts (listof state)] 
                        [sigma alphabet]
                        [start state]
                        [finals (listof state)]
                        [delta (listof mttm-rule)]
                        [num-tapes number]
                        [#:rejects rejects (listof word) '()]
                        [#:accepts accepts (listof word) '()]) tm]
           [(make-mttm  [sts (listof state)] 
                        [sigma alphabet]
                        [start state]
                        [finals (listof state)]
                        [delta (listof mttm-rule)]
                        [num-tapes number]
                        (accept state)
                        [#:rejects rejects (listof word) '()]
                        [#:accepts accepts (listof word) '()]) tm])]
Builds a nondeterministic Multitape Turing machine with the given number of
tapes. If the optional @italic{accept} argument is given then the resulting multitape
Turing machine is a language recognizer. The optional
              keyword parameters @italic{rejects} and @italic{accepts}
              are used to integrate testing. They are both lists of
              words using elements of @italic{sigma}. The first is a
              list of words that ought to be rejected and the second is
              a list of words that ought to be accepted. If any word
              produces the wrong result the constructor fails and a
              @italic{recipe-based} error is thrown.

Given that a mttm may only semidecide a language,
use @italic{rejects} with caution. Words not in the machine's
language may cause the constructor to not terminate.

@defproc[(ndfa->dfa [m ndfa])
         dfa]{Builds a @italic{deterministic} finite-state 
 automaton equivalent to the given ndfa.}

@defproc[(regexp->fsa [r regexp])
         ndfa]{Builds a fsm for the language of the given
 regular expression.}

@defproc[(sm-rename-states [sts (listof state)] [m1 machine?])
         machine?]{Builds a state machine that is excatly the same as
 the given machine except that its states are renamed
 as to not have a name in common with the given list
 of states.}

@defproc[(sm-union [m1 machine?] [m2 machine?])
         machine?]{Builds a state machine for the language obtained
 from the union of the languages of the two given
 state machines. If the inputs are Turing machines then
 they must be language recognizers. The given machines 
 must have the same type. May not be used with multitape Turing machines.}

@defproc[(sm-concat [m1 machine?] [m2 machine?])
         machine?]{Builds a state machine for the language obtained
 from the concatenation of the languages of the two given
 state machines. [Note: Not yet implemented for Turing machines language recognizers.
 May not be used with multitape Turing machines.].}

@defproc[(sm-kleenestar [m1 machine?])
         machine?]{Builds a state machine for the language obtained
 from the Kleene star of the given machine's language. May not be used with
 multitape Turing machines nor with Turing machine language recognizers.}

@defproc[(sm-complement [m1 machine?])
         machine?]{Builds a state machine for the language obtained
 from the complement of the given machine's language.
 The given machine can not be a ndpda. If the inputs are 
 Turing machines then they must be language recognizers. May not be used
 with multitape Turing machines.}

@defproc[(sm-intersection [m1 machine?] [m2 machine?])
         machine?]{Builds a state machine for the language obtained
 from the intersection of the languages of the two given
 state machines. If the inputs are Turing machines then
 they must be language recognizers. The given machines 
 must have the same type. May not be used with multitape Turing machines.}

@defproc[(grammar->sm [g grammar])
         machine?]{Builds a state machine for the language of the given
 regular or context-free grammar.}


@section{State Machine Visualization}
@bold{You must have GraphViz installed as an enviroment variable
  for this to work. For more information how to set this up, please see: @(hyperlink "https://github.com/morazanm/fsm/tree/master/GraphViz" "FSM GraphViz ReadMe")}

@defproc[(sm-graph [m machine?])
         image?]{Generates an image for the given machine's transition diagram. @(linebreak)}

@defproc[(sm-cmpgraph [m machine?] [w word?] [#:palette palette symbol? 'default] [#:cutoff cutoff natural? 100] [headpos natural? 0])
         image?]
        Generates a computation graph image for the given machine and word. A computation graph
        visually summarizes the result of all paths in the computation tree generated by applying
        the given machine to the given word. Any state at which any computation ends is highlighted
        in crimson. Given that a machine may run forever (e.g., when it semidecides a language),
        the user may specify the maximum number of steps to explore for all possible computations.
        States at which a computation is cut off by reaching this threshold are highlighted using
        gold filling. If any final state is highlighted in crimson, then we have visual proof that
        at least one computation naturally halts. @(linebreak) @(linebreak)
        The optional
        keyword parameter @italic{palette} is a symbol used to select a color blindness pallete:
        'default for the default palette and 'deut for the deuteranopia palette. The optional
        keyword argument @italic{cutoff} is used to specify the
        maximum number of steps any possible computation may perform.
        The optional argument @italic{headpos} specifies the initial head position on the main tape
        for Turing machines and multitape Turing machines. @(linebreak)


@defproc*[([(sm-visualize [sym (or/c 'dfa 'ndfa 'pda 'tm 'mttm 'tm-language-recognizer 'mttm-language-recognizer)]) void?]
           [(sm-visualize [m machine?]
                          [inv-list (listof (listof state? procedure?)) '()]) void])]{
 When supplied with a symbol as the argument the visualization tool is started for the specified machine type.
 When supplied with a state machine as the argument the visualization tool is started with
 the state machine built within the tool. When supplied with the optional list of state and
 invariant predicates the visualization tool changes the color of the arrow to green or
 red to indicate, respectively, if the invariant holds or does not hold. @bold{For more information
  about the dynamic machine execution vizualization tool, please visit the @(hyperlink "https://morazanm.github.io/fsm/index.html" "FSM Website")}}
@(linebreak)@(linebreak)
Examples: @(linebreak)@(linebreak)
Empty Tool
@(racketblock
  #| --Empty Tool-- |#
  ;; hello
  (sm-visualize 'dfa)
  (sm-visualize 'ndfa)
  )
@(linebreak)Prebuilt Machine
@(racketblock
  #| --Prebuilt Machine-- |#
  (define a*a (make-dfa '(S F A)
                        '(a b)
                        'S   
                        '(F)          
                        '((S a F)   
                          (F a F)
                          (F b A)
                          (A a F)
                          (A b A))))
  (sm-visualize a*a)
  )
@centered{@image[#:suffixes @list[".png"]]{scribImgs/aStar}}

@(linebreak)Prebuilt Machine with Invariants
@(racketblock
  #| --Prebuilt Machine with Invariants-- |#
  (define S-INV empty?)

  (define (F-INV consumed-input)
    (and (eq? (first consumed-input) 'a)
         (eq? (last consumed-input) 'a)))

  (define (A-INV consumed-input)
    (and (eq? (first consumed-input) 'a)
         (not (eq? (last consumed-input) 'a))))

  (define (DEAD-INV consumed-input)
    (not (eq? (first consumed-input) 'a)))


  ;; visualize the machine
  (sm-visualize a*a (list 'S S-INV) 
                (list 'F F-INV)
                (list 'A A-INV) 
                (list 'ds DEAD-INV)))
@centered{@image[#:suffixes @list[".png"]]{scribImgs/aStarInv}}


@section{State Machine Observers}

@defproc[(sm-states [m machine?])
         (listof state)]{Returns the states of the given state 
 machine.}

@defproc[(sm-sigma [m machine?])
         alphabet]{Returns the alphabet of the given state 
 machine.}

@defproc[(sm-rules [m machine?])
         (listof smrule)]{Returns the rules of the given state 
 machine.}

@defproc[(sm-start [m machine?])
         state]{Returns the start state of the given state machine.}

@defproc[(sm-finals [m machine?])
         (listof state)]{Returns the final states of the given state 
 machine.}

@defproc[(sm-gamma [m ndpda])
         (listof symbol)]{Returns the stack alphabet of the given pushdown
 automaton.}

@defproc[(sm-type [m machine?])
         symbol]{Returns a symbol indicating the type of the given
 machine: dfa, ndfa, ndpda, tm, tm-language-recognizer, mttm, or mttm-language-recognizer.}

@defproc[(sm-numtapes [m machine?])
         symbol]{Returns the number of tapes in the given state machine.}

@defproc[(sm-apply [m machine?] [w word?] [n natural? 0]) symbol]
 Applies the given state machine to the given word
 and returns either @racket['accept] or @racket['reject] for a dfa, a
 ndfa, a ndpda, a Turing machine language 
 recognizer, or a multitape Turing machine language recognizer. If the given machine
 is a Turing machine, but not a language recognizer, a (list @racket['Halt:] S) is
 returned where S is a state. The optional natural 
 number is only used for the initial position of the 
 Turing machine head or the head position for the first tape
 in a multitape Turing machine.

@defproc[(sm-showtransitions [m machine?] [w word?] [n natural? 0]) (or/c (listof smconfig) 'reject)]
 Applies the given state machine to the given word
 and returns a list of configurations if the machine
 reaches a final/halting state and @racket['reject] otherwise. The 
 optional natural number is only used for the initial position of a 
 Turing machine's head or the head position for the first tape
 in a multitape Turing machine.

@section{Construction Visualization}

@bold{All construction visualization tools use the arrow keys to move the
      visualization forwards or backwards. The right arrow moves the visualization
      one step forward. The left arrow moves the visualization one step backwards.
      The up arrow moves the visualization to the beginning. The down arrow moves
      the visualization to the end.}

@defproc[(union-viz [m1 ndfa] [m2 ndfa]) (void)]
Launches a visualization tool for the construction of an ndfa that decides
L = {w | w∈L(m1) ∨ w∈L(m2).

@defproc[(concat-viz [m1 ndfa] [m2 ndfa]) (void)]
Launches a visualization tool for the construction of an ndfa that decides
L = {uv | u∈L(m1) ∧ v∈L(m2).

@defproc[(complement-viz [m dfa]) (void)]
Launches a visualization tool for the construction of a dfa for the complement
of the language decided by the given dfa.

@defproc[(kleenestar-viz [m1 ndfa]) (void)]
Launches a visualization tool for the construction of an ndfa that decides
L = L(m1)*.

@defproc[(intersection-viz [m1 ndfa] [m2 ndfa]) (void)]
Launches a visualization tool for the construction of an ndfa that decides
L = L(m1) ∩ L(m2).

@defproc[(ndfa2dfa-viz [m1 ndfa]) (void)]
Launches a visualization tool for the construction of a dfa that decides
L(m1).

@defproc[(ndfa2regexp-viz [m1 ndfa]) (void)]
Launches a visualization tool for the construction of a regular expression
for L(m1).

@defproc[(regexp2ndfa-viz [r regexp]) (void)]
Launches a visualization tool for the construction of an ndfa to decide, L(r),
the language generated by the given regular expression.


@section{Derivation Visualization}

@bold{The word derivation visualization tool uses the arrow keys to move the
      visualization forwards or backwards. The right arrow moves the visualization
      one step forward. The left arrow moves the visualization one step backwards.
      The up arrow moves the visualization to the beginning. The down arrow moves
      the visualization to the end}

@defproc[(grammar-viz [g grammar]
                      [w word]
                      [#:derv-type derv-type (or/c 'left 'right 'level-left 'level-right) 'left]
                      [#:cpu-cores cpu-cores (or/c number? #f) #f]
                      [inv-list (listof (listof state? (-> word? boolean?))) (void)])
         (or/c (void) string)]
Launches a visualization tool for the construction of the derivation tree for the given
word using the given grammar. If the given word is not in the language then the tool does
not launch and a string is returned indicating that the given word is not in the given grammar's
language. The optional keyword parameter @italic{derv-type} is only used with a context-free grammar
to guide the substitution of the next nonterminal: leftmost first, rightmost first, lowest
level rightmost first, and lowest level leftmost first. The optional keyword argument @italic{cpu-cores}
is used to specify the number of cores dedicated to generating images for the visualization.
The optional argument @italic{inv-list} is used to provide a list of pairs, each containing a nonterminal
and a predicate to determine if the word generated by the nonterminal satisfies it.

@section{State Machine Testers}

@defproc*[([(sm-test [m1 machine?]) (listof (list word symbol))]
           [(sm-test [m1 machine?] [n natural?]) (listof (list word symbol))])]
         Applies the given machine to randomly generated words  and returns
          a list of words and the obtained result. If the given machine is a
          Turing machine, it must be a language recognizer. For a Turing machine
          language recognizer, the generated tests start with the left-end marker
          followed by the input word and the head on the first letter of the input
          word. The optional natural number specifies the number of tests
          (the default is 100). May not be used for Turing machines and multitape
          Turing machines.

@defproc[(sm-sameresult? [m1 machine?] [m2 machine?] [w word])
         boolean]{Tests if the two given machines return the same
 result when applied to the given word.}

@defproc[(sm-testequiv? [m1 machine?] [m2 machine?] [n natural?])
         (or boolean (listof word))]{Tests if the two given machines 
 return the same result when
 applied to the same 100 randomly
 generated words. Returns true
 if all results are the same. 
 Otherwise, a list of words for
 which different results were
 obtained is returned. May not be used for Turing machines and multitape Turing machines.}

@section{Grammar Constructors}

@defproc[(make-rg   [nt (listof nts)] 
                    [sigma alphabet] 
                    [delta (listof rrule)]
                    [start nts]
                    [#:rejects rejects (listof word) '()]
                    [#:accepts accepts (listof word) '()])
         rg]{Builds a regular grammar. The optional
             keyword parameters @italic{rejects} and @italic{accepts}
             are used to integrate testing. They are both lists of
             words using elements of @italic{sigma}. The first is a
             list of words that ought not be generated and the second is
             a list of words that ought to be generated. If any word
             produces the wrong result the constructor fails and a
             @italic{recipe-based} error is thrown.
             }

@defproc[(make-cfg  [nt (listof nts)] 
                    [sigma alphabet] 
                    [delta (listof cfrule)]
                    [start nts]
                    [#:rejects rejects (listof word) '()]
                    [#:accepts accepts (listof word) '()])
         cfg]{Builds a context-free grammar. The optional
             keyword parameters @italic{rejects} and @italic{accepts}
             are used to integrate testing. They are both lists of
             words using elements of @italic{sigma}. The first is a
             list of words that ought not be generated and the second is
             a list of words that ought to be generated. If any word
             produces the wrong result the constructor fails and a
             @italic{recipe-based} error is thrown.

             Given that derivations may be infinite, use @italic{rejects}
             with caution. For words not in the grammar's language, testing
             may cause the constructor to not terminate.
             }

@defproc[(make-csg  [nt (listof nts)] 
                    [sigma alphabet] 
                    [delta (listof csrule)]
                    [start nts]
                    [#:rejects rejects (listof word) '()]
                    [#:accepts accepts (listof word) '()])
         csg]{Builds a context-sensitive grammar. The optional
             keyword parameters @italic{rejects} and @italic{accepts}
             are used to integrate testing. They are both lists of
             words using elements of @italic{sigma}. The first is a
             list of words that ought not be generated and the second is
             a list of words that ought to be generated. If any word
             produces the wrong result the constructor fails and a
             @italic{recipe-based} error is thrown.

             Given that derivations may be infinite, use @italic{rejects}
             with caution. For words not in the grammar's language, testing
             may cause the constructor to not terminate.
             }

@defproc[(grammar-union  [g1 grammar] 
                         [g2 grammar])
         grammar]{Builds a grammar for the language obtained from
 the union of the languages of the given grammars.
 The given grammars must have the same type.}

@defproc[(grammar-concat [g1 grammar] 
                         [g2 grammar])
         grammar]{Builds a grammar for the language obtained from
 the concatenation of the languages of the given grammars.
 The given grammars must have the same type.}

@defproc[(grammar-kleenestar [g grammar])
         grammar]
        Let L(g) denote the language defined by g. This function
         builds a grammar for L(g)^*.

@defproc[(sm->grammar [m machine?])
         grammar]{Builds a grammar for the language of the given
 dfa, ndfa, or ndpda.}

@defproc[(grammar-rename-nts [nts (listof nonterminals)][g grammar])
         grammar]{Renames the nonterminals of the given grammar such that the renamed grammar
 does not include any nonterminals in the given list of nonterminals.}

@defproc[(cfg->chomsky [g cfg])
         cfg]{Converts the given context-free grammar to Chomsky normal form.}

@defproc[(cfg->greibach [g cfg])
         cfg]{Converts the given context-free grammar to Greibach normal form.}


@section{Grammar Observers}

@defproc[(grammar-nts [g grammar])
         (listof nts)]{Returns the nonterminals of the given 
 grammar.}

@defproc[(grammar-sigma [g grammar])
         alphabet]{Returns the alphabet of the given 
 grammar.}

@defproc[(grammar-rules [g grammar])
         (listof grule)]{Returns the rules of the given 
 grammar.}

@defproc[(grammar-start [g grammar])
         nts]{Returns the starting nonterminal of the given 
 grammar.}

@defproc[(grammar-type [g grammar])
         symbol]{Returns a symbol for the type of the given 
 grammar: 'rg, 'cfg, or 'csg.}

@defproc[(grammar-derive [g grammar] [w word])
         (or Derivation string)]{If the given word is in the language of the
 given grammar, a derivation is for it is 
 returned. Otherwise, a string is returned
 indicating the word is not in the language.}

@section{Grammar Testers}

@defproc[(grammar-both-derive [g1 grammar] [g2 grammar] [w word])
         boolean]{Tests if both of the given grammars obtain
 the same result when trying to derive the given
 word.}

@defproc*[([(grammar-testequiv [g1 grammar] [g2 grammar]) (or true (listof word))]
           [(grammar-testequiv [g1 grammar] [g2 grammar] [natural? n]) (or true (listof word))])]
Tests in the given grammars obtain
 the same results when deriving 100 (or the optional n)
 randomly generated words.  If all tests
 give the same result true is returned.
 Otherwise, a list or words that
 produce different results is 
 returned.

@defproc*[([(grammar-test [g1 grammar]) (listof (cons word (Derivation or string)))]
           [(grammar-test [g1 grammar] [n natural?]) (listof (cons word (Derivation or string)))])]
Tests the given grammar with 100 (or the optional n) randomly generated words.
A list of pairs containing a word and the result of attemting to derive the
word are returned.


@section{Combined Turing Machines}

@defproc[(combine-tms [d ctmd] [sigma alphabet])
         ctm]{Builds a (combined) Turing machine from the given
 ctmd and the given tape alphabet union {BLANK}.}

@defproc[(ctm-run [m ctm] [w tmtape] [i natural?])
         list]{Runs the given machine on the given tape with the
 head starting at position i (which must be a valid)
 index into w (without exceeding the length of w).
 A list containing the state the machine halts in, the
 position of the head, and the tape is returned.}

@section{Regular Expression Constructors}

@defproc[(null-regexp)
         regexp]{Builds the regular expression for the empty language.}

@defproc[(empty-regexp)
         regexp]{Builds the regular expression for the language that only contains the
                 empty string.}

@defproc[(singleton-regexp [a (or/c letter? special-char?)])
         regexp]{Builds the regular expression for the language that only has a single
                 word of length 1 representing the given letter or special character: $, &, !, *.}

@defproc[(union-regexp [r1 regexp]
                       [r2 regexp]
                       [#:sigma sigma alphabet '()]
                       [#:pred pred (-> word Boolean) (lambda (x) #t)]
                       [#:gen-cases gen-cases natnum 10]
                       [#:in-lang in-lang (listof word) empty]
                       [#:not-in-lang not-in-lang (listof word) '()])
         regexp]{Builds a union regular expression from the given
 regular expressions. The optional keyword parameters are used to
 automatically test the generated regular expression. The alphabet
 is specified by @italic{sigma}. A predicate to determine if a random word
 generated using @italic{sigma} is in the regular expression's language is
 specified by @italic{pred}. The number of words random words to test is
 specified by @italic{gen-cases}. Finally, @italic{in-lang} and @italic{non-in-lang} specify
 lists of words that are and that are not in the regular expression's
 language. If any of the tests fails the constructor fails and
 throws a @italic{recipe-based error}.}

@defproc[(concat-regexp [r1 regexp]
                        [r2 regexp]
                        [#:sigma sigma alphabet '()]
                        [#:pred pred (-> word Boolean) (lambda (x) #t)]
                        [#:gen-cases gen-cases natnum 10]
                        [#:in-lang in-lang (listof word) empty]
                        [#:not-in-lang not-in-lang (listof word) '()])
         regexp]{Builds a concatenation regular expression from the 
 given regular expressions. The optional keyword parameters are used to
 automatically test the generated regular expression. The alphabet
 is specified by @italic{sigma}. A predicate to determine if a random word
 generated using @italic{sigma} is in the regular expression's language is
 specified by @italic{pred}. The number of words random words to test is
 specified by @italic{gen-cases}. Finally, @italic{in-lang} and @italic{non-in-lang} specify
 lists of words that are and that are not in the regular expression's
 language. If any of the tests fails the constructor fails and
 throws a @italic{recipe-based error}.}

@defproc[(kleenestar-regexp [r regexp]
                            [#:sigma sigma alphabet '()]
                            [#:pred pred (-> word Boolean) (lambda (x) #t)]
                            [#:gen-cases gen-cases natnum 10]
                            [#:in-lang in-lang (listof word) empty]
                            [#:not-in-lang not-in-lang (listof word) '()])
         regexp]{Builds a Kleene star regular expression from the 
 given regular expression.The optional keyword parameters are used to
 automatically test the generated regular expression. The alphabet
 is specified by @italic{sigma}. A predicate to determine if a random word
 generated using @italic{sigma} is in the regular expression's language is
 specified by @italic{pred}. The number of words random words to test is
 specified by @italic{gen-cases}. Finally, @italic{in-lang} and @italic{non-in-lang} specify
 lists of words that are and that are not in the regular expression's
 language. If any of the tests fails the constructor fails and
 throws a @italic{recipe-based error}.}


@defproc[(fsa->regexp [m ndfa])
         regexp]{Returns a regular expression for the language of the given ndfa.}

@defproc[(simplify-regexp [r regexp])
         regexp]{Performs elementary simplifications on the given regular expression.}


@section{Regular Expression Observers}

@defproc[(regexp? [r regexp])
         Boolean]{Predicate for regular expressions.}

@defproc[(empty-regexp? [r regexp])
         Boolean]{Predicate for the empty regular expression.}

@defproc[(singleton-regexp? [r regexp])
         Boolean]{Predicate for a singleton regular expression.}

@defproc[(union-regexp? [r regexp])
         Boolean]{Predicate for a union regular expression.}

@defproc[(concat-regexp? [r regexp])
         Boolean]{Predicate for a concat regular expression.}

@defproc[(kleenestar-regexp? [r regexp])
         Boolean]{Predicate for a Kleene star regular expression.}

@defproc[(printable-regexp [r regexp])
         string]{Converts the given regular expression to a string.}

@defproc[(singleton-regexp-a [r singelton-regexp])
         string]{Extracts the string in the given singleton-regexp.}

@defproc[(union-regexp-r1 [r union-regexp])
         regexp]{Extracts the first regular expression in the given union-regexp.}

@defproc[(union-regexp-r2 [r union-regexp])
         regexp]{Extracts the second regular expression in the given union-regexp.}

@defproc[(concat-regexp-r1 [r concat-regexp])
         regexp]{Extracts the first regular expression in the given concat-regexp.}

@defproc[(concat-regexp-r2 [r concat-regexp])
         regexp]{Extracts the second regular expression in the given concat-regexp.}

@defproc[(kleenestar-regexp-r1 [r kleenestar-regexp])
         regexp]{Extracts the regular expression in the given kleenestar-regexp.}

@defproc*[([(gen-regexp-word [r regexp]) word]
           [(gen-regexp-word [r regexp] [n natural?]) word])]
        Nondeterministically generate a word in the language
        of the given regexp. The maximum nuber of repetitions
        for a Kleene star is the the optional natural number if provided.
        Otherwise, it is 20.

@defproc[(gen-concat-word [r concat-regexp] [f (regexp --> word)] [n natnum])
         word]
        Generate a word by concatenating words generated
        from the sub regular expressions in the given concat-regexp
        using the given word-generating function. The given natnum
        is used to limit the number of repetitions generated for
        nested Kleene star regular expressions (if any).

@defproc[(gen-ks-word [n natural?] [r regexp] [f (regexp --> word)])
         word]
         Generate a word in the language of the given Kleene star
         regular expression using the given word-generation function.
         The number of concatenated words generated using r is in [0..n+1].

@defproc[(extract-concat-regexps [r concat-regexp])
         (listof regexp)]
         Extract all the nested concatenated sub-regexps in the given
         concat-regexp. This includes any nested concat-regexps in r.

@defproc[(extract-union-regexps [r union-regexp])
         (listof regexp)]
         Extract all the nested unioned sub-regexps in the given
         union-regexp. This includes any nested union-regexps in r.

@defproc[(pick-regexp [r union-regexp])
         regexp]
         Nondeterministically return a nested sub-regexp from the
         given union-regexp. This includes any nested union-regexps
         in r.

@defproc[(pick-reps [n natural?])
         natnum]
         Nondeterministically return a natural number in [0..n].

@defproc[(convert-singleton [r singleton-regexp])
         word]
         Convert the given singleton-regexp to a word of length 1
         containing r's nested symbol or number.



@section{Some Useful Functions}

@defproc[(los->symbol [l (listof symbol)])
         symbol]{Converts a list of symbols into a symbol by
 concatenating the symbols in the list from
 left to right.}

@defproc[(symbol->list [s symbol])
         (listof symbol)]{Converts the given symbol into a list of 
 one-character symbols.}

@defproc[(symbol->fsmlos [s symbol])
         (listof symbol)]{Converts the given symbol into a list of 
 FSM symbols symbols. For example, 
 (symbol->fsmlos 'aS-1243b) returns '(a S-1243 b).}

@defproc[(generate-symbol [seed symbol] [l (listof symbol)])
         symbol]{Generates a random symbol that starts with seed
 and that is not in the given list of symbols.}

@defproc[(gen-state [l (listof state)])
         state]{Generates a state not in the given list of states.}

@defproc[(gen-nt [l (listof nt)])
         state]{Generates a nonterminal not in the given list of nonterminals.}

@defproc[(symbol-upcase [s symbol])
         symbol]{Builds a symbol that is the same as the given symbol,
 but with all characters in uppercase.}



@section{Contributors}
Names in no paticular order:
@itemlist[@item{Marco T. Morazán}
          @item{Rosario Antunez}
          @item{Josephine A. Des Rosiers}
          @item{Joshua Schappel}
          @item{Shamil Dzhatdoyev}
          @item{Oliwia Kempinski}
          @item{Tijana Minic}]
