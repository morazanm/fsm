; FSM Library Version 1.0
; Copyright (C) 202- by Marco T. Morazan 

#lang scribble/manual

@(require (for-label racket setup/collects))

@title{FSM}
@author[(author+email "Marco T. Morazán" "marco.morazan@shu.edu")]
@defmodule[fsm]


A DSL for the Automata Theory Classroom

FSM is a DSL designed to help ungraduate students understand Automata Theory by
allowing them to construct and minipulate state machines and grammars.
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



@defidform[alphabet] A list of lowercase symbols with a string representation
of length one not including EMP.

@defidform[word]{
 A @italic{(listof symbol)}. Each symbol is a member of the same alphabet.}

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
 either be in the alphabet of the machine or be EMP.}


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
A list containing a state, a batural number, and a list of symbols. 
The state is the current state of the machine. The natural number is
the head's position. The list of symbols is the contents of the tape
up to the rightmost position reached, so far, by the machine.

@defidform[regexp] A regular expression. That is, strings over an 
alphabet, E, and {(, ), (), U, *} defined as follows:
@itemlist[@item{() and each element of E is a reg-exp.}
          @item{If A and B are reg-exp, so is (AB).}
          @item{If A and B are reg-exp, so is (A U B).}
          @item{If A is a reg-exp, so is (A*).}
          @item{Nothing else is a reg-exp.}]

@defidform[terms] Lowercase letters.

@defidform[nts]
For an FSM programmer, a nonterminal symbol corresponds to a upercase
letter in English: A..Z. That is, FSM programmers are limited to 26 nonterminals.
The internal representation in FSM may use symbols of the form nts-<digit>+
(e.g., A-72431). Such nonterminals may not be used in an FSM program.

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
 at least one nonterminal and J is either
 EMP or a an aggregate symbol of terminals and nonterminals.}

@defidform[state-machine]  
A representation of a statemachine in FSM. A state machine is one of the following:
@itemlist[
 @item{Deterministic Finite Automaton (dfa)}
 @item{Nondeterministic Finite Automaton (ndfa)}
 @item{Pushdown Automaton (pda)}
 @item{Turing Machine (tm)}
 @item{Language Recognizer (tm-langauge-recognizer)}]

@defidform[smrule]  
A state machine rule is either a dfa-rule, an ndfa-rule, a 
ndpda-rule, or a tm-rule.

@defidform[grammar]  
A grammar is either a regular grammar, a context-free grammar, or
a context-sensitive grammar.

@defidform[grule]  
A grammar rule is either a rrule, a cfrule, or csrule.

@defidform[Derivation]  
A derivation is either a (list nts ARROW nts) or 
a (append (list ARROW nts) Derivation).

@defidform[ctmd]  
A combined Turing machine description is either:
@itemlist[@item{empty}
          @item{(cons tm ctmd)}
          @item{(cons LABEL ctmd)}
          @item{(cons symbol ctmd}
          @item{(cons BRANCH (listof (list symbol ctmd)))}
          @item{(cons (GOTO LABEL) ctm)}
          @item{(cons ((VAR symbol) ctm) ctm)}]
A LABEL is a natnum.

@section{State Machine Constructors}


@defproc[(make-dfa [sts (listof state)] 
                   [sigma alphabet] 
                   [start state] 
                   [finals (listof state)] 
                   [delta (listof dfa-rule)])
         dfa]{Builds a deterministic finite-state automaton. @italic{delta} is a transition function.}


@defproc[(make-ndfa [sts (listof state)] 
                    [sigma alphabet] 
                    [start state] 
                    [finals (listof state)] 
                    [delta (listof ndfa-rule)])
         ndfa]{Builds a nondeterministic finite-state automaton. @italic{delta} is a transition relation.}

@defproc[(make-ndpda [sts (listof state)] 
                     [sigma alphabet] 
                     [gamma (listof symbol)]
                     [start state] 
                     [finals (listof state)] 
                     [delta (listof pda-rule)])
         ndpda]{Builds a nondeterministic pushdown automaton from the
 given list of states, alphabet, list of stack symbols,
 statr state, list of final states, and list of
 pda-rule. @italic{delta} is a transition relation.}

@defproc*[([(make-tm    [sts (listof state)] 
                        [sigma alphabet]
                        [delta (listof ndfa-rule)]
                        [start state] 
                        [finals (listof state)]) tm]
           [(make-tm    [sts (listof state)] 
                        [sigma alphabet]
                        [delta (listof ndfa-rule)]
                        [start state] 
                        [finals (listof state)]
                        (accept state)) tm])]{Builds a nondeterministic Turing machine. 
 @italic{delta} is a transition relation.
 @italic{LM} is automatically added to the machine's alphabet.
 Rules for moving off the @italic{LM} are automatically added
 to the machine's rules.
 @italic{If the optional accept argument is given then the resulting
  Turing machine is a language recognizer.}}

@defproc[(ndfa->dfa [m ndfa])
         dfa]{Builds a @italic{deterministic} finite-state 
 automaton equivalent to the given ndfa.}

@defproc[(regexp->fsa [r regexp])
         ndfa]{Builds a fsm for the language of the given
 regular expression.}

@defproc[(sm-rename-states [sts (listof state)] [m1 state-machine])
         state-machine]{Builds a state machine that is excatly the same as
 the given machine except that its states are renamed
 as to not have a name in common with the given list
 of states.}

@defproc[(sm-union [m1 state-machine] [m2 state-machine])
         state-machine]{Builds a state machine for the language obtained
 from the union of the languages of the two given
 state machines. If the inputs are Turing machines then
 they must be language recognizers. The given machines 
 must have the same type.}

@defproc[(sm-concat [m1 state-machine] [m2 state-machine])
         state-machine]{Builds a state machine for the language obtained
 from the concatenation of the languages of the two given
 state machines. [Note: Not yet implemented for Turing machines language recognizers.].}

@defproc[(sm-kleenestar [m1 state-machine])
         state-machine]{Builds a state machine for the language obtained
 from the Kleene star of the given machine's language.
 [Note: Not yet implemented for Turing machine language recognizers.].}

@defproc[(sm-complement [m1 state-machine])
         state-machine]{Builds a state machine for the language obtained
 from the complement of the given machine's language.
 The given machine can not be a ndpda. If the inputs are 
 Turing machines then they must be language recognizers.}

@defproc[(sm-intersection [m1 state-machine] [m2 state-machine])
         state-machine]{Builds a state machine for the language obtained
 from the intersection of the languages of the two given
 state machines. If the inputs are Turing machines then
 they must be language recognizers. The given machines 
 must have the same type.}

@defproc[(grammar->sm [g grammar])
         state-machine]{Builds a state machine for the language of the given
 regular or context-free grammar.}


@section{State Machine Visualization}
@bold{For more information about how the Vizualization tool works please visit the @(hyperlink "https://morazanm.github.io/fsm/index.html" "FSM Website")}
@defproc[(sm-graph [m state-machine])
         image]{Converts the given state machine to .png image.@(linebreak)}

@bold{You must have GraphViz installed as an enviroment variable
  for this to work. Please see
  for more information how to set this up. @(hyperlink "https://github.com/morazanm/fsm/tree/master/GraphViz" "FSM GraphViz ReadMe")}

@defproc*[([(sm-visualize [sym symbol?]) void]
           [(sm-visualize [m state-machine]) void]
           [(sm-visualize [m state-machine]
                          [inv-list (listof (listof symbol? procedure?))]) void])]{
 When supplied with a symbol as the argument the visualiztion tool is started for the specified machine type.
 Valid symbols are:@(racketblock 'dfa  'ndfa  'pda  'tm  'tm-language-recognizer)
 When supplied with a state-machine as the argument the visualiztion tool is started with the state-machine built within the tool
 When supplied with the third option the visualiztion tool is started with 
}
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
@(image "./GithubPages/Images/aStar.png" "img1" #:scale .6)

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
@(image "./GithubPages/Images/aStarInv.png" "img2" #:scale .6)


@section{State Machine Observers}

@defproc[(sm-getstates [m state-machine])
         (listof state)]{Returns the states of the given state 
 machine.}

@defproc[(sm-getalphabet [m state-machine])
         alphabet]{Returns the alphabet of the given state 
 machine.}

@defproc[(sm-getrules [m state-machine])
         (listof smrule)]{Returns the rules of the given state 
 machine.}

@defproc[(sm-getstart [m state-machine])
         state]{Returns the start state of the given state machine.}

@defproc[(sm-getfinals [m state-machine])
         (listof state)]{Returns the final states of the given state 
 machine.}

@defproc[(sm-getstackalphabet [m ndpda])
         (listof symbol)]{Returns the stack alphabet of the given pushdown
 automaton.}

@defproc[(sm-type [m state-machine])
         symbol]{Returns a symbol indicating the type of the given
 machine: dfa, ndfa, ndpda, tm, or 
 tm-language-recognizer.}

@defproc*[([(sm-apply [m state-machine] [w word]) symbol]
           [(sm-apply [m state-machine] [w word] [n natnum]) symbol])
         ]{Applies the given state machine to the given word
 and returns either @racket['accept] or @racket['reject] for a dfa, a
 ndfa, a ndpa, or a Turing machine language 
 recognizer. If the given machine is a Turing machine,
 but not a language recognizer, a (list @racket['Halt:] S) is
 returned where S is a state. The optional natural 
 number is only used for the initial position of a 
 Turing machine head (the default position is zero).}

@defproc*[([(sm-showtransitions [m state-machine] [w word]) (or (listof smconfig) 'reject)]
           [(sm-showtransitions [m state-machine] [w word] [n natnum]) (or (listof smconfig) 'reject)])
        ]{Applies the given state machine to the given word
 and returns a list of configurations if the machine
 reaches a halting state and @racket['reject] otherwise. The 
 optional natural number is only used for the initial position of a 
 Turing machine's head (the default position is zero)}

@section{State Machine Testers}

@defproc*[([(sm-test [m1 state-machine]) (listof (list word symbol))]
           [(sm-test [m1 state-machine] [n natnum]) (listof (list word symbol))])]
         Applies the given machine to randomly generated words  and returns
          a list of words and the obtained result. If the given machine is a
          Turing machine, it must be a language recognizer. For a Turing machine
          language recognizer, the generated tests start with the left-end marker
          followed by the input word and the head on the first letter of the input
          word. The optional natural number specifies the number of tests
          (the default is 100).

@defproc[(sm-sameresult? [m1 state-machine] [m2 state-machine] [w word])
         boolean]{Tests if the two given machines return the same
 result when applied to the given word.}

@defproc[(sm-testequiv? [m1 state-machine] [m2 state-machine] [n natnum])
         (or boolean (listof word))]{Tests if the two given machines 
 return the same result when
 applied to the same 100 randomly
 generated words. Returns true
 if all results are the same. 
 Otherwise, a list of words for
 which different results were
 obtained is returned.}

@section{Grammar Constructors}

@defproc[(make-rg   [nt (listof nts)] 
                    [sigma alphabet] 
                    [delta (listof rrule)]
                    [start nts])
         rg]{Builds a regular grammar.}

@defproc[(make-cfg  [nt (listof nts)] 
                    [sigma alphabet] 
                    [delta (listof cfrule)]
                    [start nts])
         cfg]{Builds a context-free grammar.}

@defproc[(make-csg  [nt (listof nts)] 
                    [sigma alphabet] 
                    [delta (listof csrule)]
                    [start nts])
         csg]{Builds a context-sensitive grammar.}

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

@defproc[(sm->grammar [m state-machine])
         grammar]{Builds a grammar for the language of the given
 dfa, ndfa, or ndpda.}

@defproc[(grammar-rename-nts [nts (listof nonterminals)][g grammar])
         grammar]{Renames the nonterminals of the given grammar such that the renamed grammar
 does not include any nonterminals in the given list of nonterminals.}  

@section{Grammar Observers}

@defproc[(grammar-getnts [g grammar])
         (listof nts)]{Returns the nonterminals of the given 
 grammar.}

@defproc[(grammar-getalphabet [g grammar])
         alphabet]{Returns the alphabet of the given 
 grammar.}

@defproc[(grammar-getrules [g grammar])
         (listof grule)]{Returns the rules of the given 
 grammar.}

@defproc[(grammar-getstart [g grammar])
         nts]{Returns the starting nonterminal of the given 
 grammar.}

@defproc[(grammar-gettype [g grammar])
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
           [(grammar-testequiv [g1 grammar] [g2 grammar] [natnum n]) (or true (listof word))])]
Tests in the given grammars obtain
 the same results when deriving 100 (or the optional n)
 randomly generated words.  If all tests
 give the same result true is returned.
 Otherwise, a list or words that
 produce different results is 
 returned.

@defproc*[([(grammar-test [g1 grammar]) (listof (cons word (Derivation or string)))]
           [(grammar-test [g1 grammar] [n natnum]) (listof (cons word (Derivation or string)))])]
Tests the given grammar with 100 (or the optional n) randomly generated words.
A list of pairs containing a word and the result of attemting to derive the
word are returned.


@section{Combined Turing Machines}

@defproc[(combine-tms [d ctmd] [sigma alphabet])
         ctm]{Builds a (combined) Turing machine from the given
 ctmd and the given tape alphabet union {BLANK}.}

@defproc[(ctm-run [m ctm] [w tmtape] [i natnum])
         list]{Runs the given machine on the given tape with the
 head starting at position i (which must be a valid)
 index into w (without exceeding the length of w).
 A list containing the state the machine halts in, the
 position of the head, and the tape is returned.}

@section{Regular Expression Constructors}

@defproc[(empty-regexp)
         regexp]{Builds the regular expression for the empty string.}

@defproc[(singleton-regexp [a letter])
         regexp]{Builds the regular expression for a single
 letter string.}

@defproc[(union-regexp [r1 regexp] [r2 regexp])
         regexp]{Builds a union regular expression from the given
 regular expressions.}

@defproc[(concat-regexp [r1 regexp] [r2 regexp])
         regexp]{Builds a concatenation regular expression from the 
 given regular expressions.}

@defproc[(kleenestar-regexp [r regexp])
         regexp]{Builds a Kleene star regular expression from the 
 given regular expression.}


@defproc[(fsa->regexp [m ndfa])
         reg-exp]{Returns a regular expression for the language of
 the given ndfa. Warning: be careful with this function, as it can quickly cause DrRacket to run out of memory.}


@section{Regular Expression Observers}

@defproc[(printable-regexp [r regexp])
         string]{Converts the given regular expression to a string.}

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

@defproc[(symbol-upcase [s symbol])
         symbol]{Builds a symbol that is the same as the given symbol,
 but with all characters in uppercase.}

@section{Contributors}
Names in no paticular order:
@itemlist[@item{Marco T. Morazán}
          @item{Rosario Antunez}
          @item{Josephine A. Des Rosiers}
          @item{Joshua Schappel}
          @item{Sachin Mahashabde}
          @item{Sena Karsavran}
          @item{Isabella Felix}]



