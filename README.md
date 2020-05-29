# FSM
##### FSM Library Version 1.4
A Library for the Automata Theory Classroom

## Version History
### 1.5
- Fixed bug for show-transitions for Turing machines
- Fixed bug in rename-states
- Fixed bug in consume in tm.rkt for Language Recognizers
- Fixed rename states for Turing machines and Language Recognizers 
### 1.4: 
 - Added sm-graph 
 - Added colorblind mode to sm-graph 
 - Minor graphical changes to the visualization tool (new color :blush:)
### 1.3:
 - Added Turing Machine support to visualization tool
 - Minor bug fixes with the visualization tool
### 1.2:
 - Added Pushdown Automata support to the visualization tool
 - Added scroll bars to input field for visualization tool

## Documentation
The documentation can be found  [here](https://htmlpreview.github.io/?https://github.com/morazanm/fsm/blob/master/fsm.html).


## Installation
There are two ways to install fsm onto your machine.

### Using Raco

```bash
raco pkg install https://github.com/morazanm/fsm.git
```

### Using DrRacket's Package Manager
![Racket Package Manager Install](install.gif)


Once fsm is installed just require the module. 
```racket
(require fsm)
```

### GraphViz Installation 
In order to install GraphViz onto your computer and as an environment variable, check out this [link.](https://github.com/morazanm/fsm/tree/master/GraphViz)

## Basic Usage
Below are some basic examples of how to use fsm. For a more in-depth guide please visit the [fsm documentation](https://htmlpreview.github.io/?https://github.com/morazanm/fsm/blob/master/doc/fsm/index.html).

#### Building a DFA
```racket
(require fsm)

; L(a*a) = {w | w starts and ends with an a}
(define a*a (make-dfa '(S F A)       ;the states
                      '(a b)         ;the alphabet
                      'S             ;the starting state
                      '(F)           ;final states
                      '((S a F)      ;the transition function
                        (F a F)
                        (F b A)
                        (A a F)
                        (A b A))))
```

#### Building a NDFA
```racket
(require fsm)

; L(KLEENESTAR-abUaba) = (abUaba)*
(define KLEENESTAR-abUaba (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5) ;the states
                                      '(a b)                    ;the alphabet
                                     'Q-0                       ;the starting state
                                     '(Q-0)                     ;the final states
                                     `((Q-0 a Q-1)              ;the transition relation
                                       (Q-1 b Q-2)
                                       (Q-2 a Q-3)
                                       (Q-3 ,EMP Q-0)
                                       (Q-0 a Q-4)
                                       (Q-4 b Q-5)
                                       (Q-5 ,EMP Q-0))))
```
#### Building a PDA
```racket
(require fsm)

; L = {wcw^r | w in {a, b)*}
(define pda-wcw^r (make-ndpda '(S M N F)                  ;the states
                              '(a b c)                    ;the alphabet
                              '(a b)                      ;the stack alphabet
                              'S                          ;the starting state
                              '(F)                        ;the final state
                              `(((S ,EMP ,EMP) (M ,EMP))  ;the transition relation
                                ((M a ,EMP) (M (a)))
                                ((M b ,EMP) (M (b)))
                                ((M c ,EMP) (N ,EMP))
                                ((N a (a)) (N ,EMP))
                                ((N b (b)) (N ,EMP))
                                ((N ,EMP ,EMP) (F ,EMP)))))
```
#### Building a TM
```racket
(require fsm)

; write "a" on tape
(define Ma (make-tm '(S H)                  ;the states
                    `(a b ,LM)              ;the alphabet
                    `(((S ,LM) (S ,RIGHT))  ;the transition relation
                      ((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S                      ;the starting state
                    '(H)))                  ;the halting states
```


#### Visualizing a Machine 
To visualize a dfa or ndfa create a new file and require fsm. Then provide one of the follwing options:

1) sm-visualize &lt;machine-type&gt; To visualize a machine from scratch.
```racket
(sm-visualize 'pda) ;; Where the machine type is a symbol
```

2) sm-visualize &lt;pre-built-machine&gt; To visualize a pre-built fsm machine.
```racket
(sm-visualize a*) ;; See "Building a DFA" for the implementation of a*
```

3) sm-visualize &lt;pre-built-machine (state invariant-function)*&gt; To visualize a pre-built fsm machine with associated state invariants. Note that *(state invariant-function)* is a abitrary number of tuples.
```racket
;; Invariant functions
(define INV1 (lambda (v) true))
(define INV2 (lambda (v) false))

;; Visualize the machine 
(sm-visualize a* (list 'S INV1) (list 'F INV2))
```

## Future Additions
- [X] Extend the visualization tool to work for pda's
- [X] Extend the visualization tool to work for turing machines
- [ ] Extend the visualization tool to give the user the option to view the graph represention of a machine
- [ ] Upgrade the random testing extension

## Publications
- [Functional Automata - Formal Languages for Computer Science Students](https://arxiv.org/abs/1412.4878)
- [FSM Error Messages](https://arxiv.org/abs/1906.11421v1)

## Contributors
- [Marco T. Morazán](https://github.com/morazanm)
- Rosario Antunez
- [Josephine A. Des Rosiers](https://github.com/josdes)
- [Joshua Schappel](https://github.com/jschappel)
- [Sachin Mahashabde](https://github.com/sachinmahashabde)
- [Sena Karsavran](https://github.com/senakar)
- [Isabella Felix](https://github.com/felixisa)

## License
Copyright (c) 2020 by Marco T. Morazan
