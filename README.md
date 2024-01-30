# FSM   ![Test + Build Status](https://github.com/morazanm/fsm/actions/workflows/ci.yml/badge.svg) ![Docs](https://github.com/morazanm/fsm/actions/workflows/docs.yml/badge.svg)
A DSL for the Automata Theory Classroom


## Documentation
- [Library docs](https://morazanm.github.io/fsm/fsm/index.html)
- [Internal developer docs](https://morazanm.github.io/fsm/dev/index.html)
- [Website](https://morazanm.github.io/fsm/)

## Installation
- [Installing fsm on your machine](fsm-docs/readmes/fsm_install.md)
- [Installing graphviz on your machine](fsm-gviz/README.md)


## Contributing
If you wish to contribute to `fsm` please read the [contribution docs](fsm-docs/readmes/contribute.md)


## Publications
- [Programming-Based Formal Languages and Automata Theory](https://link.springer.com/book/10.1007/978-3-031-43973-5)
- [Functional Automata - Formal Languages for Computer Science Students](https://arxiv.org/abs/1412.4878)
- [FSM Error Messages](https://arxiv.org/abs/1906.11421v1)
- [Visual Designing and Debugging of Deterministic Finite-State Machines in FSM](https://arxiv.org/abs/2008.09254)
- [Regular Expressions in a CS Formal Languages Course](https://arxiv.org/abs/2308.06969v1)
- [Visualizing a Nondeterministic to Deterministic Finite-State Machine Transformation](https://arxiv.org/abs/2310.08248)
- [Visualizing Why Nondeterministic Finite-State Automata Reject](https://arxiv.org/abs/2310.08025)
- [Composing Turing Machines in FSM](https://dl.acm.org/doi/10.1145/3622780.3623647)

## Getting Started
Once fsm is installed you use one of the following two options
```racket
#lang fsm
```


### Basic Usage
Below are some basic examples of how to use fsm. For a more in-depth guide please visit the [fsm documentation](https://morazanm.github.io/fsm/fsm/index.html).


### Building a DFA
```racket
#lang fsm 

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

### Building a NDFA
```racket
#lang fsm

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
### Building a PDA
```racket
#lang fsm

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
### Building a TM
```racket
#lang fsm

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


### Visualizing a Machine 
To visualize a dfa or ndfa create a new file and require fsm. Then provide one of the follwing options:

1) sm-visualize &lt;machine-type&gt; To visualize a machine from scratch.
```racket
(sm-visualize 'pda) ;; Where the machine type is a symbol
```

2) sm-visualize &lt;pre-built-machine&gt; To visualize a pre-built fsm machine.
```racket
(sm-visualize a*) ;; See "Building a DFA" for the implementation of a*
```

3) sm-visualize &lt;pre-built-machine (state invariant-function)*&gt; To visualize a pre-built fsm machine with associated state invariants. Note that *(state invariant-function)* is a arbitrary number of tuples.
```racket
;; Invariant functions
(define INV1 (lambda (v) true))
(define INV2 (lambda (v) false))

;; Visualize the machine 
(sm-visualize a* (list 'S INV1) (list 'F INV2))
```

## Current Maintainers
- [Marco T. Morazán](https://github.com/morazanm)
- [Joshua Schappel](https://github.com/jschappel)

## Notable Contributors 
- [Marco T. Morazán](https://github.com/morazanm)
- Rosario Antunez
- [Josephine A. Des Rosiers](https://github.com/josdes)
- [Joshua Schappel](https://github.com/jschappel)
- [Sachin Mahashabde](https://github.com/sachinmahashabde)
- [Tijana Minic](https://github.com/tijanaminic1)
- [Oliwia Kempinski](https://github.com/oliwial23)

## License
Copyright (c) 2020 by Marco T. Morazan
