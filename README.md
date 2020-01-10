# FSM

One Paragraph of project description goes here



## Installation
There are two ways to install fsm onto your machine.

### Using Raco

```bash
raco pkg install https://github.com/morazanm/fsm.git
```

### Using Racket's Package Manager
![Racket Package Manager Install](install.gif)



## Usage
Once fsm is installed just require the moddule. 
```racket
(require fsm)
```



#### Building a DFA
```racket
(define a* (make-dfa '(S F)     ;; the states
                     '(a b)     ;; the input alphabet
                     'S         ;; the set of final states
                     '(F)       ;; the transition functions
                     '((S a F)
                       (F a F)
                       (F b F))))
```


#### Visualizing a Machine 
TODO


A Library for the Automata Theory Classroom

FSM Library Version 1.0

Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez

Written by: Marco T. Morazan and Rosario Antunez, 2015
