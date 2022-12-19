; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015


(module constants racket
  (provide EMP DEAD START NUM-TESTS ARROW RIGHT LEFT BLANK LM HALT BRANCH GOTO VAR)
  
  (define EMP 'ε) ; the symbol for the empty character, must not be in the alphabet.
  
  (define DEAD 'ds) ; the symbol for the dead state.
  
  (define START 's) ; the symbol for the start state.
  
  (define NUM-TESTS 100) ; the default number of tests for testing functions
  
  (define ARROW '->) ; for grammar rules
  
  (define RIGHT 'R) ; for TMs
  
  (define LEFT 'L) ; for TMs
  
  (define BLANK '_) ; for TMs
  
  (define LM '@) ; for TMs
  
  (define HALT 'H) ; for TMs
  
  (define BRANCH 'BR) ; for ctmds
  
  (define GOTO 'GOTO) ; for ctmds
  
  (define VAR 'VAR) ; for ctmds
  )