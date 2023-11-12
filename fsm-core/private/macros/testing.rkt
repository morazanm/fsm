#lang fsm
(require "constructors.rkt"
         "../constants.rkt")
(local-require test-engine/racket-tests)
(make-dfa2 'A
           '(a b c d)
           'A
           '(B C C)
           `((A b C)
             (A c D)
             (B c D)
             (B a B))
           #t)