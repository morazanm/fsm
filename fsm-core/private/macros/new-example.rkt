#lang fsm

(require "constructors.rkt"
         "../constants.rkt")

(define my-dfa (make-dfa2 '(A B C) 
                          '(a b c) 
                          'a
                          '(B C D) 
                          '((A b D) 
                            (A a c) 
                            (B b B) 
                            (B a c))))