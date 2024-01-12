#lang fsm

(define my-dfa (make-dfa '(A B C) 
                         '(a b c) 
                         'a 
                         '(B C D) 
                         '((A b D) 
                           (A a c) 
                           (B b B) 
                           (B a c))
                         ))
