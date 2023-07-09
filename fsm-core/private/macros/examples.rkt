#lang racket
(require "constructors.rkt")


(make-dfa
 `(A B C)
 '(a b)
 'A
 '(B C D)
 (list '(A a C)
       '(B a B)
       '(C a A)
       )
 ) 