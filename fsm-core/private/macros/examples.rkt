#lang racket
(require "constructors.rkt")


(make-dfa
 `(A B C)
 '(a b)
 'D
 '(B C)
 (list '(A a C)
       '(B a B)
       '(C a A)
       )
 ) 