#lang racket
(require "constructors.rkt"
         "../constants.rkt")


(make-dfa
 `(A B)
 '(a)
 'A
 '(B)
 (list '(A a A)
       '(B a C)
       )
 ) 