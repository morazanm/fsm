#lang racket
(require "main.rkt")
(define dfa1 (make-ndfa '(A B C)
                       '(a b c)
                       'A
                       '(B C)
                       (list '(A b C)
                             '(A b C)
                             '(A c B)
                             '(A c A))))
 