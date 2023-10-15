#lang racket
;;BUG LIST

;TOOD:
;;; all lists must be able to be empty

;;; andmap contract violation when list of rules is a symbol
(make-dfa2 '(A B C D)
           '(a b c d)
           'A
           '(B C)
           'A
           #t)