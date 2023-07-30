#lang racket
;;BUG LIST

;TOOD:
;;; pinpoint where in the rule we're finding issue
;;;;; ie. break it up by state, alphabet, action etc
;;; rules should be seperated by newlines
;;; need a contract for the number of tapes on a mttm

;rules-predicates
;o Should we check to make sure everything in the list is a symbol

;validation-flat-contracts
;o error message for validation words in sigma is ugly
