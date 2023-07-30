#lang racket
;;BUG LIST

;TOOD:
;;; pinpoint where in the rule the action fails


;shared-predicates
;o Final states must be a list

;rules-predicates
;o Should we check to make sure everything in the list is a symbol

;validation-predicates
;o listof-words should check if the words are only using symbols from the sigma

;shared-flat-contracts
;o if the states, sigma, or finals arent a list, it breaks the contracts