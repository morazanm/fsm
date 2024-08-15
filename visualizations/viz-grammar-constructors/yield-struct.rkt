#lang racket

(provide (struct-out yield))
;; yield is a structure that has
;; pr - processed part of the word
;; nt - nonterminal
;; up - unprocessed part of the word
(struct yield (state up) #:transparent)
