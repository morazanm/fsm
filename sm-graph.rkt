#lang racket

(require "fsm-gviz/interface.rkt")

(provide sm-graph)

;; sm-graph :: fsa optional(number) -> bitmap
;; draws a graph of the given machine and returns the bitmap so it
;; can be displayed in the DrRacket Terminal
(define (sm-graph fsa #:color [color-blind-mode 0])
  (when (or (< color-blind-mode 0) (> color-blind-mode 2))
    (error 'sm-graph "Invalid color option. Must be either 0, 1, or 2. Given ~a" color-blind-mode))
  (fsa->bitmap fsa color-blind-mode))