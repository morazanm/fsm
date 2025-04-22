#lang racket/base

(require "fsm-gviz/interface.rkt" "fsm-core/private/sm-getters.rkt"
         "fsm-core/private/callgraphs/transdiagram-mttm.rkt"
         "fsm-core/private/callgraphs/transdiagram-ctm6.rkt")

(provide sm-graph ctm-graph)

;; sm-graph :: fsa optional(number) -> bitmap
;; draws a graph of the given machine and returns the bitmap so it
;; can be displayed in the DrRacket Terminal
(define (sm-graph fsa #:color [color-blind-mode 0])
  (when (or (< color-blind-mode 0) (> color-blind-mode 2))
    (error 'sm-graph "Invalid color option. Must be either 0, 1, or 2. Given ~a" color-blind-mode))
  (if (or (eq? (sm-type fsa) 'mttm)
          (eq? (sm-type fsa) 'mttm-language-recognizer))
      (transition-diagram-mttm fsa)
      (fsa->bitmap fsa color-blind-mode)))

(define ctm-graph transition-diagram-ctm)