#lang fsm

(require "lib.rkt")

(define TG (create-graph 'TG))

(begin
  (set! TG (add-node TG 'S #:atb (hash 'color "green")))
  (set! TG (add-node TG 'F #:atb (hash 'shape "doublecircle" 'color "red")))
  (set! TG (add-edge TG 'a 'S 'F #:atb (hash 'color "blue")))
  (graph->bitmap TG (current-directory) "fsm")
  )


