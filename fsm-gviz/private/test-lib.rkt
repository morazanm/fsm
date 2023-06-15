#lang fsm

(require "lib.rkt")

(define TG (create-graph 'TG #:atb (hash 'arg "MGRAPH" 'rankdir "LR")))

(begin
  (set! TG (add-node TG 'S #:atb (hash 'color "green")))
  (set! TG (add-node TG 'F #:atb (hash 'shape "doublecircle" 'color "red")))
  (set! TG (add-edge TG 'a 'S 'F #:atb (hash 'color "blue")))
  (set! TG (add-edge TG 'a 'F 'F))
  (set! TG (add-edge TG 'b 'F 'F))
  (set! TG (add-edge TG 'c 'S 'F))
  (graph->bitmap TG (current-directory) "fsm")
  )


