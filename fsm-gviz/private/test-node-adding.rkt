#lang racket
(require "lib.rkt")

(define init-graph (create-graph 'cgraph #:atb (hash 'rankdir "LR")))

(define nodes '(A-1 A-2))

(graph->bitmap
 (add-edge (foldr (lambda (name graph) (add-node graph name)) init-graph nodes)
           'a-name
           'A-1
           'A-2)
 (current-directory)
 "test")

(delete-file "test.dot")
(delete-file "test.png")