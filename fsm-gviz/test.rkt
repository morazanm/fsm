#lang racket

(require "interface.rkt")

(graph->bitmap (add-edge
                #:atb (hash 'fontname "Sans")
                (add-nodes (create-graph 'test)
                                    '(A B)) "A &cup; B" 'A 'B)
               (current-directory)
               "test")
