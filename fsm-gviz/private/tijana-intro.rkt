#lang racket

(require fsm "lib.rkt")

(define FNAME "fsm")

(define cgraph (create-graph 'cgraph
                             #:atb (hash 'rankdir "LR")))

(set! cgraph (add-node cgraph
                       'A
                       #:atb (hash 'color 'black 'shape 'circle 'label "A" 'fontcolor 'black)))

(set! cgraph (add-node cgraph
                       'B
                       #:atb (hash 'color 'green 'shape 'circle 'label "B" 'fontcolor 'red)))

(set! cgraph (add-edge cgraph
                       "a"
                       'A
                       'B
                       #:atb (hash 'fontsize 20 'style 'solid 'fontcolor 'crimson 'color 'crimson)))

(set! cgraph (add-edge cgraph
                       'b
                       'A
                       'B
                       #:atb (hash 'fontsize 20 'style 'solid 'fontcolor 'yellow)))

(set! cgraph (add-edge cgraph
                       'b
                       'B
                       'A
                       #:atb (hash 'fontsize 20 'style 'solid)))

(let [(res (graph->bitmap cgraph))]
    res)


