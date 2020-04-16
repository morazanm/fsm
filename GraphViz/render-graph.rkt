#lang racket
(require "lib.rkt" "../fsm-main.rkt"  test-engine/racket-tests 2htdp/image)

(provide
 states->nodes
 rules->edges)



; states->nodes: (listof symbols) symbol (listof symbol) graph -> (listof nodes) 
; Purpose: Create a list of node structures given a list of state symbols
(define (states->nodes los S lof G)
  (map (lambda (x) (add-node
                      G
                      x
                      x
                      'black
                      (cond [(and (equal? x S)
                                  (member x lof)) 'startfinal]
                            [(equal? x S) 'start]
                            [(member x lof) 'final]
                            [else 'none]))) los))


;; rules->edges: (listof rules) graph -> (listof edges)
;; Purpose: Creates a list of edges when given a list of rules
(define (rules->edges lor G)
  (map (lambda (rule) (add-edge G
                                  (cadr rule)
                                  'black
                                  (car rule)
                                  (last rule))) lor))










; L(a*a) = {w | w starts and ends with an a}
(define a*a (make-dfa '(S F A)       ;the states
                      '(a b)         ;the alphabet
                      'S             ;the starting state
                      '(F)           ;final states
                      '((S a F)      ;the transition function
                        (F a F)
                        (F b A)
                        (A a F)
                        (A b A))))
 
                    
(define a*a-graph (create-graph 'aStara))
(states->nodes (sm-getstates a*a) (sm-getstart a*a) (sm-getfinals a*a) a*a-graph)
(rules->edges (sm-getrules a*a) a*a-graph)

;;(render-graph a*a-graph "test.dot")
;;(dot->png "test.dot" "test")
;;(bitmap "test.png")
(graph->bitmap a*a-graph "test.dot" "test")
