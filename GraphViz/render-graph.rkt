#lang racket
(require "lib.rkt" "../fsm-main.rkt" test-engine/racket-tests)

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

; (listof symbols) symbol (listof symbol)-> (listof nodes) 
; Purpose: Create a list of node structures given a list of state symbols
(define (states->nodes los S lof)
  (map (lambda (x) (node x
                         x
                         'black
                         (cond [(and (equal? x S)
                                     (member x lof)) 'startfinal]
                               [(equal? x S) 'start]
                               [(member x lof) 'final]
                               [else 'none]))) los))

(check-expect (states->nodes (sm-getstates a*a) (sm-getstart a*a) (sm-getfinals a*a))
              (list (node 'ds 'ds 'black 'none)
                    (node 'S 'S 'black 'start)
                    (node 'F 'F 'black 'final)
                    (node 'A 'A 'black 'none))) 
                    
(define a*a-graph (create-graph 'a*a #:nodes (states->nodes (sm-getstates a*a) (sm-getstart a*a) (sm-getfinals a*a))))

(render-graph a*a-graph "test.dot")

(test)