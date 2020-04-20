#lang racket
(require test-engine/racket-tests "render-graph.rkt" "lib.rkt"  "../fsm-main.rkt")

;; ---- TEST MACHINES ----
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

(define dummy1 (make-dfa '(A B C)
                         '(a b)
                         'A
                         '(A C)
                         '((A a A)
                           (A b B)
                           (B a C)
                           (B b A)
                           (C a C)
                           (C b C))))

; L(KLEENESTAR-abUaba) = (abUaba)*
(define KLEENESTAR-abUaba (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5) ;the states
                                      '(a b)                    ;the alphabet
                                     'Q-0                       ;the starting state
                                     '(Q-0)                     ;the final states
                                     `((Q-0 a Q-1)              ;the transition relation
                                       (Q-1 b Q-2)
                                       (Q-2 a Q-3)
                                       (Q-3 ,EMP Q-0)
                                       (Q-0 a Q-4)
                                       (Q-4 b Q-5)
                                       (Q-5 ,EMP Q-0))))


;; a*a states->nodes
(define testG1 (create-graph 'G))
(check-expect (begin
                (states->nodes (sm-getstates a*a)
                               (sm-getstart a*a)
                               (sm-getfinals a*a)
                               testG1)
                (graph-node-list testG1))
              (list (node 'A 'A 'black 'none)
                    (node 'F 'F 'black 'final)
                    (node 'S 'S 'black 'start)
                    (node 'ds 'ds 'black 'none)))

;; dummy1 states->nodes
(define testG2 (create-graph 'G))
(check-expect (begin
                (states->nodes (sm-getstates dummy1)
                               (sm-getstart dummy1)
                               (sm-getfinals dummy1)
                               testG2)
                (graph-node-list testG2))
              (list (node 'C 'C 'black 'final)
                    (node 'B 'B 'black 'none)
                    (node 'A 'A 'black 'startfinal)
                    (node 'ds 'ds 'black 'none)))


(define testG4 (create-graph 'G))
(check-expect (begin
                (states->nodes (sm-getstates KLEENESTAR-abUaba)
                               (sm-getstart KLEENESTAR-abUaba)
                               (sm-getfinals KLEENESTAR-abUaba)
                               testG4)
                (graph-node-list testG4))
              (list (node 'Q5 'Q-5 'black 'none)
                    (node 'Q4 'Q-4 'black 'none)
                    (node 'Q3 'Q-3 'black 'none)
                    (node 'Q2 'Q-2 'black 'none)
                    (node 'Q1 'Q-1 'black 'none)
                    (node 'Q0 'Q-0 'black 'startfinal)
                    (node 'ds 'ds 'black 'none)))






;; a*a rules->edges
(define testG3 (create-graph 'G))
(check-expect (begin
                (rules->edges (sm-getrules dummy1) testG3)
                (graph-edge-list testG3))
              (list (edge '(b a) 'black 'ds 'ds)
                    (edge '(b a) 'black 'C 'C)
                    (edge '(b) 'black 'B 'A)
                    (edge '(a) 'black 'B 'C)
                    (edge '(b) 'black 'A 'B)
                    (edge '(a) 'black 'A 'A)))






(test)