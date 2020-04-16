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
                           (C a A)
                           (C b C))))

;; a*a states->nodes
(check-expect (states->nodes (sm-getstates a*a) (sm-getstart a*a) (sm-getfinals a*a))
              (list (node 'ds 'ds 'black 'none)
                    (node 'S 'S 'black 'start)
                    (node 'F 'F 'black 'final)
                    (node 'A 'A 'black 'none)))

;; dummy1 states->nodes
(check-expect (states->nodes (sm-getstates dummy1) (sm-getstart dummy1) (sm-getfinals dummy1))
              (list (node 'ds 'ds 'black 'none)
                    (node 'A 'A 'black 'startfinal)
                    (node 'B 'B 'black 'none)
                    (node 'C 'C 'black 'final)))

;; a*a 
(check-expect (rules->edges (sm-getrules dummy1))
              (list (edge 'a 'black 'A 'A)
                    (edge 'b 'black 'A 'B)
                    (edge 'a 'black 'B 'C)
                    (edge 'b 'black 'B 'A)
                    (edge 'a 'black 'C 'A)
                    (edge 'b 'black 'C 'C)
                    (edge 'a 'black 'ds 'ds)
                    (edge 'b 'black 'ds 'ds)))




(test)