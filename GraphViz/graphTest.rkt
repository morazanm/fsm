#lang racket
(require test-engine/racket-tests "render-graph.rkt" "lib.rkt"  "../fsm-main.rkt")
(define NO-ABAA
  (make-dfa 
   '(Q-0 Q-1 Q-2 Q-3 Q-4)
   '(a b) 
   'Q-0
   '(Q-0 Q-1 Q-2 Q-3)
   '((Q-0 a Q-1)
     (Q-0 b Q-0)
     (Q-1 a Q-1)
     (Q-1 b Q-2)
     (Q-2 a Q-3 )
     (Q-2 b Q-0)
     (Q-3 a Q-4)
     (Q-3 b Q-2)
     (Q-4 a Q-4)
     (Q-4 b Q-4))
   'nodead))


(define J (make-dfa '(A B C)
                    '(a b)
                    'A
                    '(B)
                    '((A a B)
                      (A b B)
                      (B a C))))


(sm-graph 
 J)

(sm-graph 
 NO-ABAA)

