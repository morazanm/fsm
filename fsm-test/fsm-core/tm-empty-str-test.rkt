#lang racket

(require fsm
         rackunit)

; example from README

; write "a" on tape
(define Ma (make-tm '(S H)                  ;the states
                    `(a b ,LM)              ;the alphabet
                    `(((S ,LM) (S ,RIGHT))  ;the transition relation
                      ((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S                      ;the starting state
                    '(H)))                  ;the halting states

(check-equal? (sm-apply Ma '()) '(Halt: H))
