#lang racket

;; ------- machine.rkt -------
;; This file contains the structure for a fsm machine (dfa, ndfa, pda, ...) 
;; Written by: Joshua Schappel 8/15/2019

;; export necessary files
(provide
 (struct-out machine)
 (struct-out pda-machine))

;; machine A structure that represents a fsm machine. This structure can represent any type of machine
;; - state-list { list-of-states }: A list of state structs that the machine can be in
;; - start-state { Symbol }: The state that the machine starts in 
;; - final-state-list { list-of-symbols }: A list of states that the machine can end in
;; - rule-list { list-of-list-of-symbols }: A list of rules that the machine must follow. Each rule should be defined as '(A a B).
;;       where A and B are states and a is a sigma.
;; - sigma-list { list-of-symbols }: A list of sigmas that act as instructions for the machine
;; - alpha-list { lis-of-symbols }: A list comtaining the machines alphabet 
;; - type { Symbol }: Represents the type of machine ex: (dfa, ndfa, pda, ...)
(struct machine ([state-list #:mutable] [start-state #:mutable] [final-state-list #:mutable] [rule-list #:mutable] [sigma-list #:mutable] [alpha-list #:mutable] type) #:transparent)


;; pda-machine: A structure that is a subtype of machine
;; - stack-alpha-list { list-of-symbols }: TODO: discription
(struct pda-machine machine ([stack-alpha-list #:mutable]))