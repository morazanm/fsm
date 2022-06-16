#lang racket

;; ------- machine.rkt -------
;; This file contains the structure for a fsm machine (dfa, ndfa, pda, ...) 
;; Written by: Joshua Schappel 8/15/2019
;; Last updated: 3/3/20 by Josh


;; export necessary files
(provide
 (struct-out machine)
 (struct-out pda-machine)
 (struct-out tm-machine)
 (struct-out mttm-machine)
 (struct-out lang-rec-machine)
 update-tm-machine
 update-mttm-machine
 update-lang-rec-machine
 update-lang-rec-accept-state
 reset-tm-machine-tap-index)

;; machine A structure that represents a fsm machine. This structure can represent any type of machine
;; - state-list { list-of-states }: A list of state structs that the machine can be in
;; - start-state { Symbol }: The state that the machine starts in 
;; - final-state-list { list-of-symbols }: A list of states that the machine can end in
;; - rule-list { list-of-list-of-symbols }: A list of rules that the machine must follow. Each rule should be defined as '(A a B).
;;       where A and B are states and a is a sigma.
;; - sigma-list { list-of-symbols }: A list of sigmas that act as instructions for the machine
;; - alpha-list { lis-of-symbols }: A list comtaining the machines alphabet 
;; - type { Symbol }: Represents the type of machine ex: (dfa, ndfa, pda, ...)
(struct machine ([state-list #:mutable]
                 [start-state #:mutable]
                 [final-state-list #:mutable]
                 [rule-list #:mutable]
                 [sigma-list #:mutable]
                 [alpha-list #:mutable]
                 type) #:transparent)


;; pda-machine: A structure that is a subtype of machine
;; - stack-alpha-list { list-of-symbols }: TODO: discription
(struct pda-machine machine ([stack-alpha-list #:mutable]) #:transparent)


;; tm-machine: A structure that is a subtype of machine
;; - tape-posn { Number } the current location on the tape
(struct tm-machine machine ([tape-posn #:mutable]) #:transparent)

;; mttm-machine: A structure that is a subtype of machine
;; num-tapes { list-of-numbers } the number of tapes that the machine has
;; - tape-posn-list { list-of-numbers } the current locations on the tapes
(struct mttm-machine machine (num-tapes [tape-posn-list #:mutable]) #:transparent)


;; lang-rec-machine: A structure that is a subtype of tm-machine. The one difference
;;  is that it has an accept state
;; - accept-state { Symbol } The user-defined accepting state of the machine
(struct lang-rec-machine tm-machine ([accept-state #:mutable]) #:transparent)


;; update-tm-machine-tape-posn: tm-machine int list-of-symbols -> tm-machine
;; Purpose: Builds a new tm machine with the updated tape posn
(define (update-tm-machine m new-posn new-sigma)
  (tm-machine (machine-state-list m)
              (machine-start-state m)
              (machine-final-state-list m)
              (machine-rule-list m)
              new-sigma
              (machine-alpha-list m)
              (machine-type m)
              new-posn))

;; update-mttm-machine: mttm-machine tuple list-of-symbols -> mttm-machine
;;   where tuple = (int, int)   First int is then the index of the tape list to change
;;                              second int is the new tape posn to set
;; Purpose: Builds a new mttm machine with the updated tape posn
(define (update-mttm-machine m tuples new-sigma)
  (define (has-tuple-value target tuples)
    (match tuples
      [`() #f]
      [`((,i ,v) ,r ...) (if (eq? i target) v (has-tuple-value target r))]))
  (mttm-machine (machine-state-list m)
              (machine-start-state m)
              (machine-final-state-list m)
              (machine-rule-list m)
              new-sigma
              (machine-alpha-list m)
              (machine-type m)
              (mttm-machine-num-tapes m)
              (for/list ([cur-val (mttm-machine-tape-posn-list m)]
                         [i (in-naturals)])
                (define new-val (has-tuple-value i tuples))
                (or new-val cur-val))))
                    

;; update-lang-rec-machine: lang-rec-machine int list-of-symbols -> tm-machine
;; Purpose: Builds a new tm machine with the updated tape posn
(define (update-lang-rec-machine m new-posn new-sigma)
  (lang-rec-machine (machine-state-list m)
                    (machine-start-state m)
                    (machine-final-state-list m)
                    (machine-rule-list m)
                    new-sigma
                    (machine-alpha-list m)
                    (machine-type m)
                    new-posn
                    (lang-rec-machine-accept-state m)))


;; update-lang-rec-accept-state: lang-rec-machine: symbol -> lang-rec-machine
;; Purpose: Builds an new machine identical to the previous, but with a new
;;  accept state
(define (update-lang-rec-accept-state m new-state)
  (lang-rec-machine (machine-state-list m)
                    (machine-start-state m)
                    (machine-final-state-list m)
                    (machine-rule-list m)
                    (machine-sigma-list m)
                    (machine-alpha-list m)
                    (machine-type m)
                    (tm-machine-tape-posn m)
                    new-state))

;; reset-tm-machine-tap-index: tm-machine -> none
;; Purpose: sets the tm-machines tape index to 0
;; WARNING: this function uses mutation
(define (reset-tm-machine-tap-index tm)
  (begin
    (set-tm-machine-tape-posn! tm 0)))