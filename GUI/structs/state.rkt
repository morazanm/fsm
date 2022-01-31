#lang racket

(require "posn.rkt")

;; ------- machine.rkt -------
;; This file contains the structure for fsm state
;; Written by: Joshua and Sash 9/10/2019

(provide
 (struct-out fsm-state)
 fsm-state-pressed?)

(define RADIUS 25) ;; The radius of the state

;; fsm-state: A structure that represents a state in the GUI
;; - name { symbol }: The name of the state
;; - function { lambda }: A invariant function associated with the state
;; - posn { posn }: The position of the state
(struct fsm-state (name function posn) #:mutable #:transparent)


;; state-pressed x y -> boolean
;; Purpose: Determins if a specific state was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the state. If so returns true
(define (fsm-state-pressed? mouse-x mouse-y state)
  (let ( ;; The posn of the given state
        (loc (fsm-state-posn state)))
    (cond
      ;; Math.sqrt((x1-x0)*(x1-x0) + (y1-y0)*(y1-y0)) < r
      [(< (sqrt (+ (sqr (- mouse-x (posn-x loc))) (sqr(- mouse-y (posn-y loc))))) RADIUS) #t]
      [else #f])))