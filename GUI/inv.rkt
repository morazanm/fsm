#lang racket
(require
  "globals.rkt"
  "./structs/machine.rkt"
  "./structs/state.rkt")

(provide
 determin-inv)

;; determin-inv :: machine -> rule -> number -> Optional KeywordArg -> color
;; determins the color that should be displayed for the invariant.
(define (determin-inv machine cur-state tape-index #:graphViz[gv false])
  (define state-list (machine-state-list machine))
  (define index (get-state-index state-list cur-state 0))
  (determin-inv-color (fsm-state-function (list-ref (machine-state-list machine) index))
                      (if (or (equal? MACHINE-TYPE 'tm) (equal? MACHINE-TYPE 'tm-language-recognizer))
                          (take (machine-sigma-list machine) (tm-machine-tape-posn machine))
                          (take (machine-sigma-list machine) (if (< tape-index 0) 0 (add1 tape-index))))
                      machine
                      gv))

;; get-state-index :: list-of-states -> symbol -> num -> num
;; Purpose: finds the index of the given state in the list of states. Note that a
;;     state can not be repeated in the list.
(define (get-state-index los s accum)
  (cond
    [(empty? los) -1] ;; this case should never be reached
    [(equal? (fsm-state-name (car los)) s) accum]
    [else (get-state-index (cdr los) s (add1 accum))]))
                            

;; determin-inv-color :: function -> list -> machine -> mode -> color
(define (determin-inv-color func p-list machine mode)
  (define true-color (if COLOR-BLIND-MODE
                         (if mode TRUE-INV-CB-HEX TRUE-INV-CB)
                         (if mode TRUE-INV-HEX TRUE-INV)))
  (define false-color (if COLOR-BLIND-MODE
                          (if mode FALSE-INV-CB-HEX FALSE-INV-CB)
                          (if mode FALSE-INV-HEX FALSE-INV)))
  (define caution-color (if COLOR-BLIND-MODE
                            (if mode CAUTION-INV-HEX CAUTION-INV)
                            (if mode CAUTION-INV-HEX CAUTION-INV)))
  (define non-color (if mode "transparent" DEFAULT-ARROW-COLOR))
  (match (machine-type machine)
    ['pda
     (cond
       [(equal? #t (func p-list STACK-LIST)) true-color]
       [(equal? #f (func p-list STACK-LIST)) false-color]
       [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
       [else caution-color])]
    ['tm
     (let ((tape-posn (tm-machine-tape-posn machine))
           (tape (machine-sigma-list machine)))
       (cond
         [(equal? #t (func tape tape-posn)) true-color]
         [(equal? #f (func tape tape-posn)) false-color]
         [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
         [else caution-color]))]
    ['tm-language-recognizer
     (let ((tape-posn (tm-machine-tape-posn machine))
           (tape (machine-sigma-list machine)))
       (cond
         [(equal? #t (func tape tape-posn)) true-color]
         [(equal? #f (func tape tape-posn)) false-color]
         [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
         [else caution-color]))]
    [_
     (cond
       [(equal? #t (func p-list)) true-color]
       [(equal? #f (func p-list)) false-color]
       [(equal? PLACEHOLDER (func p-list)) non-color]
       [else caution-color])]))