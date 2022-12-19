#lang racket
(require
  "globals.rkt"
  "./structs/machine.rkt"
  "./structs/state.rkt")

(provide determin-inv)


(define (determin-inv machine cur-state #:graphViz[gv false])
  (let* ([state-list (machine-state-list machine)]
         [index (get-state-index state-list cur-state 0)])
    (determin-inv-color (fsm-state-function (list-ref (machine-state-list machine) index))
                        (if (or (equal? MACHINE-TYPE 'tm) (equal? MACHINE-TYPE 'tm-language-recognizer))
                            (take (machine-sigma-list machine) (tm-machine-tape-posn machine))
                            (take (machine-sigma-list machine) (if (< TAPE-INDEX-BOTTOM 0) 0 (add1 TAPE-INDEX-BOTTOM))))
                        machine
                        gv)))


;; get-sate-index: list-of-states symbol num -> num
;; Purpose: finds the index of the given state in the list of states. Note that a
;;     state can not be repeated in the list.
(define (get-state-index los s accum)
  (cond
    [(empty? los) -1] ;; this case should never be reached
    [(equal? (fsm-state-name (car los)) s) accum]
    [else (get-state-index (cdr los) s (add1 accum))]))
                            


(define (determin-inv-color func p-list machine mode)
  (let ([true-color (if COLOR-BLIND-MODE
                        (if mode 'pass TRUE-INV-CB)
                        (if mode 'pass TRUE-INV))]
        [false-color (if COLOR-BLIND-MODE
                         (if mode 'fail FALSE-INV-CB)
                         (if mode 'fail FALSE-INV))]
        [caution-color (if COLOR-BLIND-MODE
                         (if mode 'fail CAUTION-INV)
                         (if mode 'fail CAUTION-INV))]
        [non-color (if mode 'none DEFAULT-ARROW-COLOR)])
    (case MACHINE-TYPE
      [(pda)
       (cond
         [(equal? #t (func p-list STACK-LIST)) true-color]
         [(equal? #f (func p-list STACK-LIST)) false-color]
         [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
         [else caution-color])]
      [(tm)
       (let ((tape-posn (tm-machine-tape-posn machine))
             (tape (machine-sigma-list machine)))
         (cond
           [(equal? #t (func tape tape-posn)) true-color]
           [(equal? #f (func tape tape-posn)) false-color]
           [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
           [else caution-color]))]
      [(tm-language-recognizer)
       (let ((tape-posn (tm-machine-tape-posn machine))
             (tape (machine-sigma-list machine)))
         (cond
           [(equal? #t (func tape tape-posn)) true-color]
           [(equal? #f (func tape tape-posn)) false-color]
           [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
           [else caution-color]))]
      [(mttm)
       (let ((tape-posn #f)
             (tape #t))
         (cond
           [(equal? #t (func tape tape-posn)) true-color]
           [(equal? #f (func tape tape-posn)) false-color]
           [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
           [else caution-color]))]
       [(mttm-language-recognizer)
       (let ((tape-posn #f)
             (tape #t))
         (cond
           [(equal? #t (func tape tape-posn)) true-color]
           [(equal? #f (func tape tape-posn)) false-color]
           [(equal? PLACEHOLDER (func p-list STACK-LIST)) non-color]
           [else caution-color]))]
      [else
       (cond
         [(equal? #t (func p-list)) true-color]
         [(equal? #f (func p-list)) false-color]
         [(equal? PLACEHOLDER (func p-list)) non-color]
         [else caution-color])])))