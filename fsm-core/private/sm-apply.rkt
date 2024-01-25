#lang racket

(require "sm-getters.rkt" "fsa.rkt" "pda.rkt" "tm.rkt" "mtape-tm.rkt")

(provide sm-apply sm-showtransitions)

; fsm word [natnum] --> 'accept or 'reject
(define (sm-apply M w . l)
  (let ((head (if (null? l) 0 (car l)))
        (t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (apply-fsa M w)]
          [(eq? t1 'pda) (apply-pda M w)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-apply M w head)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (mttm-apply M w head)]
          [else (error "Incorrect input to apply-fsm")])))

; fsm word [natnum] --> path
(define (sm-showtransitions M w . l)  
  (let ((head (if (null? l) 0 (car l)))
        (t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (show-transitions-fsa M w)]
          [(eq? t1 'pda) (show-transitions-pda M w)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-showtransitions M w head)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer)) (mttm-show-transitions M w head)]
          ;[(eq? t1 'dfst) (M 'show-transitions)]
          [else (error "Incorrect input to show-transitions")])))
