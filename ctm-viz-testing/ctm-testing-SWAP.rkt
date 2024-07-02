#lang racket

(require "../fsm-core/private/callgraphs/viz-ctm.rkt"
         "../main.rkt"
         rackunit)

;;  PRE: tape = (LMw) AND i=k>0 AND w in (a b)*
;; POST: tape = (LMw) AND i=k+1 AND w in (a b)*
(define R (make-tm '(S F)
                   '(a b)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

(check-equal? (sm-showtransitions R `(,LM a b a) 1)
              `((S 1 (,LM a b a))
                (F 2 (,LM a b a))))
(check-equal? (sm-showtransitions R `(,LM a b a) 3)
              `((S 3 (,LM a b a))
                (F 4 (,LM a b a ,BLANK))))
(check-equal? (second (last (sm-showtransitions R `(,LM b b a a) 3)))
              4)

;;  PRE: tape = (LMw) AND i=k>0, where w in (a b)*
;; POST: tape = (LMw) AND i=k+1
(define L (make-tm '(S H)
                   `(a b)
                   `(((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))

(check-equal? (last (sm-showtransitions L `(,LM a a) 1))
              `(H 0 (,LM a a)))
(check-equal? (last (sm-showtransitions L `(,LM ,BLANK b b a) 4))
              `(H 3 (,LM ,BLANK b b a)))

;;  PRE: headpos = k AND tape[k] = x and tape[k+1] = y
;; POST: headpos = k AND tape[k] = y and tape[k+1] = x
(define swap (combine-tms (list (list (list VAR 'i)
                                      R
                                      (list (list VAR 'j)
                                            'i
                                            L
                                            'j)))
                          '(a b)))

(define swapl '(list (list (list VAR 'i)
                           R
                           (list (list VAR 'j)
                                 'i
                                 L
                                 'j))))

(check-equal? (rest (ctm-run swap `(,LM ,BLANK a b a) 3))
              `(3 (,LM ,BLANK a a b)))
(check-equal? (rest (ctm-run swap `(,LM ,BLANK a b a a) 5))
              `(5 (@ _ a b a _ a)))

;(ctm-viz swap swapl `(,LM ,BLANK a b a) 3)

