#lang racket

(require "../fsm-core/private/callgraphs/viz-ctm.rkt"
         "../fsm-core/private/callgraphs/transdiagram-ctm2.rkt"
         "../main.rkt"
         rackunit)

;; PRE: tape = (LM w), where w in (a b BLANK)*
;; POST: tape = (LM w)
(define HALT (make-tm '(S)
                      `(a b)
                      `()
                      'S
                      '(S)))

(check-equal? (last (sm-showtransitions HALT `(,LM ,BLANK) 1))
              `(S 1 (,LM _)))
(check-equal? (last (sm-showtransitions  HALT `(,LM a a b a b) 4))
              `(S 4 (,LM a a b a b)))

(define H (combine-tms (list HALT) '(a b)))

(define HL '(list HALT))

(define R (make-tm '(S F)
                   '(a b)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

(define M (combine-tms (list (list BRANCH (list 'a R HALT)
                                   (list 'b R R HALT)))
                       '(a b)))

(define M2 (combine-tms (list (list BRANCH
                                    (list 'a
                                          (list BRANCH
                                                (list 'a HALT)
                                                (list 'b HALT)))
                                    (list 'b
                                          (list BRANCH
                                                (list 'a R R HALT)
                                                (list 'b 'b R R HALT)))))
                        '(a b)))

(define M2L '(list (list BRANCH
                         (list 'a
                               (list BRANCH
                                     (list 'a HALT)
                                     (list 'b HALT)))
                         (list 'b
                               (list BRANCH
                                     (list 'a R R HALT)
                                     (list 'b 'b R R HALT))))))