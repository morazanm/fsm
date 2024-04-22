#lang racket

(require "../fsm-core/private/callgraphs/viz-ctm.rkt"
         "../fsm-core/private/callgraphs/transdiagram-ctm2.rkt"
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

(check-equal? (last (sm-showtransitions R `(,LM a b a) 1))
              `(F 2 (,LM a b a))
              #;`((S 1 (,LM a b a))
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

(define M (combine-tms (list L
                             0
                             (cons BRANCH
                                   (list (list 'a L (list GOTO 0))
                                         (list 'b L (list GOTO 0))
                                         (list BLANK (list GOTO 1))
                                         (list LM R L (list GOTO 0))))
                             1)
                       (list 'a 'b)))

(define ML '(list L
                  0
                  (cons BRANCH
                        (list (list 'a L (list GOTO 0))
                              (list 'b L (list GOTO 0))
                              (list BLANK (list GOTO 1))
                              (list LM L (list GOTO 0))))
                  1))

(check-equal? (ctm-run M `(,LM ,BLANK a a b) 4)
              `(H 1 (,LM ,BLANK a a b)))
(check-equal? (ctm-run M `(,LM a ,BLANK a b ,BLANK b a b b) 8)
              `(H 5 (,LM a ,BLANK a b ,BLANK b a b b)))

(define M2 (combine-tms (list L
                              0
                              (cons BRANCH
                                    (list (list 'a (list GOTO 10))
                                          (list 'b (list GOTO 10))
                                          (list BLANK (list GOTO 1))
                                          (list LM (list GOTO 20))))
                              10
                              L
                              (list GOTO 0)
                              20
                              R
                              L
                              (list GOTO 0)
                              1)
                        (list 'a 'b)))

(define M2L '(list L
                   0
                   (cons BRANCH
                         (list (list 'a (list GOTO 10))
                               (list 'b (list GOTO 10))
                               (list BLANK (list GOTO 1))
                               (list LM (list GOTO 20))))
                   10
                   L
                   (list GOTO 0)
                   20
                   R
                   L
                   (list GOTO 0)
                   1))
 
(check-equal? (ctm-run M2 `(,LM ,BLANK a a b) 4)
              `(H 1 (,LM ,BLANK a a b)))
(check-equal? (ctm-run M2 `(,LM a ,BLANK a b ,BLANK b a b b) 8)
              `(H 5 (,LM a ,BLANK a b ,BLANK b a b b)))
