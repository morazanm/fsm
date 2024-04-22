#lang racket

(require "../fsm-core/private/callgraphs/viz-ctm.rkt"
         "../main.rkt"
         rackunit)

;;  PRE: tape = (LMw) AND i=k>0 AND w in (d)*
;; POST: tape = (LMw) AND i=k+1 AND w in (d)*
(define R (make-tm '(S F)
                   '(d)
                   `(((S d) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

(check-equal? (sm-showtransitions R `(,LM d d d) 1)
              `((S 1 (,LM d d d))
                (F 2 (,LM d d d))))
(check-equal? (sm-showtransitions R `(,LM d d d) 3)
              `((S 3 (,LM d d d))
                (F 4 (,LM d d d ,BLANK))))
(check-equal? (second (last (sm-showtransitions R `(,LM d d d d) 3)))
              4)

;;  PRE: tape = (LMw) AND i=k>0, where w in (a b)*
;; POST: tape = (LMw) AND i=k+1
(define L (make-tm '(S H)
                   `(d)
                   `(((S d) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (d BLANK)* AND s in (d BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=d
(define Wd (make-tm '(S H)
                    `(d)
                    `(((S d) (H d))
                      ((S ,BLANK) (H d)))
                    'S
                    '(H)))

;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (d BLANK)* AND s in (d BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=BLANK
(define WB (make-tm '(S H)
                    `(d)
                    `(((S d) (H ,BLANK))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))

(check-equal? (last (sm-showtransitions WB `(,LM d d) 1))
              `(H 1 (,LM ,BLANK d)))
(check-equal? (last (sm-showtransitions WB `(,LM d d d d) 3))
              `(H 3 (,LM d d ,BLANK d)))

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i>k AND tape[i]=BLANK AND tape[k+1..i-1] != BLANK
(define FBR (combine-tms (list 0
                               R
                               (cons BRANCH
                                     (list (list 'd (list GOTO 0))
                                           (list BLANK (list GOTO 10))))
                               10)
                         (list 'd)))

(check-equal? (ctm-run FBR `(,LM ,BLANK) 1)
              `(F 2 (,LM ,BLANK ,BLANK)))
(check-equal? (ctm-run FBR `(,LM d d d d d ,BLANK d d) 2)
              `(F 6 (,LM d d d d d ,BLANK d d)))
(check-equal? (ctm-run FBR `(,LM ,BLANK ,BLANK d d) 3)
              `(F 5 (,LM ,BLANK ,BLANK d d ,BLANK)))

;; PRE:  tape = (LM w) AND i=k>0 AND w in (d BLANK)*
;; POST: tape = (LM w) AND i>k AND tape[i]=d AND tape[k+1..i-1] != d
(define FdR (combine-tms (list 0
                               R
                               (cons BRANCH
                                     (list (list 'd (list GOTO 10))
                                           (list BLANK (list GOTO 0))))
                               10)
                         (list 'd)))

(define swap (combine-tms (list (list (list VAR 'i)
                                      R
                                      (list (list VAR 'j)
                                            'i
                                            L
                                            'j)))
                          '(d)))

;; PRE:  tape = (LM w) AND i=k>0 AND w in (d BLANK)* AND exists j<i s.t. tape[j]=BLANK
;; POST: tape = (LM w) AND i<k AND tape[i]=BLANK AND tape[i+1..|w|] != BLANK
(define FBL (combine-tms (list 0
                               L
                               (cons BRANCH
                                     (list (list 'd (list GOTO 0))
                                           (list BLANK (list GOTO 1))
                                           (list LM (list GOTO 0))))
                               1)
                         '(d)))

(define SHL (combine-tms (list 0
                               R
                               `(,BRANCH (d (GOTO 20))
                                         (,BLANK (GOTO 10)))
                               10
                               FBL
                               '(GOTO 30)
                               20
                               L
                               swap
                               R
                               '(GOTO 0)
                               30)
                         '(d)))
                                 
                               


;;  PRE: tape = (LM BLANK a BLANK b AND i=1)
;; POST: tape = (LM BLANK a b BLANK) AND i=3 if a=b=0, i=a+b+2 otherwise
#;(define ADD (combine-tms
               (list FBR Wd FBR L WB)
               '(d)))

(define ADD (combine-tms
             (list R
                   `(,BRANCH (,BLANK (GOTO 100))
                             (d (GOTO 200)))
                   100 ;; a=0
                   R
                   R
                   `(,BRANCH (,BLANK (GOTO 500)) ;; a=b=0
                             (d (GOTO 300)))
                   200 ;; a!=0
                   FBR
                   Wd
                   FBR
                   L
                   WB
                   `(,GOTO 1000)
                   300 ;; a=0 and b!=0
                   L
                   SHL
                   FBL
                   SHL
                   `(,GOTO 1000)
                   500 ;; a=0 and b=0
                   L
                   `(,GOTO 1000)
                   1000)
             '(d)))


;; a=0 and b=0
(check-equal? (rest (ctm-run ADD `(,LM ,BLANK) 1))
              `(3 (@ ,BLANK ,BLANK ,BLANK ,BLANK)))
;; a=0 and b!=0
(check-equal? (rest (ctm-run ADD `(,LM ,BLANK ,BLANK ,BLANK d d) 1))
              `(4 (@ ,BLANK d d ,BLANK ,BLANK ,BLANK)))
;; a!=0 and b=0
(check-equal? (rest (ctm-run ADD `(,LM ,BLANK d d d d) 1))
              `(6 (@ ,BLANK d d d d ,BLANK ,BLANK)))
;; a!=0 and b!=0
(check-equal? (rest (ctm-run ADD `(,LM ,BLANK d d d ,BLANK d d) 1))
              `(7 (@ ,BLANK d d d d d ,BLANK ,BLANK)))

(define ADDL  '(list R
                   `(,BRANCH (,BLANK (GOTO 100))
                             (d (GOTO 200)))
                   100 ;; a=0
                   R
                   R
                   `(,BRANCH (,BLANK (GOTO 500)) ;; a=b=0
                             (d (GOTO 300)))
                   200 ;; a!=0
                   FBR
                   Wd
                   FBR
                   L
                   WB
                   `(,GOTO 1000)
                   300 ;; a=0 and b!=0
                   L
                   SHL
                   FBL
                   SHL
                   `(,GOTO 1000)
                   500 ;; a=0 and b=0
                   L
                   `(,GOTO 1000)
                   1000))

;(ctm-viz ADD ADDL `(,LM ,BLANK d d d ,BLANK d d) 1)


(define ADD2 (combine-tms
              (list FBR
                    Wd
                    FBR
                    L
                    WB
                    L
                    (list BRANCH (list BLANK (list GOTO 100))
                                 (list 'd (list GOTO 200)))
                    100
                    R
                    R
                    R
                    (list BRANCH (list BLANK (list GOTO 300))
                                 (list 'd (list GOTO 400)))
                    200
                    R
                    (list GOTO 500)
                    300
                    L
                    (list GOTO 500)
                    400
                    L
                    SHL
                    FBL
                    SHL
                    500)
              '(d)))

(define ADD2L '(list FBR
                    Wd
                    FBR
                    L
                    WB
                    L
                    (list BRANCH (list BLANK (list GOTO 100))
                                 (list 'd (list GOTO 200)))
                    100
                    R
                    R
                    R
                    (list BRANCH (list BLANK (list GOTO 300))
                                 (list 'd (list GOTO 400)))
                    200
                    R
                    (list GOTO 500)
                    300
                    L
                    (list GOTO 500)
                    400
                    L
                    SHL
                    FBL
                    SHL
                    500))

;; a=0 and b=0
(check-equal? (rest (ctm-run ADD2 `(,LM ,BLANK) 1))
              `(3 (@ ,BLANK ,BLANK ,BLANK ,BLANK)))
;; a=0 and b!=0
(check-equal? (rest (ctm-run ADD2 `(,LM ,BLANK ,BLANK ,BLANK d d) 1))
              `(4 (@ ,BLANK d d ,BLANK ,BLANK ,BLANK)))
;; a!=0 and b=0
(check-equal? (rest (ctm-run ADD2 `(,LM ,BLANK d d d d) 1))
              `(6 (@ ,BLANK d d d d ,BLANK ,BLANK)))
;; a!=0 and b!=0
(check-equal? (rest (ctm-run ADD2 `(,LM ,BLANK d d d ,BLANK d d) 1))
              `(7 (@ ,BLANK d d d d d ,BLANK ,BLANK)))


;(ctm-viz ADD2 ADD2L `(,LM ,BLANK d d d ,BLANK d d) 1)
;(ctm-viz ADD2 ADD2L `(,LM ,BLANK ,BLANK ,BLANK d d d) 1)
;(ctm-viz ADD2 ADD2L `(,LM ,BLANK ,BLANK ,BLANK) 1)
;(ctm-viz ADD2 ADD2L `(,LM ,BLANK d d d ,BLANK) 1)