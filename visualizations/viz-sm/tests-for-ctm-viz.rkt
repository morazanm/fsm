#lang racket
(require "../../fsm-core/interface.rkt"
         rackunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move-right machine

;; PRE:  tape = (LM w) AND i=k≥0, where w in {a b BLANK}*
;; POST: tape = (LM w) AND i=k+1
(define R (make-tm '(S F)
                   '(a b d)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S d) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

;.................................................
;; Move-left machine 

;; PRE:  tape = (LM w) AND i=k≥1, where w in {a b BLANK}*
;; POST: tape = (LM w) AND i=k-1
(define L (make-tm '(S H)
                   '(a b d)
                   `(((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
                     ((S d) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))

;.................................................
;; Halt-machine

;; PRE:  tape = (LM w), where w in {a b BLANK}*
;; POST: tape = (LM w)
(define HALT (make-tm '(S)
                      '(a b)
                      '()
                      'S
                      '(S)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=a
(define Wa (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H a))
                      ((S b) (H a))
                      ((S d) (H a))
                      ((S ,BLANK) (H a)))
                    'S
                    '(H)))

;; Tests for Wa
(check-equal? (last (sm-showtransitions Wa `(,LM b) 1))
              `(H 1 (,LM a)))
(check-equal? (last (sm-showtransitions Wa `(,LM b a ,BLANK a) 3))
              `(H 3 (,LM b a a a)))

;.................................................
;; Write b

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=b
(define Wb (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H b))
                      ((S b) (H b))
                      ((S d) (H b))
                      ((S ,BLANK) (H b)))
                    'S
                    '(H)))

;; Tests for Wb
(check-equal? (last (sm-showtransitions Wb `(,LM ,BLANK ,BLANK) 2))
              `(H 2 (,LM ,BLANK b)))
(check-equal? (last (sm-showtransitions Wb `(,LM b b b b) 3))
              `(H 3 (,LM b b b b)))

;.................................................
;; Write BLANK

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=BLANK
(define WB (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H ,BLANK))
                      ((S b) (H ,BLANK))
                      ((S d) (H ,BLANK))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))

;; Tests for WB
(check-equal? (last (sm-showtransitions WB `(,LM a a) 1))
              `(H 1 (,LM ,BLANK a)))
(check-equal? (last (sm-showtransitions WB `(,LM a b b b) 3))
              `(H 3 (,LM a b ,BLANK b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move right twice

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*
(define RR (combine-tms (list R R) '(a b d)))

;; Tests for RR
(check-equal? (ctm-run RR `(,LM b a a) 1)
              `(F 3 (,LM b a a)))
(check-equal? (ctm-run RR `(,LM a a a) 2)
              `(F 4 (,LM a a a ,BLANK)))
(check-equal? (ctm-run RR `(,LM a b b a) 3)
              `(F 5 (,LM a b b a ,BLANK)))
(check-equal? (ctm-run RR `(,LM b) 1)
              `(F 3 (,LM b ,BLANK ,BLANK)))

;.................................................
;; Find right BLANK 

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i>k AND tape[i] = BLANK AND tape[k+1..i-1] /= BLANK
(define FBR (combine-tms
             (list 0
                   R
                   (cons BRANCH
                         (list (list 'a (list GOTO 0))
                               (list 'b (list GOTO 0))
                               (list 'd (list GOTO 0))
                               (list BLANK (list GOTO 10))))
                   10)
             (list 'a 'b 'd)))

;; Tests for FBR
(check-equal? (ctm-run FBR `(,LM ,BLANK) 1)
              `(F 2 (,LM ,BLANK ,BLANK)))
(check-equal? (ctm-run FBR `(,LM a a b b b ,BLANK a a) 2)
              `(F 6 (,LM a a b b b ,BLANK a a)))
(check-equal? (ctm-run FBR `(,LM ,BLANK ,BLANK b b) 3)
              `(F 5 (,LM ,BLANK ,BLANK b b ,BLANK)))

;.................................................
;; Find left BLANK

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)∗
;;              AND there exists j<i such that tape[j]=BLANK
;; POST: tape = (LM w) AND i<k AND tape[i]=BLANK AND tape[i+1..|w|] != BLANK
(define FBL (combine-tms
             (list 0
                   L
                   (cons BRANCH
                         (list (list 'a (list GOTO 0))
                               (list 'b (list GOTO 0))
                               (list 'd (list GOTO 0))
                               (list BLANK (list GOTO 1))
                               (list LM (list GOTO 0))))
                   1)
             (list 'a 'b 'd)))

;; Tests for FBL
(check-equal? (ctm-run FBL `(,LM ,BLANK a a b) 4)
              `(H 1 (,LM ,BLANK a a b)))
(check-equal? (ctm-run FBL `(,LM a ,BLANK a b ,BLANK b a b b) 8)
              `(H 5 (,LM a ,BLANK a b ,BLANK b a b b)))

;.................................................

;; PRE:  tape = (LM BLANK w BLANK) and head on blank after w
;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w 
#;(define COPY (combine-tms
              (list FBL
                    0
                    R
                    (cons BRANCH (list (list BLANK (list GOTO 2))
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))))
                    1
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))))
                    3
                    RR
                    (list GOTO 5)
                    4
                    R
                    (list GOTO 5)
                    5)
              '(a b)))

(define COPY (combine-tms
              (list FBL
                    0
                    R
                    (cons BRANCH (list (list BLANK (list GOTO 2))
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))
                                       (list 'd (list GOTO 1))))
                    1
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))
                                       (list 'd (list GOTO 4))))
                    3
                    RR
                    (list GOTO 5)
                    4
                    R
                    (list GOTO 5)
                    5)
              '(a b d)))

;; Sample ctm as list
(define COPYL
  '(FBL
    0
    R
    (cons BRANCH (list (list _ (list GOTO 2))
                       (list a (list GOTO 1))
                       (list b (list GOTO 1))
                       (list d (list GOTO 1))))
    1
    (list (list VAR 'k)
          WB
          FBR
          FBR
          k
          FBL
          FBL
          k
          (list GOTO 0))
    2
    FBR
    L
    (cons BRANCH (list (list _ (list GOTO 3))
                       (list a (list GOTO 4))
                       (list b (list GOTO 4))
                       (list d (list GOTO 4))))
    3
    RR
    (list GOTO 5)
    4
    R
    (list GOTO 5)
    5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and implement a ctm that shifts its input word one space to the right.

;; Name: w->_w
;; Σ = {a b}

;; PRE:  tape = (@ w) and i = 1
;; POST: tape = (@ _ w) and i = |w|+2
(define shiftr (combine-tms
               (list FBR
                     0
                     L
                     (cons BRANCH (list (list 'a (list GOTO 1))
                                        (list 'b (list GOTO 1))
                                        (list 'd (list GOTO 1))
                                        (list '@ (list GOTO 3))
                                        (list '_ (list GOTO 3))))
                     1
                     (list (list VAR 'x)
                           R
                           'x
                           L
                           WB
                           (list GOTO 0))
                     3
                     FBR)
               '(a b d)))

;.................................................
#|             
;; Tests for w->_w
(check-equal? (rest (ctm-run w->_w '(@ a b a) 1))
              '(5 (@ _ a b a _)))
(check-equal? (rest (ctm-run w->_w '(@ _ _) 1))
              '(2 (@ _ _)))
(check-equal? (rest (ctm-run w->_w '(@ a b b a) 1))
              '(6 (@ _ a b b a _)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and implement a ctm that shifts its input word one space to the left.

;; Name: _w->w
;; Σ = {a b}

;; PRE:  tape = (@ _ w) and i = 1
;; POST: tape = (@ w _ _) and i = |w|+2
(define shiftl (combine-tms
               (list 0
                     R
                     (cons BRANCH (list (list 'a (list GOTO 1))
                                        (list 'b (list GOTO 1))
                                        (list 'd (list GOTO 1))
                                        (list '_ (list GOTO 3))))
                     1
                     (list (list VAR 'x)
                           L
                           'x
                           R
                           WB
                           (list GOTO 0))
                     3)
               '(a b d)))

(define shiftr-list '(FBR
                      0
                      L
                      (cons BRANCH (list (list a (list GOTO 1))
                                         (list b (list GOTO 1))
                                         (list d (list GOTO 1))
                                         (list @ (list GOTO 3))
                                         (list _ (list GOTO 3))))
                      1
                      (list (list VAR 'x)
                            R
                            x
                            L
                            WB
                            (list GOTO 0))
                      3
                      FBR))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctm to compute mult(a b) = a * b

;; A given number is represented in nr of ds. A d can be part of the first number or of the second number of the equation.
;; In our proof we distinguish between both as follows:
;; 1. ad = a d that is in the first number of the equation 
;; 2. bd = a d that is in the second number of the equation

;; PRE:  tape = '(@ _ ad* _ bd*) and i = 1
;; POST: tape = '(@ _ (_^|ad*|) _ _ (ad* * bd*) _) and i = |ad*| + |ad* * bd*| + 4
(define MULT (combine-tms
              (list R
                    (cons BRANCH (list (list 'd (list GOTO 7))
                                       (list '_ (list GOTO 8))))
                    7
                    WB
                    0
                    R
                    (cons BRANCH (list (list 'd (list GOTO 1))
                                       (list '_ (list GOTO 2))))
                    1
                    WB
                    FBR
                    FBR
                    COPY
                    FBL
                    FBL
                    FBL
                    (list GOTO 0)
                    2
                    FBR
                    (list GOTO 4)
                    8
                    R
                    (list GOTO 3)
                    9
                    WB
                    R
                    3
                    (cons BRANCH (list (list '_ (list GOTO 4))
                                       (list 'd (list GOTO 9))))
                    4
                    FBL
                    shiftr
                    FBR)           
              '(d)))

(define MULTL '(list R
                    (cons BRANCH (list (list 'd (list GOTO 7))
                                       (list '_ (list GOTO 8))))
                    7
                    WB
                    0
                    R
                    (cons BRANCH (list (list 'd (list GOTO 1))
                                       (list '_ (list GOTO 2))))
                    1
                    WB
                    FBR
                    FBR
                    COPY
                    FBL
                    FBL
                    FBL
                    (list GOTO 0)
                    2
                    FBR
                    (list GOTO 4)
                    8
                    R
                    (list GOTO 3)
                    9
                    WB
                    R
                    3
                    (cons BRANCH (list (list '_ (list GOTO 4))
                                       (list 'd (list GOTO 9))))
                    4
                    FBL
                    shiftr
                    FBR))
