#lang racket
(require "../../main.rkt")

;; CAN ONLY BE TESTED IF PRECONDITION IS SATISFIED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move-right machine

;; PRE:  tape = (LM w) AND i=k>0
;; POST: tape = (LM w) AND i=k+1
(define R (make-tm '(S F)
                   '(a b)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

;.................................................
;; Move-left machine 

;; PRE:  tape = (LM w) AND i=k≥1, where w in {a b BLANK}*
;; POST: tape = (LM w) AND i=k-1
(define L (make-tm '(S H)
                   '(a b)
                   `(((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
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
                    '(a b)
                    `(((S a) (H a))
                      ((S b) (H a))
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
                    '(a b)
                    `(((S a) (H b))
                      ((S b) (H b))
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
                    '(a b)
                    `(((S a) (H ,BLANK))
                      ((S b) (H ,BLANK))
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
(define RR (combine-tms (list R R) '(a b)))

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
                               (list BLANK (list GOTO 10))))
                   10)
             (list 'a 'b)))

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
                               (list BLANK (list GOTO 1))
                               (list LM (list GOTO 0))))
                   1)
             (list 'a 'b)))

;; Tests for FBL
(check-equal? (ctm-run FBL `(,LM ,BLANK a a b) 4)
              `(H 1 (,LM ,BLANK a a b)))
(check-equal? (ctm-run FBL `(,LM a ,BLANK a b ,BLANK b a b b) 8)
              `(H 5 (,LM a ,BLANK a b ,BLANK b a b b)))

;.................................................

;; PRE:  tape = (LM BLANK w BLANK) and head on blank after w
;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w 
(define COPY (combine-tms
              (list FBL
                    0 ;; Machine checks if there is anything left to copy
                    R
                    (cons BRANCH (list (list BLANK (list GOTO 2))
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))))
                    1 ;; Machine copies either an a or b
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2 ;; Machine checks whether word is empty or whether it has been copied
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))))
                    3 ;; Word is empty, readjust head position accordingly
                    RR
                    (list GOTO 5)
                    4 ;; Word is nonempty and copied, readjust head position accordingly
                    R
                    (list GOTO 5)
                    5 ;; End
                    )
              '(a b)))

;; Tests for COPY
(check-equal? (ctm-run COPY `(,LM ,BLANK a a b ,BLANK) 5)
              `(F 9 (,LM ,BLANK a a b ,BLANK a a b ,BLANK)))
(check-equal? (ctm-run COPY `(,LM ,BLANK a b a b a b ,BLANK) 8)
              `(F 15 (,LM ,BLANK a b a b a b ,BLANK a b a b a b ,BLANK)))

;.................................................

;; Sample ctm as list
(define COPYL
  '(FBL
    0
    R
    (cons BRANCH (list (list _ (list GOTO 2))
                       (list a (list GOTO 1))
                       (list b (list GOTO 1))))
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
                       (list b (list GOTO 4))))
    3
    RR
    (list GOTO 5)
    4
    R
    (list GOTO 5)
    5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
