#lang fsm
(require "transdiagram-ctm.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move-right machine

;; PRE:  tape = (LM w) AND i=k≥0, where w in {a b BLANK}*
;; POST: tape = (LM w) AND i=k+1
(define R (make-tm '(S F)
                   '(a b)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composed Turing machine (ctm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w') AND i=k AND tape[i]=BLANK
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
;; copy(w) = w BLANK w

;; A ctmd is a list defined as follows:
;; 1. empty list
;; 2. (cons m ctmd), where m is either a tm or a ctmd
;; 3. (cons LABEL ctmd)
;; 4. (cons (list GOTO LABEL) ctmd)
;; 5. (cons (BRANCH (list (list symbol (list GOTO LABEL)))) ctmd)
;; 6. (cons ((VAR symbol) ctmd) ctmd)
;; 7. (cons variable ctmd)

;; PRE:  tape = (LM BLANK w BLANK) and head on blank after w
;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w 
(define COPY (combine-tms
              (list FBL
                    0 ;; Traverse the input word until the BLANK after the word is reached
                    R
                    (cons BRANCH (list (list BLANK (list GOTO 2))
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))))
                    1 ;; Copy element 
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2 ;; Adjust head position
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))))
                    3 ;; Move head twice if copying BLANK
                    RR
                    (list GOTO 5)
                    4 ;; Move head once if copying a or b
                    R
                    (list GOTO 5)
                    5)
              '(a b)))

;; Tests for COPY
(check-equal? (rest (ctm-run COPY `(,LM ,BLANK ,BLANK ,BLANK) 3))
              `(5 (,LM ,BLANK ,BLANK ,BLANK ,BLANK ,BLANK)))
(check-equal? (rest (ctm-run COPY `(,LM ,BLANK a b b ,BLANK) 5))
              `(9 (,LM ,BLANK a b b ,BLANK a b b ,BLANK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define COPYL
  '(FBL
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
    5))

;(transition-diagram-ctm COPYL)


