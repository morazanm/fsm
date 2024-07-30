#lang fsm
(require "transdiagram-ctm6.rkt")
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
;; find first BLANK to the right

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

#;(define COPYTM (make-tm '(S F)
                        '(a b)
                        `(((S a) (F ,RIGHT))
                          ((S b) (F ,RIGHT))
                          ((S ,BLANK) (F ,RIGHT))
                          ((S a) (f ,BLANK))
                          ((S b) (F ,BLANK))
                          ((S ,BLANK) (F ,BLANK))
                          ((S a) (F ,LEFT))
                          ((S b) (F ,LEFT))
                          ((S d) (F ,LEFT))
                          ((S ,BLANK) (F ,LEFT))
                          ((

                          )
                        '(F)))))



;; POST: tape = (LM w) AND i>k AND tape[i] = BLANK AND tape[k+1..i-1] /= BLANK













