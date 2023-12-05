#lang racket

(require "../../../main.rkt" "transdiagram-ctm.rkt" "viz-ctm.rkt")

(define RGHT (make-tm '(S F)
                      '(i)
                      `(((S ,BLANK) (F ,RIGHT))
                        ((S i) (F ,RIGHT)))
                      'S
                      '(F)))

(define LFT (make-tm '(S F)
                     '(i)
                     `(((S ,BLANK) (F ,LEFT))
                       ((S i) (F ,LEFT)))
                     'S
                     '(F)))

(define FBL2 (make-tm '(S F A)
                      '(i)
                      `(((S ,BLANK) (A ,LEFT))
                        ((A i) (A ,LEFT))
                        ((A ,BLANK) (F ,BLANK)))
                      'S
                      '(F)))

(define SHIFTL (make-tm '(S F A B C)
                        '(i)
                        `(((S ,BLANK) (A ,RIGHT))
                          ((A i) (B ,BLANK))
                          ((A ,BLANK) (F ,LEFT))
                          ((B ,BLANK) (C ,LEFT))
                          ((C ,BLANK) (C i))
                          ((C i) (S ,RIGHT)))
                        'S
                        '(F)))

(define ADDL '(list FBL2
                    SHIFTL
                    LFT
                    (cons BRANCH (list (list BLANK (list GOTO 20))
                                       (list 'i   (list GOTO 5))))
                    5
                    RGHT
                    20))
;;  PRE: tape = (LM BLANK a BLANK b BLANK) and head on first blank after b
;; POST: tape = (LM BLANK a+b BLANK) and head on first blank after a+b
(define ADD (combine-tms (list FBL2
                               SHIFTL
                               LFT
                               (cons BRANCH (list (list BLANK (list GOTO 20))
                                                  (list 'i   (list GOTO 5))))
                               5
                               RGHT
                               20)
                         '(i)))

;;; SWAP MACHINE

;;  PRE: tape = (LMw) AND i=k>0 AND w in (a b)*
;; POST: tape = (LMw) AND i=k+1 AND w in (a b)*
(define RS (make-tm '(S F)
                    '(a b)
                    `(((S a) (F ,RIGHT))
                      ((S b) (F ,RIGHT))
                      ((S ,BLANK) (F ,RIGHT)))
                    'S
                    '(F)))

(check-equal? (sm-showtransitions RS `(,LM a b a) 1)
              `((S 1 (,LM a b a))
                (F 2 (,LM a b a))))
(check-equal? (sm-showtransitions RS `(,LM a b a) 3)
              `((S 3 (,LM a b a))
                (F 4 (,LM a b a ,BLANK))))
(check-equal? (second (last (sm-showtransitions RS `(,LM b b a a) 3)))
              4)

;;  PRE: tape = (LMw) AND i=k>0, where w in (a b)*
;; POST: tape = (LMw) AND i=k+1
(define LS (make-tm '(S H)
                    `(a b)
                    `(((S a) (H ,LEFT))
                      ((S b) (H ,LEFT))
                      ((S ,BLANK) (H ,LEFT)))
                    'S
                    '(H)))

(check-equal? (last (sm-showtransitions LS `(,LM a a) 1))
              `(H 0 (,LM a a)))
(check-equal? (last (sm-showtransitions LS `(,LM ,BLANK b b a) 4))
              `(H 3 (,LM ,BLANK b b a)))

(define swap (combine-tms (list (list (list VAR 'i)
                                      RS
                                      (list (list VAR 'j)
                                            'i
                                            LS
                                            'j)))
                          '(a b)))

(define swapL '(((VAR i)
                 RS
                 ((list VAR j)
                  i
                  LS
                  j))))

(check-equal? (rest (ctm-run swap `(,LM ,BLANK a b a) 3))
              `(3 (,LM ,BLANK a a b)))
(check-equal? (rest (ctm-run swap `(,LM ,BLANK a b a a) 5))
              `(5 (@ _ a b a _ a)))

;;; END SWAP MACHINE
