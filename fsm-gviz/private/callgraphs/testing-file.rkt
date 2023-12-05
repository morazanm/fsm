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
