#lang racket/base

(require "main.rkt")

(define M (make-dfa `(C S F ,DEAD)
                    '(a b)
                    'S
                    '(F)
                    `((S a F)
                      (S b ,DEAD)
                      (F a ,DEAD)
                      (F b F)
                      (C a ,DEAD)
                      (,DEAD a ,DEAD)
                      (,DEAD b ,DEAD))
                    'no-dead))

(define EQABC (make-mttm '(S C G Y)
                         '(a b c)
                         'S
                         '(Y)
                         (list ;; read all blanks and move all R
                          (list (list 'S (list BLANK BLANK BLANK BLANK))
                                (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
                          ;; read a on t0, copy to t1 and then move R on t0 and t1
                          (list (list 'C (list 'a BLANK BLANK BLANK))
                                (list 'C (list 'a 'a BLANK BLANK)))
                          (list (list 'C (list 'a 'a BLANK BLANK))
                                (list 'C (list RIGHT RIGHT BLANK BLANK)))
                          (list (list 'C (list 'b BLANK BLANK BLANK))
                                (list 'C (list 'b BLANK 'b BLANK)))
                          (list (list 'C (list 'b BLANK 'b BLANK))
                                (list 'C (list RIGHT BLANK RIGHT BLANK)))
                          (list (list 'C (list 'c BLANK BLANK BLANK))
                                (list 'C (list 'c BLANK BLANK 'c)))
                          (list (list 'C (list 'c BLANK BLANK 'c))
                                (list 'C (list RIGHT BLANK BLANK RIGHT)))
                          ;; read BLANK on t0, move L on t1, t2 and t3
                          (list (list 'C (list BLANK BLANK BLANK BLANK))
                                (list 'G (list BLANK LEFT LEFT LEFT)))
                          ;; read BLANK on all tapes, move to Y
                          (list (list 'G (list BLANK BLANK BLANK BLANK))
                                (list 'Y (list BLANK BLANK BLANK BLANK)))
                          ;; read a, b, c on t1, t2, and t3 them move L on t1, t2, t3
                          (list (list 'G (list BLANK 'a 'b 'c))
                                (list 'G (list BLANK LEFT LEFT LEFT))))
                         4
                         'Y))

(define wcw^r (make-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP) (P ,EMP))
                            ((P a ,EMP) (P (a)))
                            ((P b ,EMP) (P (b)))
                            ((P c ,EMP) (Q ,EMP))
                            ((Q a (a)) (Q ,EMP))
                            ((Q b (b)) (Q ,EMP))
                            ((Q ,EMP ,EMP) (F ,EMP)))))
