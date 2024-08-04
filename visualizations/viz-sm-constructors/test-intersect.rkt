#lang racket
(require "viz-intersection.rkt"
         "../../fsm-core/interface.rkt")


(define ab* (make-ndfa '(S A)
                                 '(a b)
                                 'S
                                 '(A)
                                 '((S a A)
                                   (A b A))))

(define a-aUb-b* (make-ndfa '(Z H B C D F)
                                      '(a b)
                                      'Z
                                      '(F)
                                      `((Z a H)
                                        (Z a B)
                                        (H a D)
                                        (D ,EMP F)
                                        (B a C)
                                        (C b F)
                                        (F b F))))
;(ab* 'whatami)

(intersection-viz ab* a-aUb-b*)