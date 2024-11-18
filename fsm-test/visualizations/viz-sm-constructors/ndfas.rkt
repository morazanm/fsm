#lang racket

(require "../../../main.rkt")

(define empty-tran (make-ndfa '(A)
                              '(a b)
                              'A
                              '(A)
                              `((A ,EMP A))))

(define single-tran (make-ndfa '(A)
                               '(a b)
                               'A
                               '(A)
                               `((A a A))))

(define double-loop (make-ndfa '(A)
                               '(a b)
                               'A
                               '(A)
                               `((A a A) (A b A))))

(define empties-weird (make-ndfa '(A B)
                                 '(a)
                                 'A
                                 '(A B)
                                 `((A ,EMP B) (B ,EMP A))))