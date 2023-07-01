#lang racket
(define (member-of? lst)
    (lambda (v)
      (if (member v lst)
          #t
          #f)))
(define/contract (f x y)
  (->i ([x list?]
        [y (x) (member-of? x)])
       [result boolean?])  
  (list? x))

(f (list 1 2 3) 1)