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

(define (valid-list-of-states? states)
  )
(define (valid-sigma? sigma)
  )
(define (valid-start? start)
  )

(define/contract (make-dfa states sigma start)
  (->i ([states (and/c list?
                       valid-list-of-states?)]
        [sigma (and/c list?
                      valid-sigma?]
        [start (and/c symbol?
                      valid-start?])
       [result list?])
  '(states sigma start)
  )

(f (list 1 2 3) 1)