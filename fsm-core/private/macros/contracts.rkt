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

(define (valid-state? x)
  (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
  (if (symbol? x)
      (regexp-match regex-pattern (symbol->string x))
      #f)
  )

(define (valid-alpha? x)
  (define regex-pattern #px"[a-z]")
  (if (symbol? x)
      (regexp-match regex-pattern (symbol->string x))
      #f)
  )

;applies the predicate to the list and returns false if
; any of the members of the list are invalid states/sigma
(define (valid-list-of los pred)
  (andmap pred los)
  )

(define (valid-list-of-states? states)
  (valid-list-of states valid-state?)
  )
(define (valid-sigma? sigma)
  (valid-list-of sigma valid-alpha?)
  )
(define (valid-start? states)
  (lambda (start)
    (member start states)
    )
  )

(define/contract (make-dfa states sigma start)
  (->i ([states (and/c list?
                       valid-list-of-states?)]
        [sigma (and/c list?
                      valid-sigma?)]
        [start (states) (and/c symbol?
                               (valid-start? states))])
       [result list?])
  (list states sigma start)
  )

(make-dfa '(A B C) '(a b c) 'D)
