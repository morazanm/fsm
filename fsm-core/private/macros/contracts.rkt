#lang racket
(require racket/contract)
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

(define (valid-rule? x states sigma)
  (and (list? x)
       (= (length x) 3)
       (member (first x) states)
       (member (second x) sigma)
       (member (third x) states))
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

(define (valid-final? states)
  (lambda (finals)
    (andmap (lambda (x) (member x states)) finals)
    )
  )

(define (valid-rules? states sigma)
  (lambda (rules)
    (and (andmap (lambda (x) (valid-rule? x states sigma)) rules)
         (functional? rules states sigma))
    )
  )

(define (functional? rules states sigma)
  (define pairs (map (lambda (x) (list (first x) (second x))) rules))
  (define cart-prod (cartesian-product states sigma))
  (andmap (lambda (x) (member x pairs)) cart-prod)
  )

(define/contract (make-dfa states sigma start finals rules)
  (->i ([states (and/c list?
                       valid-list-of-states?)]
        [sigma (and/c list?
                      valid-sigma?)]
        [start (states) (and/c symbol?
                               (valid-start? states))]
        [finals (states) (and/c list?
                                (valid-final? states))]
        [rules (states
                sigma) (and/c list?
                              (valid-rules? states sigma))]
        )
       [result list?])
  (list states sigma start finals rules)
  )

(make-dfa '(A B C) '(a) 'A '(B C) (list '(A a C)
                                        '(B a B)
                                        '(C a A))) 