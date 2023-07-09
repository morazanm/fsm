#lang racket
(require "../constants.rkt")
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
  (or (equal? x DEAD)
      (if (symbol? x)
          (regexp-match regex-pattern (symbol->string x))
          #f)
      )
  )

                        

(define (valid-alpha? x)
  (define regex-pattern #px"[a-z]")
  (if (symbol? x)
      (regexp-match regex-pattern (symbol->string x))
      #f)
  )

(define (valid-rule? states sigma)
  (lambda (rule)
    (and (list? rule)
         (= (length rule) 3)
         (member (first rule) states)
         (member (second rule) sigma)
         (member (third rule) states))
    )
  )

;applies the predicate to the list and returns false if
; any of the members of the list are invalid states/sigma
(define (valid-list-of los pred)
  (andmap pred los)
  )

(define (valid-list-of-states? states)
  (valid-list-of states valid-state?)
  )

;; Defines a simple contract to check that there are no duplicates in a list
;; Parameter type refers to the type of elements in the list, e.g. number, state, symbol, etc.
(define (no-duplicates/c type)
  (make-flat-contract
   #:name (string->symbol (format "distinct-list-of-~a" type))
   #:first-order (lambda (vals) (not (check-duplicates vals)))
   )
  )

(define (valid-sigma? sigma)
  (valid-list-of sigma valid-alpha?)
  )

(define/contract (valid-start? states)
  (-> (listof valid-state?) (-> valid-state? (or/c #f list? any/c)))
  (lambda (start)
    (member start states)
    )
  )

(define (valid-finals? states)
  (lambda (finals)
    (andmap (lambda (x) (member x states)) finals)
    )
  )
(define (invalid-finals states finals)
  (filter (lambda (x) (not (member x states)))finals))

(define (format-finals-error blame value message)
  (format "The following final states are not in ~a, your list of states: ~s" message value))

;; Testing out writing a custom blame projector for valid-finals.
(define (valid-finals/c states)
  (make-flat-contract
   #:name 'valid-list-of-finals
   #:first-order (valid-finals? states)
   #:projection (lambda (blame)
                  (lambda (finals)
                    (current-blame-format format-finals-error)
                    (raise-blame-error
                     blame
                     (invalid-finals states finals)
                     (format "~s" states)
                     
                     )
                    )
                  )
   )
  )

(define (valid-rules? states sigma add-dead)
  (lambda (rules)
    (and/c (andmap (lambda (x) (valid-rule? x states sigma)) rules)
           (functional? rules states sigma add-dead))
    )
  )

;; Determines if the set of rules is a function, based on the machine states
;; and alphabet (sigma). The machine must either automatically add dead states
;; (add-dead is true), or there must exist a rule with a starting state and
;; alphabet letter for every pair of state and letters in the alphabet sigma.
(define (functional? rules states sigma add-dead)
  (define pairs (map (lambda (x) (list (first x) (second x))) rules))
  (define cart-prod (cartesian-product states sigma))
  (or add-dead (andmap (lambda (x) (member x pairs)) cart-prod))
  )


(define (functional/c states sigma add-dead)
  (make-flat-contract
   #:name 'functional-list-of-rules?
   #:first-order (lambda (rules) (functional? rules states sigma add-dead))
   )
  )

;; Using the existing set of rules, the entire machine set of states, and the
;; machine alphabet, generates a list of rules that contains the original rule
;; set, plus any additional transitions from states to the DEAD state to make
;; the rule set a function.
(define (add-dead-state-rules rules states sigma)
  (define all-state-sigma-pairs (cartesian-product states sigma))
  (define existing-state-sigma-pairs
    (map (lambda (rule) (list (first rule) (second rule))) rules))
  (define missing-state-sigma-pairs
    (filter
     (lambda (pair) (not (member pair existing-state-sigma-pairs)))
     all-state-sigma-pairs)
    )
  (define dead-state-rules
    (map
     (lambda (pair) (append pair (list DEAD)))
     missing-state-sigma-pairs)
    )
  (append rules dead-state-rules))

;; make-dfa: states alphabet state states rules (boolean) -> machine
;; Purpose: Eventually, will construct a multi-tape turing-machine from the given
;; DFA inputs, but for now just parses inputs and constructs a list.

;; Open questions:
;; 1. If add-dead boolean flag is true, do we need to disallow DEAD state from states.
;; 2. This code does not yet check for duplicates in the list fields - this must
;; be added.
(define/contract (make-dfa states sigma start finals rules #:add-dead [add-dead #t])
  (->i ([states (and/c (listof valid-state?)
                       (no-duplicates/c "states"))]
        [sigma (and/c (listof valid-alpha?)
                      (no-duplicates/c "letters"))]
        [start (states) (and/c symbol?
                               (valid-start? states))]
        [finals (states) (and/c (listof valid-state?)
                                (valid-finals/c states)
                                (no-duplicates/c "states"))]
        [rules (states
                sigma
                add-dead) (and/c (listof (valid-rule? states sigma))
                                 (functional/c states sigma add-dead)
                                 (no-duplicates/c "rules"))]
        )
       (#:add-dead [add-dead boolean?])
       [result list?])
  (define all-rules
    (if add-dead
        (add-dead-state-rules rules states sigma)
        rules))
  (list states sigma start finals all-rules add-dead)
  )

(make-dfa
 `(A B C)
 '(a b)
 'A
 '(B C D)
 (list '(A a C)
       '(B a B)
       '(C a A)
       )
 ) 