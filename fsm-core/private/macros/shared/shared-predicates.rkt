#lang racket/base
(require rackunit
         "../../constants.rkt"
         "../../sm-getters.rkt"
         racket/list)
(provide dfa?
         ndfa?
         ndpda?
         tm?
         mttm?
         valid-list-of
         valid-list-of-states?
         valid-non-dead-state?
         valid-state?
         valid-nonterminal?
         valid-alpha?
         valid-tm-alpha?
         valid-gamma?
         start-in-states?
         valid-start?
         valid-finals?
         invalid-finals
         return-duplicates

         )

;dfa?: machine --> boolean
;purpose: return true if the machine is a dfa
(define (dfa? machine)
  (equal? (sm-type machine) 'dfa))

;ndfa?: machine --> boolean
;purpose: return true if the machine is a ndfa
(define (ndfa? machine)
  (equal? (sm-type machine) 'ndfa))

;ndpda?: machine --> boolean
;purpose: return true if the machine is a ndpda
(define (ndpda? machine)
  (equal? (sm-type machine) 'pda))

;tm?: machine --> boolean
;purpose: return true if the machine is a tm or a tm language recognizer
(define (tm? machine)
  (or (equal? (sm-type machine) 'tm-language-recognizer)
      (equal? (sm-type machine) 'tm)))

;mttm? machine --> boolean
;purpose: return true if the machine is a multi-tape TM or a multi-tape TM
; langauge recognizer
(define (mttm? machine)
  (or (equal? (sm-type machine) 'mttm)
      (equal? (sm-type machine) 'mttm-language-recognizer)))

;valid-list-of: (listof something) (something --> boolean) --> boolean
;purpose: takes in a list of anything, passes all to the predicate function
; and returns true if all are true, and false if even one fails
(define (valid-list-of los pred)
  (andmap pred los)
  )

;valid-list-of-states?: (listof something) --> boolean
;purpose: takes in a list of anything, returns true if all elements are valid
; state symbols, and false if any are not.
(define (valid-list-of-states? states)
  (valid-list-of states valid-state?)
  )

;valid-nonterminal: any --> boolean
;purpose: takes in any value, returns true if the value is a valid nonterminal
; symbol, and false otherwise
(define (valid-nonterminal? x)
  (define regex-pattern (regexp "^[A-Z]$"))
  (and (symbol? x)
       (not (not (regexp-match regex-pattern (symbol->string x))))))

;valid-non-dead-state?: any --> boolean
;purpose: takes in any value, returns true if the value is a valid state except
; for DEAD, and false otherwise.
(define (valid-non-dead-state? x)
  (define regex-pattern (regexp "^[A-Z](?:-[0-9]+)?$"))
  (and (symbol? x)
       (not (not (regexp-match regex-pattern (symbol->string x))))
       )
  )

;valid-state?: something --> boolean
;purpose: takes in anything and makes sure that it is a symbol that is either
; uppercase roman letter
; uppercase roman letter, -, natural number
; DEAD
(define (valid-state? x)
  (or (equal? x DEAD)
      (valid-non-dead-state? x))
  )

;valid-alpha? something --> boolean
;purpose: takes in anything and makes sure that it is a symbol that
; a lowercase roman letter
(define (valid-alpha? x)
  (define regex-pattern (regexp "^[a-z0-9$&!*]$"))
  (and (or (symbol? x) (and (number? x) (<= 0 x 9)))
       (not (not (regexp-match regex-pattern
                               (if (symbol? x)
                                   (symbol->string x)
                                   (number->string x)))))))

;valid-tm-alpha? something --> boolean
;purpose: takes in anything and makes sure that it is a symbol that
; a lowercase roman letter
(define (valid-tm-alpha? x)
  (define regex-pattern (regexp "^[A-Za-z0-9$&!*_]$"))
  (and (or (symbol? x) (and (number? x) (<= 0 x 9)))
       (not (not (regexp-match regex-pattern
                               (if (symbol? x)
                                   (symbol->string x)
                                   (number->string x))))))
  )

;valid-gamma?: something --> boolean
;purpose: takes in anything and makes sure that it is either a gamma element,
; that is: it is either an alphabet character or state
(define (valid-gamma? x)
  (and (symbol? x)
       (or (valid-alpha? x) (valid-state? x))))

;start-in-states?: (listof symbols) --> (symbol --> boolean)
;purpose: to take in the list of states, and then the start state,
; and return whether or not that symbol is in that list
; WHY IS IT A LAMBDA?: to allow for the proper order of predicates
;   to be observed by the flat contract, and so the start state
;   can be given to the function properly
(define (start-in-states? states)
  (lambda (start)
    (if (member start states) #t #f)
    )
  )

;start-in-states?: (listof symbols) --> (symbol --> boolean)
;purpose: to take in the list of states, and then the start state,
; and return whether or not that symbol is a valid state
; WHY IS IT A LAMBDA?: to allow for the proper order of predicates
;   to be observed by the flat contract, and so the start state
;   can be given to the function properly
;  This is why it takes in the states even though it doesnt use them
(define ((valid-start? states) start)
  (and (symbol? start)
       (valid-state? start))
  )

;valid-finals?: (list of states) --> (listof states --> boolean)
;purpose: to take in the list of states, and then the final states,
; and return true, only if all the final states are included
; in the list of states
(define ((valid-finals? states) finals)
  (not (not (andmap (lambda (x) (member x states)) finals))))

;invalid-finals: (listof states) (listof states) --> (listof states)
;purpose: take in the states and finals, and return all finals that
; arent included in the list of states
(define (invalid-finals states finals)
  (filter (lambda (x) (not (member x states)))finals))

;return-duplicates: (listof something) --> (listof something)
;purpose: return all the duplicated items in any list of anything
(define (return-duplicates input)
  (define (helper los)
    (cond [(empty? los) '()]
          [(member (car los) (cdr los)) (cons (car los) (return-duplicates (cdr los)))]
          [else (return-duplicates (cdr los))]))
  (remove-duplicates (helper input)))

(module+ test
  ;valid-list-of tests
  (check-equal? (valid-list-of '(A B C D E) symbol?) #t)
  (check-equal? (valid-list-of '(A B C 1 D E) symbol?) #f)

  ;valid-state? tests
  (check-equal? (valid-state? 'A) #t)
  (check-equal? (valid-state? 'A-1) #t)
  (check-equal? (valid-state? 'A-123) #t)
  (check-equal? (valid-state? '1) #f)
  (check-equal? (valid-state? 1) #f)
  (check-equal? (valid-state? 'AB) #f)
  (check-equal? (valid-state? DEAD) #t)
  (check-equal? (valid-state? 'A-1ifodsijfodsijf) #f)
  (check-equal? (valid-state? 'ajfdA) #f)

  ;valid-alpha? tests
  (check-equal? (valid-alpha? 'a) #t)
  (check-equal? (valid-alpha? '1) #t)
  (check-equal? (valid-alpha? 1) #t)
  (check-equal? (valid-alpha? 'A) #f)
  (check-equal? (valid-alpha? 'a1) #f)
  (check-equal? (valid-alpha? 'Aa) #f)
  (check-equal? (valid-alpha? EMP) #f)

  ;start-in-states? tests
  (check-equal? ((start-in-states? '(A B C)) 'A) #t)
  (check-equal? ((start-in-states? '(A B C)) 'D) #f)

  ;valid-start? tests
  (check-equal? ((valid-start? '(A B C)) 'A) #t)
  (check-equal? ((valid-start? '(A B C)) 'A-1) #t)
  (check-equal? ((valid-start? '(A B C)) DEAD) #t)
  (check-equal? ((valid-start? '(A B C)) 1) #f)
  (check-equal? ((valid-start? '(A B C)) 'A1) #f)
  (check-equal? ((valid-start? '(A B C)) '(A)) #f)

  ;valid-finals? tests
  (check-equal? ((valid-finals? '(A B C)) '(A)) #t)
  (check-equal? ((valid-finals? '(A B C)) '(A-1)) #f)
  (check-equal? ((valid-finals? '(A B C)) '(D)) #f)

  ;invalid-finals tests
  (check-equal? (invalid-finals '(A B C) '(B)) '())
  (check-equal? (invalid-finals '(A B C) '(D)) '(D))
  (check-equal? (invalid-finals '(A B C) '(B D)) '(D))
  (check-equal? (invalid-finals '(A B C) '(B C 1 D)) '(1 D))

  ;return-duplicates tests
  (check-equal? (return-duplicates '(A A B C)) '(A))
  (check-equal? (return-duplicates '(A A B B B C)) '(A B))
  (check-equal? (return-duplicates '(A B C)) '())    
  )