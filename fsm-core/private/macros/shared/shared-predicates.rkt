(module shared-predicates racket
  (require racket/contract
           rackunit
           "../../constants.rkt"
           "../../sm-getters.rkt"
           "../../fsa.rkt"
           "../../tm.rkt"
           "../../pda.rkt"
           "../../../../main.rkt")
  (provide dfa?
           ndfa?
           ndpda?
           tm?
           valid-list-of
           valid-state?
           valid-alpha?
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

  ;valid-list-of: (listof something) (something --> boolean) --> boolean
  ;purpose: takes in a list of anything, passes all to the predicate function
  ; and returns true if all are true, and false if even one fails
  (define (valid-list-of los pred)
    (andmap pred los)
    )

  ;valid-state?: something --> boolean
  ;purpose: takes in anything and makes sure that it is a symbol that is either
  ; uppercase roman letter
  ; uppercase roman letter, -, natural number
  ; DEAD
  (define (valid-state? x)
    (define regex-pattern (regexp "^[A-Z](?:-[0-9]+)?$"))
    (or (equal? x DEAD)
        (if (symbol? x)
            (if (regexp-match regex-pattern (symbol->string x)) #t #f)
            #f)
        )
    )

  ;valid-alpha? something --> boolean
  ;purpose: takes in anything and makes sure that it is a symbol that
  ; a lowercase roman letter
  (define (valid-alpha? x)
    (define regex-pattern (regexp "^[a-z]$"))
    (if (symbol? x)
        (if (regexp-match regex-pattern (symbol->string x)) #t #f)
        #f)
    )

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
  (define (valid-start? states)
    (lambda (start)
      (and (symbol? start)
           (valid-state? start)))
    )

  ;valid-finals?: (list of states) --> (listof states --> boolean)
  ;purpose: to take in the list of states, and then the final states,
  ; and return true, only if all the final states are included
  ; in the list of states
  (define (valid-finals? states)
    (lambda (finals)
      (if (andmap (lambda (x) (member x states)) finals) #t #f)
      )
    )

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
    (check-equal? (valid-alpha? '1) #f)
    (check-equal? (valid-alpha? 1) #f)
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
  )