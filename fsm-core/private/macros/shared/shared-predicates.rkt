(module predicates racket
  (require racket/contract
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
           valid-list-of-states?
           valid-state?
           valid-sigma?
           valid-alpha?
           start-in-states?
           valid-start?
           valid-finals?
           invalid-finals
           return-duplicates

           )

  (define (dfa? machine)
    (equal? (sm-type machine) 'dfa))

  (define (ndfa? machine)
    (equal? (sm-type machine) 'ndfa))
  
  (define (ndpda? machine)
    (equal? (sm-type machine) 'pda))

  (define (tm? machine)
    (or (equal? (sm-type machine) 'tm-language-recognizer)
        (equal? (sm-type machine) 'tm)))

  ;applies the predicate to the list and returns false if
  ; any of the members of the list are invalid states/sigma
  (define (valid-list-of los pred)
    (andmap pred los)
    )

  (define (valid-list-of-states? states)
    (valid-list-of states valid-state?)
    )

  (define (valid-state? x)
    (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
    (or (equal? x DEAD)
        (if (symbol? x)
            (regexp-match regex-pattern (symbol->string x))
            #f)
        )
    )

  (define (valid-sigma? sigma)
    (valid-list-of sigma valid-alpha?)
    )
  
  (define (valid-alpha? x)
    (define regex-pattern #px"[a-z]")
    (if (symbol? x)
        (regexp-match regex-pattern (symbol->string x))
        #f)
    )

  (define (start-in-states? states)
    (lambda (start)
      (member start states)
      )
    )

  (define (valid-start? states)
    (lambda (start)
      (and (symbol? start)
           (valid-state? start)))
    )

  (define (valid-finals? states)
    (lambda (finals)
      (andmap (lambda (x) (member x states)) finals)
      )
    )
  (define (invalid-finals states finals)
    (filter (lambda (x) (not (member x states)))finals))

  (define (return-duplicates los)
    (cond [(empty? los) '()]
          [(member (car los) (cdr los)) (cons (car los) (return-duplicates (cdr los)))]
          [else (return-duplicates (cdr los))]))
  
  )