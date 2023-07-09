(module predicates racket
  (require racket/contract
           "../constants.rkt")
  (provide functional?
           valid-dfa-rules?
           valid-finals?
           invalid-finals
           valid-start?
           valid-sigma?
           valid-list-of-states?
           valid-state?
           valid-alpha?
           valid-rule?
           return-duplicates)

  (define (return-duplicates los)
    (cond [(empty? los) '()]
          [(member (car los) (cdr los)) (cons (car los) (return-duplicates (cdr los)))]
          [else (return-duplicates (cdr los))]))
  
  ;applies the predicate to the list and returns false if
  ; any of the members of the list are invalid states/sigma
  (define (valid-list-of los pred)
    (andmap pred los)
    )

  (define (valid-state? x)
    (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
    (or (equal? x DEAD)
        (if (symbol? x)
            (regexp-match regex-pattern (symbol->string x))
            #f)
        )
    )

  (define (valid-list-of-states? states)
    (valid-list-of states valid-state?)
    )

  (define (valid-alpha? x)
    (define regex-pattern #px"[a-z]")
    (if (symbol? x)
        (regexp-match regex-pattern (symbol->string x))
        #f)
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


  ;; Determines if the set of rules is a function, based on the machine states
  ;; and alphabet (sigma). The machine must either automatically add dead states
  ;; (add-dead is true), or there must exist a rule with a starting state and
  ;; alphabet letter for every pair of state and letters in the alphabet sigma.
  (define (functional? rules states sigma add-dead)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (or add-dead (andmap (lambda (x) (member x pairs)) cart-prod))
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
  
  (define (valid-dfa-rules? states sigma add-dead)
    (lambda (rules)
      (and/c (andmap (lambda (x) (valid-rule? x states sigma)) rules)
             (functional? rules states sigma add-dead))
      )
    )
  
  )