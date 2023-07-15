(module predicates racket
  (require racket/contract
           "../constants.rkt"
           "../sm-getters.rkt"
           "../fsa.rkt")
  (provide functional?
           missing-functional
           valid-dfa-rule?
           valid-rules?
           valid-finals?
           invalid-finals
           valid-start?
           start-in-states?
           valid-sigma?
           valid-list-of-states?
           valid-state?
           valid-alpha?
           return-duplicates
           invalid-rules
           dfa?
           check-input-dfa
           return-input-dfa
           listof-words?)

  (define (dfa? machine)
    (equal? (sm-type machine) 'dfa))

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


  ;; Determines if the set of rules is a function, based on the machine states
  ;; and alphabet (sigma). The machine must either automatically add dead states
  ;; (add-dead is true), or there must exist a rule with a starting state and
  ;; alphabet letter for every pair of state and letters in the alphabet sigma.
  (define (functional? rules states sigma add-dead)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (or add-dead (andmap (lambda (x) (member x pairs)) cart-prod))
    )

  (define (missing-functional rules states sigma)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (filter (lambda (x) (not (member x pairs))) cart-prod)
    )

  (define ((valid-dfa-rule? states sigma) rule)
    (and (list? rule)
         (= (length rule) 3)
         (member (first rule) states)
         (member (second rule) sigma)
         (member (third rule) states))
    )

  (define (valid-rules? pred states sigma rules)
    (andmap (lambda (rule) (pred states sigma rule)) rules))

  (define (invalid-rules pred states sigma rules)
    (filter (lambda (rule) (not (pred states sigma rule)))
            rules)
    )

  (define (listof-words? words sigma)
    (and (list? words)
         (andmap (lambda (word) (and (list? word)
                                     (andmap (lambda (letter) (symbol? letter))
                                             word))) words))
    )

  (define (check-input-dfa states
                              sigma
                              start
                              finals
                              rules
                              add-dead
                              accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-dfa states
                                               sigma
                                               start
                                               finals
                                               rules
                                               add-dead))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  (define (return-input-dfa states
                               sigma
                               start
                               finals
                               rules
                               add-dead
                               words
                               accepts?)
    (define temp-machine (make-unchecked-dfa states
                                             sigma
                                             start
                                             finals
                                             rules
                                             add-dead))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )
  )