(module contracts racket
  (require
    racket/contract
    "helpers.rkt"
    "predicates.rkt"
    "../predicates.rkt"
    "../formatting.rkt" 
    )
  (provide
   dfa-input/c
   functional/c
   ndfa-input/c
   no-duplicates-dfa/c
   )

  (define (dfa-input/c states
                       sigma
                       start
                       finals
                       rules
                       add-dead
                       accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-dfa states
                                    sigma
                                    start
                                    finals
                                    rules
                                    add-dead
                                    accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (return-input-dfa states
                                         sigma
                                         start
                                         finals
                                         rules
                                         add-dead
                                         words
                                         accepts?)
                       (format "Does not ~a the predicted value" accepts?)
                       )
                      )
                    )
     )
    )

  (define (functional/c states sigma add-dead)
    (make-flat-contract
     #:name 'functional-list-of-rules?
     #:first-order (lambda (rules) (functional? rules states sigma add-dead))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (missing-functional rules states sigma)
                       (format "You must include rules for these state/alphabet letter pairings: ")
                       )
                      )
                    )
     )
    )

  (define (ndfa-input/c states
                        sigma
                        start
                        finals
                        rules
                        accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-ndfa states
                                     sigma
                                     start
                                     finals
                                     rules
                                     accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (return-input-ndfa states
                                          sigma
                                          start
                                          finals
                                          rules
                                          words
                                          accepts?)
                       (format "Does not ~a the predicted value" accepts?)
                       )
                      )
                    )
     )
    )

  ;; Defines a simple contract to check that there are no duplicates in a list
  ;; Parameter type refers to the type of elements in the list, e.g. number, state, symbol, etc.
  (define (no-duplicates-dfa/c type)
    (make-flat-contract
     #:name (string->symbol (format "distinct-list-of-~a" type))
     #:first-order (lambda (vals) (not (check-duplicates-dfa vals)))
     #:projection (lambda (blame)
                    (lambda (vals)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (check-duplicates-dfa vals)
                       (format "There following state/sigma pairs are duplicated in your ~a" type)
                       )
                      )
                    )
     )
    )
  )