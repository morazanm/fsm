(module flat-contracts racket
  (require "predicates.rkt"
           "error-formatting.rkt"
           "../constants.rkt"
           "../fsa.rkt"
           racket/contract
           )
  (provide functional/c
           no-duplicates/c
           valid-finals/c
           valid-start/c
           start-in-states/c
           listof-rules/c
           dfa-input/c
           listof-words/c
           ndfa-input/c
           tm-input/c
           no-duplicates-dfa/c
           correct-members/c)

  (define (valid-start/c states)
    (make-flat-contract
     #:name 'valid-starting-state
     #:first-order (valid-start? states)
     #:projection (lambda (blame)
                    (lambda (start)
                      (current-blame-format format-start-error)
                      (raise-blame-error
                       blame
                       start
                       (format "The starting state ~s is not a valid state" start)
                       )
                      )
                    )
     )
    )

  (define (start-in-states/c states)
    (make-flat-contract
     #:name 'starting-state-in-state-list
     #:first-order (start-in-states? states)
     #:projection (lambda (blame)
                    (lambda (start)
                      (current-blame-format format-start-error)
                      (raise-blame-error
                       blame
                       start
                       (format "The following starting state is not in ~a, your list of states: ~a" states start)
                       )
                      )
                    )
     )
    )

  ;; Defines a simple contract to check that there are no duplicates in a list
  ;; Parameter type refers to the type of elements in the list, e.g. number, state, symbol, etc.
  (define (no-duplicates/c type)
    (make-flat-contract
     #:name (string->symbol (format "distinct-list-of-~a" type))
     #:first-order (lambda (vals) (not (check-duplicates vals)))
     #:projection (lambda (blame)
                    (lambda (vals)
                      (current-blame-format format-duplicates-error)
                      (raise-blame-error
                       blame
                       (return-duplicates vals)
                       (format "There following values are duplicated in your ~a: " type )
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
                      (current-blame-format format-duplicates-error)
                      (raise-blame-error
                       blame
                       (check-duplicates-dfa vals)
                       (format "There following state/sigma pairs are duplicated in your ~a: " type)
                       )
                      )
                    )
     )
    )


  (define (listof-rules/c pred)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (valid-rules? pred rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-rule-format-error)
                      (raise-blame-error
                       blame
                       (invalid-rules pred rules)
                       (format "Improperly formatted list of rules")
                       )
                      )
                    )
     )
    )

  (define (correct-members/c pred pred2 states sigma)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (pred states sigma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-rule-error)
                      (raise-blame-error
                       blame
                       (pred2 states sigma rules)
                       (format "The following rules contain symbols not contained in the states/sigma: ")
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
                      (current-blame-format format-missing-rule-error)
                      (raise-blame-error
                       blame
                       (missing-functional rules states sigma)
                       (format "You must include rules for these state/alphabet letter pairings: ")
                       )
                      )
                    )
     )
    )

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

  (define (listof-words/c sigma)
    (make-flat-contract
     #:name 'valid-list-of-words
     #:first-order (lambda (words) (listof-words? words sigma))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       words
                       (format "Not given an accurate list of words ~s" words)
                     
                       )
                      )
                    )
     )
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
                      (current-blame-format format-accepts-error)
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
                       (format "Does not ~s the predicted value: " accepts?)
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
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       (return-input-ndfa states
                                            sigma
                                            start
                                            finals
                                            rules
                                            words
                                            accepts?)
                       (format "Does not ~s the predicted value: " accepts?)
                       )
                      )
                    )
     )
    )

  (define (tm-input/c states
                         sigma
                         start
                         finals
                         rules
                         accept
                         accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-tm states
                                       sigma
                                       start
                                       finals
                                       rules
                                       accept
                                       accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       (return-input-tm states
                                            sigma
                                            start
                                            finals
                                            rules
                                            words
                                            accept
                                            accepts?)
                       (format "Does not ~s the predicted value: " accepts?)
                       )
                      )
                    )
     )
    )
  )