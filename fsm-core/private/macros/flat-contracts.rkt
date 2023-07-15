(module flat-contracts racket
  (require "predicates.rkt"
           "error-formatting.rkt"
           "../constants.rkt"
           "../fsa.rkt"
           racket/contract
           )
  (provide valid-listof/c
           valid-states/c
           valid-alphabet/c
           functional/c
           no-duplicates/c
           valid-finals/c
           valid-start/c
           start-in-states/c
           listof-rules/c
           dfa-input/c
           listof-words/c)

  ;; Purpose: Constructs a flat contract which checks if all elements in the
  ;;          list hold true for a given predicate. If any elements fail,
  ;;          formats an error message with the list of failing elements,
  ;;          and the given element and field names.
  (define (valid-listof/c predicate element-name field-name)
    (make-flat-contract
     #:name (string->symbol (format "valid-~a" field-name))
     #:first-order (lambda (vals) (andmap predicate vals))
     #:projection (lambda (blame)
                    (lambda (vals)
                      (define invalid-vals (filter (lambda (val) (not (predicate val))) vals))
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       vals
                       (format "The following: ~a are not valid ~a(s), in the ~a"
                               invalid-vals
                               element-name
                               field-name)
                       )
                      )
                    )
     )
    )

  (define valid-states/c
    (make-flat-contract
     #:name 'valid-states
     #:first-order (lambda (states) (andmap valid-state? states))
     #:projection (lambda (blame)
                    (lambda (states)
                      (define invalid-states (filter (lambda (state) (not (valid-state? state))) states))
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       states
                       (format "The following: ~a are not valid machine states, in the set of machine states" invalid-states)
                       )
                      )
                    )
     )
    )

  (define valid-alphabet/c
    (make-flat-contract
     #:name 'valid-alphabet
     #:first-order (lambda (alphabet) (andmap valid-alpha? alphabet))
     #:projection (lambda (blame)
                    (lambda (alphabet)
                      (define invalid-alphas (filter (lambda (alpha) (not (valid-alpha? alpha))) alphabet))
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       alphabet
                       (format "The following: ~a are not valid alphabet letters, in the machine sigma" invalid-alphas)
                       )
                      )
                    )
     )
    )

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
                       (format "The following values are duplicated in your ~a, ~a : " type vals)
                       )
                      )
                    )
     )
    )

  
  (define (listof-rules/c pred states sigma)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (valid-rules? pred states sigma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-rule-error)
                      (raise-blame-error
                       blame
                       (invalid-rules pred states sigma rules)
                       (format "These rules contain improper states/sigma members")
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
  )