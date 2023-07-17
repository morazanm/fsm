(module contracts racket
  (require
    "formatting.rkt"
    "helpers.rkt"
    "predicates.rkt"
    )
  (provide
   correct-members/c
   no-duplicates/c
   listof-rules/c
   listof-words/c
   start-in-states/c
   valid-finals/c
   valid-listof/c
   valid-start/c
   )

  (define (correct-members/c pred pred2 states sigma)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (pred states sigma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (pred2 states sigma rules)
                       (format "The following rules contain symbols not contained in the states/sigma ")
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
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (return-duplicates vals)
                       (format "The following values are duplicated in your ~a" type)
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
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (invalid-rules pred rules)
                       (format "Improperly formatted list of rules")
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
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       words
                       (format "Not given an accurate list of words")
                     
                       )
                      )
                    )
     )
    )

  (define (valid-finals/c states)
    (make-flat-contract
     #:name 'valid-list-of-finals
     #:first-order (valid-finals? states)
     #:projection (lambda (blame)
                    (lambda (finals)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (invalid-finals states finals)
                       (format "Your list of states: ~a does not contain the final states" states)
                     
                       )
                      )
                    )
     )
    )

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

  (define (valid-start/c states)
    (make-flat-contract
     #:name 'valid-starting-state
     #:first-order (valid-start? states)
     #:projection (lambda (blame)
                    (lambda (start)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       start
                       "The starting state is not a valid state"
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
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       start
                       (format "The following starting state ~a, your list of states" start)
                       )
                      )
                    )
     )
    )
  
  )