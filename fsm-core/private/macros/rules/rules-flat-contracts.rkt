(module rules-flat-contracts racket
  (require "rules-predicates.rkt"
           "../error-formatting.rkt"
           "../../constants.rkt"
           "../../fsa.rkt"
           racket/contract
           )
  (provide listof-rules/c
           correct-members/c
           correct-dfa-rules/c
           correct-members-ndpda/c
           functional/c
           no-duplicates-dfa/c)

  (define (listof-rules/c pred)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (valid-rules? pred rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-rule-format-error)
                      (raise-blame-error
                       blame
                       (map (lambda (x) (format "~n~s" x)) (invalid-rules pred rules))
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
                       (map (lambda (x) (format "~n~s" x)) (pred2 states sigma rules))
                       (format "The following rules contain symbols not contained in the states/sigma: ")
                       )
                      )
                    )
     )
    )

  (define (correct-dfa-rules/c states sigma)
    (make-flat-contract
     #:name 'correct-dfa-rules
     #:first-order (lambda (rules) (correct-members-dfa? states sigma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-incorrect-rules-error)
                      (raise-blame-error
                       blame
                       (incorrect-dfa-rules states sigma rules)
                       "The following rules have errors, which make them invalid")))))

  (define (correct-members-ndpda/c pred pred2 states sigma gamma)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (pred states sigma gamma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-rule-error)
                      (raise-blame-error
                       blame
                       (map (lambda (x) (format "~n~s" x)) (pred2 states sigma gamma rules))
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
                       (map (lambda (x) (format "~n~s" x)) (missing-functional rules states sigma))
                       (format "You must include rules for these state/alphabet letter pairings: ")
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
                       (map (lambda (x) (format "~n~s" x)) (check-duplicates-dfa vals))
                       (format "There following state/sigma pairs are duplicated in your ~a: " type)
                       )
                      )
                    )
     )
    )

  
  )