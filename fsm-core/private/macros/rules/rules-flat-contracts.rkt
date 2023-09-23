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
           correct-ndpda-rules/c
           correct-tm-rules/c
           correct-mttm-rules/c
           functional/c
           no-duplicates-dfa/c)

  (define (listof-rules/c pred)
    (make-flat-contract
     #:name 'valid-list-of-rules
     #:first-order (lambda (rules) (valid-rules? pred rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (map (lambda (x) (format "~n~s" x)) (invalid-rules pred rules))
                       (format "Rule four of the design recipe was violated.\nThe following rules are improperly formatted.")
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
                       (format "Rule four of the design recipe was violated.\nThe following rules contain symbols not contained in the states/sigma: ")
                       )
                      )
                    )
     )
    )

  ;correct-dfa-rules/c: (listof state) (listof alpha) --> contract
  ;predicate: (listof x) -> boolean
  ;Purpose: Ensures that every element in the list is a valid dfa rule. It checks
  ; each rule to see if the from-state and to-states are in the list of machine
  ; states, and the consumed letter is in the machine sigma.
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
                       "Rule four of the design recipe was violated.\nThe following rules have errors, which make them invalid")))))

  ;correct-ndpda-rules/c: (listof state) (listof alpha) (listof symbol) --> contract
  ;predicate: (listof x) --> boolean
  ;Purpose: Ensures that every element in the list is a valid pda rule. It checks
  ; each rule to see that the from-state and to-state are in the list of machine
  ; states, that the consumed letter is in the machine sigma, and that the
  ; push and pop elements are either the EMP constant or lists of symbols from
  ; the machine gamma.
  (define (correct-ndpda-rules/c states sigma gamma)
    (make-flat-contract
     #:name 'correct-ndpda-rules
     #:first-order (lambda (rules) (correct-members-ndpda? states sigma gamma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-incorrect-rules-error)
                      (raise-blame-error
                       blame
                       (incorrect-ndpda-rules states sigma gamma rules)
                       "Rule four of the design recipe was violated.\nThe following rules have errors, which make them invalid")))))

  ;correct-tm-rules/c: (listof state) (list of alpha) --> contract
  ;predicate: (listof x) --> boolean
  ;Purpose: Ensures that every element in the list is a valid tm rule. It checks
  ; each rule to see that the from-state and to-state are in the list of machine
  ; states, that the read tape symbol is either in the machine sigma or is the
  ; BLANK or LM constants, and that the tm-action is either in the machine sigma
  ; or is the LEFT or RIGHT constants.
  (define (correct-tm-rules/c states sigma)
    (make-flat-contract
     #:name 'correct-tm-rules
     #:first-order (lambda (rules) (correct-members-tm? states sigma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-incorrect-rules-error)
                      (raise-blame-error
                       blame
                       (incorrect-tm-rules states sigma rules)
                       "Rule four of the design recipe was violated.\nThe following rules have errors, which make them invalid")))))

  ;correct-mttm-rules/c: (listof state) (listof alpha) --> contract
  ;predicate: (listof x) --> boolean
  ;Purpose: Ensures that every element in the list is a valid mttm rule. It checks
  ; each rule to see that the from-state and to-state are in the list of machine
  ; states, that every read tape symbol is either in the machine sigma or is the
  ; BLANK or LM constants, and that every tm-action is either in the machine
  ; sigma or is the LEFT or RIGHT constants.
  (define (correct-mttm-rules/c states sigma)
    (make-flat-contract
     #:name 'correct-mttm-rules
     #:first-order (lambda (rules) (correct-members-mttm? states sigma rules))
     #:projection (lambda (blame)
                    (lambda (rules)
                      (current-blame-format format-incorrect-rules-error)
                      (raise-blame-error
                       blame
                       (incorrect-mttm-rules states sigma rules)
                       "Rule four of the design recipe was violated.\nThe following rules have errors, which make them invalid")))))

  ;functional/c: (listof state) (listof sigma) boolean -> contract
  ;predicate: (listof x) --> boolean
  ;Purpose: Ensures that the given list of rules forms an entire function over
  ; the domain of machine states and sigma elements. For this to be true, either:
  ; a) the add-dead flag must be true, in which case dead-state rules are added
  ;    to make the rule-set a function regardless of the existing rules.
  ; b) there must be a rule in the list of rules for every pair in the Cartesian
  ;    product of state-sigma pairings.
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
                       (format "Rule four of the design recipe was violated.\nYou must include rules for these state/alphabet letter pairings: ")
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
                       (format "Rule four of the design recipe was violated.\nThere following state/sigma pairs are duplicated in your ~a: " type)
                       )
                      )
                    )
     )
    )

  
  )