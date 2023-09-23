(module flat-contracts racket
  (require "shared-predicates.rkt"
           "../error-formatting.rkt"
           "../../constants.rkt"
           "../../fsa.rkt"
           racket/contract
           )
  (provide
   is-nonempty-list/c
   valid-listof/c
   valid-start/c
   valid-non-dead-state/c
   start-in-states/c
   no-duplicates/c
   valid-finals/c
   is-state-in-finals/c
   valid-num-tapes/c
   )

  ;is-nonempty-list//c: string string --> contract
  ;; predicate: any --> boolean
  ;; Purpose: A flat contract that checks if the input is a non-empty list.
  (define (is-nonempty-list/c element-name field-name)
    (make-flat-contract
     #:name 'is-nonempty-list/c
     #:first-order (lambda (x) (and (list? x) (not (empty? x))))
     #:projection (lambda (blame)
                    (lambda (x)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       x
                       (if (equal? element-name "state")
                           (format "Step three of the design recipe has not be succesfully completed, the list of ~a cannot be empty" field-name)
                           (format "Step one of the design recipe has not be succesfully completed, the ~a cannot be empty" field-name))))
                    )
     )
    )
  
  ;valid-listof/c: ((listof any) --> boolean) string string --> contract
  ;; predicate: (listof any) --> boolean
  ;; helper: (listof any) --> (listof any)
  ;Purpose: applies a predicate to a list and makes sure that everything in
  ; the list passes the predicate. Returns an error if it doesnt
  (define (valid-listof/c predicate element-name field-name #:rule [rule ""])
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
                       (format "~a.\n The following: ~a are not valid ~as in the given ~a"
                               (if (equal? rule "")
                                   ""
                                   (format "Step ~a of the design recipe was not successfully completed" rule))
                               invalid-vals
                               element-name
                               field-name
                               )
                       )
                      )
                    )
     )
    )

  ;valid-non-dead-state/c: () --> contract
  ;; predicate: (any) --> boolean
  ;Purpose: to ensure the state given is a valid state, that isnt the dead
  ; state and to retunran error message if that is not the case
  (define valid-non-dead-state/c
    (make-flat-contract
     #:name 'valid-state
     #:first-order (lambda (state) (valid-non-dead-state? state))
     #:projection (lambda (blame)
                    (lambda (state)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       state
                       "Step three of the design recipe has not been successfully completed.\nThe following is not a valid non-dead state")))))

  ;valid-states/c: () --> contract
  ;;predicate: (listof any) --> boolean
  ;;helper: (listof any) --> listof any
  ;Purpose: to check if all the states in a list are valid, and if not, return an
  ; error containing the failing states
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

  ;valid-start/c: (listof states) --> contract
  ;; predicate: (listof states) --> (any --> boolean)
  ;Purpose: to check that the start state is a valid state and is a single symbol
  ; returns an error message if that is not the case
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
                       (format "Step three of the design recipe was not successfully completed.\nThe given starting state: ~s is not a valid state" start)
                       )
                      )
                    )
     )
    )

  ;start-in-states/c: (listof states) --> contract
  ;; predicate: (listof states) --> (symbol --> boolean)
  ;Purpose: to check if the starting state is in the list of states, and return
  ; an error containing the start state and list of states if that is not the case
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
                       (format "Step three of the design recipe has not been successfully completed.\nThe following starting state is not in ~a, the given list of states: ~a" states start)
                       )
                      )
                    )
     )
    )

  ;no-duplicates/c: string --> contract
  ;; predicate: (listof any) --> boolean
  ;; helper: (listof any) --> (listof any)
  ;Purpose: to check if there are any duplicates in a list, and if there are
  ; returns an error that contains the offending value, and the type of list that
  ; those values are coming from
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
                       (format "Step ~a of the design recipe has not been sucessfully completed.\nThere following values are duplicated in the given ~a: "
                               (if (or (equal? type "sigma") (equal? type "gamma"))
                                   "one"
                                   (if (or (equal? type "states") (equal? type "final states"))
                                       "three"
                                       "four")) type)
                       )
                      )
                    )
     )
    )

  ;valid-finals/c: (listof any) --> contract
  ;; predicate: (listof states) --> ((listof any) --> boolean)
  ;; helper: (listof states) --> ((listof any) --> (listof any))
  ;purpose: creates a flat contract that checks if a given list of elements is
  ; a valid list of machine finals.
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

  ;is-state-in-finals/c: (listof any) --> contract
  ;; predicate: any --> boolean
  ;purpose: Creates a flat contract that checks if a given state is in a list
  ; of final states and returns an error with that state and the final states
  ; if that is not the case
  (define (is-state-in-finals/c finals)
    (make-flat-contract
     #:name 'is-state-in-finals
     #:first-order (lambda (state) (member state finals))
     #:projection (lambda (blame)
                    (lambda (state)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       state
                       (format "Step three of the design recipe has not been sucecssfully completed.\nThe following state is not a member of the given list of final states ~a" finals)))))
    )

  ;valid-num-tapes/c: any --> contract
  ;; predicate: any --> boolean
  ;purpose: creates a flat contract that checks if the given input is an integer
  ; between 1 (inclusive) and infinity and returns an error if that is not the case
  (define valid-num-tapes/c
    (make-flat-contract
     #:name 'valid-num-tapes
     #:first-order (lambda (val) (and (integer? val) (> val 0)))
     #:projection (lambda (blame)
                    (lambda (val)
                      (current-blame-format format-start-error)
                      (raise-blame-error
                       blame
                       val
                       (format "Step five of the design recipe has not been successfully completed.\nThe given number of tapes: ~a is not an integer greater than 0" val))))))
  )