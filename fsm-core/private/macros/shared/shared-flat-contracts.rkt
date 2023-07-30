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
                       (format "Expected a non-empty list of ~a(s) for the ~a, but got" element-name field-name))))
     )
    )
  
;valid-listof/c: ((listof x) --> boolean) string string --> contract
  ;; predicate: (listof x) --> boolean
  ;; helper: (listof x) --> (listof x)
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
                       "The following is not a valid non-dead state")))))

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

  ;valid-finals/c: (listof any) --> boolean or exception
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

  ;is-state-in-finals/c: (listof state) --> (state --> boolean or exception)
  ;purpose: Creates a flat contract that checks if a given state is in a list
  ; of final states.
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
                       (format "The following state is not a member of your list of final states ~a" finals)))))
    )

  ;valid-num-tapes/c: any --> boolean or exception
  ;purpose: creates a flat contract that checks if the given input is an integer
  ; between 1 (inclusive) and infinity.
  (define valid-num-tapes/c
    (make-flat-contract
     #:name 'valid-num-tapes
     #:first-order (lambda (val) (and (integer? val) (> val 0)))
     #:projection (lambda (blame)
                    (lambda (val)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       val
                       "The following is not an integer greater than 0")))))
  )