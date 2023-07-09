(module flat-contracts racket
  (require "predicates.rkt"
           "error-formatting.rkt"
           "../constants.rkt"
           racket/contract
           )
  (provide functional/c
           no-duplicates/c
           valid-finals/c
           valid-start/c
           start-in-states/c)

  (define (valid-start/c start)
    (make-flat-contract
     #:name 'valid-starting-state
     #:first-order (valid-start? start)
     #:projection (lambda (blame)
                    (current-blame-format format-start-error)
                    (raise-blame-error
                     blame
                     start
                     (format "The value ~s is not a valid state" start)
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
                       (format "There following values are duplicated in your ~a, ~a : " type vals)
                       )
                      )
                    )
     )
    )

  (define (functional/c states sigma add-dead)
    (make-flat-contract
     #:name 'functional-list-of-rules?
     #:first-order (lambda (rules) (functional? rules states sigma add-dead))
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
  )