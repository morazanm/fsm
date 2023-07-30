(module flat-contracts racket
  (require "shared-predicates.rkt"
           "../error-formatting.rkt"
           "../../constants.rkt"
           "../../fsa.rkt"
           racket/contract
           )
  (provide valid-listof/c
           valid-start/c
           start-in-states/c
           no-duplicates/c
           valid-finals/c
           )
  ;valid-listof/c: ((listof x) --> boolean) string string --> contract
  ;; predicate: (listof x) --> boolean
  ;; helper: (listof x) --> (listof x)
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

  ;valid-start/c: (listof states) --> contract
  ;; predicate: ((listof x) --> (x --> boolean)
  ;PURPOSE: 
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