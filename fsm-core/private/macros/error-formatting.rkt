(module error-formatting racket
  (require racket/contract
           rackunit
           "../constants.rkt"
           "rules/rules-predicates.rkt")
  (provide format-error
           format-finals-error
           format-duplicates-error
           format-start-error
           format-rule-error
           format-accepts-error
           format-missing-rule-error
           format-rule-format-error
           format-incorrect-rules-error)

  (define (format-error blame value message)
    (if (string? value)
        (format "~a: ~s" message value)
        (format "~a: ~a" message value)))
  
  (define (format-finals-error blame value message)
    (format "Step three of the design recipe has not been successfully completed.\nThe following final states, ~a, are not in your list of states: ~a" value message ))

  (define (format-duplicates-error blame value message)
    (format "~a ~a" message value)
    )

  (define (format-start-error blame value message)
    message
    )

  (define (format-rule-error blame value message)
    (format "The following rules contain symbols not included in your state list or alphabet: ~a" value)
    )

  (define (format-rule-format-error blame value message)
    (format "~a ~a" message value))

  (define (format-accepts-error blame value message)
    (format "~a ~a" message value)
    )

  (define (format-missing-rule-error blame value message)
    (format "~a ~a" message value))

  (define (format-incorrect-rules-error blame rules-with-errors message)
    (define all-rule-errors-formatted (foldr string-append "" (map format-incorrect-rule-error rules-with-errors)))
    (format "~a:\n~a" message all-rule-errors-formatted)
    )

  ;format-incorrect-rule-error: invalid-rule --> string
  ;purpose: takes as input an invalid-rule struct, and creates a formatted
  ; string representation of the rule with its error messages.
  (define (format-incorrect-rule-error rule-with-errors)
    ;make-string: (listof string) string string string --> string
    ;purpose: concatenates the elements in the list of strings together using
    ; the provided separator. The string is prefixed and suffixed with the
    ; provided prefix and suffix strings.
    (define (make-string los prefix separator suffix)
      (string-append
       prefix
       (string-append (first los) (foldr string-append "" (map (lambda (s) (string-append separator s)) (rest los))))
       suffix)
      )
    (define rule (invalid-rule-rule rule-with-errors))
    (define formatted-rule-errors
      (make-string (invalid-rule-errors rule-with-errors) "\n  " "\n  " "\n"))
    (format "Rule ~a:~a" rule formatted-rule-errors))

  


  (module+ test

    ;format-incorrect-rule-error tests
    (check-equal? (format-incorrect-rule-error (make-invalid-rule '(A b C)
                                                                  '("This is the first error message."
                                                                    "This is the second error message."
                                                                    "This is the third error message.")))
                  "Rule (A b C):\n  This is the first error message.\n  This is the second error message.\n  This is the third error message.\n")
    )

  )