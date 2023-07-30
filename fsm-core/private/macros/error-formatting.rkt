(module error-formatting racket
  (require racket/contract
           "../constants.rkt")
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
    (format "~a: ~a" message value))
  
  (define (format-finals-error blame value message)
    (format "The following final states are not in ~a, your list of states: ~s" message value))

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
    (define (format-incorrect-rule-error rule-with-errors)
      (define rule (first rule-with-errors))
      (define from-state-error (if (first (second rule-with-errors)) "" (format " The state ~a is not in your list of states." (first rule))))
      (define read-letter-error (if (second (second rule-with-errors)) "" (format " The letter ~a is not in your machine sigma." (second rule))))
      (define to-state-error (if (third (second rule-with-errors)) "" (format " The state ~a is not in your list of states." (third rule))))
      (format "Rule ~a:~a~a~a\n" rule from-state-error read-letter-error to-state-error))
    (define all-rule-errors-formatted (foldl string-append "" (map format-incorrect-rule-error rules-with-errors)))
    (format "~a:\n~a" message all-rule-errors-formatted)
    )

  )