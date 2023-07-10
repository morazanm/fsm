(module error-formatting racket
  (require racket/contract
           "../constants.rkt")
  (provide format-error
           format-finals-error
           format-duplicates-error
           format-start-error
           format-rule-error
           format-accepts-error
           format-missing-rule-error)

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

  (define (format-accepts-error blame value message)
    (format "~a ~a" message value)
    )

  (define (format-missing-rule-error blame value message)
    (format "~a ~a" message value))

  )