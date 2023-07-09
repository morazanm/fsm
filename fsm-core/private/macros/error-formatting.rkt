(module error-formatting racket
  (require racket/contract
           "../constants.rkt")
  (provide format-finals-error
           format-duplicates-error)
  
  (define (format-finals-error blame value message)
    (format "The following final states are not in ~a, your list of states: ~s" message value))

  (define (format-duplicates-error blame value message)
    (format "~a ~a" message value)
    )

  )