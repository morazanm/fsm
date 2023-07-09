(module error-formatting racket
  (require racket/contract
           "../constants.rkt")
  (provide format-finals-error)
  
  (define (format-finals-error blame value message)
    (format "The following final states are not in ~a, your list of states: ~s" message value))

  )