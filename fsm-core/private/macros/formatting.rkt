(module formatting racket
  (require racket/contract)
  (provide format-error)

  (define (format-error blame value message)
    (format "~a: ~a" message value))
  )
