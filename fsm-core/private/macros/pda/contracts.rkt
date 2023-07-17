(module contracts racket
  (require
    "helpers.rkt"
    "predicates.rkt"
    "../formatting.rkt"
    )
  (provide
   ndpda-input/c
   )

  (define (ndpda-input/c states
                        sigma
                        gamma
                        start
                        finals
                        rules
                        accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-ndpda states
                                     sigma
                                     gamma
                                     start
                                     finals
                                     rules
                                     accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (return-input-ndpda states
                                          sigma
                                          gamma
                                          start
                                          finals
                                          rules
                                          words
                                          accepts?)
                       (format "Does not ~a the predicted value" accepts?)
                       )
                      )
                    )
     )
    )
  )
