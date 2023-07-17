(module contracts racket
  (require
    "helpers.rkt"
    "predicates.rkt"
    "../formatting.rkt"
    "../predicates.rkt"
    )
  (provide
   has-accept/c
   tm-input/c
   )
  (define (has-accept/c accept finals)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (lambda (words) (member accept finals))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       words
                       (format "Must have a value for accepted state in final states to process list of words")
                       )
                      )
                    )
     )
    )

  (define (tm-input/c states
                      sigma
                      start
                      finals
                      rules
                      accept
                      accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-tm states
                                   sigma
                                   start
                                   finals
                                   rules
                                   accept
                                   accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (return-input-tm states
                                        sigma
                                        start
                                        finals
                                        rules
                                        words
                                        accept
                                        accepts?)
                       (format "Does not ~s the predicted value" accepts?)
                       )
                      )
                    )
     )
    )
  )