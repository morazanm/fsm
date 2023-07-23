(module validation-flat-contracts racket
  (require "validation-predicates.rkt"
           "../error-formatting.rkt"
           "../../constants.rkt"
           "../../fsa.rkt"
           racket/contract
           )
  (provide listof-words/c
           dfa-input/c
           ndfa-input/c
           ndpda-input/c
           tm-input/c
           has-accept/c
           )

  (define (listof-words/c sigma)
    (make-flat-contract
     #:name 'valid-list-of-words
     #:first-order (lambda (words) (listof-words? words sigma))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       words
                       (format "Not given an accurate list of words ~s" words)
                     
                       )
                      )
                    )
     )
    )

  (define (dfa-input/c states
                       sigma
                       start
                       finals
                       rules
                       add-dead
                       accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-dfa states
                                    sigma
                                    start
                                    finals
                                    rules
                                    add-dead
                                    accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       (return-input-dfa states
                                         sigma
                                         start
                                         finals
                                         rules
                                         add-dead
                                         words
                                         accepts?)
                       (format "Does not ~s the predicted value: " accepts?)
                       )
                      )
                    )
     )
    )

  (define (ndfa-input/c states
                        sigma
                        start
                        finals
                        rules
                        accepts?)
    (make-flat-contract
     #:name 'machine-accepting-correctly
     #:first-order (check-input-ndfa states
                                     sigma
                                     start
                                     finals
                                     rules
                                     accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       (return-input-ndfa states
                                          sigma
                                          start
                                          finals
                                          rules
                                          words
                                          accepts?)
                       (format "Does not ~s the predicted value: " accepts?)
                       )
                      )
                    )
     )
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
                      (current-blame-format format-accepts-error)
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
                       (format "Does not ~s the predicted value: " accepts?)
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
                      (current-blame-format format-accepts-error)
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
                       (format "Does not ~s the predicted value: " accepts?)
                       )
                      )
                    )
     )
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
                       (format "Must have a value for accepted state in final states to process list of words: ")
                       )
                      )
                    )
     )
    )
  )