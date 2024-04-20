(module validation-flat-contracts racket
  (require "validation-predicates.rkt"
           "../error-formatting.rkt"
           "../../constants.rkt"
           racket/contract
           )
  (provide listof-words/c
           listof-words-tm/c
           words-in-sigma/c
           words-in-sigma-tm/c
           acceptable-position/c
           dfa-input/c
           ndfa-input/c
           ndpda-input/c
           tm-input/c
           mttm-input/c
           has-accept/c
           )

  (define (listof-words/c type)
    (make-flat-contract
     #:name 'valid-list-of-words
     #:first-order (lambda (words) (listof-words? words))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       words
                       (format "The expected ~a is not a list of words" type)
                     
                       )
                      )
                    )
     )
    )

    (define (listof-words-tm/c sigma)
    (make-flat-contract
     #:name 'valid-list-of-words-tm
     #:first-order (lambda (words) (listof-words-tm? words))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       words
                       (format "Turing machine words must be lists of symbols, or pairs of symbol lists and starting indexes ~s")
                     
                       )
                      )
                    )
     )
    )

  (define (words-in-sigma/c sigma field)
    (make-flat-contract
     #:name 'words-made-of-sigma-symbols
     #:first-order (lambda (words) (words-in-sigma? words sigma))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (invalid-words words sigma)
                       (format "Step six of the design recipe has not been successfully completed.\nThe following words the machine should ~a contain symbols not included in sigma" field)
                       )
                      )
                    )
     )
    )

  (define (words-in-sigma-tm/c sigma field)
    (make-flat-contract
     #:name 'words-made-of-sigma-symbols-tm
     #:first-order (lambda (words) (words-in-sigma-tm? words sigma))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (invalid-words-tm words sigma)
                       (format-error blame "Step six of the design recipe has not been successfully completed.\nThe following words the machine should ~a contain symbols not included in sigma" field)
                     
                       )
                      )
                    )
     )
    )

  (define (acceptable-position/c sigma)
    (make-flat-contract
      #:name 'tm-starting-position-in-words
     #:first-order (lambda (words) (acceptable-position? words))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       (unacceptable-position words)
                       (format "The following words have positions that are not valid in their list of words")
                     
                       )
                      )
                    )
     )
    )

  (define (accepts/rejects-formatter accepts?)
    (format "Step six of the design recipe has not been successfully completed.\nThe constructed machine does not ~s the following words: " accepts?))

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
                       (accepts/rejects-formatter accepts?)
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
                       (accepts/rejects-formatter accepts?)
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
                       (accepts/rejects-formatter accepts?)
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
                       (accepts/rejects-formatter accepts?)
                       )
                      )
                    )
     )
    )

  (define (mttm-input/c states sigma start finals rules num-tapes accept accepts?)
    (make-flat-contract
     #:name 'mttm-accepting-correctly
     #:first-order (check-input-mttm states sigma start finals rules num-tapes accept accepts?)
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-accepts-error)
                      (raise-blame-error
                       blame
                       (return-input-mttm states
                                          sigma
                                          start
                                          finals
                                          rules
                                          num-tapes
                                          words
                                          accept
                                          accepts?)
                       (accepts/rejects-formatter accepts?))))))

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
  )