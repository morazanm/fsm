(module helpers racket
  (require
    "../../pda.rkt"
    )
  (provide
   return-input-ndpda
   )
  (define (return-input-ndpda states
                              sigma
                              gamma
                              start
                              finals
                              rules
                              words
                              accepts?)
    (define temp-machine (make-unchecked-ndpda states
                                               sigma
                                               gamma
                                               start
                                               finals
                                               rules))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )
  )