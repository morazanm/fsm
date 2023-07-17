(module helpers racket
  (require
    "../../tm.rkt"
    "../../../../main.rkt"
    )
  (provide
   incorrect-members-tm
   return-input-tm
   )

  (define (incorrect-members-tm states sigma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) sigma)
                                  (member (car (cadr x)) states)
                                  (member (cadr (cadr x)) sigma)))) rules))

  (define (return-input-tm states
                           sigma
                           start
                           finals
                           rules
                           words
                           accept
                           accepts?)
    (define temp-machine (make-unchecked-tm states
                                            sigma
                                            rules
                                            start
                                            finals
                                            accept))
    (filter (lambda (x) (equal? (sm-apply temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )
  )