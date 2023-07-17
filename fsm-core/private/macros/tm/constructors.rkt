(module constructors racket
  (require
    racket/contract
    "contracts.rkt"
    "helpers.rkt"
    "predicates.rkt"
    "../contracts.rkt"
    "../predicates.rkt"
    "../../tm.rkt"
    "../../constants.rkt"
    )
  (provide
   make-tm2
   )
  (define/contract (make-tm2 states sigma rules start finals
                             [accept 'null]
                             #:accepts [accepts '()]
                             #:rejects [rejects '()]
                             )
    (->i ([states (and/c (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [rules (states
                  sigma) (and/c (listof-rules/c valid-tm-rule?)
                                (correct-members/c
                                 correct-members-tm?
                                 incorrect-members-tm
                                 states
                                 (cons RIGHT (cons LEFT (cons BLANK sigma))))
                                (no-duplicates/c "rules"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          )
         ([accept (states) (and/c symbol?
                                  (lambda (x) (member x states)))]
          #:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules
                              accept) (and/c (has-accept/c accept finals)
                                             (listof-words/c sigma)
                                             (tm-input/c states
                                                         sigma
                                                         start
                                                         finals
                                                         rules
                                                         accept
                                                         'accept)
                                             )]
          #:rejects [rejects (states
                              sigma
                              start
                              finals
                              rules
                              accept) (and/c (has-accept/c accept finals)
                                             (listof-words/c sigma)
                                             (tm-input/c states
                                                         sigma
                                                         start
                                                         finals
                                                         rules
                                                         accept
                                                         'reject))]
          )
         
         [result tm?])
  
    (if (equal? accept 'null)
        (make-unchecked-tm states sigma rules start finals)
        (make-unchecked-tm states sigma rules start finals accept))
    )
  )