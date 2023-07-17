(module constructors racket
  (require
    racket/contract
    "contracts.rkt"
    "predicates.rkt"
    "../contracts.rkt"
    "../predicates.rkt"
    "../../pda.rkt"
    )
  (provide 
   make-ndpda2
   )

  ;; Purpose: Constrcts an ndpda given a set of states, a machine alphabet,
  ;; set of stack symbols, a start state, a list of final states, and a list
  ;; of ndpda rules. The function checks that all fields are valid before
  ;; constructing the ndpda.
  (define/contract (make-ndpda2 states sigma gamma start finals rules
                                #:accepts [accepts '()]
                                #:rejects [rejects '()])
    (->i ([states (and/c (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [gamma (and/c (valid-listof/c (lambda (g) (or (valid-state? g) (valid-alpha? g))) "stack symbol" "list of stack symbols")
                        (no-duplicates/c "gamma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  gamma) (and/c (valid-listof/c (valid-ndpda-rule? states sigma gamma) "machine rule" "list of machine rules")
                                (no-duplicates/c "rules"))]
          )
         (#:accepts [accepts (states
                              sigma
                              gamma
                              start
                              finals
                              rules) (and/c (listof-words/c sigma)
                                            (ndpda-input/c states
                                                           sigma
                                                           gamma
                                                           start
                                                           finals
                                                           rules
                                                           'accept))]
          #:rejects [rejects (states
                              sigma
                              gamma
                              start
                              finals
                              rules) (and/c (listof-words/c sigma)
                                            (ndpda-input/c states
                                                           sigma
                                                           gamma
                                                           start
                                                           finals
                                                           rules
                                                           'reject))]
          )
         [result ndpda?])
    (make-unchecked-ndpda states sigma gamma start finals rules)
    ) 
  )
