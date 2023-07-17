(module constructors racket
  (require
    "contracts.rkt"
    "helpers.rkt"
    "predicates.rkt"
    "../contracts.rkt"
    "../predicates.rkt"
    "../../fsa.rkt"
    "../../constants.rkt"
    racket/contract
    )
  (provide
   make-dfa2
   make-ndfa2
   )

  ;; make-dfa: states alphabet state states rules (boolean) -> machine
  ;; Purpose: Eventually, will construct a multi-tape turing-machine from the given
  ;; DFA inputs, but for now just parses inputs and constructs an unchecked-dfa.
  (define/contract (make-dfa2 states sigma start finals rules
                              [add-dead #t]
                              #:accepts [accepts '()]
                              #:rejects [rejects '()])
    (->i ([states (and/c (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  add-dead) (and/c (listof-rules/c valid-dfa-rule?)
                                   (correct-members/c
                                    correct-members-dfa?
                                    incorrect-members-dfa
                                    states
                                    sigma)
                                   (functional/c states sigma add-dead)
                                   (no-duplicates-dfa/c "rules"))]
          )
         ([add-dead boolean?]
          #:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules
                              add-dead) (and/c (listof-words/c sigma)
                                               (dfa-input/c states
                                                            sigma
                                                            start
                                                            finals
                                                            rules
                                                            add-dead
                                                            'accept))]
          #:rejects [rejects (states
                              sigma
                              start
                              finals
                              rules
                              add-dead) (and/c (listof-words/c sigma)
                                               (dfa-input/c states
                                                            sigma
                                                            start
                                                            finals
                                                            rules
                                                            add-dead
                                                            'reject))]
          )
         
         [result dfa?])
    (define all-rules
      (if add-dead
          (add-dead-state-rules rules states sigma)
          rules))
    (make-unchecked-dfa states sigma start finals all-rules add-dead)
    )

  (define/contract (make-ndfa2 states sigma start finals rules
                               #:accepts [accepts '()]
                               #:rejects [rejects '()])
    (->i ([states (and/c (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma) (and/c (listof-rules/c valid-dfa-rule?)
                                (correct-members/c
                                 correct-members-dfa?
                                 incorrect-members-dfa
                                 states
                                 (cons EMP sigma))
                                (no-duplicates/c "rules"))]
          )
         (#:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules) (and/c (listof-words/c sigma)
                                            (ndfa-input/c states
                                                          sigma
                                                          start
                                                          finals
                                                          rules
                                                          'accept))]
          #:rejects [rejects (states
                              sigma
                              start
                              finals
                              rules) (and/c (listof-words/c sigma)
                                            (ndfa-input/c states
                                                          sigma
                                                          start
                                                          finals
                                                          rules
                                                          'reject))]
          )
         
         [result ndfa?])
  
    (make-unchecked-ndfa states sigma start finals rules)
    )
  )
