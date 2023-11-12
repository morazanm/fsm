(module constructors racket
  (require "rules/rules-flat-contracts.rkt"
           "shared/shared-flat-contracts.rkt"
           "validation/validation-flat-contracts.rkt"
           "../constants.rkt"
           "rules/rules-predicates.rkt"
           "shared/shared-predicates.rkt"
           "validation/validation-predicates.rkt"
           "error-formatting.rkt"
           "../fsa.rkt"
           "../pda.rkt"
           "../tm.rkt"
           "../mtape-tm.rkt"
           racket/contract
           )
  (provide make-dfa2
           make-ndfa2
           make-ndpda2
           make-tm2
           make-mttm2
           )

  ;; Using the existing set of rules, the entire machine set of states, and the
  ;; machine alphabet, generates a list of rules that contains the original rule
  ;; set, plus any additional transitions from states to the DEAD state to make
  ;; the rule set a function.
  (define (add-dead-state-rules rules states sigma)
    (define all-state-sigma-pairs (cartesian-product states sigma))
    (define existing-state-sigma-pairs
      (map (lambda (rule) (list (first rule) (second rule))) rules))
    (define missing-state-sigma-pairs
      (filter
       (lambda (pair) (not (member pair existing-state-sigma-pairs)))
       all-state-sigma-pairs)
      )
    (define dead-state-rules
      (map
       (lambda (pair) (append pair (list DEAD)))
       missing-state-sigma-pairs)
      )
    (append rules dead-state-rules))

  ;; make-dfa: states alphabet state states rules (boolean) -> machine
  ;; Purpose: Eventually, will construct a multi-tape turing-machine from the given
  ;; DFA inputs, but for now just parses inputs and constructs an unchecked-dfa.
  (define/contract (make-dfa2 states sigma start finals rules
                              [add-dead #t]
                              #:accepts [accepts '()]
                              #:rejects [rejects '()])
    (->i ([states (and/c (is-a-list/c "machine states" "three")
                         (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  add-dead) (and/c (is-a-list/c "machine rules" "four")
                                   correct-dfa-rule-structures/c
                                   (correct-dfa-rules/c states sigma)
                                   (functional/c states sigma add-dead)
                                   (no-duplicates-dfa/c "rules"))]
          )
         ([add-dead boolean?]
          #:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules
                              add-dead) (and/c (words-in-sigma/c sigma)
                                               (listof-words/c "accpets")
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
                              add-dead) (and/c (words-in-sigma/c sigma)
                                               (listof-words/c "rejects")
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
    (->i ([states (and/c (is-a-list/c "machine states" "three")
                         (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma) (and/c (is-a-list/c "machine rules" "four")
                          correct-dfa-rule-structures/c
                                (correct-dfa-rules/c states (cons EMP sigma))
                                (no-duplicates/c "rules"))]
          )
         (#:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules) (and/c (listof-words/c "accpets")
                                            (words-in-sigma/c sigma)
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
                              rules) (and/c (listof-words/c "rejects")
                                            (words-in-sigma/c sigma)
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

  ;; Purpose: Constructs an ndpda given a set of states, a machine alphabet,
  ;; set of stack symbols, a start state, a list of final states, and a list
  ;; of ndpda rules. The function checks that all fields are valid before
  ;; constructing the ndpda.
  (define/contract (make-ndpda2 states sigma gamma start finals rules
                                #:accepts [accepts '()]
                                #:rejects [rejects '()])
    (->i ([states (and/c (is-a-list/c "machine states" "three")
                         (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-a-list/c "machine sigma" "one")
                        (valid-listof/c valid-alpha? "alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [gamma (and/c (is-a-list/c "machine gamma" "one")
                        (valid-listof/c (lambda (g) (or (valid-state? g) (valid-alpha? g))) "stack symbol" "stack alphabet" #:rule "one")
                        (no-duplicates/c "gamma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  gamma) (and/c (is-a-list/c "machine rules" "four")
                                correct-ndpda-rule-structures/c
                                (correct-ndpda-rules/c states sigma gamma)
                                (no-duplicates/c "rules"))]
          )
         (#:accepts [accepts (states
                              sigma
                              gamma
                              start
                              finals
                              rules) (and/c (listof-words/c "accpets")
                                            (words-in-sigma/c sigma)
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
                              rules) (and/c (listof-words/c "rejects")
                                            (words-in-sigma/c sigma)
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

   
  (define/contract (make-tm2 states sigma rules start finals
                             [accept 'null]
                             #:accepts [accepts '()]
                             #:rejects [rejects '()]
                             )
    (->i ([states (and/c (is-a-list/c "machine states" "three")
                         (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [rules (states
                  sigma) (and/c (is-a-list/c "machine rules" "four")
                                correct-tm-rule-structures/c
                                (correct-tm-rules/c states sigma)
                                (no-duplicates/c "rules"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          )
         ([accept (finals) (and/c valid-non-dead-state/c
                                  (is-state-in-finals/c finals))]
          #:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules
                              accept) (and/c (has-accept/c accept finals)
                                             (listof-words-tm/c sigma)
                                             (acceptable-position/c sigma)
                                             (words-in-sigma-tm/c (append (list BLANK LM) sigma))
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
                                             (listof-words-tm/c sigma)
                                             (acceptable-position/c sigma)
                                             (words-in-sigma-tm/c (append (list BLANK LM) sigma))
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

  (define/contract (make-mttm2 states sigma start finals rules num-tapes
                               [accept 'null]
                               #:accepts [accepts '()]
                               #:rejects [rejects '()])
    (->i ([states (and/c (is-a-list/c "machine states" "three")
                         (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  num-tapes) (and/c (is-a-list/c "machine rules" "four")
                                    (correct-mttm-rule-structures/c num-tapes)
                                    (correct-mttm-rules/c states sigma)
                                    (no-duplicates/c "rules"))]
          [num-tapes  (and/c valid-num-tapes/c)]
          )
         ([accept (finals) (and/c valid-non-dead-state/c
                                  (is-state-in-finals/c finals))]
          #:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules
                              num-tapes
                              accept) (and/c (has-accept/c accept finals)
                                             (listof-words-tm/c sigma)
                                             (acceptable-position/c sigma)
                                             (words-in-sigma-tm/c (append (list BLANK LM) sigma))
                                             (mttm-input/c states
                                                           sigma
                                                           start
                                                           finals
                                                           rules
                                                           num-tapes
                                                           accept
                                                           'accept)
                                             )]
          #:rejects [rejects (states
                              sigma
                              start
                              finals
                              rules
                              num-tapes
                              accept) (and/c (has-accept/c accept finals)
                                             (listof-words-tm/c sigma)
                                             (acceptable-position/c sigma)
                                             (words-in-sigma-tm/c (append (list BLANK LM) sigma))
                                             (mttm-input/c states
                                                           sigma
                                                           start
                                                           finals
                                                           rules
                                                           num-tapes
                                                           accept
                                                           'reject))]
          )
         [result mttm?]
         )
    (if (equal? accept 'null)
        (make-mttm states sigma start finals rules num-tapes)
        (make-mttm states sigma start finals rules num-tapes accept))
    )
  )
