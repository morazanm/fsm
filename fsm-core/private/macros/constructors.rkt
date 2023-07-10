(module constructors racket
  (require "flat-contracts.rkt"
           "../constants.rkt"
           "predicates.rkt"
           "error-formatting.rkt"
           racket/contract
           )
  (provide make-dfa)

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
  ;; DFA inputs, but for now just parses inputs and constructs a list.

  ;; Open questions:
  ;; 1. If add-dead boolean flag is true, do we need to disallow DEAD state from states.
  ;; 2. This code does not yet check for duplicates in the list fields - this must
  ;; be added.
  (define/contract (make-dfa states sigma start finals rules
                             [add-dead #t]
                             #:accepts [accepts '()]
                             #:rejects [rejects '()])
    (->i ([states (and/c (listof valid-state?)
                         (no-duplicates/c "states"))]
          [sigma (and/c (listof valid-alpha?)
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (listof valid-state?)
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  add-dead) (and/c (listof-rules/c valid-dfa-rule? states sigma)
                                   (functional/c states sigma add-dead)
                                   (no-duplicates/c "rules"))]
          )
         ([add-dead boolean?]
          #:accepts [accepts (listof symbol?)]
          #:rejects [rejects (listof symbol?)]
          )
         
         [result list?])
    (define all-rules
      (if add-dead
          (add-dead-state-rules rules states sigma)
          rules))
    (list states sigma start finals all-rules add-dead)
    )
  
  )
