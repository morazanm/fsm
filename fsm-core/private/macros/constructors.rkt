#lang racket/base
(require "rules/rules-flat-contracts.rkt"
         "shared/shared-flat-contracts.rkt"
         "validation/validation-flat-contracts.rkt"
         "../constants.rkt"
         "shared/shared-predicates.rkt"
         racket/contract
         racket/list
         )
(provide make-dfa/c
         make-ndfa/c
         make-ndpda/c
         make-tm/c
         make-mttm/c
         add-dead-state-rules
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

(define make-dfa/c (->i ([states (and/c (is-a-list/c "machine states" "three")
                                        (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                                        (no-duplicates/c "states" "three"))]
                         [sigma (and/c (is-a-list/c "machine alphabet" "one")
                                       (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                                       (no-duplicates/c "sigma" "one"))]
                         [start (states) (and/c (valid-start/c states)
                                                (start-in-states/c states))]
                         [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                                 (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                                 (valid-finals/c states)
                                                 (no-duplicates/c "final states" "three"))]
                         [rules (states
                                 sigma
                                 add-dead) (and/c (is-a-list/c "machine rules" "four")
                                                  correct-dfa-rule-structures/c
                                                  (correct-dfa-rules/c states sigma)
                                                  (functional/c states sigma add-dead)
                                                  (no-duplicates-dfa/c "rules"))]
                         )
                        ([add-dead (lambda (x) (equal? 'no-dead x))]
                         #:accepts [accepts (states
                                             sigma
                                             start
                                             finals
                                             rules
                                             add-dead) (and/c (words-in-sigma/c sigma 'accepts)
                                                              (listof-words/c "accepts")
                                                              (dfa-input/c states
                                                                           sigma
                                                                           start
                                                                           finals
                                                                           rules
                                                                           add-dead
                                                                           #t))]
                         #:rejects [rejects (states
                                             sigma
                                             start
                                             finals
                                             rules
                                             add-dead) (and/c (words-in-sigma/c sigma 'rejects)
                                                              (listof-words/c "rejects")
                                                              (dfa-input/c states
                                                                           sigma
                                                                           start
                                                                           finals
                                                                           rules
                                                                           add-dead
                                                                           #f))]
                         )
         
                        [result dfa?]))

(define make-ndfa/c
  (->i ([states (and/c (is-a-list/c "machine states" "three")
                       (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                       (no-duplicates/c "states" "three"))]
        [sigma (and/c (is-a-list/c "machine alphabet" "one")
                      (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                (valid-finals/c states)
                                (no-duplicates/c "final states" "three"))]
        [rules (states
                sigma) (and/c (is-a-list/c "machine rules" "four")
                              correct-dfa-rule-structures/c
                              (correct-dfa-rules/c states (cons EMP sigma))
                              (no-duplicates/c "rules" "four"))]
        )
       (#:accepts [accepts (states
                            sigma
                            start
                            finals
                            rules) (and/c (listof-words/c "accepts")
                                          (words-in-sigma/c sigma 'accepts)
                                          (ndfa-input/c states
                                                        sigma
                                                        start
                                                        finals
                                                        rules
                                                        #t))]
        #:rejects [rejects (states
                            sigma
                            start
                            finals
                            rules) (and/c (listof-words/c "rejects")
                                          (words-in-sigma/c sigma 'rejects)
                                          (ndfa-input/c states
                                                        sigma
                                                        start
                                                        finals
                                                        rules
                                                        #f))]
        )
         
       [result ndfa?]))

(define make-ndpda/c
  (->i ([states (and/c (is-a-list/c "machine states" "three")
                       (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                       (no-duplicates/c "states" "three"))]
        [sigma (and/c (is-a-list/c "machine sigma" "one")
                      (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [gamma (and/c (is-a-list/c "machine gamma" "one")
                      (valid-listof/c (lambda (g) (or (valid-state? g) (valid-alpha? g))) "stack symbol" "stack alphabet" #:rule "one")
                      (no-duplicates/c "gamma" "one"))]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                (valid-finals/c states)
                                (no-duplicates/c "final states" "three"))]
        [rules (states
                sigma
                gamma) (and/c (is-a-list/c "machine rules" "four")
                              correct-ndpda-rule-structures/c
                              (correct-ndpda-rules/c states sigma gamma)
                              (no-duplicates/c "rules" "four"))]
        )
       (#:accepts [accepts (states
                            sigma
                            gamma
                            start
                            finals
                            rules) (and/c (listof-words/c "accepts")
                                          (words-in-sigma/c sigma 'accepts)
                                          (ndpda-input/c states
                                                         sigma
                                                         gamma
                                                         start
                                                         finals
                                                         rules
                                                         #t))]
        #:rejects [rejects (states
                            sigma
                            gamma
                            start
                            finals
                            rules) (and/c (listof-words/c "rejects")
                                          (words-in-sigma/c sigma 'rejects)
                                          (ndpda-input/c states
                                                         sigma
                                                         gamma
                                                         start
                                                         finals
                                                         rules
                                                         #f))]
        )
       [result ndpda?]))

(define make-tm/c
  (->i ([states (and/c (is-a-list/c "machine states" "three")
                       (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                       (no-duplicates/c "states" "three"))]
        [sigma (and/c (is-a-list/c "machine alphabet" "one")
                      (valid-listof/c valid-tm-alpha? "alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [rules (states
                sigma) (and/c (is-a-list/c "machine rules" "four")
                              correct-tm-rule-structures/c
                              (correct-tm-rules/c states sigma)
                              (no-duplicates/c "rules" "four"))]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                (valid-finals/c states)
                                (no-duplicates/c "final states" "three"))]
        )
       ([accept (finals) (and/c valid-non-dead-state/c
                                (is-state-in-finals/c finals))]
        #:accepts [accepts (states
                            sigma
                            start
                            finals
                            rules
                            accept) (and/c (has-accept/c accept finals)
                                           (listof-words-tm/c "accepts")
                                           (acceptable-position/c sigma)
                                           (words-in-sigma-tm/c (append (list BLANK LM) sigma) 'accept)
                                           (tm-input/c states
                                                       sigma
                                                       start
                                                       finals
                                                       rules
                                                       accept
                                                       #t)
                                           )]
        #:rejects [rejects (states
                            sigma
                            start
                            finals
                            rules
                            accept) (and/c (has-accept/c accept finals)
                                           (listof-words-tm/c "rejects")
                                           (acceptable-position/c sigma)
                                           (words-in-sigma-tm/c (append (list BLANK LM) sigma) 'reject)
                                           (tm-input/c states
                                                       sigma
                                                       start
                                                       finals
                                                       rules
                                                       accept
                                                       #f))]
        )
         
       [result tm?]))

(define make-mttm/c
  (->i ([states (and/c (is-a-list/c "machine states" "three")
                       (valid-listof/c valid-state? "machine state" "list of machine states" #:rule "three")
                       (no-duplicates/c "states" "three"))]
        [sigma (and/c (is-a-list/c "machine alphabet" "one")
                      (valid-listof/c valid-tm-alpha? "alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        [finals (states) (and/c (is-a-list/c "machine final states" "three")
                                (valid-listof/c valid-state? "machine state" "list of machine finals" #:rule "three")
                                (valid-finals/c states)
                                (no-duplicates/c "final states" "three"))]
        [rules (states
                sigma
                num-tapes) (and/c (is-a-list/c "machine rules" "four")
                                  (correct-mttm-rule-structures/c num-tapes)
                                  (correct-mttm-rules/c states sigma)
                                  (no-duplicates/c "rules" "four"))]
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
                                           (words-in-sigma-tm/c (append (list BLANK LM) sigma) 'accept)
                                           (mttm-input/c states
                                                         sigma
                                                         start
                                                         finals
                                                         rules
                                                         num-tapes
                                                         accept
                                                         #t)
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
                                           (words-in-sigma-tm/c (append (list BLANK LM) sigma) 'reject)
                                           (mttm-input/c states
                                                         sigma
                                                         start
                                                         finals
                                                         rules
                                                         num-tapes
                                                         accept
                                                         #f))]
        )
       [result mttm?]
       ))