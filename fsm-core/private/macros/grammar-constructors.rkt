(module constructors racket
  (require "rules/rules-flat-contracts.rkt"
           "shared/shared-flat-contracts.rkt"
           "validation/validation-flat-contracts.rkt"
           "../constants.rkt"
           "rules/rules-predicates.rkt"
           "shared/shared-predicates.rkt"
           "validation/validation-predicates.rkt"
           "error-formatting.rkt"
           "../grammar-getters.rkt"
           racket/contract
           )
  (provide make-cfg/c
           make-rg/c
           make-csg/c
           )

  (define make-rg/c
    (->i ([states (and/c (is-a-list/c "nonterminals" "three")
                      (valid-listof/c valid-state? "nonterminal" "list of machine nonterminals" #:rule "three")
                      (no-duplicates/c "nonterminals")
                      )]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [delta (states
                  sigma
                  start) (and/c (is-a-list/c "machine nonterminals" "four")
                                (lambda (x) (correct-grammar-rule-structures/c))
                                (no-emp-rhs/c start)
                                (correct-rg-rules/c states (cons EMP sigma))
                                (no-duplicates/c "rules"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          )
         (#:accepts [accepts (states
                              sigma
                              delta
                              start) (and/c (listof-words/c "accepts")
                                            (words-in-sigma/c sigma)
                                            (rg-input/c states
                                                        sigma
                                                        delta
                                                        start
                                                        #t))]
          #:rejects [rejects (states
                              sigma
                              delta
                              start) (and/c (listof-words/c "rejects")
                                            (words-in-sigma/c sigma)
                                            (rg-input/c states
                                                        sigma
                                                        delta
                                                        start
                                                        #f))]
          )


         
         [result (lambda (x) (equal? 'rg (grammar-type x)))]))

  (define make-cfg/c
    (->i ([states (and/c (is-a-list/c "nonterminals" "three")
                      (valid-listof/c valid-state? "nonterminal" "list of machine nonterminals" #:rule "three")
                      (no-duplicates/c "nonterminals")
                      )]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [delta (states
                  sigma) (and/c (is-a-list/c "machine nonterminals" "four")
                                (lambda (x) (correct-grammar-rule-structures/c))
                                (correct-cfg-rules/c states (cons EMP sigma))
                                (no-duplicates/c "rules"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          )
         (#:accepts [accepts (states
                              sigma
                              delta
                              start) (and/c (listof-words/c "accepts")
                                            (words-in-sigma/c sigma)
                                            (cfg-input/c states
                                                        sigma
                                                        delta
                                                        start
                                                        #t))]
          #:rejects [rejects (states
                              sigma
                              delta
                              start) (and/c (listof-words/c "rejects")
                                            (words-in-sigma/c sigma)
                                            (cfg-input/c states
                                                        sigma
                                                        delta
                                                        start
                                                        #f))]
          )


         
         [result (lambda (x) (equal? 'cfg (grammar-type x)))]))

  (define make-csg/c
    (->i ([states (and/c (is-a-list/c "nonterminals" "three")
                      (valid-listof/c valid-state? "nonterminal" "list of machine nonterminals" #:rule "three")
                      (no-duplicates/c "nonterminals")
                      )]
          [sigma (and/c (is-a-list/c "machine alphabet" "one")
                        (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                        (no-duplicates/c "sigma"))]
          [delta (states
                  sigma) (and/c (is-a-list/c "machine nonterminals" "four")
                                (lambda (x) (correct-grammar-rule-structures/c))
                                (correct-csg-rules/c states (cons EMP sigma))
                                (no-duplicates/c "rules"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          )
         (#:accepts [accepts (states
                              sigma
                              delta
                              start) (and/c (listof-words/c "accepts")
                                            (words-in-sigma/c sigma)
                                            (csg-input/c states
                                                        sigma
                                                        delta
                                                        start
                                                        #t))]
          #:rejects [rejects (states
                              sigma
                              delta
                              start) (and/c (listof-words/c "rejects")
                                            (words-in-sigma/c sigma)
                                            (csg-input/c states
                                                        sigma
                                                        delta
                                                        start
                                                        #f))]
          )


         
         [result (lambda (x) (equal? 'cfg (grammar-type x)))]))

  )
