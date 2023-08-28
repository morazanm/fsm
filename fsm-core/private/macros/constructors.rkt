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
    (->i ([states (and/c (is-nonempty-list/c "machine state" "list of machine states")
                         (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-nonempty-list/c "alphabet letter" "machine sigma")
                        (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-nonempty-list/c "final state" "list of machine final states")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  add-dead) (and/c (listof-rules/c valid-dfa-rule-structure?)
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
                                               (listof-words/c sigma)
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
                                               (listof-words/c sigma)
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
    (->i ([states (and/c (is-nonempty-list/c "machine state" "list of machine states")
                         (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-nonempty-list/c "alphabet letter" "machine sigma")
                        (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-nonempty-list/c "final state" "list of machine final states")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma) (and/c (listof-rules/c valid-dfa-rule-structure?)
                                (correct-dfa-rules/c states (cons EMP sigma))
                                (no-duplicates/c "rules"))]
          )
         (#:accepts [accepts (states
                              sigma
                              start
                              finals
                              rules) (and/c (words-in-sigma/c sigma)
                                            (listof-words/c sigma)
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
                              rules) (and/c (words-in-sigma/c sigma)
                                            (listof-words/c sigma)
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
    (->i ([states (and/c (is-nonempty-list/c "machine state" "list of machine states")
                         (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-nonempty-list/c "alphabet letter" "machine sigma")
                        (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [gamma (and/c (valid-listof/c (lambda (g) (or (valid-state? g) (valid-alpha? g))) "stack symbol" "list of stack symbols")
                        (no-duplicates/c "gamma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-nonempty-list/c "final state" "list of machine final states")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  gamma) (and/c (listof-rules/c valid-ndpda-rule-structure?)
                                (correct-ndpda-rules/c states sigma gamma)
                                (no-duplicates/c "rules"))]
          )
         (#:accepts [accepts (states
                              sigma
                              gamma
                              start
                              finals
                              rules) (and/c (words-in-sigma/c sigma)
                                            (listof-words/c sigma)
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
                              rules) (and/c (words-in-sigma/c sigma)
                                            (listof-words/c sigma)
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
    (->i ([states (and/c (is-nonempty-list/c "machine state" "list of machine states")
                         (valid-listof/c valid-state? "machine state" "list of machine states")
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-nonempty-list/c "alphabet letter" "machine sigma")
                        (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [rules (states
                  sigma) (and/c (listof-rules/c valid-tm-rule-structure?)
                                (correct-tm-rules/c states sigma)
                                (no-duplicates/c "rules"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-nonempty-list/c "final state" "list of machine final states")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals")
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
    (->i ([states (and/c (is-nonempty-list/c "machine state" "list of machine states")
                         (valid-listof/c valid-state? "machine state" "list of machine states" #:rule 3)
                         (no-duplicates/c "states"))]
          [sigma (and/c (is-nonempty-list/c "alphabet letter" "machine-sigma")
                        (valid-listof/c valid-alpha? "alphabet letter" "machine sigma")
                        (no-duplicates/c "sigma"))]
          [start (states) (and/c (valid-start/c states)
                                 (start-in-states/c states))]
          [finals (states) (and/c (is-nonempty-list/c "final state" "list of machine final states")
                                  (valid-listof/c valid-state? "machine state" "list of machine finals")
                                  (valid-finals/c states)
                                  (no-duplicates/c "final states"))]
          [rules (states
                  sigma
                  num-tapes) (and/c (listof-rules/c (valid-mttm-rule-structure? num-tapes))
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

  (define EQABC (make-mttm2 '(S Y N C D E F g)
                            '(a b c)
                            'S
                            '(Y N)
                            (list ;; read all blanks and move all R
                             (list (list 'S (list BLANK BLANK BLANK BLANK))
                                   (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
                             ;; read a on t0, copy to t1 and then move R on t0 and t1
                             (list (list 'C (list 'a BLANK BLANK BLANK))
                                   (list 'D (list 'a 'a BLANK BLANK)))
                             (list (list 'D (list 'a 'a BLANK BLANK))
                                   (list 'C (list RIGHT RIGHT BLANK BLANK)))
                             ;; read b on t0, copy to t2 and then move R on t0 and t2
                             (list (list 'C (list 'b BLANK BLANK BLANK))
                                   (list 'E (list 'b BLANK 'b BLANK)))
                             (list (list 'E (list 'b BLANK 'b BLANK))
                                   (list 'C (list RIGHT BLANK RIGHT BLANK)))
                             ;; read c on t0, copy to t3 and then move R on t0 and t3
                             (list (list 'C (list 'c BLANK BLANK BLANK))
                                   (list 'F (list 'c BLANK BLANK 'c)))
                             (list (list 'F (list 'c BLANK BLANK 'c))
                                   (list 'C (list RIGHT BLANK BLANK RIGHT)))
                             ;; read BLANK on t0, move L on t1, t2 and t3
                             (list (list 'C (list BLANK BLANK BLANK BLANK))
                                   (list 'G (list BLANK LEFT LEFT LEFT)))
                             ;; read BLANK on all tapes, move to Y
                             (list (list 'G (list BLANK BLANK BLANK BLANK))
                                   (list 'Y (list BLANK BLANK BLANK BLANK)))
                             ;; read a, b, c on t1, t2, and t3 them move L on t1, t2, t3
                             (list (list 'G (list BLANK 'a 'b 'c))
                                   (list 'G (list BLANK LEFT LEFT LEFT)))
                             ;; too many of at least 1 letter
                             (list (list 'G (list BLANK BLANK 'b 'c))
                                   (list 'N (list BLANK BLANK 'b 'c)))
                             (list (list 'G (list BLANK 'a BLANK 'c))
                                   (list 'N (list BLANK 'a BLANK 'c)))
                             (list (list 'G (list BLANK 'a 'b BLANK))
                                   (list 'N (list BLANK 'a 'b BLANK)))
                             (list (list 'G (list BLANK BLANK BLANK 'c))
                                   (list 'N (list BLANK BLANK BLANK 'c)))
                             (list (list 'G (list BLANK BLANK 'b BLANK))
                                   (list 'N (list BLANK BLANK 'b BLANK)))
                             (list (list 'G (list BLANK 'a BLANK BLANK))
                                   (list 'N (list BLANK 'a BLANK BLANK))))
                            4
                            'Y))
  )
