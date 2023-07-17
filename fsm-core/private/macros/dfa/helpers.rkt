(module helpers racket
  (require
    "../../constants.rkt"
    "../../fsa.rkt"
    )
  (provide
   add-dead-state-rules
   incorrect-members-dfa
   missing-functional
   return-input-dfa
   return-input-ndfa
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

  (define (incorrect-members-dfa states sigma rules)
    (filter (lambda (x) (not (and (member (car x) states)
                                  (member (cadr x) sigma)
                                  (member (caddr x) states)))) rules))

  (define (missing-functional rules states sigma)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (filter (lambda (x) (not (member x pairs))) cart-prod)
    )

  (define (return-input-dfa states
                            sigma
                            start
                            finals
                            rules
                            add-dead
                            words
                            accepts?)
    (define temp-machine (make-unchecked-dfa states
                                             sigma
                                             start
                                             finals
                                             rules
                                             add-dead))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )

  (define (return-input-ndfa states
                             sigma
                             start
                             finals
                             rules
                             words
                             accepts?)
    (define temp-machine (make-unchecked-ndfa states
                                              sigma
                                              start
                                              finals
                                              rules))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )

  )
