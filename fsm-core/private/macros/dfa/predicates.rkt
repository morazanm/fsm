(module predicates racket
  (require
    "../../fsa.rkt"
    )
  (provide
   check-duplicates-dfa
   check-input-dfa
   check-input-ndfa
   correct-members-dfa?
   functional?
   valid-dfa-rule?
   )

  (define (check-duplicates-dfa rules)
    (define starts (map (lambda (x) (list (car x) (cadr x))) rules))
    (define (helper input acc)
      (cond [(empty? input) acc]
            [(member (car input) (cdr input)) (helper (cdr input) (cons (car input) acc))]
            [else (helper (cdr input) acc)])
      )
    (define duplicates (helper starts '()))
    (if (empty? duplicates) #f duplicates)
    )

  (define (check-input-dfa states
                           sigma
                           start
                           finals
                           rules
                           add-dead
                           accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-dfa states
                                               sigma
                                               start
                                               finals
                                               rules
                                               add-dead))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  (define (check-input-ndfa states
                            sigma
                            start
                            finals
                            rules
                            accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-ndfa states
                                                sigma
                                                start
                                                finals
                                                rules))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  (define (correct-members-dfa? states sigma rules)
    (andmap (lambda (x) (and (member (car x) states)
                             (member (cadr x) sigma)
                             (member (caddr x) states))) rules))

  ;; Determines if the set of rules is a function, based on the machine states
  ;; and alphabet (sigma). The machine must either automatically add dead states
  ;; (add-dead is true), or there must exist a rule with a starting state and
  ;; alphabet letter for every pair of state and letters in the alphabet sigma.
  (define (functional? rules states sigma add-dead)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (or add-dead (andmap (lambda (x) (member x pairs)) cart-prod))
    )

  (define (valid-dfa-rule? rule)
    (and (list? rule)
         (= (length rule) 3))
    )
  )