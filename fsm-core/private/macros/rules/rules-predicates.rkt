(module predicates racket
  (require racket/contract
           "../../constants.rkt"
           "../../sm-getters.rkt"
           "../../fsa.rkt"
           "../../tm.rkt"
           "../../pda.rkt"
           "../../../../main.rkt")
  (provide add-dead-state-rules
           valid-rules?
           invalid-rules
           valid-dfa-rule?
           valid-ndpda-rule?
           valid-tm-rule?
           valid-mttm-rule?
           valid-mttm-rule-structure?
           functional?
           missing-functional
           check-duplicates-dfa
           correct-members-dfa?
           correct-members-tm?
           correct-members-mttm?
           correct-members-ndpda?
           incorrect-members-dfa
           incorrect-members-tm
           incorrect-members-mttm
           incorrect-members-ndpda
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

  (define (valid-rules? pred rules)
    (andmap (lambda (rule) (pred rule)) rules))

  (define (invalid-rules pred rules)
    (filter (lambda (rule) (not (pred rule)))
            rules)
    )

  (define (valid-dfa-rule? rule)
    (and (list? rule)
         (= (length rule) 3))
    )

  (define (valid-ndpda-rule? rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (car rule)) 3)
         (= (length (cadr rule)) 2)))
  
  (define (valid-tm-rule? rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (car rule)) 2)
         (= (length (cadr rule)) 2))
    )

  (define ((valid-mttm-rule-structure? num-tapes) rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (first rule)) 2)
         (= (length (second rule)) 2)
         (= (length (second (first rule))) num-tapes)
         (= (length (second (second rule))) num-tapes)))

  ;; Determines if the set of rules is a function, based on the machine states
  ;; and alphabet (sigma). The machine must either automatically add dead states
  ;; (add-dead is true), or there must exist a rule with a starting state and
  ;; alphabet letter for every pair of state and letters in the alphabet sigma.
  (define (functional? rules states sigma add-dead)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (or add-dead (andmap (lambda (x) (member x pairs)) cart-prod))
    )

  (define (missing-functional rules states sigma)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (filter (lambda (x) (not (member x pairs))) cart-prod)
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

  (define (correct-members-dfa? states sigma rules)
    (andmap (lambda (x) (and (member (car x) states)
                             (member (cadr x) sigma)
                             (member (caddr x) states))) rules))

  (define (correct-members-ndpda? states sigma gamma rules)
    (andmap (lambda (x) (and (member (car (car x)) states)
                             (member (cadr (car x)) (cons EMP sigma))
                             (or (equal? (caddr (car x)) EMP)
                                 (map (lambda (y) (member y gamma)) (caddr (car x))))
                             (member (car (cadr x)) states)
                             (or (equal? (cadr (cadr x)) EMP)
                                 (map (lambda (y) (member y gamma)) (cadr (cadr x))))
                             )) rules))

  (define (correct-members-tm? states sigma rules)
    (andmap (lambda (x) (and (member (car (car x)) states)
                             (member (cadr (car x)) sigma)
                             (member (car (cadr x)) states)
                             (member (cadr (cadr x)) sigma))) rules))

  (define (valid-mttm-rule? states sigma rule)
    (and (member (first (first rule)) states)
         (andmap (lambda (letter) (member letter sigma)) (second (first rule)))
         (member (first (second rule)) states)
         (andmap (lambda (letter) (member letter sigma)) (second (second rule))))
    )

  (define (correct-members-mttm? states sigma rules)
    (andmap (lambda (rule) (valid-mttm-rule? states sigma rule)) rules))

  (define (incorrect-members-dfa states sigma rules)
    (filter (lambda (x) (not (and (member (car x) states)
                                  (member (cadr x) sigma)
                                  (member (caddr x) states)))) rules))
  (define (incorrect-members-ndpda states sigma gamma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) (cons EMP sigma))
                                  (or (equal? (caddr (car x)) EMP)
                                      (map (lambda (y) (member y gamma)) (caddr (car x))))
                                  (member (car (cadr x)) states)
                                  (or (equal? (cadr (cadr x)) EMP)
                                      (map (lambda (y) (member y gamma)) (cadr (cadr x)))))
                             )
              )
            rules))

  (define (incorrect-members-tm states sigma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) sigma)
                                  (member (car (cadr x)) states)
                                  (member (cadr (cadr x)) sigma)))) rules))

  (define (incorrect-members-mttm states sigma rules)
    (filter (lambda (rule) (not (valid-mttm-rule? states sigma rule))) rules))

  
  )