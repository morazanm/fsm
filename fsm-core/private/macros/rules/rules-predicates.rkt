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
           valid-tm-rule?
           functional?
           missing-functional
           check-duplicates-dfa
           correct-members-dfa?
           correct-members-tm?
           incorrect-members-dfa
           incorrect-members-tm
           valid-ndpda-rule?
           valid-ndpda-push
           valid-ndpda-pop
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
  (define (valid-tm-rule? rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (car rule)) 2)
         (= (length (cadr rule)) 2))
    )

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

  (define (correct-members-tm? states sigma rules)
    (andmap (lambda (x) (and (member (car (car x)) states)
                             (member (cadr (car x)) sigma)
                             (member (car (cadr x)) states)
                             (member (cadr (cadr x)) sigma))) rules))

  (define (incorrect-members-dfa states sigma rules)
    (filter (lambda (x) (not (and (member (car x) states)
                                  (member (cadr x) sigma)
                                  (member (caddr x) states)))) rules))

  (define (incorrect-members-tm states sigma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) sigma)
                                  (member (car (cadr x)) states)
                                  (member (cadr (cadr x)) sigma)))) rules))

  ;; Purpose: Checks if the given rule is valid, according to the states,
  ;; alphabet, and gamma. An ndpda-rule is valid if it meets the following
  ;; criteria:
  ;; 1. It is a list with two elements
  ;; 2. The first element is a valid "pop" rule fragment
  ;; 3. The second element is a valid "push" rule fragment.
  ;; See valid-ndpda-pop and valid-ndpda-push for specifics on "pop" and "push"
  ;; fragments.
  (define ((valid-ndpda-rule? states alphabet gamma) rule)
    (and (list? rule)
         (= (length rule) 2)
         (valid-ndpda-pop (first rule) states alphabet gamma)
         (valid-ndpda-push (second rule) states gamma)))

  ;; Purpose: Checks if the given input is a valid ndpda pop fragment
  ;; In order to be valid, it must:
  ;; 1. Be a list with three elements
  ;; 2. The first element must be a state from the list of states
  ;; 3. The second element must be a letter from the alphabet
  ;; 4. The third element must either be the EMP constant, or a list of symbols,
  ;;    all of which must be in the gamma.
  (define (valid-ndpda-pop fragment states alphabet gamma)
    (and (list? fragment)
         (= (length fragment) 3)
         (member (first fragment) states)
         (or (member (second fragment) alphabet) (equal? (second fragment) EMP))
         (or (equal? (third fragment) EMP)
             (and (list? (third fragment))
                  (not (empty? (third fragment)))
                  (andmap (lambda (sym) (member sym gamma)) (third fragment)))))
    )

  ;; Purpose: Checks to see if the given input is a valid ndpda push fragment
  ;; In order to be valid, it must:
  ;; 1. Be a list with two elements
  ;; 2. The first element must be a state frmo the list of states
  ;; 3. The second element must either be the EMP constant, or a list of symbols,
  ;;    all of which must be in the gamma.
  (define (valid-ndpda-push fragment states gamma)
    (and (list? fragment)
         (= (length fragment) 2)
         (member (first fragment) states)
         (or (equal? (second fragment) EMP)
             (and (list? (second fragment))
                  (not (empty? (second fragment)))
                  (andmap (lambda (sym) (member sym gamma)) (second fragment))))))


  
  )