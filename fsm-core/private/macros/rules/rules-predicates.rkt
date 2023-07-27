(module predicates racket
  (require racket/contract
           rackunit
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
           functional?
           missing-functional
           check-duplicates-dfa
           correct-members-dfa?
           correct-members-tm?
           correct-members-ndpda?
           incorrect-members-dfa
           incorrect-members-tm
           incorrect-members-ndpda
           )

  ;add-dead-state-rules: (listof dfa-rules) (listof states) sigma --> (listof dfa-rules)
  ;purpose: to generate the implicit rules that go to the deadstate from a
  ; given list of rules, states, and a sigma. Any pairings of a state and sigma
  ; value that are not included in the rules explicitely, must be added as
  ; going to the dead state by this function
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

  ;valid-rules?: (something --> boolean) (listof something) --> boolean
  ;purpose: takes in a predicate function and a listof something (hopefully
  ; rules) and returns whether or not all those rules follow the predicate
  ; given.
  (define (valid-rules? pred rules)
    (andmap (lambda (rule) (pred rule)) rules))

  ;invalid-rules: (something --> boolean) (listof something) --> listof something
  ;purpose: to take a predicate and apply it to all members of a list,
  ; returning the list of things that fail the predicate
  (define (invalid-rules pred rules)
    (filter (lambda (rule) (not (pred rule)))
            rules)
    )

  ;valid-dfa-rule: something --> boolean
  ;purpose: just checks if the rule list is formatted the correct way
  ; for a dfa, this is a list of three
  (define (valid-dfa-rule? rule)
    (and (list? rule)
         (= (length rule) 3))
    )

  ;valid-ndpda-rule: something --> boolean
  ;purpose: just checks if the rule list is formatted the correct way
  ; for a ndpda, this is a list of two lists, one of 3 and one of 2
  (define (valid-ndpda-rule? rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (car rule)) 3)
         (= (length (cadr rule)) 2)
         (or (list? (caddr (car rule))) (equal? (caddr (car rule)) EMP))
         (or (list? (cadr (cadr rule))) (equal? (cadr (cadr rule)) EMP))
         ))

  ;valid-tm-rule: something --> boolean
  ;purpose: just checks if the rule list is formatted the correct way
  ; for a tm, this is a list of two lists of length 2
  (define (valid-tm-rule? rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (car rule)) 2)
         (= (length (cadr rule)) 2))
    )

  ;functional?: (listof dfa-rules) (listof states) sigma boolean --> boolean
  ;purpose: to check if every pairing of state and alphabet letter exists in
  ; the rules. The exception to this is if add-dead is true. Then it will
  ; automatically become functional later
  (define (functional? rules states sigma add-dead)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (or add-dead (if (andmap (lambda (x) (member x pairs)) cart-prod) #t #f))
    )

  ;missing-functional: (listof dfa-rules) (listof states) sigma --> boolean
  ;purpose: returns all the state/alphabet letter pairs that do not
  ; already exist in the list of rules
  (define (missing-functional rules states sigma)
    (define pairs (map (lambda (x) (list (first x) (second x))) rules))
    (define cart-prod (cartesian-product states sigma))
    (filter (lambda (x) (not (member x pairs))) cart-prod)
    )

  ;check-duplicates-dfa: (listof dfa-rules) --> boolean/(listof state/sigma pairs)
  ;purpose: to ensure that there's only one rule per state/sigma pair
  ; in the rules to ensure functionality. Returns false if there are no duplicates
  ; and a list of duplicated state/sigma pairs if there are duplicates
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

  ;correct-members-dfa?: (listof states) sigma (listof dfa-rules) --> boolean
  ;purpose: to check that the first and last element are from the states,
  ; and the second element of each rule are from the alphabet
  (define (correct-members-dfa? states sigma rules)
    (if (andmap (lambda (x) (and (member (car x) states)
                                 (member (cadr x) sigma)
                                 (member (caddr x) states))) rules) #t #f))

  ;correct-members-ndpda?: (listof states) sigma gamma (listof ndpda-rules) --> boolean
  ;purpose: to check and make sure that
  ; the first member of each list in the ndpda rule is from the states
  ; the second member of the first list is from the sigma
  ; the last element in both lists is a list of gamma elements OR EMP
  (define (correct-members-ndpda? states sigma gamma rules)
    (if (andmap (lambda (x) (and (member (car (car x)) states)
                                 (member (cadr (car x)) (cons EMP sigma))
                                 (or (equal? (caddr (car x)) EMP)
                                     (andmap (lambda (y) (member y gamma)) (caddr (car x))))
                                 (member (car (cadr x)) states)
                                 (or (equal? (cadr (cadr x)) EMP)
                                     (andmap (lambda (y) (member y gamma)) (cadr (cadr x))))
                                 )) rules) #t #f))

  ;correct-members-tm?: (list of states) (sigma) (listof tm-rules) --> boolean
  ;purpose: to make sure that the first member of each list in the tm-rule is
  ; a state from the list of states, and that the last member of each of those
  ; lists is from the sigma. Except in the last member of the last list,
  ; which can also be a turing machine action.
  (define (correct-members-tm? states sigma rules)
    (if (andmap (lambda (x) (and (member (car (car x)) states)
                                 (member (cadr (car x)) (cons BLANK sigma))
                                 (member (car (cadr x)) states)
                                 (member (cadr (cadr x)) (cons RIGHT
                                                               (cons LEFT
                                                                     (cons BLANK sigma))))
                                 ))
                rules) #t #f))

  ;incorrect-members-dfa (listof states) sigma (listof dfa-rules) --> listof dfa rules
  ;purpose: return all rules containing incorrect elements
  (define (incorrect-members-dfa states sigma rules)
    (filter (lambda (x) (not (and (member (car x) states)
                                  (member (cadr x) sigma)
                                  (member (caddr x) states)))) rules))

  ;incorrect-members-ndpda (listof states) sigma gamma (listof ndpda-rules) --> listof ndpdarules
  ;purpose: return all rules containing incorrect elements
  (define (incorrect-members-ndpda states sigma gamma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) (cons EMP sigma))
                                  (or (equal? (caddr (car x)) EMP)
                                      (andmap (lambda (y) (member y gamma)) (caddr (car x))))
                                  (member (car (cadr x)) states)
                                  (or (equal? (cadr (cadr x)) EMP)
                                      (andmap (lambda (y) (member y gamma)) (cadr (cadr x)))))
                             )
              )
            rules))

  ;incorrect-members-tm (listof states) sigma gamma (listof-tm-rules) --> listof turing machine rules
  ;purpose: return all rules containing incorrect elements
  (define (incorrect-members-tm states sigma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) sigma)
                                  (member (car (cadr x)) states)
                                  (member (cadr (cadr x)) (cons RIGHT
                                                                (cons LEFT
                                                                      (cons BLANK sigma))))))) rules))

  (module+ test
    ;add-dead-state-rules tests
    (check-equal? (add-dead-state-rules `((A a B) (A b B)) '(A B) '(a b))
                  `((A a B) (A b B) (B a ,DEAD) (B b ,DEAD)))
    (check-equal? (add-dead-state-rules `((A a B) (A b B)) '(A B C) '(a b))
                  `((A a B) (A b B) (B a ,DEAD) (B b ,DEAD) (C a ,DEAD) (C b ,DEAD)))
    
    ;valid-rules? tests
    (check-equal? (valid-rules? (lambda (x) (symbol? x)) '(A B C D)) #t)
    (check-equal? (valid-rules? (lambda (x) (symbol? x)) `(A (B) C D)) #f)
    
    ;invalid-rules tests
    (check-equal? (invalid-rules (lambda (x) (symbol? x)) '(A B C D)) '())
    (check-equal? (invalid-rules (lambda (x) (symbol? x)) `(A (B) C D)) `((B)))
    
    ;valid-dfa-rule? tests
    (check-equal? (valid-dfa-rule? '(A a B)) #t)
    (check-equal? (valid-dfa-rule? '(A a a B)) #f)
    (check-equal? (valid-dfa-rule? '(a a a)) #t)
    (check-equal? (valid-dfa-rule? `((A) b b)) #t)
    
    ;valid-ndpda-rule? tests
    (check-equal? (valid-ndpda-rule? `((A a (B)) (A (b)))) #t)
    (check-equal? (valid-ndpda-rule? `((a a (a)) (1 (b)))) #t)
    (check-equal? (valid-ndpda-rule? `((1 1 a) (A (b)))) #f)
    (check-equal? (valid-ndpda-rule? `((1 1 (a)) (A ))) #f)
    (check-equal? (valid-ndpda-rule? `((A a) (A b))) #f)
    
    ;valid-tm-rule? tests
    (check-equal? (valid-tm-rule? `((A a) (A b))) #t)
    (check-equal? (valid-tm-rule? `(((A) a) (A 1))) #t)
    (check-equal? (valid-tm-rule? `((A a (A b)))) #f)
    (check-equal? (valid-tm-rule? `(a a a)) #f)

    ;functional? tests
    (check-equal? (functional? `((A a B) (A b B)) '(A B) '(a b) #t) #t)
    (check-equal? (functional? `((A a B) (A b B)) '(A B) '(a b) #f) #f)
    (check-equal? (functional? `((A a B) (A b B) (B a B) (B b A)) '(A B) '(a b) #f) #t)


    ;missing-functional tests
    (check-equal? (missing-functional `((A a B) (A b B)) '(A B) '(a b)) `((B a) (B b)))
    (check-equal? (missing-functional `((A a B) (A b B) (B a B) (B b A)) '(A B) '(a b)) '())
    
    ;check-duplicates-dfa tests
    (check-equal? (check-duplicates-dfa `((A a B) (A b B) (B a B) (B b A))) #f)
    (check-equal? (check-duplicates-dfa `((A a B) (A b B) (B a B) (B b A) (B b B))) `((B b)))
    
    ;correct-members-dfa? tests
    (check-equal? (correct-members-dfa? '(A B) '(a b) `((A b B) (B a A))) #t)
    (check-equal? (correct-members-dfa? '(A B) '(a b) `()) #t)
    (check-equal? (correct-members-dfa? '(A B) '(a b) `((A b B) (B a A) (C b A))) #f)
    
    ;correct-members-ndpda? tests
    (check-equal? (correct-members-ndpda? '(A B)
                                          '(a b)
                                          '(c d)
                                          `(((A b (c)) (B (d))))
                                          ) #t)
    (check-equal? (correct-members-ndpda? '(A B)
                                          '(a b)
                                          '(c d)
                                          `(((C b (c)) (B (d))))
                                          ) #f)
    (check-equal? (correct-members-ndpda? '(A B)
                                          '(a b)
                                          '(c d)
                                          `(((A c (c)) (B (d))))
                                          ) #f)
    (check-equal? (correct-members-ndpda? '(A B)
                                          '(a b)
                                          '(c d)
                                          `(((A b (b)) (B (d))))
                                          ) #f)
    (check-equal? (correct-members-ndpda? '(A B)
                                          '(a b)
                                          '(c d)
                                          `(((A b (c)) (C (d))))
                                          ) #f)
    (check-equal? (correct-members-ndpda? '(A B)
                                          '(a b)
                                          '(c d)
                                          `(((A b (c)) (B (a))))
                                          ) #f)

    ;correct-members-tm? tests
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((A b) (B b)))) #t)
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((A b) (B ,RIGHT)))) #t)
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((A b) (B ,LEFT)))) #t)
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((A b) (B ,BLANK)))) #t)
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((A c) (B ,BLANK)))) #f)
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((C b) (B ,BLANK)))) #f)
    (check-equal? (correct-members-tm? '(A B)
                                       '(a b)
                                       `(((A c) (C ,BLANK)))) #f)
    
    ;incorrect-members-dfa tests
    (check-equal? (incorrect-members-dfa '(A B) '(a b) `((A b B) (B a A))) '())
    (check-equal? (incorrect-members-dfa '(A B) '(a b) `()) '())
    (check-equal? (incorrect-members-dfa '(A B) '(a b) `((A b B) (B a A) (C b A))) '((C b A)))
    
    ;incorrect-members-ndpda tests
    (check-equal? (incorrect-members-ndpda '(A B)
                                           '(a b)
                                           '(c d)
                                           `(((A b (c)) (B (d))))
                                           ) '())
    (check-equal? (incorrect-members-ndpda '(A B)
                                           '(a b)
                                           '(c d)
                                           `(((A b (c)) (B (d)))
                                             ((C b (c)) (B (d))))
                                           ) `(((C b (c)) (B (d)))))
    (check-equal? (incorrect-members-ndpda '(A B)
                                           '(a b)
                                           '(c d)
                                           `(((A b (c)) (B (d)))
                                             ((A c (c)) (B (d))))
                                           ) `(((A c (c)) (B (d)))))
    (check-equal? (incorrect-members-ndpda '(A B)
                                           '(a b)
                                           '(c d)
                                           `(((A b (c)) (B (d)))
                                             ((A b (b)) (B (d))))
                                           ) `(((A b (b)) (B (d)))))
    (check-equal? (incorrect-members-ndpda '(A B)
                                           '(a b)
                                           '(c d)
                                           `(((A b (c)) (B (d)))
                                             ((A b (c)) (C (d))))
                                           ) `(((A b (c)) (C (d)))))
    (check-equal? (incorrect-members-ndpda '(A B)
                                           '(a b)
                                           '(c d)
                                           `(((A b (c)) (B (d)))
                                             ((A b (c)) (B (a))))
                                           ) `(((A b (c)) (B (a)))))
    
    ;incorrect-members-tm tests
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B b)))) '())
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,RIGHT)))) '())
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,LEFT)))) '())
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,BLANK)))) '())
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,BLANK))
                                         ((A c) (B ,BLANK)))) `(((A c) (B ,BLANK))))
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,BLANK))
                                         ((C b) (B ,BLANK)))) `(((C b) (B ,BLANK))))
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,BLANK))
                                         ((A c) (C ,BLANK)))) `(((A c) (C ,BLANK))))
    (check-equal? (incorrect-members-tm '(A B)
                                       '(a b)
                                       `(((A b) (B ,BLANK))
                                         ((A c) (B d)))) `(((A c) (B d))))
           
    )

  
  )