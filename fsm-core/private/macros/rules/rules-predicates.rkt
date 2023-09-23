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
           incorrect-dfa-rules
           incorrect-ndpda-rules
           incorrect-tm-rules
           incorrect-mttm-rules
           valid-dfa-rule-structure?
           valid-ndpda-rule-structure?
           valid-tm-rule-structure?
           valid-mttm-rule?
           valid-mttm-rule-structure?
           functional?
           missing-functional
           make-invalid-rule
           invalid-rule-rule
           invalid-rule-errors
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

  ;true?: any -> boolean
  ;purpose: returns true if the given value is not false, and false if it is.
  (define (true? x) (not (not x)))

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

  ;valid-dfa-rule: any --> boolean
  ;purpose: just checks if the rule list is formatted the correct way
  ; for a dfa, this is a list of three
  (define (valid-dfa-rule-structure? rule)
    (and (list? rule)
         (= (length rule) 3))
    )

  ;valid-ndpda-rule: something --> boolean
  ;purpose: just checks if the rule list is formatted the correct way
  ; for a ndpda, this is a list of two lists, one of 3 and one of 2
  (define (valid-ndpda-rule-structure? rule)
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
  (define (valid-tm-rule-structure? rule)
    (and (list? rule)
         (= (length rule) 2)
         (list? (first rule))
         (= (length (first rule)) 2)
         (list? (second rule))
         (= (length (second rule)) 2))
    )

  ;valid-mttm-rule-structure?: integer --> (mttm-rule --> boolean)
  ;purpose: takes in an integer representing the number of tapes on a multi-tape
  ; tm, and returns a function that takes a mttm-rule and checks if the given
  ; rule has the correct structure.
  (define ((valid-mttm-rule-structure? num-tapes) rule)
    (and (list? rule)
         (= (length rule) 2)
         (list? (first rule))
         (= (length (first rule)) 2)
         (list? (second rule))
         (= (length (second rule)) 2)
         (list? (second (first rule)))
         (= (length (second (first rule))) num-tapes)
         (list? (second (second rule)))
         (= (length (second (second rule))) num-tapes)))


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
                                 (member (cadr (car x)) (cons BLANK (cons LM sigma)))
                                 (member (car (cadr x)) states)
                                 (member (cadr (cadr x)) (cons RIGHT
                                                               (cons LEFT
                                                                     (cons BLANK sigma))))
                                 ))
                rules) #t #f))

  ;valid-mttm-rule?: (listof state) (listof alpha) mttm-rule --> boolean
  ;purpose: checks to see if the given multi-tape tm rule is valid, when compared
  ; against the given states and sigma. A rule is invalid if either of its states
  ; is not in the list of states, or any of the symbols/actions in the rule are
  ; not in the sigma.
  (define (valid-mttm-rule? states sigma rule)
    (define actions (cons RIGHT (cons LEFT (cons BLANK sigma))))
    (and (member (first (first rule)) states)
         (andmap (lambda (letter) (member letter (cons LM (cons BLANK sigma)))) (second (first rule)))
         (member (first (second rule)) states)
         (andmap (lambda (letter) (member letter actions)) (second (second rule))))
    )

  ;correct-members-mttm?: (listof state) (listof alpha) (listof mttm-rule) --> boolean
  ;purpose: checks to see if all rules in the list of rules is valid. Returns
  ; false if any rules are invalid.
  (define (correct-members-mttm? states sigma rules)
    (not (not (andmap (lambda (rule) (valid-mttm-rule? states sigma rule)) rules))))

  ;incorrect-members-dfa (listof states) sigma (listof dfa-rules) --> listof dfa rules
  ;purpose: return all rules containing incorrect elements
  (define (incorrect-members-dfa states sigma rules)
    (filter (lambda (x) (not (and (member (car x) states)
                                  (member (cadr x) sigma)
                                  (member (caddr x) states)))) rules))

  (define-struct invalid-rule (rule errors) #:transparent)

  ;incorrect-dfa-rules: (listof state) (listof alpha) (listof dfa-rule) --> (listof (list (dfa-rule (list boolean boolean boolean)))
  ;purpose: for all the rules in the list of dfa-rules, returns tuples of all
  ; states and alphabet letters that are invalid (meaning not in the states or sigma)
  (define (incorrect-dfa-rules states sigma rules)
    ;rule-with-errors: dfa-rule --> (listof invalid-rule)
    ;Purpose: If there is anything wrong with the give rule (see possible errors
    ; in above purpose), then returns a list containing an invalid-rule with all
    ; appropriate error messages for that rule. If the rule is valid, returns
    ; an empty list.
    (define (rule-with-errors rule)
      (define from-state (first rule))
      (define consumed (second rule))
      (define to-state (third rule))
      (define all-errors
        (append (if (not (member from-state states))
                    (list (format "The from state, ~a, is not in the list of states." from-state))
                    '())
                (if (not (member consumed sigma))
                    (list (format "The consumed letter, ~a, is not in the input alphabet." consumed))
                    '())
                (if (not (member to-state states))
                    (list (format "The to state, ~a, is not in the list of states." to-state))
                    '())))
      (if (empty? all-errors) '() (list (make-invalid-rule rule all-errors))))
    (flatten (map rule-with-errors rules)))

  ;with-indices: (listof x) --> (listof (list x natnum))
  ;Purpose: Returns the input list, but each element is paired with its index
  ; in the list.
  (define (with-indices lox)
    (map list lox (range (length lox))))

  ;incorrect-ndpda-rules: (listof state) (listof alpha) (listof symbol) (listof pda-rule) --> (listof invalid-rule)
  ;purpose: for all of the rules in the list of pda-rules, returns invalid-rule for any
  ; rule that has invalid states, alpha characters, or gamma elements.
  (define (incorrect-ndpda-rules states sigma gamma rules)
    ;rule-with-errors: pda-rule --> (listof invalid-rule)
    ;Purpose: If there is anything wrong with the give rule (see possible errors
    ; in above purpose), then returns a list containing an invalid-rule with all
    ; appropriate error messages for that rule. If the rule is valid, returns
    ; an empty list.
    (define (rule-with-errors rule)
      (define rule-pop (first rule))
      (define rule-push (second rule))
      (define pop-errors
        (let [(state (first rule-pop))
              (consumed (second rule-pop))
              (pop-elems (third rule-pop))]
          (append (if (not (member state states))
                      (list (format "The from state, ~a, is not in the list of states." state))
                      '())
                  (if (not (member consumed (cons EMP sigma)))
                      (list (format "The letter ~a is not in the input alphabet." consumed))
                      '())
                  (if (equal? pop-elems EMP)
                      '()
                      (map (lambda (x) (format "The ~a at index ~a of the pop list is not in the stack alphabet." (first x) (second x)))
                           (filter (lambda (p) (not (member (first p) gamma))) (with-indices pop-elems))))))
        )
      (define push-errors
        (let [(state (first rule-push))
              (push-elems (second rule-push))]
          (append (if (not (member state states))
                      (list (format "The to state, ~a, is not in the list of states." state))
                      '())
                  (if (equal? push-elems EMP)
                      '()
                      (map (lambda (x) (format "The ~a at index ~a of the push list is not in the stack alphabet." (first x) (second x)))
                           (filter (lambda (p) (not (member (first p) gamma))) (with-indices push-elems))))))
        )
      (define all-errors (append pop-errors push-errors))
      (if (empty? all-errors) '() (list (make-invalid-rule rule all-errors))))
    (flatten (map rule-with-errors rules)))

  ;incorrect-tm-rules: (listof state) (listof alpha) (listof tm-rule) --> (listof invalid-rule)
  ;Purpose: For all of the rules in the list of tm-rules, returns an invalid-rule
  ; for any rule that has invalid states, a read element that is not in the sigma
  ; or is BLANK/LM, or a tm-action that is not in the sigma or the LEFT/RIGHT constants.
  (define (incorrect-tm-rules states sigma rules)
    ;rule-with-errors: tm-rule --> (listof invalid-rule)
    ;Purpose: If there is anything wrong with the give rule (see possible errors
    ; in above purpose), then returns a list containing an invalid-rule with all
    ; appropriate error messages for that rule. If the rule is valid, returns
    ; an empty list.
    (define (rule-with-errors rule)
      (define rule-from (first rule))
      (define rule-to (second rule))
      (define from-errors
        (append (if (not (member (first rule-from) states))
                    (list (format "The from state, ~a, is not in the list of states." (first rule-from)))
                    '())
                (if (not (member (second rule-from) (cons BLANK (cons LM sigma))))
                    (list (format "The read symbol, ~a, must be in the input alphabet, BLANK, or LM." (second rule-from)))
                    '())))
      (define to-errors
        (append (if (not (member (first rule-to) states))
                    (list (format "The to state, ~a, is not in the list of states." (first rule-to)))
                    '())
                (if (not (member (second rule-to) (cons RIGHT (cons LEFT (cons BLANK sigma)))))
                    (list (format "The action ~a must be in the input alphabet, LEFT, RIGHT, or BLANK." (second rule-to)))
                    '())))
      (define all-errors (append from-errors to-errors))
      (if (empty? all-errors) '() (list (make-invalid-rule rule all-errors))))
    (flatten (map rule-with-errors rules)))

  ;incorrect-mttm-rules: (listof state) (listof alpha) (listof mttm-rule) --> (listof invalid-rule)
  ;Purpose: For all of the rules in the list of mttm-rules, returns an invalid-rule
  ; for any rule that has invalid states, any read elements that are not in the
  ; sigma or are BLANK/LM, or any tm actions that are not in the sigma or are the
  ; LEFT/RIGHT constants.
  (define (incorrect-mttm-rules states sigma rules)
    ;rule-with-errors: tm-rule --> (listof invalid-rule)
    ;Purpose: If there is anything wrong with the give rule (see possible errors
    ; in above purpose), then returns a list containing an invalid-rule with all
    ; appropriate error messages for that rule. If the rule is valid, returns
    ; an empty list.
    (define (rule-with-errors rule)
      (define rule-from (first rule))
      (define rule-to (second rule))
      (define from-errors
        (let [(state (first rule-from))
              (tape-reads (second rule-from))]
          (append (if (not (member state states))
                      (list (format "The from state, ~a, is not in the list of states." state))
                      '())
                  (map (lambda (x)
                         (format "The read symbol, ~a, on tape ~a must be in the input alphabet, BLANK, or LM."
                                 (first x)
                                 (second x)))
                       (filter (lambda (r) (not (member (first r) (cons BLANK (cons LM sigma))))) (with-indices tape-reads)))))
        )
      (define to-errors
        (let [(state (first rule-to))
              (tm-actions (second rule-to))]
          (append (if (not (member state states))
                      (list (format "The to state, ~a, is not in the list of states." state))
                      '())
                  (map (lambda (x)
                         (format "The action ~a on tape ~a must be in the input alphabet, LEFT, RIGHT, or BLANK."
                                 (first x)
                                 (second x)))
                       (filter (lambda (r) (not (member (first r) (cons RIGHT (cons LEFT (cons BLANK sigma)))))) (with-indices tm-actions)))))
        )
      (define all-errors (append from-errors to-errors))
      (if (empty? all-errors) '() (list (make-invalid-rule rule all-errors))))
    (flatten (map rule-with-errors rules)))

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

  ;incorrect-members-tm (listof states) sigma (listof-tm-rules) --> listof turing machine rules
  ;purpose: return all rules containing incorrect elements
  (define (incorrect-members-tm states sigma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) (cons BLANK (cons LM sigma)))
                                  (member (car (cadr x)) states)
                                  (member (cadr (cadr x)) (cons RIGHT
                                                                (cons LEFT
                                                                      (cons BLANK sigma))))))) rules))

  ;incorrect-members-mttm: (listof state) (listof alpha) (listof mttm-rule) --> (listof mttm-rule)
  ;purpose: returns all rules in the list of rules containing incorrect elements.
  ; Incorrect elements can be either unexpected states or alphabet characters.
  (define (incorrect-members-mttm states sigma rules)
    (filter (lambda (rule) (not (valid-mttm-rule? states sigma rule))) rules))

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
    
    ;valid-dfa-rule-structure? tests
    (check-equal? (valid-dfa-rule-structure? '(A a B)) #t)
    (check-equal? (valid-dfa-rule-structure? '(A a a B)) #f)
    (check-equal? (valid-dfa-rule-structure? '(a a a)) #t)
    (check-equal? (valid-dfa-rule-structure? `((A) b b)) #t)
    
    ;valid-ndpda-rule-structure? tests
    (check-equal? (valid-ndpda-rule-structure? `((A a (B)) (A (b)))) #t)
    (check-equal? (valid-ndpda-rule-structure? `((a a (a)) (1 (b)))) #t)
    (check-equal? (valid-ndpda-rule-structure? `((1 1 a) (A (b)))) #f)
    (check-equal? (valid-ndpda-rule-structure? `((1 1 (a)) (A ))) #f)
    (check-equal? (valid-ndpda-rule-structure? `((A a) (A b))) #f)
    
    ;valid-tm-rule-structure? tests
    (check-equal? (valid-tm-rule-structure? `((A a) (A b))) #t)
    (check-equal? (valid-tm-rule-structure? `(((A) a) (A 1))) #t)
    (check-equal? (valid-tm-rule-structure? `((A a (A b)))) #f)
    (check-equal? (valid-tm-rule-structure? `(a a a)) #f)

    ;valid-mttm-rule-structure? tests
    (check-equal? ((valid-mttm-rule-structure? 3) `((A (a b ,BLANK)) (B (,RIGHT ,BLANK b)))) #t)
    (check-equal? ((valid-mttm-rule-structure? 2) `(a b)) #f)
    (check-equal? ((valid-mttm-rule-structure? 1) `((A a) (B b))) #f)

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

    ;correct-members-mttm? tests
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         '(((A (b b)) (B (a b))))) #t)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,RIGHT))))) #t)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,LEFT))))) #t)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,BLANK))))) #t)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,BLANK)))
                                           ((A (c)) (B (,BLANK))))) #f)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,BLANK)))
                                           ((C (b)) (B (,BLANK))))) #f)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,BLANK)))
                                           ((A (c)) (C (,BLANK))))) #f)
    (check-equal? (correct-members-mttm? '(A B)
                                         '(a b)
                                         `(((A (b)) (B (,BLANK)))
                                           ((A (c)) (B (d))))) #f)
    
    ;incorrect-members-dfa tests
    (check-equal? (incorrect-members-dfa '(A B) '(a b) `((A b B) (B a A))) '())
    (check-equal? (incorrect-members-dfa '(A B) '(a b) `()) '())
    (check-equal? (incorrect-members-dfa '(A B) '(a b) `((A b B) (B a A) (C b A))) '((C b A)))

    ;incorrect-dfa-rules tests
    (check-equal? (incorrect-dfa-rules '(A B) '(a b) `((A b B) (B a A))) '())
    (check-equal? (incorrect-dfa-rules '(A B) '(a b) `((C d E) (A f G)))
                  (list
                   (make-invalid-rule '(C d E)
                                      '("The from state, C, is not in the list of states."
                                        "The consumed letter, d, is not in the input alphabet."
                                        "The to state, E, is not in the list of states."))
                   (make-invalid-rule '(A f G)
                                      '("The consumed letter, f, is not in the input alphabet."
                                        "The to state, G, is not in the list of states."))))
    
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

    ;incorrect-ndpda-rules tests
    (check-equal? (incorrect-ndpda-rules '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d)))))
                  '())
    (check-equal? (incorrect-ndpda-rules '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((D e (f)) (G (,EMP)))))
                  (list (make-invalid-rule `((D e (f)) (G (,EMP)))
                                           (list
                                            "The from state, D, is not in the list of states."
                                            "The letter e is not in the input alphabet."
                                            "The f at index 0 of the pop list is not in the stack alphabet."
                                            "The to state, G, is not in the list of states."
                                            "The ε at index 0 of the push list is not in the stack alphabet."))))

    ;incorrect-tm-rules tests
    (check-equal? (incorrect-tm-rules '(A B C)
                                      '(a b)
                                      `(((A b) (B b))
                                        ((B ,LM) (A ,LEFT))
                                        ((C ,BLANK) (B ,RIGHT))))
                  '())
    (check-equal? (incorrect-tm-rules '(A B C)
                                      '(a b)
                                      `(((D ,LEFT) (E ,EMP))))
                  (list (make-invalid-rule `((D ,LEFT) (E ,EMP))
                                           '("The from state, D, is not in the list of states."
                                             "The read symbol, L, must be in the input alphabet, BLANK, or LM."
                                             "The to state, E, is not in the list of states."
                                             "The action ε must be in the input alphabet, LEFT, RIGHT, or BLANK."))))

    ;incorrect-mttm-rules tests
    (check-equal? (incorrect-mttm-rules '(A B C)
                                        '(a b)
                                        `(((A (,LM ,BLANK)) (B (a b)))
                                          ((B (a b)) (C (,LEFT ,RIGHT)))))
                  '())
    (check-equal? (incorrect-mttm-rules '(A B C)
                                        '(a b)
                                        `(((D (,LEFT ,EMP)) (E (f g)))))
                  (list (make-invalid-rule `((D (,LEFT ,EMP)) (E (f g)))
                                           '("The from state, D, is not in the list of states."
                                             "The read symbol, L, on tape 0 must be in the input alphabet, BLANK, or LM."
                                             "The read symbol, ε, on tape 1 must be in the input alphabet, BLANK, or LM."
                                             "The to state, E, is not in the list of states."
                                             "The action f on tape 0 must be in the input alphabet, LEFT, RIGHT, or BLANK."
                                             "The action g on tape 1 must be in the input alphabet, LEFT, RIGHT, or BLANK."))))
    
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

    ;incorrect-members-mttm tests
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          '(((A (b b)) (B (a b))))) '())
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,RIGHT))))) '())
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,LEFT))))) '())
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,BLANK))))) '())
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,BLANK)))
                                            ((A (c)) (B (,BLANK)))))
                  `(((A (c)) (B (,BLANK)))))
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,BLANK)))
                                            ((C (b)) (B (,BLANK)))))
                  `(((C (b)) (B (,BLANK)))))
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,BLANK)))
                                            ((A (c)) (C (,BLANK)))))
                  `(((A (c)) (C (,BLANK)))))
    (check-equal? (incorrect-members-mttm '(A B)
                                          '(a b)
                                          `(((A (b)) (B (,BLANK)))
                                            ((A (c)) (B (d)))))
                  `(((A (c)) (B (d)))))
    )
  )