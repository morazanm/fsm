(module predicates racket
  (require racket/contract
           "../constants.rkt"
           "../sm-getters.rkt"
           "../fsa.rkt"
           "../tm.rkt"
           "../pda.rkt"
           "../../../main.rkt")
  (provide functional?
           missing-functional
           valid-dfa-rule?
           valid-tm-rule?
           valid-rules?
           valid-finals?
           invalid-finals
           valid-start?
           start-in-states?
           valid-sigma?
           valid-list-of-states?
           valid-state?
           valid-alpha?
           return-duplicates
           invalid-rules
           dfa?
           ndpda?
           ndfa?
           tm?
           check-duplicates-dfa
           check-input-dfa
           check-input-ndfa
           check-input-ndpda
           check-input-tm
           return-input-dfa
           return-input-ndpda
           listof-words?
           valid-ndpda-rule?
           return-input-ndfa
           return-input-tm
           listof-words?
           incorrect-members-dfa
           correct-members-dfa?
           correct-members-tm?
           incorrect-members-tm)

  (define (dfa? machine)
    (equal? (sm-type machine) 'dfa))

  (define (ndpda? machine)
    (equal? (sm-type machine) 'pda))
  
  (define (ndfa? machine)
    (equal? (sm-type machine) 'ndfa))

  (define (tm? machine)
    (or (equal? (sm-type machine) 'tm-language-recognizer)
        (equal? (sm-type machine) 'tm)))

  (define (return-duplicates los)
    (cond [(empty? los) '()]
          [(member (car los) (cdr los)) (cons (car los) (return-duplicates (cdr los)))]
          [else (return-duplicates (cdr los))]))
  
  ;applies the predicate to the list and returns false if
  ; any of the members of the list are invalid states/sigma
  (define (valid-list-of los pred)
    (andmap pred los)
    )

  (define (valid-state? x)
    (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
    (or (equal? x DEAD)
        (if (symbol? x)
            (regexp-match regex-pattern (symbol->string x))
            #f)
        )
    )

  (define (valid-list-of-states? states)
    (valid-list-of states valid-state?)
    )

  (define (valid-alpha? x)
    (define regex-pattern #px"[a-z]")
    (if (symbol? x)
        (regexp-match regex-pattern (symbol->string x))
        #f)
    )

  (define (valid-sigma? sigma)
    (valid-list-of sigma valid-alpha?)
    )

  (define (start-in-states? states)
    (lambda (start)
      (member start states)
      )
    )

  (define (valid-start? states)
    (lambda (start)
      (and (symbol? start)
           (valid-state? start)))
    )

  (define (valid-finals? states)
    (lambda (finals)
      (andmap (lambda (x) (member x states)) finals)
      )
    )
  (define (invalid-finals states finals)
    (filter (lambda (x) (not (member x states)))finals))


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

  (define (correct-members-dfa? states sigma rules)
    (andmap (lambda (x) (and (member (car x) states)
                             (member (cadr x) sigma)
                             (member (caddr x) states))) rules))

  (define (incorrect-members-dfa states sigma rules)
    (filter (lambda (x) (not (and (member (car x) states)
                                  (member (cadr x) sigma)
                                  (member (caddr x) states)))) rules))

  (define (correct-members-tm? states sigma rules)
    (andmap (lambda (x) (and (member (car (car x)) states)
                             (member (cadr (car x)) sigma)
                             (member (car (cadr x)) states)
                             (member (cadr (cadr x)) sigma))) rules))

  (define (incorrect-members-tm states sigma rules)
    (filter (lambda (x) (not (and (member (car (car x)) states)
                                  (member (cadr (car x)) sigma)
                                  (member (car (cadr x)) states)
                                  (member (cadr (cadr x)) sigma)))) rules))

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

  (define (valid-rules? pred rules)
    (andmap (lambda (rule) (pred rule)) rules))

  (define (invalid-rules pred rules)
    (filter (lambda (rule) (not (pred rule)))
            rules)
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

  (define (listof-words? words sigma)
    (and (list? words)
         (andmap (lambda (word) (and (list? word)
                                     (andmap (lambda (letter) (symbol? letter))
                                             word))) words))
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

  (define (check-input-ndpda states
                            sigma
                            gamma
                            start
                            finals
                            rules
                            accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-ndpda states
                                                sigma
                                                gamma
                                                start
                                                finals
                                                rules))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  (define (check-input-tm states
                          sigma
                          start
                          finals
                          rules
                          accept
                          accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-tm states
                                              sigma
                                              rules
                                              start
                                              finals
                                              accept))
      (andmap (lambda (x) (equal? (sm-apply temp-machine x) accepts?)) words)
      )
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

  (define (return-input-ndpda states
                             sigma
                             gamma
                             start
                             finals
                             rules
                             words
                             accepts?)
    (define temp-machine (make-unchecked-ndpda states
                                              sigma
                                              gamma
                                              start
                                              finals
                                              rules))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )

  (define (return-input-tm states
                           sigma
                           start
                           finals
                           rules
                           words
                           accept
                           accepts?)
    (define temp-machine (make-unchecked-tm states
                                            sigma
                                            rules
                                            start
                                            finals
                                            accept))
    (filter (lambda (x) (equal? (sm-apply temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )
  )
