#lang racket
(require (for-syntax
          syntax/parse
          racket/bool
          racket/contract
          racket/list
          racket/match
          syntax/to-string
          syntax/stx))

(begin-for-syntax

  ;; valid-state-name?: syntax -> boolean
  ;; Purpose: Checks to see if the given syntax expression matches a valid
  ;;          machine state, which is either a single capital letter or a
  ;;          capital letter followed by a dash '-' and at least one digit.
  ;;          If the syntax is valid, it is returned, otherwise false is
  ;;          returned.
  (define/contract (valid-state-name? stx)
    (-> syntax? (or/c boolean? (listof string?)))
    (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
    (regexp-match regex-pattern (symbol->string (syntax->datum stx)))
    )

  ;; valid-alphabet-letter?: syntax -> boolean | syntax
  ;; Purpose: Checks to see if the given syntax expression matches a valid
  ;;          alphabet member, which is a single lowercase letter. If the
  ;;          syntax is valid, 
  (define/contract (valid-alphabet-letter? stx)
    (-> syntax? (or/c boolean? (listof string?)))
    (define regex-pattern #px"[a-z]")
    (regexp-match regex-pattern (symbol->string (syntax->datum stx))))

  ;; member-stx: syntax -> [syntax] -> boolean
  ;; Purpose: Checks if the given target syntax represents the same datum
  ;;          as any member of the syntax objects in the syntax-list.
  (define/contract (member-stx target-syntax syntax-list)
    (-> syntax? (listof syntax?) boolean?)
    (list? (member (syntax->datum target-syntax) (map syntax->datum syntax-list))))

  ;; invalid-start-state?: syntax -> syntax -> syntax | boolean
  ;; Purpose: Checks to see if the given start state syntax is invalid,
  ;;          depending on the states-list syntax. If the start state
  ;;          is a member of the list of machine states, the function
  ;;          returns false, otherwise it returns the invalid start state syntax.
  (define/contract (invalid-start-state? start-stx machine-states-stx)
    (-> syntax? syntax? (or/c syntax? boolean?))
    (if (member-stx start-stx (syntax->list machine-states-stx))
        #f
        start-stx)
    )

  ;; find-unexpected-final-states: syntax -> syntax -> (listof symbol)
  ;; Purpose: Given a syntax object representing the list of final states of a
  ;;          machine and a syntax object representing the list of machine
  ;;          states, returns a list of states (as symbols) of every final
  ;;          state not in the list of machine states.
  (define/contract
    (find-unexpected-final-states final-states-stx machine-states-stx)
    (-> syntax? syntax? (listof symbol?))
    (define machine-states (map syntax->datum (syntax->list machine-states-stx)))
    (define final-states (map syntax->datum (syntax->list final-states-stx)))
    (filter
     (lambda (final-state) (not (member final-state machine-states)))
     (map syntax->datum (syntax->list final-states-stx))
     )
    )

  ;; invalid-final-states? syntax -> syntax -> syntax | boolean
  ;; Purpose: Checks to see if the given final-states syntax is invalid,
  ;;          depending on the states-list syntax. If any of the final states
  ;;          is not a member of the states list, returns that final state.
  ;;          If all of the final states are in the states-list, returns false.
  (define (invalid-final-states? final-states-stx machine-states-stx)
    (define machine-states-stx-list (syntax->list machine-states-stx))
    (define final-states-stx-list (syntax->list final-states-stx))
    (define (find-invalid-final finals all-states)
      (cond [(empty? finals) #f]
            [(member-stx (car finals) all-states)
             (find-invalid-final (cdr finals) all-states)]
            [else (car finals)]))
    (find-invalid-final final-states-stx-list machine-states-stx-list)
    )

  ;; get-pair: '(id id id) -> '(id id)
  ;; Purpose: Given a datum representing a rule (a triplet of identifiers)
  ;;          returns a list containing the from-state and alphabet letter.
  (define/contract (get-pair rule-datum)
    (-> (listof symbol?) (listof symbol?))
    (list (car rule-datum) (cadr rule-datum)))

  ;; find-duplicate-state-letter-pair: syntax -> syntax | boolean
  ;; Purpose: Checks to see if the given syntax representing the list of
  ;;          machine rules has rules with duplicate (start-state, letter)
  ;;          pairs. If any two rules have matching start states and alphabet
  ;;          letters, returns the first invalid rule. If no rules have matching
  ;;          start state and alphabet letter, then returns false.
  (define (find-duplicate-state-letter-pair rules-stx)
    (define (find-first-duplicate rules-list)
      (cond [(empty? rules-list) #f]
            [(member
              (get-pair (syntax->datum (car rules-list)))
              (map (lambda (rule-stx) (get-pair (syntax->datum rule-stx)))
                   (cdr rules-list)))
             (car rules-list)]
            [else (find-first-duplicate (cdr rules-list))]))
    (find-first-duplicate (syntax->list rules-stx)))

  (define/contract (not-dead-state state)
    (-> (or/c symbol? list?) boolean?)
    (not (equal? state ',DEAD)))

  ;; missing-state-letter-pairs: syntax -> syntax -> syntax -> [(state letter)]
  ;; Purpose: Given syntax representing the list of machine rules, syntax
  ;;          representing machine states, and syntax representing a machine
  ;;          alphabet, returns a list of every (from-state, letter) pairs
  ;;          that are missing from the rule-set - preventing the rule-set
  ;;          from being considered a function. In more simple-terms,
  ;;          this is the set difference of the (from-state, letter) pairs in
  ;;          the list of rules compared to the Cartesian product of all states
  ;;          and letters in the alphabet.
  (define (missing-state-letter-pairs rules-stx states-stx alphabet-stx)
    (define all-pairs
      (cartesian-product (filter not-dead-state (map syntax->datum (syntax->list states-stx)))
                         (map syntax->datum (syntax->list alphabet-stx))))
    (define existing-pairs
      (map (lambda (rule-stx) (get-pair (syntax->datum rule-stx)))
           (syntax->list rules-stx)))
    (filter (lambda (pair) (not (member pair existing-pairs))) all-pairs)
    )

  (define/contract (find-duplicate-state machine-states-stx)
    (-> syntax? (or/c syntax? boolean?))
    (define datums (map syntax->datum (syntax->list machine-states-stx)))
    (if (check-duplicates datums)
        machine-states-stx
        #f)
    )
  )

(define-syntax (make-dfa2 stx)
  ;; state: Syntax class representing a single machine state.
  ;; Description: A state is an identifier that can either be:
  ;; 1. The variable DEAD
  ;; 2. A single upper-case character [A-Z]
  ;; 3. A single upper-case character followed by a dash and at least one digit.
  (define-syntax-class state
    #:description "a single state of the machine"
    (pattern name:id 
      #:fail-unless (valid-state-name? #'name)
      "state name must be a single upper-case letter (e.g. A), or an uppercase letter followed by a dash and at least one digit (e.g. B-1234)")
    )
  ;; syntax class for a quoted state 
  (define-syntax-class qstate
    #:description "a quoted state"
    (pattern 'field:id
      #:fail-unless (valid-state-name? #'field)
      "Invalid state name")
    )
  (define-syntax-class quasiquoted-state
    #:description "an unquoted state or DEAD"
    (pattern s:state)
    (pattern ,DEAD))
  ;; machine-states: Syntax class representing the set of machine states.
  (define-syntax-class machine-states
    #:description "the set of machine states"
    (pattern `(s:quasiquoted-state ...)
      #:with error? (find-duplicate-state #`(s ...))
      #:fail-when (syntax-e #'error?)
      "machine states cannot contain duplicates.")
    (pattern (~or* '(s:state ...) (list s:qstate ...))
      #:fail-when (check-duplicate-identifier (syntax->list #`(s ...)))
      "machine states cannot contain duplicates.")
    )
  ;; letter: Syntax class representing a single letter in the machine alphabet.
  ;; Description: Must be a single lowercase letter.
  (define-syntax-class letter
    #:description "a single letter in the machine alphabet"
    (pattern name:id
      #:fail-unless (valid-alphabet-letter? #'name)
      "alphabet member must be a single lowercase letter"
      )
    )
  ;; alphabet: Syntax class representing the machine alphabet.
  ;; Description: Must be a set of lowercase letters.
  (define-syntax-class alphabet
    #:description "the machine alphabet"
    (pattern '(l:letter ...)
      #:fail-when (check-duplicate-identifier (syntax->list #`(l ...)))
      "machine sigma cannot contain duplicate letters")
    )
  ;; start-state: Syntax class representing the machine start state.
  ;; Description: Must be a machine state. 
  (define-syntax-class start-state
    #:description "the machine start state"
    (pattern 's:state))
  ;; final-states: Syntax class representing the list of final states of the
  ;;               machine.
  ;; Description: Must be a set of states, each member of which must be in the
  ;;              list of machine-states.
  (define-syntax-class final-states
    #:description "the set of machine final states"
    (pattern '(s:state ...)
      #:fail-when (check-duplicate-identifier (syntax->list #`(s ...)))
      "machine final states cannot contain duplicates.")
    (pattern (list `s:state ...)
      #:fail-when (check-duplicate-identifier (syntax->list #`(s ...)))
      "machine final states cannot contain duplicates."))
  ;; rule: Syntax class representing a single machine rule.
  ;; Description: Must be a triple containing a state, alphabet letter, and
  ;;              state in that order.
  (define-syntax-class rule
    #:description "a machine rule"
    (pattern (from:state l:letter to:state)))
  ;; machine-rules: Syntax class representing the set of machine rules.
  ;; Description: Must be a set of rules (see above syntax-class), which all
  ;;              together form a function, meaning the entire domain of
  ;;              (states x sigma) is covered.
  (define-syntax-class machine-rules
    #:description "the set of machine rules"
    (pattern (~or* '(r:rule ...) (list r:rule ...))
      #:fail-when (find-duplicate-state-letter-pair #`(r ...))
      "machine rules cannot contain duplicate (from-state, letter) pairs"))
  (syntax-parse stx
    [(_ states:machine-states sigma:alphabet start:start-state finals:final-states delta:machine-rules)
     #:fail-when (invalid-start-state? #'start.s #`(states.s ...))
     (format "Start state must be in the list of states. Either use a start state in the list: ~s, or add state: ~a to the list of machine states"
             (syntax->datum #`(states.s ...))
             (syntax->datum #'start.s))
     #:with unexpected-final-states (find-unexpected-final-states #`(finals.s ...) #`(states.s ...))
     #:fail-when (if (not (empty? (syntax->datum #'unexpected-final-states))) #`finals #f)
     (format "Unexpected states found in the set of final states. Either use finals from the set of machine states: ~s, or add the following states to the machine states: ~s."
             (syntax->datum #`(states.s ...))
             (syntax->datum #`unexpected-final-states))
     #:with missing-pairs (missing-state-letter-pairs #`(delta.r ...) #`(states.s ...) #`(sigma.l ...))
     #:fail-when (if (not (empty? (syntax->datum #'missing-pairs))) #`delta #f)
     (format "Machine delta (set of rules) is not a function because it is missing rules with the following (from-state, letter) pairs: ~s" (syntax->datum #'missing-pairs))
     #`(void)])
  )

(make-dfa2 `(S F A F ,DEAD)
           '(a b)
           'S
           (list 'F 'A)
           `((S a F)
             (S b DEAD)
             (F a F)
             (F b A)
             (A a F)
             (A b A)))

(make-dfa2 (list 'S 'F 'A DEAD)
           '(a b)
           'S
           (list 'F 'A)
           '((S a F)
             (S b DEAD)
             (F a F)
             (F b A)
             (A a F)
             (A b A)))
