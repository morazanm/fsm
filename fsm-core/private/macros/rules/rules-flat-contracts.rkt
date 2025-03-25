#lang racket/base
(require "rules-predicates.rkt"
         "../error-formatting.rkt"
         racket/contract
         racket/list
         )
(provide listof-rules/c
         correct-members/c
         correct-dfa-rules/c
         correct-dfa-rule-structures/c
         correct-ndfa-rule-structures/c
         correct-ndpda-rules/c
         correct-ndpda-rule-structures/c
         correct-tm-rules/c
         correct-tm-rule-structures/c
         correct-mttm-rules/c
         correct-mttm-rule-structures/c
         functional/c
         no-duplicates-dfa/c

         ;;grammars
         no-emp-rhs/c
         correct-grammar-rule-structures/c
         correct-rg-rules/c
         correct-cfg-rules/c
         correct-csg-rules/c
         )

(define design-recipe-message "Step four of the design recipe has not been successfully completed.")

(define (listof-rules/c pred)
  (make-flat-contract
   #:name 'valid-list-of-rules
   #:first-order (lambda (rules) (valid-rules? pred rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     (map (lambda (x) (format "~n~s" x)) (invalid-rules pred rules))
                     (format "~a\nThe following rules are improperly formatted" design-recipe-message)
                     )
                    )
                  )
   )
  )

(define (correct-members/c pred pred2 states sigma)
  (make-flat-contract
   #:name 'valid-list-of-rules
   #:first-order (lambda (rules) (pred states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-rule-error)
                    (raise-blame-error
                     blame
                     (map (lambda (x) (format "~n~s" x)) (pred2 states sigma rules))
                     (format "~a\nThe following rules contain symbols not contained in the states/sigma: " design-recipe-message)
                     )
                    )
                  )
   )
  )

;correct-dfa-rule-structures/c: contract
;predicate: (listof x) -> boolean
;purpose: Ensures that every element in the list is structured as a valid dfa rule.
; It checks each rule to see that it is a (list state alphabet state)
(define correct-dfa-rule-structures/c
  (make-flat-contract
   #:name 'correct-dfa-rule-structures
   #:first-order (lambda (rules)  (empty? (incorrect-dfa-rule-structures rules)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (if (empty? (incorrect-dfa-rule-structures rules))
                        rules
                        (raise-blame-error
                         blame
                         (list (car (incorrect-dfa-rule-structures rules)))
                         (format "~a\nThe following rules have structural errors" design-recipe-message)))
                    ))))

;correct-dfa-rules/c: (listof state) (listof alpha) --> contract
;predicate: (listof x) -> boolean
;Purpose: Ensures that every element in the list is a valid dfa rule. It checks
; each rule to see if the from-state and to-states are in the list of machine
; states, and the consumed letter is in the machine sigma.
(define (correct-dfa-rules/c states sigma)
  (make-flat-contract
   #:name 'correct-dfa-rules
   #:first-order (lambda (rules) (correct-members-dfa? states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-dfa-rules states sigma rules)))
                     (format "~a\nThe following rules have errors" design-recipe-message))))))

;correct-ndfa-rule-structures/c: contract
;predicate: (listof x) -> boolean
;purpose: Ensures that every element in the list is structured as a valid ndfa rule.
; It checks each rule to see that it is a (list state alphabet state)
(define correct-ndfa-rule-structures/c
  (make-flat-contract
   #:name 'correct-dfa-rule-structures
   #:first-order (lambda (rules)  (empty? (incorrect-ndfa-rule-structures rules)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (if (empty? (incorrect-ndfa-rule-structures rules))
                        rules
                        (raise-blame-error
                         blame
                         (list (car (incorrect-ndfa-rule-structures rules)))
                         (format "~a\nThe following rules have structural errors" design-recipe-message)))
                    ))))

;correct-ndpda-rule-structures/c: contract
;predicate: (listof x) -> boolean
;purpose: Ensures that every element in the list is structured as a valid ndpda rule.
; It checks each rule to see that it is a (list (list state symbol pop) (list state push))
(define correct-ndpda-rule-structures/c
  (make-flat-contract
   #:name 'correct-ndpda-rule-structures
   #:first-order (lambda (rules) (empty? (incorrect-ndpda-rule-structures rules)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (if (empty? (incorrect-ndpda-rule-structures rules))
                        rules
                        (raise-blame-error
                         blame
                         (list (car (incorrect-ndpda-rule-structures rules)))
                         (format "~a\nThe following rules have structural errors" design-recipe-message)))
                    ))))

;correct-ndpda-rules/c: (listof state) (listof alpha) (listof symbol) --> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that every element in the list is a valid pda rule. It checks
; each rule to see that the from-state and to-state are in the list of machine
; states, that the consumed letter is in the machine sigma, and that the
; push and pop elements are either the EMP constant or lists of symbols from
; the machine gamma.
(define (correct-ndpda-rules/c states sigma gamma)
  (make-flat-contract
   #:name 'correct-ndpda-rules
   #:first-order (lambda (rules) (correct-members-ndpda? states sigma gamma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-ndpda-rules states sigma gamma rules)))
                     (format "~a\nThe following rules have errors, which make them invalid" design-recipe-message))))))

;correct-tm-rule-structures/c: contract
;predicate: (listof x) -> boolean
;purpose: Ensures that every element in the list is structured as a valid tm rule.
; It checks each rule to see that it is a (list (list state symbol) (list state tm-action))
(define correct-tm-rule-structures/c
  (make-flat-contract
   #:name 'correct-tm-rule-structures
   #:first-order (lambda (rules) (empty? (incorrect-tm-rule-structures rules)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (if (empty? (incorrect-tm-rule-structures rules))
                        rules
                        (raise-blame-error
                         blame
                         (list (car (incorrect-tm-rule-structures rules)))
                         (format "~a\nThe following rules have structural errors" design-recipe-message)))
                    ))))

;correct-tm-rules/c: (listof state) (list of alpha) --> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that every element in the list is a valid tm rule. It checks
; each rule to see that the from-state and to-state are in the list of machine
; states, that the read tape symbol is either in the machine sigma or is the
; BLANK or LM constants, and that the tm-action is either in the machine sigma
; or is the LEFT or RIGHT constants.
(define (correct-tm-rules/c states sigma)
  (make-flat-contract
   #:name 'correct-tm-rules
   #:first-order (lambda (rules) (correct-members-tm? states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-tm-rules states sigma rules)))
                     (format "~a\nThe following rules have errors, which make them invalid" design-recipe-message))))))

;correct-tm-rule-structures/c: natnum -> contract
;predicate: (listof x) -> boolean
;purpose: Ensures that every element in the list is structured as a valid mttm rule.
; It checks each rule to see that it is a (list (list state (listof symbol)) (list state (listof tm-action)))
(define (correct-mttm-rule-structures/c num-tapes)
  (make-flat-contract
   #:name 'correct-mttm-rule-structures
   #:first-order (lambda (rules) (empty? (incorrect-mttm-rule-structures rules num-tapes)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (if (empty? (incorrect-mttm-rule-structures rules num-tapes))
                        rules
                        (raise-blame-error
                         blame
                         (list (car (incorrect-mttm-rule-structures rules num-tapes)))
                         (format "~a\nThe following rules have structural errors" design-recipe-message)))
                    ))))

;correct-mttm-rules/c: (listof state) (listof alpha) --> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that every element in the list is a valid mttm rule. It checks
; each rule to see that the from-state and to-state are in the list of machine
; states, that every read tape symbol is either in the machine sigma or is the
; BLANK or LM constants, and that every tm-action is either in the machine
; sigma or is the LEFT or RIGHT constants.
(define (correct-mttm-rules/c states sigma)
  (make-flat-contract
   #:name 'correct-mttm-rules
   #:first-order (lambda (rules) (correct-members-mttm? states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-mttm-rules states sigma rules)))
                     (format "~a\nThe following rules have errors, which make them invalid" design-recipe-message))))))

;functional/c: (listof state) (listof sigma) symbol -> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that the given list of rules forms an entire function over
; the domain of machine states and sigma elements. For this to be true, either:
; a) the add-dead flag must be true, in which case dead-state rules are added
;    to make the rule-set a function regardless of the existing rules.
; b) there must be a rule in the list of rules for every pair in the Cartesian
;    product of state-sigma pairings.
(define (functional/c states sigma no-dead)
  (make-flat-contract
   #:name 'functional-list-of-rules?
   #:first-order (lambda (rules) (functional? rules states sigma no-dead))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-missing-rule-error)
                    (raise-blame-error
                     blame
                     (map (lambda (x) (format "~n~s" x)) (missing-functional rules states sigma))
                     (format "~a\nYou must include rules for these state/alphabet letter pairings: " design-recipe-message)
                     )
                    )
                  )
   )
  )

;; Defines a simple contract to check that there are no duplicates in a list
;; Parameter type refers to the type of elements in the list, e.g. number, state, symbol, etc.
(define (no-duplicates-dfa/c type)
  (make-flat-contract
   #:name (string->symbol (format "distinct-list-of-~a" type))
   #:first-order (lambda (vals) (not (check-duplicates-dfa vals)))
   #:projection (lambda (blame)
                  (lambda (vals)
                    (current-blame-format format-duplicates-error)
                    (raise-blame-error
                     blame
                     (map (lambda (x) (format "~n~s" x)) (check-duplicates-dfa vals))
                     (format "~a\nThe following state/sigma pairs are duplicated in your ~a: " design-recipe-message type)
                     )
                    )
                  )
   )
  )

;; GRAMMARS

;; Purpose: Ensures that only a regular grammar rule that has the start state
;; as the left-hand-side can have the empty state on the right hand side.
(define (no-emp-rhs/c start)
  (make-flat-contract
   #:name 'emp-check-for-rg
   #:first-order (lambda (rules) (empty? (incorrect-rhs-rg rules start)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-error)
                    (if (empty? (incorrect-rhs-rg rules start))
                        rules
                        (raise-blame-error
                         blame
                         (incorrect-rhs-rg rules start)
                         (format "~a\nThe following rules cannot have EMP in their RHS" design-recipe-message)))
                    ))))
  
;correct-tm-rule-structures/c: natnum -> contract
;predicate: (listof x) -> boolean
;purpose: Ensures that every element in the list is structured as a valid mttm rule.
; It checks each rule to see that it is a (list (list state (listof symbol)) (list state (listof tm-action)))
(define correct-grammar-rule-structures/c
  (make-flat-contract
   #:name 'correct-grammar-rule-structures
   #:first-order (lambda (rules) (empty? (incorrect-grammar-rule-structures rules)))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (let [(incorrect-rules (incorrect-grammar-rule-structures rules))]
                      (current-blame-format format-incorrect-rules-error)
                      (if (empty? incorrect-rules)
                          rules
                          (raise-blame-error
                           blame
                           (list (car incorrect-rules))
                           (format "~a\nThe following rules have structural errors" design-recipe-message)))
                      )
                    ))))

;correct-rg-rules/c: (listof nonterminals) (listof alpha) --> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that every element in the list is a valid regular grammar rule.
; It checks each rule to see that the first element is in the list of machine
; nonterminals, and that every right hand side is a combination of terminal symbols
(define (correct-rg-rules/c states sigma)
  (make-flat-contract
   #:name 'correct-rg-rules
   #:first-order (lambda (rules) (correct-members-rg? states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-rg-rules states sigma rules)))
                     (format "~a\nThe following rules have errors, which make them invalid" design-recipe-message))))))

;correct-cfg-rules/c: (listof nonterminals) (listof alpha) --> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that every element in the list is a valid regular grammar rule.
; It checks each rule to see that the first element is in the list of machine
; nonterminals, and that every right hand side is a combination of terminal symbols
(define (correct-cfg-rules/c states sigma)
  (make-flat-contract
   #:name 'correct-cfg-rules
   #:first-order (lambda (rules) (correct-members-cfg? states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-cfg-rules states sigma rules)))
                     (format "~a\nThe following rules have errors, which make them invalid" design-recipe-message))))))

;correct-cfg-rules/c: (listof nonterminals) (listof alpha) --> contract
;predicate: (listof x) --> boolean
;Purpose: Ensures that every element in the list is a valid regular grammar rule.
; It checks each rule to see that the first element is in the list of machine
; nonterminals, and that every right hand side is a combination of terminal symbols
(define (correct-csg-rules/c states sigma)
  (make-flat-contract
   #:name 'correct-csg-rules
   #:first-order (lambda (rules) (correct-members-csg? states sigma rules))
   #:projection (lambda (blame)
                  (lambda (rules)
                    (current-blame-format format-incorrect-rules-error)
                    (raise-blame-error
                     blame
                     (list (car (incorrect-csg-rules states sigma rules)))
                     (format "~a\nThe following rules have errors, which make them invalid" design-recipe-message))))))
