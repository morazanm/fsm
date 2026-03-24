#lang racket/base

(module+ tests
  (require "../../../fsm-core/private/rg-constructors.rkt"
           "../../../fsm-core/private/constants.rkt"
           rackunit
           )

  ; Regular Grammars
  ; Nonterminal errors
  ; Nonterminals must be a list
  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The given nonterminals must be a list: A"
   (lambda () (make-rg 'A
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  ; The list must contain only valid nonterminals
  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following: \\(5\\) are not valid nonterminals in the given list of nonterminals: \\(A S 5\\)"
   (lambda () (make-rg '(A S 5)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  ; The list must contain only distinct elements
  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following values, \\(S\\), are duplicated in the given nonterminals: \\(A S S\\)"
   (lambda () (make-rg '(A S S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  
  ; Alphabet Errors
  ; The alphabet field must be a list
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The given grammar alphabet must be a list: a"
   (lambda () (make-rg '(A S)
                        'a
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  ; The alphabet must only contain lowercase alphabet letters
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following: \\(B\\) are not valid lowercase alphabet letters in the given input alphabet: \\(a B\\)"
   (lambda () (make-rg '(A S)
                        '(a B)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  ; The alphabet must contain only distinct elements
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b a\\)"
   (lambda () (make-rg '(A S)
                        '(a b a)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  
  ; Rule errors
  ; The rules must be a list
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given grammar nonterminals must be a list: S"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `S
                        'S))
               )
  ; The rules must be a list of valid rule tuples
  ; This is supposed to be a rule error (since it doesn't have the right structure),
  ; but I can't get the error to compare correctly.
  (check-exn
   exn:fail:contract?
   (lambda () (make-rg '(A S)
                        '(a b)
                        `(S ,ARROW ,EMP)
                        'S)))
  ; The rules must have valid left hand and right hand sides
  ; This is supposed to be a rule error (since it doesn't have the right structure),
  ; but I can't get the error to compare correctly.
  (check-exn
   exn:fail:contract?
   (lambda () (make-rg '(S A)
                        '(a b)
                        `((s ,ARROW EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)))
  ; Only a rule with the start state on the LHS can have EMP on the RHS
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following rules cannot have EMP in their RHS: \\(\\(A -> ε\\)\\)"
   (lambda () (make-rg '(S A)
                        '(a b)
                        `((A ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  ; The list of rules must contain distinct elements
  (check-exn
   "Step three of the design recipe has not been successfully completed.
The following values, \\(\\(S -> aA\\)\\), are duplicated in the given rules: \\(\\(S -> ε\\) \\(S -> aA\\) \\(S -> aA\\) \\(A -> bS\\)\\)"
   (lambda () (make-rg '(S A)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
               )
  
  ; Starting state errors
  ; The starting state must be a valid state symbol
  (check-exn
   "Step three of the design recipe has not been successfully completed.
The given starting state: 5 is not a valid state"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        5))
               )
  ; The starting state must be in the list of nonterminals
  (check-exn
   "Step three of the design recipe has not been successfully completed.
The following starting state, B, is not in the given list of states: \\(A S\\)"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'B))
               )

  ; Accepts Field errors
  (check-exn
   "Step four of the design recipe has not been successfully completed.
The expected accepts is not a list of words: aaa"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:accepts 'aaa))
               )

  (check-exn
   "Step four of the design recipe has not been successfully completed.
The following words in the accepts list contain symbols not included in sigma: \\(\\(a c d\\)\\)"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:accepts '((a c d))))
               )
  (check-exn
   "Step six of the design recipe has not been successfully completed.
The constructed grammar does not derive the following words it should derive: \\(\\(a a a\\)\\)"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:accepts '((a a a) (a b a b))))
               )
  (check-exn
   "Step six of the design recipe has not been successfully completed.
The constructed grammar derives the following words it should not derive: \\(\\(a b a b a b\\)\\)"
   (lambda () (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:rejects '((a a a) (a b a b a b))))
               )
  )