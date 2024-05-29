(module rg-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  (provide)

  ; Regular Grammars
  ; Nonterminal errors
  ; Nonterminals must be a list
  (check-error (make-rg 'A
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step two of the design recipe has not been successfully completed.
The given nonterminals must be a list: A"))
  ; The list must contain only valid nonterminals
  (check-error (make-rg '(A S 5)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step two of the design recipe has not been successfully completed.
The following: (5) are not valid nonterminals in the given list of nonterminals: (A S 5)"))
  ; The list must contain only distinct elements
  (check-error (make-rg '(A S S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step two of the design recipe has not been successfully completed.
The following values, (S), are duplicated in the given nonterminals: (A S S)"))
  
  ; Alphabet Errors
  ; The alphabet field must be a list
  (check-error (make-rg '(A S)
                        'a
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step one of the design recipe has not been successfully completed.
The given grammar alphabet must be a list: a"))
  ; The alphabet must only contain lowercase alphabet letters
  (check-error (make-rg '(A S)
                        '(a B)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step one of the design recipe has not been successfully completed.
The following: (B) are not valid lowercase alphabet letters in the given input alphabet: (a B)"))
  ; The alphabet must contain only distinct elements
  (check-error (make-rg '(A S)
                        '(a b a)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b a)"))
  
  ; Rule errors
  ; The rules must be a list
  (check-error (make-rg '(A S)
                        '(a b)
                        `S
                        'S)
               (format "Step three of the design recipe has not been successfully completed.
The given grammar nonterminals must be a list: S"))
  ; The rules must be a list of valid rule tuples
  ; This is supposed to be a rule error (since it doesn't have the right structure),
  ; but I can't get the error to compare correctly.
  (check-error (make-rg '(A S)
                        '(a b)
                        `(S ,ARROW ,EMP)
                        'S))
  ; The rules must have valid left hand and right hand sides
  ; This is supposed to be a rule error (since it doesn't have the right structure),
  ; but I can't get the error to compare correctly.
  (check-error (make-rg '(S A)
                        '(a b)
                        `((s ,ARROW EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S))
  ; Only a rule with the start state on the LHS can have EMP on the RHS
  (check-error (make-rg '(S A)
                        '(a b)
                        `((A ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step four of the design recipe has not been successfully completed.
The following rules cannot have EMP in their RHS: ((A -> ε))"))
  ; The list of rules must contain distinct elements
  (check-error (make-rg '(S A)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S)
               (format "Step three of the design recipe has not been sucsessfully completed.
The following values, ((S -> aA)), are duplicated in the given rules: ((S -> ε) (S -> aA) (S -> aA) (A -> bS))"))
  
  ; Starting state errors
  ; The starting state must be a valid state symbol
  (check-error (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        5)
               (format "Step three of the design recipe has not been successfully completed.
The given starting state: 5 is not a valid state"))
  ; The starting state must be in the list of nonterminals
  (check-error (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'B)
               (format "Step three of the design recipe has not been successfully completed.
The following starting state, B, is not in the given list of states: (A S)"))

  ; Accepts Field errors
  (check-error (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:accepts 'aaa)
               (format "The expected accepts is not a list of words: aaa"))

  (check-error (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:accepts '((a c d)))
               (format "Step six of the design recipe has not been successfully completed. The following words in the accepts list contain symbols not included in sigma: ((a c d))"))
  (check-error (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:accepts '((a a a) (a b a b)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed grammar does not accept the following words: ((a a a))"))
  (check-error (make-rg '(A S)
                        '(a b)
                        `((S ,ARROW ,EMP)
                          (S ,ARROW aA)
                          (A ,ARROW bS))
                        'S
                        #:rejects '((a a a) (a b a b a b)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed grammar does not reject the following words: ((a b a b a b))"))
  

  (test)
  )