(module cfg-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  (provide)

  ; Context Free Grammars
  ;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S))
  ; States errors
  ; States must be a list
  (check-error (make-cfg 'S
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S)
               (format "Step two of the design recipe has not been successfully completed.
The given nonterminals must be a list: S"))
  ; The list must contain only valid state symbols
  (check-error (make-cfg '(S 6)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S)
               (format "Step two of the design recipe has not been successfully completed.
The following: (6) are not valid nonterminals in the given list of nonterminals: (S 6)"))
  ; The list must contain only distinct elements
  (check-error (make-cfg '(S S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S)
               (format "Step two of the design recipe has not been successfully completed.
The following values, (S), are duplicated in the given nonterminals: (S S)"))

  ; Sigma errors
  ; Sigma must be a list
  (check-error (make-cfg '(S)
                         'ab
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S)
               (format "Step one of the design recipe has not been successfully completed.
The given grammar alphabet must be a list: ab"))
  ; The list must contain only valid lowercase alphabet symbols
  (check-error (make-cfg '(S)
                         '(a B)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S)
               (format "Step one of the design recipe has not been successfully completed.
The following: (B) are not valid lowercase alphabet letters in the given input alphabet: (a B)"))
  ; The list must contain only distinct elements
  (check-error (make-cfg '(S)
                         '(a b b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S)
               (format "Step one of the design recipe has not been successfully completed.
The following values, (b), are duplicated in the given sigma: (a b b)"))

  ; Delta errors
  ; Delta must be a list
  (check-error (make-cfg '(S)
                         '(a b)
                         'A
                         'S)
               (format "Step three of the design recipe has not been successfully completed.
The given grammar nonterminals must be a list: A"))
  ; Each rule in delta must have the correct structure (symbol -> symbol)
  (check-error (make-cfg '(S)
                         '(a b)
                         '((5 6 7))
                         'S)
               ; This is the expected error message, but check-error isn't comparing it correctly.
               ;               (format "Step four of the design recipe has not been successfully completed.
               ;The following rules have structural errors:
               ;Rule (5 6 7):
               ;  The first element in the rule, 5, is not a symbol.
               ;  The second element in the rule, 6, is not the expected ARROW symbol.
               ;  The third element in the rule, 7, is not a symbol with 2 or less elements.")
               )
  ; The left and right hand sides of the rules must contain elements from
  ; the list of states and sigma
  (check-error (make-cfg '(S)
                         '(a b)
                         `((s ,ARROW EMP)
                           (S ,ARROW aSb))
                         'S)
               ; This is the expected error message, but check-error isn't comparing it correctly.
               ;               (format "Step four of the design recipe has not been successfully completed.
               ;The following rules have errors, which make them invalid:
               ;Rule (s -> EMP):
               ;  The following members (s) of the left hand side, s, is not a combination of valid defined nonterminals and terminals.
               ;  The following (E M P) of the right hand side, EMP, are not in your list of terminals or nonterminals.")
               )
  ; The list of rules in delta must contain only distinct elements
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb)
                           (S ,ARROW aSb))
                         'S)
               (format "Step three of the design recipe has not been successfully completed.
The following values, ((S -> aSb)), are duplicated in the given rules: ((S -> ε) (S -> aSb) (S -> aSb))"))

  ; Start state errors
  ; The start state must be a valid state symbol
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         5)
               (format "Step three of the design recipe has not been successfully completed.
The given starting state: 5 is not a valid state"))
  ; If the input valid state is type-valid, it must be in the list of states
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'C)
               (format "Step three of the design recipe has not been successfully completed.
The following starting state, C, is not in the given list of states: (S)"))

  ; Accepts/Rejects errors
  ; The input to #:accepts must be a list
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:accepts 'abab)
               (format "Step four of the design recipe has not been successfully completed.
The expected accepts is not a list of words: abab"))
  ; The input to #:accepts must be a list of words (where a word is a list of symbols)
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:accepts '(1 2 3 4 5))
               (format "Step four of the design recipe has not been successfully completed.
The expected accepts is not a list of words: (1 2 3 4 5)"))
  ; Each word in #:accepts must contain only symbols from the grammar alphabet (sigma)
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:accepts '((a b) (c d e f g)))
               (format "Step four of the design recipe has not been successfully completed.
The following words in the accepts list contain symbols not included in sigma: ((c d e f g))"))
  ; Words in the #:accepts list that are not in the language of the grammar are returned
  ; in the error message
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:accepts '((a b) (b)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed grammar does not derive the following words it should derive: ((b))"))
  
  ; The input to #:rejects must be a list
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:rejects 'abab)
               (format "Step four of the design recipe has not been successfully completed.
The expected rejects is not a list of words: abab"))
  ; The input to #:rejects must be a list of words (where a word is a list of symbols)
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:rejects '(1 2 3 4 5))
               (format "Step four of the design recipe has not been successfully completed.
The expected rejects is not a list of words: (1 2 3 4 5)"))
  ; Each word in #:rejects must contain only symbols from the grammar alphabet (sigma)
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:rejects '((a b) (c d e f g)))
               (format "Step four of the design recipe has not been successfully completed.
The following words in the rejects list contain symbols not included in sigma: ((c d e f g))"))
  ; Words in the #:rejects list that are in the language of the grammar are returned
  ; in the error message
  (check-error (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S
                         #:rejects '((a b) (b) (a a b b)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed grammar derives the following words it should not derive: ((a b) (a a b b))"))

  (test)
  )
