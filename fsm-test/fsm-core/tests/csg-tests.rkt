(module csg-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  (provide)

  ; Context Sensitive Grammars
  (define anbn (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S))
  ; States errors
  ; States must be a list
  (check-error (make-csg 'S
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S)
               (format "Step two of the design recipe has not been successfully completed.
The given nonterminals must be a list: S"))
  ; The list must contain only valid state symbols
  (check-error (make-csg '(S A 6)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S)
               (format "Step two of the design recipe has not been successfully completed.
The following: (6) are not valid nonterminals in the given list of nonterminals: (S A 6)"))
  ; The list must contain only distinct elements
  (check-error (make-csg '(S A S)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S)
               (format "Step two of the design recipe has not been successfully completed.\nThe following values, (S), are duplicated in the given nonterminals: (S A S)"))

  ; Sigma errors
  ; Sigma must be a list
  (check-error (make-csg '(S A B)
                         'ab
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S)
               (format "Step one of the design recipe has not been successfully completed.
The given grammar alphabet must be a list: ab"))
  ; The list must contain only valid lowercase alphabet symbols
  (check-error (make-csg '(S A B)
                         '(a B)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S)
               (format "Step one of the design recipe has not been successfully completed.
The following: (B) are not valid lowercase alphabet letters in the given input alphabet: (a B)"))
  ; The list must contain only distinct elements
  (check-error (make-csg '(S A B)
                         '(a b b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S)
               (format "Step one of the design recipe has not been successfully completed.
The following values, (b), are duplicated in the given sigma: (a b b)"))

  ; Delta errors
  ; Delta must be a list
  (check-error (make-csg '(S A B)
                         '(a b)
                         'A
                         'S)
               (format "Step three of the design recipe has not been successfully completed.
The given grammar nonterminals must be a list: A"))
  ; Each rule in delta must have the correct structure (symbol -> symbol)
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((5 6 7))
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
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (s ,ARROW EMP)
                           (B ,ARROW A))
                         'S)
               ; This is the expected error message, but check-error isn't comparing it correctly.
               ;               (format "Step four of the design recipe has not been successfully completed.
               ;The following rules have errors, which make them invalid:
               ;Rule (s -> EMP):
               ;  The following members (s) of the left hand side, s, is not a combination of valid defined nonterminals and terminals.
               ;  The following (E M P) of the right hand side, EMP, are not in your list of terminals or nonterminals.")
               )
  ; The list of rules in delta must contain only distinct elements
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A)
                           (B, ARROW A))
                         'S)
               (format "Step three of the design recipe has not been successfully completed.
The following values, ((B -> A)), are duplicated in the given rules: ((S -> AAaAAB) (AAaAAA -> aSb) (AAaAAA -> ε) (B -> A) (B -> A))"))

  ; Start state errors
  ; The start state must be a valid state symbol
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         5)
               (format "Step three of the design recipe has not been successfully completed.
The given starting state: 5 is not a valid state"))
  ; If the input valid state is type-valid, it must be in the list of states
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'C)
               (format "Step three of the design recipe has not been successfully completed.
The following starting state, C, is not in the given list of states: (S A B)"))

  ; Accepts/Rejects errors
  ; The input to #:accepts must be a list
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S
                         #:accepts 'abab)
               (format "Step two of the design recipe has not been successfully completed.
The expected accepts is not a list of words: abab"))
  ; The input to #:accepts must be a list of words (where a word is a list of symbols)
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S
                         #:accepts '(1 2 3 4 5))
               (format "Step two of the design recipe has not been successfully completed.
The expected accepts is not a list of words: (1 2 3 4 5)"))
  ; Each word in #:accepts must contain only symbols from the grammar alphabet (sigma)
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S
                         #:accepts '((a b a b) (c d e f g)))
               (format "Step two of the design recipe has not been successfully completed.
The following words in the accepts list contain symbols not included in sigma: ((c d e f g))"))
  ; The input to #:rejects must be a list
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S
                         #:rejects 'abab)
               (format "Step two of the design recipe has not been successfully completed.
The expected rejects is not a list of words: abab"))
  ; The input to #:rejects must be a list of words (where a word is a list of symbols)
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S
                         #:rejects '(1 2 3 4 5))
               (format "Step two of the design recipe has not been successfully completed.
The expected rejects is not a list of words: (1 2 3 4 5)"))
  ; Each word in #:rejects must contain only symbols from the grammar alphabet (sigma)
  (check-error (make-csg '(S A B)
                         '(a b)
                         `((S ,ARROW AAaAAB)
                           (AAaAAA ,ARROW aSb)
                           (AAaAAA ,ARROW ,EMP)
                           (B ,ARROW A))
                         'S
                         #:rejects '((a b a b) (c d e f g)))
               (format "Step two of the design recipe has not been successfully completed.
The following words in the rejects list contain symbols not included in sigma: ((c d e f g))"))

  (test)
  )
