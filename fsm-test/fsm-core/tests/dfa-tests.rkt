(module dfa-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;;valid-listof/c tests
  ;;STATES
  ;not a list
  (check-error (make-dfa 'A
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The given machine states must be a list: A"))
  ;;Invalid number
  (check-error (make-dfa '(A B C 1)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (1) are not valid machine states in the given list of machine states: (A B C 1)"))
  (check-error (make-dfa '(A B C 1 2)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (1 2) are not valid machine states in the given list of machine states: (A B C 1 2)"))
  ;;Invalid letter
  (check-error (make-dfa '(A B C a)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (a) are not valid machine states in the given list of machine states: (A B C a)"))
  (check-error (make-dfa '(A B C a b)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (a b) are not valid machine states in the given list of machine states: (A B C a b)"))
  ;;Duplicate states
  (check-error (make-dfa '(A B C A)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following values, (A), are duplicated in the given states: (A B C A)"))
  (check-error (make-dfa '(A B C A A)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following values, (A), are duplicated in the given states: (A B C A A)"))
  (check-error (make-dfa '(A B B C A A)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c C)
                           (B c B)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following values, (A B), are duplicated in the given states: (A B B C A A)"))
  ;;SIGMA
  ;invalid number
  (check-error (make-dfa '(A B C D)
                         '(a b c 1)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following: (1) are not valid lowercase alphabet letters in the given input alphabet: (a b c 1)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c 1 2)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following: (1 2) are not valid lowercase alphabet letters in the given input alphabet: (a b c 1 2)"))
  ;invalid letter
  (check-error (make-dfa '(A B C D)
                         '(a b c A)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following: (A) are not valid lowercase alphabet letters in the given input alphabet: (a b c A)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c A B)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following: (A B) are not valid lowercase alphabet letters in the given input alphabet: (a b c A B)"))
  ;duplicate letter
  (check-error (make-dfa '(A B C D)
                         '(a b c a)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b c a)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c a a)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b c a a)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c a b)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step one of the design recipe has not been successfully completed.
The following values, (a b), are duplicated in the given sigma: (a b c a b)"))
  ;;FINALS
  ;invalid final state
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C 1)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (1) are not valid machine states in the given list of machine finals: (B C 1)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C 1 2)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (1 2) are not valid machine states in the given list of machine finals: (B C 1 2)"))
  ;invalid letter
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C a)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (a) are not valid machine states in the given list of machine finals: (B C a)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C a a)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (a a) are not valid machine states in the given list of machine finals: (B C a a)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C a b)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following: (a b) are not valid machine states in the given list of machine finals: (B C a b)"))
  ;not in list of states
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C F)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following final states, (F), are not in your list of states: (A B C D)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C F G)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following final states, (F G), are not in your list of states: (A B C D)"))
  ;duplicates
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following values, (C), are duplicated in the given final states: (B C C)"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C C B)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following values, (B C), are duplicated in the given final states: (B C C B)"))

  ;;START STATE
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         '1
                         '(B C C B)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The given starting state: 1 is not a valid state"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'a
                         '(B C C B)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The given starting state: a is not a valid state"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         '(A)
                         '(B C C B)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The given starting state: (A) is not a valid state"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'F
                         '(B C C B)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B)))
               (format "Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: (A B C D)"))

  ;;RULES
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C)
                         'A)
               (format "Step four of the design recipe has not been successfully completed.
The given machine rules must be a list: A"))

  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C)
                         '(A))
               (format "Step four of the design recipe has not been successfully completed.
The following rules have structural errors:
Rule A:
  The given rule, A, does not have the correct structure. A DFA rule must be a list with three elements.
"))
  (check-error (make-dfa '(A B C D)
                         '(a b c d)
                         'A
                         '(B C)
                         '((A b B)
                           (A b B)))
               (format "Step four of the design recipe has not been successfully completed.
The following state/sigma pairs are duplicated in your rules:  (
(A b))")
               )

  ;; Accepts/Rejects errors
  (check-error (make-dfa '(A B)
                         '(a b)
                         'A
                         '(A)
                         '((A a A)
                           (B b B)
                           (A b B)
                           (B a B))
                         #:accepts '((c)))
               (format "Step two of the design recipe has not been successfully completed.
The following words in the accepts list contain symbols not included in sigma: ((c))"))

  (check-error (make-dfa '(A B)
                         '(a b)
                         'A
                         '(A)
                         '((A a A)
                           (B b B)
                           (A b B)
                           (B a B))
                         #:accepts '(c)
               (format "Step two of the design recipe has not been successfully completed.
The expected accepts is not a list of words")))
  
  (check-error (make-dfa '(A B)
                         '(a b)
                         'A
                         '(A)
                         '((A a A)
                           (B b B)
                           (A b B)
                           (B a B))
                         #:rejects '((c)))
               (format "Step two of the design recipe has not been successfully completed.
The following words in the rejects list contain symbols not included in sigma: ((c))"))

  (check-error (make-dfa '(A B)
                         '(a b)
                         'A
                         '(A)
                         '((A a A)
                           (B b B)
                           (A b B)
                           (B a B))
                         #:rejects '(c)
               (format "Step two of the design recipe has not been successfully completed.
The expected rejects is not a list of words")))
  
  (check-error (make-dfa '(A B)
                         '(a b)
                         'A
                         '(A)
                         '((A a A)
                           (B b B)
                           (A b B)
                           (B a B))
                         #:accepts '((a a b a a a))
                         #:rejects '((b a a a) (a a a a b a)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words: ((a a b a a a))"))

  (check-error (make-dfa '(A B)
                         '(a b)
                         'A
                         '(A)
                         '((A a A)
                           (B b B)
                           (A b B)
                           (B a B))
                         #:accepts '((a a a a a))
                         #:rejects '((a a a) (a a a a b a)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words: ((a a a))"))
  
  (test)

  )
