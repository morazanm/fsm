
(module ndpda-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-error (make-ndpda '(A B C 1)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (1) are not valid machine states in the given list of machine states: (A B C 1)"))
  (check-error (make-ndpda '(A B C 1 2)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (1 2) are not valid machine states in the given list of machine states: (A B C 1 2)"))
  ;;Invalid letter
  (check-error (make-ndpda '(A B C a)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (a) are not valid machine states in the given list of machine states: (A B C a)"))
  (check-error (make-ndpda '(A B C a b)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (a b) are not valid machine states in the given list of machine states: (A B C a b)"))
  ;;Duplicate states
  (check-error (make-ndpda '(A B C A)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (A), are duplicated in the given states:  (A B C A)"))
  (check-error (make-ndpda '(A B C A A)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (A), are duplicated in the given states:  (A B C A A)"))
  (check-error (make-ndpda '(A B B C A A)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C)
                            `((A b C)
                              (A c C)
                              (B c B)
                              (B a B))
                            ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (A B), are duplicated in the given states:  (A B B C A A)"))
  ;;SIGMA
  ;invalid number
  (check-error (make-ndpda '(A B C D)
                            '(a b c 1)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe was not successfully completed.
The following: (1) are not valid lowercase alphabet letters in the given input alphabet: (a b c 1)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c 1 2)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe was not successfully completed.
The following: (1 2) are not valid lowercase alphabet letters in the given input alphabet: (a b c 1 2)"))
  ;invalid letter
  (check-error (make-ndpda '(A B C D)
                            '(a b c A)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe was not successfully completed.
The following: (A) are not valid lowercase alphabet letters in the given input alphabet: (a b c A)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c A B)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe was not successfully completed.
The following: (A B) are not valid lowercase alphabet letters in the given input alphabet: (a b c A B)"))
  ;duplicate letter
  (check-error (make-ndpda '(A B C D)
                            '(a b c a)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe has not been sucessfully completed.
There following values, (a), are duplicated in the given sigma:  (a b c a)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c a a)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe has not been sucessfully completed.
There following values, (a), are duplicated in the given sigma:  (a b c a a)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c a b)
                            '(f g)
                            'A
                            '(B C)
                            `((A a (g)) (B (f)))
                            ) (format "Step one of the design recipe has not been sucessfully completed.
There following values, (a b), are duplicated in the given sigma:  (a b c a b)"))
  ;;FINALS
  ;invalid final state
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C 1)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (1) are not valid machine states in the given list of machine finals: (B C 1)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C 1 2)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (1 2) are not valid machine states in the given list of machine finals: (B C 1 2)"))
  ;invalid letter
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C a)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (a) are not valid machine states in the given list of machine finals: (B C a)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C a a)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (a a) are not valid machine states in the given list of machine finals: (B C a a)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C a b)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The following: (a b) are not valid machine states in the given list of machine finals: (B C a b)"))
  ;not in list of states
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C F)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe has not been successfully completed.
The following final states, (F), are not in your list of states: (A B C D)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C F G)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe has not been successfully completed.
The following final states, (F G), are not in your list of states: (A B C D)"))
  ;duplicates
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C C)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (C), are duplicated in the given final states:  (B C C)"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'A
                            '(B C C B)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (B C), are duplicated in the given final states:  (B C C B)"))

  ;;START STATE
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            '1
                            '(B C C B)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The given starting state: 1 is not a valid state"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'a
                            '(B C C B)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The given starting state: a is not a valid state"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            '(A)
                            '(B C C B)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe was not successfully completed.
The given starting state: (A) is not a valid state"))
  (check-error (make-ndpda '(A B C D)
                            '(a b c d)
                            '(f g)
                            'F
                            '(B C C B)
                            `((A a (g)) (B (f)))
                            ) (format "Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: (A B C D)"))

  ;;RULES

  (check-error (make-ndpda '(S P Q F)
                            '(a b c)
                            '(a b)
                            'S
                            '(F)
                            `(((S ,EMP ,EMP) (P ,EMP))
                              ((P a ,EMP) (P (a)))
                              ((P b ,EMP) (P (b)))
                              ((P c ,EMP) (Q ,EMP))
                              ((Q a (a)) (Q ,EMP))
                              ((Q b (b)) (Q ,EMP))
                              ((Q ,EMP ,EMP) (F ,EMP)))
                            #:accepts '((a a a a))
                            #:rejects '((b b b b)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words:  ((a a a a))"))
  
  (check-error (make-ndpda '(S P Q F)
                            '(a b c)
                            '(a b)
                            'S
                            '(F)
                            `(((S ,EMP ,EMP) (P ,EMP))
                              ((P a ,EMP) (P (a)))
                              ((P b ,EMP) (P (b)))
                              ((P c ,EMP) (Q ,EMP))
                              ((Q a (a)) (Q ,EMP))
                              ((Q b (b)) (Q ,EMP))
                              ((Q ,EMP ,EMP) (F ,EMP)))
                            #:accepts '((c))
                            #:rejects '((c)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words:  ((c))"))
  
  (test)

  )
