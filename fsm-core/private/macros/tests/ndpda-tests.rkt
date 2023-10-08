(module ndpda-tests racket
  (require "../constructors.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-error (make-ndpda2 '(A B C 1)
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
  (check-error (make-ndpda2 '(A B C 1 2)
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
  (check-error (make-ndpda2 '(A B C a)
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
  (check-error (make-ndpda2 '(A B C a b)
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
  (check-error (make-ndpda2 '(A B C A)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                           ) (format "Step three of the design recipe has not been sucessfully completed.
There following values are duplicated in the given states:  (A)"))
  (check-error (make-ndpda2 '(A B C A A)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                           ) (format "Step three of the design recipe has not been sucessfully completed.
There following values are duplicated in the given states:  (A)"))
  (check-error (make-ndpda2 '(A B B C A A)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                           ) (format "Step three of the design recipe has not been sucessfully completed.
There following values are duplicated in the given states:  (A B)"))
  ;;SIGMA
  ;invalid number
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c 1)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe was not successfully completed.
 The following: (1) are not valid alphabet letters in the given input alphabet: (a b c 1)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c 1 2)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe was not successfully completed.
 The following: (1 2) are not valid alphabet letters in the given input alphabet: (a b c 1 2)"))
  ;invalid letter
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c A)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe was not successfully completed.
 The following: (A) are not valid alphabet letters in the given input alphabet: (a b c A)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c A B)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe was not successfully completed.
 The following: (A B) are not valid alphabet letters in the given input alphabet: (a b c A B)"))
  ;duplicate letter
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c a)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe has not been sucessfully completed.
There following values are duplicated in the given sigma:  (a)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c a a)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe has not been sucessfully completed.
There following values are duplicated in the given sigma:  (a)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c a b)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (f)) (A (f)))
                           ) (format "Step one of the design recipe has not been sucessfully completed.
There following values are duplicated in the given sigma:  (a b)"))
  ;;FINALS
  ;invalid final state
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C 1)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
 The following: (1) are not valid machine states in the given list of machine finals: (B C 1)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C 1 2)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
 The following: (1 2) are not valid machine states in the given list of machine finals: (B C 1 2)"))
  ;invalid letter
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C a)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
 The following: (a) are not valid machine states in the given list of machine finals: (B C a)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C a a)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
 The following: (a a) are not valid machine states in the given list of machine finals: (B C a a)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C a b)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
 The following: (a b) are not valid machine states in the given list of machine finals: (B C a b)"))
  ;not in list of states
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C F)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe has not been successfully completed.
The following final states are not in, (A B C D), your list of states: (F)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C F G)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe has not been successfully completed.
The following final states are not in, (A B C D), your list of states: (F G)"))
  ;duplicates
 (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C C)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe has not been sucessfully completed.
There following values are duplicated in the given final states:  (C)"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C C B)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe has not been sucessfully completed.
There following values are duplicated in the given final states:  (B C)"))

  ;;START STATE
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          '1
                          '(B C C B)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
For the given starting state: 1 is not a valid state"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'a
                          '(B C C B)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
For the given starting state: a is not a valid state"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          '(A)
                          '(B C C B)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe was not successfully completed.
For the given starting state: (A) is not a valid state"))
  (check-error (make-ndpda2 '(A B C D)
                          '(a b c d)
                          '(f g)
                          'F
                          '(B C C B)
                          `((A a (f)) (A (f)))
                           ) (format "Step three of the design recipe has not been successfully completed.
The following starting state is not in (A B C D), the given list of states: F"))

  ;;RULES
  
  (test)

  )
