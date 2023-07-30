(module main-tests racket
  (require "constructors.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
           )

  ;;valid-listof/c tests
  ;;STATES
  (check-error (make-dfa2 '(A B C 1)
                         '(a b c d)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B))
                         #t) "The following: (1) are not valid machine state(s), in the list of machine states: (A B C 1)")
  ;;SIGMA
  (check-error (make-dfa2 '(A B C D)
                         '(a b c 1)
                         'A
                         '(B C)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B))
                         #t) "The following: (1) are not valid alphabet letter(s), in the machine sigma: (a b c 1)")
  ;;FINALS
  (check-error (make-dfa2 '(A B C D)
                         '(a b c d)
                         'A
                         '(B C 1)
                         `((A b C)
                           (A c D)
                           (B c D)
                           (B a B))
                         #t) "The following: (1) are not valid machine state(s), in the list of machine finals: (B C 1)")
  
  (test)

  )
