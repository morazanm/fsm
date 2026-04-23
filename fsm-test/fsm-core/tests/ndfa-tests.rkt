#lang racket/base
(module+ test
  (require "../../../fsm-core/private/ndfa-constructors.rkt"
           rackunit)

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1\\) are not valid machine states in the given list of machine states: \\(A B C 1\\)"
   (lambda ()
     (make-ndfa '(A B C 1)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1 2\\) are not valid machine states in the given list of machine states: \\(A B C 1 2\\)"
   (lambda () (make-ndfa '(A B C 1 2)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          )) )
  ;;Invalid letter
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a\\) are not valid machine states in the given list of machine states: \\(A B C a\\)"
   (lambda () (make-ndfa '(A B C a)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a b\\) are not valid machine states in the given list of machine states: \\(A B C a b\\)"
   (lambda ()
     (make-ndfa '(A B C a b)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          ) ))
  ;;Duplicate states
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(A\\), are duplicated in the given states: \\(A B C A\\)"
   (lambda () (make-ndfa '(A B C A)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(A\\), are duplicated in the given states: \\(A B C A A\\)"
   (lambda ()
     (make-ndfa '(A B C A A)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          )) )
  
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(A B\\), are duplicated in the given states: \\(A B B C A A\\)"
   (lambda () (make-ndfa '(A B B C A A)
                          '(a b c d)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          )) )
  ;;SIGMA
  ;invalid letter
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following: \\(AA\\) are not valid alphanumeric symbols in the given input alphabet: \\(a b c AA\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c AA)
                          'A
                          '(B C)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following: \\(AA BB\\) are not valid alphanumeric symbols in the given input alphabet: \\(a b c AA BB\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c AA BB)
                          'A
                          '(B C)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  ;duplicate letter
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b c a\\)"
   (lambda ()
     (make-ndfa '(A B C D)
                          '(a b c a)
                          'A
                          '(B C)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b c a a\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c a a)
                          'A
                          '(B C)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a b\\), are duplicated in the given sigma: \\(a b c a b\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c a b)
                          'A
                          '(B C)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  ;;FINALS
  ;invalid final state
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1\\) are not valid machine states in the given list of machine finals: \\(B C 1\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C 1)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1 2\\) are not valid machine states in the given list of machine finals: \\(B C 1 2\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C 1 2)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  ;invalid letter
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a\\) are not valid machine states in the given list of machine finals: \\(B C a\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C a)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a a\\) are not valid machine states in the given list of machine finals: \\(B C a a\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C a a)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a b\\) are not valid machine states in the given list of machine finals: \\(B C a b\\)"
   (lambda ()
     (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C a b)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  ;not in list of states
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following final states, \\(F\\), are not in your list of states: \\(A B C D\\)"
   (lambda ()
     (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C F)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following final states, \\(F G\\), are not in your list of states: \\(A B C D\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C F G)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  ;duplicates
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(C\\), are duplicated in the given final states: \\(B C C\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C C)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(B C\\), are duplicated in the given final states: \\(B C C B\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'A
                          '(B C C B)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )

  ;;START STATE
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: 1 is not a valid state"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          '1
                          '(B C C B)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: a is not a valid state"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'a
                          '(B C C B)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: \\(A\\) is not a valid state"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          '(A)
                          '(B C C B)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: \\(A B C D\\)"
   (lambda () (make-ndfa '(A B C D)
                          '(a b c d)
                          'F
                          '(B C C B)
                          `((A b C)
                            (A c D)
                            (B c D)
                            (B a B))
                          )) )

  ;; Accepts/Rejects

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following words in the accepts list contain symbols not included in sigma: \\(\\(c\\)\\)"
   (lambda () (make-ndfa '(A B)
                          '(a b)
                          'A
                          '(A)
                          '((A a A)
                            (B b B)
                            (A b B)
                            (B a B))
                          #:accepts '((c))))
               )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The expected accepts is not a list of words: \\(c\\)"
   (lambda () (make-ndfa '(A B)
                          '(a b)
                          'A
                          '(A)
                          '((A a A)
                            (B b B)
                            (A b B)
                            (B a B))
                          #:accepts '(c)))
               )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following words in the rejects list contain symbols not included in sigma: \\(\\(c\\)\\)"
   (lambda () (make-ndfa '(A B)
                          '(a b)
                          'A
                          '(A)
                          '((A a A)
                            (B b B)
                            (A b B)
                            (B a B))
                          #:rejects '((c))))
               )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The expected rejects is not a list of words: \\(c\\)"
   (lambda () (make-ndfa '(A B)
                          '(a b)
                          'A
                          '(A)
                          '((A a A)
                            (B b B)
                            (A b B)
                            (B a B))
                          #:rejects '(c)))
               )

  (check-exn
   #rx"Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words: \\(\\(a a b a a a\\)\\)"
   (lambda () (make-ndfa '(A B)
                          '(a b)
                          'A
                          '(A)
                          '((A a A)
                            (B b B)
                            (A b B)
                            (B a B))
                          #:accepts '((a a b a a a))
                          #:rejects '((b a a a) (a a a a b a))))
               )

  (check-exn
   #rx"Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words: \\(\\(a a a\\)\\)"
   (lambda ()
     (make-ndfa '(A B)
                          '(a b)
                          'A
                          '(A)
                          '((A a A)
                            (B b B)
                            (A b B)
                            (B a B))
                          #:accepts '((a a a a a))
                          #:rejects '((a a a) (a a a a b a))))
               )

  )
