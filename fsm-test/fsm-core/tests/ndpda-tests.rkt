#lang racket/base
(module+ test
  (require "../../../fsm-core/private/pda-constructors.rkt"
           "../../../fsm-core/private/constants.rkt"
           rackunit
           )
  
  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1\\) are not valid machine states in the given list of machine states: \\(A B C 1\\)"
   (lambda () (make-ndpda '(A B C 1)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1 2\\) are not valid machine states in the given list of machine states: \\(A B C 1 2\\)"
   (lambda () (make-ndpda '(A B C 1 2)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          ))
   )
  ;;Invalid letter
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a\\) are not valid machine states in the given list of machine states: \\(A B C a\\)"
   (lambda () (make-ndpda '(A B C a)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          )) )
  
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a b\\) are not valid machine states in the given list of machine states: \\(A B C a b\\)"
   (lambda () (make-ndpda '(A B C a b)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          ))
   )
  ;;Duplicate states
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(A\\), are duplicated in the given states: \\(A B C A\\)"
   (lambda () (make-ndpda '(A B C A)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C)
                          `((A b C)
                            (A c C)
                            (B c B)
                            (B a B))
                          )))
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(A\\), are duplicated in the given states: \\(A B C A A\\)"
   (lambda () (make-ndpda '(A B C A A)
                          '(a b c d)
                          '(f g)
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
   (lambda () (make-ndpda '(A B B C A A)
                          '(a b c d)
                          '(f g)
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
   (lambda () (make-ndpda '(A B C D)
                          '(a b c AA)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following: \\(AA BB\\) are not valid alphanumeric symbols in the given input alphabet: \\(a b c AA BB\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c AA BB)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (g)) (B (f)))
                          ))
   )
  ;duplicate letter
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b c a\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c a)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (g)) (B (f)))
                          )) )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b c a a\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c a a)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (g)) (B (f)))
                          )) )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a b\\), are duplicated in the given sigma: \\(a b c a b\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c a b)
                          '(f g)
                          'A
                          '(B C)
                          `((A a (g)) (B (f)))
                          ))
   )
  ;;FINALS
  ;invalid final state
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1\\) are not valid machine states in the given list of machine finals: \\(B C 1\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C 1)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1 2\\) are not valid machine states in the given list of machine finals: \\(B C 1 2\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C 1 2)
                          `((A a (g)) (B (f)))
                          ))
   )
  ;invalid letter
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a\\) are not valid machine states in the given list of machine finals: \\(B C a\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C a)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a a\\) are not valid machine states in the given list of machine finals: \\(B C a a\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C a a)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a b\\) are not valid machine states in the given list of machine finals: \\(B C a b\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C a b)
                          `((A a (g)) (B (f)))
                          ))
   )
  ;not in list of states
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following final states, \\(F\\), are not in your list of states: \\(A B C D\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C F)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following final states, \\(F G\\), are not in your list of states: \\(A B C D\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C F G)
                          `((A a (g)) (B (f)))
                          )) )
  ;duplicates
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(C\\), are duplicated in the given final states: \\(B C C\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C C)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(B C\\), are duplicated in the given final states: \\(B C C B\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'A
                          '(B C C B)
                          `((A a (g)) (B (f)))
                          )) )

  ;;START STATE
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: 1 is not a valid state"
   (lambda ()
     (make-ndpda '(A B C D)
                 '(a b c d)
                 '(f g)
                 '1
                 '(B C C B)
                 `((A a (g)) (B (f)))
                 ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: a is not a valid state"
   (lambda ()
     (make-ndpda '(A B C D)
                 '(a b c d)
                 '(f g)
                 'a
                 '(B C C B)
                 `((A a (g)) (B (f)))
                 )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: (A) is not a valid state"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          '(A)
                          '(B C C B)
                          `((A a (g)) (B (f)))
                          ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: \\(A B C D\\)"
   (lambda () (make-ndpda '(A B C D)
                          '(a b c d)
                          '(f g)
                          'F
                          '(B C C B)
                          `((A a (g)) (B (f)))
                          )) )

  ;; Accepts/Rejects

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following words in the accepts list contain symbols not included in sigma: \\(\\(d\\)\\)"
   (lambda () (make-ndpda '(S P Q F)
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
                          #:accepts '((d))))
   )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The expected accepts is not a list of words: \\(a\\)"
   (lambda ()
     (make-ndpda '(S P Q F)
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
                 #:accepts '(a)))
   )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following words in the rejects list contain symbols not included in sigma: \\(\\(d\\)\\)"
   (lambda () (make-ndpda '(S P Q F)
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
                          #:rejects '((d))))
   )

  (check-exn
   "Step two of the design recipe has not been successfully completed.
The expected rejects is not a list of words: \\(a\\)"
   (lambda () (make-ndpda '(S P Q F)
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
                          #:rejects '(a)))
   )

  (check-exn
   #rx"Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words: \\(\\(a a a a\\)\\)"
   (lambda () (make-ndpda '(S P Q F)
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
                          #:rejects '((b b b b))))
   )
  
  (check-exn
   #rx"Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words: \\(\\(c\\)\\)"
   (lambda ()
     (make-ndpda '(S P Q F)
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
                 #:rejects '((c))))
   )
  )
