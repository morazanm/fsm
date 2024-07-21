(module mttm-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-error (make-mttm '(S Y N 1)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (1) are not valid machine states in the given list of machine states: (S Y N 1)"))
  (check-error (make-mttm '(S Y N 1 2)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (1 2) are not valid machine states in the given list of machine states: (S Y N 1 2)"))
  ;;Invalid letter
  (check-error (make-mttm '(S Y N a)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (a) are not valid machine states in the given list of machine states: (S Y N a)"))
  (check-error (make-mttm '(S Y N a b)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (a b) are not valid machine states in the given list of machine states: (S Y N a b)"))
  ;;Duplicate states
  (check-error (make-mttm '(S Y N S)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following values, (S), are duplicated in the given states: (S Y N S)"))
  (check-error (make-mttm '(S Y N S S)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following values, (S), are duplicated in the given states: (S Y N S S)"))
  (check-error (make-mttm '(S Y Y N S S)
                           `(a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following values, (S Y), are duplicated in the given states: (S Y Y N S S)"))
  ;;SIGMA
  ;invalid number
  (check-error (make-mttm '(S Y N)
                           `(a b 1)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following: (1) are not valid alphabet letters in the given input alphabet: (a b 1)"))
  (check-error (make-mttm '(S Y N)
                           `(a b 1 2)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following: (1 2) are not valid alphabet letters in the given input alphabet: (a b 1 2)"))
  ;invalid letter
  (check-error (make-mttm '(S Y N)
                           `(a b 1)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following: (1) are not valid alphabet letters in the given input alphabet: (a b 1)"))
  (check-error (make-mttm '(S Y N)
                           `(a b 1 2)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following: (1 2) are not valid alphabet letters in the given input alphabet: (a b 1 2)"))
  ;duplicate letter
  (check-error (make-mttm '(S Y N)
                           `(a b a)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b a)"))
  (check-error (make-mttm '(S Y N)
                           `(a b a a)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b a a)"))
  (check-error (make-mttm '(S Y N)
                           `(a b a b)
                           'S
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step one of the design recipe has not been successfully completed.
The following values, (a b), are duplicated in the given sigma: (a b a b)"))
  ;;FINALS
  ;invalid final state
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N 1)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (1) are not valid machine states in the given list of machine finals: (Y N 1)"))
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N 1 2)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (1 2) are not valid machine states in the given list of machine finals: (Y N 1 2)"))
  ;invalid letter
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N a)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (a) are not valid machine states in the given list of machine finals: (Y N a)"))
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N a a)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (a a) are not valid machine states in the given list of machine finals: (Y N a a)"))
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N a b)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following: (a b) are not valid machine states in the given list of machine finals: (Y N a b)"))
  ;not in list of states
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N F)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following final states, (F), are not in your list of states: (S Y N)"))
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N F G)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following final states, (F G), are not in your list of states: (S Y N)"))
  ;duplicates
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following values, (N), are duplicated in the given final states: (Y N N)"))
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'S
                           '(Y N N Y)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following values, (Y N), are duplicated in the given final states: (Y N N Y)"))

  ;;START STATE
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           '1
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The given starting state: 1 is not a valid state"))
  
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'a
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The given starting state: a is not a valid state"))
  
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           '(A)
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The given starting state: (A) is not a valid state"))
  
  (check-error (make-mttm '(S Y N)
                           `(a b)
                           'F
                           '(Y N)
                           (list
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'S (list 'a BLANK BLANK BLANK))
                                  (list 'Y (list 'a 'a BLANK BLANK)))
                            (list (list 'S (list 'a 'a BLANK BLANK))
                                  (list 'Y (list RIGHT RIGHT BLANK BLANK))))
                           4
                           'Y
                           ) (format "Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: (S Y N)"))

  ;; Accepts/Rejects

  (check-error (make-mttm '(S Y N C D E F G)
                           '(a b c)
                           'S
                           '(Y N)
                           (list ;; read all blanks and move all R
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'C (list 'a BLANK BLANK BLANK))
                                  (list 'D (list 'a 'a BLANK BLANK)))
                            (list (list 'D (list 'a 'a BLANK BLANK))
                                  (list 'C (list RIGHT RIGHT BLANK BLANK)))
                            ;; read b on t0, copy to t2 and then move R on t0 and t2
                            (list (list 'C (list 'b BLANK BLANK BLANK))
                                  (list 'E (list 'b BLANK 'b BLANK)))
                            (list (list 'E (list 'b BLANK 'b BLANK))
                                  (list 'C (list RIGHT BLANK RIGHT BLANK)))
                            ;; read c on t0, copy to t3 and then move R on t0 and t3
                            (list (list 'C (list 'c BLANK BLANK BLANK))
                                  (list 'F (list 'c BLANK BLANK 'c)))
                            (list (list 'F (list 'c BLANK BLANK 'c))
                                  (list 'C (list RIGHT BLANK BLANK RIGHT)))
                            ;; read BLANK on t0, move L on t1, t2 and t3
                            (list (list 'C (list BLANK BLANK BLANK BLANK))
                                  (list 'G (list BLANK LEFT LEFT LEFT)))
                            ;; read BLANK on all tapes, move to Y
                            (list (list 'G (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list BLANK BLANK BLANK BLANK)))
                            ;; read a, b, c on t1, t2, and t3 them move L on t1, t2, t3
                            (list (list 'G (list BLANK 'a 'b 'c))
                                  (list 'G (list BLANK LEFT LEFT LEFT)))
                            ;; too many of at least 1 letter
                            (list (list 'G (list BLANK BLANK 'b 'c))
                                  (list 'N (list BLANK BLANK 'b 'c)))
                            (list (list 'G (list BLANK 'a BLANK 'c))
                                  (list 'N (list BLANK 'a BLANK 'c)))
                            (list (list 'G (list BLANK 'a 'b BLANK))
                                  (list 'N (list BLANK 'a 'b BLANK)))
                            (list (list 'G (list BLANK BLANK BLANK 'c))
                                  (list 'N (list BLANK BLANK BLANK 'c)))
                            (list (list 'G (list BLANK BLANK 'b BLANK))
                                  (list 'N (list BLANK BLANK 'b BLANK)))
                            (list (list 'G (list BLANK 'a BLANK BLANK))
                                  (list 'N (list BLANK 'a BLANK BLANK))))
                           4
                           'Y
                           #:accepts `((,LM ,BLANK a b c c))
                           #:rejects `((,LM ,BLANK a b c)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words: ((@ _ a b c c))"))

  (check-error (make-mttm '(S Y N C D E F G)
                           '(a b c)
                           'S
                           '(Y N)
                           (list ;; read all blanks and move all R
                            (list (list 'S (list BLANK BLANK BLANK BLANK))
                                  (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
                            ;; read a on t0, copy to t1 and then move R on t0 and t1
                            (list (list 'C (list 'a BLANK BLANK BLANK))
                                  (list 'D (list 'a 'a BLANK BLANK)))
                            (list (list 'D (list 'a 'a BLANK BLANK))
                                  (list 'C (list RIGHT RIGHT BLANK BLANK)))
                            ;; read b on t0, copy to t2 and then move R on t0 and t2
                            (list (list 'C (list 'b BLANK BLANK BLANK))
                                  (list 'E (list 'b BLANK 'b BLANK)))
                            (list (list 'E (list 'b BLANK 'b BLANK))
                                  (list 'C (list RIGHT BLANK RIGHT BLANK)))
                            ;; read c on t0, copy to t3 and then move R on t0 and t3
                            (list (list 'C (list 'c BLANK BLANK BLANK))
                                  (list 'F (list 'c BLANK BLANK 'c)))
                            (list (list 'F (list 'c BLANK BLANK 'c))
                                  (list 'C (list RIGHT BLANK BLANK RIGHT)))
                            ;; read BLANK on t0, move L on t1, t2 and t3
                            (list (list 'C (list BLANK BLANK BLANK BLANK))
                                  (list 'G (list BLANK LEFT LEFT LEFT)))
                            ;; read BLANK on all tapes, move to Y
                            (list (list 'G (list BLANK BLANK BLANK BLANK))
                                  (list 'Y (list BLANK BLANK BLANK BLANK)))
                            ;; read a, b, c on t1, t2, and t3 them move L on t1, t2, t3
                            (list (list 'G (list BLANK 'a 'b 'c))
                                  (list 'G (list BLANK LEFT LEFT LEFT)))
                            ;; too many of at least 1 letter
                            (list (list 'G (list BLANK BLANK 'b 'c))
                                  (list 'N (list BLANK BLANK 'b 'c)))
                            (list (list 'G (list BLANK 'a BLANK 'c))
                                  (list 'N (list BLANK 'a BLANK 'c)))
                            (list (list 'G (list BLANK 'a 'b BLANK))
                                  (list 'N (list BLANK 'a 'b BLANK)))
                            (list (list 'G (list BLANK BLANK BLANK 'c))
                                  (list 'N (list BLANK BLANK BLANK 'c)))
                            (list (list 'G (list BLANK BLANK 'b BLANK))
                                  (list 'N (list BLANK BLANK 'b BLANK)))
                            (list (list 'G (list BLANK 'a BLANK BLANK))
                                  (list 'N (list BLANK 'a BLANK BLANK))))
                           4
                           'Y
                           #:accepts `(((,LM ,BLANK a a b b c c) 1))
                           #:rejects `(((,LM ,BLANK a b c) 1)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words: (((@ _ a b c) 1))"))
  
  ;;RULES
  
  (test)

  )
