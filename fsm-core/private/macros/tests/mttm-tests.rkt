(module mttm-tests racket
  (require "../constructors.rkt"
           "../../constants.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-error (make-mttm2 '(S Y N 1)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (1) are not valid machine states in the given list of machine states: (S Y N 1)"))
  (check-error (make-mttm2 '(S Y N 1 2)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (1 2) are not valid machine states in the given list of machine states: (S Y N 1 2)"))
  ;;Invalid letter
  (check-error (make-mttm2 '(S Y N a)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (a) are not valid machine states in the given list of machine states: (S Y N a)"))
  (check-error (make-mttm2 '(S Y N a b)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (a b) are not valid machine states in the given list of machine states: (S Y N a b)"))
  ;;Duplicate states
  (check-error (make-mttm2 '(S Y N S)
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
                         ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (S), are duplicated in the given states:  (S Y N S)"))
  (check-error (make-mttm2 '(S Y N S S)
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
                         ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (S), are duplicated in the given states:  (S Y N S S)"))
  (check-error (make-mttm2 '(S Y Y N S S)
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
                         ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (S Y), are duplicated in the given states:  (S Y Y N S S)"))
  ;;SIGMA
  ;invalid number
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step one of the design recipe was not successfully completed.
The following: (1) are not valid lowercase alphabet letters in the given input alphabet: (a b 1)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step one of the design recipe was not successfully completed.
The following: (1 2) are not valid lowercase alphabet letters in the given input alphabet: (a b 1 2)"))
  ;invalid letter
  (check-error (make-mttm2 '(S Y N)
                         `(a b A)
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
                         ) (format "Step one of the design recipe was not successfully completed.
The following: (A) are not valid lowercase alphabet letters in the given input alphabet: (a b A)"))
  (check-error (make-mttm2 '(S Y N)
                         `(a b A B)
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
                         ) (format "Step one of the design recipe was not successfully completed.
The following: (A B) are not valid lowercase alphabet letters in the given input alphabet: (a b A B)"))
  ;duplicate letter
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step one of the design recipe has not been sucessfully completed.
There following values, (a), are duplicated in the given sigma:  (a b a)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step one of the design recipe has not been sucessfully completed.
There following values, (a), are duplicated in the given sigma:  (a b a a)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step one of the design recipe has not been sucessfully completed.
There following values, (a b), are duplicated in the given sigma:  (a b a b)"))
  ;;FINALS
  ;invalid final state
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (1) are not valid machine states in the given list of machine finals: (Y N 1)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (1 2) are not valid machine states in the given list of machine finals: (Y N 1 2)"))
  ;invalid letter
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (a) are not valid machine states in the given list of machine finals: (Y N a)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (a a) are not valid machine states in the given list of machine finals: (Y N a a)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The following: (a b) are not valid machine states in the given list of machine finals: (Y N a b)"))
  ;not in list of states
  (check-error (make-mttm2 '(S Y N)
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
  (check-error (make-mttm2 '(S Y N)
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
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (N), are duplicated in the given final states:  (Y N N)"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe has not been sucessfully completed.
There following values, (Y N), are duplicated in the given final states:  (Y N N Y)"))

  ;;START STATE
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The given starting state: 1 is not a valid state"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The given starting state: a is not a valid state"))
  (check-error (make-mttm2 '(S Y N)
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
                         ) (format "Step three of the design recipe was not successfully completed.
The given starting state: (A) is not a valid state"))
  (check-error (make-mttm2 '(S Y N)
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

  ;;RULES
  
  (test)

  )
