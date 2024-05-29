(module tm-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-error (make-tm '(S Y N 1)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (1) are not valid machine states in the given list of machine states: (S Y N 1)"))
  (check-error (make-tm '(S Y N 1 2)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (1 2) are not valid machine states in the given list of machine states: (S Y N 1 2)"))
  ;;Invalid letter
  (check-error (make-tm '(S Y N a)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (a) are not valid machine states in the given list of machine states: (S Y N a)"))
  (check-error (make-tm '(S Y N a b)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (a b) are not valid machine states in the given list of machine states: (S Y N a b)"))
  ;;Duplicate states
  (check-error (make-tm '(S Y N S)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following values, (S), are duplicated in the given states: (S Y N S)"))
  (check-error (make-tm '(S Y N S S)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following values, (S), are duplicated in the given states: (S Y N S S)"))
  (check-error (make-tm '(S Y Y N S S)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following values, (S Y), are duplicated in the given states: (S Y Y N S S)"))
  ;;SIGMA
  ;invalid number
  (check-error (make-tm '(S Y N)
                         `(a b 1)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following: (1) are not valid alphabet letters in the given input alphabet: (a b 1)"))
  (check-error (make-tm '(S Y N)
                         `(a b 1 2)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following: (1 2) are not valid alphabet letters in the given input alphabet: (a b 1 2)"))
  ;invalid letter
  (check-error (make-tm '(S Y N)
                         `(a b 1)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following: (1) are not valid alphabet letters in the given input alphabet: (a b 1)"))
  (check-error (make-tm '(S Y N)
                         `(a b 1 2)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following: (1 2) are not valid alphabet letters in the given input alphabet: (a b 1 2)"))
  ;duplicate letter
  (check-error (make-tm '(S Y N)
                         `(a b a)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b a)"))
  (check-error (make-tm '(S Y N)
                         `(a b a a)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following values, (a), are duplicated in the given sigma: (a b a a)"))
  (check-error (make-tm '(S Y N)
                         `(a b a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ) (format "Step one of the design recipe has not been successfully completed.
The following values, (a b), are duplicated in the given sigma: (a b a b)"))
  ;;FINALS
  ;invalid final state
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N 1)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (1) are not valid machine states in the given list of machine finals: (Y N 1)"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N 1 2)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (1 2) are not valid machine states in the given list of machine finals: (Y N 1 2)"))
  ;invalid letter
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N a)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (a) are not valid machine states in the given list of machine finals: (Y N a)"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N a a)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (a a) are not valid machine states in the given list of machine finals: (Y N a a)"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N a b)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following: (a b) are not valid machine states in the given list of machine finals: (Y N a b)"))
  ;not in list of states
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N F)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following final states, (F), are not in your list of states: (S Y N)"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N F G)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following final states, (F G), are not in your list of states: (S Y N)"))
  ;duplicates
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following values, (N), are duplicated in the given final states: (Y N N)"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N N Y)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following values, (Y N), are duplicated in the given final states: (Y N N Y)"))

  ;;START STATE
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         '1
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The given starting state: 1 is not a valid state"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'a
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The given starting state: a is not a valid state"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         '(A)
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The given starting state: (A) is not a valid state"))
  (check-error (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'F
                         '(Y N)
                         'Y
                         ) (format "Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: (S Y N)"))

(check-error (make-tm '(S A B C D E F G H I J K L Y)
                        '(a b c x)
                        `(((S ,BLANK) (J ,RIGHT))
                          ((J ,BLANK) (Y ,BLANK))
                          ((J a) (A ,RIGHT))
                          ((A a) (A ,RIGHT))
                          ((A b) (B ,RIGHT))
                          ((B b) (B ,RIGHT))
                          ((B c) (C ,RIGHT))
                          ((C c) (C ,RIGHT))
                          ((C ,BLANK) (D ,LEFT))
                          ((D a) (D ,LEFT))
                          ((D b) (D ,LEFT))
                          ((D c) (D ,LEFT))
                          ((D x) (D ,LEFT))
                          ((D ,BLANK) (E ,RIGHT))
                          ((E x) (E ,RIGHT))
                          ((E a) (F x))
                          ((E a) (H x))
                          ((F a) (F ,RIGHT))
                          ((F b) (G x))
                          ((F x) (F ,RIGHT))
                          ((G b) (G ,RIGHT))
                          ((G x) (G ,RIGHT))
                          ((G c) (D x))
                          ((H x) (H ,RIGHT))
                          ((H b) (I x))
                          ((I x) (I ,RIGHT))
                          ((I c) (K x))            
                          ((K x) (L ,RIGHT))  
                          ((L ,BLANK) (Y ,BLANK)))     
                        'S
                        '(Y)
                        'Y
                        #:accepts `((,LM ,BLANK a b c))
                        #:rejects `((,LM ,BLANK a b c)))
             (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words: ((@ _ a b c))"))

  (check-error (make-tm '(S A B C D E F G H I J K L Y)
                        '(a b c x)
                        `(((S ,BLANK) (J ,RIGHT))
                          ((J ,BLANK) (Y ,BLANK))
                          ((J a) (A ,RIGHT))
                          ((A a) (A ,RIGHT))
                          ((A b) (B ,RIGHT))
                          ((B b) (B ,RIGHT))
                          ((B c) (C ,RIGHT))
                          ((C c) (C ,RIGHT))
                          ((C ,BLANK) (D ,LEFT))
                          ((D a) (D ,LEFT))
                          ((D b) (D ,LEFT))
                          ((D c) (D ,LEFT))
                          ((D x) (D ,LEFT))
                          ((D ,BLANK) (E ,RIGHT))
                          ((E x) (E ,RIGHT))
                          ((E a) (F x))
                          ((E a) (H x))
                          ((F a) (F ,RIGHT))
                          ((F b) (G x))
                          ((F x) (F ,RIGHT))
                          ((G b) (G ,RIGHT))
                          ((G x) (G ,RIGHT))
                          ((G c) (D x))
                          ((H x) (H ,RIGHT))
                          ((H b) (I x))
                          ((I x) (I ,RIGHT))
                          ((I c) (K x))            
                          ((K x) (L ,RIGHT))  
                          ((L ,BLANK) (Y ,BLANK)))     
                        'S
                        '(Y)
                        'Y
                        #:accepts `((,LM ,BLANK a b c c))
                        #:rejects `((,LM ,BLANK a b c)))
               (format "Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words: ((@ _ a b c c))"))
  
  ;;ACCEPTS/REJECTS
  (check-error (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                    #:accepts '((c)))
                (format "Step six of the design recipe has not been successfully completed.
The following words in the accept list contain symbols not included in sigma: ((c))"))

  (check-error (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                    #:rejects '((c)))
                (format "Step six of the design recipe has not been successfully completed.
The following words in the reject list contain symbols not included in sigma: ((c))"))
  
  (test)
  
  )
