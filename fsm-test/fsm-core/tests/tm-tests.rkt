#lang racket/base
(module+ tests
  (require "../../../fsm-core/private/tm-constructors.rkt"
           "../../../fsm-core/private/constants.rkt"
           rackunit
           )

  ;;valid-listof/c tests
  ;;STATES
  ;;Invalid number
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1\\) are not valid machine states in the given list of machine states: \\(S Y N 1\\)"
   (lambda () (make-tm '(S Y N 1)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ))
     )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1 2\\) are not valid machine states in the given list of machine states: \\(S Y N 1 2\\)"
   (lambda () (make-tm '(S Y N 1 2)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         ))
   )
  ;;Invalid letter
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a\\) are not valid machine states in the given list of machine states: \\(S Y N a\\)"
   (lambda () (make-tm '(S Y N a)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )))
  (check-exn #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a b\\) are not valid machine states in the given list of machine states: \\(S Y N a b\\)"
             (lambda () (make-tm '(S Y N a b)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  ;;Duplicate states
  (check-exn
   "Step three of the design recipe has not been successfully completed.
The following values, \\(S\\), are duplicated in the given states: \\(S Y N S\\)"
   (lambda () (make-tm '(S Y N S)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(S\\), are duplicated in the given states: \\(S Y N S S\\)"
   (lambda () (make-tm '(S Y N S S)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  (check-exn
   "Step three of the design recipe has not been successfully completed.
The following values, \\(S Y\\), are duplicated in the given states: \\(S Y Y N S S\\)"
   (lambda () (make-tm '(S Y Y N S S)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  ;;SIGMA
  ;invalid letter
  ;duplicate letter
  (check-exn
   "Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b a\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b a)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a\\), are duplicated in the given sigma: \\(a b a a\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b a a)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  (check-exn
   #rx"Step one of the design recipe has not been successfully completed.
The following values, \\(a b\\), are duplicated in the given sigma: \\(a b a b\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N)
                         'Y
                         )) )
  ;;FINALS
  ;invalid final state
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1\\) are not valid machine states in the given list of machine finals: \\(Y N 1\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N 1)
                         'Y
                         ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(1 2\\) are not valid machine states in the given list of machine finals: \\(Y N 1 2\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N 1 2)
                         'Y
                         )))
  ;invalid letter
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a\\) are not valid machine states in the given list of machine finals: \\(Y N a\\)"
   (lambda ()
     (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N a)
                         'Y
                         ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a a\\) are not valid machine states in the given list of machine finals: \\(Y N a a\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N a a)
                         'Y
                         )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following: \\(a b\\) are not valid machine states in the given list of machine finals: \\(Y N a b\\)"
   (lambda ()
     (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N a b)
                         'Y
                         ))
   )
  ;not in list of states
  (check-exn
   "Step three of the design recipe has not been successfully completed.
The following final states, \\(F\\), are not in your list of states: \\(S Y N\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N F)
                         'Y
                         ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following final states, \\(F G\\), are not in your list of states: \\(S Y N\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N F G)
                         'Y
                         ))
   )
  ;duplicates
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(N\\), are duplicated in the given final states: \\(Y N N\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N N)
                         'Y
                         ))
   )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following values, \\(Y N\\), are duplicated in the given final states: \\(Y N N Y\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'S
                         '(Y N N Y)
                         'Y
                         )) )

  ;;START STATE
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: 1 is not a valid state"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         '1
                         '(Y N)
                         'Y
                         )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: a is not a valid state"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'a
                         '(Y N)
                         'Y
                         )) )
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The given starting state: \\(A\\) is not a valid state"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         '(A)
                         '(Y N)
                         'Y
                         )))
  (check-exn
   #rx"Step three of the design recipe has not been successfully completed.
The following starting state, F, is not in the given list of states: \\(S Y N\\)"
   (lambda () (make-tm '(S Y N)
                         `(a b)
                         `(((S a) (S ,RIGHT))
                           ((S b) (N b))
                           ((S ,BLANK) (Y ,BLANK)))
                         'F
                         '(Y N)
                         'Y
                         ) ))

(check-exn
 #rx"Step six of the design recipe has not been successfully completed.
The constructed machine does not reject the following words: \\(\\(@ _ a b c\\)\\)"
 (lambda () (make-tm '(S A B C D E F G H I J K L Y)
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
                        #:rejects `((,LM ,BLANK a b c))))
             )

  (check-exn
   #rx"Step six of the design recipe has not been successfully completed.
The constructed machine does not accept the following words: \\(\\(@ _ a b c c\\)\\)"
   (lambda ()
     (make-tm '(S A B C D E F G H I J K L Y)
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
                        #:rejects `((,LM ,BLANK a b c))))
               )
  
  ;;ACCEPTS/REJECTS
  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following words in the accept list contain symbols not included in sigma: \\(\\(c\\)\\)"
   (lambda () (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                    #:accepts '((c))))
                )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The expected accepts is not lists of symbols, or pairs of symbol lists and starting indexes: \\(a\\)"
   (lambda () (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                    #:accepts '(a)))
                )

  (check-exn
   #rx"Step two of the design recipe has not been successfully completed.
The following words in the reject list contain symbols not included in sigma: \\(\\(c\\)\\)"
   (lambda () (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                    #:rejects '((c))))
                )

  (check-exn
   "Step two of the design recipe has not been successfully completed.
The expected rejects is not lists of symbols, or pairs of symbol lists and starting indexes: \\(a\\)"
   (lambda () (make-tm '(S Y N)
                    '(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                    #:rejects '(a)))
                )
  
  )
