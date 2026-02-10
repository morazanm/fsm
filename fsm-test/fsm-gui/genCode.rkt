#lang racket/base

(module+ test
  (require rackunit
           "../../fsm-gui/genCode.rkt"
         "../../fsm-gui/structs/state.rkt"
         "../../fsm-gui/structs/machine.rkt"
         "../../fsm-core/private/constants.rkt"
         racket/string)
  (define (to-state-struct s) (fsm-state s #f #f))
  (define dfa-m (machine (map to-state-struct '(S F D))
                         'S
                         '(F)
                         '((S a F) (F a F) (F b F))
                         '()
                         '(a b)
                         'dfa))

  (check-true (string-contains? (machine->fsm-code dfa-m) "'(S F D)"))
  (check-true (string-contains? (machine->fsm-code dfa-m) "'(a b)"))
  (check-true (string-contains? (machine->fsm-code dfa-m) "'S"))
  (check-true (string-contains? (machine->fsm-code dfa-m) "'(F)"))
  (check-true (string-contains? (machine->fsm-code dfa-m) "'((S a F) (F a F) (F b F))"))



  (define pda-m (pda-machine (map to-state-struct '(S M F))
                             'S
                             '(F)
                             `(((S ,EMP ,EMP) (M ,EMP))
                               ((M ,EMP ,EMP) (F ,EMP))
                               ((M a ,EMP) (M (a)))
                               ((M b ,EMP) (M (b)))
                               ((M a (b)) (M ,EMP))
                               ((M b (a)) (M ,EMP)))
                             '()
                             '(a b)
                             'pda
                             '(a b)))


  (check-true (string-contains? (machine->fsm-code pda-m) "'(S M F)"))
  (check-true (string-contains? (machine->fsm-code pda-m) "'(a b)"))
  (check-true (string-contains? (machine->fsm-code pda-m) "'S"))
  (check-true (string-contains? (machine->fsm-code pda-m) "'(F)"))
  (check-true (string-contains? (machine->fsm-code pda-m)
                                (format "~s"
                                        `(((S ,EMP ,EMP) (M ,EMP))
                                          ((M ,EMP ,EMP) (F ,EMP))
                                          ((M a ,EMP) (M (a)))
                                          ((M b ,EMP) (M (b)))
                                          ((M a (b)) (M ,EMP))
                                          ((M b (a)) (M ,EMP))))))


  (define tm-m (tm-machine (map to-state-struct '(S H))
                           'S
                           '(H)
                           `(((S ,LM) (S ,RIGHT))
                             ((S a) (H a))
                             ((S b) (H a))
                             ((S ,BLANK) (H a)))
                           '(,LM)
                           `(a b ,LM)
                           'tm
                           0))

  (check-true (string-contains? (machine->fsm-code tm-m) "'(S H)"))
  (check-true (string-contains? (machine->fsm-code tm-m) "'(a b @)"))
  (check-true (string-contains? (machine->fsm-code tm-m) "'S"))
  (check-true (string-contains? (machine->fsm-code tm-m) "'(H)"))
  (check-true (string-contains? (machine->fsm-code tm-m)
                                (format "~s"
                                        `(((S ,LM) (S ,RIGHT))
                                          ((S a) (H a))
                                          ((S b) (H a))
                                          ((S ,BLANK) (H a))))))


  (define lang-rec-m (lang-rec-machine (map to-state-struct '(S B C D E Y N))
                                       'S
                                       '(Y N)
                                       `(((S a) (B z))
                                         ((S b) (N b))
                                         ((S c) (N c))
                                         ((S ,BLANK) (Y ,BLANK))
                                         ((S z) (N z))
                                         ((E z) (E ,RIGHT))
                                         ((E ,BLANK) (Y ,BLANK))
                                         ((E a) (N a))
                                         ((E b) (N b))
                                         ((E c) (N c))
                                         ((B a) (B ,RIGHT))
                                         ((B b) (C z))
                                         ((B c) (N c))
                                         ((B ,BLANK) (N ,BLANK))
                                         ((B z) (B ,RIGHT))
                                         ((C a) (N a))
                                         ((C b) (C ,RIGHT))
                                         ((C c) (D z))
                                         ((C ,BLANK) (N ,BLANK))
                                         ((C z) (C ,RIGHT))
                                         ((D a) (S a))
                                         ((D b) (D ,LEFT))
                                         ((D c) (D ,LEFT))
                                         ((D ,BLANK) (N ,BLANK))
                                         ((D z) (D ,LEFT))
                                         ((D ,LM) (E R)))
                                       `(,LM)
                                       '(a b c z)
                                       'tm-language-recognizer
                                       0
                                       'Y))


  (check-true (string-contains? (machine->fsm-code lang-rec-m) "'(S B C D E Y N)"))
  (check-true (string-contains? (machine->fsm-code lang-rec-m) "'(a b c z)"))
  (check-true (string-contains? (machine->fsm-code lang-rec-m) "'S"))
  (check-true (string-contains? (machine->fsm-code lang-rec-m) "'(Y N)"))
  (check-true (string-contains? (machine->fsm-code lang-rec-m) "'Y"))
  (check-true (string-contains? (machine->fsm-code lang-rec-m)
                                (format "~s"
                                        `(((S a) (B z))
                                          ((S b) (N b))
                                          ((S c) (N c))
                                          ((S ,BLANK) (Y ,BLANK))
                                          ((S z) (N z))
                                          ((E z) (E ,RIGHT))
                                          ((E ,BLANK) (Y ,BLANK))
                                          ((E a) (N a))
                                          ((E b) (N b))
                                          ((E c) (N c))
                                          ((B a) (B ,RIGHT))
                                          ((B b) (C z))
                                          ((B c) (N c))
                                          ((B ,BLANK) (N ,BLANK))
                                          ((B z) (B ,RIGHT))
                                          ((C a) (N a))
                                          ((C b) (C ,RIGHT))
                                          ((C c) (D z))
                                          ((C ,BLANK) (N ,BLANK))
                                          ((C z) (C ,RIGHT))
                                          ((D a) (S a))
                                          ((D b) (D ,LEFT))
                                          ((D c) (D ,LEFT))
                                          ((D ,BLANK) (N ,BLANK))
                                          ((D z) (D ,LEFT))
                                          ((D ,LM) (E R))))))
  )