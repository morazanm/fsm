#lang racket/base
(require "../fsm-core/private/ndfa-constructors.rkt"
         "../fsm-core/private/pda-constructors.rkt"
         "../fsm-core/private/tm-constructors.rkt"
         "../fsm-core/private/constants.rkt")
(provide
 (all-defined-out))

(define a* (make-dfa '(S F)     ;; the states
                     '(a b)     ;; the input alphabet
                     'S         ;; the staring state
                     '(F)       ;; the set of final states
                     '((S a F)  ;; the transition functions
                       (F a F)
                       (F b F))))

(define a*a (make-dfa '(S F A)         ;the states
                      '(a b)           ;the alphabet
                      'S               ;the starting state
                      '(F)             ;final states
                      '((S a F)        ;the transition function
                        (F a F)
                        (F b A)
                        (A a F)
                        (A b A))))

(define pda-numa=numb (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M ,EMP ,EMP) (F ,EMP))
                                    ((M a ,EMP) (M (a)))
                                    ((M b ,EMP) (M (b)))
                                    ((M a (b)) (M ,EMP))
                                    ((M b (a)) (M ,EMP)))))

(define FSM (make-ndfa '(F S M)
                       '(f s m)
                       'F
                       '(M)
                       '((F s S)
                         (S m M)
                         (M f F))))

(define Ma (make-tm '(S H)                  ;the states
                    `(a b)              ;the alphabet
                    `(((S ,LM) (S ,RIGHT))  ;the transition relation
                      ((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S                      ;the starting state
                    '(H)))

(define a^nb^nc^n (make-tm '(S B C D E Y N)
                           '(a b c z)
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
                           'S
                           '(Y N)
                           'Y))
