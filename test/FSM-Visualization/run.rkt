#lang racket
(require "../../FSM-Visualization/run.rkt" "../../FSM-Visualization/structs/machine.rkt" "../../fsm-main.rkt"
         "../../FSM-Visualization/structs/state.rkt"  "../../FSM-Visualization/globals.rkt" "../../FSM-Visualization/structs/posn.rkt")

(define a* (make-dfa '(S F)     ;; the states
                     '(a b)     ;; the input alphabet
                     'S         ;; the staring state
                     '(F)       ;; the set of final states
                     '((S a F)  ;; the transition functions
                       (F a F)
                       (F b F))
                     'nodead))

(define FSM (make-ndfa '(F S M)
                       '(f s m)
                       'F
                       '(M)
                       '((F s S)
                         (S m M)
                         (M f F)) 'nodead))

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

(define Ma (make-tm '(S H)
                    `(a b ,LM)
                    `(((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S
                    '(H)))


(define Alla (make-tm '(S Y N)
                      `(a b ,LM)
                      `(((S a) (S ,RIGHT))
                        ((S b) (N b))
                        ((S ,BLANK) (Y ,BLANK)))
                      'S
                      '(Y N)
                      'Y))

(define INV1 (lambda (v) 'a))
(define INV2 (lambda (v) 'b))
(define INV3 (lambda (v) 'b))
(define INV4 (lambda (c v) 'a))
(define INV5 (lambda (c v) 'b))
(define INV6 (lambda (c v) 'c))


(module+ test
  (require rackunit)
  ;;---------------------------------------------------------------------------------------------------------
  ;; Empty machines
  ;;---------------------------------------------------------------------------------------------------------
  (check-equal? (parse-input 'dfa #:test #t) make-new-dfa)
  (check-equal? (parse-input 'ndfa #:test #t) make-new-ndfa)
  (check-equal? (parse-input 'pda #:test #t) make-new-pda)
  (check-equal? (parse-input 'tm #:test #t) make-new-tm)
  (check-equal? (parse-input 'tm-language-recognizer #:test #t) make-new-lang-req)

  ;;---------------------------------------------------------------------------------------------------------
  ;; Premade machines, no invariants
  ;;---------------------------------------------------------------------------------------------------------
  (check-equal? (parse-input a* #:test #t) (machine (list
                                           (fsm-state 'S TRUE-FUNCTION (posn 0 0))
                                           (fsm-state 'F TRUE-FUNCTION (posn 0 0)))
                                          'S
                                          '(F)
                                          '((F b F) (F a F) (S a F))
                                          '()
                                          '(a b)
                                          'dfa))

  (check-equal? (parse-input FSM #:test #t) (machine (list
                                            (fsm-state 'F TRUE-FUNCTION (posn 0 0))
                                            (fsm-state 'S TRUE-FUNCTION (posn 0 0))
                                            (fsm-state 'M TRUE-FUNCTION (posn 0 0)))
                                           'F
                                           '(M)
                                           '((M f F) (S m M) (F s S))
                                           '()
                                           '(f s m)
                                           'ndfa))
  
  (check-equal? (parse-input pda-numa=numb #:test #t) (pda-machine
                                             (list
                                              (fsm-state 'S PDA-TRUE-FUNCTION (posn 0 0))
                                              (fsm-state 'M PDA-TRUE-FUNCTION (posn 0 0))
                                              (fsm-state 'F PDA-TRUE-FUNCTION (posn 0 0)))
                                             'S
                                             '(F)
                                             '(((M b (a)) (M ε)) ((M a (b)) (M ε)) ((M b ε) (M (b))) ((M a ε) (M (a))) ((M ε ε) (F ε)) ((S ε ε) (M ε)))
                                             '()
                                             '(a b)
                                             'pda
                                             '(a b)))

  (check-equal? (parse-input Ma #:test #t) (tm-machine (list
                                              (fsm-state 'S TM-TRUE-FUNCTION (posn 0 0))
                                              (fsm-state 'H TM-TRUE-FUNCTION (posn 0 0)))
                                             'S
                                             '(H)
                                             '(((S _) (H a)) ((S b) (H a)) ((S a) (H a)) ((S @) (S R)))
                                             '(@)
                                             '(a b @)
                                             'tm
                                             0))

  (check-equal? (parse-input Alla #:test #t) (lang-rec-machine (list
                                                      (fsm-state 'S TM-TRUE-FUNCTION (posn 0 0))
                                                      (fsm-state 'Y TM-TRUE-FUNCTION (posn 0 0))
                                                      (fsm-state 'N TM-TRUE-FUNCTION (posn 0 0)))
                                                     'S
                                                     '(Y N)
                                                     '(((S _) (Y _)) ((S b) (N b)) ((S a) (S R)) ((S @) (S R)))
                                                     '(@)
                                                     '(a b @)
                                                     'tm-language-recognizer
                                                     0
                                                     'Y))

  ;; --------------------------------------------------------------------------------------------------------
  ;; Premade machines, with invariants
  ;;---------------------------------------------------------------------------------------------------------
  (check-equal? (parse-input a* #:test #t
                             `(S ,INV1)
                             `(F ,INV2))
                (machine (list
                          (fsm-state 'S INV1 (posn 0 0))
                          (fsm-state 'F INV2 (posn 0 0)))
                         'S
                         '(F)
                         '((F b F) (F a F) (S a F))
                         '()
                         '(a b)
                         'dfa))

  (check-equal? (parse-input FSM #:test #t
                             `(S ,INV1)
                             `(F ,INV2)
                             `(M ,INV3))
                (machine (list
                          (fsm-state 'F INV2 (posn 0 0))
                          (fsm-state 'S INV1 (posn 0 0))
                          (fsm-state 'M INV3 (posn 0 0)))
                         'F
                         '(M)
                         '((M f F) (S m M) (F s S))
                         '()
                         '(f s m)
                         'ndfa))

  (check-equal? (parse-input pda-numa=numb #:test #t
                             `(S ,INV4)
                             `(F ,INV5))
                (pda-machine
                 (list
                  (fsm-state 'S INV4 (posn 0 0))
                  (fsm-state 'M PDA-TRUE-FUNCTION (posn 0 0))
                  (fsm-state 'F INV5 (posn 0 0)))
                 'S
                 '(F)
                 '(((M b (a)) (M ε)) ((M a (b)) (M ε)) ((M b ε) (M (b))) ((M a ε) (M (a))) ((M ε ε) (F ε)) ((S ε ε) (M ε)))
                 '()
                 '(a b)
                 'pda
                 '(a b)))

  (check-equal? (parse-input Ma #:test #t
                             `(S ,INV4)
                             `(H ,INV5))
                (tm-machine (list
                             (fsm-state 'S INV4 (posn 0 0))
                             (fsm-state 'H INV5 (posn 0 0)))
                            'S
                            '(H)
                            '(((S _) (H a)) ((S b) (H a)) ((S a) (H a)) ((S @) (S R)))
                            '(@)
                            '(a b @)
                            'tm
                            0))

  (check-equal? (parse-input Alla #:test #t
                             `(S ,INV4)
                             `(Y ,INV6))
                (lang-rec-machine (list
                                   (fsm-state 'S INV4 (posn 0 0))
                                   (fsm-state 'Y INV6 (posn 0 0))
                                   (fsm-state 'N TM-TRUE-FUNCTION (posn 0 0)))
                                  'S
                                  '(Y N)
                                  '(((S _) (Y _)) ((S b) (N b)) ((S a) (S R)) ((S @) (S R)))
                                  '(@)
                                  '(a b @)
                                  'tm-language-recognizer
                                  0
                                  'Y))
                
  
) ; end module+ test