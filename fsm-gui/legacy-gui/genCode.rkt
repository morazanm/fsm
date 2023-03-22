#lang racket

(require racket/date
         "globals.rkt"
         "./structs/machine.rkt"
         "./structs/world.rkt"
         "./structs/state.rkt"
         "../../fsm-core/interface.rkt")

(provide genCode)

;; genCode: world -> world
;; Purpose: Writes the current machine in the visualization tool to a specified file location
(define (genCode world file-path)
  (define machine (world-fsm-machine world))
  (define pass? (valid-machine? machine))
  (begin
    (write-to-file file-path machine pass?)
    (if pass?
        (redraw-world-with-msg world
                               (format "The machine was sucessfuly built and exported to ~s."
                                       (path->string file-path))
                               "Success!"
                               MSG-SUCCESS)
        (redraw-world-with-msg world
                               (format "The machine built with errors! Please see the cmd for more info. The machine was exported to ~s"
                                       (path->string file-path))
                               "Error"
                               MSG-ERROR))))


;; valid-machine?: machine -> boolean
;; Purpose: Determins if the given machine is valid
(define (valid-machine? fsm-machine)
  (define state-list (map fsm-state-name (machine-state-list fsm-machine)))
  (not (void? (case MACHINE-TYPE
                [(tm) (check-machine state-list
                                     (machine-alpha-list fsm-machine)
                                     (machine-final-state-list fsm-machine)
                                     (machine-rule-list fsm-machine)
                                     (machine-start-state fsm-machine)
                                     (machine-type fsm-machine))]
                [(pda) (check-machine  state-list
                                       (machine-alpha-list fsm-machine)
                                       (machine-final-state-list fsm-machine)
                                       (machine-rule-list fsm-machine)
                                       (machine-start-state fsm-machine)
                                       (machine-type fsm-machine)
                                       (pda-machine-stack-alpha-list fsm-machine))]
                [(tm-language-recognizer) (check-machine state-list
                                                         (remove-duplicates (machine-alpha-list fsm-machine))
                                                         (machine-final-state-list fsm-machine)
                                                         (machine-rule-list fsm-machine)
                                                         (machine-start-state fsm-machine)
                                                         'tm)]
                [else
                 (check-machine state-list
                                (machine-alpha-list fsm-machine)
                                (machine-final-state-list fsm-machine)
                                (machine-rule-list fsm-machine)
                                (machine-start-state fsm-machine)
                                (machine-type fsm-machine))]))))


;; write-to-file: path machine boolean -> ()
;; Purpose: writes the comments and code to create a machine in the specified file.
;;   If the file does not exist it will create a file in the users current directory. If the file
;;   does exist then it adds the supplied args to the end of the file.
(define (write-to-file file-path machine pass?)
  (define file-comments
    (format ";; Created by fsm-GUI on: ~a\n;; ~a"
            (date->string (current-date) #t)
            (if pass? "This machine passed all tests." "WARNING: this machine failed to build!")))
  (define (is-empty-file? file-path)
    (define in (open-input-file file-path))
    (eof-object? (read-line in)))

  (call-with-output-file file-path
    #:exists 'append
    (lambda (out)
      (when (is-empty-file? file-path)
        (displayln "#lang fsm" out))
      (displayln " " out)
      (displayln file-comments out)
      (displayln (machine->fsm-code machine) out))))


;; construct-machine: machine -> string
;; converts a machine to the fsm representation
;; NOTE: We do not support codegen for mttm's because we don not provide
;; the gencode button
(define (machine->fsm-code gui-machine)
  (match-define (machine state-structs start finals rules _ alpha type) gui-machine)
  (define states (map fsm-state-name state-structs))
  (match type
    [(or 'dfa 'ndfa)
     (format "(define ~s (make-~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s))"
             (gensym)
             type
             states
             alpha
             start
             finals
             rules)]
    ['pda (format "(define ~s (make-ndpda \n\t'~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s))"
                  (gensym)
                  states
                  alpha
                  (pda-machine-stack-alpha-list gui-machine)
                  start
                  finals
                  rules)]
    ['tm (format "(define ~s (make-tm  \n\t'~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s))"
                 (gensym)
                 states
                 alpha
                 rules
                 start
                 finals)]
    ['tm-language-recognizer (format "(define ~s (make-tm  \n\t'~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s \n\t'~s))"
                                     (gensym)
                                     states
                                     alpha
                                     rules
                                     start
                                     finals
                                     (lang-rec-machine-accept-state gui-machine))]
    [else (error (format "The machine type: ~s, is not currently supported" ) type)]))


(module+ test
  (require rackunit)
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
  );; end module+ test
