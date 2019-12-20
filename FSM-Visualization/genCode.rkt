#lang racket

#|
Created by Joshua Schappel on 12/19/19
  This file contains all functionality for the GenCode function
|#


(require racket/date "./structs/machine.rkt" "./structs/world.rkt" "./structs/state.rkt"
          "../fsm-main.rkt" "globals.rkt")

(provide genCode)


;; genCode: world -> world
;; Purpose: Exports the GUI machine to a external file called: 'fsmGUIFunctions.rkt'
(define genCode (lambda (w)
                  (letrec(
                          ;; The machine type ('dfa, 'ndfa, 'pda, ect...)
                          (type (machine-type (world-fsm-machine w)))
                          
                          (fsm-machine (world-fsm-machine w)) ;; The machine for the world
                          ;; condensed state list
                          (state-list (map (lambda (x) (fsm-state-name x)) (machine-state-list (world-fsm-machine w))))
                         
                          ;; construct-machine: list-of-states symbol list-of-finals list-of-rules list-of-alpha symbol
                          ;; Purpose: Constructs the code to create the specified machine
                          (construct-machine (lambda (states start finals rules alpha type)
                                               (letrec (
                                                        ;; nameGen: none -> symbol
                                                        ;; Purpose: generates a random name that consists of one capital letter(A-Z) and one number(1-9)
                                                        ;;   for the mechine being created.
                                                        (nameGen (lambda ()
                                                                   (let ((letter (string (integer->char (random 65 91))))
                                                                         (number (string (integer->char (random 48 58))))
                                                                         (number2 (string (integer->char (random 48 58)))))
                                                                     (string->symbol (string-append letter number number2))))))
                      
                                                 (case type
                                                   [(dfa) `(define ,(nameGen) (make-dfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
                                                   [(ndfa) `(define ,(nameGen) (make-ndfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
                                                   [(pda) (println "TODO ADD PDA")]
                                                   [(dfst) (println "TODO ADD DFST")]
                                                   [else (error (format "The machine type: ~s, is not currently supported" type))]))))

                          ;; write-to-file: string quasiquote boolean -> File
                          ;; Purpose writes the comments and code to create a machine in the specified file.
                          ;;   If the file does not exist it will create a file in the users current directory. If the file
                          ;;   does exist then it adds the supplied args to the end of the file.
                          (write-to-file (lambda (file machine status)
                                           (letrec (
                                                    ;; comments: boolean -> string
                                                    ;; Purpose: Creates the comments for the machine
                                                    (comments (lambda (status)
                                                                (letrec (
                                                                         (d (current-date)) ;; The current date
                                                                         (formatt-date (string-append (number->string (date-month d)) "/" (number->string (date-day d)) "/" (number->string (date-year d)))) ;; formatted date in form: mm/dd/yyyy
                                                                         (formatt-date-minute (if (< (date-minute d) 10) (string-append "0" (number->string (date-minute d))) (number->string (date-minute d))))
                                
                                                                         ;; formatt-time: null -> string
                                                                         ;; Purpose: formatts military time into the from hh:mm:am/pm
                                                                         (formatt-time (lambda ()
                                                                                         (cond
                                                                                           [(and (> (date-hour d) 12) (< (date-hour d) 24)) (string-append (number->string (- (date-hour d) 12)) ":" formatt-date-minute "pm")]
                                                                                           [(equal? (date-hour d) 12) (string-append (number->string (date-hour d)) ":" formatt-date-minute "pm")]
                                                                                           [(equal? (date-hour d) 24) (string-append (number->string (- (date-hour d) 12)) ":" formatt-date-minute "am")]
                                                                                           [else (string-append (number->string (date-hour d)) ":" formatt-date-minute "am")]))))
                                                                  (cond
                                                                    [status (string-append ";; Created by fsm-GUI on: " formatt-date " at " (formatt-time)"\n;; This machine passed all tests.")]
                                                                    [else (string-append ";; Created by fsm-GUI on: " formatt-date " at " (formatt-time)"\n;; WARNING: this machine failed to build!")])))))
                                             (cond
                                               [(file-exists? file) (call-with-output-file file
                                                                      #:exists 'append
                                                                      (lambda (out)
                                                                        (displayln " " out)
                                                                        (displayln (comments status) out)
                                                                        (displayln machine out)))]
                                               [else (call-with-output-file file
                                                       (lambda (out)
                                                         (displayln "#lang Racket" out)
                                                         (displayln "(require fsm)" out)
                                                         (displayln "" out)
                                                         (displayln (comments status) out)
                                                         (displayln machine out)))])))))

                    (cond
                      [(equal? #t (check-machine state-list (machine-alpha-list fsm-machine) (machine-final-state-list fsm-machine) (machine-rule-list fsm-machine) (machine-start-state fsm-machine) (machine-type fsm-machine)))
                       (begin
                         (write-to-file
                          "fsmGUIFunctions.rkt"
                          (construct-machine
                           state-list
                           (machine-start-state fsm-machine)
                           (machine-final-state-list fsm-machine)
                           (machine-rule-list fsm-machine)
                           (machine-alpha-list fsm-machine)
                           (machine-type fsm-machine))
                          #t)
                         (redraw-world-with-msg w (string-append "The machine was sucessfuly built and exported to fsmGUIFunctions.rkt. This file can be found at: ~n "
                                                                 (path->string (current-directory))) "Success!" MSG-SUCCESS))]

                      [else
                       (begin
                         (write-to-file
                          "fsmGUIFunctions.rkt"
                          (construct-machine
                           state-list
                           (machine-start-state fsm-machine)
                           (machine-final-state-list fsm-machine)
                           (machine-rule-list fsm-machine)
                           (machine-alpha-list fsm-machine)
                           (machine-type fsm-machine))
                          #f)
                         (redraw-world-with-msg w (string-append "The machine built with errors! Please see the cmd for more info. ~n ~n The machine was exported to fsmGUIFunctions.rkt. This file can be found at: ~n "
                                                                 (path->string (current-directory)) "Please fix the erros and press 'Run' again.")
                                                "Error" MSG-ERROR))]))))
