#lang racket

#|
Created by Joshua Schappel on 4/5/20
  This file contains all functionality for the GenCode function
|#

(require racket/date "./structs/machine.rkt" "./structs/world.rkt" "./structs/state.rkt"
         "../fsm-main.rkt" "globals.rkt" "./structs/input.rkt")

(provide genCode)

;; The name of the file that the code is placed in
(define FILE-NAME "fsmGUIFunctions.rkt")


;; gen-code: world -> world
;; Purpose: Writes the current machine in the visualization tool to a file
(define (genCode world)
  (letrec
      ((fsm-machine (world-fsm-machine world))
       (state-list (map (lambda (x) (fsm-state-name x)) (machine-state-list (world-fsm-machine world)))))
    (cond
      [(valid-machine? fsm-machine state-list)
       (begin
         (write-to-file
          (determine-file-name (world-input-list world))
          (construct-machine
           state-list
           (machine-start-state fsm-machine)
           (machine-final-state-list fsm-machine)
           (machine-rule-list fsm-machine)
           (machine-alpha-list fsm-machine)
           (machine-type fsm-machine)
           fsm-machine)
          #t)
         (redraw-world-with-msg world (string-append "The machine was sucessfuly built and exported to "
                                                     FILE-NAME
                                                     ". This file can be found at: ~n "
                                                     (path->string (current-directory)))
                                "Success!" MSG-SUCCESS))]
      
      [else
       (begin
         (write-to-file
          FILE-NAME
          (construct-machine
           state-list
           (machine-start-state fsm-machine)
           (machine-final-state-list fsm-machine)
           (machine-rule-list fsm-machine)
           (machine-alpha-list fsm-machine)
           (machine-type fsm-machine)
           fsm-machine)
          #f)
         (redraw-world-with-msg world (string-append "The machine built with errors! Please see the cmd for more info. ~n ~n The machine was exported to "
                                                     FILE-NAME
                                                     ". This file can be found at: ~n "
                                                     (path->string (current-directory)) "Please fix the erros and press 'Run' again.")
                                "Error" MSG-ERROR))])))

;; (listOf inputFields) -> String
;; Purpose: Determines the files name
(define (determine-file-name input-list)
  (let ([inputFieldValue (case MACHINE-TYPE
                           [(pda) (string-trim (textbox-text (list-ref input-list 11)))]
                           [(tm) (string-trim (textbox-text (list-ref input-list 10)))]
                           [(tm-language-recognizer) (string-trim (textbox-text (list-ref input-list 11)))]
                           [else (textbox-text (list-ref input-list 8))])])
    (cond
      [(equal? "" inputFieldValue) FILE-NAME]
      [(and
        (> (string-length inputFieldValue) 4)
        (equal? (list->string (take-right (string->list inputFieldValue) 4)) ".rkt")) inputFieldValue]
      [else (string-append inputFieldValue ".rkt")])))

;; valid-machine?: machine state-list
;; Purpose: Determins if the given machine is valid
(define (valid-machine? fsm-machine state-list)
  (case MACHINE-TYPE
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
                    (machine-type fsm-machine))]))


;; write-to-file: string string boolean
;; Purpose: writes the comments and code to create a machine in the specified file.
;;   If the file does not exist it will create a file in the users current directory. If the file
;;   does exist then it adds the supplied args to the end of the file.
(define (write-to-file file-name machine status)
  (letrec
      ((formatt-date (string-append (number->string (date-month (current-date)))
                                    "/"
                                    (number->string (date-year (current-date)))))
       
       (formatt-minute (if (< (date-minute (current-date)) 10)
                           (string-append "0" (number->string (date-minute (current-date))))
                           (number->string (date-minute (current-date)))))

       ;; formatt-time: null -> string
       ;; Purpose: formatts military time into the from hh:mm:am/pm
       (formatt-time (lambda ()
                       (cond
                         [(and (> (date-hour (current-date)) 12) (< (date-hour (current-date)) 24)) (string-append (number->string (- (date-hour (current-date)) 12)) ":" formatt-minute "pm")]
                         [(equal? (date-hour (current-date)) 12) (string-append (number->string (date-hour (current-date))) ":" formatt-minute "pm")]
                         [(equal? (date-hour (current-date)) 24) (string-append (number->string (- (date-hour (current-date)) 12)) ":" formatt-minute "am")]
                         [else (string-append (number->string (date-hour (current-date))) ":" formatt-minute "am")])))

       ;; build-comments: boolean -> string
       ;; Purpose: constructs the comments that are written to the file
       (build-comments (lambda (status)
                         (cond
                           [status (string-append ";; Created by fsm-GUI on: " formatt-date " at " (formatt-time)"\n;; This machine passed all tests.")]
                           [else (string-append ";; Created by fsm-GUI on: " formatt-date " at " (formatt-time)"\n;; WARNING: this machine failed to build!")]))))
    
    (cond
      [(file-exists? file-name) (call-with-output-file file-name
                                  #:exists 'append
                                  (lambda (out)
                                    (displayln " " out)
                                    (displayln (build-comments status) out)
                                    (displayln machine out)))]
      [else (call-with-output-file file-name
              (lambda (out)
                (displayln "#lang Racket" out)
                (displayln "(require fsm)" out)
                (displayln "" out)
                (displayln (build-comments status) out)
                (displayln machine out)))])))



;; construct-machine: list-of-states symbol list-of-finals list-of-rules list-of-alpha symbol machine -> String
;; Purpose: Constructs the code to create the specified machine
(define (construct-machine states start finals rules alpha type m)
  (let
      (;; nameGen: none -> symbol
       ;; Purpose: generates a random name that consists of one capital letter(A-Z) and one number(1-9)
       ;;   for the mechine being created.
       (nameGen (lambda ()
                  (let ((letter (string (integer->char (random 65 91))))
                        (number (string (integer->char (random 48 58))))
                        (number2 (string (integer->char (random 48 58)))))
                    (string->symbol (string-append letter number number2))))))
    #|(println `(quote (,@states)))
    (println `(quote (,@alpha)))
    (println `(quote (,@rules)))
    (println `(quote (,@start)))
    (println `(quote (,@finals)))|#
    (case type
      [(dfa) `(define ,(nameGen) (make-dfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
      [(ndfa) `(define ,(nameGen) (make-ndfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
      [(pda)  `(define ,(nameGen) (make-ndpda (quote (,@states)) (quote (,@alpha)) (quote (,@(pda-machine-stack-alpha-list m))) (quote ,start) (quote ,finals) (quote (,@rules))))]
      [(tm) `(define ,(nameGen) (make-tm (quote (,@states)) (quote (,@alpha)) (quote (,@rules)) (quote (,@start)) (quote (,@finals))))]
      [(tm-language-recognizer) `(define ,(nameGen) (make-tm (quote (,@states)) (quote (,@alpha)) (quote (,@rules)) (quote (,@start)) (quote (,@finals))))]
      [else (error (format "The machine type: ~s, is not currently supported" type))])))

       
                      
       
       

