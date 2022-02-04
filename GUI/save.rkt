#lang racket

(require
  racket/date
  "./structs/machine.rkt"
  "./structs/world.rkt"
  "./structs/state.rkt"
  "./globals.rkt"
  "../fsm-main.rkt")

(provide saveMachine)

;; gen-code :: world -> file-path -> bool
;; Purpose: Writes the current machine in the visualization tool to the specified file.
;; Returns true if the written machine was valid
(define (saveMachine world file-path)
  (define machine (get-field machine world))
  (define type (get-field type world))
  (define state-list (map (lambda (x) (fsm-state-name x))
                          (machine-state-list machine)))
  (define machine-status (valid-machine? machine state-list type))
  (cond
    [machine-status
     (begin
       (write-to-file file-path
                      (construct-machine
                       state-list
                       (machine-start-state machine)
                       (machine-final-state-list machine)
                       (machine-rule-list machine)
                       (machine-alpha-list machine)
                       (machine-type machine)
                       machine)
                      machine-status)
       #t)]
    [else
     (begin 
       (write-to-file file-path
                      (construct-machine
                       state-list
                       (machine-start-state machine)
                       (machine-final-state-list machine)
                       (machine-rule-list machine)
                       (machine-alpha-list machine)
                       (machine-type machine)
                       machine)
                      machine-status)
       #f)]))

;; valid-machine? :: machine -> state-list -> type -> bool
;; Purpose: Determins if the given machine is valid
(define (valid-machine? fsm-machine state-list type)
  (match type
    ['tm (check-machine state-list
                        (machine-alpha-list fsm-machine)
                        (machine-final-state-list fsm-machine)
                        (machine-rule-list fsm-machine)
                        (machine-start-state fsm-machine)
                        (machine-type fsm-machine))]
    ['pda (check-machine  state-list
                          (machine-alpha-list fsm-machine)
                          (machine-final-state-list fsm-machine)
                          (machine-rule-list fsm-machine)
                          (machine-start-state fsm-machine)
                          (machine-type fsm-machine)
                          (pda-machine-stack-alpha-list fsm-machine))]
    ['tm-language-recognizer (check-machine state-list
                                            (remove-duplicates (machine-alpha-list fsm-machine))
                                            (machine-final-state-list fsm-machine)
                                            (machine-rule-list fsm-machine)
                                            (machine-start-state fsm-machine)
                                            'tm)]
    [_
     (check-machine state-list
                    (machine-alpha-list fsm-machine)
                    (machine-final-state-list fsm-machine)
                    (machine-rule-list fsm-machine)
                    (machine-start-state fsm-machine)
                    (machine-type fsm-machine))]))


;; write-to-file :: string -> fiel-path -> boolean -> boolean
;; Purpose: writes the comments and code to create a machine in the specified file.
;;   If the file does not exist it will create a file in the users current directory. If the file
;;   does exist then it adds the supplied args to the end of the file.
(define (write-to-file file-path machine status)
  (define formatt-date (string-append ((compose number->string date-month current-date))
                                      "/"
                                      ((compose number->string date-year current-date))))
  (define formatt-minute (if (< (date-minute (current-date)) 10)
                             (string-append "0" ((compose number->string date-minute current-date)))
                             ((compose number->string date-minute current-date))))

  ;; Purpose: formatts military time into the form hh:mm:am/pm
  (define formatt-time
    (cond
      [(and (> (date-hour (current-date)) 12)
            (< (date-hour (current-date)) 24))
       (string-append (number->string (- (date-hour (current-date)) 12))
                      ":"
                      formatt-minute
                      "pm")]
      [(equal? (date-hour (current-date)) 12)
       (string-append (number->string (date-hour (current-date)))
                      ":"
                      formatt-minute
                      "pm")]
      [(equal? (date-hour (current-date)) 24)
       (string-append (number->string (- (date-hour (current-date)) 12))
                      ":"
                      formatt-minute
                      "am")]
      [else
       (string-append (number->string (date-hour (current-date)))
                      ":"
                      formatt-minute
                      "am")]))

  ;; Purpose: constructs the comments that are written to the file
  (define (build-comments)
    (define msg (if status
                    "\n;; This machine passed all tests."
                    "\n;; WARNING: this machine failed to build!"))
    (string-append ";; Created by fsm-GUI on: "
                   formatt-date
                   " at "
                   formatt-time
                   msg))
    
  (cond
    [(file-exists? file-path) (call-with-output-file file-path
                                #:exists 'append
                                (lambda (out)
                                  (displayln " " out)
                                  (displayln (build-comments) out)
                                  (displayln machine out)))]
    [else (call-with-output-file file-path
            (lambda (out)
              (displayln "#lang Racket" out)
              (displayln "(require fsm)" out)
              (displayln "" out)
              (displayln (build-comments) out)
              (displayln machine out)))]))



;; construct-machine :: list-of-states -> symbol -> list-of-finals -> list-of-rules -> list-of-alpha -> symbol -> machine -> string
;; Purpose: Constructs the code to create the specified machine
(define (construct-machine states start finals rules alpha type m)
  ;; Purpose: generates a random name that consists of one capital letter(A-Z) and one number(1-9)
  ;;   for the mechine being created.
  (define (nameGen)
    (let ((letter (string (integer->char (random 65 91))))
          (number (string (integer->char (random 48 58))))
          (number2 (string (integer->char (random 48 58)))))
      (string->symbol (string-append letter number number2))))
  (case type
    [(dfa) `(define ,(nameGen) (make-dfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
    [(ndfa) `(define ,(nameGen) (make-ndfa (quote (,@states)) (quote (,@alpha)) (quote ,start) (quote ,finals) (quote (,@rules))))]
    [(pda)  `(define ,(nameGen) (make-ndpda (quote (,@states)) (quote (,@alpha)) (quote (,@(pda-machine-stack-alpha-list m))) (quote ,start) (quote ,finals) (quote (,@rules))))]
    [(tm) `(define ,(nameGen) (make-tm (quote (,@states)) (quote (,@alpha)) (quote (,@rules)) (quote (,@start)) (quote (,@finals))))]
    [(tm-language-recognizer) `(define ,(nameGen) (make-tm (quote (,@states)) (quote (,@alpha)) (quote (,@rules)) (quote (,@start)) (quote (,@finals))))]
    [else (error (format "The machine type: ~s, is not currently supported" type))]))

       
                      
       
       

