#lang racket
(require "../fsm-main.rkt"
         "./structs/state.rkt"
         "./structs/machine.rkt"
         "./structs/posn.rkt"
         "./globals.rkt")

(provide
 (struct-out new-machine-vars)
 check-for-errors
 build-new-interal-machine)

;; new-machine-vars represents the fields that are changed when building the new GUI
;; machine. See "build-new-interal-machine for me details"
(struct new-machine-vars
  [unprocessed-list
   processed-list
   machine] #:transparent)

;; check-for-errors :: machine -> string
;; This function checks for both internal machine errors like missing fields on the world machine
;;  that need to be added to create a fsm-machine. It also checks for errors when creating the valid fsm-machine
;;  such as invalid symbols. (AKA uses fsm's error messages). If there are not any errors then we return ""
(define (check-for-errors m)
  (define missing-field-errors (check-for-missing-fields m))
  (cond
    [(not (eq? "" missing-field-errors)) missing-field-errors]
    [(not isValidMachine?) "The Machine failed to build. Please see the cmd for more info"]
    [else ""]))
  

;; check-for-missing-fields :: string
;; Makes sure that all the machines fields are filled in, if so returns "" 
(define (check-for-missing-fields m)
  (match-let* ([(machine states start finals rules sigma alpha _) m]
               [states-msg (if (empty? states) "\n-states " "")]
               [start-mgs (if (empty? start) "\n-start " "")]
               [finals-msg (if (empty? finals) "\n-finals " "")]
               [rules-msg (if (empty? finals) "\n-rules " "")]
               [sigma-msg (if (empty? sigma) "\n-sigma " "")]
               [alpha-msg (if (empty? alpha) "\n-alpha " "")]
               [error-msg (string-append
                           states-msg
                           start-mgs
                           finals-msg
                           rules-msg
                           sigma-msg
                           alpha-msg)])
    (cond
      [(and (lang-rec-machine? m)
            (equal? '|| (lang-rec-machine-accept-state m)))
       "You must specify an accept state"]
      [(equal? error-msg "") ""]
      [else (string-append
             "The following needs to be filled in before continuing:"
             error-msg)])))


;; build-new-interal-machine :: machine -> new-machine-vars
;; First we convert the internal machine into a fsm-machine to get the transitions,
;;  then we convert it back the a GUI interval machine with the changes
(define (build-new-interal-machine machine)
  (define fsm-machine (internal-machine->fsm-machine machine))
  (define new-machine (fsm-machine->internal-machine
                       fsm-machine
                       machine))
  (define transitions (build-unprocessed-list
                       machine
                       fsm-machine))
  (new-machine-vars
   (if (list? transitions)
       (cdr transitions)
       '())
   (if (list? transitions)
       (list (car transitions))
       '())
   new-machine))
  
  


  
;; fsm-machine->internal-machine -> fsm-machine -> machine -> machine
;; Coverts a FSM machine into a internal GUI machine
(define (fsm-machine->internal-machine fsm-machine m)
  (define type (machine-type m))
  (define state-list (remove-duplicates (append
                                         (sm-getstates fsm-machine)
                                         (map (lambda (x) (fsm-state-name x))
                                              (machine-state-list m)))))
  (match type
    ['pda
     (pda-machine
      (map-true-functions state-list m type)
      (sm-getstart fsm-machine)
      (sm-getfinals fsm-machine)
      (sm-getrules fsm-machine)
      (machine-sigma-list machine)
      (sm-getalphabet fsm-machine)
      type
      (sm-getstackalphabet fsm-machine))]
    ['tm
     (tm-machine
      (map-true-functions state-list m type)
      (sm-getstart fsm-machine)
      (sm-getfinals fsm-machine)
      (sm-getrules fsm-machine)
      (decide-machine-input TM-ORIGIONAL-TAPE)
      (sm-getalphabet fsm-machine)
      type
      (tm-machine-tape-posn machine))]
    ['tm-language-recognizer
     (lang-rec-machine
      (map-true-functions state-list m type)
      (sm-getstart fsm-machine)
      (sm-getfinals fsm-machine)
      (sm-getrules fsm-machine)
      (decide-machine-input TM-ORIGIONAL-TAPE)
      (sm-getalphabet fsm-machine)
      type
      (tm-machine-tape-posn machine)
      (sm-getaccept fsm-machine))]
    [_
     (machine
      (map-true-functions state-list m type)
      (sm-getstart fsm-machine)
      (sm-getfinals fsm-machine)
      (sm-getrules fsm-machine)
      (machine-sigma-list m)
      (sm-getalphabet fsm-machine)
      type)]))


; decide-world-input: list -> list
;; Purpose: builds the new world list for tms and lang-recs
(define (decide-machine-input list)
  (cond
    [(equal? (car list) LM)
     list]
    [else
     (cons LM list)]))
  


;; addTrueFunctions: listOfStates -> machine -> type -> listOfStates
;; Adds the default true function to every state in the list if it doesnt have a function
(define (map-true-functions los m type)
  ;; Returns a state-struct if its name is the same as the symbol, otherwise it returns false.
  (define (in-cur-state-list s msl)
    (match msl
      [`() #f]
      [`(,f ,_ ...) #:when (eq? s f) f]
      [`(,_ ,r ...) (in-cur-state-list s r)]))
  (define (build-state s)
    (match type
      ['pda (fsm-state s PDA-TRUE-FUNCTION (posn 0 0))]
      ['tm (fsm-state s TM-TRUE-FUNCTION (posn 0 0))]
      ['tm-language-recognizer (fsm-state s TM-TRUE-FUNCTION (posn 0 0))]
      [else (fsm-state s TRUE-FUNCTION (posn 0 0))]))
  (map (lambda (s)
         (define hasState (in-cur-state-list s (machine-state-list m)))
         (if hasState
             hasState
             (build-state s)))
       los))
         

;; build-passing-fsm-machine :: machine -> fsm-machine
;; Converts the GUI internal representation of a machine into the FSM representation
(define (internal-machine->fsm-machine m)
  (match-let* ([(machine states start finals rules sigma alpha type) m]
               [state-list (map (lambda (x) (fsm-state-name x)) states)])
    (match type
      [(or 'dfa 'ndfa) (make-unchecked-dfa state-list
                                           alpha
                                           start
                                           finals
                                           rules)]
      ['ndfa (make-unchecked-ndfa state-list
                                  alpha
                                  start
                                  finals
                                  rules)]
      ['tm (make-unchecked-tm state-list
                              alpha
                              rules
                              start
                              finals)]
      [_ (make-unchecked-tm state-list
                            alpha
                            rules
                            start
                            finals
                            (lang-rec-machine-accept-state m))])))

;; build-unprocessed-list :: machine -> fsm-machine -> listOfSymbols
;; constructs the unporcessed list for a machine
(define (build-unprocessed-list machine fsm-machine)
  (match (sm-type fsm-machine)
    ['tm (let* ((sig-list TM-ORIGIONAL-TAPE)
                (proper-list (cond
                               [(empty? sig-list) #f]
                               [(equal? LM (car sig-list))
                                TM-ORIGIONAL-TAPE]
                               [else
                                (cons LM TM-ORIGIONAL-TAPE)]))
                (trans (sm-showtransitions fsm-machine
                                           (if proper-list
                                               proper-list
                                               '('()'()))
                                           (tm-machine-tape-posn machine))))
           (if (string? trans)
               (list trans)
               (append trans '(halt))))]
    ['tm-language-recognize (let* ((sig-list TM-ORIGIONAL-TAPE)
                                   (proper-list (cond
                                                  [(empty? sig-list) #f]
                                                  [(equal? LM (car sig-list))
                                                   TM-ORIGIONAL-TAPE]
                                                  [else
                                                   (cons LM TM-ORIGIONAL-TAPE)]))
                                   (trans (sm-showtransitions fsm-machine
                                                              (if proper-list
                                                                  proper-list
                                                                  '('()'()))
                                                              (tm-machine-tape-posn machine))))
                              (if (string? trans)
                                  (list trans)
                                  trans))]
    [_ (sm-showtransitions fsm-machine
                           (machine-sigma-list machine))]))



;; isValidMachine? :: machine -> boolean
;; Purpose: Determines if the given input is a valid machine
;;     The machine is valid if check-machine returns a boolean 
(define (isValidMachine? machine)
  (define state-list (map (lambda (x) (fsm-state-name x)) (machine-state-list machine)))
  (case MACHINE-TYPE
    [(pda) (boolean?
            (check-machine
             state-list
             (machine-alpha-list machine)
             (machine-final-state-list machine)
             (machine-rule-list machine)
             (machine-start-state machine)
             (machine-type machine)
             (pda-machine-stack-alpha-list machine)))]
    [(tm) (boolean?
           (check-machine
            state-list
            (remove-duplicates (machine-alpha-list machine))
            (machine-final-state-list machine)
            (machine-rule-list machine)
            (machine-start-state machine)
            (machine-type machine)))]
    [(tm-language-recognizer)
     (boolean? 
      (check-machine
       state-list
       (remove-duplicates (machine-alpha-list machine))
       (machine-final-state-list machine)
       (machine-rule-list machine)
       (machine-start-state machine)
       'tm))]
    [else
     (boolean?
      (check-machine
       state-list
       (machine-alpha-list machine)
       (machine-final-state-list machine)
       (machine-rule-list machine)
       (machine-start-state machine)
       (machine-type machine)))]))
  
        
  
