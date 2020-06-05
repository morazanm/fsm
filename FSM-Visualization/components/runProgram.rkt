#lang racket
#|
Created by Joshua Schappel on 12/19/19
  This field contains the runPorgam function. That checks a given machine and determines if the program should run
|#


(require "../../fsm-main.rkt" "../structs/world.rkt" "../structs/state.rkt" "../structs/machine.rkt"
         "../structs/posn.rkt" "../structs/msgWindow.rkt" "../globals.rkt")


(provide runProgram)

;; runProgram: world -> world
;; Purpose: Determines if there are any unfilled in fields. If there are not any then the machine is constructed in fsm
(define runProgram(lambda (w)
                    (let* (
                           ;; The world fsm-machine
                           (fsm-machine (world-fsm-machine w))
                           ;; A condensed list of just the state-name symbols
                           (state-list (map (lambda (x) (fsm-state-name x)) (machine-state-list (world-fsm-machine w))))
                           )
                      (begin
                        (reset-bottom-indices MACHINE-TYPE w)
                        (cond
                          ;; If the input list if empty tell the user
                          #|[(empty? (machine-sigma-list (world-fsm-machine w)))
                           (redraw-world-with-msg w "You must first add input to the machine!" "Notice" MSG-CAUTION)]|#
                          [(lang-rec-machine? fsm-machine)
                           (if (equal? '|| (lang-rec-machine-accept-state fsm-machine))
                               (redraw-world-with-msg w "You must specify an accept state" "Notice" MSG-CAUTION)
                               (construct-world state-list fsm-machine w))]
                          [else
                           (construct-world state-list fsm-machine w)])))))
                        


;; construct-world: state-list machine world -> world
;; Purpose: Determins if the machien is valid. If it is then it creates a new world with valid machines parts and transition rules
(define (construct-world state-list fsm-machine w)
  (cond
    [(isValidMachine? state-list fsm-machine)
     (letrec (
              ;; Build a passing machine
              (m (case (machine-type fsm-machine)
                   ['dfa (if (member 'ds state-list)
                             (make-unchecked-dfa state-list
                                                 (machine-alpha-list (world-fsm-machine w))
                                                 (machine-start-state (world-fsm-machine w))
                                                 (machine-final-state-list (world-fsm-machine w))
                                                 (machine-rule-list (world-fsm-machine w)))
                             (make-unchecked-dfa state-list
                                                 (machine-alpha-list (world-fsm-machine w))
                                                 (machine-start-state (world-fsm-machine w))
                                                 (machine-final-state-list (world-fsm-machine w))
                                                 (machine-rule-list (world-fsm-machine w))
                                                 'nodead))]
                   ['ndfa (make-unchecked-ndfa state-list
                                               (machine-alpha-list (world-fsm-machine w))
                                               (machine-start-state (world-fsm-machine w))
                                               (machine-final-state-list (world-fsm-machine w))
                                               (machine-rule-list (world-fsm-machine w)))]
                   ['pda (make-unchecked-ndpda state-list
                                               (machine-alpha-list (world-fsm-machine w))
                                               (pda-machine-stack-alpha-list (world-fsm-machine w))
                                               (machine-start-state (world-fsm-machine w))
                                               (machine-final-state-list (world-fsm-machine w))
                                               (machine-rule-list (world-fsm-machine w)))]
                   ['tm
                    (make-unchecked-tm state-list
                                       (machine-alpha-list (world-fsm-machine w))
                                       (machine-rule-list (world-fsm-machine w))
                                       (machine-start-state (world-fsm-machine w))
                                       (machine-final-state-list (world-fsm-machine w)))]
                   [else (make-unchecked-tm state-list
                                            (machine-alpha-list (world-fsm-machine w))
                                            (machine-rule-list (world-fsm-machine w))
                                            (machine-start-state (world-fsm-machine w))
                                            (machine-final-state-list (world-fsm-machine w))
                                            (lang-rec-machine-accept-state (world-fsm-machine w)))]))

              ;; Unprocessed transitions
              (unprocessed-list (case MACHINE-TYPE
                                  [(tm)
                                   (let* ((sig-list TM-ORIGIONAL-TAPE)
                                          (proper-list (cond
                                                         [(empty? sig-list) #f]
                                                         [(equal? LM (car sig-list))
                                                          TM-ORIGIONAL-TAPE]
                                                         [else
                                                          (cons LM TM-ORIGIONAL-TAPE)]))
                                          (trans (sm-showtransitions m
                                                                     (if proper-list
                                                                         proper-list
                                                                         '('()'()))
                                                                     (tm-machine-tape-posn (world-fsm-machine w)))))
                                     (if (string? trans)
                                         (list trans)
                                         (append trans '(halt))))]
                                  [(tm-language-recognizer)
                                   (let* ((sig-list TM-ORIGIONAL-TAPE)
                                          (proper-list (cond
                                                         [(empty? sig-list) #f]
                                                         [(equal? LM (car sig-list))
                                                          TM-ORIGIONAL-TAPE]
                                                         [else
                                                          (cons LM TM-ORIGIONAL-TAPE)]))
                                          (trans (sm-showtransitions m
                                                                     (if proper-list
                                                                         proper-list
                                                                         '('()'()))
                                                                     (tm-machine-tape-posn (world-fsm-machine w)))))
                                     (if (string? trans)
                                         (list trans)
                                         trans))]
                                  ;; dfa, ndfa, pda
                                  [else (sm-showtransitions m
                                                            (machine-sigma-list (world-fsm-machine w)))]))

              )
                           
       ;; Set up the world to have all the valid machine components below                 
       (begin                 
         (define new-list (remove-duplicates (append (sm-getstates m) state-list))) ;; new-list: checks for any fsm state add-ons (ie. 'ds)
         (world
          (constructWorldMachine new-list fsm-machine m)
          (world-tape-position w)
          CURRENT-RULE
          (machine-start-state (world-fsm-machine w))
          (world-button-list w)
          (world-input-list w)    
          (if (list? unprocessed-list)
              (list (car unprocessed-list))
              '())
                                    
          (if (list? unprocessed-list)
              (cdr unprocessed-list)
              '())
                                    
          (if (list? unprocessed-list)
              (msgWindow "The machine was sucessfully built. Press Next and Prev to show the machine's transitions" "Success"
                         (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)
              (msgWindow "The Input was rejected" "Warning"
                         (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-CAUTION))
          0)))]
    [else
     (redraw-world-with-msg w "The Machine failed to build. Please see the cmd for more info" "Error" MSG-ERROR)]))


;; decide-world-input: list -> list
;; Purpose: builds the new world list for tms and lang-recs
(define (decide-machine-input list)
  (cond
    [(equal? (car list) LM)
     list]
    [else
     (cons LM list)]))
  


;; isValidMachine?: list-of-states machine -> boolean
;; Purpose: Determines if the given input is a valid machine
;;     The machine is valid if check-machine returns a boolean 
(define (isValidMachine? state-list fsm-machine)
  (case MACHINE-TYPE
    [(pda) (boolean?
            (check-machine
             state-list
             (machine-alpha-list fsm-machine)
             (machine-final-state-list fsm-machine)
             (machine-rule-list fsm-machine)
             (machine-start-state fsm-machine)
             (machine-type fsm-machine)
             (pda-machine-stack-alpha-list fsm-machine)))]
    [(tm) (boolean?
           (check-machine
            state-list
            (remove-duplicates (machine-alpha-list fsm-machine))
            (machine-final-state-list fsm-machine)
            (machine-rule-list fsm-machine)
            (machine-start-state fsm-machine)
            (machine-type fsm-machine)))]
    [(tm-language-recognizer)
     (boolean? 
      (check-machine
       state-list
       (remove-duplicates (machine-alpha-list fsm-machine))
       (machine-final-state-list fsm-machine)
       (machine-rule-list fsm-machine)
       (machine-start-state fsm-machine)
       'tm))]
    [else
     (boolean?
      (check-machine
       state-list
       (machine-alpha-list fsm-machine)
       (machine-final-state-list fsm-machine)
       (machine-rule-list fsm-machine)
       (machine-start-state fsm-machine)
       (machine-type fsm-machine)))]))



;; constructworldMachine: list-of-states sigma-list machine -> machine/pda-machine
;; Purpose: constructs the proper machine based on the type needed
(define (constructWorldMachine state-list worldMachine newMachine)
  (case MACHINE-TYPE
    [(pda)
     (pda-machine
      (addTrueFunctions state-list worldMachine)
      (sm-getstart newMachine)
      (sm-getfinals newMachine)
      (sm-getrules newMachine)
      (machine-sigma-list worldMachine)
      (sm-getalphabet newMachine)
      (sm-type newMachine)
      (sm-getstackalphabet newMachine))]
    [(tm)
     (tm-machine
      (addTrueFunctions state-list worldMachine)
      (sm-getstart newMachine)
      (sm-getfinals newMachine)
      (sm-getrules newMachine)
      (decide-machine-input TM-ORIGIONAL-TAPE)
      (sm-getalphabet newMachine)
      (sm-type newMachine)
      (tm-machine-tape-posn worldMachine))]
    [(tm-language-recognizer)
     (lang-rec-machine
      (addTrueFunctions state-list worldMachine)
      (sm-getstart newMachine)
      (sm-getfinals newMachine)
      (sm-getrules newMachine)
      (decide-machine-input TM-ORIGIONAL-TAPE)
      (sm-getalphabet newMachine)
      (sm-type newMachine)
      (tm-machine-tape-posn worldMachine)
      (sm-getaccept newMachine))]
    [else
     (machine
      (addTrueFunctions state-list worldMachine)
      (sm-getstart newMachine)
      (sm-getfinals newMachine)
      (sm-getrules newMachine)
      (machine-sigma-list worldMachine)
      (sm-getalphabet newMachine)
      (sm-type newMachine))]))
      

;; addTrueFunctions: list-of-states -> list-of-states
;; Adds the default true function to every state in the list if it doesnt have a function
(define (addTrueFunctions los m)
  (letrec (
           ;; in-cur-state-list: symbol machine-state-list -> boolean/state-struct
           ;; Purpose: Returns a state-struct if its name is the same as the symbol, otherwise
           ;;   it returns false.
           (in-cur-state-list (lambda (s msl)
                                (cond
                                  [(empty? msl) #f]
                                  [(equal? s (fsm-state-name (car msl))) (car msl)]
                                  [else (in-cur-state-list s (cdr msl))]))))
    (map (lambda (x)
           (let (
                 (state (in-cur-state-list x (machine-state-list m))))
             (cond
               [(not (equal? #f state)) state]
               [else
                (case MACHINE-TYPE
                  [(pda) (fsm-state x PDA-TRUE-FUNCTION (posn 0 0))]
                  [(tm) (fsm-state x TM-TRUE-FUNCTION (posn 0 0))]
                  [(tm-language-recognizer) (fsm-state x TM-TRUE-FUNCTION (posn 0 0))]
                  [else (fsm-state x TRUE-FUNCTION (posn 0 0))])])))
         los)))
  


;; reset-bottom-indices: tm-machine world (optional) -> none
;; Purpose: Resest the bottom indicies to there origional value
(define (reset-bottom-indices type w)
  (cond
    [(or (equal? 'tm type) (equal? 'tm-language-recognizer type))
     (begin
       (set-tm-machine-tape-posn! (world-fsm-machine w) TM-ORIGIONAL-TAPE-POSN)
       (set-tape-index-bottom -1)
       (set-tape-index 0)
       (set-init-index-bottom 0))]
    [else
     (begin
       (set-tape-index-bottom -1)
       (set-tape-index 0)
       (reset-stack)
       (set-stack-index 0)
       (set-init-index-bottom 0))]))
