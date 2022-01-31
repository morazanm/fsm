#lang racket
(require "../fsm-main.rkt" "./structs/machine.rkt" "./structs/state.rkt" "globals.rkt" "./structs/posn.rkt"
         "./gui.rkt" "./structs/world.rkt")

(provide parse-input)

;; run :: machine -> GUI
;; This function kicks off the GUI
(define (run machine)
  (kick-off-gui machine))

;; parse-input :: fsm-machine | symbol -> listOfArgs -> machine | GUI
;; Given the user input this is incharge of creating the approperate internal-representaion
;; for a machine
;; NOTE: the test keyword arg should be set to true for debugging the machine that is created
(define (parse-input fsm-machine #:test[test-mode #f] . varargs)
  (define machine (match fsm-machine
                    [(? symbol?) (build-new-machine fsm-machine)]
                    [_ (build-existing-machine fsm-machine varargs)]))
  (if test-mode
      machine
      (run machine)))

;; build-new-machine :: symbol -> machine
;; In charge of creating new machines
(define (build-new-machine machine-type)
  (match machine-type
    ['dfa make-new-dfa]
    ['ndfa make-new-ndfa]
    ['pda make-new-pda]
    ['tm make-new-tm]
    ['tm-language-recognizer make-new-lang-req]
    [else (error (format "~s is not a valid machine type" machine-type))]))

;; build-existing-machine :: fms-machine -> machine
;; In charge of taking a fsm machine and converting it to the GUI interal representation
;; of a machine where each state is mapped to its appropriate invariant, if no invariant is
;; supplied then we map it to the true function
(define (build-existing-machine fsm-machine invariants)
  (define state-list (sm-getstates fsm-machine))
  (define type (sm-type fsm-machine))
  (define true-function (match type
                          [(or 'dfa 'ndfa) TRUE-FUNCTION]
                          ['pda PDA-TRUE-FUNCTION]
                          [_ TM-TRUE-FUNCTION]))
  ;; Looks for the corrsponding invariant function given a state
  (define (get-invariant-for-state state loi)
    (cond
      [(empty? loi) '()]
      [(equal? (caar loi) state) (car loi)]
      [else (get-invariant-for-state state (cdr loi))]))
  ;; Builds a list for states with their invariants, if a invariant is not supplied for a state
  ;; then a default one is given
  (define map-states-to-invariants
    (for/list [(state state-list)]
      (define state-inv (get-invariant-for-state state invariants))
      (if (empty? state-inv)
          (fsm-state state true-function (posn 0 0))
          (fsm-state state (cadr state-inv) (posn 0 0)))))           
  (match type
    [(or 'dfa 'ndfa) (machine map-states-to-invariants
                              (sm-getstart fsm-machine)
                              (sm-getfinals fsm-machine)
                              (reverse (sm-getrules fsm-machine))
                              '()
                              (sm-getalphabet fsm-machine)
                              type)]
    ['pda (pda-machine map-states-to-invariants
                       (sm-getstart fsm-machine)
                       (sm-getfinals fsm-machine)
                       (reverse (sm-getrules fsm-machine))
                       '()
                       (sm-getalphabet fsm-machine)
                       type
                       (sm-getstackalphabet fsm-machine))]
    ['tm (tm-machine map-states-to-invariants
                     (sm-getstart fsm-machine)
                     (sm-getfinals fsm-machine)
                     (reverse (sm-getrules fsm-machine))
                     `(,LM)
                     (sm-getalphabet fsm-machine)
                     type
                     0)]
    ['tm-language-recognizer (lang-rec-machine map-states-to-invariants
                                               (sm-getstart fsm-machine)
                                               (sm-getfinals fsm-machine)
                                               (reverse (sm-getrules fsm-machine))
                                               `(,LM)
                                               (sm-getalphabet fsm-machine)
                                               type
                                               0
                                               (sm-getaccept fsm-machine))]
    [_ (error (format "~s is not a valid machine type" fsm-machine))]))
   
   
   
  