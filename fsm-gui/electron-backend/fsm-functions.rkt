#lang racket
(require
 json
 "./jsexpr-converters.rkt"
 "../../fsm-core/interface.rkt")
(provide
 fsa->jsexpr
 build-machine)

;; build-machine :: jsexpr -> jsexpr
;; takes the electron-gui machine json-expr, unparses it and runs it in fsm-core. It then
;; returns the machine or the error msg if it fails to build
(define (build-machine data)
  (define (json-null? v) (equal? v (json-null)))
  ;; find-finals :: jsexpr -> listof(symbol)
  ;; returns the final states as a list of symbols
  (define (find-finals states) 
    (map (lambda (s) (string->symbol (hash-ref s 'name)))
         (filter (lambda (s) (or (equal? "startfinal" (hash-ref s 'type))
                                 (equal? "final" (hash-ref s 'type))))
                 states)))
  ;; find-start :: jsexpr -> symbol | false
  ;; returns the symbol of the start state if it exists otherwise returns #f
  (define/match (find-start _states)
    [('()) #f]
    [(`(,(hash-table ('name n) ('type t)) ,xs ...))
     (if (or (equal? "start" t) (equal? "startfinal" t)) (string->symbol n) (find-start xs))])
  (define/match (find-accept _states)
    [('()) #f]
    [(`(,(hash-table ('name n) ('type t)) ,xs ...))
     (if (equal? "accept" t) (string->symbol n) (find-accept xs))])
  ;; parse-rules :: jsexpr symbol -> listof(fsm-core-rules)
  ;; Converts the jsexpr into the fsm-core representation of rules
  (define (parse-rules rules type)
    (map (lambda (r) (match type
                       [(or 'dfa 'ndfa) (list (string->symbol (hash-ref r 'start))
                                              (string->symbol (hash-ref r 'input))
                                              (string->symbol (hash-ref r 'end)))]
                       ['pda
                        (define popped (hash-ref r 'popped))
                        (define pushed (hash-ref r 'pushed))
                        (list (list (string->symbol (hash-ref r 'start))
                                    (string->symbol (hash-ref r 'input))
                                    (if (empty? popped) EMP (map string->symbol popped)))
                              (list (string->symbol (hash-ref r 'end))
                                    (if (empty? pushed) EMP (map string->symbol pushed))))]
                       [(or 'tm 'tm-language-recognizer) "TODO"]))
         rules))
  (define no-dead (hash-ref data 'nodead))
  (define un-parsed-states (hash-ref data 'states))
  (define alpha (map string->symbol (hash-ref data 'alphabet)))
  (define type (string->symbol (hash-ref data 'type)))
  (define states (map (lambda (s) (string->symbol (hash-ref s 'name))) un-parsed-states))
  (define invariants (map (lambda (s) (cons (string->symbol (hash-ref s 'name))
                                            (hash-ref s 'invFunc)))
                          (filter (lambda (s) (not (json-null? (hash-ref s 'invFunc))))
                                  un-parsed-states)))
  (define start (find-start un-parsed-states))
  (define finals (find-finals un-parsed-states))
  (define rules (parse-rules (hash-ref data 'rules) type))
  (define input (map string->symbol (hash-ref data 'input)))
  (define stack-alpha (if (equal? type 'pda) (map string->symbol (hash-ref data 'stackAlpha)) #f))
  (define tape-index (if (or (equal? type 'tm) (equal? type 'tm-language-recognizer))
                         (hash-ref data 'tapeIndex)
                         #f))
  (define accept-state (if (equal? type 'tm-langauge-recognizer) (find-accept un-parsed-states) #f))

  ;; TODO: Talk to marco about how we want to handle fsm-error msgs. Since they print to the
  ;; stdio we cant display them in the GUI. Ideally this would just return a string and we
  ;; could pass the message over json to the new GUI
  (define fsa (build-fsm-core-machine states start finals alpha rules type stack-alpha accept-state no-dead))
  (define trans (sm-showtransitions fsa input))
  (cond
    [(equal? trans 'reject)
     (hash 'data (json-null)
           'responseType "build_machine"
           'error "The given input for the machine was rejected")]
    [fsa
     (hash 'data (hash 'transitions (transitions->jsexpr fsa invariants input tape-index)
                       ;; since fsm sometimes adds states (ds) we will return the list of states,
                       ;; so the gui can update accordingly
                       'states (map (lambda (s) (state->jsexpr s fsa invariants)) (sm-states fsa))
                       ;; same with rules.
                       'rules (map rule->jsexpr (sm-rules fsa)))
           'responseType "build_machine"
           'error (json-null))]
    [else (hash 'data (json-null)
                'responseType "build_machine"
                'error "Failed check-machine function")]))

;; isValidMachine? :: listof(symbol) symbol listof(symbol) listof(symbol) listof(fsm-core-rules) symbol listof(symbol) | false boolean -> fsa | false
;; returns a fsm-core fsa if the machine passes the fsm-core error messages check. If there is an error the
;; error is printed to stdio
(define (build-fsm-core-machine states start finals alpha rules type stack-alpha accept no-dead)
  (define has-error? (match type
                       ['pda (check-machine states alpha finals rules start type stack-alpha)]
                       [(or 'dfa 'ndfa)
                        (check-machine states alpha finals rules start type)]
                       [(or 'tm 'tm-language-recognizer)
                        (check-machine states alpha finals rules start 'tm)]))
  (if (not (boolean? has-error?))
      #f
      (match type
        ['dfa (if no-dead
                  (make-dfa states alpha start finals rules 'nodead)
                  (make-dfa states alpha start finals rules))]
        ['ndfa (if no-dead
                   (make-ndfa states start finals alpha rules 'nodead)
                   (make-ndfa states start finals alpha rules))]
        ;; pda's dont use the dead-state
        ['pda (make-ndpda states alpha stack-alpha start finals rules)]
        ['tm-language-recognizer (make-tm states alpha rules start finals accept)]
        ['tm (make-tm states alpha rules start finals)])))







(module+ test
  (require rackunit
           rackunit/text-ui)

  (define build-machine-tests
    (test-suite "build-machine tests"
                (test-case "dfa"
                  (define a*a-jsexpr (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (v) #t)")
                                                         (hash 'name "A" 'type "normal" 'invFunc (json-null))
                                                         (hash 'name "F" 'type "final" 'invFunc (json-null)))
                                           'alphabet (list "a" "b")
                                           'type "dfa"
                                           'rules (list (hash 'start "S" 'input "a" 'end "F")
                                                        (hash 'start "F" 'input "a" 'end "F")
                                                        (hash 'start "F" 'input "b" 'end "A")
                                                        (hash 'start "S" 'input "b" 'end "A")
                                                        (hash 'start "A" 'input "a" 'end "F")
                                                        (hash 'start "A" 'input "b" 'end "A"))
                                           'input (list "a" "a" "a" "b" "a")
                                           'nodead false))
                  (define expected (hash 'data
                                         (hash
                                          'states (list (hash 'name "ds" 'type "normal" 'invFunc (json-null))
                                                        (hash 'name "S" 'type "start" 'invFunc "(lambda (v) #t)")
                                                        (hash 'name "A" 'type "normal" 'invFunc (json-null))
                                                        (hash 'name "F" 'type "final" 'invFunc (json-null)))
                                          'rules (list (hash 'start "S" 'input "a" 'end "F")
                                                       (hash 'start "F" 'input "a" 'end "F")
                                                       (hash 'start "F" 'input "b" 'end "A")
                                                       (hash 'start "S" 'input "b" 'end "A")
                                                       (hash 'start "A" 'input "a" 'end "F")
                                                       (hash 'start "A" 'input "b" 'end "A")
                                                       (hash 'start "ds" 'input "a" 'end "ds")
                                                       (hash 'start "ds" 'input "b" 'end "ds"))
                                          'transitions (list (hash 'start "S"
                                                                   'invPass #t)
                                                             (hash 'rule (hash 'start "S" 'input "a" 'end "F")
                                                                   'invPass (json-null))
                                                             (hash 'rule (hash 'start "F" 'input "a" 'end "F")
                                                                   'invPass (json-null))
                                                             (hash 'rule (hash 'start "F" 'input "a" 'end "F")
                                                                   'invPass (json-null))
                                                             (hash 'rule (hash 'start "F" 'input "b" 'end "A")
                                                                   'invPass (json-null))
                                                             (hash 'rule (hash 'start "A" 'input "a" 'end "F")
                                                                   'invPass (json-null))
                                                             (hash 'end "F"
                                                                   'action "accept"
                                                                   'invPass (json-null))))
                                         'error (json-null)))
                  (define actual (build-machine a*a-jsexpr))
                  (check-equal? (hash-ref actual 'error)
                                (hash-ref expected 'error)
                                "Error msg field for a*a should be null for a valid machine")
                  (check-equal? (hash-ref actual 'data)
                                (hash-ref expected 'data)
                                "Data for a*a should be the propper json values"))

                (test-case "pda"
                  (define EMP-str (symbol->string EMP))
                  (define pda=2ba-jsexpr (hash 'states (list (hash 'name "S" 'type "start" 'invFunc (json-null))
                                                             (hash 'name "M1" 'type "normal" 'invFunc (json-null))
                                                             (hash 'name "F" 'type "final" 'invFunc (json-null)))
                                               'alphabet '("a" "b")
                                               'stackAlpha '("a" "b")
                                               'rules (list (hash 'start "S" 'input EMP-str 'popped '() 'end "M1" 'pushed '())
                                                            (hash 'start "M1" 'input "a" 'popped '() 'end "M1" 'pushed (list "a" "a"))
                                                            (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                                            (hash 'start "M1" 'input "a" 'popped (list "b") 'end "M1" 'pushed (list "a"))
                                                            (hash 'start "M1" 'input "a" 'popped (list "b" "b") 'end "M1" 'pushed '())
                                                            (hash 'start "M1" 'input "b" 'popped (list "a") 'end "M1" 'pushed '())
                                                            (hash 'start "M1" 'input EMP-str 'popped '() 'end "F" 'pushed '()))
                                               'input '("b" "b" "a")
                                               'nodead false
                                               'type "pda"))
                  (define expected (hash 'data
                                         (hash
                                          'states (list (hash 'name "S" 'type "start" 'invFunc (json-null))
                                                        (hash 'name "M1" 'type "normal" 'invFunc (json-null))
                                                        (hash 'name "F" 'type "final" 'invFunc (json-null)))
                                          'rules (list (hash 'start "S" 'input EMP-str 'popped '() 'end "M1" 'pushed '())
                                                       (hash 'start "M1" 'input "a" 'popped '() 'end "M1" 'pushed (list "a" "a"))
                                                       (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                                       (hash 'start "M1" 'input "a" 'popped (list "b") 'end "M1" 'pushed (list "a"))
                                                       (hash 'start "M1" 'input "a" 'popped (list "b" "b") 'end "M1" 'pushed '())
                                                       (hash 'start "M1" 'input "b" 'popped (list "a") 'end "M1" 'pushed '())
                                                       (hash 'start "M1" 'input EMP-str 'popped '() 'end "F" 'pushed '()))
                                          'transitions (list
                                                        (hash 'start "S"
                                                              'invPass (json-null))
                                                        (hash 'rule (hash 'start "S" 'input EMP-str 'popped '() 'end "M1" 'pushed '())
                                                              'invPass (json-null)
                                                              'stack (list))
                                                        (hash 'rule (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                                              'invPass (json-null)
                                                              'stack (list "b"))
                                                        (hash 'rule (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                                              'invPass (json-null)
                                                              'stack (list "b" "b"))
                                                        (hash 'rule (hash 'start "M1" 'input "a" 'popped (list "b" "b") 'end "M1" 'pushed '())
                                                              'invPass (json-null)
                                                              'stack (list))
                                                        (hash 'rule (hash 'start "M1" 'input EMP-str 'popped '() 'end "F" 'pushed '())
                                                              'invPass (json-null)
                                                              'stack (list))
                                                        (hash 'end "F"
                                                              'action "accept"
                                                              'invPass (json-null))))
                                          'error (json-null)))
                    (define actual (build-machine pda=2ba-jsexpr))
                    (check-equal? (hash-ref actual 'error)
                                  (hash-ref expected 'error)
                                  "Error msg field for pda=2ba should be null for a valid machine")
                    (check-equal? (hash-ref actual 'data)
                                  (hash-ref expected 'data)
                                  "Data for pda=2ba should be the propper json values"))))

    (run-tests build-machine-tests)

    ) ;; end module test
