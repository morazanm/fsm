#lang racket
(require json
         "./jsexpr-converters.rkt"
         "./parsers.rkt"
         "../../fsm-core/interface.rkt"
         "../../fsm-gviz/interface.rkt")
(provide
 fsa->jsexpr
 build-machine
 regenerate-graph)

;; build-machine :: jsexpr optional(listof(cons symbol func) optional(boolean) -> jsexpr
;; takes the electron-gui machine json-expr, unparses it and runs it in fsm-core. It then
;; returns the machine or the error msg if it fails to build
;;
;; If pre-computed-invariants is supplied then they are used instead of the invarint string
;; from the json file
;;
;; The optional arg when set to true will not build the graphviz graph. This should be set
;; to true when using this function in tests
(define (build-machine data (pre-computed-invariants '()) #:test (test-mode #f))
  ;; append-gviz-svgs :: listof(jsexpr-transition) -> listof(jsexpr-transitions)
  ;; computes the graphviz images for each of the transitions
  (define (append-gviz-svgs trans)
    (map (lambda (t i)
           (hash-set t
                     'filepath
                     (path->string (electron-machine->svg states start finals rules type t accept-state 0 i))))
         trans
         (range (length trans))))
  (define no-dead (hash-ref data 'nodead))
  (define un-parsed-states (hash-ref data 'states))
  (define alpha (parse-alpha data))
  (define type (parse-type data))
  (define states (parse-states un-parsed-states))
  (define invariants (if (null? pre-computed-invariants) (parse-invariants un-parsed-states) pre-computed-invariants))
  (define start (parse-start un-parsed-states))
  (define finals (parse-finals un-parsed-states))
  (define rules (parse-rules (hash-ref data 'rules) type))
  (define input (parse-input data))
  (define stack-alpha (parse-stack-alpha data type))
  (define tape-index (if (or (equal? type 'tm) (equal? type 'tm-language-recognizer))
                         (hash-ref data 'tapeIndex)
                         #f))
  (define accept-state (if (equal? type 'tm-language-recognizer) (parse-accept un-parsed-states) #f))

  ;; TODO: Talk to marco about how we want to handle fsm-error msgs. Since they print to the
  ;; stdio we cant display them in the GUI. Ideally this would just return a string and we
  ;; could pass the message over json to the new GUI
  (define fsa (build-fsm-core-machine states start finals alpha rules type stack-alpha accept-state no-dead))
  (define trans (if fsa (sm-showtransitions fsa input) #f))
  (cond
    [(equal? trans 'reject)
     (hash 'data (json-null)
           'responseType "build_machine"
           'error "The given input for the machine was rejected")]
    [fsa
     (define jsexpr-trans (transitions->jsexpr fsa invariants input tape-index))
     (hash 'data (hash 'transitions (if (or test-mode (not (has-dot-executable?)))
                                        jsexpr-trans
                                        (append-gviz-svgs jsexpr-trans))
                       ;; since fsm sometimes adds states (ds) we will return the list of states,
                       ;; so the gui can update accordingly
                       'states (map (lambda (s) (state->jsexpr s fsa (if (null? pre-computed-invariants) invariants '())))
                                    (sm-states fsa))
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
                        (check-machine states alpha finals rules start 'tm)]
                       [_ (error 'build-fsm-core-machine "Unsupported machine type ~a" type)]))
  (if (not (boolean? has-error?))
      #f
      (match type
        ['dfa (if no-dead
                  (make-dfa states alpha start finals rules 'nodead)
                  (make-dfa states alpha start finals rules))]
        ['ndfa (if no-dead
                   (make-ndfa states alpha start finals rules 'nodead)
                   (make-ndfa states alpha start finals rules))]
        ;; pda's dont use the dead-state
        ['pda (make-ndpda states alpha stack-alpha start finals rules)]
        ['tm-language-recognizer (make-tm states alpha rules start finals accept)]
        ['tm (make-tm states alpha rules start finals)]
        [_ (error 'build-fsm-core-machine "Unsupported machine type ~a" type)])))


;; regenerate-graph :: jsexpr -> jsexpr
;; rebuilds the graphviz image and returns the filepath to where the image was saved
;; HACK: The electron browser caches the image once it is loaded. This means that if we update the
;; image and supply the same filepath then the browser will not realize that the image was updated.
;; The current workaround is to ping-pong between two image file paths to force the browser to update
;; the image. The current 2 filenames are:
;; - vizTool_electron1
;; - vizTool_electron2
(define (regenerate-graph data)
  (define un-parsed-states (hash-ref data 'states))
  (define current-filename
    (with-handlers ([exn:fail (lambda () "vizTool_electron1")])
      (path->string
       (file-name-from-path
        (path-replace-extension (string->path (hash-ref data 'currentFilepath)) "")))))
  (define new-file-name (regexp-replace #rx"1|2" current-filename (lambda (v) (if (equal? "1" v) "2" "1"))))
  (define type (parse-type data))
  (hash 'data (hash 'filepath
                    (if (not (has-dot-executable?))
                        (json-null)
                        (path->string (electron-machine->svg
                                       (parse-states un-parsed-states)
                                       (parse-start un-parsed-states)
                                       (parse-finals un-parsed-states)
                                       (parse-rules (hash-ref data 'rules) type)
                                       type
                                       (hash)
                                       (parse-accept un-parsed-states)
                                       0
                                       new-file-name))))
        'responseType "redraw"
        'error (json-null)))



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
                  (define actual (build-machine a*a-jsexpr #:test #t))
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
                    (define actual (build-machine pda=2ba-jsexpr #:test #t))
                    (check-equal? (hash-ref actual 'error)
                                  (hash-ref expected 'error)
                                  "Error msg field for pda=2ba should be null for a valid machine")
                    (check-equal? (hash-ref actual 'data)
                                  (hash-ref expected 'data)
                                  "Data for pda=2ba should be the propper json values"))

                (test-case "tm"
                  (define Ma-jsexpr (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (v k) #t)")
                                                        (hash 'name "H" 'type "final" 'invFunc (json-null)))
                                           'alphabet (list "a" "b" "@")
                                           'type "tm"
                                           'rules (list (hash 'start "S" 'startTape "@" 'end "S" 'endTape "R")
                                                        (hash 'start "S" 'startTape '("a") 'end "H" 'endTape '("a"))
                                                        (hash 'start "S" 'startTape '("b") 'end "H" 'endTape '("a"))
                                                        (hash 'start "S" 'startTape "_" 'end "H" 'endTape '("a")))
                                           'input (list "@" "b" "b")
                                           'tapeIndex 1
                                           'nodead false))
                  (define expected (hash 'data
                                         (hash
                                          'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (v k) #t)")
                                                        (hash 'name "H" 'type "final" 'invFunc (json-null)))
                                          'rules (list (hash 'start "S" 'startTape "@" 'end "S" 'endTape "R")
                                                       (hash 'start "S" 'startTape '("a") 'end "H" 'endTape '("a"))
                                                       (hash 'start "S" 'startTape '("b") 'end "H" 'endTape '("a"))
                                                       (hash 'start "S" 'startTape "_" 'end "H" 'endTape '("a")))
                                          'transitions (list (hash 'start "S"
                                                                   'tapeIndex 1
                                                                   'tape (list (symbol->string LM) "b" "b")
                                                                   'invPass #t)
                                                             (hash 'rule (hash 'start "S" 'startTape '("b") 'end "H" 'endTape '("a"))
                                                                   'tapeIndex 1
                                                                   'tape (list (symbol->string LM) "a" "b")
                                                                   'invPass (json-null))
                                                             (hash 'end "H"
                                                                   'tapeIndex 1
                                                                   'tape (list (symbol->string LM) "a" "b")
                                                                   'invPass (json-null))))
                                         'error (json-null)))
                  (define actual (build-machine Ma-jsexpr #:test #t))
                  (check-equal? (hash-ref actual 'error)
                                (hash-ref expected 'error)
                                "Error msg field for Ma should be null for a valid machine")
                  (check-equal? (hash-ref actual 'data)
                                (hash-ref expected 'data)
                                "Data for Ma should be the propper json values"))))

    (run-tests build-machine-tests)

    ) ;; end module test
