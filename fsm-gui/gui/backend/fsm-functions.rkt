#lang racket
(require json
         "./jsexpr-converters.rkt"
         "./parsers.rkt"
         "../../../fsm-core/interface.rkt"
         "../../../fsm-gviz/interface.rkt")
(provide
 fsa->jsexpr
 build-machine
 regenerate-graph
 recompute-invariant)

;; build-machine :: jsexpr namespace | false optional(boolean) -> jsexpr
;; takes the electron-gui machine json-expr, unparses it and runs it in fsm-core. It then
;; returns the machine or the error msg if it fails to build
;;
;; If pre-computed-invariants is supplied then they are used instead of the invarint string
;; from the json file
;;
;; The optional arg when set to true will not build the graphviz graph. This should be set
;; to true when using this function in tests
(define (build-machine data namespace #:test (test-mode #f))
  ;; generate-graphviz-images :: listof(jsexpr-transition) -> listof(jsexpr-transitions)
  ;; computes the graphviz images for each of the transtions using threads
  (define (generate-graphviz-images-threaded transitions)
    (define start-time (current-seconds))
    (define (make-graphviz-img-thread)
      (thread (lambda ()
                (let loop ()
                  (match (thread-receive)
                    [(list states start finals rules type transition accept-state i res-thread)
                     (thread-send
                      res-thread
                      (cons i (path->string (electron-machine->svg states
                                                                   start
                                                                   finals
                                                                   rules
                                                                   type
                                                                   transition
                                                                   accept-state 0 i))))
                     (loop)])))))

    (begin
      ;; Note: After some testing it seems that 3 threads is sweet spot when
      ;; dealing with large machine (more then 120 transitions)
      (define workers (vector (make-graphviz-img-thread)
                              (make-graphviz-img-thread)
                              (make-graphviz-img-thread)))
      ;; start thread tasks
      (for ([tran transitions]
            [i (length transitions)])
        (thread-send (vector-ref workers (modulo i (vector-length workers)))
                     (list states start finals rules type tran accept-state i (current-thread))))

      ;; handle thread results
      (define data (for/hash ([trans transitions]
                              [i (length transitions)])
                     (define res (thread-receive))
                     (values (car res) (cdr res))))

      ;; clean up the threads
      (for ([worker workers])
        (kill-thread worker))

      (define return-data  (map (lambda (t i)
                                  (hash-set t 'filepath (hash-ref data i)))
                                transitions
                                (range (length transitions))))
      (displayln (format "Time to create gviz imgs: ~s" (- (current-seconds) start-time)))
      return-data))

  (define no-dead (hash-ref data 'nodead))
  (define un-parsed-states (hash-ref data 'states))
  (define alpha (parse-alpha data))
  (define type (parse-type data))
  (define states (parse-states un-parsed-states))
  (define invariants (parse-invariants un-parsed-states))
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
     (define jsexpr-trans (transitions->jsexpr fsa invariants input namespace tape-index))
     (hash 'data (hash 'transitions (if (or test-mode (not (has-dot-executable?)))
                                        jsexpr-trans
                                        (generate-graphviz-images-threaded jsexpr-trans))
                       ;; since fsm sometimes adds states (ds) we will return the list of states,
                       ;; so the gui can update accordingly
                       'states (map (lambda (s) (state->jsexpr s fsa invariants))
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
      (get-filename (string->path (hash-ref data 'currentFilepath)))))
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



;; recompute-invariant :: jsexpr namespace optional(boolean) -> jsexpr
;; Recomputes the invariants for the machine
;; ASSUMPTION: We are assuming that the current machine has already been verified by fsm
;;
;; The optional arg when set to true will not build the graphviz graph. This should be set
;; to true when using this function in tests
(define (recompute-invariant data namespace #:test (test-mode #f))
  (define no-dead (hash-ref data 'nodead #t))
  (define un-parsed-states (hash-ref data 'states))
  (define alpha (parse-alpha data))
  (define type (parse-type data))
  (define states (parse-states un-parsed-states))
  (define invariants (parse-invariants un-parsed-states))
  (define start (parse-start un-parsed-states))
  (define finals (parse-finals un-parsed-states))
  (define rules (parse-rules (hash-ref data 'rules) type))
  (define input (parse-input data))
  (define stack-alpha (parse-stack-alpha data type))
  (define tape-index (if (or (equal? type 'tm) (equal? type 'tm-language-recognizer))
                         (hash-ref data 'tapeIndex)
                         #f))
  (define accept-state (if (equal? type 'tm-language-recognizer) (parse-accept un-parsed-states) #f))
  (define fsa (build-fsm-core-machine states start finals alpha rules type stack-alpha accept-state no-dead))
  (define trans (filter (lambda (t)
                          (define end-state (if (hash-has-key? t 'rule)
                                                (hash-ref (hash-ref t 'rule) 'end #f)
                                                (hash-ref t 'start (hash-ref t 'end #f))))
                          (equal? (hash-ref data 'targetState) end-state))
                        (transitions->jsexpr fsa invariants input namespace tape-index)))
  (define generated-imgs
    (for/list ([t trans]
               [s (hash-ref data 'invStatuses)]
               #:when (not (equal? (hash-ref s 'status) (hash-ref t 'invPass))))
      
      (define filename (string-append (get-filename (build-path (hash-ref s 'filepath))) "_new"))
      ;; If the inv failed to compute we will return the old filepath and report the error
      ;; otherwise we recompute the image and send the updated file path
      (hash 'status (hash-ref t 'invPass)
            'index (hash-ref s 'index)
            'filepath (cond
                        [(string? (hash-ref t 'invPass)) (hash-ref s 'filepath)]
                        [test-mode filename]
                        [else (path->string (electron-machine->svg states
                                                                   start
                                                                   finals
                                                                   rules
                                                                   type
                                                                   t
                                                                   accept-state
                                                                   0
                                                                   filename))]))))
  (hash 'data (hash 'targetState (hash-ref data 'targetState)
                    'changedStatuses generated-imgs)
        'responseType "recompute_inv"
        'error (json-null)))


;; get-filename ::path -> path 
;; returns the filename from the given path
(define (get-filename path)
  ((compose1 path->string file-name-from-path path-replace-extension) path ""))



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
                           (define actual (build-machine a*a-jsexpr (current-namespace) #:test #t))
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
                           (define actual (build-machine pda=2ba-jsexpr (current-namespace) #:test #t))
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
                           (define actual (build-machine Ma-jsexpr (current-namespace) #:test #t))
                           (check-equal? (hash-ref actual 'error)
                                         (hash-ref expected 'error)
                                         "Error msg field for Ma should be null for a valid machine")
                           (check-equal? (hash-ref actual 'data)
                                         (hash-ref expected 'data)
                                         "Data for Ma should be the propper json values"))))
  (define recompute-invariant-tests
    (test-suite "recompute-invariant"
                (test-case "No Errors"
                           (define a*a-jsexpr (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (v) #f)")
                                                                  (hash 'name "A" 'type "normal" 'invFunc "(lambda (v) #t)")
                                                                  (hash 'name "F" 'type "final" 'invFunc "(lambda (v) #t)"))
                                                    'alphabet (list "a" "b")
                                                    'type "dfa"
                                                    'rules (list (hash 'start "S" 'input "a" 'end "F")
                                                                 (hash 'start "F" 'input "a" 'end "F")
                                                                 (hash 'start "F" 'input "b" 'end "A")
                                                                 (hash 'start "S" 'input "b" 'end "A")
                                                                 (hash 'start "A" 'input "a" 'end "F")
                                                                 (hash 'start "A" 'input "b" 'end "A"))
                                                    'input (list "a" "a")
                                                    'nodead false
                                                    'targetState "S"
                                                    'invStatuses (list (hash 'index 0 'status #t 'filepath "test_1.svg")
                                                                       (hash 'index 1 'status #f 'filepath "test_2.svg")
                                                                       (hash 'index 2 'status #t 'filepath "test_3.svg"))))
                           
                           (check-equal? (recompute-invariant a*a-jsexpr (current-namespace) #:test #t)
                                         (hash 'data (hash 'changedStatuses (list (hash 'status #f 'index 0 'filepath "test_1_new"))
                                                           'targetState "S")
                                               'error (json-null)
                                               'responseType "recompute_inv")))
                (test-case "Updates for a*"
                           (define a-aUb*-jexpr #hash((alpha . ("a" "b"))
                                                      (filepath . "/var/tmp/vizTool_electron1.svg")
                                                      (hotReload . #t)
                                                      (alphabet . ("a" "b"))
                                                      (graphVizImage . "/var/tmp/vizTool_electron1.svg")
                                                      (initialTapePosition . 0)
                                                      (input . ("a" "a" "a"))
                                                      (nodead . #t)
                                                      (targetState . "F")
                                                      (rules . (#hash((end . "F") (input . "a") (start . "S"))
                                                                #hash((end . "F") (input . "a") (start . "F"))
                                                                #hash((end . "F") (input . "b") (start . "F"))))
                                                      (invStatuses . (#hash((filepath . "/var/tmp/viztool_2.svg") (index . 1) (status . #t))
                                                                      #hash((filepath . "/var/tmp/viztool_3.svg") (index . 2) (status . #t))
                                                                      #hash((filepath . "/var/tmp/viztool_4.svg") (index . 3) (status . #t))))
                                                      (states . (#hash((invFunc . "(define (S-inv ci) (empty? ci))") (name . "S") (type . "start"))
                                                                 #hash((invFunc . "(define (F-inv ci) (define ans (and (empty? ci) (eq? (first ci) 'a) (andmap (λ (s) (or (eq? s 'a) (eq? s 'b))) (rest ci))))\n  ans)")
                                                                       (name . "F") (type . "final"))))
                                                      (type . "ndfa")
                                                      (error . null)
                                                      (responseType . "prebuilt_machine")))
                           (define actual (recompute-invariant a-aUb*-jexpr (current-namespace) #:test #t))

                           
                           (check-equal? actual #hash((data
                                                       .
                                                       #hash((changedStatuses
                                                              .
                                                              (#hash((filepath . "viztool_2_new") (index . 1) (status . #f))
                                                               #hash((filepath . "viztool_3_new") (index . 2) (status . #f))
                                                               #hash((filepath . "viztool_4_new") (index . 3) (status . #f))))
                                                             (targetState . "F")))
                                                      (error . null)
                                                      (responseType . "recompute_inv"))))

                           
                (test-case "No Updates"
                           (define a*a-jsexpr (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (v) #t)")
                                                                  (hash 'name "A" 'type "normal" 'invFunc "(lambda (v) #t)")
                                                                  (hash 'name "F" 'type "final" 'invFunc "(lambda (v) #t)"))
                                                    'alphabet (list "a" "b")
                                                    'type "dfa"
                                                    'rules (list (hash 'start "S" 'input "a" 'end "F")
                                                                 (hash 'start "F" 'input "a" 'end "F")
                                                                 (hash 'start "F" 'input "b" 'end "A")
                                                                 (hash 'start "S" 'input "b" 'end "A")
                                                                 (hash 'start "A" 'input "a" 'end "F")
                                                                 (hash 'start "A" 'input "b" 'end "A"))
                                                    'input (list "a" "a")
                                                    'nodead false
                                                    'targetState "S"
                                                    'invStatuses (list (hash 'index 0 'status #t 'filepath "test_1.svg")
                                                                       (hash 'index 1 'status #f 'filepath "test_2.svg")
                                                                       (hash 'index 2 'status #t 'filepath "test_3.svg"))))
                           
                           (check-equal? (recompute-invariant a*a-jsexpr (current-namespace) #:test #t)
                                         (hash 'data (hash 'changedStatuses (list)
                                                           'targetState "S")
                                               'error (json-null)
                                               'responseType "recompute_inv")))
                (test-case "invalid syntax"
                           (define a*a-jsexpr (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (v) #t")
                                                                  (hash 'name "A" 'type "normal" 'invFunc "(lambda (v) #k)")
                                                                  (hash 'name "F" 'type "final" 'invFunc "(lambda (v) #t)"))
                                                    'alphabet (list "a" "b")
                                                    'type "dfa"
                                                    'rules (list (hash 'start "S" 'input "a" 'end "F")
                                                                 (hash 'start "F" 'input "a" 'end "F")
                                                                 (hash 'start "F" 'input "b" 'end "A")
                                                                 (hash 'start "S" 'input "b" 'end "A")
                                                                 (hash 'start "A" 'input "a" 'end "F")
                                                                 (hash 'start "A" 'input "b" 'end "A"))
                                                    'input (list "a" "a")
                                                    'nodead false
                                                    'targetState "S"
                                                    'invStatuses (list (hash 'index 0 'status #f 'filepath "test_0.svg")
                                                                       (hash 'index 1 'status #f 'filepath "test_1.svg")
                                                                       (hash 'index 2 'status #f 'filepath "test_2.svg"))))
                           (define actual (recompute-invariant a*a-jsexpr (current-namespace) #:test #t))
                           
                           (check-equal? (hash-ref actual 'error) (json-null))
                           (check-equal? (hash-ref actual 'responseType) "recompute_inv")
                           (check-equal? (hash-ref (hash-ref actual 'data) 'targetState) "S")

                           (define data (list-ref (hash-ref (hash-ref actual 'data) 'changedStatuses) 0))
                           (check-equal? (hash-ref data 'filepath) "test_0.svg")
                           (check-equal? (hash-ref data 'index) 0)
                           (check-regexp-match "read: expected a `\\)` to close `\\(" (hash-ref data 'status)))))

  (run-tests build-machine-tests)
  (run-tests recompute-invariant-tests)
  ) ;; end module test
