(module abstract-predicate racket
  (require "constants.rkt" 
           )
  (provide make-dfst)
  (require test-engine/racket-tests)

  ;make-dfst: (listof symbols) (listof symbols) symbol (listof symbols) (listof rules)
  (define (make-dfst K Σ s F δ)
    ;K - set of states
    ;Σ - set of alphabet characters
    ;s - start state
    ;F - set of final states
    ;δ - transitional function
    ;      a rule must be
    ;        state
    ;        tape element
    ;        to state
    ;        output
    (local [(define states K)
            (define alphabet Σ)
            (define start s)
            (define finals F)
            (define transition δ);pluralize, automatically add dead states

            ;manager: key --> Deterministic Transducer Interface
            ;purpose: to either use a key to retrieve data from the dfst or apply
            ;         the rules of the transducer to the list of symbols
            (define (manager key)
              (local [;find-rule: char state rules --> (output state)/false
                      (define (find-rule in current rules)
                        (cond [(empty? rules) #f]
                              [(and (equal? (first (car rules)) current)
                                    (equal? (second (car rules)) in)) (list (third (car rules))
                                                                            (fourth (car rules)))]
                              [else (find-rule in current (cdr rules))])) ;;use filter, there will always be a rule
                      ;                                                     if the rule goes to the ds, stop,
                    
                      ;apply-transition: (listof symbols) state (listof transitions) --> (listof transitions)
                      (define (apply-transition input current accum)
                        (cond [(empty? input) (if (member current finals) (cons 'accept accum) (cons 'reject accum))]
                              [else (local [(define output (find-rule (car input) current transition))]
                                      (cond [(boolean? output) (cons 'reject accum)]
                                            [else (apply-transition (cdr input) (car output) (cons (list (cons (cadr output) (car (car accum)))
                                                                                                         (cdr input)
                                                                                                         (car output))
                                                                                                   accum))
                                                  ]))])
                        )
                      ]
                (cond [(not (symbol? key)) (error "The input to the transducer must be a symbol, contact fsm developers")]
                      [(or (equal? key 'apply)
                           (equal? key 'transitions)) (lambda (word) (local [(define transitions (apply-transition word start (list (list empty))))]
                                                                       (if (symbol=? key 'apply) (if (equal? (car transitions) 'reject) 'reject
                                                                                                     (if (equal? (car transitions) 'accept) (car (cadr transitions))
                                                                                                         (car (car transitions))))
                                                                           (reverse transitions))))]
                      ;where x is the required input of a word (aka a list of symbols)
                      [(symbol=? key 'get-states) states]
                      [(symbol=? key 'get-sigma) alphabet]
                      [(symbol=? key 'get-start) start]
                      [(symbol=? key 'get-finals) finals]
                      [(symbol=? key 'get-deltas) transition]
                      [(symbol=? key 'whatami) 'dfst]
                      [(symbol=? key 'deterministic?) #t]
                      [(symbol=? key 'apply) (lambda (x) (x start '()))]
                      [else (error (format "The command ~s is not recognized by the interface" key))])
                )
              )
            ]
      manager))
) ;; end module