(module machine-predicate racket
  (require "constants.rkt" "sm-getters.rkt" "fsa.rkt" "tm.rkt" "pda.rkt")
  (provide valid-machine?)
  

  ;valid-machine?: machine --> boolean
  ;purpose: determine the validity of any machine
  (define (valid-machine? the-machine)
    (local [(define the-type (the-machine 'whatami 0 'whatami))

            (define d the-type)
          
            (define s (sm-getstart the-machine))
            (define S (sm-getstates the-machine))
            (define Σ (sm-getalphabet the-machine))
            (define delta (sm-getrules the-machine))

            (define (format-error the-list)
              (if (list? the-list)
                  (if (null? the-list) " "
                      (local [(define a-list (filter (lambda (x) (not (equal? x #t))) (flatten the-list)))]
                        (string-append (car a-list)
                                       (string-append "
"
                                                      (format-error (cdr a-list))))))
                  #t))
          
            ;no-repeats?: (listof items) --> boolean/ listof repeated symbols
            ;purpose: determine if there are doubles of the rule conditions in a set
            (define (no-repeats? a-list)
              (local [;repeat-accum: (listof symbols) (listof symbols) --> listof symbols
                      ;purpose: accumulate a list of symbols that repeat
                      ;accumulator invariant: accum starts as empty and on each recursion adds the first
                      ;                       symbol in init added to the list if that symbol is in the
                      ;                       rest of init
                      (define (repeat-accum init accum)
                        (cond [(empty? init) accum]
                              [(set-member? (rest init) (first init)) (repeat-accum (rest init)
                                                                                    (cons (first init)
                                                                                          accum))]
                              [else (repeat-accum (rest init) accum)]))
                      (define repeats (repeat-accum a-list empty))]
                (cond [(empty? repeats) true]
                      [else repeats])))
          
            ;CHECKING FOR VALID STATE
            ;valid-state?: symbol --> boolean/invalid-state
            ;purpose: determine if a given symbol is a valid state
            (define (valid-state? a-state)
              (local [;check-first: symbol --> boolean/invalid-state
                      ;purpose: make sure the symbol starts with a letter
                      (define (check-first a-string)
                        (if (and (char-alphabetic? (string-ref a-string 0))
                             (char-upper-case? (string-ref a-string 0))) #t a-string))]
                (cond [(or (not (symbol? a-state))
                           (symbol=? a-state BLANK)
                           (symbol=? a-state EMP)) a-state]
                      [else (check-first (symbol->string a-state))])))
          
            ;VALID SET OF STATES?
            ;valid-state-set?: (listof states) --> boolean/string
            ;purpose: determine if a set of states is a valid set of states
            (define (valid-states? state-set)
              (local [(define invalid-states (filter (lambda (l) (or (equal? 'ds l)
                                                                     (not (equal? true l)))) (map (lambda (x) (valid-state? x)) state-set)))
                      (define the-set (no-repeats? state-set))]
                (cond [(empty? state-set) "the set of states cannot be empty"]
                      [(and (not (list? the-set))
                            (empty? invalid-states)) true]
                      [(and (list? the-set)
                            (not (empty? invalid-states))) (format "~s are not valid uppercase alphabetic states and ~s are repeated values in the set ~s" invalid-states the-set state-set)]
                      [(list? the-set) (format "~s are repeated in the given state set~s " the-set state-set)]
                      [else (format "~s are not valid uppercase alphabetic states" invalid-states)])))
          
            ;VALID STARTING STATE
            ;valid-start?: state --> boolean/string
            ;purpose: to check if a given state is a valid starting state
            (define (valid-start? starter)
              (cond [(not (valid-state? starter)) (format "~s must be a valid state, to be the starting state" starter)]
                    [(not (set-member? S starter)) (format "~s must be in the list of states for the machine in order to be a valid starting state" starter)]
                    [else true]))
          
            ;VALID FINAL LIST OF STATES?
            ;valid-finals?: (listof states) --> boolean/string
            ;purpose: determine if the listof final states is valid
            (define (valid-finals? F)
              (local [;check-final: state --> state/boolean
                      ;purpose: determine if a state is a valid final state
                      (define (check-final final)
                        (cond [(set-member? S final) true]
                              [else final]))
                    
                      (define invalid-finals (filter (lambda (x) (not (equal? true (check-final x)))) F))
                    
                      (define repeats (no-repeats? F))]
              
                (cond [(and (empty? invalid-finals)
                            (equal? true repeats)) true]
                      [(equal? true repeats) (format "~s cannot be in a final state because they are not in the initial list of states for the machine" invalid-finals)]
                      [(empty? invalid-finals) (format "~s are repeats in the set of final states" repeats)]
                      [else (format "~s cannot be a final states because they are not in the initial list of states for the machine and ~s are repeats" invalid-finals repeats)])))
          
            ;VALID SIGMA
            ;valid-sigma?: (listof symbols) --> boolean
            ;purpose: determine if a sigma is valid for any kind of state
            (define (valid-sigma? sigma)
              (local [;valid-sigma-dfa?: (listof symbols) --> boolean/string
                      ;purpose: tests if an alphabet is valid to a dfa
                      (define (valid-sigma-dfa? sigma)
                        (local [(define repeats (no-repeats? sigma))
                                (define (create-error)
                                  (local [(define invalid-sigmas (filter (lambda (x) (or (not (symbol? x))
                                                                                         (not (char-lower-case? (string-ref (symbol->string x) 0)))
                                                                                         (or (equal? EMP x)
                                                                                             (equal? BLANK x)
                                                                                             (equal? LM x)
                                                                                             (equal? RIGHT x)))) sigma))
                                          ]
                                    (cond [(and (equal? repeats true)
                                                (empty? invalid-sigmas)) true]
                                          [(and (not (equal? repeats true))
                                                (not (empty? invalid-sigmas))) (format "~s are now valid lowercase alphabetic alphabet members and ~s are repeated values in the sigma" invalid-sigmas repeats)]
                                          [(equal? repeats true) (format "~s are not valid lowercase alphabetic alphabet members" invalid-sigmas)]
                                          [else (format "~s are repeated values in the sigma" repeats)])))]
                        
                          (cond [(empty? sigma) #t]
                                [(symbol? repeats) (format "the symbol ~s appears in the alphabet more than once" repeats)]
                                [else (create-error)])))
                    
                      ;valid-sigma-ndfa/pda: (listof symbols) --> boolean/string
                      ;purpose: test if a list of symbols is a valid alphabet for a ndfa or pda
                      (define (valid-sigma-ndfa/pda? sigma)
                            (valid-sigma-dfa? (filter (lambda (x) (and (not (equal? x EMP)) (not (equal? x LM)) (not (equal? x RIGHT)))) sigma)))
                    
                      ;valid-simga-tm?: (listof symbols) --> boolean/string
                      ;purpose: test if a list of symbols is a valid alphabet for a tm
                      (define (valid-sigma-tm? sigma)
                        (valid-sigma-ndfa/pda? (filter (lambda (x) (and (not (equal? x LM))
                                                                       (not (equal? x RIGHT))
                                                                       (not (number? x)))) sigma)))]
              
                (cond [(empty? sigma) (format "the sigma ~s cannot be empty" sigma)]
                      [(equal? the-type 'dfa) (valid-sigma-dfa? sigma)]
                      [(or (equal? the-type 'ndfa)
                           (equal? the-type 'pda)) (valid-sigma-ndfa/pda? sigma)]
                      [(or (equal? the-type 'tm-language-recognizer)
                           (equal? the-type 'tm)) (valid-sigma-tm? sigma)])))
          
            ;start-in-list?: (listof rules) --> boolean/string
            ;purpose: see if the starting symbol is in the list
            (define (start-in-list? some-rules start-index)
              (cond [(ormap (lambda (x) (equal? s (start-index x))) some-rules) true]
                    [else (format "the starting state ~s is not in the delta ~s" s some-rules)]))
          
          
            ;create-initials: void --> listof strings
            ;purpose: compile a list of errors and leave out all true values
            (define (create-initials)
              (local [(define check1 (valid-start? s))
                      (define check2 (valid-finals? (sm-getfinals the-machine)))
                      (define check3 (valid-sigma? Σ))
                      (define check4 (valid-states? S))]
                (filter (lambda (x) (not (equal? x true))) (list check4 check3 check1 check2))))
          
            (define initials (create-initials))
          
          
            ;valid-n/dfa-rule?: (three-list) (listof symbols)--> boolean/string
            ;purpose: determines whether the list of symbols is a valid rule
            (define (valid-n/dfa-rule? a-rule sigma)
              (local [;create-error: (void) --> string/boolean
                      ;purpose: creates a string for an error
                      (define (create-error)
                        (local [;sigma-error?: void --> true/string
                                ;purpose: determine if the symbol in the rule is in the sigma
                                (define (sigma-error?)
                                  (cond [(not (set-member? sigma (second a-rule)))
                                         (format "the second item in the rule ~s must be in sigma" a-rule)]
                                        [else true]))
                              
                                ;in-set?: determines if the states in the rule are in the given set of states
                                (define (in-set? rule-part)
                                  (cond [(not (set-member? S rule-part))
                                         (format "the state ~s in the rule ~s must be in your list of states" rule-part a-rule)]
                                        [else true]))
                              
                                (define sigma-error (sigma-error?))
                                (define in-set1 (in-set? (first a-rule)))
                                (define in-set3 (in-set? (third a-rule)))
                                ]
                          (cond [(not (and (equal? true sigma-error)
                                           (equal? true in-set1)
                                           (equal? true in-set3)))
                                 (filter (lambda (x) (not (equal? true x))) (list sigma-error
                                                                                  in-set1
                                                                                  in-set3))]
                                [else true])))
                    
                      ]
                (cond [(not (equal? true (create-error))) (create-error)]
                      [else true])))
          
          
          
            ;valid-rules?: (listof rules) ((listof rules) --> boolean) --> boolean/string
            ;purpose: determine if all rules are valid
            (define (valid-rules? rules checker)
              (cond [(empty? rules) (format "the list of rules ~s cannot be empty" rules)]
                    [else (local [;check-rules: listof rules --> boolean/listof broken rules
                                  ;purpose: check the rules for validity and return true or a list of broken rules
                                  (define (check-rules new-rules)
                                    (cond [(empty? new-rules) empty]
                                          [(not (equal? (checker (first new-rules)) true))
                                           (append (checker (first new-rules)) (check-rules (rest new-rules)))]
                                          [else (check-rules (rest new-rules))]))
                                
                                  (define the-error (filter (lambda (x) (not (equal? true x))) (check-rules rules)))]
                            (cond [(not (empty? the-error)) the-error]
                                  [else true]))]))
          
          
            ;valid-delta: (listof rules)
            ;             ((listof rules) --> boolean)
            ;             ((listof rules) --> rule)
            ;             ((listof rules) --> boolean)
            ;                                           --> boolean
            ;purpose: determines if the given list of rules is a valid ndfa
            (define (valid-delta? rules rule-check repeat-check)
              (local [;check-ds: listof rules --> listof rules
                      ;purpose: filters out the rules with ds as their final state
                      (define (check-ds a-rule-list)
                        (cond [(or (equal? the-type 'dfa)
                                   (equal? the-type 'ndfa)) (filter (lambda (x) (not (equal? 'ds (third x)))) rules)]
                              [else a-rule-list]))
                    
                      ;checker1: listof rules --> boolean/broken rule list
                      ;purpose: use the rule-check to compile a list of invalid rules
                      (define (checker1 some-rules)
                        (rule-check some-rules))
                    
                      ;checker2: listof rules --> boolean/broken rule list
                      ;purpose: use the repeat checker to compile a list of repeated rules
                      (define (checker2 some-rules)
                        (local [(define repeat-list (repeat-check some-rules))]
                          (cond [(and (list? repeat-list)
                                      (string? (first repeat-list))) repeat-list]
                                [(not (equal? true repeat-list)) (format "the rules ~s are repeated and invalidate the delta" repeat-list)]
                                [else true])))
                    
                      (define the-rules (check-ds rules))
                    
                      (define a-list (list (checker1 the-rules)
                                           (checker2 the-rules)
                                           ))
                    
                      (define the-errors (filter (lambda (x) (not (equal? true x))) a-list))]
              
                (cond [(empty? the-errors) true]
                      [else the-errors])))
          
          
          
            ;valid-dfa?: machine --> boolean
            ;purpose: determine the validity of a dfa machine
            (define (valid-dfa? a-machine)
              (local [;no-dfa-repeats?: (listof (listof rules)) --> boolean
                      ;purpose: determine if there are doubles of the rule conditions in a set
                      ;where the second (listof rules) is the accumulator
                      (define (no-dfa-repeats? the-rules)
                        (local [;no-repeated?: (list of rules) --> boolean/string
                                ;purpose: check for repeats in the first two symbols
                                ;lorr is the accumulator accumulating seen rules  
                                ;accum is the accumulator accumulating errors
                                (define (no-repeated? lor lorr accum)
                                  (cond [(empty? lor) accum]
                                        [else (local [(define errors (any-repeats? (first lor) lorr))]
                                                (cond [(not (empty? errors)) (no-repeated? (rest lor) lorr (append errors accum))]
                                                      [else (no-repeated? (rest lor) (cons (first lor) lorr) accum)]))]))
                              
                                ;any-repeats?: rule (listof rules) --> boolean/listof strings
                                ;purpose: returns true if any rule matches any of the other rules
                                (define (any-repeats? x loR)
                                  (cond [(empty? loR) empty]
                                        [(and (equal? (first (first loR))
                                                      (first x))
                                              (equal? (second (first loR))
                                                      (second x))) (cons (format "Cannot have more than one rule starting with the state ~s and the symbol ~s"
                                                                                 (first x)
                                                                                 (second x)) (any-repeats? x (rest loR)))]
                                        [else (any-repeats? x (rest loR))]))
                              
                              
                                (define my-error (no-repeated? the-rules empty empty))]
                          (cond [(empty? my-error) true]
                                [else my-error])))
                    
                    
                      ;valid-dfa-rule?: (listof symbols) --> boolean/broken rule
                      ;purpose: determines whether the list of symbols is a valid dfa rule
                      (define (valid-dfa-rule? rule)
                        (valid-n/dfa-rule? rule Σ))
                    
                      ;valid-rules-dfa?: (listof rules) --> boolean/broken rule
                      ;purpose: determine if all rules are valid for a dfa
                      (define (valid-rules-dfa? rules)
                        (valid-rules? rules valid-dfa-rule?))
                    
                      ;valid-delta-dfa?: (listof rules) --> boolean/listof strings
                      ;purpose: determine if all the rules are valid for a dfda delta
                      (define (valid-delta-dfa? rules)
                        (valid-delta? rules valid-rules-dfa? no-dfa-repeats?))
                    
                      (define start-err (start-in-list? delta first))
                    
                      (define big-error (filter (lambda (x) (not (equal? true x))) (flatten (cons start-err (append initials (list (valid-delta-dfa? delta)))))))]
                (cond [(empty? big-error) true]
                      [else big-error])
              
                )
              )
          
            ;valid-ndfa-rule?: (listof symbols) --> boolean/broken rule
            ;purpose: determines whether the list of symbols is a valid ndfa rule
            (define (valid-ndfa-rule? rule)
              (valid-n/dfa-rule? rule (append (list BLANK EMP) Σ)))
          
          
            ;valid-ndfa?: machine --> boolean
            ;purpose: determine the validity of an ndfa machine
            (define (valid-ndfa? a-machine)
              (local [;valid-rules-ndfa?: (listof rules) --> boolean/ listof broken rules
                      ;purpose: determine if all rules are valid for a dfa
                      (define (valid-rules-ndfa? rules)
                        (valid-rules? rules valid-ndfa-rule?))
                    
                      ;valid-delta-ndfa?: listof rules --> boolean/listof strings
                      ;purpose: determine the validity of an ndfa delta
                      (define (valid-delta-ndfa? rules)
                        (valid-delta? rules valid-rules-ndfa? no-repeats?))
                    
                      (define start-err (start-in-list? delta first))
                    
                      (define big-error (filter (lambda (x) (not (equal? true x))) (cons start-err (cons (valid-delta-ndfa? delta) initials))))]
                (cond [(empty? big-error) true]
                      [else big-error])))
          
            ;valid-pda?: machine --> boolean
            ;purpose: determine the validity of a pda machine
            (define (valid-pda? a-machine)
              (local [(define Γ (sm-getstackalphabet a-machine))
                    
                      ;valid-pda-rule?: listof rules --> boolean/broken rule
                      ;purpose: determine the validity of a rule in a pda
                      (define (valid-pda-rule? base-rule)
                        (local [;internal-errors?: (list (three list) (two list)) --> listof strings/boolean
                                ;purpose: checks the internal input for a rule
                                (define (internal-errors? a-rule)
                                  (local [(define part1 (first a-rule))
                                          (define part2 (second a-rule))
                                          (define f (first part1))
                                          (define b (second part1))
                                          (define g (third part1))
                                          (define t (first part2))
                                          (define l (second part2))
                                        
                                          ;state-error: possible state --> boolean/string
                                          ;purpose: determine the validity of a state for the rule
                                          (define (state-error a-state)
                                            (cond [(equal? true (set-member? S a-state)) true]
                                                  [else (format "the state ~s in the rule ~s must be in the initial set of states" a-state a-rule)]))
                                        
                                          ;sigma-error: possible symbol --> boolean/string
                                          ;purpose: determine the validity of a symbol for the rule
                                          (define (sigma-error a-symbol)
                                            (cond [(or (equal? EMP a-symbol)
                                                       (equal? BLANK a-symbol)
                                                       (set-member? Σ a-symbol)) true]
                                                  [else (format "the symbol ~s is not in the given sigma" a-symbol)]))

                                          ;alphabet-error: listof symbols, symbol, symbol --> boolean/string
                                          ;purpose: determine if the alphabet for a part of the rule meets its criteria                                         
                                          (define (alphabet-error the-list rule-part part-part)
                                            (cond [(equal? the-list EMP) true]
                                                  [(not (list? the-list)) (format "the ~s item of the ~s item in the rule ~s must be a list or EMP" part-part rule-part a-rule)]
                                                  [else (local [;invalid-gammas: listof symbols --> listof symbols
                                                                ;pupose: to compile a list of invalid symbols in the rule's gamma
                                                                (define (invalid-gammas a-list)
                                                                  (cond [(empty? a-list) empty]
                                                                        [(set-member? Γ (first a-list)) (invalid-gammas (rest a-list))]
                                                                        [else (cons (first a-list) (invalid-gammas (rest a-list)))]))
                                                              
                                                                (define bad-gammas (invalid-gammas the-list))]
                                                        
                                                          (cond [(empty? bad-gammas) true]
                                                                [else (format "the symbols ~s in the rule ~s must be in the gamma" bad-gammas a-rule)]))]))

                                          
                                          ;g-error: listof symbols --> boolean/string
                                          ;purpose: check the first list of symbols from the stack alphabet for errors
                                          (define (g-error given-g)
                                            (alphabet-error given-g 'third 'first))
                                        
                                          ;l-error: listof symbols --> boolean/string
                                          ;purpose: check the second list of symbols from the stack alphabet for errors
                                          (define (l-error given-l)
                                            (alphabet-error given-l 'second 'second))
                                        
                                          (define err-list (list (state-error f)
                                                                 (state-error t)
                                                                 (sigma-error b)
                                                                 (g-error g)
                                                                 (l-error l)
                                                                 ))
                                        
                                          (define the-err-list (filter (lambda (x) (not (equal? true x))) err-list))]
                                  
                                    (cond [(empty? the-err-list) true]
                                          [else the-err-list])))
                              
                                ;create-error: (listof (listof symbols)) --> listof strings or true
                                ;pupose: filter the error lists down to just strings
                                (define (create-error RULE)
                                  (local [(define err? (internal-errors? RULE))]
                                    (cond [(not (equal? true err?)) (filter (lambda (x) (not (equal? true x))) err?)]
                                          [else true])))
                              
                               
                                (define internal-errors (create-error base-rule))
                              
                                ]
                          internal-errors)
                          )
                    
                      ;valid-rules-pda?: (listof rules) --> boolean/listof broken rules
                      ;purpose: determine if all rules are valid for a pda
                      (define (valid-rules-pda? rules)
                        (valid-rules? rules valid-pda-rule?))
                    
                      ;valid-delta-pda?: (listof rules) --> boolean/string
                      ;purpose: determine the validity of the given pda delta
                      (define (valid-delta-pda? rules)
                        (local [(define good-delta? (valid-delta? rules valid-rules-pda? no-repeats?))]
                          (cond [(equal? true good-delta?) (start-in-list? rules caar)]
                                [else good-delta?])))
                    
                      ]
                (append initials (valid-delta-pda? delta))
                )
              )
          
            ;JO DONT FORGET TO ADD START-ERR
            ;valid-tm?: machine --> boolean
            ;purpose: determine the validity of a tm
            (define (valid-tm? a-machine)
              (local [;valid-tm-rule?: (listof (two-lists of a three list and a two list)) --> boolean/listof strings
                      ;purpose: to determine if the given rule is valid for a tm
                      (define (valid-tm-rule? rule)
                        (local [(define is-empty (cond [(empty? rule) (format "the given rule ~s cannot be empty" rule)]
                                                       [else true]))
                                ]
                          (cond [else (local [(define alphabet1 (cond
                                                                  [(and (not (equal? (second (first rule)) BLANK))
                                                                        (not (set-member? (cons LM Σ) (second (first rule))))) (format "the second item of the first item in the rule ~s must be either BLANK or a symbol in the alphabet" rule)]
                                                                  [else true]))
                                              (define alphabet2 (cond
                                                                  [(and (not (equal? (second (second rule)) BLANK))
                                                                        (not (set-member? (append (list LEFT RIGHT) Σ) (second (second rule)))) (format "the second item of the second item in the rule ~s must be either BLANK or a symbol in the alphabet" rule))]
                                                                  [else true]))
                                              (define state-check (andmap (lambda (x) (let ((t (not (set-member? S x))))
                                                                                        (if t (format "the state ~s in the rule ~s must be in the given list of states for the machine" x rule)
                                                                                            true))) (list (first (first rule)) (first (second rule)))))
                                              (define other-errors (filter (lambda (x) (not (equal? x true))) (list alphabet1 alphabet2 state-check)))]
                                        (cond [(empty? other-errors) true]
                                              [else other-errors]))])))
                    
                      ;valid-rules-tm?: (listof (rules)) --> boolean
                      ;purpose: determine if all rules are valid for a tm
                      (define (valid-rules-tm? rules)
                        (valid-rules? rules valid-tm-rule?))
                    
                      ;valid-delta-tm?: (listof (listof rules)) --> boolean
                      ;purpose: determine if a delta is valid for a tm
                      (define (valid-delta-tm? rules)
                        (local [(define good-delta? (valid-delta? rules valid-rules-tm? no-repeats?))]
                          (cond [(equal? true good-delta?) (start-in-list? rules caar)]
                                [else good-delta?])))
                    
                      (define rule-errors (valid-delta-tm? delta))
                    
                      ]
                ;DO THE CONDITIONAL
                (cond [(and (empty? initials) (empty? rule-errors)) true]
                      [(empty? initials) rule-errors]
                      [(empty? rule-errors) initials]
                      [else (list initials rule-errors)])))
          
            ]
      (cond [(equal? the-type 'dfa) (format-error (valid-dfa? the-machine))]
            [(equal? the-type 'ndfa) (format-error (valid-ndfa? the-machine))]
            [(equal? the-type 'pda) (format-error (valid-pda? the-machine))]
            [(or (equal? the-type 'tm-language-recognizer)
                 (equal? the-type 'tm)) (format-error (valid-tm? the-machine))]
            [else (error "why")])
      )
    )
  )

