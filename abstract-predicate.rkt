(module abstract-predicate racket
  (require "rule-pred.rkt" "constants.rkt"
           "sm-getters.rkt" "fsa.rkt" "tm.rkt" "pda.rkt"
           "grammar-getters.rkt"
           ) 
  (require test-engine/racket-tests)
  (provide check-machine check-grammar) 

  ;check-types: three different things that need to be checked and can be anything and a name --> listof strings
  ;purpose: to see if all lengths are correct
  (define (check-length upper-list
                        upper-name
                      
                        sigma 
                        start

                        delta
                        type
                        check-delta)
    (local [(define upper-bool (if (list? upper-list) ""
                                   (format "The given list of ~s is not a list: ~s" upper-name upper-list)))
            (define sigma-bool (if (list? sigma) ""
                                   (format "The given alphabet is not a list: ~s" sigma)))          
            (define start-bool (if (symbol? start) ""
                                   (format "The given starting nonterminal is not a symbol: ~s" start)))
               
            ;we need to check and make sure that the delta is in the right shape
            ; if it isnt, we cant actually check it for anything else
            (define delta-bool
              ;if the rules are empty, give an error
              (if (empty? delta) (format "The given list of rules is empty ~s: " delta)
                  (if (list? delta)
                      ;otherwise check the rules for correct lengths
                      (local [(define invalid-rules (check-delta))]
                        ;if we returned a string, than we have an error
                        (if (empty? invalid-rules) ""
                            (string-append "  The following rules are not rules of three symbols: \n "
                                           (foldr string-append "" (map (lambda (x)
                                                                          (string-append "\t"
                                                                                         (format "~s " x)
                                                                                         "\n"))
                                                                        invalid-rules)))))
                      (format "The delta ~s is not a list" delta))))
            ]
      (list upper-bool sigma-bool start-bool delta-bool)
      ))

  ;check-nondependent (listof something) (listof something) --> string
  ;purpose: to check the states and sigma for validity
  (define (check-nondependent upper-list sigma lower-name upper-name)
    (local [;repeats: (listof something) --> (listof something)
            (define (repeats alist)
              (local [;inner: (listof something) (listof something) --> (listof sometihng)
                      ;purpose: to accumulate repeats
                      (define (inner a-list accum)
                        (cond [(empty? a-list) accum]
                              [(member (car a-list) (cdr a-list)) (inner (cdr a-list)
                                                                         (cons (car a-list) accum))]
                              [else (inner (cdr a-list) accum)]))]
                (inner alist '())))

            ;wrong-case: (listof something) (x --> boolean) --> (listof something)
            ;purpose: to accumulate all the non-symbols in the list
            (define (wrong-case a-list pred)
              (filter (lambda (x) (not (pred x))) a-list))

            ;wrong-type: (listof something) --> (listof not symbols)
            ;purpose: to accumulte all the non-symbols in the list
            (define (wrong-type a-list)
              (filter (lambda (x) (not (symbol? x))) a-list))

            ;check-list: (listof something) (x --> boolean) string --> string
            ;purpose: to create the errors for the list
            (define (check-list a-list pred name single?)
              (local [;collect the repeats
                      (define repeating (repeats a-list))
                      ;to collect the items in the wrong case
                      (define case
                        (wrong-case a-list
                                    (lambda (x)
                                      (if (symbol? x)
                                          (and (char-alphabetic? (string-ref (symbol->string x) 0))
                                               (pred (string-ref (symbol->string x) 0))
                                               (if single? (= (string-length (symbol->string x)) 1)
                                                   #t))
                                          #t))))
                      ;to collect the items that are not symbols
                      (define typing (wrong-type a-list))]
              
                ;compile the error
                (string-append (if (empty? typing) "" 
                                   (string-append "\n    These are invalid symbols in your "
                                                  name
                                                  (format ": ~s" typing)))
                               (if (empty? case) ""
                                   (if (string=? name "alphabet")
                                       (format "\n    These are non-alphabetic or uppercase in your ~s ~s" name case)
                                       (format "\n    These are non-alphabetic or lowercase in your ~s ~s" name case)))
                               (if (empty? repeating) ""
                                   (string-append "\n    These are repeated in your "
                                                  name
                                                  (format ": ~s" repeating))) 
                               ))
              )                       

            ;check-uper: no input --> string
            ;the abstract function for checking a list that is a list of states
            (define (check-upper)
              (local [(define errors (check-list upper-list
                                                 char-upper-case? 
                                                 lower-name
                                                 #f))]
                (if (string=? errors "") ""
                    (string-append (format "\n ~s ERRORS for: ~s" upper-name upper-list) errors))                 
                ))
          
            ;check-sigma no input --> string
            ;the abstract function for checking a list that is a list of sigma
            (define (check-sigma)
              (local [(define errors (check-list sigma
                                                 char-lower-case? 
                                                 (if (equal? upper-name "NONTERMINAL")
                                                     "terminals"
                                                     (if (equal? upper-name "")
                                                         "gamma"
                                                         "alphabet"))
                                                 #t))]
                (if (string=? errors "") ""
                    (string-append (format "\n ALPHABET ERRORS for: ~s" sigma) errors))                 
                ))
            ]
      ;append the errors
      (string-append (local [(define upper-errors (check-upper))
                             (define sigma-errors (check-sigma))]
                       (if (string=? upper-errors "") (if (string=? sigma-errors "")
                                                          "" sigma-errors)
                           (if (string=? sigma-errors "") upper-errors
                               (string-append upper-errors "\n" sigma-errors)))))
      )
    )

  (define (check-dependent v a s d t name . f)
    (local [(define (remove-repeats a-list)
              (if (not (equal? t 'dfa)) (repeat-rule a-list)
                  (repeat-rule (map (lambda (x) (cons (car x) (cadr x))) a-list))))

            (define (repeat-rule a-list)
              (cond [(empty? a-list) empty]
                    [(member (car a-list) (cdr a-list)) (repeat-rule (cdr a-list))]
                    [else (cons (car a-list) (repeat-rule (cdr a-list)))])) 

            (define start (member s v))
            (define rules (filter (lambda (y)
                                    (not (member y
                                                 (append
                                                  (list EMP ARROW BLANK RIGHT LEFT GOTO DEAD LM BRANCH VAR START)
                                                  v
                                                  a))))
                                  (filter (lambda (x) (= (string-length (symbol->string x)) 1))
                                          (remove-repeats (flatten d)))))

            (define start-message (if start (begin
                                              (newline)
                                              (display (format "Start ~s looks good!" name))
                                              "")
                                      (format "Starting ~s ~s is not in the list of states ~s" name s v))) 
            (define rule-message (if (empty? rules)
                                     (begin
                                       (newline)
                                       (display "Rules contain viable symbols!")
                                       "")
                                     (format "The following symbols in your rule list are not defined in your construct: ~s" rules)))

            (define (final-check)
              (if (null? f) ""
                  (local [(define f-list (filter (lambda (x)
                                                   (not (member x v))) (car f)))]
                    (if (empty? f-list)
                        (begin
                          (newline)
                          (display "Final states look good!")
                          "")
                        (format "The final states ~s are not contained in the list of states ~s"
                                f-list v)))))

            (define final-message (final-check))

            (define end-message
              (string-append
               (if (string=? start-message "") ""
                   (string-append (format "\n STARTING ~s CONTENT ERROR: \n" name) start-message))
               (if (string=? final-message "") ""
                   (string-append (format "\n FINAL STATES CONTENT ERRORS: \n") final-message))
               (if (string=? rule-message "") ""
                   (string-append (format "\n RULE CONTENT ERRORS: \n") rule-message))))]
                                        
      end-message))

  (define (check-machine states sigma finals delta start type . gamma)
    (local [
            ;;something for gamma
          
            ;check-delt: no input --> string or (list of broken rules)
            ;purpose: to determine if the delta is of the correct lengths
            (define (delta-check-m)
              (local [;check-lengths: (listof something) num num --> string
                      ;purpose: to check if every rule in the list is correctly formatted
                      (define (check-lengths del islist how-many how-long)
                        ;so we begin to accumulate rules that look wrong
                        (local [(define (inner delt accum)
                                  (local [ ;check-length: something --> boolean
                                          ;purpose: checks an individual rule for if it looks good
                                          (define (check-length rule how-many how-long)
                                            ;if everything is at base case, all parts of rule were fine
                                            (cond [(and (empty? rule)
                                                        (zero? how-many)) #t]
                                                  ;but if only one is at base case, than its broken
                                                  [(or (zero? how-many)
                                                       (empty? rule)) #f]
                                                  [(not (list? (car rule))) #f]
                                                  ;if the length of the rule is good, we can keep checking the rules
                                                  [(= (length (car rule))
                                                      (car how-long)) (check-length (cdr rule)
                                                                                    (sub1 how-many)
                                                                                    (cdr how-long))]
                                                  [else #f]))]
                                    (cond [(empty? delt) accum] ;if we've reached the end, return accum
                                          ;otherwise check the first rule
                                          ;if the rule is a list of lists we check all the bits
                                          [islist (if (check-length (car delt)
                                                                    how-many
                                                                    how-long)
                                                      ;if the rule was okay, do nothing
                                                      (inner (cdr delt)
                                                             accum)
                                                      ;otherwise add the rule to the list of bad rules
                                                      (inner (cdr delt)
                                                             (cons (car delt) accum)))]
                                          ;otherwise we just make sure the right amount of stuff is in the list
                                          [else (if (= (length (car delt)) how-many)
                                                    (inner (cdr delt)
                                                           accum)
                                                    (inner (cdr delt)
                                                           (cons (car delt) accum)))])))]
                          (inner del '()))) 
                      ]
                (if (list? delta)
                    ;if the rules are a list, then we can see if they are rules of
                    ; correct format
                    (cond [(symbol=? type 'dfa) (check-lengths delta #f 3 empty)]
                          [(symbol=? type 'ndfa) (check-lengths delta #f 3 empty)]
                          [(symbol=? type 'pda) (check-lengths delta #t 2 (list 3 2))]
                          [(symbol=? type 'tm) (check-lengths delta #t 2 (list 2 2))]
                          [(symbol=? type 'ctm) (error "not yet implemented")])
                    ;otherwise, we say that the rules must be a list
                    (format "The given list of rules is not a list: ~s" delta))))

            ;list returned is
            ; state errors
            ; sigma errors
            ; start errors
            ; delta errors
            (define length-errors (check-length states
                                                "list of states"
                                              
                                                sigma 
                                                start
                                              
                                                delta
                                                type
                                                delta-check-m))
            (define final-bool (if (list? finals) ""
                                   (format "The given list of final states is not a list: ~s" finals)))
            (define big-length-error
              ;append all the errors together
              (string-append (if (string=? (first length-errors) "") ""
                                 (string-append "\n" (first length-errors)))
                             (if (string=? (second length-errors) "") ""
                                 (string-append "\n" (second length-errors)))
                             (if (string=? (third length-errors) "") ""
                                 (string-append "\n" (third length-errors)))
                             (if (string=? final-bool "") ""
                                 (string-append "\n" final-bool))
                             (if (string=? (fourth length-errors) "") ""
                                 (string-append "\nErrors for given rule list: \n" (fourth length-errors)))))
            ]
      (cond [(not (string=? big-length-error "")) (display big-length-error)]
            ;otherwise
            [else
             (begin
               (newline)
               ;display things are good with lenghts and 
               (display "Structure of the machine looks good!")
               (newline)
               ;check for nondependent errors
               (local [(define non-dep-errors (string-append (check-nondependent states
                                                                                 sigma
                                                                                 "list of states" 
                                                                                 "STATE")
                                                             
                                                             "\n"
                                                             (check-nondependent '(A) gamma "" "")))]
                 ;if there are nondependent errors, keep looking, return them
                 (cond [(not (string=? non-dep-errors "")) (display non-dep-errors)]
                       ;otherwise, return that the state and sigma look good and keep checking
                       [else (begin (newline)
                                    (display "List of States and Sigma look good")
                                    (local [(define dep-errors (check-dependent states
                                                                                sigma
                                                                                start
                                                                                delta
                                                                                type
                                                                                "state"
                                                                                finals))]
                                      (begin (display dep-errors)
                                             (local [;rule-errors
                                                     (define rule-errors
                                                       (cond [(equal? type 'dfa) (check-dfarule states sigma delta)]
                                                             [(equal? type 'ndfa) (check-ndfarule states sigma delta)]
                                                             [(equal? type 'pda) (check-pdarule states sigma delta)]
                                                             [(equal? type 'tm) (check-tmrule states sigma delta)]
                                                             [else (error "Machine type not implemented")]))
                                                     ]
                                               (if (and (equal? dep-errors "")
                                                        (equal? rule-errors "")) (begin (newline)
                                                                                        (display "Rules look good!")
                                                                                        #t)
                                                                                 (display rule-errors)))
                                             )))])))])))

  ;; nts: none terminals
  (define (check-grammar nts sigma delta start type)
    (local [
            ;;remove errors about -->
            ;; is it composed of elements from the nts and sigma
          
            (define (delta-check-g)
              (filter (lambda (x) (or (not (list? x))
                                      (not (= (length x) 3)))) delta))
           
            ;list returned is
            ; nts errors
            ; sigma errors
            ; start errors
            ; delta errors
            (define length-errors (check-length nts
                                                "list of nonterminals"
                                              
                                                sigma 
                                                start

                                                delta 
                                                type
                                                delta-check-g))
            (define big-length-error
              ;append all the errors together
              (string-append (if (string=? (first length-errors) "") ""
                                 (string-append "\n" (first length-errors)))
                             (if (string=? (second length-errors) "") ""
                                 (string-append "\n" (second length-errors)))
                             (if (string=? (fourth length-errors) "") ""
                                 (string-append "\nErrors for given rule list: \n" (fourth length-errors)))
                             (if (string=? (third length-errors) "") ""
                                 (string-append "\n" (third length-errors)))))
            ]
      ;If there are type errors, return them
      (cond [(not (string=? big-length-error "")) (display big-length-error)]
            ;otherwise
            [else
             (begin
               (newline)
               ;display things are good with lenghts and 
               (display "Structure of the grammar looks good!")
               (newline)
               ;check for nondependent errors
               (local [(define non-dep-errors  (check-nondependent nts
                                                                   sigma
                                                                   "list of nonterminals" 
                                                                   "NONTERMINAL"))]
                 ;if there are nondependent errors, keep looking, return them
                 (cond [(not (string=? non-dep-errors "")) (display non-dep-errors)]
                       ;otherwise, return that the state and sigma look good and keep checking
                       [else (begin (newline)
                                    (display "List of nonterminals and Sigma look good")
                                    (local [(define dep-errors (check-dependent nts
                                                                                sigma
                                                                                start
                                                                                delta
                                                                                type
                                                                                "nonterminal"))]
                                      (begin (display dep-errors)
                                             (local [;rule-errors
                                                     (define rule-errors
                                                       (cond [(equal? type 'rg) (check-rgrule nts sigma
                                                                                              (filter
                                                                                               (lambda (x) (and (equal? (car x) start)
                                                                                                                (equal? (cadr x) ARROW)
                                                                                                                (equal? (caddr x) EMP)))
                                                                                               delta))]
                                                             [(equal? type 'cfg) (check-cfgrule nts sigma delta)]
                                                             [(equal? type 'csg) (check-csgrule nts sigma delta)]
                                                             [else (error "Grammar type not implemented")]))
                                                     ]
                                               (if (and (equal? dep-errors "")
                                                        (equal? rule-errors "")) (begin (newline)
                                                                                        (display "Rules look good!")
                                                                                        #t)
                                                                                 (display rule-errors))
                                               ))))])))])))

  (check-expect (check-machine '(A B C)
                               '(a b c)
                               '(B C)
                               (list '(A b C)
                                     '(A c B)
                                     '(B a C))
                               'A
                               'dfa)
                #t)
  (test)

  )