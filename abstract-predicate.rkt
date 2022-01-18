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
                                   (format "The given starting ~s is not a symbol: ~s" (if (equal? upper-name "list of states")
                                                                                           'state
                                                                                           'nonterminal)
                                           start)))
               
            ;we need to check and make sure that the delta is in the right shape
            ; if it isnt, we cant actually check it for anything else
            (define delta-bool
              ;if the rules are empty, give an error
              ;(if (empty? delta) (format "The given list of rules is empty ~s: " delta)
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
                      (format "The delta ~s is not a list" delta)));)
            ]
      (list upper-bool sigma-bool start-bool delta-bool)
      ))

  ;; get-repeats: list-of-strings -> list
  ;; Purpose: Given list-of-strings will return a new list that contians all the elements in the origional list that have repeats
  (define (get-repeats a-list)
    (local [;; find-repeats: list-of-strings list -> list
            ;; Purpose: Given list-of-strings will return a new list that contians all the elements in the origional list that have repeats
            (define (find-repeats los accum)
              (cond
                [(empty? los) (remove-duplicates accum)]
                [(member (car los) (cdr los)) (find-repeats (cdr los) (cons (car los) accum))]
                [else (find-repeats (cdr los) accum)]))]
      (find-repeats a-list '())))

  ;check-nondependent (listof something) (listof something) --> string
  ;purpose: to check the states and sigma for validity
  (define (check-nondependent upper-list sigma lower-name upper-name delta)
    (local [ ;wrong-case: (listof something) (x --> boolean) --> (listof something)
            ;purpose: to accumulate all the non-symbols in the list
            (define (wrong-case a-list pred)
              (filter (lambda (x) (and (not (pred x)) (not (eq? x DEAD)))) a-list))

            ;wrong-type: (listof something) --> (listof not symbols)
            ;purpose: to accumulte all the non-symbols in the list
            (define (wrong-type a-list)
              (filter (lambda (x) (not (or (number? x) (symbol? x)))) a-list))

            ;check-list: (listof something) (x --> boolean) string --> string
            ;purpose: to create the errors for the list
            (define (check-list a-list pred name single?)
              (local [;collect the repeats
                      (define repeating (get-repeats a-list))
                      ;to collect the items in the wrong case
                      (define case
                        (wrong-case a-list
                                    (lambda (x)
                                      (if (symbol? x)
                                          (and (or (char-alphabetic? (string-ref (symbol->string x) 0))
                                                   (number? (string-ref (symbol->string x) 0)))
                                               (or (pred (string-ref (symbol->string x) 0))
                                                   (equal? (substring (symbol->string x) 0 2) "ds")
                                                   (and (>= (string-length (symbol->string x)) 3)
                                                        (equal? (substring (symbol->string x) 0 2) "ds")
                                                        (equal? (substring (symbol->string x) 2 3) "-")))
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
              (local [(define errors (check-list (filter (lambda (s) (not (eq? s LM))) sigma)
                                                 (lambda (a) (or (char-lower-case? a) (number? a)))
                                                 (if (equal? upper-name "NONTERMINAL")
                                                     "terminals"
                                                     (if (equal? upper-name "")
                                                         "gamma"
                                                         "alphabet"))
                                                 #t))]
                (if (string=? errors "") ""
                    (string-append (format "\n ALPHABET ERRORS for: ~s" sigma) errors))                 
                )) 
            

            ;;;;;;;;check for repeats in delta and final
            (define delta-repeats (get-repeats delta))
            ]
      ;append the errors
      (string-append (string-append (local [(define upper-errors (check-upper))
                                            (define sigma-errors (check-sigma))]
                                      (if (string=? upper-errors "") (if (string=? sigma-errors "")
                                                                         "" sigma-errors)
                                          (if (string=? sigma-errors "") upper-errors
                                              (string-append upper-errors "\n" sigma-errors)))))
                     (if (empty? delta-repeats) ""
                         (string-append "\n \n These rules are repeated in your list of rules: \n"
                                        (foldr (lambda (x y) (string-append (format "~s \n" x) y)) "" delta-repeats)))
                     )
      ))

  ;;checks the nondependent aspects that are specific to machines
  (define (check-nondependent-m delta finals type)
    (local [;; get-repeats: list-of-strings -> list
            ;; Purpose: Given list-of-strings will return a new list that contians all the elements in the origional list that have repeats
            (define (get-repeats a-list)
              (local [;; find-repeats: list-of-strings list -> list
                      ;; Purpose: Given list-of-strings will return a new list that contians all the elements in the origional list that have repeats
                      (define (find-repeats los accum)
                        (cond
                          [(empty? los) (remove-duplicates accum)]
                          [(member (car los) (cdr los)) (find-repeats (cdr los) (cons (car los) accum))]
                          [else (find-repeats (cdr los) accum)]))]
                (find-repeats a-list '())))
            
            (define finals-repeats (get-repeats finals)) 

            ;;removes the rules that would incur nondeterminism from the dfa
            (define (repeat-dfa a-list)
              (local [(define (inner los accum f-accum)
                        (cond [(empty? los) f-accum]
                              [(member (cons (car (car los)) (list (cadr (car los)))) accum)
                               (inner (cdr los) accum (cons (cons (car (car los)) (cadr (car los))) f-accum))]
                              [else (inner (cdr los)
                                           (cons (list (car (car los)) (cadr (car los))) accum)
                                           f-accum)]))]
                (inner a-list empty empty)))
            ]
      (string-append (if (empty? finals-repeats) ""
                         (format "\n These states are repeated in your list of final states \n ~s" finals-repeats))
                     (if (not (equal? type 'dfa))
                         ""
                         (local [(define dfa-repeats (repeat-dfa delta))]
                           (if (empty? dfa-repeats) ""
                               (string-append "\n More than one of your rules begin with: \n"
                                              (foldr (lambda (x y) (string-append (format "~s \n" x) y)) "" dfa-repeats))))))))

  ;; check-dependent: states alpha start delta type (type?...) (f?)
  (define (check-dependent v a s d t name . f)
    (local [
            (define (remove-repeats a-list)
              (cond [(empty? a-list) empty]
                    [(member (car a-list) (cdr a-list)) (remove-repeats (cdr a-list))]
                    [else (cons (car a-list) (remove-repeats (cdr a-list)))]))
            (define start (member s v))
            (define rules (filter (lambda (y)
                                    (not (member y
                                                 (append
                                                  (list EMP ARROW BLANK RIGHT LEFT GOTO DEAD LM BRANCH VAR START)
                                                  v
                                                  a))))
                                  (filter (lambda (x) (if (symbol? x)
                                                             (= (string-length (symbol->string x)) 1)
                                                             (<= 0 x 9)))
                                          (remove-repeats (flatten d)))))

            (define start-message (if start (begin
                                              ;(newline)
                                              ;(display (format "Start ~s looks good!" name))
                                              "")
                                      (format "Starting ~a ~s is not in the list of states ~s" name s v))) 
            (define rule-message (if (empty? rules)
                                     (begin
                                       ;(newline)
                                       ;(display "Rules contain viable symbols!")
                                       "")
                                     (case t
                                       [(pda) (format "The following symbols are not defined in your list of ~a, alphabet, or gamma: ~s.\n" name rules)]
                                       [else
                                        (format "The following symbols are not defined in your list of ~a or alphabet: ~s.\n" name rules)])))

            (define (final-check)
              (if (null? f) ""
                  (local [(define f-list (filter (lambda (x)
                                                   (not (member x v))) (car f)))]
                    (if (empty? f-list)
                        (begin
                          ;(newline)
                          ;(display "Final states look good!")
                          "")
                        (format "The final states ~s are not contained in the list of states ~s"
                                f-list v)))))

            (define final-message (final-check))

            (define end-message
              (string-append
               (if (string=? start-message "") ""
                   (string-append (format "\n-STARTING ~a CONTENT ERROR: \n" name) start-message))
               (if (string=? final-message "") ""
                   (string-append (format "\n-FINAL STATES CONTENT ERRORS: \n") final-message))
               (if (string=? rule-message "") ""
                   (string-append (format "\n-RULE CONTENT ERRORS: \n") rule-message))))]
                                        
      end-message))

  (define (los->string l)
  (cond [(empty? l) ""]
        [else (string-append (symbol->string (car l)) (los->string (cdr l)))]))

  ;; check-machine states alphabet final-states rules start-state machine-type [stack-alpha (optional)] -> True or Void
  ;;    Returns true if the machine passes. Otherwise returns void if the machine fails.
  (define (check-machine states sigma finals delta start type . gamma)
    (local [
            ;;something for gamma
            (define (check-gamma g)
              (let [(problems (filter (lambda (s) (not (symbol? s))) g))]
                (if (null? problems)
                    ""
                    (format "The following are not valid for gamma: ~s" problems))))
              
          
            ;check-delt: no input --> string or (list of broken rules)
            ;purpose: to determine if the delta is of the correct lengths
            (define (delta-check-m)
              (local [;check-lengths: (listof something) num num --> string
                      ;purpose: to check if every rule in the list is correctly formatted
                      (define (check-lengths del islist how-many how-long)
                        ;so we begin to accumulate rules that look wrong
                        (local [(define (inner delt accum)
                                  ;(displayln delt)
                                  (local [ ;check-length: something --> boolean
                                          ;purpose: checks an individual rule for if it looks good
                                          (define (check-length rule how-many how-long)
                                            ;if everything is at base case, all parts of rule were fine
                                            ;(displayln delt)
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
               ;(newline)
               ;display things are good with lenghts and 
               ;(display "Structure of the machine looks good!")
               ;(newline)
               ;check for nondependent errors
               (local [(define non-dep-errors (if (null? gamma)
                                                  (string-append (check-nondependent states
                                                                                     sigma
                                                                                     "list of states" 
                                                                                     "STATE"
                                                                                     delta) 
                                                                 (check-nondependent-m delta
                                                                                       finals
                                                                                       type))
                                                  (string-append (check-nondependent states
                                                                                     sigma
                                                                                     "list of states" 
                                                                                     "STATE"
                                                                                     delta) 
                                                                 (check-nondependent-m delta
                                                                                       finals
                                                                                       type)
                                                                 (check-gamma (car gamma)))))]  
                 ;(check-nondependent '(A) (car gamma) "" ""))))]
                 ;if there are nondependent errors, keep looking, return them
                 (cond [(not (string=? non-dep-errors "")) (display non-dep-errors)]
                       ;otherwise, return that the state and sigma look good and keep checking
                       [else (begin ;(newline)
                               ;(display "Nondependent components look good!")
                               (local [(define dep-errors (check-dependent states
                                                                           (if(eq? type 'pda) (append sigma (car gamma))
                                                                              sigma)
                                                                           ;;sigma ;; if pda append sigma + gamma else sigma
                                                                           start
                                                                           delta
                                                                           type
                                                                           "states"
                                                                           finals))]
                                 (begin (display dep-errors)
                                        (local [;rule-errors
                                                (define rule-errors
                                                  (cond [(equal? type 'dfa) (check-dfarule states sigma delta)]
                                                        [(equal? type 'ndfa) (check-ndfarule states sigma delta)]
                                                        [(equal? type 'pda) (check-pda-rules states sigma (car gamma) delta)]
                                                        [(equal? type 'tm) (check-tmrules states sigma delta)]
                                                        [else (error "Machine type not implemented")]))
                                                ]
                                          (if (and (equal? dep-errors "")
                                                   (equal? rule-errors "")) (begin ;(newline)
                                                                              ;(display "Rules look good!")
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
               ;(newline)
               ;display things are good with lenghts and 
               ;(display "Structure of the grammar looks good!")
               ;(newline)
               ;check for nondependent errors
               (local [(define non-dep-errors  (check-nondependent nts
                                                                   sigma
                                                                   "list of nonterminals" 
                                                                   "NONTERMINAL"
                                                                   delta))]
                 ;if there are nondependent errors, keep looking, return them
                 (cond [(not (string=? non-dep-errors "")) (display non-dep-errors)]
                       ;otherwise, return that the state and sigma look good and keep checking
                       [else (begin ;(newline)
                               ;(display "Nondependent components look good!")
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
                                                                                          (lambda (x) (not (and (equal? (car x) start)
                                                                                                           (equal? (cadr x) ARROW)
                                                                                                           (equal? (caddr x) EMP))))
                                                                                          delta))]
                                                        [(equal? type 'cfg) (check-cfgrule nts sigma delta)]
                                                        [(equal? type 'csg) (check-csgrule nts sigma delta)]
                                                        [else (error "Grammar type not implemented")]))
                                                ]
                                          (if (and (equal? dep-errors "")
                                                   (equal? rule-errors "")) (begin ;(newline)
                                                                              ;(display "Rules look good!")
                                                                              #t)
                                                                            (display rule-errors))
                                          ))))])))])))
  )
