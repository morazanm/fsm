(module temp-rulepreds racket
  (require "constants.rkt")
  (require test-engine/racket-tests)
  (provide  check-rgrule check-cfgrule check-csgrule
            check-dfarule check-ndfarule check-pda-rules check-tmrules
            ;check-pdarule check-tmrule

            ;; experted for testing
            allBoth oneAndOne allOne hasOne
            
            )

  ;sub-member: (listof symbols) string num num --> boolean
  ;purpose: to check if a string is present in a list in symbolform
  (define (sub-member los str low high)
    (if (member (string->symbol (substring str low high)) los) #t #f))

  ;purpose: to check that the symbol given is composed
  ;         entirely of the first and second given list
  (define (allBoth sym list1 list2)
    (local [(define combined (append list1 list2))
            (define str (symbol->string sym))
            ;loop: number --> boolean
            ;purpose: to iterate through the word
            (define (loop end)
              (cond [(zero? end) #t]
                    [(sub-member combined str (sub1 end) end) (loop (sub1 end))]
                    [else #f]))
            ]
      (if (< (string-length str) 1) #f
          (loop (string-length str)))))

  ;purpose: to check that the symbol given is composed
  ;         of one symbol from the second list,
  ;         possibly followed by one symbol from the first list
  (define (oneAndOne sym list1 list2)
    (local [(define str (symbol->string sym))]
      (cond [(or (> (string-length str) 2)
                 (< (string-length str) 1)) #f]
            [else (and (sub-member list2 str 0 1)
                       (if (> (string-length str) 1)
                           (sub-member list1 str 1 2)
                           #t))])))

  ;purpose: to check if the symbol is composed entirely of
  ;         symbols from the list
  (define (allOne sym list1)
    (allBoth sym list1 '()))

  ;purpose: to ensure that at least one symbol from a list is contained
  (define (hasOne sym list1)
    (local [(define str (symbol->string sym))
            ;loop: number --> boolean
            ;purpose: to iterate through the word
            (define (loop end)
              (cond [(zero? end) #f]
                    [(sub-member list1 str (sub1 end) end) #t]
                    [else (loop (sub1 end))]))]
      (loop (string-length str))))

  ;purpose: to check any three-part rule
  (define (checkThrupples LHpred Mpred RHpred rules)
    (local [(define (theFold a-list)
              (foldl (lambda (x y) (string-append y (format "\n\t ~s" x))) "" a-list))
            (define LHerrors (filter (lambda (x) (not (LHpred (car x)))) rules))]
      (cond [(not (empty? LHerrors))
             (string-append "\n THE LHS OF THE FOLLOWING RULES ARE NOT VALID: "
                            (theFold LHerrors))]
            [else (local [(define Merrors (filter (lambda (x) (not (Mpred (cadr x)))) rules))]
                    (cond [(not (empty? Merrors))
                           (string-append "\n THE MIDDLES OF THE FOLLOWING RULES ARE NOT VALID: "
                                          (theFold Merrors))]
                          [else (local [(define RHerrors (filter (lambda (x) (not (RHpred (caddr x)))) rules))]
                                  (cond [(not (empty? RHerrors))
                                         (string-append "\n THE RHS OF THE FOLLOWING RULES ARE NOT VALID: "
                                                        (theFold RHerrors))]
                                        [else ""]))]))])))

  ;purpose: to make sure the rule is
  ;          (NTs ARROW (Ts V (Ts && NTs)))
  ;       or (S ARROW empty)
  (define (check-rgrule nts sigma delta)
    (checkThrupples (lambda (x) (member x nts))
                    (lambda (x) (equal? ARROW x))
                    (lambda (x) (oneAndOne x nts sigma))
                    delta))

  ;purpose: to make sure the rule is
  ;          (NTs ARROW (Ts V NTs)*)
  (define (check-cfgrule nts sigma delta)
    (checkThrupples (lambda (x) (member x nts))
                    (lambda (x) (equal? ARROW x))
                    (lambda (x) (or (equal? EMP x)
                                    (allBoth x nts sigma)))
                    delta))

  ;purpose: to make sure the rule is
  ;         (((NTs V Ts)* && NTs && (NTs V Ts)*) ARROW (NTs V Ts)*)
  (define (check-csgrule nts sigma delta)
    (checkThrupples (lambda (x) (and (hasOne x nts)
                                     (allBoth x nts sigma)))
                    (lambda (x) (equal? ARROW x))
                    (lambda (x) (or (equal? EMP x)
                                    (allBoth x nts sigma)))
                    delta))

  ;purpose: to make sure the rule is a state, a sigma element, and a state
  (define (check-dfarule states sigma delta)
    (checkThrupples (lambda (x) (member x states))
                    (lambda (x) (member x sigma))
                    (lambda (x) (member x states))
                    delta))

  ;purpose: to make sure the rule is a state, a sigma element or empty, and a state
  (define (check-ndfarule states sigma delta)
    (checkThrupples (lambda (x) (member x states))
                    (lambda (x) (or (equal? x EMP)
                                    (member x sigma)))
                    (lambda (x) (member x states))
                    delta))


  (define (check-pda-rules  states sigma gamma delta)
    (cond
      [(empty? delta) ""]
      [else
       (string-append (check-pdarule states sigma gamma (car delta)) (check-pda-rules  states sigma gamma (cdr delta)))]))
  

  ;purpose: to make sure the rule is two lists
  ;      (state, sigma element or empty, gamma element or empty)
  ;      (state, gamma element or empty)
  (define (check-pdarule states sigma gamma delta)
    (let* ((triple (car delta))
           
           (double (cadr delta))
           (fromerror (if (not (member (car triple) states))
                          (list (car triple))
                          '()))
           (frommsg (if (null? fromerror)
                        ""
                        (format "The from state ~s is not in the list of states for rule: ~s." (car triple) delta)))
           (consumeerror (if (not (member (cadr triple) (cons EMP sigma)))
                             (list (cadr triple))
                             '()))
           (consumemsg (if (null? fromerror)
                           (if (null? consumeerror)
                               frommsg
                               (format "The consumed input ~s is not in sigma for rule: ~s." (cadr triple) delta))
                           (if (null? consumeerror) frommsg
                               (string-append frommsg
                                              (format "\nThe consumed input ~s is not in sigma for rule: ~s." (cadr triple) delta)))))
           (poperror (if (not (list? (caddr triple)))
                         (if (eq? EMP (caddr triple))
                             '()
                             (list (caddr triple)))
                         (foldl (lambda (s a)
                                  (if (not (member s gamma)) (cons s a) a))
                                empty
                                (caddr triple))))
           (popmsg (cond [(null? poperror) consumemsg]
                         [(equal? "" consumemsg) 
                          (format "The pop list ~s contains non-gamma elements for rule: ~s." (caddr triple) delta)]
                         [else (string-append consumemsg
                                              (format "\nThe pop list ~s contains non-gamma elements for rule: ~s." (caddr triple) delta))]))
           (toerror (if (not (member (car double) states))
                        (list (car double))
                        '()))
           (tomsg (cond [(null? toerror) popmsg]
                        [(equal? "" popmsg)
                         (format "The to state ~s is not in the list of states for rule: ~s." (car double) delta)]
                        [else (string-append popmsg
                                             (format "\nThe to state ~s is not in the list of states for rule: ~s." (car double) delta))]))
           (pusherror (if (not (list? (cadr double)))
                          (if (equal? EMP (cadr double))
                              '()
                              (list (cadr double)))
                          (foldl (lambda (s a)
                                   (if (not (member s gamma)) (cons s a) a))
                                 empty
                                 (cadr double))))
           (pushmsg (cond [(null? pusherror) tomsg]
                          [(equal? "" tomsg)
                           (format "The push list ~s contains non-gamma elements for rule: ~s." (cadr double) delta)]
                          [else (string-append tomsg
                                               (format "\nThe push list ~s contains non-gamma elements for rule: ~s." (cadr double) delta))])))
      pushmsg))

  ;purpose: to make sure the rule is two lists
  ;      (state, sigma elem or empty or space)
  ;      (state, sigma elem or empty or space)
  (define (check-tmrules states sigma deltas)
    (define (check-tmrule sts alpha rule)
      (let* ((from (caar rule))
             (consume (cadar rule))
             (to (caadr rule))
             (action (cadadr rule))
             (fromerror (if (member from states)
                            ""
                            (format "The from state ~s is not in the list of state for rule: ~s" from rule)))
             (consumeerror (if (member consume (cons BLANK (cons LM alpha)))
                               ""
                               (format "The consumed item ~s is not in sigma for rule: ~s" consume rule)))
             (consumemsg (cond [(eq? consumeerror "") fromerror]
                               [(eq? fromerror "") consumeerror]
                               [else (string-append fromerror
                                                    (format "\n~a" consumeerror))]))                              
             (toerror (if (member to states)
                          ""
                          (format "The to state ~s is not in the list of state for rule: ~s" to rule)))
             (tomsg (cond [(eq? toerror "") consumemsg]
                          [(eq? consumemsg "") consumeerror]
                          [else (string-append consumemsg
                                               (format "\n~a" toerror))]))
             (actionerror (if (member action (append (list RIGHT LEFT BLANK) alpha))
                              ""
                              (format "The action ~s is not is not valid for rule: ~s" action rule)))
             (actionmsg (cond [(eq? actionerror "") tomsg]
                              [(eq? tomsg "") actionerror]
                              [else (string-append tomsg
                                                   (format "\n~a" actionerror))])))
        actionmsg))
  (if (null? deltas)
      ""
      (let ((problem (check-tmrule states sigma (car deltas))))
        (string-append problem (check-tmrules states sigma (cdr deltas))))))

)