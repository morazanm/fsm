#lang racket/base
(require "constants.rkt"
         "misc.rkt"
         racket/list
         racket/set
         )
(provide  check-rgrule check-cfgrule check-csgrule
          check-dfarule check-ndfarule check-pda-rules check-tmrules
          ;check-pdarule check-tmrule

          ;; experted for testing
          ;allBoth oneAndOne allOne hasOne
            
          )

;sub-member: (listof symbols) string num num --> boolean
;purpose: to check if a string is present in a list in symbolform
(define (sub-member set-of-symb str low high)
  (set-member? set-of-symb (string->symbol (substring str low high))))

;purpose: to check that the symbol given is composed
;         entirely of the first and second given list
(define (allBoth sym set1 set2)
  (define combined (set-union set1 set2))
  (define str (symbol->string sym))
  (define str-length (string-length str))
  (define losfsm (symbol->fsmlos sym));; added by Marco
  ;loop: number --> boolean
  ;purpose: to iterate through the word
  (define (loop end)
    (cond [(zero? end) #t]
          [(sub-member combined str (sub1 end) end)
           (loop (sub1 end))]
          [else #f]))
            
  (cond [(andmap (λ (s) (set-member? combined s)) losfsm) #t]
        [(< str-length 1) #f]
        [else (loop str-length)]))

;purpose: to check that the symbol given is composed
;         of one symbol from the second list,
;         possibly followed by one symbol from the first list
(define (oneAndOne sym set1 set2)
  (define str (symbol->string sym))
  (define str-length (string-length str))
  (cond [(or (> str-length 2)
             (< str-length 1)) #f]
        [else (and (sub-member set2 str 0 1)
                   (if (> (string-length str) 1)
                       (sub-member set1 str 1 2)
                       #t))]))

;purpose: to check if the symbol is composed entirely of
;         symbols from the list
(define (allOne sym list1)
  (allBoth sym list1 '()))

;purpose: to ensure that at least one symbol from a list is contained
(define (hasOne sym list1)
  (define str (symbol->string sym))
  ;loop: number --> boolean
  ;purpose: to iterate through the word
  (define (loop end)
    (cond [(zero? end) #f]
          [(sub-member list1 str (sub1 end) end) #t]
          [else (loop (sub1 end))]))
  (loop (string-length str)))

;purpose: to check any three-part rule
(define (checkThrupples LHpred Mpred RHpred rules)
  (define (theFold a-list)
    (foldl (lambda (x y) (string-append y (format "\n\t ~s" x))) "" a-list))
  (define-values (LHerrors Merrors RHerrors)
    (for/fold ([LHerrors '()]
               [Merrors '()]
               [RHerrors '()])
              ([rule (in-list rules)])
      (values (cons (if (LHpred (car rule))
                        '()
                        rule) LHerrors)
              (cons (if (Mpred (cadr rule))
                        '()
                        rule) Merrors)
              (cons (if (RHpred (caddr rule))
                        '()
                        rule) RHerrors))))
  (cond [(not (empty? LHerrors))
         (string-append "\n THE LHS OF THE FOLLOWING RULES ARE NOT VALID: "
                        (theFold LHerrors))]
        [(not (empty? Merrors))
         (string-append "\n THE MIDDLES OF THE FOLLOWING RULES ARE NOT VALID: "
                        (theFold Merrors))]
        [(not (empty? RHerrors))
         (string-append "\n THE RHS OF THE FOLLOWING RULES ARE NOT VALID: "
                        (theFold RHerrors))]
        [else ""])
    
  #;(define LHerrors (filter (lambda (x) (not (LHpred (car x)))) rules))
  #;(cond [(not (empty? LHerrors))
           (string-append "\n THE LHS OF THE FOLLOWING RULES ARE NOT VALID: "
                          (theFold LHerrors))]
          [else
           (define Merrors (filter (lambda (x) (not (Mpred (cadr x)))) rules))
           (cond [(not (empty? Merrors))
                  (string-append "\n THE MIDDLES OF THE FOLLOWING RULES ARE NOT VALID: "
                                 (theFold Merrors))]
                 [else
                  (define RHerrors (filter (lambda (x) (not (RHpred (caddr x)))) rules))
                  (cond [(not (empty? RHerrors))
                         (string-append "\n THE RHS OF THE FOLLOWING RULES ARE NOT VALID: "
                                        (theFold RHerrors))]
                        [else ""])])]))

  
;purpose: to make sure the rule is
;          (NTs ARROW (Ts V (Ts && NTs)))
;       or (S ARROW empty)
(define (check-rgrule nts sigma delta)
  (define nts-set (list->set nts))
  (checkThrupples (lambda (x) (set-member? nts-set x))
                  (lambda (x) (eq? ARROW x))
                  (lambda (x) (oneAndOne x nts sigma))
                  delta))

;; (listof symbol) (listof symbol) (listof cfg-rule) --> string
;purpose: to make sure the rule is
;          (NTs ARROW (Ts V NTs)*)
(define (check-cfgrule nts sigma delta)
  (define nts-set (list->set nts))
  (define sigma-set (list->set sigma))
  (checkThrupples (lambda (x) (set-member? nts-set x))
                  (lambda (x) (eq? ARROW x))
                  (lambda (x) (or (eq? EMP x)
                                  (for/and ([rhs (in-list x)])
                                    (or (set-member? nts-set rhs)
                                        (set-member? sigma-set rhs)))))
                  delta))

(define (has-one-nt nts-set sigma-set lhs)
  (define (loop-without-check lhs)
    (if (null? lhs)
        #t
        (if (or (set-member? nts-set (car lhs))
                (set-member? sigma-set (car lhs)))
            (loop-without-check (cdr lhs))
            #f)))
  (define (loop-with-check lhs)
    (if (null? lhs)
        #f
        (if (set-member? nts-set (car lhs))
            (loop-without-check (cdr lhs))
            (if (set-member? sigma-set (car lhs))
                (loop-with-check (cdr lhs))
                #f))))
  (loop-with-check lhs))
            

;purpose: to make sure the rule is
;         (((NTs V Ts)* && NTs && (NTs V Ts)*) ARROW (NTs V Ts)*)
(define (check-csgrule nts sigma delta)
  (define nts-set (list->set nts))
  (define sigma-set (list->set sigma))
  (checkThrupples (lambda (x) (has-one-nt nts-set sigma-set x)
                    #;(and (hasOne x nts)
                           (allBoth x nts sigma)))
                  (lambda (x) (eq? ARROW x))
                  (lambda (x) (or (eq? EMP x)
                                  (for/and ([rhs-elem (in-list x)])
                                    (or (set-member? nts-set rhs-elem)
                                        (set-member? sigma-set rhs-elem)))
                                  #;(allBoth x nts sigma)))
                  delta))

;purpose: to make sure the rule is a state, a sigma element, and a state
(define (check-dfarule states sigma delta)
  (define states-set (list->set states))
  (define sigma-set (list->set sigma))
  (checkThrupples (lambda (x) (set-member? states-set x))
                  (lambda (x) (set-member? sigma-set x))
                  (lambda (x) (set-member? states-set x))
                  delta))

;purpose: to make sure the rule is a state, a sigma element or empty, and a state
(define (check-ndfarule states sigma delta)
  (define states-set (list->set states))
  (define sigma-set (list->set sigma))
  (checkThrupples (lambda (x) (set-member? states-set x))
                  (lambda (x) (or (eq? x EMP)
                                  (set-member? sigma-set x)))
                  (lambda (x) (set-member? states-set x))
                  delta))


(define (check-pda-rules states sigma gamma delta)
  (cond
    [(empty? delta) ""]
    [else
     (define states-set (list->set states))
     (define sigma-set (list->set (cons EMP sigma)))
     (define gamma-set (list->set gamma))
     (apply string-append
            (for/list ([rule (in-list delta)])
              (check-pdarule states-set sigma-set gamma-set rule)))]))
  

;purpose: to make sure the rule is two lists
;      (state, sigma element or empty, gamma element or empty)
;      (state, gamma element or empty)
(define (check-pdarule states-set sigma-set gamma-set rule)
  (define triple (car rule))
  (define double (cadr rule))
  (define fromerror (set-member? states-set (car triple))
    #;(if (not (set-member? states-set (car triple)))
          (list (car triple))
          '()))
  (define frommsg (if fromerror
                      ""
                      (format "The from state ~s is not in the list of states for rule: ~s." (car triple) rule)))
  (define consumeerror (set-member? sigma-set (cadr triple))
    #;(if (not (set-member? sigma-set (cadr triple)))
          (list (cadr triple))
          '()))
  (define consumemsg (if fromerror
                         (if consumeerror
                             ""
                             (format "The consumed input ~s is not in sigma for rule: ~s." (cadr triple) rule))
                         (if consumeerror
                             frommsg
                             (string-append frommsg
                                            (format "\nThe consumed input ~s is not in sigma for rule: ~s." (cadr triple) rule)))))
  (define poperror
    (or (eq? EMP (caddr triple))
        (and (list? (caddr triple))
             (for/and ([elem (in-list (caddr triple))])
               (set-member? gamma-set elem))))
    #;(if (not (list? (caddr triple)))
          (if (eq? EMP (caddr triple))
              '()
              (list (caddr triple)))
          (foldl (lambda (s a)
                   (if (not (set-member? gamma-set s))
                       (cons s a)
                       a))
                 '()
                 (caddr triple))))
  (define popmsg (cond [poperror consumemsg]
                       [(string=? "" consumemsg) 
                        (format "The pop list ~s contains non-gamma elements for rule: ~s." (caddr triple) rule)]
                       [else (string-append consumemsg
                                            (format "\nThe pop list ~s contains non-gamma elements for rule: ~s." (caddr triple) rule))]))
  (define toerror (set-member? states-set (car double))
    #;(if (not (set-member? states-set (car double)))
          (list (car double))
          '()))
  (define tomsg (cond [toerror popmsg]
                      [(string=? "" popmsg)
                       (format "The to state ~s is not in the list of states for rule: ~s." (car double) rule)]
                      [else (string-append popmsg
                                           (format "\nThe to state ~s is not in the list of states for rule: ~s." (car double) rule))]))
  (define pusherror
    (or (eq? EMP (cadr double))
        (and (list? (cadr double))
             (for/and ([elem (in-list (cadr double))])
               (set-member? gamma-set elem))))
    #;(if (not (list? (cadr double)))
          (if (equal? EMP (cadr double))
              '()
              (list (cadr double)))
          (foldl (lambda (s a)
                   (if (not (set-member? gamma-set s)) (cons s a) a))
                 '()
                 (cadr double))))

  (cond [pusherror tomsg]
        [(string=? "" tomsg)
         (format "The push list ~s contains non-gamma elements for rule: ~s." (cadr double) rule)]
        [else (string-append tomsg
                             (format "\nThe push list ~s contains non-gamma elements for rule: ~s." (cadr double) rule))]))

;purpose: to make sure the rule is two lists
;      (state, sigma elem or empty or space)
;      (state, sigma elem or empty or space)
(define (check-tmrules states sigma deltas)
  (define states-set (list->set states))
  (define consume-set (list->set (cons BLANK (cons LM sigma))))
  (define action-set (list->set (cons RIGHT (cons LEFT (cons BLANK sigma)))))
  (define (check-tmrule rule)
    (define from (caar rule))
    (define consume (cadar rule))
    (define to (caadr rule))
    (define action (cadadr rule))
    (define fromerror (if (set-member? states-set from)
                          ""
                          (format "The from state ~s is not in the list of state for rule: ~s" from rule)))
    (define consumeerror (if (set-member? consume-set consume)
                             ""
                             (format "The consumed item ~s is not in sigma for rule: ~s" consume rule)))
    (define consumemsg (cond [(string=? consumeerror "") fromerror]
                             [(string=? fromerror "") consumeerror]
                             [else (string-append fromerror
                                                  (format "\n~a" consumeerror))]))
    (define toerror (if (set-member? states-set to)
                        ""
                        (format "The to state ~s is not in the list of state for rule: ~s" to rule)))
    (define tomsg (cond [(string=? toerror "") consumemsg]
                        [(string=? consumemsg "") consumeerror]
                        [else (string-append consumemsg
                                             (format "\n~a" toerror))]))
    (define actionerror (if (set-member? action-set action)
                            ""
                            (format "The action ~s is not is not valid for rule: ~s" action rule)))
    (cond [(eq? actionerror "") tomsg]
          [(eq? tomsg "") actionerror]
          [else (string-append tomsg
                               (format "\n~a" actionerror))]))
  (if (null? deltas)
      ""
      (apply string-append (for/list ([rule (in-list deltas)])
                             (check-tmrule rule)))
      #;(let ((problem (check-tmrule states sigma (car deltas))))
        (string-append problem (check-tmrules states sigma (cdr deltas))))))