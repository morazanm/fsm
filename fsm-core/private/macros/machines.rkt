#lang racket/base
(require (for-syntax
          racket/base
          racket/syntax
          syntax/stx
          syntax/parse
          racket/bool
          racket/list
          racket/string
          syntax/to-string
          racket/match)
         )

(begin-for-syntax
  ;; member-stx :: syntax -> [syntax] -> boolean
  ;; Returns true if the target syntax is in the list of syntax
  ;; Note: This uses datum level comparison not syntax-object level comparison
  (define (member-stx? target-stx stx-list)
    (not (false? (member (syntax->datum target-stx)
                         (map syntax->datum stx-list)))))

  ;; lists-eq-error :: [syntax] -> [syntax] -> boolean | syntax
  ;; returns false if all vals of the first list are present in the second list
  ;; If there is invalid form then the invalid syntax is returned to racket can
  ;; highlight the appropriate syntax to error on
  ;; Note: This uses datum level comparison not syntax-object level comparison
  (define (lists-eq-error target-stx-list stx-list)
    (cond
      [(empty? target-stx-list) #f]
      [(member-stx? (car target-stx-list) stx-list) (lists-eq-error (cdr target-stx-list)
                                                                    stx-list)]
      [else (car target-stx-list)]))


  ;; invalid-rules? :: [syntax] -> [syntax] ->  [syntax] ->  [syntax] ->  [syntax] ->  syntax | boolean
  ;; returns false if all rule start, ends, and alphas are vaild for the machine.
  ;; If there is invalid form then the invalid syntax is returned to racket can
  ;; highlight the appropriate syntax to error on
  (define (invalid-rules? rule-starts rule-alphas rule-ends alphas states)
    (define rule-states (remove-duplicates (append  rule-starts
                                                    rule-ends)))
    (define check1 (lists-eq-error rule-states states))
    (if (false? check1)
        (let ([check2 (lists-eq-error rule-alphas alphas)])
          (if (false? check2)
              #f
              check2))
        check1))
    
    
  
  ;; stx->char-list :: syntax -> [char]
  ;; Converts syntax to a list of characters
  (define (stx->char-list stx)
    ((compose string->list symbol->string syntax-e) stx))

  ;; invalid-state-name? :: syntax -> boolean | syntax
  ;; Determies if the given syntax is a valid state name. If there is invalid form
  ;; then the invalid syntax is returned to racket can highlight the appropriate syntax to error on
  ;; A valid state name is one fo the following:
  ;; - Uppercase Letter
  ;; - Number
  ;; - The character `-`
  (define (invalid-state-name? stx)
    (define (helper c-list)
      (cond
        [(empty? c-list) #f]
        [(not (or (char-upper-case? (car c-list))
                  (char-numeric? (car c-list))
                  (eq? (car c-list) #\-))) stx]
        [else (helper (cdr c-list))]))
    (helper (stx->char-list stx)))

  ;; invalid-alpha-name? :: syntax -> boolean | syntax
  ;; Determies if the given syntax is a valid alpha name. If there is invalid form
  ;; then the invalid syntax is returned to racket can highlight the appropriate syntax to error on
  ;; A valid alpha name is a single lowercase letter [a-z]
  (define (invalid-alpha-name? stx)
    (match (stx->char-list stx)
      [`(,c) #:when (char-lower-case? c) #f]
      [_ stx])))

(define-syntax (make-dfa stx)

  ;; syntax class for a single state 
  (define-syntax-class state
    #:description "A single state of the machine"
    (pattern field:id
      #:fail-when (invalid-state-name? #'field)
      "Invalid state name"))

  ;; syntax class for a single alphabet
  (define-syntax-class alpha
    #:description "A single alphabet for the machine"
    (pattern field:id
      #:fail-when (invalid-alpha-name? #'field)
      "Invalid alphabet name"))
  
  ;; syntax class for a list of states
  (define-syntax-class states
    #:description "The states that the machine can transition to"
    (pattern '(fields:state ...)        
      #:fail-when (check-duplicate-identifier (syntax->list #'(fields ...)))
                   
      "Duplicate state name or invalid state name"))

  ;; syntax class for a list of alphabet
  (define-syntax-class alphas
    #:description "The alphabet that the machine accepts"
    (pattern '(fields:alpha ...)
      #:fail-when  (check-duplicate-identifier (syntax->list #'(fields ...)))
                   
      "Duplicate state name or invalid state name"))

  ;; syntax class for start state
  (define-syntax-class start
    #:description "The starting state of the machine"
    (pattern `field:id
      #:fail-when (invalid-state-name? #'field)
      "Invalid start state name"))

  ;; syntax class for a list of finals 
  (define-syntax-class finals
    #:description "The final states of the machine"
    (pattern '(fields:state ...)
      #:fail-when (check-duplicate-identifier (syntax->list #'(fields ...)))
      "Duplicate or invalid final state name"))

  ;; syntax class for a list of rules
  (define-syntax-class rules
    #:description "The transition rules thae the machine must follow"
    (pattern '((s1:state a:alpha s2:state) ...)))
  
  (syntax-parse stx
    [(_ sts:states a:alphas s:start f:finals r:rules)
     ;; Make sure the start state is in the list of states
     #:fail-when (false? (member-stx? #`s.field (syntax->list #`(sts.fields ...))))
     (raise-syntax-error #f "Start state must be in the list of states" #`s)
     ;; Make sure the final states are in the list of states
     #:fail-when (lists-eq-error (syntax->list #`(f.fields ...)) (syntax->list #`(sts.fields ...)))
     "Final state must be in the list of states"
     ;; Make sure the rules checkout
     #:fail-when (invalid-rules?  (syntax->list #`(r.s1 ...))
                                  (syntax->list #`(r.a ...))
                                  (syntax->list #`(r.s2 ...))
                                  (syntax->list #`(a.fields ...))
                                  (syntax->list #`(sts.fields ...)))
     "Invalid rules supplied:"
     (begin
       #`(void))]))


(make-dfa '(A B-1 C)
          '(a b c)
          'A
          '(A B-1 C)
          '((A a C)
            (B-1 a A)))