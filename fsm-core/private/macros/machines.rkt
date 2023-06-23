#lang racket
(require (for-syntax
          syntax/parse
          racket/bool
          racket/list
          racket/match
          syntax/stx))

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

  (define (check-finals stxlist slist)
    (define (helper stx-list state-list acc)
      (cond [(empty? stx-list) (if (empty? acc) #f acc)]
            [(member-stx? (car stx-list) state-list) (helper (cdr stx-list)
                                                             state-list
                                                             acc)]
            [else (helper (cdr stx-list)
                          state-list
                          (cons (car stx-list) acc))]))
    (helper stxlist slist '())
    )


  ;; invalid-rules? :: [syntax] -> [syntax] ->  [syntax] ->  [syntax] ->  [syntax] ->  syntax | boolean
  ;; returns false if all rule start, ends, and alphas are vaild for the machine.
  ;; If there is invalid form then the invalid syntax is returned to racket can
  ;; highlight the appropriate syntax to error on
  (define (invalid-rules? rule-starts rule-alphas rule-ends alphas states)
    (define rule-states (remove-duplicates (append rule-starts
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
    (define (valid-state-name? state-name)
      (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
      (regexp-match regex-pattern state-name))
    (define state-name (syntax->datum stx))
    (if (or (not (symbol? state-name))
            (false? (valid-state-name? (symbol->string state-name))))
        stx
        #f)
    )

  ;; invalid-alpha-name? :: syntax -> boolean | syntax
  ;; Determies if the given syntax is a valid alpha name. If there is invalid form
  ;; then the invalid syntax is returned to racket can highlight the appropriate syntax to error on
  ;; A valid alpha name is a single lowercase letter [a-z]
  (define (invalid-alpha-name? stx)
    (match (stx->char-list stx)
      [`(,c) #:when (char-lower-case? c) #f]
      [_ stx]))

  (define (invalid-lostx? stx pred)
    (define failing-list
      (foldl (lambda (x y) (if (pred x)
                               (cons x y)
                               y))
             '()
             (syntax->list stx)))
    (define duplicates (return-all-duplicates (syntax->list stx)))
    (if (and (empty? failing-list)
             (empty? duplicates)) #f
                                  (if (empty? failing-list) duplicates
                                      failing-list)))
        

  (define (construct-error-message pred name-of-list stx)
    (string-append
     (let ([duplicates (return-all-duplicates (syntax->list stx))])
       (if (empty? duplicates)
           ""
           (format "\n\tDuplicates in list: ~s " duplicates)))
     (let ([invalids (foldr (λ (s acc) (if (pred s)
                                           (cons (syntax->datum s) acc)
                                           acc))
                            empty
                            (map (lambda (x) (datum->syntax stx x))
                                 (remove-duplicates
                                  (map syntax->datum (syntax->list stx))
                                  )
                                 )
                            )])
       (if (empty? invalids)
           ""
           (format "\n\tInvalid ~a elements: ~s" name-of-list invalids)) 
       )))
  
  (define (return-all-duplicates los)
    (define (helper list-of-symbols duplicates)
      (cond [(empty? list-of-symbols) duplicates]
            [(member (car list-of-symbols) (cdr list-of-symbols))
             (helper (cdr list-of-symbols)
                     (cons (car list-of-symbols) duplicates))]
            [else (helper (cdr list-of-symbols)
                          duplicates)]))
    (remove-duplicates (helper (map syntax->datum los) '()))
    )

  ;takes in (list of (pair of syntax for symbols))
  (define (return-duplicate-rules lor)
    (define no-duplicates (remove-duplicates lor compare-rule-start))
    (foldr (lambda (x y) (if (member x no-duplicates)
                             y
                             (cons x y))) '() lor)
    )

  
  (define (compare-rule-start rule1 rule2)
    (equal? (list (syntax-e (car rule1))
                  (syntax-e (car (cdr rule1))))
            (list (syntax-e (car rule2))
                  (syntax-e (car (cdr rule2))))))

  (define (rule-member? rule rule-list)
    (ormap (lambda (x) (compare-rule-start rule x)) rule-list))
  
  (define (check-functional rule-list)
    (define rule-start-list (map (lambda (x) (list (car x) (car (cdr x)))) rule-list))
    (define duplicate-rules (return-duplicate-rules rule-start-list))
    (if (empty? duplicate-rules)
        #f
        duplicate-rules)
    )

  (define (check-included rule-list pair-list)
    (define rule-start-list (map (lambda (x) (list (car x) (car (cdr x)))) rule-list))
    (define leftovers (foldr (lambda (x y) (if (rule-member? x rule-start-list)
                                               y
                                               (cons x y))) '() pair-list))
    (if (empty? leftovers) #f
        leftovers))
    
  )


(define-syntax (make-dfa stx)
  ;; syntax class for a single state 
  (define-syntax-class state
    #:description "a single state of the machine"
    (pattern field:id
             #:fail-when (invalid-state-name? #'field)
             "Invalid state name"))

  ;; syntax class for a single alphabet
  (define-syntax-class alpha
    #:description "a single alphabet for the machine"
    (pattern field:id
             ;#:fail-when (invalid-alpha-name? #'field)
             #;"Alphabet member is not a single lowercase letter."))
  
  ;; syntax class for a list of states
  (define-syntax-class states
    #:description "the machine's states"
    (pattern '(fields ...)
             #:fail-when (invalid-lostx? #'(fields ...) invalid-state-name?)
             (construct-error-message invalid-state-name? "states" #'(fields ...)))
    (pattern (list fields ...)
             #:fail-when (invalid-lostx? #'(fields ...) invalid-state-name?)
             (construct-error-message invalid-state-name? "states" #'(fields ...))))


  ;; syntax class for a list of alphabet
  (define-syntax-class alphas
    #:description "the input alphabet:"
    (pattern '(fields ...)
             #:fail-when (invalid-lostx? #'(fields ...) invalid-alpha-name?)
             (construct-error-message invalid-alpha-name? "alphabet" #'(fields ...))))


  ;; syntax class for start state
  (define-syntax-class start
    #:description "the starting state of the machine"
    (pattern `field
             #:fail-when (invalid-state-name? #'field)
             (format "~s is an invalid start state name" (syntax->datum #'field))))

  ;; syntax class for a list of finals 
  (define-syntax-class finals
    #:description "the final states of the machine"
    (pattern '(fields:state ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(fields ...)))
             "Duplicate or invalid final state name"))

  ;; syntax class for a list of rules
  (define-syntax-class rules
    #:description "The transition rules thae the machine must follow"
    (pattern '((s1:state a:alpha s2:state) ...)))
  
  (syntax-parse stx
    [(_ sts:states a:alphas s:start f:finals r:rules (~optional (~var no-dead)))
     ;; Make sure the start state is in the list of states
     #:fail-when (false? (member-stx? #`s.field (syntax->list #`(sts.fields ...))))
     (raise-syntax-error 'make-dfa
                         (format "Start state must be in the list of states ~s" (syntax->datum #`(sts.fields ...)))
                         #'s)
     ;; Make sure the final states are in the list of states
     #:with failing-finals (check-finals (syntax->list #`(f.fields ...)) (syntax->list #`(sts.fields ...)))
     #:fail-when (if (boolean? (syntax-e #'failing-finals)) #f #`f)
     (format "final states ~s must be in the list of states ~s"
             (syntax->datum #'failing-finals)
             (syntax->datum #`(sts.fields ...))
             )
     ;; Make sure the rules checkout
     #:fail-when (invalid-rules?  (syntax->list #`(r.s1 ...))
                                  (syntax->list #`(r.a ...))
                                  (syntax->list #`(r.s2 ...))
                                  (syntax->list #`(a.fields ...))
                                  (syntax->list #`(sts.fields ...)))
     "Invalid rules supplied:"
     ;bring this part into the rules syntax class
     ;error?: syntax or #f
     #:with error? (check-functional (stx-map syntax-e #`((r.s1 r.a r.s2) ...)))
     #:fail-when (if (syntax-e #'error?)
                     #'r
                     #f)
     (format "State/alphabet pairs ~s is duplicated in rules" (map syntax->datum (syntax-e #'error?)))
     
     #:with error (check-included (stx-map syntax-e #`((r.s1 r.a r.s2) ...))
                                 (cartesian-product (syntax->list #`(sts.fields ...))
                                                    (syntax->list #`(a.fields ...))))
     #:fail-when (if (syntax-e #'error)
                     #'r
                     #f)
     (format "Must have rules for state/alphabet pairs: ~s" (map syntax->datum (syntax-e #'error)))
     
     (begin
       #`(void))]))


(module+ test
  (require rackunit syntax/macro-testing)

  (define-syntax (check-exn! stx)
    (syntax-parse stx
      [(_ msg:expr exn-msg:expr machine:expr)
       #`(check-exn exn-msg
                    (lambda () (convert-compile-time-error machine))
                    msg)]
      [(_ exn-msg:expr machine:expr)
       #`(check-exn exn-msg
                    (lambda () (convert-compile-time-error machine)))]
      [_ (error 'check-exn! "Invalid syntax")]))



  (check-exn! "Fails when there are invalid states"
              #rx"Invalid states elements: \\(a D\\-a\\)"
              (make-dfa '(a B-1 R C D-a)
                        '(a b e)
                        'D
                        '(A C)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when there are duplicate states"
              #rx"Duplicates in list: \\(D A\\)"
              (make-dfa '(A A B-1 R C D D)
                        '(a b e)
                        'D
                        '(A C)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when there are invalid alpha"
              #rx"Invalid alphabet elements: \\(D a\\-1\\)"
              (make-dfa '(A B-1 R C D)
                        '(a b e D a-1)
                        'D
                        '(A C)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when there are duplicate alpha"
              #rx"Duplicates in list: \\(a\\)"
              (make-dfa '(A B-1 R C D)
                        '(a b e a)
                        'D
                        '(A C)
                        '((A a C)
                          (B-1 a A))))

  (check-exn! "Fails when start state is not in the list of states"
              #rx"Start state must be in the list of states \\(A B\\-1 R C D\\)"
              (make-dfa '(A B-1 R C D)
                        '(a b e)
                        'F
                        '(A C)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when start state is a invalid state"
              #rx"a is an invalid start state name"
              (make-dfa '(A B-1 R C D)
                        '(a b e)
                        'a
                        '(A C)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when Final states are not in list of states"
              #rx"final states \\(Z Y X\\) must be in the list of states \\(A B\\-1 R C D\\)"
              (make-dfa '(A B-1 R C D)
                        '(a b e)
                        'A
                        '(X Y Z)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when Final states are invalid states"
              #rx"Invalid state name\n  at: X\\-a"
              (make-dfa '(A B-1 R C D)
                        '(a b e)
                        'A
                        '(X-a)
                        '((A a C)
                          (B-1 a A))))


  (check-exn! "Fails when duplicate rules are provided"
              #rx"State\\/alphabet pairs \\(\\(A a\\) \\(B-1 a\\)\\) is duplicated in rules"
              (make-dfa '(A B-1 R C D)
                        '(a b e)
                        'A
                        '(B-1)
                        '((A a C)
                          (A a C)
                          (B-1 a A)
                          (B-1 a A))))


  (check-exn! "Fails when duplicate rules are provided"
              #rx"Must have rules for state\\/alphabet pairs: \\(\\(A b\\) \\(A e\\) \\(B-1 b\\) \\(B-1 e\\) \\(R a\\) \\(R b\\) \\(R e\\) \\(C a\\) \\(C b\\) \\(C e\\) \\(D a\\) \\(D b\\) \\(D e\\)\\)"
              (make-dfa '(A B-1 R C D)
                        '(a b e)
                        'D
                        '(A C)
                        '((A a C)
                          (B-1 a A))))

  ) ;;end module+ test
