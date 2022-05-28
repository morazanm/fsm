#lang racket
(require (for-syntax
          racket/syntax
          syntax/stx
          syntax/parse
          racket/list
          racket/string
          syntax/to-string
          racket/match))


#|
  ;; invalid-state-names :: [syntax] -> boolean | syntax
  ;; Determies if each value in the syntax list is a valid state name. If there is invalid form
  ;; then the invalid syntax is returned to racket can highlight the appropriate syntax to error on
  ;; A valid state name is one fo the following:
  ;; - Uppercase Letter
  ;; - Number
  ;; - The character `-`
  (define (invalid-state-names stx-list)
    (define (invalid? stx acc)
      (define val (invalid-state-name? stx))
      (if val (cons stx acc) acc))
    (define vals (foldr invalid? '() stx-list))
    (if (empty? vals) #f (car vals))) ;; racket only allows one syntax to hightlight at a time

  ;; invalid-alpha-names :: [syntax] -> boolean | syntax
  ;; Determies if each value in the syntax list is a valid alpha name. If there is invalid form
  ;; then the invalid syntax is returned to racket can highlight the appropriate syntax to error on
  ;; A valid alpha name is single lowercase letter [a-z]
  (define (invalid-alpha-names stx-list)
    (define (invaild? stx acc)
      (define c-list (stx->char-list stx))
      (match c-list
        [`(,c) #:when (char-lower-case? c) acc]
        [_ (cons stx acc)]))
    (define vals (foldr invaild? '() stx-list))
    (if (empty? vals) #f (car vals))
    )
|#

(begin-for-syntax
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
    (pattern `s:id
      #:fail-when (invalid-state-name? #'s)
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
  ;#:fail-when (check-duplicate-identifier (syntax->list #'((s1 a s2) ...)))
  ;"Duplicate transitionn rule"))
  
  (syntax-parse stx
    [(_ sts:states a:alphas s:start f:finals r:rules) #`(void)]))




(make-dfa '(A-1 B-1 C)
          '(a b c)
          'A-1
          '(A B)
          '((A a A)
            (B b A)))