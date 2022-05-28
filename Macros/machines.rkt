#lang racket
(require (for-syntax
          racket/syntax
          syntax/stx
          syntax/parse
          racket/list
          racket/string
          syntax/to-string
          racket/match))

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
    ))

(define-syntax (make-dfa stx)
  ;; syntax class for a valid state
  (define-syntax-class states
    #:description "The states that the machine can transition to"
    (pattern '(state:id ...)        
      #:fail-when (or
                   (check-duplicate-identifier (syntax->list #'(state ...)))
                   (invalid-state-names (syntax->list #`(state ...))))
      "Duplicate state name or invalid state name"))

  ;; syntax class for alphabet
  (define-syntax-class alphas
    #:description "The alphabet that the machine accepts"
    (pattern '(alpha:id ...)
      #:fail-when (or
                   (check-duplicate-identifier (syntax->list #'(alpha ...)))
                   (invalid-alpha-names (syntax->list #`(alpha ...))))
      "Duplicate state name or invalid state name"))

  ;; cyntax class for start state
  (define-syntax-class start
    #:description "The starting state of the machine"
    (pattern `s:id
      #:fail-when (invalid-state-name? #'s)
      "Invalid start state name"))

  (define-syntax-class finals
    #:description "The final states of the machine"
    (pattern '(final:id ...)
      #:fail-when (or
                   (check-duplicate-identifier (syntax->list #'(final ...)))
                   (invalid-state-names (syntax->list #`(final ...))))
      "Invalid final state name"))
  
  (syntax-parse stx
    [(_ sts:states a:alphas s:start) #`(void)]))




(make-dfa '(A B C)
          '(a b c)
          'A-1)