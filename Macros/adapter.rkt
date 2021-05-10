#lang racket
(require (for-syntax racket/syntax syntax/stx syntax/parse racket/list racket/string))
(provide adapter)

(begin-for-syntax
  ;; transform-id :: syntax -> synatx
  ;; Purpose: trandformes the syntax into 'a-<randome_number>'
  (define (transform-id id)
    (gensym "a-"))

  ;; transform-list :: syntax -> syntax-pair
  ;; syntax pair: '(identifier procedure/'NONE)
  ;; Purpse: if the syntax contains a procedure(i.e. number?) we
  ;;   return a new identifier and the origional
  (define (transform-list stx)
    (for/list ([val (syntax->list stx)])
      (if (symbol? (syntax->datum val))
          (let ([s ((compose symbol->string syntax->datum) val)])
            (cond
              [(string-contains? (substring s (sub1 (string-length s))) "?") (list (transform-id val) val)]
              [else (list val 'NONE)]))
          (list val 'NONE))))

  ;; parse-match :: synatx -> listof(listof(syntax-pairs))
  ;; Purpose: converts the syntax to pairs for parsing the match statment for
  ;;  guard clauses
  (define (parse-match clause)
    (syntax-parse clause
      [(list (s ...) ...) (stx-map (lambda (inner) (transform-list inner))
                                   #'((s ...) ...))]
      [(s ...) (list (transform-list #'(s ...)))]))

  ;; build-guard :: listof(listof(pairs)) -> synatx
  ;; Purpose: builds the guard clause for the function. returns false if
  ;;  there are not any guard clauses to be made.
  (define (build-guard lop)
    (define guards (flatten
                    (filter-map (lambda (outter)
                                  (let ([vals (filter-map (lambda (inner)
                                                            (if (eq? (cadr inner) 'NONE)
                                                                #f
                                                                (with-syntax ([t1 (second inner)]
                                                                              [t2 (first inner)])
                                                                  #`(t1 t2)))) outter)])
                                    (if (empty? vals) #f vals))) lop)))
    (define guard-clause
      (cond
        [(= 1 (length guards)) (with-syntax([t (car guards)])
                                 #`(#:when t))]
        [(> (length guards) 1)
         #`(#:when (and #,@guards))]
        [else #'(void)]))

    (if (empty? guards)
        #f
        #`(#,@guard-clause)))
        
  ;; transform-match :: syntax syntax -> syntax
  ;; Purpose: takes in a syntax and converts it the the propper racket match synatx
  (define (transform-match clause func)
    (let* ((parsed-match (parse-match clause))
           ;; fields are the first of the syntax pairs
           (fields (let ([t (map (lambda (outter) (map (lambda (inner) (car inner)) outter)) parsed-match)])
                     (if (eq? 1 (length t))
                         (car t)
                         t)))
           ;; the guard cause syntax i.e. #:when ...
           (guard-clause (build-guard parsed-match)))
      (with-syntax ([f func])
        (if guard-clause ;; if there is not a guard clause then dont add it...
            (syntax-parse fields
              [((field ...) ...) #`[(list (field ...) ...) #,@guard-clause (map f data)]]
              [(field ...) #`[(field ...) #,@guard-clause (map f data)]])
            (syntax-parse fields
              [((field ...) ...) #`[(list (field ...) ...) (map f data)]]
              [(field ...) #`[(field ...) (map f data)]]))))))

;; adapter :: synatx -> syntax
;; Purpose: This is the marco that converts the adapter syntax into the approperate racket
;;  function. The name of the function is: <name supplied>-adapter. See examples below for
;;  more details.
(define-syntax (adapter stx)
  (define-syntax-class wc
    #:description "basic symbol"
    (pattern _))
  (define-syntax-class lowc
    #:description "valid lou"
    (pattern (patt:wc ...))
    (pattern ((npatt:wc ...) ...)))

  (syntax-parse stx
    [(_ adapter-name:id (p:lowc ... <- func:id) ...)
     #:with adapt-fn-name (format-id #'adapter-name "~a-adapter" #'adapter-name)
     #:with (conds ...) (stx-map (lambda (p f)
                                   (syntax-parse p
                                     [(((s ...) ...))
                                      #:with fn f
                                      #:with t (transform-match #'(list (list s ...) ...) #'fn)
                                      #'t]
                                     [((s ...))
                                      #:with fn f
                                      #:with t (transform-match #'(list s ...) #'fn)
                                      #'t]))
                                 #'((p ...) ...)
                                 #'(func ...))
     #`(define (adapt-fn-name data)
         (match (car data)
           conds ...
           [else (error "Invalid pattern supplied")]))]))

(module+ test
  (require rackunit)

  (define (fsa-special-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (if (number? v)
                                                      (number->string v)
                                                      (symbol->string v))))
           "SPECIAL "
           rules))
  
  (define (fsa-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (symbol->string v)))
           ""
           rules))
  
  (define (pda-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (symbol->string v)))
           ""
           (flatten rules)))
  
  (define (tm-rule-to-string rules)
    (foldl (lambda (v accum) (string-append accum (symbol->string v)))
           ""
           (flatten rules)))
  
  (adapter graph
           [(_ number? _) <- fsa-special-rule-to-string]
           [(_ _ _) <- fsa-rule-to-string]
           [((_ _ _) (_ _)) <- pda-rule-to-string]
           [((_ _) (_ _)) <- tm-rule-to-string])

  (check-equal? (graph-adapter '((A a B) (B a B))) '("AaB" "BaB") "fsa rule should return AaBBaB")
  (check-equal? (graph-adapter '((A 1 B) (B a B))) '("SPECIAL A1B" "SPECIAL BaB") "special fsa rule should return AaBHELLO BaB")
  (check-equal? (graph-adapter '(((A a A) (A a)) ((B b B) (B b)))) '("AaAAa" "BbBBb") "pda rule should return AaAAaBbBBb")
  (check-equal? (graph-adapter '(((A a) (B b)) ((C c) (D d)))) '("AaBb" "CcDd") "tm rule should return AaBbCcDd"))