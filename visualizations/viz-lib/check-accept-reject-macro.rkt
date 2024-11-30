#lang racket

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base
                     racket/stxparam
                     syntax/contract
                     )
         racket/syntax-srcloc
         racket/stxparam
         syntax/contract
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/grammar-getters.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "../../fsm-core/private/sm-apply.rkt")

(provide check-accept check-reject)

(define (display-failed-test desc exn)
  ((error-display-handler) desc exn))

;; exeption-structure
(struct exn:fail:check-failed exn:fail:user
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:check-failed msg marks (list a-srcloc ...))
       a-srcloc])))

;; Any -> Boolean
;; Purpose: Checks if m is a machine (not tm)
(define (is-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'ndfa m-type)
        (eq? 'dfa m-type)
        (eq? 'pda m-type))))

;; Any -> Boolean
;; Purpose: Checks if m is a turing machine
(define (is-turing-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'tm m-type)
        (eq? 'tm-language-recognizer m-type))))


;; Any -> Boolean
;; Purpose: Checks is g is a grammar
(define (is-grammar? g)
  (or (rg? g)
      (cfg? g)
      (csg? g)))

;; Syntax -> Syntax
;; Changes srcloc data associated with syntax so as to raise errors up to caller code
(define-syntax (reattribute-syntax-loc stx)
    (syntax-parse stx
      [(_ x xc)
       (syntax/loc #'x (identity xc))]))

(define-syntax (reattribute-syntax-loc0 stx)
    (syntax-parse stx
      [(_ x xc)
       (syntax/loc #'x (identity xc))]))

;; stx -> stx
;; Purpose: Tests grammars
(define-syntax (check-accept-grammar stx)
  (define-syntax-class grammar
    (pattern G))

  (define-syntax-class (grammar-word g-contract)
    (pattern (quote w)
      #:with word #'(quote w)
      #:declare word (expr/c g-contract)
      #:with cword #'word.c))
  
  (syntax-parse stx
    [(_ C:id G:grammar (~var w (grammar-word #'C)) ...)
     #`(let* ([res (foldr (lambda (word cword word-stx accum)
                            (if (not (string? (cond [(rg? G) (rg-derive G cword)]
                                                    [(cfg? G) (cfg-derive G cword)]
                                                    [(csg? G) (csg-derive G cword)]
                                                    [else (raise (exn:fail:check-failed
                                                                  "Unknown grammar type"
                                                                  (current-continuation-marks)
                                                                  (list (syntax-srcloc #'G)))
                                                                 #t)])
                                              ))
                                accum
                                (cons (list word word-stx) accum)))
                          '()
                          (list w.word ...)
                          (list w.cword ...)
                          (list #'w ...))]
              [word-lst (map first res)]
              [word-stx-lst (map second res)])
         (unless (empty? res)
           (let ([failure-str (if (= (length word-lst) 1)
                       (format "~s doesn't derive the following word: ~a"
                               (syntax-e #'G)
                               (first word-lst))
                       (format "~s doesn't derive the following words: ~a"
                               (syntax-e #'G)
                               (apply string-append (cons (format "\n~s" (first word-lst))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest word-lst))))))])
             (display-failed-test failure-str (exn:fail:check-failed
                                               failure-str
                                               (current-continuation-marks)
                                               (map (lambda (z)
                                                      (srcloc (syntax-source z)
                                                              (syntax-line z)
                                                              (syntax-column z)
                                                              (syntax-position z)
                                                              (syntax-span z)))
                                                    word-stx-lst))
                                  #t))
           ))]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-possibly-correct-grammar-syntax stx)
  (define-syntax-class
    valid-word
    (pattern (quote w)))
  
  (define-syntax-class
    invalid-word
    (pattern (~not (quote w))))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var x valid-word) ...)
     #'(check-accept-grammar C M x ...)]
    [(_ accept?:boolean C:id M (~or (~var valids valid-word)
                    (~var invalids invalid-word)) ...)
     #'(raise (exn:fail:check-failed
                   (format "The following expressions are not valid words that can be tested:\n~a"
                           (foldr (lambda (val accum) (string-append (format "~a\n" val) accum)) "" (list invalids ...)))
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        (list #'invalids ...)))
                  #t)]
    ))

;; Syntax -> Syntax
;; Purpose: Checks turing machines
(define-syntax (check-accept-turing-machine stx)
  (define-syntax-class machine
    (pattern M))
  
  (define-syntax-class (tm-word word-contract)
    (pattern (quote (word0 head-pos0))
      #:with word (syntax/loc #'word0 (quote word0))
      #:declare word (expr/c word-contract #:positive #'word)
      #:with cword #'word.c
      #:with head-pos #'head-pos0))
  
  (syntax-parse stx
    [(_ C:id M:machine (~var x (tm-word #'C)) ...)
     #`(let* ([res (foldr (lambda (val word-val cword-val head accum)
                            (if (equal? (sm-apply M cword-val head) 'accept)
                                accum
                                (cons (list val word-val) accum)
                                ))
                          '()
                          (list #'x.word ...)
                          (list x.word ...)
                          (list (reattribute-syntax-loc x x.cword) ...)
                          (list x.head-pos ...))]
              [word-lst (map second res)]
              [word-stx-lst (map first res)])
         (unless (empty? res)
           (let ([failure-str (if (= (length word-lst) 1)
                       (format "~s does not accept the following word: ~a"
                               (syntax-e #'M)
                               (first word-lst))
                       (format "~s does not accept the following words: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (first word-lst))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest word-lst))))))])
             (display-failed-test failure-str (exn:fail:check-failed
                   failure-str
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        word-stx-lst))))))]))

(define-syntax (check-reject-turing-machine stx)
  (define-syntax-class machine
    (pattern M))
  
  (define-syntax-class (tm-word word-contract)
    (pattern (quote (word0 head-pos0))
      #:with word (syntax/loc #'word0 (quote word0))
      #:declare word (expr/c word-contract #:positive #'word)
      #:with cword #'word.c
      #:with head-pos #'head-pos0))
  
  (syntax-parse stx
    [(_ C:id M:machine (~var x (tm-word #'C)) ...)
     #'(void)]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-possibly-correct-turing-machine-syntax stx)
  (define-syntax-class
    valid-pair
    (pattern (quote (w n))))

  (define-syntax-class
    missing-one-expr
    (pattern (quote (x))
      #:with exp #'x))

  (define-syntax-class
    missing-both-exprs
    (pattern (quote ())))
  
  (define-syntax-class
    invalid-pair
    (pattern (~not (quote (w n)))))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var first-pairs valid-pair) ... )
     #'(if accept?
           (check-accept-turing-machine C M first-pairs ...)
           (check-reject-turing-machine C M first-pairs ...))]
    [(_ accept?:boolean C:id M (~or (~var valids valid-pair)
                                    (~var missing-one missing-one-expr)
                                    (~var missing-both missing-both-exprs)
                                    (~var invalids invalid-pair)) ...)
     #`(raise (exn:fail:check-failed
               (let [(position-missing-exprs (filter (lambda (x) (list? (first x))) (list missing-one ...)))
                     (word-missing-exprs (filter (lambda (x) (exact-nonnegative-integer? (first x))) (list missing-one ...)))]
                 (string-append
                  (if (not (empty? position-missing-exprs))
                      (format "The following test cases are missing a head position:\n~a"
                              (foldr (lambda (val accum) (string-append (format "~a\n" val) accum)) "" position-missing-exprs))
                      "")
                  (if (not (empty? word-missing-exprs))
                      (format "The following test cases are missing a word:\n~a"
                              (foldr (lambda (val accum) (string-append (format "~a\n" val) accum)) "" word-missing-exprs))
                      "")
                  (if (not (empty? (list missing-both ...)))
                      (format "The following test cases are missing both a word and a head position:\n~a"
                              (foldr (lambda (val accum) (string-append (format "~a\n" val) accum)) "" (list missing-both ...)))
                      "")
                  (if (not (empty? (list invalids ...)))
                      (format "The following test cases are malformed:\n~a"
                              (foldr (lambda (val accum) (string-append (format "~a\n" val) accum)) "" (list invalids ...)))
                      "")))
               (current-continuation-marks)
               (map (lambda (z)
                        (srcloc (syntax-source z)
                                (syntax-line z)
                                (syntax-column z)
                                (syntax-position z)
                                (syntax-span z)))
                      (append (list #'missing-one ...) (list #'missing-both ...) (list #'invalids ...))))
              #t)]))

;; Syntax -> Syntax
;; Purpose: Checks state machines (without turing)
(define-syntax (check-accept-machine stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-machine?)
      #:with m #'M
      #:with c (attribute M.c)))
  
  (syntax-parse stx 
    [(_ C:id M:machine ((~var x) ...))
     #`(let* ([unprocessed-word-lst (list x ...)]
              [unprocessed-word-stx-lst (list #'x ...)]
              [invalids (foldr (lambda (w w-stx accum) (if (C w)
                                                           accum
                                                           (cons (list w w-stx) accum)))
                                    '()
                                    unprocessed-word-lst
                                    unprocessed-word-stx-lst)]
              [invalid-words (map first invalids)]
              [invalid-word-stx (map second invalids)])
         (if (empty? invalids)
             (let* ([res (foldr (lambda (word cword wordstx accum)
                                  (if (equal? (sm-apply M.c word) 'accept)
                                      accum
                                      (cons (list word wordstx) accum)))
                                '()
                                unprocessed-word-lst
                                unprocessed-word-stx-lst)]
                    [word-lst (map first res)]
                    [word-stx-lst (map second res)])
               (unless (empty? res)
                 (let ([failure-str (cond [(and (= (length word-lst) 1) (identifier? #'M.m))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not accept the following word: ~a"
                                                   (syntax-e #'M.m)
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (identifier? #'M.m))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not accept the following words: ~a"
                                                   (syntax-e #'M.m)
                                                   (apply string-append (cons (format "\n~s" (first word-lst))
                                                                              (map (lambda (n) (format "\n~s" n))
                                                                                   (rest word-lst)))))]
                                          [(and (= (length word-lst) 1) (not (identifier? #'M.m)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not accept the following word: ~a"
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (not (identifier? #'M.m)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not accept the following words: ~a"
                                                   (apply string-append (cons (format "\n~s" (first word-lst))
                                                                              (map (lambda (n) (format "\n~s" n))
                                                                                   (rest word-lst)))))]
                                          )])
                   (display-failed-test failure-str (exn:fail:check-failed
                                                     failure-str
                                                     (current-continuation-marks)
                                                     (map (lambda (z)
                                                            (srcloc (syntax-source z)
                                                                    (syntax-line z)
                                                                    (syntax-column z)
                                                                    (syntax-position z)
                                                                    (syntax-span z)))
                                                          word-stx-lst))))))
             (let ([failure-str (apply string-append
                                       (cons "Step X of the design recipe has not been sucessfully completed. The word following words contain nonterminals not in the language of the constructed machine"
                                             (map (lambda (n) (format "\n~a" n)) invalid-words))) 
                                       ])
               (display-failed-test failure-str (exn:fail:check-failed
                                                     failure-str
                                                     (current-continuation-marks)
                                                     (map (lambda (z)
                                                            (srcloc (syntax-source z)
                                                                    (syntax-line z)
                                                                    (syntax-column z)
                                                                    (syntax-position z)
                                                                    (syntax-span z)))
                                                          invalid-word-stx))))
         
           )
         )
           ]))
(define (sm-word-lst/c0 sigma)
  (lambda (x)
    (andmap (lambda (y) (andmap (lambda (z) (member z sigma)) y)) x)))

(define (sm-word-lst/c sigma)
  #;(make-flat-contract
   #:first-order (listof (listof (apply or/c sigma)))
   
   #:projection
   (lambda (blame)
     (lambda (x)
       #;(current-blame-format format-error)
       (if ((listof (listof (apply or/c sigma))) x)
           x
           (raise-blame-error blame x (format "Step 2 of the design recipe has not been successfully completed. The following words that should be accepted contain symbols not included in sigma")))
       )
     
     ))
  (listof (apply or/c sigma))
  )

#;(define-syntax (check-accept-machine0 stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-machine?)
      #:with m #'M
      #:with c (attribute M.c)))

  

  (define-splicing-syntax-class (new-sm-word-lst sigma-contract)
    (pattern (~seq w ...+)
      #:with lst #'(list w ...)
      #:declare lst (expr/c #'sm-word-contract #;sigma-contract)
      #:with lst-c (attribute lst.c)
      #:with stx-lst #'(list #'w ...)))

  #;(define-splicing-syntax-class word-lst
    (pattern (~var x new-sm-word-lst)))

  (define-syntax-class (sm-words contr)
    (pattern (w ...)
      #:with lst (syntax/loc #'(w ...) (list w ...))
      #:declare lst (expr/c contr)
      #:with lst-c (attribute lst.c)
      #:with stx-lst #'(list #'w ...)
      ))
  (syntax-parse stx 
    [(_ C:id M:machine (w ...)) ;(~var x (sm-word #'C)) ...)
     #:with lst #'(list w ...)
     #:with stx-lst #'(list #'w ...)
         #`(begin
            (let ([invalid-words (filter (lambda (x) (not (C x))) (list w ...))])
              (if (not (empty? invalid-words))
                  (error "TODO implement contract like RBE")
                  (let* ([res (foldr (lambda (word cword wordstx accum)
                                       (if (equal? (sm-apply M.c cword) 'accept)
                                           accum
                                           (cons (list word wordstx) accum)))
                                     '()
                                     lst
                                     lst
                                     stx-lst)]
                         [word-lst (map first res)]
                         [word-stx-lst (map second res)])
                    (unless (empty? res)
                      (let ([failure-str (cond [(and (= (length word-lst) 1) (identifier? #'M.m))
                                                (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not accept the following word: ~a"
                                                        (syntax-e #'M.m)
                                                        (first word-lst))]
                                               [(and (> (length word-lst) 1) (identifier? #'M.m))
                                                (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not accept the following words: ~a"
                                                        (syntax-e #'M.m)
                                                        (apply string-append (cons (format "\n~s" (first word-lst))
                                                                                   (map (lambda (n) (format "\n~s" n))
                                                                                        (rest word-lst)))))]
                                               [(and (= (length word-lst) 1) (not (identifier? #'M.m)))
                                                (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not accept the following word: ~a"
                                                        (first word-lst))]
                                               [(and (> (length word-lst) 1) (not (identifier? #'M.m)))
                                                (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not accept the following words: ~a"
                                                        (apply string-append (cons (format "\n~s" (first word-lst))
                                                                                   (map (lambda (n) (format "\n~s" n))
                                                                                        (rest word-lst)))))]
                                               )])
                        (display-failed-test failure-str (exn:fail:check-failed
                                                          failure-str
                                                          (current-continuation-marks)
                                                          (map (lambda (z)
                                                                 (srcloc (syntax-source z)
                                                                         (syntax-line z)
                                                                         (syntax-column z)
                                                                         (syntax-position z)
                                                                         (syntax-span z)))
                                                               word-stx-lst))))))
                  ))
           )
       
           ]))




(define-syntax (check-reject-machine stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-machine?)
      #:with m #'M
      #:with c (attribute M.c)))
  
  (define-syntax-class (sm-word sigma-contract)
    (pattern W
      #:declare W (expr/c sigma-contract
                          #:positive #'W)
      #:with w #'W
      #:with c (attribute W.c)))
  
  (syntax-parse stx 
    [(_ C:id M:machine (~var x (sm-word #'C)) ...)
     #'(void)]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-possibly-correct-machine-syntax stx)
  (define-syntax-class
    valid-word
    (pattern (quote w)))
  
  (define-syntax-class
    invalid-word
    (pattern (~not (quote w))))

  (define-splicing-syntax-class valid-words
    (pattern (~seq (~var x valid-word) ...)))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var x valid-word) ...)
     #;(syntax/loc #'(~seq x ...) (check-accept-machine C M (x ...)))
     #`(if accept?
           (check-accept-machine C M (x ...))
           #,(syntax/loc stx (check-reject-machine C M (x ...))))]
    [(_ accept?:boolean C:id M (~or (~var valids valid-word)
                                    (~var invalids invalid-word)) ...)
     #'(raise (exn:fail:check-failed
                   (format "The following expressions are not valid words that can be tested:\n~a"
                           (foldr (lambda (val accum) (string-append (format "~a\n" val) accum)) "" (list invalids ...)))
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        (list #'invalids ...)))
                  #t)]))

;; (Listof Symbol) -> Contract
;; Creates contract based off of language of turing machine provided in sigma
(define (new-tm-word/c sigma)
  (define tm-word-char/c (apply or/c sigma))
  (define tm-word/c (or/c null?
                          (cons/c tm-word-char/c (recursive-contract tm-word/c #:flat))))
  tm-word/c)

;; (Listof Symbol) -> Contract
;; Creates contract based off of language of state machine (not turing) provided in sigma
(define (new-sm-word/c sigma)
  (define sm-word-char/c (apply or/c sigma))
  (define sm-word/c (listof sm-word-char/c))
  (make-flat-contract
   #:first-order sm-word/c
   #:projection (lambda (blame)
                  (lambda (x)
                    #;(current-blame-format format-error)
                    (raise-blame-error blame x (format "Step 2 of the design recipe has not been successfully completed. The following words that should be accepted contain symbols not included in sigma"))))))


   


;; (Listof Symbol) -> Contract
;; Creates contract based off of language of grammar provided in sigma
(define (new-grammar-word/c sigma)sm-word-lst/c
  (define grammar-word-char/c (apply or/c sigma))
  (define grammar-word/c (listof grammar-word-char/c))
  grammar-word/c)

;; Syntax -> Syntax
;; Purpose: To determine whether a given machine/grammar can accept/process a given word
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ unknown-expr)
     #`(raise (exn:fail:check-failed
               "This test does not contain any words to test"
               (current-continuation-marks)
               (list #,(syntax-srcloc stx)))
              #t)]
    [(_ unknown-expr x ...)
     #`(cond [(is-turing-machine? unknown-expr)
              (let ([tm-word-contract (new-tm-word/c (cons '_ (sm-sigma unknown-expr)))])
                #,(syntax/loc stx (check-possibly-correct-turing-machine-syntax #t tm-word-contract unknown-expr x ...)))]
             [(is-machine? unknown-expr)
              (let [(sm-word-contract (sm-word-lst/c (sm-sigma unknown-expr)))]
                #,(syntax/loc stx (check-possibly-correct-machine-syntax #t sm-word-contract unknown-expr x ...)))]
             [(is-grammar? unknown-expr)
              (let [(grammar-word-contract (new-grammar-word/c (grammar-sigma unknown-expr)))]
                #,(syntax/loc stx (check-possibly-correct-grammar-syntax #t grammar-word-contract unknown-expr x ...)))]
             [else (raise (exn:fail:check-failed
                           (format "~s is not a valid FSM value that can be tested" (syntax->datum #'unknown-expr))
                           (current-continuation-marks)
                           (list (syntax-srcloc #'unknown-expr)))
                          #t)])]))

;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can reject a given word
(define-syntax (check-reject stx)
  (syntax-parse stx
    ;; Turing machines
    [(_ M [word header-pos:nat]...)
     #`(let* ([word-lst (list word ...)]
              [word-stx-lst (list #'word ...)]
              [header-pos-lst (list header-pos ...)]
              [res (foldr (lambda (val word-val head accum)
                            (if (equal? (sm-apply M word-val head) 'reject)
                                accum
                                (cons (list val word-val) accum)
                                )                  
                            )
                          '()
                          word-stx-lst
                          word-lst
                          header-pos-lst
                          )])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length word-lst) 1)
                       (format "~s does not reject the following word: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (first (map second res)))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest (map second res))))))
                       (format "~s does not reject the following words: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (first (map second res)))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest (map second res))))))
                       )
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        (map first res)))
                  #t
                  )))
     ]

    ;; ndfas and dfas
    [(_ M . w)
     #:with (x ...) #'w
     #`(let ([res (foldr (lambda (val accum)
                           (if (equal? (sm-apply M (second (syntax->datum val))) 'reject)
                               accum
                               (cons val accum)
                               )                  
                           )
                         '()
                         (list #'x ...))])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length (list #'x ...)) 1)
                       (format "~s does not reject the following word: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (second (syntax->datum (first res))))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (map second (map syntax->datum (rest res)))))))
                       (format "~s does not reject the following words: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (second (syntax->datum (first res))))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (map second (map syntax->datum (rest res)))))))
                       )
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        res))
                  #t
                  )))
     ]

    ;; Grammars

    [(_ G . w)
     #:with (x ...) #'w
     #`(let ([res (foldr (lambda (val accum)
                           (if (string? (grammar-derive G (second (syntax->datum val))))
                               accum
                               (cons val accum)
                               )                  
                           )
                         '()
                         (list #'x ...))])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length (list #'x ...)) 1)
                       (format "~s derives the following word: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (second (syntax->datum (first res))))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (map second (map syntax->datum (rest res)))))))
                       (format "~s derives the following words: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (second (syntax->datum (first res))))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (map second (map syntax->datum (rest res)))))))
                       )
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        res))
                  #t
                  )))
     ]
     

    
    ))


