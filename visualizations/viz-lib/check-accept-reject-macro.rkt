#lang racket

(require #;(for-template racket/base
                       racket/syntax-srcloc
                       syntax/parse)
         (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base
                     racket/match
                     syntax/to-string
                     "../../fsm-core/private/sm-getters.rkt"
                     "viz-state.rkt"
                     racket/struct-info
                     syntax/parse/experimental/template)
         ;syntax/parse/experimental/template
         racket/syntax-srcloc
         "../../fsm-core/private/grammar-getters.rkt"
         syntax/to-string
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "../../fsm-core/private/sm-apply.rkt"
         "../../fsm-core/private/tm.rkt"
         "viz-state.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")

(provide check-accept check-reject)


;; exeption-structure
(struct exn:fail:check-failed exn:fail
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:check-failed msg marks (list a-srcloc ...))
       a-srcloc])))

;; smth -> Boolean
;; Purpose: Checks if smth is a machine (not tm)
(define (is-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'ndfa m-type)
        (eq? 'dfa m-type)
        (eq? 'pda m-type))))

;; smth -> Boolean
;; Purpose: Checks if smth is a turing machine
(define (is-turing-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'tm m-type)
        (eq? 'tm-language-recognizer m-type))))


;; smth -> Boolean
;; Purpose: Checks is smth is a grammar
(define (is-grammar? g)
  (or (rg? g)
      (cfg? g)
      (csg? g)))



;; stx -> stx
;; Purpose: Tests grammars
(define-syntax (check-accept-grammar stx)
  (define-syntax-class grammar
    (pattern G))
  (syntax-parse stx
    [(_ G:grammar . w)
     #:with (x ...) #'w
     #`(let* ([res (foldr (lambda (word word-stx accum)
                            (if (not (string? (grammar-derive G word)))
                                accum
                                (cons (list word word-stx) accum)))
                          '()
                          (list x ...)
                          (list #'x ...))]
              [word-lst (map first res)]
              [word-stx-lst (map second res)])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length word-lst) 1)
                       (format "~s doesn't derive the following word: ~a"
                               (syntax-e #'G)
                               (first word-lst))
                       (format "~s doesn't derive the following words: ~a"
                               (syntax-e #'G)
                               (apply string-append (cons (format "\n~s" (first word-lst))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest word-lst))))))
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        word-stx-lst))
                  #t)))]))

(define-syntax (check-accept-possibly-correct-turing-machine stx)
  (displayln stx)

  (define-syntax-class
    testing
    (pattern (~not (~datum quote))))
  (define-syntax-class
    valid-pair
    (pattern (quote (w n))))

  (define-splicing-syntax-class
    valid-pairs
    (pattern (~seq (~var x valid-pair) ...))
    (pattern (~seq))
    )
  
  (define-syntax-class
    invalid-pair
    (pattern (~not (quote (w n))) #;(~or (quote ())
                  (quote (w))
                  )))

  (define-splicing-syntax-class
    invalid-pairs
    (pattern (~seq (~var bad-pair invalid-pair) ...+))
    )
  
  (define-splicing-syntax-class
    bad-pairs
    (pattern (~and (~var x valid-pairs) (~var y invalid-pairs) (~var z valid-pairs)) #;(~and (~seq (~var before-pairs valid-pair) ...) (~seq (~var bad-pairs invalid-pair) ...+) (~seq (~var after-pairs valid-pair) ...))))

  (syntax-parse stx
    [(_ C:id M)
     ;; TODO raise some error
     #`(raise (exn:fail:check-failed
                   "Test does not contain any words to test"
                   (current-continuation-marks)
                   (list #,(syntax-srcloc stx))
                   #;(map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        word-stx-lst))
              #t)
     #;(error "only machine")]
    #;[(_ C:id M (~var yikes bad-pairs))
     #'(error "it worked")]
    [(_ C:id M (~var before-pairs valid-pair) ... (~var bad-pair invalid-pair) _ ...)
     #'(error "illformed pair")]
    [(_ C:id M (~var first-pairs valid-pair) ... )
     #'(error "valid matched")]
    #;[(_ C:id M (~var first-pairs valid-pair) ... [(~not (datum quote))] _ ...)
     #'(void)]
    ))
        ;(~var first-pairs valid-pair) ... [(~not (datum quote))] [(~not (datum quote)) head-pos] ...)


;; stx -> stx
;; Purpose: Checks turing machines
(define-syntax (check-accept-turing-machine stx)
  (define-syntax-class machine
    (pattern M
      ;#:declare M (expr/c #'is-turing-machine?)
      ;#:with m #'M
      ;#:with c (attribute M.c)
      ))
  (define-syntax-class unknown-tm-word
    (pattern (~not (~datum quote))))
  (define-syntax-class (tm-word word-contract)
    (pattern [word0 header-pos0]
      #:declare word0 (expr/c word-contract #:positive #'word0)
                            
      #:with word #'word0
      #:with cword #'word0.c
      #:with head-pos #'header-pos0
      
      ))
  (syntax-parse stx
    [(_ C:id M:machine (~var x (tm-word #'C)) ...)
     #`(let* ([res (foldr (lambda (val word-val cword-val head accum)
                            (if (equal? (sm-apply M cword-val head) 'accept)
                                accum
                                (cons (list val word-val) accum)
                                )                  
                            )
                          '()
                          (list #'x.word ...)
                          (list x.word ...)
                          (apply list #,#'(values (syntax/loc #'x #'x.cword) ...))
                          (list x.head-pos ...)
                          )]
              [word-lst (map second res)]
              [word-stx-lst (map first res)])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length word-lst) 1)
                       (format "~s does not accept the following word: ~a"
                               (syntax-e #'M)
                               (first word-lst))
                       (format "~s does not accept the following words: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (first word-lst))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest word-lst)))))
                       )
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        word-stx-lst))
                  #t)))]))
#;(define-syntax (testing stx)
  (syntax-parse stx
    [(_ ((~var x) (~var xc)) ...)
     #`(list #,(syntax/loc x xc) ...)]))








;; stx -> stx
;; Purpose: Checks state machines (without turing)
(define-syntax (check-accept-machine stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-machine?)
      #:with m #'M
      #:with c (attribute M.c)
      ))
  (define-syntax-class (sm-word sigma-contract)
    (pattern W
      #:declare W (expr/c sigma-contract
                          #:positive #'W)
      #:with w #'W
      #:with c (attribute W.c)))
  (define-template-metafunction (reattribute-syntax-list stx)
    (syntax-parse stx
      [(_ x xc)
       (template/loc #'x xc)]))
  (syntax-parse stx 
    [(_ C:id M:machine (~var x (sm-word #'C)) ...)
        ;#:with ((~var x (sm-word #'C)) ...) #'words
     #`(let* (
              [res (foldr (lambda (word cword wordstx accum)
                            (if (equal? (sm-apply M.c cword) 'accept)
                                accum
                                (cons (list word wordstx) accum)
                                )
                            )
                          '()
                          (list x ...)
                          (list (reattribute-syntax-list #'x x.c) ...)
                          #;(list x.c ...)
                          (list #'x ...))]
              [word-lst (map first res)]
              [word-stx-lst (map second res)])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length word-lst) 1)
                       (format "~s does not accept the following word: ~a"
                               (syntax-e #'M.m)
                               (first word-lst))
                       (format "~s does not accept the following words: ~a"
                               (syntax-e #'M.m)
                               (apply string-append (cons (format "\n~s" (first word-lst))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest word-lst)))))
                       )
                   (current-continuation-marks)
                   (map (lambda (z)
                          (srcloc (syntax-source z)
                                  (syntax-line z)
                                  (syntax-column z)
                                  (syntax-position z)
                                  (syntax-span z)))
                        word-stx-lst))
                  #t)))]))

#;(define (tm-word/c lang)
  (listof (apply or/c lang)))

(define (new-tm-word/c sigma)
  (define tm-word-char/c (apply or/c sigma))
  (define tm-word/c (or/c null?
                          (cons/c tm-word-char/c (recursive-contract tm-word/c #:flat))))
  tm-word/c
  #;(make-flat-contract
   ;#:first-order tm-word/c
   ;#:late-neg-projection (contract-late-neg-projection tm-word/c)
   #:projection (lambda (blame)
    (lambda (z)
      (map (lambda (x) (if (member x lang)
                           x
                           (raise-blame-error
                            blame
                            z
                            '(expected nonterminal in machines language, given: "~e")
                            x)))
           z)
      ))))

(define (new-sm-word/c sigma)
  (define sm-word-char/c (apply or/c sigma))
  (define sm-word/c (listof sm-word-char/c))
  sm-word/c)

;; stx -> stx
;; Purpose: To determine whether a given machine/grammar can accept/process a given word
(define-syntax (check-accept stx)
  (define-syntax-class tmachine
    (pattern M
      #:declare M (expr/c #'is-turing-machine?)
      #:with m #'M
      #:with c (attribute M.c)
      ))
  (define-syntax-class unknown-tm-word
    (pattern (~not (~datum quote))))
  (syntax-parse stx
    ;; Turing machines
    #;[(_ M:tmachine [(~seq word:unknown-tm-word header-pos:expr)] ...)
     #`(let ([tm-word-contract (new-tm-word/c (cons '_ (sm-sigma M.c)))])
         (check-accept-turing-machine tm-word-contract M.m [word header-pos]...)
         )
     ]
    ;; State machines (no turing) or grammars
    [(_ unknown-expr . words)
     #:with (x ...) #'words
     #`(cond [(is-turing-machine? unknown-expr)
              (let ([tm-word-contract (new-tm-word/c (cons '_ (sm-sigma unknown-expr)))])
                #,(syntax/loc stx (check-accept-possibly-correct-turing-machine tm-word-contract unknown-expr x ...))
                )]
             [(is-machine? unknown-expr) (let [(sm-word-contract (new-sm-word/c (sm-sigma unknown-expr)))]
                                           #,(syntax/loc stx (check-accept-machine sm-word-contract unknown-expr x ...)))]
             [(is-grammar? unknown-expr) #,(syntax/loc stx (check-accept-grammar unknown-expr x ...))]
             [else (raise (exn:fail:check-failed
                           (format "~s is not a valid FSM value that can be tested" (syntax->datum #'unknown-expr))
                           (current-continuation-marks)
                           (list (syntax-srcloc #'unknown-expr)))
                          #t)]
             )]
    ))

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


