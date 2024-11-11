#lang racket

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base
                     racket/match
                     syntax/to-string
                     "../../fsm-core/private/sm-getters.rkt"
                     "viz-state.rkt"
                     racket/struct-info)
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
       a-srcloc]
      )))

(define (is-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'ndfa m-type)
        (eq? 'dfa m-type)
        (eq? 'pda m-type)
        (eq? 'tm m-type)
        (eq? 'tm-language-recognizer m-type))))

(define (is-turing-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'tm m-type)
        (eq? 'tm-language-recognizer m-type))))

(define (is-grammar? g)
  (or (rg? g)
      (cfg? g)
      (csg? g)))


(define-syntax (check-accept-grammar stx)
  (define-syntax-class grammar
    (pattern G
      ))
  (syntax-parse stx
    [(_ G:grammar . w)
     #:with (x ...) #'w
     #`(let* ([res (foldr (lambda (word word-stx accum)
                           (if (not (string? (grammar-derive G word)))
                               accum
                               (cons (list word word-stx) accum)
                               )                  
                           )
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
                  #t
                  )))
     ]
    )
  )


(define (tm-word/c lang)
  (listof (apply or/c lang))
  )

(define-syntax (check-accept-turing-machine stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-machine?)
      #:with m #'M
      #:with c (attribute M.c)
      ))
  (define-syntax-class tm-word
    (pattern [word0:expr header-pos0]
      #:with word #'word0
      #:with head-pos #'header-pos0
      ))
  (syntax-parse stx
    [(_ M:machine x:tm-word ...)
     #`(let* ([res (foldr (lambda (val word-val head accum)
                            (if (equal? (sm-apply M.c word-val head) 'accept)
                                accum
                                (cons (list val word-val) accum)
                                )                  
                            )
                          '()
                          (list #'x.word ...)
                          (list x.word ...)
                          (list x.head-pos ...)
                          )]
              [word-lst (map second res)]
              [word-stx-lst (map first res)])
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
                  #t
                  )))
       
     ]
    ))

(define-syntax (check-accept-machine stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-machine?)
      #:with m #'M
      #:with c (attribute M.c)
      ))

  
  (syntax-parse stx
    
    [(_ M:machine . w)
     ;#:declare m (expr/c #'is-ndfa?)
     ;#:with M (attribute m.c)
     #:with (x ...) #'w
     #`(let* ([res (foldr (lambda (word wordstx accum)
                            (if (equal? (sm-apply M.c word) 'accept)
                                accum
                                (cons (list word wordstx) accum)
                                )                 
                            )
                          '()
                          (list x ...)
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
                  #t
                  )))
     ]
    ))

#;(define (check-accept1 m . x)
  (cond
    [(is-turing-machine? m) (match x
                              ([word head-pos]... (check-accept-turing-machine m [word head-pos]...)))]
    [(is-machine? m) (check-accept-machine m . x)]
    [(is-grammar? m) (check-accept-grammar m . x)]))
;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can accept/process a given word
(define-syntax (check-accept stx)
  (define-syntax-class machine
    (pattern M
      #:declare M (expr/c #'is-turing-machine?)
      #:with m #'M
      #:with c (attribute M.c)
      ))
  (define-syntax-class tm-word
    (pattern [word0:expr header-pos0]
      #:with word #'word0
      #:with head-pos #'header-pos0
      ))
  (define-syntax-class unknown-tm-word
    (pattern (~not (~datum quote))))
  (syntax-parse stx
    [(_ M:machine [(~seq word:unknown-tm-word header-pos:expr)] ...)
     #`(begin
         (displayln (syntax->datum #'word))...
         ;(check-accept-turing-machine M [word header-pos] ...)
         )
     #;(let* ([res (foldr (lambda (val word-val head accum)
                            (if (equal? (sm-apply M.c word-val head) 'accept)
                                accum
                                (cons (list val word-val) accum)
                                )                  
                            )
                          '()
                          (list #'x.word ...)
                          (list x.word ...)
                          (list x.head-pos ...)
                          )]
              [word-lst (map second res)]
              [word-stx-lst (map first res)])
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
                  #t
                  )))
       
     ]
    [(_ unknown-expr . words)
     #:with (x ...) #'words
     #`(cond ;[(is-turing-machine? unknown-expr) (check-accept-turing-machine unknown-expr x ...)]
             [(is-machine? unknown-expr) (check-accept-machine unknown-expr x ...)]
             [(is-grammar? unknown-expr) (check-accept-grammar unknown-expr x ...)]
             [else (raise (exn:fail:check-failed
                           (format "~s is not a valid FSM value that can be tested" (syntax->datum #'unknown-expr))
                           (current-continuation-marks)
                           (list (syntax-srcloc #'unknown-expr))
                           )
                          #t)
                           #;(error "TODO make new exception for this")]
             )
         
     ]
    
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


