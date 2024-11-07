#lang racket

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base
                     racket/match
                     syntax/to-string
                     "viz-state.rkt"
                     racket/struct-info)
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


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can accept/process a given word
(define-syntax (check-accept stx)
  (syntax-parse stx

    ;; Turing machines
    [(_ M [word header-pos:nat]...)
     #`(let* ([word-lst (list word ...)]
              [word-stx-lst (list #'word ...)]
              [header-pos-lst (list header-pos ...)]
              [res (foldr (lambda (val word-val head accum)
                            (if (equal? (sm-apply M word-val head) 'accept)
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
                       (format "~s does not accept the following word: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (first (map second res)))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (rest (map second res))))))
                       (format "~s does not accept the following words: ~a"
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
                           (if (equal? (sm-apply M (second (syntax->datum val))) 'accept)
                               accum
                               (cons val accum)
                               )                  
                           )
                         '()
                         (list #'x ...))])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length (list #'x ...)) 1)
                       (format "~s does not accept the following word: ~a"
                               (syntax-e #'M)
                               (apply string-append (cons (format "\n~s" (second (syntax->datum (first res))))
                                                          (map (lambda (n) (format "\n~s" n))
                                                               (map second (map syntax->datum (rest res)))))))
                       (format "~s does not accept the following words: ~a"
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
                           (if (not (string? (grammar-derive G (second (syntax->datum val)))))
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


