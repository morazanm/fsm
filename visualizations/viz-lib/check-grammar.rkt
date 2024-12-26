#lang racket/base

(require (for-syntax syntax/parse
                     racket/syntax-srcloc
                     racket/base)
         racket/syntax-srcloc
         racket/list
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "check-exn.rkt")

(provide check-grammar)


;; stx -> stx
;; Purpose: Tests grammars
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ C:id G (~var w ) ...)
     #`(let* ([unprocessed-word-lst (list w ...)]
              [unprocessed-word-stx-lst (list #'w ...)]
              [invalids (foldr (lambda (x x-stx accum) (if (C x)
                                                           accum
                                                           (cons (list x x-stx) accum)))
                               '()
                               unprocessed-word-lst
                               unprocessed-word-stx-lst)]
              [invalid-words (map first invalids)]
              [invalid-word-stx (map second invalids)])
         (if (empty? invalids)
             (let* ([res (foldr (lambda (word word-stx accum)
                                  (if (not (string? (cond [(rg? G) (rg-derive G word)]
                                                          [(cfg? G) (cfg-derive G word)]
                                                          [(csg? G) (csg-derive G word)]
                                                          [else (raise (exn:fail:check-failed
                                                                        "Unknown grammar type"
                                                                        (current-continuation-marks)
                                                                        (list (syntax-srcloc #'G)))
                                                                       #t)])
                                                    ))
                                      accum
                                      (cons (list word word-stx) accum)))
                                '()
                                (list w ...)
                          
                                (list #'w ...))]
                    [word-lst (map first res)]
                    [word-stx-lst (map second res)])
               (unless (empty? res)
                 (let ([failure-str (cond [(and (= (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not derive the following word: ~a"
                                                   (syntax-e #'M)
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not derive the following words: ~a"
                                                   (syntax-e #'M)
                                                   (apply string-append (cons (format "\n~s" (first word-lst))
                                                                              (map (lambda (n) (format "\n~s" n))
                                                                                   (rest word-lst)))))]
                                          [(and (= (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not derive the following word: ~a"
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not derive the following words: ~a"
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
                                                          word-stx-lst))
                                        #t))
                 ))
             (let ([failure-str (apply string-append
                                       (cons "Step X of the design recipe has not been sucessfully completed. The following words contain nonterminals not in the language of the constructed grammar"
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

(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ C:id G (~var w ) ...)
     #`(let* ([unprocessed-word-lst (list w ...)]
              [unprocessed-word-stx-lst (list #'w ...)]
              [invalids (foldr (lambda (x x-stx accum) (if (C x)
                                                           accum
                                                           (cons (list x x-stx) accum)))
                               '()
                               unprocessed-word-lst
                               unprocessed-word-stx-lst)]
              [invalid-words (map first invalids)]
              [invalid-word-stx (map second invalids)])
         (if (empty? invalids)
             (let* ([res (foldr (lambda (word word-stx accum)
                                  (if (string? (cond [(rg? G) (rg-derive G word)]
                                                          [(cfg? G) (cfg-derive G word)]
                                                          [(csg? G) (csg-derive G word)]
                                                          [else (raise (exn:fail:check-failed
                                                                        "Unknown grammar type"
                                                                        (current-continuation-marks)
                                                                        (list (syntax-srcloc #'G)))
                                                                       #t)])
                                                    )
                                      accum
                                      (cons (list word word-stx) accum)))
                                '()
                                (list w ...)
                          
                                (list #'w ...))]
                    [word-lst (map first res)]
                    [word-stx-lst (map second res)])
               (unless (empty? res)
                 (let ([failure-str (cond [(and (= (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, derives the following word: ~a"
                                                   (syntax-e #'M)
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, derives the following words: ~a"
                                                   (syntax-e #'M)
                                                   (apply string-append (cons (format "\n~s" (first word-lst))
                                                                              (map (lambda (n) (format "\n~s" n))
                                                                                   (rest word-lst)))))]
                                          [(and (= (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine derives the following word: ~a"
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine derives the following words: ~a"
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
                                                          word-stx-lst))
                                        #t))
                 ))
             (let ([failure-str (apply string-append
                                       (cons "Step X of the design recipe has not been sucessfully completed. The following words contain nonterminals not in the language of the constructed grammar"
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

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-grammar stx)
  (define-syntax-class
    valid-word
    (pattern (quote w)))
  
  (define-syntax-class
    invalid-word
    (pattern (~not (quote w))))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var x valid-word) ...)
     #'(if accept?
           (check-accept C M x ...)
           (check-reject C M x ...))]
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