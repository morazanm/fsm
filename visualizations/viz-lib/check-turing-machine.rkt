#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         racket/list
         "../../fsm-core/private/sm-apply.rkt"
         "check-exn.rkt")

(provide check-turing-machine)

;; Syntax -> Syntax
;; Purpose: Checks turing machines
(define-syntax (check-accept stx)

  (define-syntax-class tm-word 
    (pattern (quote (word0 head-pos0))
      #:with word (syntax/loc #'word0 (quote word0))
      #:with head-pos #'head-pos0))
  
  (syntax-parse stx
    [(_ C:id M (~var x tm-word) ...)
     #`(let* ([unprocessed-word-lst (list x.word ...)]
              [unprocessed-word-stx-lst (list #'x.word ...)]
              [invalids (foldr (lambda (w w-stx accum) (if (C w)
                                                           accum
                                                           (cons (list w w-stx) accum)))
                                    '()
                                    unprocessed-word-lst
                                    unprocessed-word-stx-lst)]
              [invalid-words (map first invalids)]
              [invalid-word-stx (map second invalids)])
         (if (empty? invalids)
             (let* ([res (foldr (lambda (val word-val head-pos accum)
                                  (if (equal? (sm-apply M word-val head-pos) 'accept)
                                      accum
                                      (cons (list val word-val) accum)
                                      ))
                                '()
                                unprocessed-word-stx-lst
                                unprocessed-word-lst
                                (list x.head-pos ...))]
                    [word-lst (map second res)]
                    [word-stx-lst (map first res)])
               (unless (empty? res)
                 (let ([failure-str (cond [(and (= (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not accept the following word: ~a"
                                                   (syntax-e #'M)
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not accept the following words: ~a"
                                                   (syntax-e #'M)
                                                   (apply string-append (cons (format "\n~s" (first word-lst))
                                                                              (map (lambda (n) (format "\n~s" n))
                                                                                   (rest word-lst)))))]
                                          [(and (= (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not accept the following word: ~a"
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (not (identifier? #'M)))
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
                                                      invalid-word-stx)))))
         )
         ]))

(define-syntax (check-reject stx)
  (define-syntax-class tm-word
    (pattern (quote (word0 head-pos0))
      #:with word (syntax/loc #'word0 (quote word0))
      #:with head-pos #'head-pos0))
  
  (syntax-parse stx
    [(_ C:id M (~var x tm-word) ...)
     #`(let* ([unprocessed-word-lst (list x.word ...)]
              [unprocessed-word-stx-lst (list #'x.word ...)]
              [invalids (foldr (lambda (w w-stx accum) (if (C w)
                                                           accum
                                                           (cons (list w w-stx) accum)))
                                    '()
                                    unprocessed-word-lst
                                    unprocessed-word-stx-lst)]
              [invalid-words (map first invalids)]
              [invalid-word-stx (map second invalids)])
         (if (empty? invalids)
             (let* ([res (foldr (lambda (val word-val head-pos accum)
                                  (if (equal? (sm-apply M word-val head-pos) 'reject)
                                      accum
                                      (cons (list val word-val) accum)
                                      ))
                                '()
                                unprocessed-word-stx-lst
                                unprocessed-word-lst
                                (list x.head-pos ...))]
                    [word-lst (map second res)]
                    [word-stx-lst (map first res)])
               (unless (empty? res)
                 (let ([failure-str (cond [(and (= (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not reject the following word: ~a"
                                                   (syntax-e #'M)
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (identifier? #'M))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine, ~s, does not reject the following words: ~a"
                                                   (syntax-e #'M)
                                                   (apply string-append (cons (format "\n~s" (first word-lst))
                                                                              (map (lambda (n) (format "\n~s" n))
                                                                                   (rest word-lst)))))]
                                          [(and (= (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not reject the following word: ~a"
                                                   (first word-lst))]
                                          [(and (> (length word-lst) 1) (not (identifier? #'M)))
                                           (format "Step 6 of the design recipe has not been succesfully completed. The constructed machine does not reject the following words: ~a"
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
                                                          word-stx-lst)))
                   )))
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
                                                      invalid-word-stx)))))
         )
     ]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-turing-machine stx)
  (define-syntax-class
    valid-pair
    (pattern (quote (w n))))

  (define-syntax-class
    invalid-pair
    (pattern (~not (quote (w n)))))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var first-pairs valid-pair) ... )
     #'(if accept?
           (check-accept C M first-pairs ...)
           (check-reject C M first-pairs ...))]
    [(_ accept?:boolean C:id M (~or (~var valids valid-pair)
                                    (~var invalids invalid-pair)) ...)
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
                  #t)
     ]))