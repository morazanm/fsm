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



;; check-accept
(define-check (check-accept? M w line column name)
  (unless (equal? (sm-apply M w) 'accept)
    (with-check-info*
        (list (make-check-name 'check-accept)
              (make-check-location (list 'check-accept line column #f #f)))
      (lambda () (begin
                   (fail-check (format "~s does not accept ~s." name w))
                   #f
                   ))
      ))
  )

;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can accept/process a given word
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M [word header-pos:nat]...)
     #`(let* ([word-lst (list #'word ...)]
              [header-pos-lst (list header-pos ...)]
              [res (foldr (lambda (val head accum)
                           (if (equal? (sm-apply M (second (syntax->datum val)) head) 'accept)
                               accum
                               (cons val accum)
                               )                  
                           )
                         '()
                         word-lst
                         header-pos-lst
                         )])
         (unless (empty? res)
           (raise (exn:fail:check-failed
                   (if (= (length word-lst) 1)
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
            
    #;[(_ M w n)
       #'(check-accept? M w n)]))

;;

;; check-accept
#;(define-check (check-reject? M w line column)
    (unless (equal? (sm-apply M w) 'reject)
      (with-check-info*
          (list (make-check-name 'check-reject)
                (make-check-location (list 'check-reject line column #f #f))
                (make-check-params (list M w))
                (make-check-message (format "The machine does not reject ~s" w)))
        (lambda () (fail-check)))
      ))


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can reject a given word
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M w)
     #`(unless (equal? (sm-apply M w) 'reject)
         (raise (exn:fail:check-failed
                 (format "The machine does not reject ~s" w)
                 (current-continuation-marks)
                 (srcloc '#,(syntax-source stx)
                         '#,(syntax-line stx)
                         '#,(syntax-column stx)
                         '#,(syntax-position stx)
                         '#,(syntax-span stx)))))]
    #;[(_ M w n)
       #'(check-reject? M w n)]))


