#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         racket/syntax-srcloc
         "../../fsm-core/private/sm-apply.rkt"
         racket/list
         "check-exn.rkt"
         "check-utils.rkt"
         "check-accept-reject-failure-strings.rkt")

(provide check-machine)

(define (accept C M M-stx unprocessed-word-lst unprocessed-word-stx-lst)
  (let* ([invalids (accumulate-invalid-words C unprocessed-word-lst unprocessed-word-stx-lst)]
         [invalid-words (map first invalids)]
         [invalid-word-stx (map second invalids)])
    (if (empty? invalids)
        (let* ([res (foldr (lambda (word wordstx accum)
                             (if (equal? (sm-apply M word) 'accept)
                                 accum
                                 (cons (list word wordstx) accum)))
                           '()
                           unprocessed-word-lst
                           unprocessed-word-stx-lst)]
               [word-lst (map first res)]
               [word-stx-lst (map second res)])
          (unless (empty? res)
            (let ([failure-str (create-failure-str machine-accept M-stx word-lst)])
              (display-failed-test failure-str (exn:fail:check-failed
                                                failure-str
                                                (current-continuation-marks)
                                                (map syntax-srcloc word-stx-lst))))))
        (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
          (display-failed-test failure-str (exn:fail:check-failed
                                            failure-str
                                            (current-continuation-marks)
                                            (map syntax-srcloc invalid-word-stx)))))))

;; Syntax -> Syntax
;; Purpose: Checks state machines (without turing)
(define-syntax (check-accept stx)
  (syntax-parse stx 
    [(_ C:id M ((~var x) ...))
     #`(accept C M #'M (list x ...) (list #'x ...))]))

(define (reject C M M-stx unprocessed-word-lst unprocessed-word-stx-lst)
  (let* ([invalids (accumulate-invalid-words C unprocessed-word-lst unprocessed-word-stx-lst)]
                [invalid-words (map first invalids)]
                [invalid-word-stx (map second invalids)])
           (if (empty? invalids)
               (let* ([res (foldr (lambda (word wordstx accum)
                                    (if (equal? (sm-apply M word) 'reject)
                                        accum
                                        (cons (list word wordstx) accum)))
                                  '()
                                  unprocessed-word-lst
                                  unprocessed-word-stx-lst)]
                      [word-lst (map first res)]
                      [word-stx-lst (map second res)])
                 (unless (empty? res)
                   (let ([failure-str (create-failure-str machine-reject M-stx word-lst)])
                     (display-failed-test failure-str (exn:fail:check-failed
                                                       failure-str
                                                       (current-continuation-marks)
                                                       (map syntax-srcloc word-stx-lst))))))
               (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
                 (display-failed-test failure-str (exn:fail:check-failed
                                                   failure-str
                                                   (current-continuation-marks)
                                                   (map syntax-srcloc invalid-word-stx)))))))

(define-syntax (check-reject stx)
  (syntax-parse stx 
    [(_ C:id M ((~var x) ...))
     #`(reject C M #'M (list x ...) (list #'x ...))]))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-machine stx)
  #;(define-syntax-class
    not-a-nested-list
    (pattern (~not (w ...))))
  
  #;(define-syntax-class
    valid-word
    (pattern (quote ((~var w not-a-nested-list)...))))
  
  #;(define-syntax-class
    invalid-word
    (pattern (~not (quote ((~var w not-a-nested-list) ...)))))
  
  (syntax-parse stx
    [(_ accept?:boolean C:id M (~var x #;valid-word) ...)
     #`(if accept?
           (check-accept C M (x ...))
           (check-reject C M (x ...)))]
    #;[(_ accept?:boolean C:id M (~or (~var valids valid-word)
                                    (~var invalids invalid-word)) ...)
     #'(raise (exn:fail:check-failed
               (create-failure-str machine-invalid-expression M (list invalids ...))
               (current-continuation-marks)
               (map syntax-srcloc (list #'invalids ...)))
              #t)]))