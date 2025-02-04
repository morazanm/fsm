#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         racket/syntax-srcloc
         "../sm-apply.rkt"
         racket/list
         "check-exn.rkt"
         "check-utils.rkt"
         "check-accept-reject-failure-strings.rkt")

(provide check-machine)

;; machine machine-syntax (Listof word-expression) (Listof word-expression-syntax) (Listof head-position-expression)
;; Checks if the machine accepts the words inside of the test cases
(define (accept M M-stx unprocessed-word-lst unprocessed-word-stx-lst)
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
                                                (map syntax-srcloc word-stx-lst)))))))

;; machine machine-syntax (Listof word-expression) (Listof word-expression-syntax) (Listof head-position-expression)
;; Checks if the machine rejects the words inside of the test cases
(define (reject M M-stx unprocessed-word-lst unprocessed-word-stx-lst)
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
                                                       (map syntax-srcloc word-stx-lst)))))))

;; Boolean flat-contract machine machine-syntax (Listof word) (Listof word-syntax)
;; Checks the validity of the arguments given
(define (check-validity-of-arguments accept? C M M-stx unprocessed-word-lst unprocessed-word-stx-lst)
  (let* ([invalids (accumulate-invalid-words C unprocessed-word-lst unprocessed-word-stx-lst)]
                [invalid-words (map first invalids)]
                [invalid-word-stx (map second invalids)])
           (if (empty? invalids)
               (if accept?
                   (accept M M-stx unprocessed-word-lst unprocessed-word-stx-lst)
                   (reject M M-stx unprocessed-word-lst unprocessed-word-stx-lst))
               (let ([failure-str (create-failure-str machine-invalid-nonterminal M-stx invalid-words)])
                 (display-failed-test failure-str (exn:fail:check-failed
                                                   failure-str
                                                   (current-continuation-marks)
                                                   (map syntax-srcloc invalid-word-stx)))))))

;; Syntax -> Syntax
;; Matches incorrect syntatic forms and provides specialized errors messages based on them
(define-syntax (check-machine stx)
  (syntax-parse stx
    [(_ accept? C:id M (~var x) ...)
     #'(check-validity-of-arguments accept? C M #'M (list x ...) (list #'x ...))]))
