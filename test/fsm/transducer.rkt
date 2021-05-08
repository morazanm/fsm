#lang racket
(require "../../transducer.rkt" "../test-helpers.rkt")

(define a-trans (make-dfst '(A B C)
                             '(a b c)
                             'A
                             '(B C)
                             (list '(A a B pie)
                                   '(B a A bye))))

(module+ test 
  (require rackunit rackunit/text-ui)

  (define transducer-test
    (test-suite "Basic Transducer test"
                (test-case "Marco tests"
                           (check-equal? ((a-trans 'apply) '(b b)) 'reject)
                           (check-equal? ((a-trans 'apply) '(a)) '(pie))
                           (check-equal? ((a-trans 'apply) '(a a)) 'reject)
                           (check-equal? ((a-trans 'apply) '(a a a))  '(pie bye pie))
                           (check-equal? ((a-trans 'apply) '()) 'reject)
                           (check-equal? ((a-trans 'transitions) '(a a))  '((()) ((pie) (a) B) ((bye pie) () A) reject))
                           (check-equal? ((a-trans 'transitions) '(b b)) '((()) reject))
                           (check-equal? ((a-trans 'transitions) '(a)) '((()) ((pie) () B) accept))
                           (check-equal? ((a-trans 'transitions) '(a a a)) '((()) ((pie) (a a) B) ((bye pie) (a) A) ((pie bye pie) () B) accept))
                           (check-equal? ((a-trans 'transitions) '()) '((()) reject)))))


  (test-all 'verbose
            (transducer-test))
  ) ;; end sub-module





;(define test-6 (equal? (a-trans 'a) "error"))