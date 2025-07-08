#lang racket/base
(require rackcheck
         rackunit
         racket/list
         "reg-exp-function.rkt"
         "testing-file-for-sm-test-invs.rkt"
         "../fsm-core/private/regexp.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-apply.rkt")

(define MAX-KLEENE-LENGTH 20)
(define (translate-regexp regexp)
  (cond [(null-regexp? regexp)
         (gen:const '∅)]
        [(singleton-regexp? regexp)
         (gen:const (string->symbol (singleton-regexp-a regexp)))]
        [(concat-regexp? regexp)
         (gen:tuple (translate-regexp (concat-regexp-r1 regexp))
                    (translate-regexp (concat-regexp-r2 regexp)))]
        [(union-regexp? regexp)
         (gen:choice (translate-regexp (union-regexp-r1 regexp))
                     (translate-regexp (union-regexp-r2 regexp)))]
        [(kleenestar-regexp? regexp)
         (gen:bind (gen:integer-in 0 MAX-KLEENE-LENGTH)
                   (λ (x) (gen:list (translate-regexp (kleenestar-regexp-r1 regexp))
                                    #:max-length x)))]
        [(empty-regexp? regexp)
         (gen:const '())]))

(define (testing-function state regexp inv tests)
  (let [(machine (regexp->fsa regexp))]
    (check-property
     (make-config #:tests tests)
     (property #:name (format "~a-invariant" state)
               ([translated-regexp (gen:map (translate-regexp regexp) flatten)])
               (check-equal? (inv translated-regexp)
                             (eq? (sm-apply machine translated-regexp)
                                  'accept))))))

(define (quickcheck-invs machine loi #:num-tests [tests 1000])
  (define state2inv (make-hash))
  (define los&regexp (get-all-reg-expr machine))
  (for ([inv (in-list loi)])
    (hash-set! state2inv (first inv) (second inv)))
  (for ([regexp (in-list los&regexp)])
    (testing-function (first regexp) (second regexp) (hash-ref state2inv (first regexp)) tests)))