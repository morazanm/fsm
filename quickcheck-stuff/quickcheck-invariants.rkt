#lang racket/base
(require rackcheck
         rackunit
         racket/list
         "reg-exp-function.rkt"
         "../fsm-core/private/regexp.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-apply.rkt")
(provide quickcheck-invs)

(define MAX-KLEENE-LENGTH 20)

;; regexp -> generator throw error
;; Purpose: To translate the given regexp into a generator, that generates
;;          words that are in the language in the regular expression
(define (translate-regexp regexp)
  (cond [(equal? null-regexp regexp)
         (error "cannot generate a word for the null-regexp")]
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

;; state regexp invariant natnum -> void throw error
;; Purpose: To test if a word generated from the given regexp holds for the invariant of the state
(define (testing-function state regexp inv tests)
  (let [(machine (regexp->fsa regexp))]
    (check-property
     (make-config #:tests tests)
     (property #:name (format "~a-invariant" state)
               ([generated-word (gen:map (translate-regexp regexp) flatten)])
               (inv generated-word)))))


;; machine (listof (state invariant)) natnum -> void throw error 
;; Purpose: To quickcheck the invariants of the states of the given machine
(define (quickcheck-invs machine los&inv #:num-tests [tests 1000])
  (define los&regexp (get-all-regexp machine))
  (define los&inv-only-states-that-reach-finals
    (filter (λ (s&inv) (hash-has-key? los&regexp (first s&inv))) los&inv))
  (for ([inv (in-list
                 los&inv-only-states-that-reach-finals)])
    (testing-function (first inv) (hash-ref los&regexp (first inv)) (second inv) tests)))

