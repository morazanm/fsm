#lang racket
(require rackcheck
         rackunit
         racket/list
         "reg-exp-function.rkt"
         "new-new-regexp.rkt"
         "../fsm-core/private/regexp.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-apply.rkt")
(provide quickcheck-invs)

(define MAX-KLEENE-LENGTH 20)
(define NUM-QC-TESTS 300)

;; regexp -> generator throw error
;; Purpose: To translate the given regexp into a generator, that generates
;;          words that are in the language in the regular expression
(define (regexp->generator regexp)
  (cond [(equal? null-regexp regexp)
         (error "cannot generate a word for the null-regexp")]
        [(singleton-regexp? regexp)
         (gen:const (string->symbol (singleton-regexp-a regexp)))]
        [(concat-regexp? regexp)
         (gen:tuple (regexp->generator (concat-regexp-r1 regexp))
                    (regexp->generator (concat-regexp-r2 regexp)))]
        [(union-regexp? regexp)
         (gen:choice (regexp->generator (union-regexp-r1 regexp))
                     (regexp->generator (union-regexp-r2 regexp)))]
        [(kleenestar-regexp? regexp)
         (gen:bind (gen:integer-in 0 MAX-KLEENE-LENGTH)
                   (λ (x) (gen:list (regexp->generator (kleenestar-regexp-r1 regexp))
                                    #:max-length x)))]
        [(empty-regexp? regexp)
         (gen:const '())]))

;; state regexp invariant natnum -> void throw error
;; Purpose: To test if a word generated from the given regexp holds for the invariant of the state
(define (testing-function state generator inv tests)
  (check-property
     (make-config #:tests tests)
     (property #:name (format "~a-invariant" state)
               ([generated-word (gen:map generator flatten)])
               (inv generated-word))))


;; machine (listof (state invariant)) [natnum] -> void throws error 
;; Purpose: To quickcheck the invariants of the states of the given machine
(define (quickcheck-invs machine state-invs-pairs #:num-tests [tests NUM-QC-TESTS])
  (define state-regexp-pairs (get-all-regexp-new-new machine))
  (define non-dead-state-inv-pairs
    (filter (λ (state-inv-pair)
              (hash-has-key? state-regexp-pairs (first state-inv-pair)))
            state-invs-pairs))
  (for ([st-inv-pair (in-list non-dead-state-inv-pairs)])
    (testing-function (first st-inv-pair)
                      (regexp->generator (hash-ref state-regexp-pairs (first st-inv-pair)))
                      (second st-inv-pair)
                      tests)))

