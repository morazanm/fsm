#lang fsm
(require rackcheck "reg-exp-function.rkt" "testing-file-for-sm-test-invs.rkt")

(define (translate-regexp regexp)
  (cond [(null-regexp? regexp)
         (gen:list (gen:symbol (gen:const (first (string->list "∅"))) #:max-length 1) #:max-length 1)]
        [(singleton-regexp? regexp)
         (gen:list (gen:symbol (gen:const (first (string->list (singleton-regexp-a regexp)))) #:max-length 1) #:max-length 1)]
        [(concat-regexp? regexp)
         (gen:tuple (translate-regexp (concat-regexp-r1 regexp)) (translate-regexp (concat-regexp-r2 regexp)))]
        [(union-regexp? regexp)
         (gen:choice (translate-regexp (union-regexp-r1 regexp)) (translate-regexp (union-regexp-r2 regexp)))]
        [(kleenestar-regexp? regexp)
         (gen:bind (gen:integer-in 0 20) (λ (x) (gen:list (translate-regexp (kleenestar-regexp-r1 regexp)) #:max-length x)))]
        [(empty-regexp? regexp)
         (gen:const '())]))

(define (testing-function regexp inv)
  (let [(machine (regexp->fsa regexp))]
    (check-property (property ([translated-regexp (gen:map (translate-regexp regexp) flatten)])
                              (let [(flattened-translated-regexp (flatten translated-regexp))]
                                (check-equal? (inv flattened-translated-regexp) (eq? (sm-apply machine flattened-translated-regexp) 'accept)))))))

(define (quickcheck-invs machine los&regexp loi)
  (define state2inv (make-hash))
  (for ([inv (in-list loi)])
    (hash-set! state2inv (first inv) (second inv)))

  (for ([regexp (in-list los&regexp)])
    (testing-function (second regexp) (hash-ref state2inv (first regexp)))))

                             
