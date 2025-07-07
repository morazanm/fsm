#lang fsm
(require rackcheck "reg-exp-function.rkt" "testing-file-for-sm-test-invs.rkt")

(define (translate-regexp regexp)
  (cond [(null-regexp? (gen:list (gen:symbol (gen:const (first (string->list "∅"))) #:max-length 1) #:max-length 1))]
        [(singleton-regexp? regexp) (gen:list (gen:symbol (gen:const (first (string->list (symbol->string (singleton-regexp-a regexp))))) #:max-length 1) 1)]
        [(concat-regexp? regexp) (gen:tuple (translate-regexp (concat-regexp-r1 regexp)) (translate-regexp (concat-regexp-r2 regexp)))]
        [(union-regexp? regexp) (gen:choice (translate-regexp (union-regexp-r1 regexp)) (translate-regexp (union-regexp-r2 regexp)))]
        [(kleenestar-regexp? regexp) (gen:bind gen:natural (λ (x) (gen:list (translate-regexp regexp) #:max-length x)))]
        [(empty-regexp? regexp) (gen:const '())]))


(define (testing-function regexp inv)
  (let [(machine (regexp->fsa regexp))]
    (check-property (property ([translated-regexp (translate-regexp regexp)])
                              (let [(flattened-translated-regexp (flatten translated-regexp))]
                                (check-equal? (inv flattened-translated-regexp) (sm-apply machine flattened-translated-regexp)))))))

(define (quickcheck-invs machine los&regexp loi)
  (define state2inv (make-hash))
  (for ([inv (in-list loi)])
    (hash-set! state2inv (first inv) (second inv)))

  (for ([regexp (in-list los&regexp)])
    (testing-function (second regexp) (hash-ref state2inv (first regexp)))))

                             
