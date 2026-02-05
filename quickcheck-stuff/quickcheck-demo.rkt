#lang fsm
(require rackcheck)

(define MAX-LIST-LENGTH 100)
(define MIN-LIST-LENGTH 0)

;; machine invariant-predicate -> (Void)
;; Testing a machine with quickcheck
(define (test-machine m inv)
  (define valid-word-gen (let* ([valid-alphabet (gen:one-of (sm-sigma m))]
                                [rand (gen:integer-in MIN-LIST-LENGTH MAX-LIST-LENGTH)]
                                [valid-word (gen:bind rand (lambda (x) (gen:list valid-alphabet #:max-length x)))])
                           valid-word))
  (define test-prop
    (property ([word valid-word-gen])
              (equal? (inv word) (eq? (sm-apply m word) 'accept))))
  (check-property test-prop))

(define AT-LEAST-ONE-MISSING
  (make-ndfa '(S A B C)
             '(a b c)
             'S
             '(A B C)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                          (A b A) (A c A) (A a A)
                          (B a B) (B c B)
                          (C a C) (C b C))))

(define (AT-LEAST-ONE-MISSING-invariant word)
  (or (= (length (filter (lambda (x) (eq? x 'a)) word)) 0)
      (= (length (filter (lambda (x) (eq? x 'b)) word)) 0)
      (= (length (filter (lambda (x) (eq? x 'c)) word)) 0)))

(test-machine AT-LEAST-ONE-MISSING AT-LEAST-ONE-MISSING-invariant)