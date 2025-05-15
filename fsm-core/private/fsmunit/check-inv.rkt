#lang racket/base

(require
  (for-syntax syntax/parse
              racket/base)
  racket/list
  "check-utils.rkt")


(define-syntax (check-inv-holds? stx)
  (syntax-parse stx
    [(_ inv words ...)
     #'(let ([word-vals (list words ...)]
             [word-stxs (list #'words ...)])
         (check-syntax (list (fsm-error (macro-contract inv) #'inv 'inv-holds)
                             word-vals
                             word-stxs)))]))

(define-syntax (check-inv-fails? stx)
  (syntax-parse stx
    [(_ inv words ...)
     #'(let ([word-vals (list words ...)]
             [word-stxs (list #'words ...)])
         (check-syntax (list (fsm-error (macro-contract (lambda (x) (not (inv x)))) #'inv 'inv-fails)
                             word-vals
                             word-stxs)))]))

;; L = a(ab)*aa+
(define (A-INV ci)
  (and (eq? (first ci) 'a)
       (even? (length (rest ci)))
       (andmap (λ (p) (equal? p '(a b)))
               (foldl (λ (s acc)
                        (if (or (empty? acc) (= (length (first acc)) 2))
                            (cons (list s) acc)
                            (cons (list (first (first acc)) s) (rest acc))))
                      '()
                      (rest ci)))))

(check-inv-holds? A-INV '(a a a) '(a a b a b) '(a b a a))
(check-inv-fails? A-INV '(b b) '(a a b) '(b a))