#lang fsm

(require "transformations.rkt")

(define anbn (make-cfg '(S)
                       '(a b)
                       `((S ,ARROW ,EMP)
                         (S ,ARROW aSb))
                       'S))

(check-equal? (grammar-derive anbn '()) "The word () is too short to test.")
(check-equal? (last (grammar-derive anbn '(a a a b b b))) 'aaabbb)
(check-equal? (last (grammar-derive anbn '(a b))) 'ab)
(check-equal? (last (grammar-derive anbn '(a a a a a a b b b b b b))) 'aaaaaabbbbbb)

(define anbn-chomsky (chomsky anbn))

(check-equal? (grammar-derive anbn-chomsky '()) "The word () is too short to test.")
(check-equal? (last (grammar-derive anbn-chomsky '(a a a b b b))) 'aaabbb)
(check-equal? (last (grammar-derive anbn-chomsky '(a b))) 'ab)
(check-equal? (last (grammar-derive anbn-chomsky '(a a a a a a b b b b b b))) 'aaaaaabbbbbb)

(define anbn-greibach (greibach anbn))

(check-equal? (grammar-derive anbn-greibach '()) "The word () is too short to test.")
(check-equal? (last (grammar-derive anbn-greibach '(a a a b b b))) 'aaabbb)
(check-equal? (last (grammar-derive anbn-greibach '(a b))) 'ab)
(check-equal? (last (grammar-derive anbn-greibach '(a a a a a a b b b b b b))) 'aaaaaabbbbbb) 
