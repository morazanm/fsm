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

;;; a^nb^nc^md^me^jf^j

(define anbncmdmejfj (make-cfg '(S A B C)
                               '(a b c d e f)
                               `((S ,ARROW ABC)
                                 (A ,ARROW ,EMP)
                                 (A ,ARROW aAb)
                                 (B ,ARROW ,EMP)
                                 (B ,ARROW cBd)
                                 (C ,ARROW ,EMP)
                                 (C ,ARROW eCf))                                 
                               'S))

(check-equal? (grammar-derive anbncmdmejfj '()) "The word () is too short to test.")
(check-equal? (last (grammar-derive anbncmdmejfj '(a a a b b b))) 'aaabbb)
(check-equal? (last (grammar-derive anbncmdmejfj '(a b c d e f))) 'abcdef)
(check-equal? (last (grammar-derive anbncmdmejfj '(c c d d e f))) 'ccddef)
(check-equal?
 (last (grammar-derive anbncmdmejfj '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)


(define anbncmdmejfj-chomsky (chomsky anbncmdmejfj))

(check-equal? (grammar-derive anbncmdmejfj-chomsky '()) "The word () is too short to test.")
(check-equal? (last (grammar-derive anbncmdmejfj-chomsky '(a a a b b b))) 'aaabbb)
(check-equal? (last (grammar-derive anbncmdmejfj-chomsky '(a b c d e f))) 'abcdef)
(check-equal? (last (grammar-derive anbncmdmejfj-chomsky '(c c d d e f))) 'ccddef)
(check-equal?
 (last (grammar-derive anbncmdmejfj-chomsky '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)

(define anbncmdmejfj-greibach (greibach anbncmdmejfj))

(check-equal? (grammar-derive anbncmdmejfj-greibach '()) "The word () is too short to test.")
(check-equal? (last (grammar-derive anbncmdmejfj-greibach '(a a a b b b))) 'aaabbb)
(check-equal? (last (grammar-derive anbncmdmejfj-greibach '(a b c d e f))) 'abcdef)
(check-equal? (last (grammar-derive anbncmdmejfj-greibach '(c c d d e f))) 'ccddef)
(check-equal?
 (last (grammar-derive anbncmdmejfj-greibach '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)
