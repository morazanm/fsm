#lang racket

(require "chomsky.rkt" "greibach.rkt"
         #;"../../interface.rkt" "../cfg.rkt" "../constants.rkt" rackunit)

(define anbn (make-unchecked-cfg '(S)
                                 '(a b)
                                 `((S ,ARROW ,EMP)
                                   (S ,ARROW aSb))
                                 'S))

(check-equal? (cfg-derive anbn '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbn '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbn '(a b))) 'ab)
(check-equal? (last (cfg-derive anbn '(a a a a a a b b b b b b))) 'aaaaaabbbbbb)

(define anbn-chomsky (chomsky anbn))

(check-equal? (cfg-derive anbn-chomsky '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbn-chomsky '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbn-chomsky '(a b))) 'ab)
(check-equal? (last (cfg-derive anbn-chomsky '(a a a a a a b b b b b b))) 'aaaaaabbbbbb)

(define anbn-greibach (greibach anbn))

(check-equal? (cfg-derive anbn-greibach '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbn-greibach '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbn-greibach '(a b))) 'ab)
(check-equal? (last (cfg-derive anbn-greibach '(a a a a a a b b b b b b))) 'aaaaaabbbbbb)

;;; a^nb^nc^md^me^jf^j

(define anbncmdmejfj (make-unchecked-cfg '(S A B C)
                                         '(a b c d e f)
                                         `((S ,ARROW ABC)
                                           (A ,ARROW ,EMP)
                                           (A ,ARROW aAb)
                                           (B ,ARROW ,EMP)
                                           (B ,ARROW cBd)
                                           (C ,ARROW ,EMP)
                                           (C ,ARROW eCf))                                 
                                         'S))

(check-equal? (cfg-derive anbncmdmejfj '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbncmdmejfj '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbncmdmejfj '(a b c d e f))) 'abcdef)
(check-equal? (last (cfg-derive anbncmdmejfj '(c c d d e f))) 'ccddef)
(check-equal?
 (last (cfg-derive anbncmdmejfj '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)


(define anbncmdmejfj-chomsky (chomsky anbncmdmejfj))

(check-equal? (cfg-derive anbncmdmejfj-chomsky '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbncmdmejfj-chomsky '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbncmdmejfj-chomsky '(a b c d e f))) 'abcdef)
(check-equal? (last (cfg-derive anbncmdmejfj-chomsky '(c c d d e f))) 'ccddef)
(check-equal?
 (last (cfg-derive anbncmdmejfj-chomsky '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)

(define anbncmdmejfj-greibach (greibach anbncmdmejfj))

(check-equal? (cfg-derive anbncmdmejfj-greibach '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbncmdmejfj-greibach '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbncmdmejfj-greibach '(a b c d e f))) 'abcdef)
(check-equal? (last (cfg-derive anbncmdmejfj-greibach '(c c d d e f))) 'ccddef)
(check-equal?
 (last (cfg-derive anbncmdmejfj-greibach '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)



;; even b odd a

(define even-bs-odd-as (make-unchecked-cfg '(S A B C)
                                           '(a b)
                                           `((S ,ARROW aA)
                                             (S ,ARROW bB)
                                             (S ,ARROW a)
                                             (A ,ARROW aS)
                                             (A ,ARROW bC)
                                             (B ,ARROW aC)
                                             (B ,ARROW bS)
                                             (C ,ARROW aB)
                                             (C ,ARROW bA)
                                             (C ,ARROW b))
                                           'S))

(check-equal? (last (cfg-derive even-bs-odd-as '(a a a b b b b)))
              'aaabbbb)
(check-equal? (last (cfg-derive even-bs-odd-as '(b b b b b a a a b)))
              'bbbbbaaab)
(check-equal? (last (cfg-derive even-bs-odd-as '(a a b b b a b b a a b)))
              'aabbbabbaab)

(define even-bs-odd-as-chomsky (chomsky even-bs-odd-as))

(check-equal? (last (cfg-derive even-bs-odd-as-chomsky '(a a a b b b b)))
              'aaabbbb)
(check-equal? (last (cfg-derive even-bs-odd-as-chomsky '(b b b b b a a a b)))
              'bbbbbaaab)
(check-equal? (last (cfg-derive even-bs-odd-as-chomsky '(a a b b b a b b a a b)))
              'aabbbabbaab)

(define even-bs-odd-as-greibach (greibach even-bs-odd-as))

(check-equal? (last (cfg-derive even-bs-odd-as-greibach '(a a a b b b b)))
              'aaabbbb)
(check-equal? (last (cfg-derive even-bs-odd-as-greibach '(b b b b b a a a b)))
              'bbbbbaaab)
(check-equal? (last (cfg-derive even-bs-odd-as-greibach '(a a b b b a b b a a b)))
              'aabbbabbaab)

;;; anbncmdmejfj again

(define anbncmdmejfj-chomsky2 (chomsky anbncmdmejfj-greibach))

(check-equal? (cfg-derive anbncmdmejfj-chomsky2 '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbncmdmejfj-chomsky2 '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbncmdmejfj-chomsky2 '(a b c d e f))) 'abcdef)
(check-equal? (last (cfg-derive anbncmdmejfj-chomsky2 '(c c d d e f))) 'ccddef)
(check-equal?
 (last (cfg-derive anbncmdmejfj-chomsky2 '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)


(define anbncmdmejfj-greibach2 (greibach anbncmdmejfj-chomsky2))

(check-equal? (cfg-derive anbncmdmejfj-greibach2 '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbncmdmejfj-greibach2 '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbncmdmejfj-greibach2 '(a b c d e f))) 'abcdef)
(check-equal? (last (cfg-derive anbncmdmejfj-greibach2 '(c c d d e f))) 'ccddef)
(check-equal?
 (last (cfg-derive anbncmdmejfj-greibach2 '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)


(define anbncmdmejfj-greibach3 (greibach anbncmdmejfj-greibach))

(check-equal? (cfg-derive anbncmdmejfj-greibach3 '()) "The word () is too short to test.")
(check-equal? (last (cfg-derive anbncmdmejfj-greibach3 '(a a a b b b))) 'aaabbb)
(check-equal? (last (cfg-derive anbncmdmejfj-greibach3 '(a b c d e f))) 'abcdef)
(check-equal? (last (cfg-derive anbncmdmejfj-greibach3 '(c c d d e f))) 'ccddef)
(check-equal?
 (last (cfg-derive anbncmdmejfj-greibach3 '(a a a a b b b b c c c c d d d d e e e e f f f f)))
 'aaaabbbbccccddddeeeeffff)

