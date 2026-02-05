#lang fsm
;; State Documentation
;; S: none of the pattern detected, starting state
;; A: a detected and no other prefix detected 
;; B: aa detected and no other prefix detected 
;; C: aab detected and no other prefix detected 
;; D: aaba detected and no other prefix detected 
;; E: pattern detected, final state
;; L = {w|w contains aabab}
(define CONTAINS-aabab (make-dfa
				'(S A B C D E)
				'(a b)
				'S
				'(E)
				'((S a A) (C b S) (S b S) (A b S) (B a B) (B b C) (C a D) (A a B) 
				 (D a A) (D b E) (E a E) (E b E))))

(check-accept? CONTAINS-aabab '(a a b a b) '(b b a a a b a b b) '(a a a b a b b) '(a b a b a a b a b b a))
(check-reject? CONTAINS-aabab '() '(a) '(a a) '(a a b)  '(a a b a) '(a a b a a a a b)  '(b b a a a b a) '(a a b a a))
