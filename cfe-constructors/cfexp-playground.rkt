#lang racket

(require  "../fsm-core/private/constants.rkt"
          "context-free-expressions-constructors.rkt")

(define E (empty-cfexp))

(define A (singleton-cfexp 'a))

(define B (singleton-cfexp 'b))

(define S (var-cfexp (empty-cfexp-env) 'S))

(define ASB (concat-cfexp A S B))

(define EUASB (union-cfexp E (concat-cfexp A S B)))

;;w = a^nb^n
(define ANBN (make-varcfexp-binding S EUASB))

(define K (var-cfexp (empty-cfexp-env) 'K))

(define EUAAKB (union-cfexp E (concat-cfexp A A K B)))

;;w = a^2ib^i
(define A2iBi (make-varcfexp-binding K EUAAKB))

(define H (var-cfexp (empty-cfexp-env) 'H))

(define AHA (concat-cfexp A H A))

(define BHB (concat-cfexp B H B))

(define EUAHAUBHB (union-cfexp E AHA BHB))

;; w = ww^r
(define WWR (make-varcfexp-binding H EUAHAUBHB))

(define I (var-cfexp (empty-cfexp-env) 'I))

(define AIB (concat-cfexp A I B))

(define AIBB (concat-cfexp A I B B))

(define EUAIBUAIBB (union-cfexp E AIB AIBB))

;;w = A^iB^j
(define AiBj (make-varcfexp-binding I EUAIBUAIBB))



(define (loopinator cfe a-num)
  (define (loopinator-helper a-num)
    (if (= a-num 0)
        '()
        (cons (gen-cfexp-word cfe) (loopinator-helper (sub1 a-num)))))
  (remove-duplicates (loopinator-helper a-num)))