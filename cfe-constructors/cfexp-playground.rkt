#lang racket

(require  "../fsm-core/private/constants.rkt"
          "context-free-expressions-constructors.rkt")

(define E (empty-cfexp))

(define A (singleton-cfexp 'a))

(define B (singleton-cfexp 'b))

(define OLD-ANBN (let [(S-ENV void)
                   (S void)]
               (begin
               (set! S-ENV (empty-cfexp-env))
               (set! S (var-cfexp 'S S-ENV))
               (set! S-ENV (env-cfexp 'S (union-cfexp E (concat-cfexp (list A S B)))))
               (set-var-cfexp-env! S S-ENV)
               (set! S (var-cfexp 'S S-ENV))
               S)))

(define S (var-cfexp 'S (empty-cfexp-env)))

(define ASB (concat-cfexp (list A S B)))

(define EUASB (union-cfexp E (concat-cfexp (list A S B))))

;;w = a^nb^n
(define ANBN (make-varcfexp-binding S EUASB))

(define K (var-cfexp 'K (empty-cfexp-env)))

(define EUAAKB (union-cfexp E (concat-cfexp A A K B)))

;;w = a^2ib^i
(define A2iBi (make-varcfexp-binding K EUAAKB))

(define H (var-cfexp 'H (empty-cfexp-env)))

(define AHA (concat-cfexp A H A))

(define BHB (concat-cfexp B H B))

(define EUAHAUBHB (union-cfexp E AHA BHB))

;; w = ww^r
(define WWR (make-varcfexp-binding H EUAHAUBHB))

(define I (var-cfexp 'I (empty-cfexp-env)))

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