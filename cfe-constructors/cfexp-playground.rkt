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
    
(define ANBN (make-varcfexp-binding S EUASB))

(define (loopinator cfe a-num)
  (define (loopinator-helper a-num)
    (if (= a-num 0)
        '()
        (cons (gen-cfexp-word cfe) (loopinator-helper (sub1 a-num)))))
  (remove-duplicates (loopinator-helper a-num)))