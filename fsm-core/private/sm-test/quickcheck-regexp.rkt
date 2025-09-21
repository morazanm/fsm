#lang fsm
(require rackcheck
         racket/list)

(define (gen:kleene-star-regexp gen)
  (gen:list gen))

(define (gen:union-regexp gen0 gen1)
  (gen:choice gen0 gen1))

(define (gen:const-regexp const)
  (gen:const (string->symbol const)))

(define (gen:concat-regexp gen0 gen1)
  (gen:tuple gen0 gen1))

;; need to remember to flatten output
(define (gen:fsm-regexp fsm-regexp)
  (cond [(singleton-regexp? fsm-regexp)
         (gen:const-regexp (singleton-regexp-a fsm-regexp))]
        [(union-regexp? fsm-regexp)
         (gen:union-regexp (gen:fsm-regexp (union-regexp-r1 fsm-regexp))
                           (gen:fsm-regexp (union-regexp-r2 fsm-regexp)))]
        [(concat-regexp? fsm-regexp)
         (gen:concat-regexp (gen:fsm-regexp (concat-regexp-r1 fsm-regexp))
                            (gen:fsm-regexp (concat-regexp-r2 fsm-regexp)))]
        [(kleenestar-regexp? fsm-regexp)
         (gen:kleene-star-regexp (gen:fsm-regexp (kleenestar-regexp-r1 fsm-regexp)))]
        [else (error "how did you get here")]))

(define A (singleton-regexp "a"))
(define B (singleton-regexp "b"))
(define AUB (union-regexp A B))
(define AUB* (kleenestar-regexp AUB))
(define ENDS-WITH-A
 (concat-regexp AUB* A))

(map flatten (sample (gen:fsm-regexp ENDS-WITH-A)))