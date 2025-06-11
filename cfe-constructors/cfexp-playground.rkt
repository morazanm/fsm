#lang racket

(require  "../fsm-core/private/constants.rkt"
          "context-free-expressions-constructors.rkt")

(define A (singleton-cfexp "a"))
(define B (singleton-cfexp "b"))
(define emp (empty-cfexp))

(define ASB (add2env (add2env (concat-cfexp A B) (list "S" "aSb")) (list "S" EMP)))

(define S (add2env (add2env (singleton-cfexp "aSb") (list "S" "aSb")) (list "S" (symbol->string EMP))))

(define anbn (kleene-cfexp S))

(define a^nb^n (kleene-cfexp (add2env (add2env (singleton-cfexp "S") (list "S" "aSb")) (list "S" (symbol->string EMP)))))

(define anbn (concat-cfg (list A S B) (add2env 'S '((concat-cfg (list A S B)) emp))) '()))

(define anbn-pre-expression (concat-pre-cfg (list A S B)))

(define anbn (concat-cfg anbn-pre-expression (add2env 'S '(anbn-pre-expression emp))))

;(define anbn (union-cfexp (concat-cfexp (list A S B) ) EMP))

(define E (empty-cfexp (empty-cfexp-env)))

(define A (singleton-cfexp 'a (empty-cfexp-env)))

(define B (singleton-cfexp 'b (empty-cfexp-env)))

(define ANBN (begin
               (set! S-ENV (empty-cfexp-env))
               (set! S null)
               (set! S-ENV (env-cfexp 'S (union-cfexp E (concat-cfexp (list A S B) S-ENV) S-ENV)))
               (set! S (var-cfexp 'S (env-cfexp 'S S-ENV)))
               S))