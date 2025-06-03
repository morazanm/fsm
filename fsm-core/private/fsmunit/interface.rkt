#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         "fsm-error-types/broken-unit-tests-error-types.rkt"
         "fsm-type-predicates.rkt"
         "unit-tests/check-turing-machine.rkt"
         "unit-tests/check-machine.rkt"
         "unit-tests/check-grammar.rkt"
         "unit-tests/check-inv.rkt"
         "unit-tests/check-regexp.rkt"
         "syntax-value-struct.rkt"
         "fsm-error-types/base-fsm-error-types.rkt")

(provide check-in-lang? check-not-in-lang?
         check-derive? check-not-derive?
         check-gen? check-not-gen?
         check-accept? check-reject?
         check-inv-holds? check-inv-fails?)

(define (handle-one-val fsm-val stx)
  (if (eq? 'notanfsmval (whatami? fsm-val))
      (display-fsm-err (warn:fsm:test:no-fsm-val stx))
      (display-fsm-err (warn:fsm:test:no-cases stx))))

(define (handle-no-vals stx)
  (display-fsm-err (warn:fsm:test:no-vals stx)))

(define (handle-std-case accept? fsm-val-stx-pair word-lst-val-stx-pairs)
  (cond [(eq? (whatami? (val-stx-pair-val fsm-val-stx-pair)) 'turing-machine)
         (check-turing-machine accept? fsm-val-stx-pair word-lst-val-stx-pairs)]
        [(eq? (whatami? (val-stx-pair-val fsm-val-stx-pair)) 'machine)
         (check-machine accept? fsm-val-stx-pair word-lst-val-stx-pairs)]
        [(eq? (whatami? (val-stx-pair-val fsm-val-stx-pair)) 'grammar)
         (check-grammar accept? fsm-val-stx-pair word-lst-val-stx-pairs)]
        [(eq? (whatami? (val-stx-pair-val fsm-val-stx-pair)) 'regexp)
         (check-regexp accept? fsm-val-stx-pair word-lst-val-stx-pairs)]
        [(eq? (whatami? (val-stx-pair-val fsm-val-stx-pair)) 'inv)
         (check-invariant accept? fsm-val-stx-pair word-lst-val-stx-pairs)]
        [else (display-fsm-err (warn:fsm:test:invalid-fsm-val fsm-val-stx-pair))]))

;; FSM-Val Word ... -> (void)
;; Purpose: To determine whether a given machine/grammar can accept/process a given word
(define-syntax (check-in-lang? stx)
  (syntax-parse stx
    [(_)
     #'(handle-no-vals (val-stx-pair stx #'stx))]
    [(_ fsm-val)
     #'(handle-one-val fsm-val (val-stx-pair stx #'stx))]
    [(_ fsm-val word ...)
     #'(handle-std-case #t (val-stx-pair fsm-val #'fsm-val) (list (val-stx-pair word #'word) ...))]))

;; FSM-Val Word ... -> (void)
;; Purpose: To determine whether a given machine/grammar can reject/not process a given word
(define-syntax (check-not-in-lang? stx)
  (syntax-parse stx
    [(_)
     #'(handle-no-vals (val-stx-pair stx #'stx))]
    [(_ fsm-val)
     #'(handle-one-val fsm-val (val-stx-pair stx #'stx))]
    [(_ fsm-val word ...)
     #'(handle-std-case #f (val-stx-pair fsm-val #'fsm-val) (list (val-stx-pair word #'word) ...))]))

(define-syntax check-derive? (make-rename-transformer #'check-in-lang?))
(define-syntax check-gen? (make-rename-transformer #'check-in-lang?))
(define-syntax check-accept? (make-rename-transformer #'check-in-lang?))
(define-syntax check-inv-holds? (make-rename-transformer #'check-in-lang?))

(define-syntax check-not-derive? (make-rename-transformer #'check-not-in-lang?))
(define-syntax check-not-gen? (make-rename-transformer #'check-not-in-lang?))
(define-syntax check-reject? (make-rename-transformer #'check-not-in-lang?))
(define-syntax check-inv-fails? (make-rename-transformer #'check-not-in-lang?))
