#lang racket/base

(require "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt")

(provide (all-defined-out))

;; Any -> Boolean
;; Purpose: Checks if m is a machine (not tm)
(define (is-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'ndfa m-type)
        (eq? 'dfa m-type)
        (eq? 'pda m-type))))

;; Any -> Boolean
;; Purpose: Checks if m is a turing machine
(define (is-turing-machine? m)
  (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (m 'whatami 0 'whatami))])
    (or (eq? 'tm m-type)
        (eq? 'tm-language-recognizer m-type)
        (eq? 'mttm m-type)
        (eq? 'mttm-language-recognizer m-type))))


;; Any -> Boolean
;; Purpose: Checks is g is a grammar
(define (is-grammar? g)
  (or (rg? g)
      (cfg? g)
      (csg? g)))

(define (whatami? unknown-val)
  (cond [(or (is-turing-machine? unknown-val)
             (is-machine? unknown-val)) 'machine]
        [(is-grammar? unknown-val) 'grammar]
        [else 'notanfsmval]))
