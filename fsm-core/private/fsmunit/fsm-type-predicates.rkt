#lang racket/base

(require "../csg.rkt"
         "../cfg-struct.rkt"
         "../regular-grammar.rkt"
         "../regexp.rkt"
         "../cfe-constructors/cfexp-structs.rkt")

(provide parse-fsm-val-type)

(define (parse-fsm-val-type unknown-val)
  ;; Any -> Boolean
  ;; Purpose: Checks is g is a grammar
  (define (is-grammar? g)
    (or (rg? g)
        (cfg? g)
        (csg? g)))

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
  ;; Purpose: Checks if m is a machine (not tm)
  (define (is-machine? m)
    (let ([m-type (with-handlers ([exn:fail? (lambda (exn) #f)])
                    (m 'whatami 0 'whatami))])
      (or (eq? 'ndfa m-type)
          (eq? 'dfa m-type)
          (eq? 'pda m-type))))
  
  (cond [(is-turing-machine? unknown-val) 'turing-machine]
        [(is-machine? unknown-val) 'machine]
        [(is-grammar? unknown-val) 'grammar]
        [(regexp? unknown-val) 'regexp]
        [(cfexp? unknown-val) 'cfexp]
        [(procedure? unknown-val) 'inv]
        [else (if (equal? unknown-val 'Aqua)
                  (begin (displayln "Garced was here")
                         'notanfsmval)
                  'notanfsmval)]))
