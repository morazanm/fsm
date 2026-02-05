#lang racket/base

(require "viz-ndfa.rkt"
         "viz-tm.rkt"
         "viz-pda.rkt"
         "viz-mttm.rkt"
         "sm-viz-contracts/sm-viz-contracts.rkt"
         racket/contract)

(provide sm-viz)

;; M tape [boolean] [natnum] [natnum] [symbol] . (listof (list state (X -> boolean))) -> void
(define/contract (sm-viz M a-word #:add-dead [add-dead #f] #:cut-off [cut-off 100] #:head-pos [head-pos 0] #:palette [palette 'default] . invs)
  sm-viz/c
  (let ([m-type (with-handlers ([exn:fail:contract:arity?
                                 (λ (e) (M 'whatami 0 'whatami))])
                  (M 'whatami))])
    (cond [(or (eq? m-type 'ndfa) (eq? m-type 'dfa))
           (ndfa-viz M a-word invs #:add-dead add-dead #:palette palette)]
          [(eq? m-type 'pda)
           (pda-viz M a-word #:add-dead add-dead #:cut-off cut-off invs)]
          [(or (eq? m-type 'tm) (eq? m-type 'tm-language-recognizer))
           (tm-viz M a-word head-pos #:cut-off cut-off invs)]
          [(or (eq? m-type 'mttm) (eq? m-type 'mttm-language-recognizer))
           (error (format "Stay tuned: sm-viz for mttm and mttm language recognizers is not yet implemented"))]
          [else (error "Unknown finite-state machine type given to sm-viz.")])))
