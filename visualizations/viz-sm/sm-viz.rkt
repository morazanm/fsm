#lang racket/base

(require "viz-ndfa.rkt"
         "viz-tm.rkt"
         "viz-pda.rkt")

(provide sm-viz)

(define (sm-viz M a-word #:add-dead [add-dead #f] #:cut-off [cut-off 100] #:head-pos [head-pos 0] . invs)
  (let ([m-type (with-handlers ([exn:fail:contract:arity?
                                 (λ (e) (M 'whatami 0 'whatami))])
                  (M 'whatami))])
    (cond [(or (eq? m-type 'ndfa) (eq? m-type 'dfa))
           (ndfa-viz M a-word #:add-dead add-dead invs)]
          [(eq? m-type 'pda)
           (pda-viz M a-word #:add-dead add-dead #:cut-off cut-off invs)]
          [(or (eq? m-type 'tm) (eq? m-type 'tm-language-recognizer))
           (tm-viz M a-word head-pos #:cut-off cut-off invs)]
          [(or (eq? m-type 'mttm) (eq? m-type 'mttm-language-recognizer))
           (error (format "Stay tuned: sm-viz for mttm and mttm language recognizers is not yet implemented"))]
          [else (error "Unknown finite-state machine type given to sm-viz.")])))
