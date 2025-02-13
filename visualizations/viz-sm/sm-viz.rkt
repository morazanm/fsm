#lang racket/base

(require "viz-ndfa.rkt"
         "viz-pda.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/tm.rkt")

(provide sm-viz)

(define (sm-viz M a-word #:add-dead [add-dead #f] #:cut-off [cut-off 100] . invs)
  (cond [(or (eq? (M 'whatami) 'ndfa) (eq? (M 'whatami) 'dfa))
         (ndfa-viz M a-word #:add-dead add-dead invs)]
        [(eq? (M 'whatami) 'pda)
         (pda-viz M a-word #:add-dead add-dead #:cut-off cut-off invs)]
        [(or (eq? (M 'whatami) 'tm) (eq? (M 'whatami) 'tm-language-recognizer))
         (error (format "Stay tuned: sm-viz for tm and tm language recognizers is not yet implemented"))]
        [(or (eq? (M 'whatami) 'mttm) (eq? (M 'whatami) 'mttm-language-recognizer))
         (error (format "Stay tuned: sm-viz for mttm and mttm language recognizers is not yet implemented"))]
        [else (error "Unknown finite-state machine type given to sm-viz.")]))
