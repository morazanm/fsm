#lang racket/base

(require "viz-ndfa.rkt"
         "viz-pda.rkt"
         "../../fsm-core/interface.rkt")

(provide sm-viz)

(define (sm-viz M a-word #:add-dead [add-dead #f] #:max-cmps [max-cmps 100] . invs)
  (cond [(or (eq? (sm-type M) 'ndfa) (equal? (sm-type M) 'dfa))
         (apply ndfa-viz M a-word #:add-dead add-dead invs)]
        [(eq? (sm-type M) 'pda)
         (apply pda-viz M a-word #:add-dead add-dead #:max-cmps max-cmps invs)]
        [(or (eq? M 'tm) (eq? M 'tm-language-recognizer))
         (error (format "Stay tuned: sm-viz for tm and tm language recognizers is not yet implemented"))]
        [(or (eq? M 'mttm) (eq? M 'mttm-language-recognizer))
         (error (format "Stay tuned: sm-viz for mttm and mttm language recognizers is not yet implemented"))]
        [else (error "Unknown finite-state machine type given to sm-viz.")]))
