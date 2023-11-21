#lang fsm
(require 2htdp/universe rackunit)
(provide run-viz)


;; a-vst --> void
(define (run-viz a-vst draw-etc process-key a-name)
  (begin
    (big-bang
        a-vst                
      [on-draw draw-etc]
      [on-key process-key]
      [name a-name]))
  (void))