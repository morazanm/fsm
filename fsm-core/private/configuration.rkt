; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015


;;; CONFIG

(module config racket
  (provide wi-config state-config mk-config member-config? first-config-path rest-path empty-path?
           printable-path)
  
  ; A configuration (config) is (list word-index state).
  ; A path is a (listof config).
  
  ; config --> wi
  (define wi-config car)
  
  ; config --> state
  (define state-config cadr)
  
  ; wi state --> config
  (define (mk-config a-wi a-state) (list a-wi a-state))
  
  ; config (listof config) --> boolean
  (define (member-config? c l) (not (false? (member c l))))
  
  ; path --> boolean
  (define empty-path? null?)
  
  ; path --> config
  (define first-config-path car)
  
  ; path --> path
  (define rest-path cdr)
  
  ; word path --> (listof (list word state))
  (define (printable-path w p)
    (reverse (map (lambda (c) (list (list-tail w (wi-config c)) (state-config c))) p)))
  )