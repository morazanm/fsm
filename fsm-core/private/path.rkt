; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

;;; PATH

; A path is a (listof config).

(module rules racket
  (require "configuration.rkt" "word.rkt")
  
  (provide mk-path convert-path)
  
  ; config path --> path
  (define (mk-path c p) (cons c p))
  
  ; path word --> (listof (list word state))
  (define (convert-path p w) 
    (map (lambda (c) 
           (cond [(symbol? c) c]
                 [(= (wi-config c) (length w)) (list null (state-config c))]
                 [else (list (get-subword w (wi-config c)) (state-config c))])) 
         p))
  )