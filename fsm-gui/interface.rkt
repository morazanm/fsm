#lang racket

(require "legacy-gui/visualize.rkt")
(require "electron-backend/server.rkt")
(provide (rename-out (visualize sm-visualize))
         sm-visualize2
         sm-visualize3)


;; sm-visualize2 :: fsa listof(listof symbol func)
(define (sm-visualize2 fsa . invariants)
  (define parsed-invariants (map (lambda (i) (cons (first i) (second i)))
                                 invariants))
  (run-with-prebuilt fsa parsed-invariants))




;; sm-visualize3 :: fsa listof(listof symbol string)
;; TODO: Make this a marco
(define (sm-visualize3 fsa . invariants)
  (define parsed-invariants (map (lambda (i) (cons (first i) (second i)))
                                 invariants))
  (run-with-prebuilt-hotload fsa parsed-invariants))
