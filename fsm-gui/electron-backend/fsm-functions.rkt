#lang racket
(provide
 build-machine)

;; build-machine :: jsexpr -> jsexpr
;; takes the electron-gui machine and runs it in fsm-core. It then
;; returns the machien or the error msg if it fails to build
(define (build-machine data)
  (hash 'data "Hello from racket"))
