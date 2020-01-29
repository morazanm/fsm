#lang racket

(provide
 exe-script)

;; exe-script: String (shell command) -> None
;; Purpose: executes the given shell command
(define (exe-script cmd-command)
  (system cmd-command))



 ;; just an ex of how to open an image
;; TODO: REMOVE THIS AFTER DONE!!!!
(require 2htdp/image)

(bitmap "hello_world.png")
