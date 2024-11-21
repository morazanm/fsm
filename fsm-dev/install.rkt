#lang racket/base
;; This is a install script to help switch the fsm install path between local and prod

(require racket/runtime-path
         racket/system
         racket/path)


(define-runtime-path file-dir "install.rkt")
(define REPO "https://github.com/morazanm/fsm.git")


;; removes the current fsm install
(define (uninstall-fsm)
  (displayln "Uninstalling FSM")
  (system "raco pkg remove fsm"))

;; installs fsm
;; opt: 1 -> local, 2 -> prod
(define (install-fsm opt)
  (define fsm-dir (simplify-path (build-path (path-only file-dir) 'up)))
  (if (eq? opt 2)
      (begin
        (displayln "Installing prod FSM")
        (system (format "raco pkg install ~s" REPO)))
      (begin
        (displayln (format "Installing local FSM at: ~s" (path->string fsm-dir)))
        (system (format "raco pkg install --link ~s" (path->string fsm-dir))))))


(define (run)
  (define-values (in out) (make-pipe))
  (displayln "Would you like to install Local or Prod FSM?")
  (displayln "[1] - Local")
  (displayln "[2] - Prod")
  (define option (string->number (read-line (current-input-port))))
  (when (or (not option)
            (or (> option 2) (< option 1)))
    (error "Invalid user input"))
  (uninstall-fsm)
  (install-fsm option)
  (displayln "Done"))


(run)