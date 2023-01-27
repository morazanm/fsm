#lang racket
;; NOTE: this code was coppied from https://github.com/racket/gui/blob/master/gui-lib/mrlib/private/dot.rkt
(provide find-dot has-dot-executable?)

;; these paths are explicitly checked (when find-executable-path
;; fails) because starting drracket from the finder (or the dock) 
;; under mac os x generally does not get the path right.
(define dot-paths 
  '("/usr/bin"
    "/bin"
    "/usr/local/bin"
    "/opt/local/bin/"))

(define dot.exe (if (eq? (system-type) 'windows) "dot.exe" "dot"))

;; has-dot? : boolean
(define (has-dot-executable?)
  (path? (find-dot)))

;; find-dot : path | false
;; looks for the dot path on computer, if exists then returns it otherwise returns false
(define (find-dot)
  (with-handlers ([(lambda (e) ; may not have permission
                     (and (exn:fail? e)
                          (regexp-match "access denied" (exn-message e))))
                   (λ (x) #f)])
    (define dp (find-executable-path dot.exe))
    (cond
      [dp dp]
      [else
       (ormap (λ (x) (and (file-exists? (build-path x dot.exe))
                          (build-path x dot.exe)))
              dot-paths)])))