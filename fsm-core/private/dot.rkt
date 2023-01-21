;; NOTE: this code was coppied from https://github.com/racket/gui/blob/master/gui-lib/mrlib/private/dot.rkt
(provide find-dot)

;; these paths are explicitly checked (when find-executable-path
;; fails) because starting drracket from the finder (or the dock) 
;; under mac os x generally does not get the path right.
(define dot-paths 
  '("/usr/bin"
    "/bin"
    "/usr/local/bin"
    "/opt/local/bin/"))

(define dot.exe (if (eq? (system-type) 'windows) "dot.exe" "dot"))

(define (find-dot [neato? #f])
  (with-handlers ([(lambda (e) ; may not have permission
                     (and (exn:fail? e)
                          (regexp-match "access denied" (exn-message e))))
                   (λ (x) #f)])
    (define dp (find-executable-path dot.exe))
    (cond
      [dp dp]
      [else
       (ormap (λ (x) (and (file-exists? (build-path x dot.exe))
                          (file-exists? (build-path x neato.exe))
                          (build-path x dot.exe)))
              dot-paths)])))