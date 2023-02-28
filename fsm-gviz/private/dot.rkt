#lang racket
;; NOTE: Some of the code in this file was copied from
;; https://github.com/racket/gui/blob/master/gui-lib/mrlib/private/dot.rkt
(provide
 find-dot
 has-dot-executable?
 find-tmp-dir)

;; these paths are explicitly checked (when find-executable-path
;; fails) because starting drracket from the finder (or the dock) 
;; under mac os x generally does not get the path right.
(define dot-paths 
  '("/usr/bin"
    "/bin"
    "/opt/homebrew/bin"
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

;; find-tmp-dir: path
;; Looks for the systems tmp directory, if it exists then returns the path to that directory.
;; If the tmp dir is not found then the current-directory from which the program is ran is used
;; instead
;; NOTE: The directory that is returned must have read and write access.
(define (find-tmp-dir)
  (define (has-read-write-access? path)
    (define permissions (file-or-directory-permissions path))
    (andmap (lambda (p) (member p permissions))
            '(read write)))
  (define tmp-dir (find-system-path 'temp-dir))
  (match tmp-dir
    [dir #:when (has-read-write-access? dir) dir]
    ;; If the tmp dir does not have read/write then try the current-dir
    [dir #:when (and (not (equal? (current-directory) dir))
                     (has-read-write-access? (current-directory)))
         (current-directory)]
    [_ (error (format "Unable to write or read to directory ~s" (path->string tmp-dir)))]))
