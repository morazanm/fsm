#lang racket/base

(require racket/match)

(provide display-failed-test (struct-out exn:fail:check-failed))

(define (display-failed-test desc exn)
  ((error-display-handler) desc exn))

;; exeption-structure
(struct exn:fail:check-failed exn:fail:user
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:check-failed msg marks (list a-srcloc ...))
       a-srcloc])))