#lang racket

(provide (struct-out exn:fail:check-accept-failed))

;; exn struct
(struct exn:fail:check-accept-failed exn:fail
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:check-accept-failed msg marks a-srcloc)
       (list a-srcloc)])))

