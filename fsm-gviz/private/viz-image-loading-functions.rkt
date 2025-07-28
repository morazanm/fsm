#lang racket/base

(require racket/promise)
(provide load-image
         cache-image)

(define (load-image new-img)
  (force new-img))

(define (cache-image new-img)
  (delay/thread (new-img)))