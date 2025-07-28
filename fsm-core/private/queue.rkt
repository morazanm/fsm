#lang racket/base

(require racket/treelist
         data/queue)

(provide make-queue
         qempty?
         enqueue!
         dequeue!)

(define (qempty? a-queue)
  (queue-empty? a-queue))

(define (qpeek a-queue)
  (treelist-last a-queue))