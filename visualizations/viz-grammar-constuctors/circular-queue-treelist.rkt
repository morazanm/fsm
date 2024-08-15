#lang racket

(require racket/treelist)

(provide make-queue
         qempty?
         enqueue!
         dequeue!
         qpeek)

(define (make-queue)
  (treelist))

(define (qempty? a-queue)
  (treelist-empty? a-queue))

(define (qpeek a-queue)
  (treelist-last a-queue))

(define (enqueue! a-queue val)
  (treelist-insert a-queue 0 val))

(define (dequeue! a-queue)
  (treelist-delete a-queue (sub1 (treelist-length a-queue))))
