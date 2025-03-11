#lang racket/base
(require racket/treelist
         racket/list)
(provide (all-defined-out))

#;(define (treelist-filter pred tl)
  (define (treelist-filter-helper pred tl i)
  (if (= i -1)
      tl
      (if (pred (treelist-ref tl i))
          (treelist-filter-helper pred tl (sub1 i))
          (treelist-filter-helper pred (treelist-delete tl i) (sub1 i)))))
  (treelist-filter-helper pred tl (sub1 (treelist-length tl))))

(define (treelist-insert-list tl i lst)
  (if (empty? lst)
      tl
      (treelist-insert-list (treelist-insert tl i (first lst)) (add1 i) (rest lst))))