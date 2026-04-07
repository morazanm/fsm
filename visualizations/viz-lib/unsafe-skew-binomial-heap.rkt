#lang racket/base
(#%declare #:unsafe)
(require (for-syntax racket/base
                     racket/set)
         racket/require
         (filtered-in
          (λ (name)
            (define unsafe-methods-used
              (set "unsafe-vector*-ref"
                   "unsafe-vector*-set!"
                   "unsafe-vector*-length"
                   "unsafe-fxvector-ref"
                   "unsafe-fxvector-set!"
                   "unsafe-fxvector-length"
                   "unsafe-car"
                   "unsafe-cdr"))
            (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
                  [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
                  [(regexp-match #rx"^unsafe-cons-list" name) "cons"]
                  [else name]))
          racket/unsafe/ops))

(provide root-empty? heap-insert find-min/max delete-min/max heap)

(struct Tree (rank
              elem
              elems
              trees))

(struct Root (val heap))

;; Helper functions for merge and insert functions
;(: link (All (A) (-> (Tree (Root A)) (Tree (Root A)) (Comparator A) (Tree (Root A)))))
(define (link tree1 tree2 func)
  (if (func (unsafe-struct*-ref (unsafe-struct*-ref tree1 1) 0) (unsafe-struct*-ref (unsafe-struct*-ref tree2 1) 0))
      (Tree (fx+ (unsafe-struct*-ref tree1 0) 1)
            (unsafe-struct*-ref tree1 1)
            (unsafe-struct*-ref tree1 2) 
            (cons tree2 (unsafe-struct*-ref tree1 3)))
      (Tree (fx+ (unsafe-struct*-ref tree1 0) 1)
            (unsafe-struct*-ref tree2 1)
            (unsafe-struct*-ref tree2 2) 
            (cons tree1 (unsafe-struct*-ref tree2 3)))))


;(: skew-link (All (A) (-> (Root A) (Tree (Root A)) (Tree (Root A)) (Comparator A) (Tree (Root A)))))
(define (skew-link elem tree1 tree2 func)
  (define tree (link tree1 tree2 func))
  (if (func (unsafe-struct*-ref elem 0) (unsafe-struct*-ref (unsafe-struct*-ref tree 1) 0))
      (Tree (fx+ (unsafe-struct*-ref tree1 0) 1)
            elem
            (cons (unsafe-struct*-ref tree 1) (unsafe-struct*-ref tree 2))
            (unsafe-struct*-ref tree 3))
      (Tree (unsafe-struct*-ref tree 0)
            (unsafe-struct*-ref tree 1)
            (cons elem (unsafe-struct*-ref tree 2))
            (unsafe-struct*-ref tree 3))))

;(: ins-tree (All (A) (-> (Tree (Root A)) (Trees (Root A)) (Comparator A) (Trees (Root A)))))
(define (ins-tree tree lst func)
  (if (null? lst)
      (list tree)
      (if (fx< (unsafe-struct*-ref tree 0) (unsafe-struct*-ref (car lst) 0))
          (cons tree lst)
          (ins-tree (link tree (car lst) func) (cdr lst) func))))


;(: merge-trees (All (A) (-> (Trees (Root A)) (Trees (Root A)) (Comparator A) (Trees (Root A)))))
(define (merge-trees list1 list2 func)
  (cond 
    [(null? list1) list2]
    [(null? list2) list1]
    [(fx< (unsafe-struct*-ref (car list1) 0) (unsafe-struct*-ref (car list2) 0))
     (cons (car list1) (merge-trees (cdr list1) (cons (car list2) (cdr list2)) func))]
    [(fx> (unsafe-struct*-ref (car list2) 0) (unsafe-struct*-ref (car list1) 0))
     (cons (car list2) (merge-trees (cons (car list1) (cdr list1)) (cdr list2) func))]
    [else 
     (ins-tree (link (car list1) (car list2) func) (merge-trees (cdr list1) (cdr list2) func) func)]))

;(: normalize (All (A) (-> (Trees (Root A)) (Comparator A) (Trees (Root A)))))
(define (normalize trees func)
  (if (null? trees)
      trees
      (ins-tree (car trees) (cdr trees) func)))

;; -----------------------------------------------------------------

;; Inserts an element into the heap
;(: heap-insert-internal (All (A) (-> (Root A) (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (heap-insert-internal elem heap comparator)
  (define new-ts (cons (Tree 0 elem null null) heap))
  (if (null? new-ts)
      new-ts
      (if (null? (cdr new-ts))
          new-ts
          (if (fx= (unsafe-struct*-ref (car new-ts) 0) (unsafe-struct*-ref (car (cdr new-ts)) 0))
              (cons (skew-link elem (car new-ts) (car (cdr new-ts)) comparator) (cdr (cdr new-ts)))
              new-ts))))

;; Merges two given heaps
;(: merge (All (A) (-> (Heap (Root A)) (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (merge heap1 heap2 comparator)
  (merge-trees (normalize heap1 comparator) 
               (normalize heap2 comparator)
               comparator))

;; Helper for find and delete-min/max
;(: remove-mintree (All (A) (-> (Trees (Root A)) (Comparator A) (Pair (Tree (Root A)) (Trees (Root A))))))
(define (remove-mintree trees func)
  (if (null? trees)
      (error "Heap is empty")
      (if (null? (cdr trees))
          (cons (car trees) null)
          (let ([pair (remove-mintree (cdr trees) func)])
            (if (func (unsafe-struct*-ref (unsafe-struct*-ref (car trees) 1) 0)
                      (unsafe-struct*-ref (unsafe-struct*-ref (car pair) 1) 0))
                trees
                (cons (car pair) (cons (car trees) (cdr pair))))))))

;; Returns min or max element of the heap. Uses exception handling to
;; throw specific errors
;(: find-min/max-internal (All (A) (-> (Heap (Root A)) (Comparator A) (Root A))))
(define (find-min/max-internal heap comparator)
  (unsafe-struct*-ref (car (remove-mintree heap comparator)) 1))


;(: ins-all (All (A) (-> (Listof (Root A)) (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (ins-all lst hp comparator)
  (if (null? lst)
      hp
      (ins-all (cdr lst) (heap-insert-internal (car lst) hp comparator) comparator)))


(define (unsafe-reverse lst)
  (define (loop a l)
    (if (null? l)
        a
        (loop (cons (car l) a) (cdr l))))
  (loop null lst))

;; Deletes min or max element of the heap. Uses exception handling to
;; throw specific errors as bot h find and delete use the same helper. 
;(: delete-min/max-internal (All (A) (-> (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (delete-min/max-internal heap comparator)
  (define pair (remove-mintree heap comparator))
  (ins-all (unsafe-struct*-ref (car pair) 2)
           (merge (unsafe-reverse (unsafe-struct*-ref (car pair) 3)) 
                  (cdr pair)
                  comparator)
           comparator))

;(: find-min/max (All (A) (-> (Root A) (Option A))))
(define (find-min/max root)
  (unsafe-struct*-ref root 0))

;(: root-empty? (All (A) (-> (Root A) Boolean)))
(define (root-empty? root)
  (boolean? (unsafe-struct*-ref root 0)))

;(: merge-root (All (A) (-> (Root A) (Root A) (Comparator A) (Root A))))
(define (merge-root h0 h1 comparator)
  (if (root-empty? h0)
      h1
      (if (root-empty? h1)
          h0
          (if (comparator (unsafe-struct*-ref h0 0) (unsafe-struct*-ref h1 0))
              (Root (unsafe-struct*-ref h0 0)
                    (heap-insert-internal h1 (unsafe-struct*-ref h0 1) comparator))
              (Root (unsafe-struct*-ref h1 0)
                    (heap-insert-internal h0 (unsafe-struct*-ref h1 1) comparator))))))

;(: heap-insert (All (A) (-> A (Root A) (Comparator A) (Root A))))
(define (heap-insert val a-heap comparator)
  (merge-root (Root val null)
              a-heap
              comparator))

;; Heap constructor
;(: heap : (All (A) ((Comparator A) A * -> (U (List) (Root A)))))
(define (heap comparator . lst)
  (foldl (lambda (val
                  accum)
           (heap-insert val accum comparator))
         (Root #f null)
         lst))

;(: delete-min/max (All (A) (-> (Root A) (Comparator A) (Root A))))
(define (delete-min/max heap comparator)
  (if (null? (unsafe-struct*-ref heap 1))
      (Root #f null)
      (let ([r (find-min/max-internal (unsafe-struct*-ref heap 1) comparator)]
            [h2 (delete-min/max-internal (unsafe-struct*-ref heap 1) comparator)])
        (Root (unsafe-struct*-ref r 0) (merge (unsafe-struct*-ref r 1) h2 comparator)))))