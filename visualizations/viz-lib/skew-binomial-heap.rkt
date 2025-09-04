#lang typed/racket/base
(require racket/match
         typed/racket/unsafe)
(unsafe-provide root-empty? heap-insert find-min/max delete-min/max heap)

(struct (A) Tree ([rank : Natural]
                  [elem : A]
                  [elems : (Listof A)]
                  [trees : (Listof (Tree A))]))

(define-type-alias (Trees A) (Listof (Tree A)))

(struct (A) Heap ([trees : (Trees A)]))

(struct (A) Root ([val : (Option A)]
                  [heap : (Heap (Root A))]))

(define-type-alias (Comparator A) ((Option A) (Option A) -> Boolean))

;; Checks for empty
(: heap-empty? (All (A) (-> (Heap A) Boolean)))
(define (heap-empty? heap)
  (null? (Heap-trees heap)))

;; Helper functions for merge and insert functions
(: link (All (A) (-> (Tree (Root A)) (Tree (Root A)) (Comparator A) (Tree (Root A)))))
(define (link tree1 tree2 func)
  (if (func (Root-val (Tree-elem tree1)) (Root-val (Tree-elem tree2)))
      (Tree (add1 (Tree-rank tree1))
            (Tree-elem tree1)
            (Tree-elems tree1) 
            (cons tree2 (Tree-trees tree1)))
      (Tree (add1 (Tree-rank tree1))
            (Tree-elem tree2)
            (Tree-elems tree2) 
            (cons tree1 (Tree-trees tree2)))))


(: skew-link (All (A) (-> (Root A) (Tree (Root A)) (Tree (Root A)) (Comparator A) (Tree (Root A)))))
(define (skew-link elem tree1 tree2 func)
  (define tree (link tree1 tree2 func))
  (if (func (Root-val elem) (Root-val (Tree-elem tree)))
      (Tree (add1 (Tree-rank tree1))
            elem
            (cons (Tree-elem tree) (Tree-elems tree))
            (Tree-trees tree))
      (Tree (Tree-rank tree)
            (Tree-elem tree)
            (cons elem (Tree-elems tree))
            (Tree-trees tree))))

(: ins-tree (All (A) (-> (Tree (Root A)) (Trees (Root A)) (Comparator A) (Trees (Root A)))))
(define (ins-tree tree lst func)
  (if (null? lst)
      (list tree)
      (if (< (Tree-rank tree) (Tree-rank (car lst)))
          (cons tree lst)
          (ins-tree (link tree (car lst) func) (cdr lst) func))))


(: merge-trees (All (A) (-> (Trees (Root A)) (Trees (Root A)) (Comparator A) (Trees (Root A)))))
(define (merge-trees list1 list2 func)
  (cond 
    [(null? list1) list2]
    [(null? list2) list1]
    [(< (Tree-rank (car list1)) (Tree-rank (car list2)))
     (cons (car list1) (merge-trees (cdr list1) (cons (car list2) (cdr list2)) func))]
    [(> (Tree-rank (car list2)) (Tree-rank (car list1)))
     (cons (car list2) (merge-trees (cons (car list1) (cdr list1)) (cdr list2) func))]
    [else 
     (ins-tree (link (car list1) (car list2) func) (merge-trees (cdr list1) (cdr list2) func) func)]))

(: normalize (All (A) (-> (Trees (Root A)) (Comparator A) (Trees (Root A)))))
(define (normalize trees func)
  (if (null? trees)
      trees
      (ins-tree (car trees) (cdr trees) func)))

;; -----------------------------------------------------------------

;; Inserts an element into the heap
(: heap-insert-internal (All (A) (-> (Root A) (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (heap-insert-internal elem heap comparator)
  (define new-ts (Heap (cons (Tree 0 elem null null) (Heap-trees heap))))
  (match (Heap-trees heap)
    [(list #{t1 : (Tree (Root A))} #{t2 : (Tree (Root A))} #{ts : (Trees (Root A))} ...) 
     (if (= (Tree-rank t1) (Tree-rank t2))
         (Heap (cons (skew-link elem t1 t2 comparator) ts))
         new-ts)]
    [else new-ts]))

;; Merges two given heaps
(: merge (All (A) (-> (Heap (Root A)) (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (merge heap1 heap2 comparator)
  (Heap (merge-trees (normalize (Heap-trees heap1) comparator) 
                     (normalize (Heap-trees heap2) comparator)
                     comparator)))

;; Helper for find and delete-min/max
(: remove-mintree
   (All (A) (-> (Trees (Root A)) (Comparator A) (Pair (Tree (Root A)) (Trees (Root A))))))
(define (remove-mintree trees func)
  (match trees 
    [(list) (error "Heap is empty")]
    [(list t) (cons t null)]
    [(list #{t : (Tree (Root A))} #{ts : (Trees (Root A))} ...) 
     (let ([pair (remove-mintree ts func)])
       (if (func (Root-val (Tree-elem t)) (Root-val (Tree-elem (car pair))))
           (cons t ts)
           (cons (car pair) (cons t (cdr pair)))))]))

;; Returns min or max element of the heap. Uses exception handling to
;; throw specific errors
(: find-min/max-internal (All (A) (-> (Heap (Root A)) (Comparator A) (Root A))))
(define (find-min/max-internal heap comparator)
  (Tree-elem (car (remove-mintree (Heap-trees heap) comparator))))


(: ins-all (All (A) (-> (Listof (Root A)) (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (ins-all lst hp comparator)
  (if (null? lst)
      hp
      (ins-all (cdr lst) (heap-insert-internal (car lst) hp comparator) comparator)))

;; Deletes min or max element of the heap. Uses exception handling to
;; throw specific errors as bot h find and delete use the same helper. 
(: delete-min/max-internal (All (A) (-> (Heap (Root A)) (Comparator A) (Heap (Root A)))))
(define (delete-min/max-internal heap comparator)
  (define pair (remove-mintree (Heap-trees heap) comparator))
  (ins-all (Tree-elems (car pair))
           (merge (Heap (reverse (Tree-trees (car pair)))) 
                  (Heap (cdr pair))
                  comparator)
           comparator))

(: find-min/max (All (A) (-> (Root A) (Option A))))
(define (find-min/max root)
  (Root-val root))

(: root-empty? (All (A) (-> (Root A) Boolean)))
(define (root-empty? root)
  (boolean? (Root-val root)))

(: merge-root (All (A) (-> (Root A) (Root A) (Comparator A) (Root A))))
(define (merge-root h0 h1 comparator)
  (if (root-empty? h0)
      h1
      (if (root-empty? h1)
          h0
          (if (comparator (Root-val h0) (Root-val h1))
              (Root (Root-val h0)
                    (heap-insert-internal h1 (Root-heap h0) comparator))
              (Root (Root-val h1)
                    (heap-insert-internal h0 (Root-heap h1) comparator))))))

(: heap-insert (All (A) (-> A (Root A) (Comparator A) (Root A))))
(define (heap-insert val a-heap comparator)
  (merge-root (Root val ((inst Heap (Root A)) null))
              a-heap
              comparator))

;; Heap constructor
(: heap : (All (A) ((Comparator A) A * -> (U (List) (Root A)))))
(define (heap comparator . lst)
  (foldl (lambda ([val : A]
                  [accum : (Root A)])
           ((inst heap-insert A) val accum comparator))
         (Root #f ((inst Heap (Root A)) null))
         lst))

(: delete-min/max (All (A) (-> (Root A) (Comparator A) (Root A))))
(define (delete-min/max heap comparator)
  (if (heap-empty? (Root-heap heap))
      (Root #f ((inst Heap (Root A)) null))
      (let ([r (find-min/max-internal (Root-heap heap) comparator)]
            [h2 (delete-min/max-internal (Root-heap heap) comparator)])
        (Root (Root-val r) (merge (Root-heap r) h2 comparator)))))