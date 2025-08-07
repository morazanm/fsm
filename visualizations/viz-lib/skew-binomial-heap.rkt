#lang typed/racket/base
(require racket/match
         typed/racket/unsafe)
(unsafe-provide heap-map heap-ormap heap-andmap
         heap-fold  heap-filter heap-remove
         heap-empty? heap-insert merge find-min/max delete-min/max 
         heap sorted-list heap-empty Heap build-heap)

(define-struct: (A) Tree ([rank : Natural]
                          [elem : A]
                          [elems : (Listof A)]
                          [trees : (Trees A)]))

(define-struct: (A) Heap ([comparer : (A A -> Boolean)]
                          [trees : (Trees A)]))

(define-type-alias (Trees A) (Listof (Tree A)))

(define-type-alias (FUNC A) (A A -> Boolean))

;; Checks for empty
(: heap-empty? : (All (A) ((Heap A) -> Boolean)))
(define (heap-empty? heap)
  (null? (Heap-trees heap)))

;; An empty heap
(define heap-empty null)

;; Helper functions for merge and insert functions
(: link : (All (A) ((Tree A) (Tree A) (A A -> Boolean) -> (Tree A))))
(define (link tree1 tree2 func)
  (if (func (Tree-elem tree1) (Tree-elem tree2))
      (make-Tree (add1 (Tree-rank tree1)) (Tree-elem tree1) (Tree-elems tree1) 
                 (cons tree2 (Tree-trees tree1)))
      (make-Tree (add1 (Tree-rank tree1)) (Tree-elem tree2) (Tree-elems tree2) 
                 (cons tree1 (Tree-trees tree2)))))


(: skew-link : (All (A) (A (Tree A) (Tree A) (FUNC A) -> (Tree A))))
(define (skew-link elem tree1 tree2 func)
  (define tree (link tree1 tree2 func))
  (if (func elem (Tree-elem tree))
      (make-Tree (Tree-rank tree) elem (cons (Tree-elem tree) (Tree-elems tree)) (Tree-trees tree))
      (make-Tree (Tree-rank tree) (Tree-elem tree) (cons elem (Tree-elems tree)) (Tree-trees tree))))

(: ins-tree : (All (A) ((Tree A) (Trees A) (FUNC A) -> (Trees A))))
(define (ins-tree tree lst func)
  (if (null? lst)
      (list tree)
      (if (< (Tree-rank tree) (Tree-rank (car lst)))
          (cons tree lst)
          (ins-tree (link tree (car lst) func) (cdr lst) func))))


(: merge-trees : (All (A) ((Trees A) (Trees A) (FUNC A) -> (Trees A))))
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

(: normalize : (All (A) ((Trees A) (FUNC A) -> (Trees A))))
(define (normalize trees func)
  (if (null? trees)
      trees
      (ins-tree (car trees) (cdr trees) func)))

;; -----------------------------------------------------------------

;; Inserts an element into the heap
(: heap-insert : (All (A) (A (Heap A) -> (Heap A))))
(define (heap-insert elem heap)
  (define new-ts (make-Heap (Heap-comparer heap) (cons (make-Tree 0 elem null null) (Heap-trees heap))))
  (match (Heap-trees heap)
    [(list t1 t2 #{ts : (Trees A)} ...) 
     (if (= (Tree-rank t1) (Tree-rank t2))
         (make-Heap (Heap-comparer heap) 
                    (cons (skew-link elem t1 t2 (Heap-comparer heap)) ts))
         new-ts)]
    [else new-ts]))

;; Merges two given heaps
(: merge : (All (A) ((Heap A) (Heap A) -> (Heap A))))
(define (merge heap1 heap2)
  (make-Heap (Heap-comparer heap1)
             (merge-trees (normalize (Heap-trees heap1) (Heap-comparer heap1)) 
                          (normalize (Heap-trees heap2) (Heap-comparer heap1))
                          (Heap-comparer heap1))))

;; Helper for find and delete-min/max
(: remove-mintree : 
   (All (A) ((Trees A) (FUNC A) -> (Pair (Tree A) (Trees A)))))
(define (remove-mintree trees func)
  (match trees 
    [(list) (error "Heap is empty")]
    [(list t) (cons t null)]
    [(list t #{ts : (Trees A)} ...) 
     (let ([pair (remove-mintree ts func)])
       (if (func (Tree-elem t) (Tree-elem (car pair)))
           (cons t ts)
           (cons (car pair) (cons t (cdr pair)))))]))

;; Returns min or max element of the heap. Uses exception handling to
;; throw specific errors
(: find-min/max : (All (A) ((Heap A) -> A)))
(define (find-min/max heap)
  (Tree-elem (car (remove-mintree (Heap-trees heap) (Heap-comparer heap)))))


(: ins-all : (All (A) ((Listof A) (Heap A) -> (Heap A))))
(define (ins-all lst hp)
  (if (null? lst)
      hp
      (ins-all (cdr lst) (heap-insert (car lst) hp))))

;; Deletes min or max element of the heap. Uses exception handling to
;; throw specific errors as bot h find and delete use the same helper. 
(: delete-min/max : (All (A) ((Heap A) -> (Heap A))))
(define (delete-min/max heap)
  (define pair (remove-mintree (Heap-trees heap) (Heap-comparer heap)))
  (ins-all (Tree-elems (car pair)) 
           (merge (make-Heap (Heap-comparer heap) (reverse (Tree-trees (car pair)))) 
                  (make-Heap (Heap-comparer heap) (cdr pair)))))

;; Heap constructor
(: heap : (All (A) ((FUNC A) A * -> (Heap A))))
(define (heap func . lst)
  (foldl (inst heap-insert A) ((inst make-Heap A) func null) lst))


(: sorted-list : (All (A) ((Heap A) -> (Listof A))))
(define (sorted-list heap)
  (if (heap-empty? heap)
      null
      (cons (find-min/max heap) (sorted-list (delete-min/max heap)))))


(: heap-filter-helper : (All (A) ((A -> Boolean) (Heap A) (Heap A) -> (Heap A))))
(define (heap-filter-helper func hep accum)
  (if (heap-empty? hep)
      accum
      (let ([head (find-min/max hep)]
            [tail (delete-min/max hep)])
        (if (func head)
            (heap-filter-helper func tail (heap-insert head accum))
            (heap-filter-helper func tail accum)))))

;; similar to list filter function
(: heap-filter : (All (A) ((A -> Boolean) (Heap A) -> (Heap A))))
(define (heap-filter func hep)
  (heap-filter-helper func hep ((inst make-Heap A) (Heap-comparer hep) heap-empty)))


(: heap-remove-helper : (All (A) ((A -> Boolean) (Heap A) (Heap A) -> (Heap A))))
(define (heap-remove-helper func hep accum)
  (if (heap-empty? hep)
      accum
      (let ([head (find-min/max hep)]
            [tail (delete-min/max hep)])
        (if (func head)
            (heap-remove-helper func tail accum)
            (heap-remove-helper func tail (heap-insert head accum))))))

;; similar to list remove function
(: heap-remove : (All (A) ((A -> Boolean) (Heap A) -> (Heap A))))
(define (heap-remove func hep)
  (heap-remove-helper func hep ((inst make-Heap A) (Heap-comparer hep) heap-empty)))

;; similar to list map function. apply is expensive so using case-lambda
;; in order to saperate the more common case
(: heap-map : 
   (All (A C B ...) 
        (case-lambda 
          ((C C -> Boolean) (A -> C) (Heap A) -> (Heap C))
          ((C C -> Boolean)
           (A B ... B -> C) (Heap A) (Heap B) ... B -> (Heap C)))))
(define heap-map
  (pcase-lambda: (A C B ...)
                 [([comp : (C C -> Boolean)]
                   [func : (A -> C)]
                   [heap : (Heap A)])
                  (map-single ((inst make-Heap C) comp heap-empty) func heap)]
                 [([comp : (C C -> Boolean)]
                   [func : (A B ... B -> C)]
                   [heap : (Heap A)] . [heaps : (Heap B) ... B])
                  (apply map-multiple
                         ((inst make-Heap C) comp heap-empty)
                         func heap heaps)]))


(: map-single : (All (A C) ((Heap C) (A -> C) (Heap A) -> (Heap C))))
(define (map-single accum func heap)
  (if (heap-empty? heap)
    accum
    (map-single (heap-insert (func (find-min/max heap)) accum)
                func
                (delete-min/max heap))))

(: map-multiple : 
   (All (A C B ...) 
        ((Heap C) (A B ... B -> C) (Heap A) (Heap B) ... B -> (Heap C))))
(define (map-multiple accum func heap . heaps)
  (if (or (heap-empty? heap) (ormap heap-empty? heaps))
    accum
    (apply map-multiple
           (heap-insert (apply func
                          (find-min/max heap)
                          (map find-min/max heaps))
                   accum)
           func 
           (delete-min/max heap)
           (map delete-min/max heaps))))


;; similar to list foldr or foldl
(: heap-fold : 
   (All (A C B ...) 
        (case-lambda ((C A -> C) C (Heap A) -> C)
                     ((C A B ... B -> C) C (Heap A) (Heap B) ... B -> C))))
(define heap-fold
  (pcase-lambda: (A C B ...) 
                 [([func : (C A -> C)]
                   [base : C]
                   [heap  : (Heap A)])
                  (if (heap-empty? heap)
                    base
                    (heap-fold func (func base (find-min/max heap))
                          (delete-min/max heap)))]
                 [([func : (C A B ... B -> C)]
                   [base : C]
                   [heap  : (Heap A)] . [heaps : (Heap B) ... B])
                  (if (or (heap-empty? heap) (ormap heap-empty? heaps))
                    base
                    (apply heap-fold 
                           func 
                           (apply func base (find-min/max heap)
                                  (map find-min/max heaps))
                           (delete-min/max heap)
                           (map delete-min/max heaps)))]))

;; Similar to build-list
(: build-heap : (All (A) (Natural (Natural -> A) (A A -> Boolean) -> (Heap A))))
(define (build-heap size func comparer)
  (let: loop : (Heap A) ([n : Natural size])
        (if (zero? n)
            ((inst Heap A) comparer heap-empty)
            (let ([nsub1 (sub1 n)])
              (heap-insert (func nsub1) (loop nsub1))))))



;; similar to list andmap function
(: heap-andmap : 
   (All (A B ...) 
        (case-lambda ((A -> Boolean) (Heap A) -> Boolean)
                     ((A B ... B -> Boolean) (Heap A) (Heap B) ... B 
                                             -> Boolean))))
(define heap-andmap
  (pcase-lambda: (A B ... ) 
                 [([func  : (A -> Boolean)]
                   [deque : (Heap A)])
                  (or (heap-empty? deque)
                      (and (func (find-min/max deque))
                           (heap-andmap func (delete-min/max deque))))]
                 [([func  : (A B ... B -> Boolean)]
                   [deque : (Heap A)] . [deques : (Heap B) ... B])
                  (or (heap-empty? deque) (ormap heap-empty? deques)
                      (and (apply func (find-min/max deque) 
                                  (map find-min/max deques))
                           (apply heap-andmap func (delete-min/max deque) 
                                  (map delete-min/max deques))))]))

;; Similar to ormap
(: heap-ormap : 
   (All (A B ...) 
        (case-lambda ((A -> Boolean) (Heap A) -> Boolean)
                     ((A B ... B -> Boolean) (Heap A) (Heap B) ... B 
                                             -> Boolean))))
(define heap-ormap
  (pcase-lambda: (A B ... ) 
                 [([func  : (A -> Boolean)]
                   [deque : (Heap A)])
                  (and (not (heap-empty? deque))
                       (or (func (find-min/max deque))
                           (heap-ormap func (delete-min/max deque))))]
                 [([func  : (A B ... B -> Boolean)]
                   [deque : (Heap A)] . [deques : (Heap B) ... B])
                  (and (not (or (heap-empty? deque) (ormap heap-empty? deques)))
                       (or (apply func (find-min/max deque) 
                                  (map find-min/max deques))
                           (apply heap-ormap func (delete-min/max deque) 
                                  (map delete-min/max deques))))]))