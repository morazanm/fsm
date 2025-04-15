#lang typed/racket/base
(require scheme/match
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

;; Returns the rank of the tree
(: rank : (All (A) ((Tree A) -> Natural)))
(define (rank tree)
  (Tree-rank tree))

;; Returns the root of the tree
(: root : (All (A) ((Tree A) -> A)))
(define (root tree)
  (Tree-elem tree))

;; Helper functions for merge and insert functions
(: link : (All (A) ((Tree A) (Tree A) (A A -> Boolean) -> (Tree A))))
(define (link tree1 tree2 func)
  (let ([root1 (root tree1)]
        [root2 (root tree2)]
        [new-rank (add1 (rank tree1))])
    (if (func root1 root2)
        (make-Tree new-rank root1 (Tree-elems tree1) 
                   (cons tree2 (Tree-trees tree1)))
        (make-Tree new-rank root2 (Tree-elems tree2) 
                   (cons tree1 (Tree-trees tree2))))))


(: skew-link : (All (A) (A (Tree A) (Tree A) (FUNC A) -> (Tree A))))
(define (skew-link elem tree1 tree2 func)
  (let* ([tree (link tree1 tree2 func)]
         [lroot (root tree)]
         [lrank (rank tree)]
         [lelems (Tree-elems tree)]
         [ltrees (Tree-trees tree)])
    (if (func elem lroot)
        (make-Tree lrank elem (cons lroot lelems) ltrees)
        (make-Tree lrank lroot (cons elem lelems) ltrees))))

(: ins-tree : (All (A) ((Tree A) (Trees A) (FUNC A) -> (Trees A))))
(define (ins-tree tree lst func)
  (if (null? lst)
      (list tree)
      (let ([fst (car lst)])
        (if (< (rank tree) (rank fst))
            (cons tree lst)
            (ins-tree (link tree fst func) (cdr lst) func)))))


(: merge-trees : (All (A) ((Trees A) (Trees A) (FUNC A) -> (Trees A))))
(define (merge-trees list1 list2 func)
  (cond 
    [(null? list1) list2]
    [(null? list2) list1]
    [else (merge-help (car list1) (cdr list1) (car list2) (cdr list2) func)]))


(: merge-help : 
   (All (A) ((Tree A) (Trees A) (Tree A) (Trees A) (FUNC A) -> (Trees A))))
(define (merge-help tre1 tres1 tre2 tres2 fun)
  (let ([rank1 (rank tre1)]
        [rank2 (rank tre2)])
    (cond
      [(< rank1 rank2) (cons tre1 (merge-trees tres1 (cons tre2 tres2) fun))]
      [(> rank2 rank1) (cons tre2 (merge-trees (cons tre1 tres1) tres2 fun))]
      [else 
       (ins-tree (link tre1 tre2 fun) (merge-trees tres1 tres2 fun) fun)])))

(: normalize : (All (A) ((Trees A) (FUNC A) -> (Trees A))))
(define (normalize trees func)
  (if (null? trees)
      trees
      (ins-tree (car trees) (cdr trees) func)))

;; -----------------------------------------------------------------

;; Inserts an element into the heap
(: heap-insert : (All (A) (A (Heap A) -> (Heap A))))
(define (heap-insert elem heap)
  (let* ([trees (Heap-trees heap)]
         [func (ann (Heap-comparer heap) (FUNC A))]
         [new-ts (make-Heap func (cons (make-Tree 0 elem null null) trees))])
    (match trees
      [(list t1 t2 #{ts : (Trees A)} ...) 
       (if (= (rank t1) (rank t2))
           (make-Heap func 
                      (cons (skew-link elem t1 t2 func) ts))
           new-ts)]
      [else new-ts])))

;; Merges two given heaps
(: merge : (All (A) ((Heap A) (Heap A) -> (Heap A))))
(define (merge heap1 heap2)
  (let ([func (Heap-comparer heap1)]
        [trees1 (Heap-trees heap1)]
        [trees2 (Heap-trees heap2)])
    (make-Heap func (merge-trees (normalize trees1 func) 
                                 (normalize trees2 func)
                                 func))))

;; Helper for find and delete-min/max
(: remove-mintree : 
   (All (A) ((Trees A) (FUNC A) -> (Pair (Tree A) (Trees A)))))
(define (remove-mintree trees func)
  (match trees 
    [(list) (error "Heap is empty")]
    [(list t) (cons t null)]
    [(list t #{ts : (Trees A)} ...) 
     (let* ([pair (remove-mintree ts func)]
            [t1 (car pair)]
            [ts1 (cdr pair)])
       (if (func (root t) (root t1))
           (cons t ts)
           (cons t1 (cons t ts1))))]))

;; Returns min or max element of the heap. Uses exception handling to
;; throw specific errors
(: find-min/max : (All (A) ((Heap A) -> A)))
(define (find-min/max heap)
  (let* ([func (Heap-comparer heap)]
         [trees (Heap-trees heap)]
         [pair (with-handlers 
                   ([exn:fail? (lambda (error?) 
                                 (error 'find-min/max "given heap is empty"))])
                 (remove-mintree trees func))]
         [tree (car pair)])
    (root tree)))

;; Deletes min or max element of the heap. Uses exception handling to
;; throw specific errors as bot h find and delete use the same helper. 
(: delete-min/max : (All (A) ((Heap A) -> (Heap A))))
(define (delete-min/max heap)
  (let* ([func (Heap-comparer heap)]
         [trees (Heap-trees heap)]
         [pair (with-handlers 
                   ([exn:fail? (lambda (error?) 
                                 (error 'delete-min/max
                                        "given heap is empty"))])
                 (remove-mintree trees func))]
         [tree (car pair)]
         [ts (cdr pair)])
    (: ins-all : ((Listof A) (Heap A) -> (Heap A)))
    (define (ins-all lst hp)
      (if (null? lst)
          hp
          (ins-all (cdr lst) (heap-insert (car lst) hp))))
    (ins-all (Tree-elems tree) 
             (merge (make-Heap func (reverse (Tree-trees tree))) 
                    (make-Heap func ts)))))

;; Heap constructor
(: heap : (All (A) ((FUNC A) A * -> (Heap A))))
(define (heap func . lst)
  (foldl (inst heap-insert A) ((inst make-Heap A) func null) lst))


(: sorted-list : (All (A) ((Heap A) -> (Listof A))))
(define (sorted-list heap)
  (if (heap-empty? heap)
      null
      (cons (find-min/max heap) (sorted-list (delete-min/max heap)))))


;; similar to list filter function
(: heap-filter : (All (A) ((A -> Boolean) (Heap A) -> (Heap A))))
(define (heap-filter func hep)
  (: inner : (All (A) ((A -> Boolean) (Heap A) (Heap A) -> (Heap A))))
  (define (inner func hep accum)
    (if (heap-empty? hep)
        accum
        (let ([head (find-min/max hep)]
              [tail (delete-min/max hep)])
          (if (func head)
              (inner func tail (heap-insert head accum))
              (inner func tail accum)))))
  (inner func hep ((inst make-Heap A) (Heap-comparer hep) heap-empty)))


;; similar to list remove function
(: heap-remove : (All (A) ((A -> Boolean) (Heap A) -> (Heap A))))
(define (heap-remove func hep)
  (: inner : (All (A) ((A -> Boolean) (Heap A) (Heap A) -> (Heap A))))
  (define (inner func hep accum)
    (if (heap-empty? hep)
        accum
        (let ([head (find-min/max hep)]
              [tail (delete-min/max hep)])
          (if (func head)
              (inner func tail accum)
              (inner func tail (heap-insert head accum))))))
  (inner func hep ((inst make-Heap A) (Heap-comparer hep) heap-empty)))

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