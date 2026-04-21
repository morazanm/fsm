#lang racket/base

;; Here be dragons
;; This code was written for computers not humans
;; We have sacrificed every principle for the sake of performance
;; and said "Skill issue" to anyone who can't understand the code
;;
;; Failed attempts to fix: 7
;; Total hours wasted: 19

(#%declare #:unsafe)

;; This implementation is based on Chase-Lev dynamic worker deques https://doi.org/10.1145/1073970.1073974
;; And uses the memory fences from https://doi.org/10.1145/2517327.2442524

(require
  ;; Yes these imports are required for the filtered-in to work
  (for-syntax racket/base
              racket/set
              syntax/parse
              racket/future)
  (only-in racket/future
           processor-count)
  racket/require
  (filtered-in
   (λ (name)
     (define unsafe-methods-used
       (set "unsafe-vector*-ref"
            "unsafe-vector*-set!"
            "unsafe-vector*-length"))
     (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
           [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
           [else name]))
   racket/unsafe/ops)
  ;; For debugging unsafe operations used
  ;; God help you if you actually need to use these though
  #;racket/fixnum
  #;racket/vector
  #;racket/unsafe/ops)

(provide run-parallel push!)

(struct work-dequeue (stack-vec
                      bottom
                      top) #:mutable)

(begin-for-syntax
  (define proc-count (processor-count)))

;; In lieu of fork join tasks, we just check to see if every work deque is empty
;; This effectively just reimplements an and check based on the number of cores on the current system
(define-syntax (all-dequeue-empty-helper? stx)
  (syntax-parse stx
    [(_ q-vec n)
     (if (= (syntax->datum #'n) (sub1 proc-count))
         #'(fx= (unsafe-struct*-ref (vector*-ref q-vec n) 1)
                (unsafe-struct*-ref (vector*-ref q-vec n) 2))
         #`(if (fx= (unsafe-struct*-ref (vector*-ref q-vec n) 1)
                    (unsafe-struct*-ref (vector*-ref q-vec n) 2))
               (all-dequeue-empty-helper? q-vec #,(add1 (syntax->datum #'n)))
               #f))]))

(define-syntax (all-dequeue-empty? stx)
  (syntax-parse stx
    [(_ q-vec)
     #`(all-dequeue-empty-helper? q-vec 0)]))

;; (A) (-> (work-queue A) A Void)
(define (push! a-queue val)
  (define curr-bottom (unsafe-struct*-ref a-queue 1))
  (memory-order-release)
  
  ;; Start growable array code
  
  (define curr-top (unsafe-struct*-ref a-queue 2))
  (memory-order-release)
  (define curr-vec-mask (fx- (vector*-length (unsafe-struct*-ref a-queue 0)) 1))
  (when (fx>= (fx- curr-bottom curr-top) curr-vec-mask)
    (define new-vec-len (fx* 2 (vector*-length (unsafe-struct*-ref a-queue 0))))
    (define new-vec-mask (fx- new-vec-len 1))
    (define new-vec (make-vector new-vec-len #f))
    (for ([idx (in-range curr-top curr-bottom)])
      (vector*-set! new-vec
                    (fxand idx new-vec-mask)
                    (vector*-ref (unsafe-struct*-ref a-queue 0)
                                 (fxand idx
                                        curr-vec-mask))))
    (unsafe-struct*-set! a-queue 0 new-vec))
  
  ;; End growable array code
  
  (vector*-set! (unsafe-struct*-ref a-queue 0)
                (fxand curr-bottom (fx- (vector*-length (unsafe-struct*-ref a-queue 0)) 1))
                val)
  
  (memory-order-release)

  (unsafe-struct*-set! a-queue 1 (fx+ curr-bottom 1)))


;; (A) (-> (work-queue A) A)
(define (pop! a-queue)
  (define new-bottom (fx- (unsafe-struct*-ref a-queue 1) 1))
  (unsafe-struct*-set! a-queue 1 new-bottom)
  (memory-order-acquire)
  (memory-order-release)
  (define curr-top (unsafe-struct*-ref a-queue 2))
  (define size (fx- new-bottom curr-top))
  (cond [(fx< size 0)
         (unsafe-struct*-set! a-queue 1 curr-top)
         #f]
        [else
         (define res
           (vector*-ref (unsafe-struct*-ref a-queue 0)
                        (fxand new-bottom
                               (fx- (vector*-length (unsafe-struct*-ref a-queue 0)) 1))))
         (cond [(fx> size 0)
                res]
               [else
                (define a-res
                  (cond [(unsafe-struct*-cas! a-queue 2 curr-top (fx+ curr-top 1))
                         res]
                        [else #f]))
                (unsafe-struct*-set! a-queue 1 (fx+ curr-top 1))
                a-res])]))

;; (A) (-> (work-queue A) A)
(define (steal! a-queue)
  (define curr-top (unsafe-struct*-ref a-queue 2))
  (memory-order-acquire)
  (memory-order-release)
  (define curr-bottom (unsafe-struct*-ref a-queue 1))
  (memory-order-acquire)
  (memory-order-release)
  (cond [(fx<= (fx- curr-bottom curr-top) 0) #f]
        [else
         (define res
           (vector*-ref (unsafe-struct*-ref a-queue 0)
                        (fxand curr-top
                               (fx- (vector*-length (unsafe-struct*-ref a-queue 0)) 1))))
         (memory-order-acquire)
         (memory-order-release)
         (if (unsafe-struct*-cas! a-queue 2 curr-top (fx+ curr-top 1))
             res
             #f)]))

;; (A) (-> (Listof A) (-> (work-queue A) A B) Power-of-2 (Vectorof (Listof B)))
(define (run-parallel initial-queue task-func queue-size)
  (define num-workers (processor-count))
  (when (not (= (modulo queue-size 2) 0))
    (error "queue size needs to be power of 2"))
  (define job-queues
    (unsafe-vector*->immutable-vector!
     (for/vector
         #:length num-workers
       ([i (in-range num-workers)])
       (work-dequeue (make-vector queue-size #f) 0 0))))
  (define pthread-pool (make-parallel-thread-pool num-workers))

  (for ([elem (in-list initial-queue)]
        [idx (in-naturals)])
    (push! (vector*-ref job-queues (modulo idx num-workers)) elem))

  (define ((check-if-nonempty? q-idx))
    (define res
      (fx> (fx- (unsafe-struct*-ref (vector*-ref job-queues q-idx) 1)
                (unsafe-struct*-ref (vector*-ref job-queues q-idx) 2))
           0))
    res)
  
  (memory-order-release)
  (memory-order-acquire)
  (define rng (make-pseudo-random-generator))
  (define result-vector
    (for/vector 
        #:length num-workers
      ([queue-idx (in-range num-workers)])

      (define check-if-not-empty? (check-if-nonempty? queue-idx))
      (thread #:pool pthread-pool
              #:keep 'results
              (lambda ()
                (define (attempt-steal)
                  (define idx (random num-workers rng))
                  (cond [(fx= idx queue-idx) (attempt-steal)]
                        [else (steal! (vector*-ref job-queues idx))]))
                
                (define (loop results)
                  (cond [(all-dequeue-empty? job-queues)
                         results]
                        [(check-if-not-empty?)
                         (define new-task (pop! (vector*-ref job-queues queue-idx)))
                         (cond [new-task
                                (define task-res (task-func (vector*-ref job-queues queue-idx)
                                                            new-task))
                                (if task-res
                                    (loop (cons task-res results))
                                    (loop results))]
                               [else (loop results)])]
                        [else
                         (define new-task (attempt-steal))
                         (cond [new-task
                                (define task-res (task-func (vector*-ref job-queues queue-idx)
                                                            new-task))
                                (if task-res
                                    (loop (cons task-res results))
                                    (loop results))]
                               [else (loop results)])]))
                (loop '())))))
  (for ([idx (in-range num-workers)])
    (vector*-set! result-vector idx (thread-wait (vector*-ref result-vector idx))))
  result-vector)