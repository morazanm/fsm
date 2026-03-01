#lang racket/base

(#%declare #:unsafe)

(require
  ;; Yes these imports are required for the filtered-in to work
  (for-syntax racket/base
              racket/set)
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
  #;racket/fixnum
  #;racket/vector
  #;racket/unsafe/ops)

(provide run-parallel push!)

(struct Task (parent-task
              num-childs
              task-data
              locked?) #:mutable)

(struct work-dequeue (stack-vec
                      bottom
                      top) #:mutable)

;; (A) (work-queue (task A)) A (task A) -> Void
(define (push! a-queue val parent-task)
  (define curr-bottom (work-dequeue-bottom a-queue))
  (memory-order-release)
  ;; Start growable array code
  
  (define curr-top (work-dequeue-top a-queue))
  (memory-order-release)
  (define curr-vec-mask (fx- (vector*-length (work-dequeue-stack-vec a-queue)) 1))
  (when (fx>= (fx- curr-bottom curr-top) curr-vec-mask)
    (define new-vec-len (fx* 2 (vector*-length (work-dequeue-stack-vec a-queue))))
    (define new-vec-mask (fx- new-vec-len 1))
    (define new-vec (make-vector new-vec-len #f))
    (for ([idx (in-range curr-top curr-bottom)])
      (vector*-set! new-vec
                    (fxand idx new-vec-mask)
                    (vector*-ref (work-dequeue-stack-vec a-queue)
                                 (fxand idx
                                        curr-vec-mask))))
    (set-work-dequeue-stack-vec! a-queue new-vec))
  
  ;; End growable array code
  
  (vector*-set! (work-dequeue-stack-vec a-queue)
                (fxand curr-bottom (fx- (vector*-length (work-dequeue-stack-vec a-queue)) 1))
                (Task parent-task 0 val #f))
  
  (memory-order-release)
  (define (spin-loop a-task)
    (cond [(Task-locked? a-task) (spin-loop a-task)]
          [(unsafe-struct*-cas! a-task 3 #f #t)
           (set-Task-num-childs! parent-task (fx+ (Task-num-childs parent-task) 1))
           (set-Task-locked?! parent-task #f)]
          [else (spin-loop a-task)]))
  (spin-loop parent-task)

  (set-work-dequeue-bottom! a-queue (fx+ curr-bottom 1)))

;; (A) (work-queue (Task A)) -> (Task A)
(define (pop! a-queue)
  (define new-bottom (fx- (work-dequeue-bottom a-queue) 1))
  (set-work-dequeue-bottom! a-queue new-bottom)
  (memory-order-acquire)
  (memory-order-release)
  (define curr-top (work-dequeue-top a-queue))

  (cond [(fx< new-bottom curr-top)
         (set-work-dequeue-bottom! a-queue curr-top)
         #f]
        [else
         (define res
           (vector*-ref (work-dequeue-stack-vec a-queue)
                        (fxand new-bottom
                               (fx- (vector*-length (work-dequeue-stack-vec a-queue)) 1))))
         (cond [(fx> new-bottom curr-top)
                res]
               [(unsafe-struct*-cas! a-queue 2 curr-top (fx+ curr-top 1))
                (set-work-dequeue-bottom! a-queue (fx+ curr-top 1))
                res]
               [else #f])]))

;; (A) (work-queue (Task A)) -> (Task A)
(define (steal! a-queue)
  (define curr-top (work-dequeue-top a-queue))
  (memory-order-acquire)
  (memory-order-release)
  (define curr-bottom (work-dequeue-bottom a-queue))
  (memory-order-release)
  (cond [(fx< curr-top curr-bottom)
         (define res
           (vector*-ref (work-dequeue-stack-vec a-queue)
                        (fxand curr-top
                               (fx- (vector*-length (work-dequeue-stack-vec a-queue)) 1))))
         (if (unsafe-struct*-cas! a-queue 2 curr-top (fx+ curr-top 1))
             res
             #f)]
        [else #f]))

(define (spin-loop-decrement-num-childs a-task)
  (cond [(Task-locked? a-task) (spin-loop-decrement-num-childs a-task)]
        [(unsafe-struct*-cas! a-task 3 #f #t)
         (set-Task-num-childs! a-task (fx- (Task-num-childs a-task) 1))
         (set-Task-locked?! a-task #f)
         (when (Task-parent-task a-task)
           (spin-loop-check-num-childs (Task-parent-task a-task)))]
        [else (spin-loop-decrement-num-childs a-task)]))

(define (spin-loop-check-num-childs a-task)
  (cond [(Task-locked? a-task) (spin-loop-check-num-childs a-task)]
        [(unsafe-struct*-cas! a-task 3 #f #t)
         (when (and (Task-parent-task a-task)
                    (fx= (Task-num-childs a-task) 0))
           (set-Task-locked?! a-task #f)
           (spin-loop-decrement-num-childs (Task-parent-task a-task)))]
        [else (spin-loop-check-num-childs a-task)]))

;; (A) (Listof A) (-> (work-queue (Task A)) A (Task A) B) Nat Power-of-2 -> (Vectorof (Listof B))
(define (run-parallel initial-queue task-func num-workers queue-size)
  (when (not (= (modulo queue-size 2) 0))
    (error "queue size needs to be power of 2"))
  (define job-queues
    (unsafe-vector*->immutable-vector!
     (for/vector
         #:length num-workers
       ([i (in-range num-workers)])
       (work-dequeue (make-vector queue-size #f) 0 0))))
  (define pthread-pool (make-parallel-thread-pool num-workers))
  (define main-parent-task (Task #f 0 #f #f))

  (for ([elem (in-list initial-queue)]
        [idx (in-naturals)])
    (push! (vector*-ref job-queues (modulo idx num-workers)) elem main-parent-task))

  (memory-order-release)
  (memory-order-acquire)
  (define rng (make-pseudo-random-generator))
  (define result-vector
    (for/vector 
        #:length num-workers
      ([queue-idx (in-range num-workers)])
      
      (thread #:pool pthread-pool
              #:keep 'results
              (lambda ()
                (define (attempt-steal)
                  (define idx (random num-workers rng))
                  (cond [(fx= idx queue-idx) (attempt-steal)]
                        [else (steal! (vector*-ref job-queues idx))]))
                
                (define (loop results)
                  (cond [(fx= (Task-num-childs main-parent-task) 0) results]
                        [(fx> (fx- (work-dequeue-bottom (vector*-ref job-queues queue-idx))
                                   (work-dequeue-top (vector*-ref job-queues queue-idx)))
                              0)
                         (define new-task (pop! (vector*-ref job-queues queue-idx)))
                         (cond [new-task
                                (define task-res (task-func (vector*-ref job-queues queue-idx)
                                                            (Task-task-data new-task)
                                                            (Task-parent-task new-task)))
                                (spin-loop-check-num-childs new-task)
                                (loop (append task-res results))]
                               [else (loop results)])]
                        [else
                         (define new-task (attempt-steal))
                         (cond [new-task
                                (define task-res (task-func (vector*-ref job-queues queue-idx)
                                                            (Task-task-data new-task)
                                                            (Task-parent-task new-task)))
                                (spin-loop-check-num-childs new-task)
                                (loop (append task-res results))]
                               [else (loop results)])]))
                (loop '())))))
  (for ([idx (in-range num-workers)])
    (vector*-set! result-vector idx (thread-wait (vector*-ref result-vector idx))))
  result-vector)