#lang racket
(require racket/fixnum)

(provide make-queue qfull? qempty? enqueue! dequeue! qpeek queue)
(struct queue (values front rear) #:mutable #:transparent)

(define (make-queue init-size [init-value 0])
  (queue (make-vector init-size init-value) -1 -1))

(define (qfull? a-queue)
  (or (and (fx= (queue-front a-queue) 0)
           (fx= (queue-rear a-queue) (fx- (vector*-length (queue-values a-queue)) 1)
                       ))
      (fx= (queue-rear a-queue) (fxremainder (fx- (queue-front a-queue) 1) (fx- (vector*-length (queue-values a-queue)) 1)))
      )
  )

(define (qempty? a-queue)
  (fx= (queue-front a-queue) -1))

(define (qpeek a-queue) (vector*-ref (queue-values a-queue) (queue-front a-queue)))

(define (enqueue! a-queue val)
  (if (qfull? a-queue)
      (begin
        (displayln "doubled")
        (set-queue-values! a-queue (vector-extend (queue-values a-queue) (fx* (vector*-length (queue-values a-queue)) 2)))
        (enqueue! a-queue val)
        )
      (if (qempty? a-queue)
          (begin
            (set-queue-front! a-queue 0)
            (set-queue-rear! a-queue 0)
            (vector*-set! (queue-values a-queue) (queue-rear a-queue) val)
            a-queue
            )
          (if (and (fx= (queue-rear a-queue) (fx- (vector*-length (queue-values a-queue)) 1))
                   (not (fx= (queue-front a-queue) 1))
                   )
              (begin
                (set-queue-rear! a-queue 0)
                (vector*-set! (queue-values a-queue) (queue-rear a-queue) val)
                a-queue
                )
              (begin
                (set-queue-rear! a-queue (fx+ (queue-rear a-queue) 1))
                (vector*-set! (queue-values a-queue) (queue-rear a-queue) val)
                a-queue
                )
              )
          )
      )
  )

(define (dequeue! a-queue)
  (let [(temp (vector*-ref (queue-values a-queue) (queue-front a-queue)))]
    (if (fx= (queue-front a-queue) (queue-rear a-queue))
        (begin
          (set-queue-front! a-queue -1)
          (set-queue-rear! a-queue -1)
          temp
          )
        (if (fx= (queue-front a-queue) (fx- (vector*-length (queue-values a-queue)) 1))
            (begin
              (set-queue-front! a-queue 0)
              temp
              )
            (begin
              (set-queue-front! a-queue (fx+ (queue-front a-queue) 1))
              temp
              )
            )
        )
    )
  )