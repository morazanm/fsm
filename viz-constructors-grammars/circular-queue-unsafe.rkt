#lang racket
(require racket/unsafe/ops)

(provide make-queue qfull? qempty? enqueue! dequeue! qpeek queue)
(struct queue (values front rear) #:mutable #:transparent)

(define (make-queue init-size [init-value 0])
  (queue (make-vector init-size init-value) -1 -1))

(define (qfull? a-queue)
  (or (and (unsafe-fx= (unsafe-struct*-ref a-queue 1) 0)
           (unsafe-fx= (unsafe-struct*-ref a-queue 2) (unsafe-fx- (unsafe-vector*-length (unsafe-struct*-ref a-queue 0)) 1)
                       ))
      (unsafe-fx= (unsafe-struct*-ref a-queue 2) (unsafe-fxremainder (unsafe-fx- (unsafe-struct*-ref a-queue 1) 1) (unsafe-fx- (unsafe-vector*-length (unsafe-struct*-ref a-queue 0)) 1)))
      )
  )

(define (qempty? a-queue)
  (unsafe-fx= (unsafe-struct*-ref a-queue 1) -1))

(define (qpeek a-queue) (unsafe-vector*-ref (unsafe-struct*-ref a-queue 0) (unsafe-struct*-ref a-queue 1)))

(define (enqueue! a-queue val)
  (if (qfull? a-queue)
      (begin
        
        (unsafe-struct*-set! a-queue 0 (vector-extend (unsafe-struct*-ref a-queue 0) (unsafe-fx* (unsafe-vector*-length (unsafe-struct*-ref a-queue 0)) 2)))
        (enqueue! a-queue val)
        )
      (if (qempty? a-queue)
          (begin
            (unsafe-struct*-set! a-queue 1 0)
            (unsafe-struct*-set! a-queue 2 0)
            (unsafe-vector*-set! (unsafe-struct*-ref a-queue 0) (unsafe-struct*-ref a-queue 2) val)
            a-queue
            )
          (if (and (unsafe-fx= (unsafe-struct*-ref a-queue 2) (unsafe-fx- (unsafe-vector*-length (unsafe-struct*-ref a-queue 0)) 1))
                   (not (unsafe-fx= (unsafe-struct*-ref a-queue 1) 1))
                   )
              (begin
                (unsafe-struct*-set! a-queue 2 0)
                (unsafe-vector*-set! (unsafe-struct*-ref a-queue 0) (unsafe-struct*-ref a-queue 2) val)
                a-queue
                )
              (begin
                (unsafe-struct*-set! a-queue 2 (unsafe-fx+ (unsafe-struct*-ref a-queue 2) 1))
                (unsafe-vector*-set! (unsafe-struct*-ref a-queue 0) (unsafe-struct*-ref a-queue 2) val)
                a-queue
                )
              )
          )
      )
  )
(define (dequeue! a-queue)
  (let [(temp (unsafe-vector*-ref (unsafe-struct*-ref a-queue 0) (unsafe-struct*-ref a-queue 1)))]
    (if (unsafe-fx= (unsafe-struct*-ref a-queue 1) (unsafe-struct*-ref a-queue 2))
        (begin
          (unsafe-struct*-set! a-queue 1 -1)
          (unsafe-struct*-set! a-queue 2 -1)
          temp
          )
        (if (unsafe-fx= (unsafe-struct*-ref a-queue 1) (unsafe-fx- (unsafe-vector*-length (unsafe-struct*-ref a-queue 0)) 1))
            (begin
              (unsafe-struct*-set! a-queue 1 0)
              temp
              )
            (begin
              (unsafe-struct*-set! a-queue 1 (unsafe-fx+ (unsafe-struct*-ref a-queue 1) 1))
              temp
              )
            )
        )
    )
  )