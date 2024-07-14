#lang racket

(require racket/fixnum
         racket/treelist)

;(provide make-queue qfull? qempty? enqueue! dequeue! qpeek (struct-out queue))
#;(struct queue (values front rear) #:mutable #:transparent)
(provide make-queue qempty? enqueue! dequeue! qpeek)

(define (make-queue)
  (treelist))

#;(define (qfull? a-queue)
  (or (and (fx= (queue-front a-queue) 0)
           (fx= (queue-rear a-queue) (fx- (treelist-length (queue-values a-queue)) 1)
                       ))
      (fx= (queue-rear a-queue) (fxremainder (fx- (queue-front a-queue) 1) (fx- (treelist-length (queue-values a-queue)) 1)))
      )
  )

(define (qempty? a-queue)
  (treelist-empty? a-queue)
  #;(fx= (queue-front a-queue) -1)
  )

(define (qpeek a-queue) (treelist-last a-queue))

(define (enqueue! a-queue val)
  (treelist-insert a-queue 0 val)
  #;(if (qfull? a-queue)
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
           ; a-queue
            )
          (if (and (fx= (queue-rear a-queue) (fx- (vector*-length (queue-values a-queue)) 1))
                   (not (fx= (queue-front a-queue) 1))
                   )
              (begin
                (set-queue-rear! a-queue 0)
                (vector*-set! (queue-values a-queue) (queue-rear a-queue) val)
                ;a-queue
                )
              (begin
                (set-queue-rear! a-queue (fx+ (queue-rear a-queue) 1))
                (vector*-set! (queue-values a-queue) (queue-rear a-queue) val)
                ;a-queue
                )
              )
          )
      )
  )

(define (dequeue! a-queue)
  (treelist-delete a-queue (sub1 (treelist-length a-queue)))
  #;(let [(temp (vector*-ref (queue-values a-queue) (queue-front a-queue)))]
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