#lang racket

(provide (all-defined-out))

;; A zipper is a data structure that allows for a "text cursor" of sorts through other data structures

;; box nat -> zipper
(struct vector-zipper (data idx length) #:transparent)

(define (vector-zipper-current zip) (vector-ref (vector-zipper-data zip) (vector-zipper-idx zip)))

;; zipper -> zipper
;; Moves the cursor to the next item within the original input list
(define (vector-zipper-next zip) (struct-copy vector-zipper zip
                                       [idx (add1 (vector-zipper-idx zip))])
                                       #;(struct-copy zipper zip
                                       [processed (cons (zipper-current zip) (zipper-processed zip))]
                                       [current (first (zipper-unprocessed zip))]
                                       [unprocessed (rest (zipper-unprocessed zip))]
                                       [idx (add1 (zipper-idx zip))]
                                       )
  )

;; zipper -> zipper
;; Moves the cursor to the previous item within the original input list
(define (vector-zipper-prev zip) (struct-copy vector-zipper zip
                                       [idx (sub1 (vector-zipper-idx zip))])
  #;(struct-copy zipper zip
                                       [processed (rest (zipper-processed zip))]
                                       [current (first (zipper-processed zip))]
                                       [unprocessed (cons (zipper-current zip) (zipper-unprocessed zip))]
                                       [idx (sub1 (zipper-idx zip))]
                                       )
  )

;; zipper Any -> zipper
;; Updates the value of whatever was previously in this position of the list with val
(define (vector-zipper-set zip val) (begin
                               (vector-set! (vector-zipper-data zip) (vector-zipper-idx zip) val)
                               zip)
  #;(struct-copy zipper zip
                                          [current val]
                                          )
  )

;; zipper -> zipper
;; Jumps to the beginning of the original input list
(define (vector-zipper-to-begin zip) (struct-copy vector-zipper zip
                                       [idx 0])
  #;(struct-copy zipper zip
                                           [processed '()]
                                           [current (last (zipper-processed zip))]
                                           [unprocessed (append (rest (reverse (zipper-processed zip))) (list (zipper-current zip)) (zipper-unprocessed zip))]
                                           [idx 0]
                                           )
  )

;; zipper -> zipper
;; Jumps to the end of the original input list
(define (vector-zipper-to-end zip) (struct-copy vector-zipper zip
                                       [idx (sub1 (vector-zipper-length zip))])
  #;(let ([new-processed (append (rest (reverse (zipper-unprocessed zip))) (list (zipper-current zip)) (zipper-processed zip))])
                              (struct-copy zipper zip
                                           [processed new-processed]
                                           [current (last (zipper-unprocessed zip))]
                                           [unprocessed '()]
                                           [idx (length new-processed)]
                                           )
                              )
  )

;; zipper -> zipper
;; Jumps to specific element in zipper
#;(define (zipper-to-idx zip idx)
  (define (zipper-to-idx-helper-down zip)
    (if (= (zipper-idx zip) idx)
        zip
        (zipper-to-idx-helper-down (zipper-prev zip))
        )
    )
  (define (zipper-to-idx-helper-up zip)
    (if (= (zipper-idx zip) idx)
        zip
        (zipper-to-idx-helper-up (zipper-next zip))
        )
    )
  (if (= (zipper-idx zip) idx)
      zip
      (if (< (zipper-idx zip) idx)
          (zipper-to-idx-helper-up (zipper-next zip))
          (zipper-to-idx-helper-down (zipper-prev zip))
          )
      )
  )
(define (vector-zipper-to-idx zip idx)
  (struct-copy vector-zipper zip
               [idx idx]))
  

;; (listof Any) -> zipper
;; Converts a list into a zipper
(define (list->vector-zipper lst) (vector-zipper (list->vector lst) 0 (length lst))#;(zipper '() (first lst) (rest lst) 0))

(define (vector->vector-zipper vec) (vector-zipper vec 0 (vector-length vec)))

;; zipper -> (listof Any)
;; Converts a zipper into a list
(define (vector-zipper->list zip) (vector->list (vector-zipper-data zip))#;(append (reverse (zipper-processed zip)) (list (zipper-current zip)) (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the end of the original input list
(define (vector-zipper-at-end? zip) (= (sub1 (vector-zipper-length zip)) (vector-zipper-idx zip))#;(empty? (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the beginning of the original input list
(define (vector-zipper-at-begin? zip) (= 0 (vector-zipper-idx zip))#;(empty? (zipper-processed zip)))
