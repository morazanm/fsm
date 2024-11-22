#lang racket/base

(provide (all-defined-out))

;; A zipper is a data structure that allows for a "text cursor" of sorts through other data structures

;; box nat -> zipper
(struct vector-zipper (data idx length) #:transparent)

(define (vector-zipper-current zip) (vector-ref (vector-zipper-data zip) (vector-zipper-idx zip)))

;; zipper -> zipper
;; Moves the cursor to the next item within the original input list
(define (vector-zipper-next zip) (struct-copy vector-zipper zip
                                       [idx (add1 (vector-zipper-idx zip))])
  )

;; zipper -> zipper
;; Moves the cursor to the previous item within the original input list
(define (vector-zipper-prev zip) (struct-copy vector-zipper zip
                                       [idx (sub1 (vector-zipper-idx zip))])
  )

;; zipper Any -> zipper
;; Updates the value of whatever was previously in this position of the list with val
(define (vector-zipper-set zip val) (begin
                               (vector-set! (vector-zipper-data zip) (vector-zipper-idx zip) val)
                               zip)
  
  )

;; zipper -> zipper
;; Jumps to the beginning of the original input list
(define (vector-zipper-to-begin zip) (struct-copy vector-zipper zip
                                       [idx 0])
  
  )

;; zipper -> zipper
;; Jumps to the end of the original input list
(define (vector-zipper-to-end zip) (struct-copy vector-zipper zip
                                       [idx (sub1 (vector-zipper-length zip))])
  
  )

;; zipper -> zipper
;; Jumps to specific element in zipper
(define (vector-zipper-to-idx zip idx)
  (struct-copy vector-zipper zip
               [idx idx]))
  

;; (listof Any) -> zipper
;; Converts a list into a zipper
(define (list->vector-zipper lst) (vector-zipper (list->vector lst) 0 (length lst)))

(define (vector->vector-zipper vec) (vector-zipper vec 0 (vector-length vec)))

;; zipper -> (listof Any)
;; Converts a zipper into a list
(define (vector-zipper->list zip) (vector->list (vector-zipper-data zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the end of the original input list
(define (vector-zipper-at-end? zip) (= (sub1 (vector-zipper-length zip)) (vector-zipper-idx zip))#;(empty? (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the beginning of the original input list
(define (vector-zipper-at-begin? zip) (= 0 (vector-zipper-idx zip)))

;; zipper -> boolean
;; Determine if the zipper is empty
(define (vector-zipper-empty? zip) (= (vector-zipper-length zip) 0))
