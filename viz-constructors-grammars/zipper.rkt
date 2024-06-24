#lang racket

(provide zipper zipper-next zipper-prev zipper-set zipper-to-begin zipper-to-end list->zipper zipper->list zipper-processed zipper-current zipper-unprocessed
         zipper-at-end? zipper-at-begin?)

;; A zipper is a data structure that allows for a "text cursor" of sorts through other data structures
;; In this case it is a cursor through a given list, allowing for persistent bidirectional movement through
;; the list (a close analogy would be a functional version of a doubly linked list)
;;
;; (listof Any) (listof Any) (listof Any) -> zipper
(struct zipper (processed current unprocessed))

;; zipper -> zipper
;; Moves the cursor to the next item within the original input list
(define (zipper-next zip) (zipper (cons (zipper-current zip) (zipper-processed zip)) (first (zipper-unprocessed zip)) (rest (zipper-unprocessed zip))))

;; zipper -> zipper
;; Moves the cursor to the previous item within the original input list
(define (zipper-prev zip) (zipper (rest (zipper-processed zip)) (first (zipper-processed zip)) (cons (zipper-current zip) (zipper-unprocessed zip))))

;; zipper Any -> zipper
;; Updates the value of whatever was previously in this position of the list with val
(define (zipper-set zip val) (zipper (zipper-processed zip) val (zipper-unprocessed zip)))

;; zipper -> zipper
;; Jumps to the beginning of the original input list
(define (zipper-to-begin zip) (zipper '() (last (zipper-processed zip)) (append (rest (reverse (zipper-processed zip))) (list (zipper-current zip)) (zipper-unprocessed zip))))

;; zipper -> zipper
;; Jumps to the end of the original input list
(define (zipper-to-end zip) (zipper (append (rest (reverse (zipper-unprocessed zip))) (list (zipper-current zip)) (zipper-processed zip)) (last (zipper-unprocessed zip)) '()))

;; (listof Any) -> zipper
;; Converts a list into a zipper
(define (list->zipper lst) (zipper '() (first lst) (rest lst)))

;; zipper -> (listof Any)
;; Converts a zipper into a list
(define (zipper->list zip) (append (reverse (zipper-processed zip)) (list (zipper-current zip)) (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the end of the original input list
(define (zipper-at-end? zip) (empty? (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the beginning of the original input list
(define (zipper-at-begin? zip) (empty? (zipper-processed zip)))