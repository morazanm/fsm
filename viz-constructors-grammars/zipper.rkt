#lang racket

(provide zipper zipper-next zipper-prev zipper-set zipper-to-begin zipper-to-end list->zipper zipper->list zipper-processed zipper-current zipper-unprocessed)
(struct zipper (processed current unprocessed))

(define (zipper-next zip) (zipper (cons (zipper-current zip) (zipper-processed zip)) (first (zipper-unprocessed zip)) (rest (zipper-unprocessed zip))))
(define (zipper-prev zip) (zipper (rest (zipper-processed zip)) (first (zipper-processed zip)) (cons (zipper-current zip) (zipper-unprocessed zip))))
(define (zipper-set zip val) (zipper (zipper-processed zip) val (zipper-unprocessed zip)))
(define (zipper-to-begin zip) (zipper '() (last (zipper-processed zip)) (append (rest (reverse (zipper-processed zip))) (list (zipper-current zip)) (zipper-unprocessed zip))))
(define (zipper-to-end zip) (zipper (append (rest (reverse (zipper-unprocessed zip))) (list (zipper-current zip)) (zipper-processed zip)) (last (zipper-unprocessed zip)) '()))
(define (list->zipper lst) (zipper '() (first lst) (rest lst)))
(define (zipper->list zip) (append (reverse (zipper-processed zip)) (list (zipper-current zip)) (zipper-unprocessed zip)))

#|
(define test '(a b c d e f g h))
(define zipped (list->zipper test))
(define end (zipper-to-end zipped))
;(displayln (zipper-processed end))
;(displayln (zipper-current end))
;(displayln (zipper-unprocessed end))
(define beginning (zipper-to-begin end))
(displayln (zipper-processed beginning))
(displayln (zipper-current beginning))
(displayln (zipper-unprocessed beginning))
|#