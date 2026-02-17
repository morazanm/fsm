#lang racket/base
(require racket/list)
(provide (struct-out zipper)
         zipper-empty?
         zipper-length
         zipper-next
         zipper-prev
         zipper-set
         zipper-to-begin
         zipper-to-end
         zipper-to-idx
         list->zipper
         zipper->list
         zipper-at-end?
         zipper-at-begin?
         )

;; A zipper is a data structure that allows for a "text cursor" of sorts through other data structures
;; In this case it is a cursor through a given list, allowing for persistent bidirectional movement through
;; the list (a close analogy would be a functional version of a doubly linked list)
;;
;; (listof Any) (listof Any) (listof Any) -> zipper
(struct zipper (processed current unprocessed idx) #:transparent)

(define (zipper-empty? zip) (and (null? (zipper-current zip))
                                 (null? (zipper-processed zip))
                                 (null? (zipper-unprocessed zip))))

(define (zipper-length zip) (if (zipper-empty? zip) 0 (+ (length (zipper-processed zip)) 1 (length (zipper-unprocessed zip)))))

;; zipper -> zipper
;; Moves the cursor to the next item within the original input list
(define (zipper-next zip) (if (zipper-empty? zip)
                              zip
                              (struct-copy zipper zip
                                           [processed (cons (zipper-current zip) (zipper-processed zip))]
                                           [current (car (zipper-unprocessed zip))]
                                           [unprocessed (cdr (zipper-unprocessed zip))]
                                           [idx (add1 (zipper-idx zip))]
                                           )))

;; zipper -> zipper
;; Moves the cursor to the previous item within the original input list
(define (zipper-prev zip) (if (zipper-empty? zip)
                              zip
                              (struct-copy zipper zip
                                           [processed (cdr (zipper-processed zip))]
                                           [current (car (zipper-processed zip))]
                                           [unprocessed (cons (zipper-current zip) (zipper-unprocessed zip))]
                                           [idx (sub1 (zipper-idx zip))]
                                           )))

;; zipper Any -> zipper
;; Updates the value of whatever was previously in this position of the list with val
(define (zipper-set zip val) (struct-copy zipper zip
                                          [current val]
                                          )
  )

;; zipper -> zipper
;; Jumps to the beginning of the original input list
(define (zipper-to-begin zip) (if (zipper-empty? zip)
                                  zip
                                  (struct-copy zipper zip
                                               [processed '()]
                                               [current (last (zipper-processed zip))]
                                               [unprocessed (append (cdr (reverse (zipper-processed zip))) (list (zipper-current zip)) (zipper-unprocessed zip))]
                                               [idx 0]
                                               )))

;; zipper -> zipper
;; Jumps to the end of the original input list
(define (zipper-to-end zip) (if (zipper-empty? zip)
                                zip
                                (let ([new-processed (append (cdr (reverse (zipper-unprocessed zip))) (list (zipper-current zip)) (zipper-processed zip))])
                                  (struct-copy zipper zip
                                               [processed new-processed]
                                               [current (last (zipper-unprocessed zip))]
                                               [unprocessed '()]
                                               [idx (length new-processed)]
                                               )
                                  )))

;; zipper -> zipper
;; Jumps to specific element in zipper
(define (zipper-to-idx zip idx)
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
  

;; (listof Any) -> zipper
;; Converts a list into a zipper
(define (list->zipper lst) (if (null? lst)
                               (zipper '() '() '() 0)
                               (zipper '() (car lst) (cdr lst) 0)
                               ))

;; zipper -> (listof Any)
;; Converts a zipper into a list
(define (zipper->list zip) (append (reverse (zipper-processed zip)) (list (zipper-current zip)) (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the end of the original input list
(define (zipper-at-end? zip) (null? (zipper-unprocessed zip)))

;; zipper -> boolean
;; Checks if the zipper's cursor is at the beginning of the original input list
(define (zipper-at-begin? zip) (null? (zipper-processed zip)))
