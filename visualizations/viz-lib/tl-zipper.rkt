#lang racket/base
(require racket/treelist)
(provide (all-defined-out))
;; A zipper is a data structure that allows for a "text cursor" of sorts through other data structures
;; In this case it is a cursor through a given list, allowing for persistent bidirectional movement through
;; the list (a close analogy would be a functional version of a doubly linked list)
;;
;; (listof Any) (listof Any) (listof Any) -> zipper
(struct tl-zipper (tl idx))

(define (tl-zipper-empty? zip) (treelist-empty? (tl-zipper-tl zip)))
(define (tl-zipper-next zip) (if (tl-zipper-empty? zip)
                                 zip
                                 (tl-zipper (tl-zipper-tl zip)
                                        (add1 (tl-zipper-idx zip)))))
(define (tl-zipper-prev zip) (if (tl-zipper-empty? zip)
                                 zip
                                 (tl-zipper (tl-zipper-tl zip)
                                        (sub1 (tl-zipper-idx zip)))))
(define (tl-zipper-set zip val)
  (tl-zipper (treelist-set (tl-zipper-tl zip) (tl-zipper-idx zip) val)
             (tl-zipper-idx zip)))

(define (tl-zipper-to-begin zip)
  (tl-zipper (tl-zipper-tl zip)
             0))

(define (tl-zipper-to-end zip)
  (if (tl-zipper-empty? zip)
      zip
      (tl-zipper (tl-zipper-tl zip)
             (sub1 (treelist-length (tl-zipper-tl zip))))))

(define (tl-zipper-to-idx zip idx)
  (tl-zipper (tl-zipper-tl zip)
             idx))

(define (tl->tl-zipper tl)
  (tl-zipper tl
             0))

(define (tl-zipper->tl zip)
  (tl-zipper-tl zip))
