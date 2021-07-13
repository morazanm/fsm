#lang racket
(require rackunit "../../Macros/builder.rkt" "../test-helpers.rkt")

;; --- Pre defined builder ---
(builder pb2 (point2 ([x : 10]
                      [y : (list 1 2 3)]
                      [z])))



(pb2.add-z 5)
(pb2.add-y (list 4 5 6))
(define p2 (pb2.build))

(pb2.add-z 50) ;; pd is reset
(define p-defualt2 (pb2.build))

(check-equal? (point2-x p2) 10 "10 is the default value that was set")
(check-equal? (point2-y p2) (list 4 5 6) "'(4 5 6) is the new list that was set")
(check-equal? (point2-z p2) 5 "5 is the value that the pb set for z")
(check-equal? (point2-x p-defualt2) 10 "10 is the default value that was set")
(check-equal? (point2-y p-defualt2) (list 1 2 3) "'(1 2 3) is the default list that was set")
(check-equal? (point2-z p-defualt2) 50 "50 is the value that the pb set for z")


;(define (add-x val) (set-point2-builder-s-x! temp val))
;(define (add-y val) (set-point2-builder-s-y! temp val))
;(define (add-z val) (set-point2-builder-s-z! temp val))