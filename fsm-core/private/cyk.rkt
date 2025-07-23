#lang racket/base
(require racket/list
         "grammar-getters.rkt"
         "misc.rkt")

(provide cyk)

(define (3d-array x y z init)
  (build-vector x (lambda (row) (build-vector y (lambda (row0) (make-vector z init))))))

(define (3d-array-get arr x y z)
  (vector-ref (vector-ref (vector-ref arr x) y) z))

(define (3d-array-set! arr x y z val)
  (vector-set! (vector-ref (vector-ref arr x) y) z val))

(struct grammar-rule (from to))
;; grammar word -> Boolean
;; CYK
(define (cyk g w)
  (define word-vec (list->vector w))
  (define word-len (vector-length word-vec))
  (define nts-vec (list->vector (grammar-nts g)))

  (define rules-vec (for/vector ([r (in-list (grammar-rules g))])
                      (grammar-rule (first r) (third r))))
  (define arr-bools (3d-array (add1 word-len) (add1 word-len) (add1 (vector-length nts-vec)) #f))
  (define enumeration-ht (make-hash))
  
  (for ([i (in-vector nts-vec)]
        [k (in-range (vector-length nts-vec))])
    (hash-set! enumeration-ht i k))
  
  (for* ([s (in-range word-len)]
         [rule (in-vector rules-vec)])
    (when (let ([symb (symbol->fsmlos (grammar-rule-to rule))])
            (and (= 1 (length symb))
                 (eq? (vector-ref word-vec s) (first symb))))
      (3d-array-set! arr-bools 0 s (hash-ref enumeration-ht (grammar-rule-from rule)) #t)))

  (for* ([l (in-range 2 (add1 word-len) 1)]
         [s (in-range (+ 1 (- word-len l)))]
         [p (in-range 1 l 1)]
         [rule (in-vector rules-vec)])
    (when (let ([to-symb (symbol->fsmlos (grammar-rule-to rule))])
            (and (= 2 (length to-symb))
                 (3d-array-get arr-bools (sub1 p) s (hash-ref enumeration-ht (first to-symb)))
                 (3d-array-get arr-bools (sub1 (- l p)) (+ s p) (hash-ref enumeration-ht (second to-symb)))))
      (3d-array-set! arr-bools (sub1 l) s (hash-ref enumeration-ht (grammar-rule-from rule)) #t)))
  (3d-array-get arr-bools (sub1 word-len) 0 (hash-ref enumeration-ht (grammar-start g))))