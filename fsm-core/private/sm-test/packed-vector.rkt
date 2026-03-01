#lang racket/base
(require (for-syntax racket/base
                     racket/set
                     (only-in racket/fixnum most-positive-fixnum)
                     )
         racket/require
         (filtered-in
          (λ (name)
            (define unsafe-methods-used
              (set "unsafe-vector*-ref"
                   "unsafe-vector*-set!"
                   "unsafe-vector*-length"
                   "unsafe-fxvector-ref"
                   "unsafe-fxvector-set!"
                   "unsafe-fxvector-length"))
            (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
                  [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          racket/unsafe/ops)
         (only-in racket/fixnum make-fxvector fxvector-copy))

(#%declare #:unsafe)

(provide create-compressedvec-functions
         (struct-out compressedvec)
         create-gcfxv-functions
         (struct-out gcfxvector))

;; Only implemented for little endian currently since I do not have big endian hardware to test on

(struct compressedvec (buffer
                       size)
  #:mutable)

(begin-for-syntax
  
  (define (determine-number-of-bits-in-fixnum)
    (cond [(= (most-positive-fixnum) (sub1 (expt 2 60)))
           61]
          [(= (most-positive-fixnum) (sub1 (expt 2 62)))
           63]
          [else (error "Something has gone terribly wrong")]))
  (define precompute-num-bits
    (determine-number-of-bits-in-fixnum)))

(define-syntax (num-bits stx)
  #`#,(determine-number-of-bits-in-fixnum))

(define (make-mask bit-width)
  (fx- (fxlshift 1 bit-width) 1))


(define ((make-compressed-vec create-vals-func) size init-val)
  (define-values (num-fixnums-allocated cvec-size)
    (create-vals-func size))
  (compressedvec (make-fxvector num-fixnums-allocated
                                init-val)
                 cvec-size))

(define ((compressed-vec-ref bit-width mask) vec idx) 
  (define-values (word-idx bit-offset) (quotient/remainder (fx* bit-width idx) num-bits))
  (cond [(fx<= (fx+ bit-offset bit-width) num-bits)
         (fxand (fxrshift/logical (fxvector-ref (compressedvec-buffer vec) word-idx) bit-offset) mask)]
        [else
         (fxand (fxior (fxrshift/logical (fxvector-ref (compressedvec-buffer vec) word-idx) bit-offset)
                       (fxlshift/wraparound (fxvector-ref (compressedvec-buffer vec) (fx+ word-idx 1)) (fx- num-bits bit-offset)))
                mask)]))


(define ((compressed-vec-set! bit-width mask) vec idx val)
  (define-values (word-idx bit-offset) (quotient/remainder (fx* bit-width idx) num-bits))
  (cond [(fx<= (fx+ bit-offset bit-width) num-bits)
         (fxvector-set! (compressedvec-buffer vec)
                        word-idx
                        (fxior (fxand (fxvector-ref (compressedvec-buffer vec) word-idx)
                                      (fxnot (fxlshift/wraparound mask bit-offset)))
                               (fxlshift/wraparound val bit-offset)))]
        [else
         (define remaining-bits (fx- num-bits bit-offset))
         ;; Low part
         (fxvector-set! (compressedvec-buffer vec)
                        word-idx
                        (fxior (fxand (fxvector-ref (compressedvec-buffer vec) word-idx)
                                      (fx-/wraparound (fxlshift/wraparound 1 bit-offset) 1))
                               (fxlshift/wraparound val bit-offset)))
           
         ;; High part
         (define new-idx (fx+ word-idx 1))
         (fxvector-set! (compressedvec-buffer vec)
                        new-idx
                        (fxior (fxand (fxvector-ref (compressedvec-buffer vec) new-idx)
                                      (fxnot (fxrshift/logical mask remaining-bits)))
                               (fxrshift/logical val remaining-bits)))]))


(define ((compute-size-values bit-width) size)
  (define-values (num-fixnums-quotient num-fixnums-remainder)
    (quotient/remainder (fx* bit-width size) num-bits))
  (define num-fixnums-allocated
    (if (fx= 0 num-fixnums-remainder)
        num-fixnums-quotient
        (fx+ num-fixnums-quotient 1)))
  
  (values num-fixnums-allocated (fxquotient (fx* num-fixnums-allocated num-bits) bit-width)))


(define (create-compressedvec-functions bit-width)
  (unless (and (fx> bit-width 0)
               (fx<= bit-width num-bits))
    (error "too large for fixnums"))
  (define mask (make-mask bit-width))
  (define compute-size-func (compute-size-values bit-width))
  (values (make-compressed-vec compute-size-func)
          (compressed-vec-ref bit-width mask)
          (compressed-vec-set! bit-width mask)
          compute-size-func))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct gcfxvector (vec n) #:mutable)


(define (create-gcfxv-functions bit-width)
  (define-values (g-make-compressed-vec
                  g-compressed-vec-ref
                  g-compressed-vec-set!
                  g-compute-size-values)
    (create-compressedvec-functions bit-width))
  
  
  (define (make-gcfxvector capacity init-value)
    (define vec (g-make-compressed-vec capacity 0))
    (g-compressed-vec-set! vec 0 init-value)
    (gcfxvector vec 1))

  
  (define (gcfxvector-ref gv index)
    (g-compressed-vec-ref (gcfxvector-vec gv) index))

  
  (define (ensure-free-space-gcfxvec! gcfxv)
    (define-values (nums-allocated size) (g-compute-size-values (fx* 2 (compressedvec-size (gcfxvector-vec gcfxv)))))
    (define new-vec (make-fxvector nums-allocated 0))
    (for ([idx (in-range (fxvector-length (compressedvec-buffer (gcfxvector-vec gcfxv))))])
      (fxvector-set! new-vec idx (fxvector-ref (compressedvec-buffer (gcfxvector-vec gcfxv)) idx)))
    (set-compressedvec-buffer! (gcfxvector-vec gcfxv) new-vec)
    (set-compressedvec-size! (gcfxvector-vec gcfxv) size))
  
  
  (define (gcfxvector-add! gv item)
    (when (fx>= (gcfxvector-n gv) (compressedvec-size (gcfxvector-vec gv)))
      (ensure-free-space-gcfxvec! gv))
    (g-compressed-vec-set! (gcfxvector-vec gv) (gcfxvector-n gv) item)
    (set-gcfxvector-n! gv (fx+ (gcfxvector-n gv) 1)))

  
  (define (gcfxvector-add/copy gv item)
    (define new-vec
      (gcfxvector (compressedvec (fxvector-copy (compressedvec-buffer (gcfxvector-vec gv)))
                                 (compressedvec-size (gcfxvector-vec gv)))
                  (gcfxvector-n gv)))
    (gcfxvector-add! new-vec item)
    new-vec)
  
  (values make-gcfxvector
          gcfxvector-ref
          gcfxvector-add!
          gcfxvector-add/copy))