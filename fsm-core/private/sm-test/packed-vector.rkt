#lang typed/racket/base #:with-refinements
(require typed/racket/unsafe
         (for-syntax typed/racket/base
                     syntax/parse
                     racket/fixnum))

(provide create-compressedvec-functions
                (struct-out compressedvec)
                create-gcfxv-functions
                (struct-out gcfxvector))

(require/typed racket/fixnum
                      [fxlshift/wraparound (-> Fixnum Fixnum Fixnum)]
                      [fxlshift (-> Fixnum Fixnum Fixnum)]
                      [fxrshift/logical (-> Fixnum Fixnum Fixnum)]
                      [fxand (-> Fixnum Fixnum Fixnum)]
                      [fxior (-> Fixnum Fixnum Fixnum)]
                      [fxnot (-> Fixnum Fixnum)]
                      [fxvector-copy (-> FxVector FxVector)]
                      [fx-/wraparound (-> Fixnum Fixnum Fixnum)]
                      [fx- (-> Fixnum Fixnum Fixnum)]
                      [make-fxvector (-> Index Fixnum FxVector)]
                      [fxvector-set! (-> FxVector Fixnum Fixnum Void)]
                      [fxvector-length (-> FxVector Index)]
                      [in-fxvector (-> FxVector (Sequenceof Fixnum))]
                      [fxvector-ref (-> FxVector Index Fixnum)])

;; Only implemented for little endian currently since I do not have big endian hardware to test on


(struct compressedvec ([buffer : FxVector]
                       [size : Fixnum])
  #:mutable)

(begin-for-syntax
  (: determine-number-of-bits-in-fixnum (-> (U 61 63)))
  (define (determine-number-of-bits-in-fixnum)
    (cond [(= (most-positive-fixnum) (sub1 (expt 2 60)))
           61]
          [(= (most-positive-fixnum) (sub1 (expt 2 62)))
           63]
          [else (error "Something has gone terribly wrong")]))
  (define precompute-num-bits
    (determine-number-of-bits-in-fixnum)))

(define-syntax (num-bits stx)
  #`(ann #,(determine-number-of-bits-in-fixnum) (Refine [n : Byte] (= n #,precompute-num-bits))))

(define-syntax (bit-width-ann-macro stx)
  (syntax-parse stx
    [(_ bit-width)
     #`(ann bit-width (Refine [n : Byte] (<= n #,precompute-num-bits)))]))



(: make-mask (-> Fixnum Fixnum))
(define (make-mask bit-width)
  (fx- (fxlshift 1 bit-width) 1))

(: make-compressed-vec (-> (-> Fixnum (Values Index Fixnum)) (-> Fixnum Fixnum compressedvec)))
(define ((make-compressed-vec create-vals-func) size init-val)
  (define-values (num-fixnums-allocated cvec-size)
    (create-vals-func size))
  (compressedvec (make-fxvector num-fixnums-allocated
                                init-val)
                 cvec-size))

(define-syntax (bit-offset-ann-macro stx)
  (syntax-parse stx
    [(_ bit-offset)
     #`(ann bit-offset (Refine [n : Byte] (and (> n 0) (< n #,precompute-num-bits))))]))


(define-syntax (compressed-vec-ref-type-macro stx)
  (syntax-parse stx
    [(_)
     #`(: compressed-vec-ref (-> (Refine [n : Byte] (and (> n 0) (<= n #,precompute-num-bits))) Fixnum (-> compressedvec Integer Fixnum)))]))
(compressed-vec-ref-type-macro)
(define ((compressed-vec-ref bit-width mask) vec idx) 
  (define-values (word-idx bit-offset) (quotient/remainder (* (bit-width-ann-macro bit-width) idx) num-bits))
  (unless (index? word-idx)
    (error "idk"))
  (cond [(<= (+ bit-offset (bit-width-ann-macro bit-width)) num-bits)
         (fxand (fxrshift/logical (fxvector-ref (compressedvec-buffer vec) word-idx) bit-offset) mask)]
        [else
         (define new-idx (add1 word-idx))
         (define new-offset (fx- num-bits bit-offset))
         (unless (and (fixnum? new-offset)
                      (index? new-idx))
           (error "idk1"))
         (fxand (fxior (fxrshift/logical (fxvector-ref (compressedvec-buffer vec) word-idx) bit-offset)
                       (fxlshift/wraparound (fxvector-ref (compressedvec-buffer vec) new-idx) new-offset))
                mask)]))

(: compressed-vec-set! (-> Byte Fixnum (-> compressedvec Index Fixnum Void)))
(define ((compressed-vec-set! bit-width mask) vec idx val)
  (define-values (word-idx bit-offset) (quotient/remainder (* bit-width idx) num-bits))
  (unless (index? word-idx)
    (error "idk"))
  (cond [(<= (+ bit-offset bit-width) num-bits)
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
         (define new-idx (add1 word-idx))
         (unless (and (fixnum? remaining-bits)
                      (index? new-idx))
           (error "idk1"))
         (fxvector-set! (compressedvec-buffer vec)
                        new-idx
                        (fxior (fxand (fxvector-ref (compressedvec-buffer vec) new-idx)
                                      (fxnot (fxrshift/logical mask remaining-bits)))
                               (fxrshift/logical val remaining-bits)))]))

(: compute-size-values (-> Byte (-> Fixnum (Values Index Fixnum))))
(define ((compute-size-values bit-width) size)
  (define-values (num-fixnums-quotient num-fixnums-remainder)
    (quotient/remainder (* bit-width size) num-bits))
  (define num-fixnums-allocated
    (if (= 0 num-fixnums-remainder)
        num-fixnums-quotient
        (add1 num-fixnums-quotient)))
  (define cvec-size (quotient (* num-fixnums-allocated num-bits) bit-width))
  (unless (index? num-fixnums-allocated)
    (error "idk"))
  (unless (fixnum? cvec-size)
    (error "idk"))
  (values num-fixnums-allocated cvec-size))

(: create-compressedvec-functions (-> Byte (Values (-> Fixnum Fixnum compressedvec)
                                                   (-> compressedvec Index Fixnum)
                                                   (-> compressedvec Index Fixnum Void)
                                                   (-> Fixnum (Values Index Fixnum)))))
(define (create-compressedvec-functions bit-width)
  (unless (and (> bit-width 0)
               (<= bit-width num-bits))
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

(struct gcfxvector ([vec : compressedvec] [n : Positive-Index]) #:mutable)

(: create-gcfxv-functions (-> Byte (Values (-> Positive-Index Fixnum gcfxvector)
                                           (-> gcfxvector Index Fixnum)
                                           (-> gcfxvector Fixnum Void)
                                           (-> gcfxvector Fixnum gcfxvector))))
(define (create-gcfxv-functions bit-width)
  (define-values (g-make-compressed-vec
                  g-compressed-vec-ref
                  g-compressed-vec-set!
                  g-compute-size-values)
    (create-compressedvec-functions bit-width))
  
  (: make-gcfxvector (-> Positive-Index Fixnum gcfxvector))
  (define (make-gcfxvector capacity init-value)
    (define vec (g-make-compressed-vec capacity 0))
    (g-compressed-vec-set! vec 0 init-value)
    (gcfxvector vec 1))

  (: gcfxvector-ref (-> gcfxvector Index Fixnum))
  (define (gcfxvector-ref gv index)
    (g-compressed-vec-ref (gcfxvector-vec gv) index))

  (: ensure-free-space-gcfxvec! (-> gcfxvector Void))
  (define (ensure-free-space-gcfxvec! gcfxv)
    (define new-vec-size (* 2 (compressedvec-size (gcfxvector-vec gcfxv))))
    (cond [(index? new-vec-size)
           (define-values (nums-allocated size) (g-compute-size-values new-vec-size))
           (define new-vec (make-fxvector nums-allocated 0))
           (for ([elem : Fixnum (in-fxvector (compressedvec-buffer (gcfxvector-vec gcfxv)))]
                 [idx : Nonnegative-Fixnum (in-range (fxvector-length (compressedvec-buffer (gcfxvector-vec gcfxv))))])
             (fxvector-set! new-vec idx elem))
           (set-compressedvec-buffer! (gcfxvector-vec gcfxv) new-vec)
           (set-compressedvec-size! (gcfxvector-vec gcfxv) size)]
          [else (error "Too large")]))
  
  (: gcfxvector-add! (-> gcfxvector Fixnum Void))
  (define (gcfxvector-add! gv item)
    (define new-curr-posn (add1 (gcfxvector-n gv)))
    (cond [(index? new-curr-posn)
           (when (>= (gcfxvector-n gv) (compressedvec-size (gcfxvector-vec gv)))
             (ensure-free-space-gcfxvec! gv))
           (g-compressed-vec-set! (gcfxvector-vec gv) (gcfxvector-n gv) item)
           (set-gcfxvector-n! gv new-curr-posn)]
          [else (error "Vector size is too large")]))

  (: gcfxvector-add/copy (-> gcfxvector Fixnum gcfxvector))
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