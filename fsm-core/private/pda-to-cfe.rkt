#lang rosette

(require (for-syntax racket/base
                     racket/set)
         racket/require
         #;racket/fixnum
         (filtered-in
          (λ (name)
            (define unsafe-methods-used
              (set "unsafe-vector*-ref"
                   "unsafe-vector*-set!"
                   "unsafe-vector*-length"
                   "unsafe-fxvector-ref"
                   "unsafe-fxvector-set!"
                   "unsafe-fxvector-length"
                   "unsafe-car"
                   "unsafe-cdr"))
            (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
                  [(regexp-match #rx"^unsafe-cons-list" name) "cons"]
                  [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          racket/unsafe/ops)
         (only-in racket/fixnum for/fxvector in-fxvector)
         #;"testing-utils/structs.rkt"
         (filtered-in
          (λ (name)
            (cond [(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          "testing-utils/unsafe-struct-accessors.rkt")
         "constants.rkt"
         "testing-utils/unsafe-packed-vector.rkt"
         "testing-utils/work-stealing-deque.rkt"
         "pda.rkt"
         "misc.rkt"
         racket/set
         racket/list
         data/queue)

(define (determine-num-bits-needed num-rules)
  (define (helper num)
    (if (>= (expt 2 num) num-rules)
        num
        (helper (add1 num))))
  (define res (helper 1))
  (if (byte? res)
      res
      (error "idk")))

(struct rule-struct (start-state read-elem dest-state push-expr pop-expr) #:transparent)
(struct path-with-rep-count (path rep-counts) #:transparent)


(define (rule->rule-struct rule symb-ht)
  (rule-struct (car (car rule))
               (cadr (car rule))
               (car (cadr rule))
               (for/fold ([curr-equation 0])
                         ([push-elem (in-list (cadr (cadr rule)))])
                 (+ curr-equation
                    (* (hash-ref symb-ht (car (car rule)))
                       (hash-ref symb-ht push-elem))))
               (for/fold ([curr-equation 0])
                         ([pop-elem (in-list (caddr (car rule)))])
                 (- curr-equation
                    (* (hash-ref symb-ht (car (car rule)))
                       (hash-ref symb-ht pop-elem))))))

(define (sm-test-invs-fsa a-machine rep-limit)
  (define rules-len (length (pda-getrules a-machine)))
  (define finals-set (list->seteq (pda-getfinals a-machine)))
  (define-values (make-gcfxvector
                  gcfxvector-ref
                  gcfxvector-add!
                  gcfxvector-add/copy)
    (create-gcfxv-functions (determine-num-bits-needed rules-len)))
  
  (define rules
    (for/vector #:length rules-len
      ([rule (in-list (pda-getrules a-machine))])
      (rule-struct (car rule) (cadr rule) (caddr rule) )))
           
  (define rule-idx-from-final
    (for/hasheq ([rule (in-vector rules)])
      (values (unsafe-struct*-ref rule 2)
              (for/fxvector ([a-rule (in-vector rules)]
                             [idx (in-naturals)]
                             #:when (eq? (unsafe-struct*-ref rule 2)
                                         (unsafe-struct*-ref a-rule 0)))
                idx))))
           
  (define (word-of-path a-gv)
    (define (word-of-path-helper accum idx)
      (if (fx= idx 0)
          (if (eq? 'ε (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv 0)) 1))
              accum
              (cons (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv 0)) 1) accum))
          (if (eq? 'ε (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv idx)) 1))
              (word-of-path-helper accum (fx- idx 1))
              (word-of-path-helper (cons (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv idx)) 1) accum)
                                   (fx- idx 1)))))
    (word-of-path-helper '() (fx- (unsafe-struct*-ref a-gv 1) 1)))

           
  (define (create-new-words stack qfirst)
    (define final-state
      (unsafe-struct*-ref
       (vector*-ref rules
                    (gcfxvector-ref (unsafe-struct*-ref qfirst 0)
                                    (fx- (unsafe-struct*-ref (unsafe-struct*-ref qfirst 0) 1) 1)))
       2))
                    
    (for ([idx (in-fxvector (hash-ref rule-idx-from-final final-state))]
          #:do [(define curr-rep-count (bytes-ref (unsafe-struct*-ref qfirst 1)
                                                  idx))]
          #:when (fx< curr-rep-count
                      rep-limit))
      (define len (bytes-length (unsafe-struct*-ref qfirst 1)))
      (define new-vec (make-bytes len 0))
      (bytes-copy! new-vec 0 (unsafe-struct*-ref qfirst 1) 0 len)
      (bytes-set! new-vec idx (fx+ curr-rep-count 1))
      (push! stack
             (path-with-rep-count (gcfxvector-add/copy (unsafe-struct*-ref qfirst 0) idx)
                                  new-vec)))
    (define word-path (word-of-path (unsafe-struct*-ref qfirst 0)))
    (if (set-member? finals-set final-state)
        (unsafe-struct*-ref qfirst 0)
        #;(if (inv word-path)
              #f
              (list nt-tested word-path))
        #f))

  (define (accumulate-results-into-lst vec)
    (define (loop idx accum)
      (if (fx= idx (vector*-length vec))
          accum
          (loop (fx+ idx 1) (append (vector*-ref vec idx) accum))))
    (loop 0 '()))
           
  (accumulate-results-into-lst
   (run-parallel (for/list ([rule (in-vector rules)]
                            [idx (in-naturals)]
                            #:when (eq? (unsafe-struct*-ref rule 0) (pda-getstart a-machine)))
                   (define vec (make-bytes rules-len 0))
                   (bytes-set! vec idx 1)
                   (path-with-rep-count (make-gcfxvector 16 idx) vec))
                 create-new-words
                 2048)))


(define (calculate-nums a-pda)
  (define symb-ht (make-hash))
  (for ([gamma-elem (in-list (pda-getgamma a-pda))])
    (define-symbolic* _ integer?)
    (hash-set! symb-ht gamma-elem _))
  (for ([state-elem (in-list (pda-getstates a-pda))])
    (define-symbolic* _ integer?)
    (hash-set! symb-ht state-elem _))
  (define all-paths (sm-test-invs-fsa a-pda (error "need to remember to get rid of rep limit first")))
  
  (define (create-assertion-from-path a-path)
    (= (for/fold ([curr-equation 0])
                 ([rule (in-list a-path)])
         (+ curr-equation
            (rule-struct-push-expr rule)
            (rule-struct-pop-expr rule)))
       0))
  
  (define num-lst
    (for/list ([a-path (in-list all-paths)])
    (define res-ht
      (model (synthesize #:forall (for/list ([gamma-elem (in-list (pda-getgamma a-pda))])
                                    (hash-ref symb-ht gamma-elem))
                         #:guarantee
                         (begin
                           (for ([state-elem (in-list (pda-getstates a-pda))])
                             (assume (>= (hash-ref symb-ht state-elem) 1)))
                           (assert (create-assertion-from-path a-path))))))))

  (define path-cfes
    (for/list ([a-num-res (in-list num-lst)])
      
  )