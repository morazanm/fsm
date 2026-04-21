#lang racket/base

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
         #;"../testing-utils/structs.rkt"
         (filtered-in
          (λ (name)
            (cond [(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          "../testing-utils/unsafe-struct-accessors.rkt")
         ;"../cfg-struct.rkt"
         ;"../cfg-constructors.rkt"
         "../rg-constructors.rkt"
         "../regular-grammar.rkt"
         "../constants.rkt"
         ;"../pda.rkt"
         "../fsa.rkt"
         "../testing-utils/unsafe-packed-vector.rkt"
         "../testing-utils/work-stealing-deque.rkt"
         "../misc.rkt"
         racket/set
         racket/list
         data/queue)
(provide rg-test-invs)

(define (determine-num-bits-needed num-rules)
  (define (helper num)
    (if (>= (expt 2 num) num-rules)
        num
        (helper (add1 num))))
  (define res (helper 1))
  (if (byte? res)
      res
      (error "idk")))

(define (rg->state-ndfa a-rg new-start)
  (define old-nts (list->seteq (cons EMP (rg-getnts a-rg))))
  (define new-rules (mutable-set))
  (define new-nts (mutable-seteq))

  (define (found-new-rule rules)
    (cond [(null? rules)
           (find-new-rule (rg-getunparsedrules a-rg))]
          [else
           (define first-rule (car rules))
           (define lhs-nt (car first-rule))
           (cond [(and (set-member? new-nts lhs-nt)
                       (not (set-member? new-rules first-rule)))
                  (set-add! new-rules first-rule)
                  (set-add! new-nts lhs-nt)
                  (for ([symb (in-list (symbol->fsmlos (caddr first-rule)))]
                        #:when (set-member? old-nts symb))
                    (when (not (eq? symb EMP))
                      (set-add! new-nts symb)))
                  (found-new-rule (cdr rules))]
                 [else
                  (found-new-rule (cdr rules))])]))
  
  (define (find-new-rule rules)
    (cond [(null? rules)
           (void)]
          [(and (set-member? new-nts (caar rules))
                (not (set-member? new-rules (car rules))))
           (set-add! new-rules (car rules))
           (set-add! new-nts (caar rules))
           (for ([symb (in-list (symbol->fsmlos (caddar rules)))]
                 #:when (set-member? old-nts symb))
             (when (not (eq? symb EMP))
               (set-add! new-nts symb)))
           (found-new-rule (cdr rules))]
          [else
           (find-new-rule (cdr rules))]))
  (set-add! new-nts new-start)
  (find-new-rule (rg-getunparsedrules a-rg))
        
  (define new-rg
    (make-unchecked-rg
     (set->list new-nts)
     (rg-getalphabet a-rg)
     (set->list new-rules)
     new-start))
  (define new-state-pda
    (rg->fsa new-rg))
  new-state-pda)

(struct rule-struct (start-state read-elem dest-state) #:transparent)
(struct path-with-rep-count (path rep-counts) #:transparent)

(define (sm-test-invs-fsa a-machine rep-limit nt-tested inv)
  (define rules-len (length (fsa-getrules a-machine)))
  (define finals-set (list->seteq (fsa-getfinals a-machine)))
  (define-values (make-gcfxvector
                  gcfxvector-ref
                  gcfxvector-add!
                  gcfxvector-add/copy)
    (create-gcfxv-functions (determine-num-bits-needed rules-len)))
  
  (define rules
    (for/vector #:length rules-len
      ([rule (in-list (fsa-getrules a-machine))])
      (rule-struct (car rule) (cadr rule) (caddr rule))))
           
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
        (if (inv word-path)
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
                            #:when (eq? (unsafe-struct*-ref rule 0) (fsa-getstart a-machine)))
                   (define vec (make-bytes rules-len 0))
                   (bytes-set! vec idx 1)
                   (path-with-rep-count (make-gcfxvector 16 idx) vec))
                 create-new-words
                 2048)))

(define (rg-test-invs a-rg a-loi)
  (define state-fsas
    (map (lambda (start) (list start (rg->state-ndfa a-rg start))) (rg-getnts a-rg)))
  (displayln state-fsas)
  (define (test-rgs rg-lst)
    (cond [(null? rg-lst)
           '()]
          [else
           (define rg-pair (car rg-lst))
           (cons (sm-test-invs-fsa (cadr rg-pair) 2 (car rg-pair) (cadr (assoc (car rg-pair) a-loi)))
                 (test-rgs (cdr rg-lst)))]))
  (test-rgs state-fsas))

(define rg-STARTS-WITH-aa (make-rg '(S A B) 
                                   '(a b) 
                                   '((S -> aA)
                                     (A -> a)
                                     (A -> aB)
                                     (B -> a)
                                     (B -> aB)
                                     (B -> b)
                                     (B -> bB))
                                   'S))
(rg-test-invs rg-STARTS-WITH-aa 2 (list (list 'S (lambda (x) #f))
                                        (list 'A (lambda (x) #f))
                                        (list 'B (lambda (x) #f))))