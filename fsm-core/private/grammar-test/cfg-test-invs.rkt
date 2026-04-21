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
         #;"../testing-utils/structs.rkt"
         (filtered-in
          (λ (name)
            (cond [(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          "../testing-utils/unsafe-struct-accessors.rkt")
         "../cfg-struct.rkt"
         "../cfg-constructors.rkt"
         "../constants.rkt"
         "../pda.rkt"
         "../misc.rkt"
         racket/set
         racket/list
         data/queue)
(provide cfg-test-invs)

(define (pred a-stack next-rules)
  (or (eq? (PDA-rule-pop next-rules) EMP)
      (and (fx<= (PDA-rule-pop-length next-rules) (stack-len a-stack))
           (equal? (take (stack-elems a-stack)
                         (PDA-rule-pop-length next-rules))
                   (PDA-rule-pop next-rules)))))

(define (cfg->state-pda a-cfg new-start)
  (define old-nts (list->seteq (cons EMP (cfg-get-v a-cfg))))
  (define new-rules (mutable-set))
  (define new-nts (mutable-seteq))

  (define (found-new-rule rules)
    (cond [(null? rules)
           (find-new-rule (cfg-get-rules a-cfg))]
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
           (found-new-rule (cdr rules))]))
  
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
  (find-new-rule (cfg-get-rules a-cfg))
        
  (define new-cfg
    (make-unchecked-cfg
     (set->list new-nts)
     (cfg-get-alphabet a-cfg)
     (set->list new-rules)
     new-start))
  (define new-state-pda
    (cfg->pda new-cfg))
  new-state-pda)

(define (rule->PDA-rule rule)
  (PDA-rule (car (car rule))
            (cadr (car rule))
            (caddr (car rule))
            (car (cadr rule))
            (cadr  (cadr  rule))
            (if (eq? EMP (cadr (cadr rule)))
                0
                (length (cadr (cadr rule))))
            (if (eq? EMP (caddr (car rule)))
                0
                (length (caddr (car rule))))))

(define (make-new-stack new-rule curr-config-stack)
  (if (eq? (PDA-rule-push new-rule) EMP)
      (if (eq? (PDA-rule-pop new-rule) EMP)
          curr-config-stack
          (stack (drop (stack-elems curr-config-stack)
                       (PDA-rule-pop-length new-rule))
                 (fx- (stack-len curr-config-stack)
                      (PDA-rule-pop-length new-rule))))
      (if (eq? (PDA-rule-pop new-rule) EMP)
          (stack (append (PDA-rule-push new-rule)
                         (stack-elems curr-config-stack))
                 (fx+ (stack-len curr-config-stack)
                      (PDA-rule-push-len new-rule)))
          (stack (append (PDA-rule-push new-rule)
                         (drop (stack-elems curr-config-stack)
                               (PDA-rule-pop-length new-rule)))
                 (fx+ (stack-len curr-config-stack)
                      (fx- (PDA-rule-push-len new-rule)
                           (PDA-rule-pop-length new-rule)))))))

(define (make-new-word new-rule curr-config-word)
  (if (eq? EMP (PDA-rule-read new-rule))
      curr-config-word
      (cons (PDA-rule-read new-rule) curr-config-word)))

(define (make-new-config new-rule curr-config-stack curr-config-word)
  (config (PDA-rule-destination new-rule)
          (make-new-word new-rule curr-config-word)
          (make-new-stack new-rule curr-config-stack)))

(define (get-accepting-paths a-pda max-length grammar-nt inv)
  (define queue (make-queue))
  (define visited (mutable-set))
  (define pda-finals (list->seteq (pda-getfinals a-pda)))
  (define pda-rules (map rule->PDA-rule (pda-getrules a-pda)))
  
  (define rule-ht
    (for/hasheq ([state (in-list (pda-getstates a-pda))])
      (values state
              (for/list ([a-rule (in-list pda-rules)]
                         #:when (eq? state (PDA-rule-source a-rule)))
                a-rule))))

  (define (test-accepting-computation a-path accum)
    (define unreversed-word (reverse (config-word a-path)))
    (cond [(inv unreversed-word)
           accum]
          [else
           (cons (list unreversed-word
                       grammar-nt)
                 accum)]))

  (define (enqueue-new-configs a-computation next-rules accum)
    (define a-config (computation-loc a-computation))
    (define new-config
      (make-new-config (car next-rules)
                       (config-stack a-config)
                       (config-word a-config)))
    (cond [(set-member? visited new-config)
           (found-new-config a-computation (cdr next-rules) accum)]
          [else
           (set-add! visited new-config)
           (enqueue! queue (computation
                            new-config
                            (fx+ 1 (computation-length a-computation))))
           (found-new-config a-computation (cdr next-rules) accum)]))

  (define (found-new-config a-computation next-rules accum)
    (cond [(null? next-rules) (find-configs a-computation next-rules accum)]
          [(pred (config-stack (computation-loc a-computation)) (car next-rules))
           (enqueue-new-configs a-computation next-rules accum)]
          [else
           (found-new-config a-computation (cdr next-rules) accum)]))

  (define (searching-for-new-config a-computation next-rules accum)
    (define qfirst (computation-loc a-computation))
    (cond [(null? next-rules)
           (cond [(and (set-member? pda-finals (config-state qfirst))
                       (null? (stack-elems (config-stack qfirst))))
                  (find-configs a-computation next-rules (test-accepting-computation (computation-loc a-computation) accum))]
                 [else (find-configs a-computation next-rules accum)])]
          [(pred (config-stack qfirst) (car next-rules))
           (enqueue-new-configs a-computation next-rules accum)]
          [else (searching-for-new-config a-computation (cdr next-rules) accum)]))
  
  (define (find-configs curr-computation next-rules accum)
    (cond [(queue-empty? queue)
           accum]
          [else
           (define qfirst (dequeue! queue))
           (define another-qfirst (computation-loc qfirst))
           (cond [(fx= max-length (computation-length qfirst))
                  (cond [(and (null? (stack-elems (config-stack another-qfirst)))
                              (set-member? pda-finals (config-state another-qfirst)))
                         (find-configs curr-computation next-rules (test-accepting-computation (computation-loc qfirst) accum))]
                        [else
                         (find-configs curr-computation next-rules accum)])]
                 [else
                  (searching-for-new-config qfirst
                                            (hash-ref rule-ht (config-state another-qfirst))
                                            accum)])]))
  
  (for ([i (in-list pda-rules)]
        #:when (eq? (PDA-rule-source i) (pda-getstart a-pda)))
    (define new-config
      (config (PDA-rule-destination i)
              (if (eq? EMP (PDA-rule-read i))
                  '()                                 ;; <- the current word of the path 
                  (list (PDA-rule-read i)))
              (if (eq? EMP (PDA-rule-push i))  ;;  ⚙️making/adding the first paths to the queue
                  (stack '() 0)
                  (stack (PDA-rule-push i) (PDA-rule-push-len i)))))
    (set-add! visited
              new-config)
    (enqueue! queue (computation
                     new-config
                     1)))
  (find-configs #f #f '()))

(define (cfg-test-invs a-cfg max-length a-loi)
  (define state-pdas
    (map (lambda (start) (list start (cfg->state-pda a-cfg start))) (cfg-get-v a-cfg)))
  
  (define (test-pdas pda-lst)
    (cond [(null? pda-lst)
           '()]
          [else
           (define pda-pair (car pda-lst))
           (cons (get-accepting-paths (cadr pda-pair) max-length (car pda-pair) (cadr (assoc (car pda-pair) a-loi)))
                 (test-pdas (cdr pda-lst)))]))
  (test-pdas state-pdas))

(define numb>numa
  (make-cfg '(S A)
            '(a b)
            `((S ,ARROW b)
              (S ,ARROW AbA)
              (A ,ARROW AaAbA)
              (A ,ARROW AbAaA)
              (A ,ARROW ,EMP)
              (A ,ARROW bA))
            'S))

(time (cfg-test-invs numb>numa 15 (list (list 'S (lambda (x) #t))
                                        (list 'A (lambda (x) #t)))))