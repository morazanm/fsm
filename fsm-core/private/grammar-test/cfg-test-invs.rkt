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
         "../../../sm-graph.rkt"
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
           (cons #;(list unreversed-word ;<- gina change 
                       grammar-nt)
                 unreversed-word
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
  (list grammar-nt (find-configs #f #f '())))





(define (cfg-test-invs a-cfg max-length a-loi)
  (define inv-nts (map (λ (inv-pair) (first inv-pair)) a-loi)) ;; <- all nts that need to be tested
  (define state-pdas
    (map (lambda (start) (list start (cfg->state-pda a-cfg start))) (filter (λ (nt) (member nt inv-nts))  (cfg-get-v a-cfg))))
  
  (define (test-pdas pda-lst)
    (cond [(null? pda-lst)
           '()]
          [else
           (define pda-pair (car pda-lst))
           (cons (get-accepting-paths (cadr pda-pair) max-length (car pda-pair) (cadr (assoc (car pda-pair) a-loi)))
                 (test-pdas (cdr pda-lst)))]))
  (filter (λ (x) (not (empty? (second x)))) (test-pdas state-pdas)))




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

#;(cfg-test-invs numb>numa 15 (list (list 'S (lambda (x) #f))
                                  (list 'A (lambda (x) #f))))


;; syntactic categories
;; S: word is a palindrome
(define palindrome
  (make-cfg '(S)
            '(a b)
            `((S ,ARROW ,EMP)
              (S ,ARROW aSa)
              (S ,ARROW bSb)
              (S ,ARROW a)
              (S ,ARROW b))
            'S))

(define (S-INV w)
  (equal? w (reverse w)))



;; Syntactic categories
;; S: word is c^mb^na^nd^m
;; A: word is b^na^n
(define c^mb^na^nd^m
  (make-cfg '(S A)
            '(a b c d)
            `((S -> cSd)
              ;(S -> cAd)
              (S -> A)
              (A -> ,EMP)
              (A -> bAa))
            'S))

;;unit tests 
;(check-derive? c^mb^na^nd^m '() '(c d) '(b a) '(c b a d) '(c c b a d d))
;(check-not-derive? c^mb^na^nd^m '(c) '(a) '(b) '(d) '(c d d) '(a d) '(c b b a d))

;; invariants

;; word -> boolean
;; purpose: to determine if given word ought to be generated by S
(define (S-INV-c^mb^na^nd^m w)
  (let* ([Cs (takef w (λ (x) (eq? 'c x)))]
         [Bs (takef (drop w (length Cs)) (λ (x) (eq? 'b x)))]
         [As (takef (drop w (+ (length Bs) (length Cs))) (λ (x) (eq? 'a x)))]
         [Ds (takef (drop w (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? 'd x)))])
    (and (append Cs Bs As Ds)
         (= (length Cs) (length Ds))
         (= (length Bs) (length As)))))


(define (S-INV-c^mb^na^nd^m-buggy w)
  (let* ([Cs (takef w (λ (x) (eq? 'c x)))]
         [Bs (takef (drop w (length Cs)) (λ (x) (eq? 'b x)))]
         [As (takef (drop w (+ (length Bs) (length Cs))) (λ (x) (eq? 'b x)))]
         [Ds (takef (drop w (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? 'd x)))])
    (and (append Cs Bs As Ds)
         (= (length Cs) (length Ds))
         (= (length Bs) (length As)))))
  

;; word -> boolean
;; purpose: to determine if given word ought to be generated by A
(define (A-INV-c^mb^na^nd^m w)
  (let* ([Bs (takef w (λ (x) (eq? 'b x)))]
         [As (takef (drop w (length Bs)) (λ (x) (eq? 'a x)))])
    (and (append Bs As)
         (= (length Bs) (length As)))))
  






(define (order-output results)
    ;; have to get the states of the results
    (define state-set (mutable-set))
  
    (for ([pair (in-list results)])
      (set-add! state-set (second pair)))  ;<-- adds all the states in the set

    ;; making the lists for the states
  
    ;; (listof symbol) -> (listof (symbol (listof word)))
    ;; Purpose: To generate a list of lists of a state and an empty list for each given states
    (define (build-list-of-states-&-empty-low states)
      ;; (listof symbol) (listof word)-> (listof (symbol '()))
      ;; Purpose: To generate a list of lists of a state and an empty list for each given states
      ;; Accumulator Invariant: accum = list containing a list with the state and an empty list of words
      (define (build-list-of-states-&-empty-low-helper states accum)
        (if (null? states) accum
            (build-list-of-states-&-empty-low-helper (cdr states) (cons (list (car states) '()) accum))))
      (build-list-of-states-&-empty-low-helper states '()))
  
    (define states-&-empty-low (build-list-of-states-&-empty-low (set->list state-set)))

    ;; now adding in all the words to the lists

    ;; (listof (listof word state)) -> (listof (listof state (listof word)))
    ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
    (define (sort-words listof-all-words-&-states)
      ;; (listof (word state)) -> (listof word)
      ;; Purpose: To make the given (listof (word state)) into a list of words
      (define (make-low list-of-words-&-states)
        ;; (listof (word state)) -> (listof word)
        ;; Purpose: To make the given (listof (word state)) into a list of words
        ;; Accumulator Invariant: accum = current list of words for the state
        (define (make-low-helper list-of-words-&-states accum)
          (if (null? list-of-words-&-states) accum
              (make-low-helper (cdr list-of-words-&-states) (cons (car (car list-of-words-&-states)) accum))))
        (make-low-helper list-of-words-&-states '()))
      ;; (listof (listof word state)) -> (listof (listof state (listof word)))
      ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
      ;; Accumulator Invairant: accum = list of states with all the possible words in them 
      (define (sort-words-helper states-&-empty-low accum)
        (if (null? states-&-empty-low) accum
            (sort-words-helper (cdr states-&-empty-low)
                               (cons (list (car (car states-&-empty-low))
                                           (make-low (remove-duplicates
                                                      (filter (λ (x) (eq? (car (car states-&-empty-low)) (cadr x)))
                                                              listof-all-words-&-states)))) accum))))
            
      (sort-words-helper states-&-empty-low '()))

    (sort-words results))


#;(map order-output (cfg-test-invs c^mb^na^nd^m 15 (list (list 'S S-INV-c^mb^na^nd^m-buggy)
                                     (list 'A A-INV-c^mb^na^nd^m)
                                  )))

#;(cfg-test-invs c^mb^na^nd^m 15 (list (list 'S S-INV-c^mb^na^nd^m)
                                     (list 'A A-INV-c^mb^na^nd^m)
                                  ))