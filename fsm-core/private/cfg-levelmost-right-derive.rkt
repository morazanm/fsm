#lang racket/base

(require "cfg-struct.rkt"
         "constants.rkt"
         "queue.rkt"
         racket/treelist
         "yield-struct.rkt"
         "cyk.rkt"
         "chomsky.rkt"
         racket/list
         racket/set)

(provide cfg-derive-level-rightmost)


(define (treelist-insert-list tl i lst)
  (if (empty? lst)
      tl
      (treelist-insert-list (treelist-insert tl i (first lst)) (add1 i) (rest lst))))

(define (tlos->symbol l)
  (define (tlostr->string l)
    (if (treelist-empty? l)
        ""
        (string-append (treelist-first l) (tlostr->string (treelist-rest l)))))
  (define (tlos->tlostr ss)
    (treelist-map ss symbol->string))
  (string->symbol (tlostr->string (tlos->tlostr l))))

(define (cfg-derive-level-rightmost g w)
  (define alphabet-ht
    (list->seteq (cfg-get-alphabet g)))

  (define nt-to-rhss-ht
    (for/hasheq ([nt (in-list (cfg-get-v g))])
      (values nt (map cfg-rule-rhs (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) (cfg-get-the-rules g))))))
  
  (define treelist-w (list->treelist w))

  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt-helper st i)
    (if (= i -1)
        #f
        (if (set-member? alphabet-ht (treelist-ref (yield-state st) i))
            (treelist-ref (yield-state st) i)
            (get-last-nt-helper st (sub1 i)))))

  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st)
    (get-last-nt-helper st (yield-up st)))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals-rightmost st)
    (define (get-starting-terminals-rightmost-helper i)
      (if (= i (treelist-length st))
          st
          (if (hash-ref alphabet-ht (treelist-ref st (- (sub1 (treelist-length st)) i)) #f)
              (get-starting-terminals-rightmost-helper (add1 i))
              (if (= i 0)
                  (treelist)
                  (treelist-sublist st (- (treelist-length st) i) (treelist-length st))))))
    (get-starting-terminals-rightmost-helper 0))

  (define (get-last-n-terms w n)
    (if (= n 0) (treelist) (treelist-sublist w (- (treelist-length w) n) (treelist-length w))))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals-right? st)
    (let* ([start-terms-st (get-starting-terminals-rightmost st)]
           [start-terms-w (if (> (treelist-length start-terms-st) (treelist-length treelist-w))
                              #f
                              (get-last-n-terms treelist-w (treelist-length start-terms-st)))])
      (cond
        [(not start-terms-w) #f]
        [else (equal? start-terms-st start-terms-w)])))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals-leftmost st)
    (define (get-starting-terminals-helper i)
      (if (= i (treelist-length st))
          st
          (if (hash-ref alphabet-ht (treelist-ref st i) #f)
              (get-starting-terminals-helper (add1 i))
              (treelist-sublist st 0 i))))
    (get-starting-terminals-helper 0))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    (treelist-sublist w 0 n))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals-left? st)
    (let* ([start-terms-st (get-starting-terminals-leftmost st)]
           [start-terms-w (if (> (treelist-length start-terms-st) (treelist-length treelist-w))
                              #f
                              (get-first-n-terms treelist-w (treelist-length start-terms-st)))])
      (cond
        [(not start-terms-w) #f]
        [else (equal? start-terms-st start-terms-w)])))

  ; ASSUMPTION: yield has at least one NT in unprocessed field
  ;; (listof symbol) (listof symbol) (listof symbol) -> yield
  ;; Purpose: Replaces the leftmost nonterminal within the unprocessed field of a yield
  (define (subst-last-nt yd rght)
    (define (subst-last-nt-helper i)
      (if (= i -1)
          (subst-last-nt (yield (yield-state yd) (sub1 (treelist-length (yield-state yd)))) rght)
          (begin
            (displayln "here 0")
            (if (not (set-member? alphabet-ht (treelist-ref (yield-state yd) i)))
              (if (eq? (first rght) EMP)
                  (struct-copy yield yd [state (treelist-delete (yield-state yd) i)] [up (sub1 i)])
                  (struct-copy yield
                               yd
                               [state
                                (treelist-insert-list (treelist-delete (yield-state yd) i) i rght)]
                               [up (sub1 i)]))
              ;(treelist-append (treelist-sublist
              (subst-last-nt-helper (sub1 i))))))
    (subst-last-nt-helper (yield-up yd)))

  ;; (listof symbol) -> boolean
  ;; Purpose: Checks to see if there are any nonterminals within the given state
  (define (any-nt? state)
    (define (any-nt?-helper i)
      (if (= i (treelist-length state))
          #f
          (if (set-member? alphabet-ht (treelist-ref state i)) (any-nt?-helper (add1 i)) #t)))
    (any-nt?-helper 0))

  (define (count-terminals st)
    (treelist-length (treelist-filter (lambda (a) (set-member? alphabet-ht a)) st)))


  (define visited (mutable-set))
  (define derivs (make-queue))
  (define (make-deriv)
    
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (if (qempty? derivs)
        (format "~s is not in L(G)." w)
        (let ([current-deriv (dequeue! derivs)])
          (displayln (first current-deriv))
          (if (> (count-terminals (yield-state (first current-deriv))) (treelist-length treelist-w))
              (make-deriv)
              (let ([current-nt (get-last-nt (first current-deriv))])
                (if current-nt
                    (begin
                      (for/list ([rhs (in-list (hash-ref nt-to-rhss-ht current-nt))]
                                 #:do [(define new-state (subst-last-nt (first current-deriv) rhs))]
                                 #:when (and (not (set-member? visited (yield-state new-state)))
                                             (check-terminals-left? (yield-state new-state))
                                             (check-terminals-right? (yield-state new-state))))
                        (set-add! visited new-state)
                        (enqueue! derivs (cons new-state current-deriv)))
                      (make-deriv))
                    (if (equal? treelist-w (yield-state (first current-deriv)))
                        (apply append
                               (map (lambda (l)
                                      (if (equal? treelist-w (yield-state l))
                                          (if (null? (yield-state l))
                                              (list EMP)
                                              (list (tlos->symbol (yield-state l))))
                                          (list (tlos->symbol (yield-state l)) ARROW)))
                                    (reverse current-deriv)))
                        (if (any-nt? (yield-state (first current-deriv)))
                            (begin
                              (enqueue! derivs (cons (yield (yield-state (first current-deriv)) (sub1 (treelist-length (yield-state (first current-deriv)))))
                                                     current-deriv))
                              (make-deriv))
                            (make-deriv)))
                    ))))))
  (if (> (treelist-length treelist-w) 0)
      (if (cyk (chomsky g) w)
          (begin
            (enqueue! derivs (list (yield (treelist (cfg-get-start g)) 0)))
            (make-deriv))
          (format "~s is not in L(G)." w))
      (begin
        (enqueue! derivs (list (yield (treelist (cfg-get-start g)) 0)))
        (make-deriv))))

(define testcfg
  (make-unchecked-cfg
   '(S A B)
   '(a b c d)
   `((S ,ARROW ,EMP) (S ,ARROW AB) (A ,ARROW aSb) (B ,ARROW cBd) (A ,ARROW ,EMP) (B ,ARROW ,EMP))
   'S))