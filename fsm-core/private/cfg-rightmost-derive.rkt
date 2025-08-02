#lang racket/base
(require racket/treelist
         "cfg-struct.rkt"
         "cyk.rkt"
         "chomsky.rkt"
         "constants.rkt"
         "queue.rkt"
         racket/set)

(provide cfg-derive-rightmost)

(define (cfg-derive-rightmost g w)
  (define nt-set (list->seteq (cfg-get-v g)))
  (define nt-to-rhss-ht
    (for/hasheq ([nt (in-list (cfg-get-v g))])
      (values nt (map cfg-rule-rhs (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) (cfg-get-the-rules g))))))
  
  (define treelist-w (list->treelist w))
  
  (define (get-last-nt-helper st i)
    (if (= i -1)
        #f
        (if (set-member? nt-set (treelist-ref st i))
            (treelist-ref st i)
            (get-last-nt-helper st (sub1 i)))))
  
  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st)
    (get-last-nt-helper st (sub1 (treelist-length st))))

  (define (treelist-insert-list tl i lst)
    (if (null? lst)
        tl
        (treelist-insert-list (treelist-insert tl i (car lst)) (add1 i) (cdr lst))))
  
  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol) (listof symbol) -> (listof symbol)
  ;; Purpose: Replaces the rightmost nonterminal with a righthand side of a rule
  (define (subst-last-nt-helper st rght i)
    (if (= i -1)
        (treelist-insert-list (treelist-delete st 0) 0 rght)
        (if (not (member (treelist-ref st i) (cfg-get-alphabet g)))
            (if (eq? (car rght) EMP)
                (treelist-delete st i)
                ;(subst-last-nt-helper st rght (sub1 i))
                (treelist-insert-list (treelist-delete st i) i rght))
            ;(treelist-append (treelist-sublist
            (subst-last-nt-helper st rght (sub1 i)))))

  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (subst-last-nt st rght)
    (subst-last-nt-helper st rght (sub1 (treelist-length st))))

  (define (get-starting-terminals-rightmost st)
    (define (get-starting-terminals-rightmost-helper st i)
      (if (= i (treelist-length st))
          st
          (if (set-member? nt-set (treelist-ref st (- (sub1 (treelist-length st)) i)))
              (if (= i 0)
                  (treelist)
                  (treelist-sublist st (- (treelist-length st) i) (treelist-length st)))
              (get-starting-terminals-rightmost-helper st (add1 i)))))
    (get-starting-terminals-rightmost-helper st 0))

  (define (get-last-n-terms w n)
    (if (= n 0)
        (treelist)
        (treelist-sublist w (- (treelist-length w) n) (treelist-length w))))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let ([start-terms-st (get-starting-terminals-rightmost st)])
      (and (not (> (treelist-length start-terms-st) (treelist-length treelist-w)))
           (equal? start-terms-st (get-last-n-terms treelist-w (treelist-length start-terms-st))))))
  
  (define (tlos->symbol l)
    (define (tlostr->string l)
      (if (treelist-empty? l)
          ""
          (string-append (treelist-first l) (tlostr->string (treelist-rest l)))))
    (define (tlos->tlostr ss)
      (treelist-map ss symbol->string))
    (string->symbol (tlostr->string (tlos->tlostr l))))

  (define visited (mutable-set))
  (define derivs (make-queue))
  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv)
    (if (qempty? derivs)
        (format "~s is not in L(G)." w)
        (let* ([fderiv (dequeue! derivs)]
               [fnt (get-last-nt (car fderiv))])
          (format "~s is not in L(G)." w)
          (if fnt
              (begin
                (for ([rhs (in-list (hash-ref nt-to-rhss-ht fnt))]
                      #:do [(define new-state (subst-last-nt (car fderiv) rhs))]
                      #:when (and (not (set-member? visited new-state))
                                  (check-terminals? new-state)))
                  (set-add! visited new-state)
                  (enqueue! derivs (cons new-state fderiv)))
                (make-deriv))
              (if (equal? treelist-w (car fderiv))
                  (apply append
                         (map (lambda (l)
                                (if (equal? treelist-w l)
                                    (if (null? l)
                                        (list EMP)
                                        (list (tlos->symbol l)))
                                    (list (tlos->symbol l) ARROW)))
                              (reverse fderiv)))
                  (make-deriv))))))
  (if (> (treelist-length treelist-w) 0)
      (if (cyk (chomsky g) w)
          (begin
            (enqueue! derivs (list (treelist (cfg-get-start g))))
            (make-deriv))
          (format "~s is not in L(G)." w))
      (begin
        (enqueue! derivs (list (treelist (cfg-get-start g))))
        (make-deriv))))

(define even-bs-odd-as
  (make-unchecked-cfg '(S A B C)
                      '(a b)
                      `((S ,ARROW aA) (S ,ARROW bB)
                                      (S ,ARROW a)
                                      (A ,ARROW aS)
                                      (A ,ARROW bC)
                                      (B ,ARROW aC)
                                      (B ,ARROW bS)
                                      (C ,ARROW aB)
                                      (C ,ARROW bA)
                                      (C ,ARROW b))
                      'S))