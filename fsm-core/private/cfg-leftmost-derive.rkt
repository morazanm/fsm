#lang racket/base

(require "cfg-struct.rkt"
         "constants.rkt"
         "misc.rkt"
         "cyk.rkt"
         "chomsky.rkt"
         "queue.rkt"
         racket/list
         racket/set)

(provide cfg-derive-leftmost)

(define (cfg-derive-leftmost g w)
  (define nt-set (list->seteq (cfg-get-v g)))
  (define nt-to-rhss-ht
    (for/hasheq ([nt (in-list (cfg-get-v g))])
      (values nt (map cfg-rule-rhs (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) (cfg-get-the-rules g))))))
  (define input-word-length (length w))
  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (cond
      [(null? st) #f]
      [(set-member? nt-set (car st)) (car st)]
      [else (get-first-nt (cdr st))]))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol)s (listof symbol)s -> (listof symbol)s
  ;; Purpose: Replaces the leftmost nonterminal with a righthand side of a rule
  (define (subst-first-nt state rght)
    (if (set-member? nt-set (car state))
        (if (eq? (car rght) EMP)
            (cdr state)
            (append rght (cdr state)))
        (cons (car state) (subst-first-nt (cdr state) rght))))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (if (empty? st)
        '()
        (if (not (set-member? nt-set (car st)))
            (cons (car st) (get-starting-terminals (cdr st)))
            '())))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (define start-terms-st (get-starting-terminals st))
    (define start-terms-st-len (length start-terms-st))
    (and (not (> (count (lambda (x) (not (set-member? nt-set x))) st) #;start-terms-st-len input-word-length))
         (equal? start-terms-st (take w (length start-terms-st)))))

  (define visited (mutable-set))
  (define derivs (make-queue))
  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv)
    (if (qempty? derivs)
        (format "~s is not in L(G)." w)
        (let* ([fderiv (dequeue! derivs)]
               [fnt (get-first-nt (car fderiv))])
          (format "~s is not in L(G)." w)
          (if fnt
              (begin
                (for ([rhs (in-list (hash-ref nt-to-rhss-ht fnt))]
                           #:do [(define new-state (subst-first-nt (car fderiv) rhs))]
                           #:when (and (not (set-member? visited new-state))
                                       (check-terminals? new-state)))
                  (set-add! visited new-state)
                  (enqueue! derivs (cons new-state fderiv)))
                (make-deriv))
              (if (equal? w (car fderiv))
                  (append-map (lambda (l)
                                (if (equal? w l)
                                    (if (null? l)
                                        (list EMP)
                                        (list (los->symbol l)))
                                    (list (los->symbol l)
                                          ARROW)))
                              (reverse fderiv))
                  (make-deriv))))))
  (if (> input-word-length 0)
      (if (cyk (chomsky g) w)
          (begin
            (enqueue! derivs (list (list (cfg-get-start g))))
            (make-deriv))
          (format "~s is not in L(G)." w))
      (begin
        (enqueue! derivs (list (list (cfg-get-start g))))
        (make-deriv))))

(define testcfg
  (make-unchecked-cfg
   '(S A B)
   '(a b c d)
   `((S ,ARROW ,EMP) (S ,ARROW AB) (A ,ARROW aSb) (B ,ARROW cBd) (A ,ARROW ,EMP) (B ,ARROW ,EMP))
   'S))