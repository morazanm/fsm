#lang racket/base

(require "../../fsm-core/private/cfg-struct.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/misc.rkt"
         "../../fsm-core/private/cyk.rkt"
         "../../fsm-core/private/chomsky.rkt"
         "circular-queue-treelist.rkt"
         racket/list
         racket/set)

(provide cfg-derive-leftmost)

(define (cfg-derive-leftmost g w)
  (define alphabet-set (list->set (cfg-get-alphabet g)))
  (define nt-set (list->set (cfg-get-v g)))
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
    (if (> start-terms-st-len input-word-length)
        #f
        (equal? start-terms-st (take w start-terms-st-len))))

  (define visited (mutable-set))
  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv derivs)
    (if (qempty? derivs)
        (format "~s is not in L(G)." w)
        (let* ([fderiv (qpeek derivs)]
               [state (car fderiv)]
               [fnt (get-first-nt state)])
          (if fnt
              (make-deriv (foldr (lambda (val accum) (enqueue! accum val))
                                 (dequeue! derivs)
                                 (for/list ([rhs (in-list (hash-ref nt-to-rhss-ht fnt))]
                                            #:do [(define new-state (subst-first-nt state rhs))]
                                            #:when (and (not (set-member? visited new-state))
                                                        (check-terminals? new-state)))
                                   (set-add! visited new-state)
                                   (cons new-state fderiv))))
              (if (equal? w state)
                  (append-map (lambda (l)
                                (if (equal? w l)
                                    (if (null? l)
                                        (list EMP)
                                        (list (los->symbol l)))
                                    (list (los->symbol l)
                                          ARROW)))
                              (reverse fderiv))
                  (make-deriv (dequeue! derivs)))))))
  (if (> input-word-length 0)
      (if (cyk (chomsky g) w)
          (make-deriv (enqueue! (make-queue) (list (list (cfg-get-start g)))))
          (format "~s is not in L(G)." w))
      (make-deriv (enqueue! (make-queue) (list (list (cfg-get-start g)))))))

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

(define cfg-moreAs-than-Bs (make-cfg '(S T)
                                       '(a b)
                                       `((S -> TaT)
                                         (T -> aTb)
                                         (T -> bTa)
                                         (T -> TT)
                                         (T -> a)
                                         (T -> ,EMP))
                                       'S))
;(last (cfg-derive-leftmost cfg-moreAs-than-Bs '(a b b a a)))
;(cfg-derive-leftmost even-bs-odd-as '(b b b a b))