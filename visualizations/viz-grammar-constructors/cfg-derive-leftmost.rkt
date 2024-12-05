#lang racket/base
(require "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/misc.rkt"
         "circular-queue-treelist.rkt"
         racket/list)
(provide cfg-derive-leftmost)

(define (cfg-derive-leftmost g w)
  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (cond
      [(empty? st) #f]
      [(not (member (car st) (cfg-get-alphabet g))) (car st)]
      [else (get-first-nt (cdr st))]))

  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st)
    (get-first-nt (reverse st)))

  ;; symbol CFG -> (Listof CFG-rule)
  ; A CFG-rule is a structure, (CFG-rule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (define (get-rules nt g)
    (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) (cfg-get-the-rules g)))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol)s (listof symbol)s -> (listof symbol)s
  ;; Purpose: Replaces the leftmost nonterminal with a righthand side of a rule
  (define (subst-first-nt state rght)
    (cond
      [(not (member (car state) (cfg-get-alphabet g)))
       (if (eq? (car rght) EMP) (cdr state) (append rght (cdr state)))]
      [else (cons (car state) (subst-first-nt (cdr state) rght))]))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol) (listof symbol) -> (listof symbol)
  ;; Purpose: Replaces the rightmost nonterminal with a righthand side of a rule
  (define (subst-last-nt state rght)
    (reverse (subst-first-nt (reverse state) (reverse rght))))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (cond
      [(not (member (car st) (cfg-get-alphabet g))) '()]
      [else (cons (car st) (get-starting-terminals (cdr st)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    (cond
      [(= n 0) '()]
      [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let* ([start-terms-st (get-starting-terminals st)]
           [start-terms-w (if (> (length start-terms-st) (length w))
                              #f
                              (get-first-n-terms w (length start-terms-st)))])
      (cond
        [(not start-terms-w) #f]
        [else (equal? start-terms-st start-terms-w)])))

  (define input-word-length (length w))

  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv visited derivs g chomsky)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))
    (cond
      [(qempty? derivs) (format "~s is not in L(G)." w)]
      [(or (and chomsky (> (length (first (first (qpeek derivs)))) (+ 2 input-word-length)))
           (> (count-terminals (first (first (qpeek derivs))) (cfg-get-alphabet g))
              input-word-length))
       (make-deriv visited (dequeue! derivs) g chomsky)]
      [else
       (let* ([fderiv (qpeek derivs)] [state (first fderiv)] [fnt (get-first-nt (first state))])
         (if (not fnt)
             (if (equal? w (first state))
                 (append-map (lambda (l)
                               (if (equal? w (first l))
                                   (if (null? l)
                                       (list EMP)
                                       (list (list (los->symbol (first l)) (los->symbol (second l)))))
                                   (list (list (los->symbol (first l)) (los->symbol (second l)))
                                         ARROW)))
                             (reverse fderiv))
                 (make-deriv visited (dequeue! derivs) g chomsky))
             (let* ([rls (get-rules fnt g)]
                    [rights (map cfg-rule-rhs rls)]
                    [new-states
                     (filter (lambda (st)
                               (and (not (hash-ref visited st #f)) (check-terminals? (first state))))
                             (map (lambda (rght) (list (subst-first-nt (first state) rght) rght))
                                  rights))])

               (make-deriv (begin
                             (map (lambda (new-st) (hash-set! visited new-st 1)) new-states)
                             visited)
                           (let ([new-queue (dequeue! derivs)])
                             (foldr (lambda (val accum) (enqueue! accum val))
                                    new-queue
                                    (map (lambda (st) (cons st fderiv)) new-states)))
                           g
                           chomsky))))]))
  (if (< (length w) 2)
      (format "The word ~s is too short to test." w)
      (let* (;; derive using g ONLY IF derivation found with g in CNF
             #;(ng (convert-to-cnf g))
             #;(ng-derivation (make-deriv (list (list (list (cfg-get-start ng)) '()))
                                          (list (list (list (list (cfg-get-start ng)) '())))
                                          ng
                                          true)))
        ;(if (string? ng-derivation)
        ;ng-derivation
        (let ([deriv-queue (make-queue)])
          (make-deriv (make-hash)
                      (enqueue! deriv-queue (list (list (list (cfg-get-start g)) '())))
                      g
                      #f)))))