#lang racket

(require  "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         )

(define (cfg-derive-queue-and-hash g w derv-type)
  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (cond [(empty? st) #f]
          [(not (member (car st) (cfg-get-alphabet g))) (car st)]
          [else (get-first-nt (cdr st))]
          )
    )
  
  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st) (get-first-nt (reverse st)))

  ;; symbol CFG -> (Listof CFG-rule)
  ; A CFG-rule is a structure, (CFG-rule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                   (cfg-get-the-rules g)))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol)s (listof symbol)s -> (listof symbol)s
  ;; Purpose: Replaces the leftmost nonterminal with a righthand side of a rule
  (define (subst-first-nt state rght)
    (cond [(not (member (car state) (cfg-get-alphabet g)))
           (if (eq? (car rght) EMP)
               (cdr state)
               (append rght (cdr state))
               )
           ]
          [else (cons (car state) (subst-first-nt (cdr state) rght))]))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol) (listof symbol) -> (listof symbol)
  ;; Purpose: Replaces the rightmost nonterminal with a righthand side of a rule
  (define (subst-last-nt state rght) (reverse (subst-first-nt (reverse state) (reverse rght))))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (cond 
      [(not (member (car st) (cfg-get-alphabet g))) '()]
      [else (cons (car st) (get-starting-terminals (cdr st)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    (cond [(= n 0) '()]
          [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let* ((start-terms-st (get-starting-terminals st))
           (start-terms-w (if (> (length start-terms-st) (length w))
                              #f
                              (get-first-n-terms w (length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))

  (define visited-hash (make-hash))
  (define QUEUE-CAPACITY 2097152)
  (define queue (make-vector QUEUE-CAPACITY))

  (define (qempty? a-queue) (= (first a-queue) -1))
  (define (qfull? a-queue) (or (and (= (first a-queue) 0) (= (second a-queue) (sub1 QUEUE-CAPACITY)))
                               (= (first a-queue) (add1 (second a-queue)))
                               )
    )

  (define (dequeue! a-queue) (let [
                                   (result (vector-ref queue (first a-queue)))
                                   ]
                               (if (= (first a-queue) (second a-queue))
                                   (list (list -1 -1) result)
                                   (list (list (modulo (add1 (first a-queue)) QUEUE-CAPACITY) (second a-queue)) result)
                                   )
                               )
    )

  (define (qpeek a-queue) (vector-ref queue (first a-queue)))

  (define (enqueue! a-queue val) (if (qfull? a-queue)
                                     (error "uh oh")
                                     (if (= -1 (first a-queue))
                                         (begin (vector-set! queue (modulo (add1 (second a-queue)) QUEUE-CAPACITY) val)
                                                (list 0 (modulo (add1 (second a-queue)) QUEUE-CAPACITY))
                                                )
                                         (begin (vector-set! queue (modulo (add1 (second a-queue)) QUEUE-CAPACITY) val)
                                                (list (first a-queue) (modulo (add1 (second a-queue)) QUEUE-CAPACITY))
                                                )
                                         )
                                     )
    )
  
  (define input-word-length (length w))

  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv visited derivs g chomsky)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))

    (cond [(eq? derv-type 'left)
           (cond [(qempty? derivs) (format "~s is not in L(G)." w)]
                 [(begin
                    ;(displayln (first (first (qpeek derivs))))
                    (or (and chomsky
                             (> (length (first (first (qpeek derivs)))) (+ 2 input-word-length)))
                        (> (count-terminals (first (first (qpeek derivs))) (cfg-get-alphabet g)) input-word-length))
                    )
                  (make-deriv visited (first (dequeue! derivs)) g chomsky)]
                 [else 
                  (let* ((fderiv (qpeek derivs))
                         ;(test (displayln fderiv))
                         (state (first fderiv))
                         ;(test (displayln state))
                         (fnt (get-first-nt (first state))))
                    (if (false? fnt)
                        (if (equal? w (first state))
                            (append-map (lambda (l) (if (equal? w (first l))
                                                        (if (null? l)
                                                            (list EMP)
                                                            (list (list (los->symbol (first l)) (los->symbol (second l)))))
                                                        (list (list (los->symbol (first l))
                                                                    (los->symbol (second l))) ARROW)))
                                        (reverse fderiv))
                            (make-deriv visited (first (dequeue! derivs)) g chomsky))
                        (let*
                            ((rls (get-rules fnt g))
                             (rights (map cfg-rule-rhs rls))
                             (new-states (filter (lambda (st) (begin
                                                                ;(display "state: ")
                                                                ;(displayln st)
                                                                (and (not (hash-ref visited st #f))
                                                                     (check-terminals? (first state))
                                                                     )
                                                                )
                                                   ) 
                                                        (map (lambda (rght) (list (subst-first-nt (first state) rght)
                                                                                  rght)) rights)     
                                                 )
                                         )
                             )

                          
                          
                          (make-deriv (begin
                                        (map (lambda (new-st) (hash-set! visited new-st 1)) new-states)
                                        visited
                                        )
                                      (let [ (new-queue (first (dequeue! derivs)))]
                                        (foldr (lambda (val accum) (if (not (qfull? accum))
                                                                       (enqueue! accum val)
                                                                       (error "too many")
                                                                       )
                                                 )
                                               new-queue
                                               (map (lambda (st) (cons st fderiv))
                                                    new-states))
                                        )
                                      g
                                      chomsky))))]
                 )
           ]
          [(eq? derv-type 'right)
           (cond [(empty? derivs) (format "~s is not in L(G)." w)]
                 [(or (and chomsky
                           (> (length (first (first (first derivs)))) (+ 2 (length w))))
                      (> (count-terminals (first (first (first derivs))) (cfg-get-alphabet g)) (length w)))
                  (make-deriv visited (cdr derivs) g chomsky)]
                 [else 
                  (let* (
                         (fderiv (car derivs))
                         (state (car fderiv))
                         (fnt (get-last-nt (first state)))
                         )
                    (if (false? fnt)
                        (if (equal? w (first state))
                            (append-map (lambda (l) (if (equal? w (first l))
                                                        (if (null? l)
                                                            (list EMP)
                                                            (list (list (los->symbol (first l))
                                                                        (los->symbol (second l))))
                                                            )
                                                        (list (list (los->symbol (first l))
                                                                    (los->symbol (second l))) ARROW)))
                                        (reverse fderiv)
                                        )
                            (make-deriv visited (cdr derivs) g chomsky))
                        (let*
                            ((rls (get-rules fnt g))
                             (rights (map cfg-rule-rhs rls))
                             (new-states (filter (lambda (st) (and (not (hash-ref visited st #f))
                                                                   (check-terminals? (first state)))) 
                                                 (map (lambda (rght) (list (subst-last-nt (first state) rght)
                                                                           rght))
                                                      rights))))
                          (make-deriv (begin
                                        (map (lambda (new-st) (hash-set! visited new-st 1)) new-states)
                                        visited
                                        )
                                      (append (cdr derivs) 
                                              (map (lambda (st) (cons st fderiv)) 
                                                   new-states))
                                      g
                                      chomsky))))])])) 
  (if (< (length w) 2)
      (format "The word ~s is too short to test." w)
      (let* ( ;; derive using g ONLY IF derivation found with g in CNF
             #;(ng (convert-to-cnf g))
             #;(ng-derivation (make-deriv (list (list (list (cfg-get-start ng)) '() )) 
                                          (list (list (list (list (cfg-get-start ng)) '() )))
                                          ng
                                          true)))
        ;(if (string? ng-derivation)
        ;ng-derivation
        (make-deriv
         (make-hash)
         (enqueue! (list -1 -1) (list (list (list (cfg-get-start g)) '() )))
         g
         false)
        ;)
        )))