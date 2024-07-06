#lang racket
(require racket/unsafe/ops
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         "../fsm-core/private/string.rkt"
         racket/treelist
         "circular-queue-safe.rkt"
         )

#|
(define test (make-queue 5))
(enqueue! test 'a)
test
(dequeue! test)
|#

(define (cfg-derive-unsafe g w derv-type)
  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (cond [(empty? st) #f]
          [(not (member (car st) (cfg-get-alphabet g))) (car st)]
          [else (get-first-nt (cdr st))]))
  
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
               (append rght (cdr state)))]
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

  ;(define visited-hash (make-hash))
  (define QUEUE-CAPACITY 2097152)
  (define input-word-length (length w))
  
  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv visited derivs g chomsky)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))

    (cond [(eq? derv-type 'left)
           (cond [(qempty? derivs) (format "~s is not in L(G)." w)]
                 [(or (and chomsky
                           (> (length (first (first (qpeek derivs)))) (+ 2 input-word-length)))
                      (> (count-terminals (first (first (qpeek derivs))) (cfg-get-alphabet g)) input-word-length))
                  (make-deriv visited (begin (dequeue! derivs)
                                             derivs) g chomsky)]
                 [else 
                  (let* ((fderiv (qpeek derivs))
                         (state (first fderiv))
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
                            (make-deriv visited (begin (dequeue! derivs)
                                                       derivs) g chomsky))
                        (let*
                            ((rls (get-rules fnt g))
                             (rights (map cfg-rule-rhs rls))
                             (new-states (filter (lambda (st) (and (not (hash-ref visited st #f))
                                                                     (check-terminals? (first state))
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
                                      (let [ (new-queue (begin (dequeue! derivs)
                                                               derivs))]
                                        (foldr (lambda (val accum) (enqueue! accum val))
                                               new-queue
                                               (map (lambda (st) (cons st fderiv))
                                                    new-states))
                                        )
                                      g
                                      chomsky))))]
                 )
           ]
          [(eq? derv-type 'right)
           (cond [(qempty? derivs) (format "~s is not in L(G)." w)]
                 [(or (and chomsky
                           (> (length (first (first (qpeek derivs)))) (+ 2 (length w))))
                      (> (count-terminals (first (first (qpeek derivs))) (cfg-get-alphabet g)) (length w)))
                  (make-deriv visited (begin (dequeue! derivs)
                                             derivs) g chomsky)]
                 [else 
                  (let* (
                         (fderiv (qpeek derivs))
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
                            (make-deriv visited (begin (dequeue! derivs)
                                                       derivs) g chomsky))
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
                                      (let [ (new-queue (begin (dequeue! derivs)
                                                               derivs))]
                                        (foldr (lambda (val accum) (enqueue! accum val))
                                               new-queue
                                               (map (lambda (st) (cons st fderiv))
                                                    new-states))
                                        )
                                      g
                                      chomsky))))])])) 
  (if (< (length w) 2)
      (format "The word ~s is too short to test." w)
      (let* ( ;; derive using g ONLY IF derivation found with g in CNF
             #;(ng (convert-to-cnf g))
             #;(ng-derivation (make-deriv (list (list (list (cfg-get-start ng)) '() )) 
                                          (list (list (list (list (cfg-get-start ng)) '() )))
                                          ng
                                          true))
             )
        ;(if (string? ng-derivation)
        ;ng-derivation
        (let [(deriv-queue (make-queue 2097152))]
          (make-deriv
           (make-hash)
           (begin
             (enqueue! deriv-queue (list (list (list (cfg-get-start g)) '() )))
             deriv-queue
             )
           #;(enqueue! (list -1 -1) (list (list (list (cfg-get-start g)) '() )))
           g
           false)
          )
        ;)
        )))















(define (cfg-derive-unsafe-rightmost g w derv-type)

  (define treelist-w (list->treelist w))
  (define (get-last-nt-helper st i)
      (if (= i -1)
          #f
          (if (not (member (treelist-ref st i) (cfg-get-alphabet g)))
              (treelist-ref st i)
              (get-last-nt-helper st (sub1 i))
              )
          )
      )
  
  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st)
    (get-last-nt-helper st (sub1 (treelist-length st)))
    )
  #;(define (get-last-nt st) (get-first-nt (reverse st)))

  ;; symbol CFG -> (Listof CFG-rule)
  ; A CFG-rule is a structure, (CFG-rule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                   (cfg-get-the-rules g)))

  (define (treelist-insert-list tl i lst)
    (if (empty? lst)
        tl
        (treelist-insert-list (treelist-insert tl i (first lst)) (add1 i) (rest lst))
        )
    )
  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol) (listof symbol) -> (listof symbol)
  ;; Purpose: Replaces the rightmost nonterminal with a righthand side of a rule
  (define (subst-last-nt-helper st rght i)
    (if (= i -1)
        (treelist-insert-list (treelist-delete st 0) 0 rght)
        (if (not (member (treelist-ref st i) (cfg-get-alphabet g)))
            (if (eq? (first rght) EMP)
                (treelist-delete st i)
                ;(subst-last-nt-helper st rght (sub1 i))
                (treelist-insert-list (treelist-delete st i) i rght)
                ) 
            ;(treelist-append (treelist-sublist 
            (subst-last-nt-helper st rght (sub1 i))
            )
        )
    )
  
  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
 (define (subst-last-nt st rght)
    (subst-last-nt-helper st rght (sub1 (treelist-length st)))
    )
  #;(define (subst-last-nt state rght) (reverse (subst-first-nt (reverse state) (reverse rght))))

#;(define (get-starting-terminals st)
    (cond 
      [(not (member (car st) (cfg-get-alphabet g))) '()]
      [else (cons (car st) (get-starting-terminals (cdr st)))]))
  
  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (define st-length (treelist-length st))
    (define (get-starting-terminals-helper st tl i)
      (if (= i st-length)
          tl
          (if (not (member (treelist-ref st i) (cfg-get-alphabet g)))
              (begin
                tl)
              (get-starting-terminals-helper st (treelist-insert tl (treelist-length tl) (treelist-ref st i)) (add1 i))
              )
          )
      )
    (get-starting-terminals-helper st (treelist) 0)
    )
  #;(define (get-starting-terminals st)
    (cond 
      [(not (member (treelist-first st) (cfg-get-alphabet g))) '()]
      [else (cons (treelist-first st) (get-starting-terminals (cdr st)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  #;(define (get-first-n-terms w n)
    (cond [(= n 0) '()]
          [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    (treelist-sublist w 0 n)
    #;(cond [(= n 0) '()]
          [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))

  #;(define (check-terminals? st)
    (let* ((start-terms-st (get-starting-terminals st))
           (start-terms-w (if (> (length start-terms-st) (length w))
                              #f
                              (get-first-n-terms w (length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let* ((start-terms-st (get-starting-terminals st))
           (start-terms-w (if (> (treelist-length start-terms-st) (treelist-length treelist-w))
                              #f
                              (get-first-n-terms treelist-w (treelist-length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))

  ;(define visited-hash (make-hash))
  (define QUEUE-CAPACITY 2097152)
  
  (define input-word-length (length w))


  (define (treelist-filter-helper pred tl i)
      (if (= i -1)
          tl
          (if (pred (treelist-ref tl i))
              (treelist-filter-helper pred tl (sub1 i))
              (treelist-filter-helper pred (treelist-delete tl i) (sub1 i))
              )
          )
      )
  (define (treelist-filter pred tl)
    (treelist-filter-helper pred tl (sub1 (treelist-length tl)))
    )

  (define (tlos->symbol l)
    (define (tlostr->string l)
      (cond [(treelist-empty? l) ""]
            [else (string-append (treelist-first l) (tlostr->string (treelist-rest l)))]))
    (define (tlos->tlostr ss) (treelist-map ss symbol->string ))
    (string->symbol (tlostr->string (tlos->tlostr l))))
  
  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG Boolean ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv visited derivs g chomsky)
    (displayln derivs)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (treelist-length (treelist-filter (lambda (a) (member a sigma)) st)))

    (cond [(eq? derv-type 'right)
           (cond [(qempty? derivs) (format "~s is not in L(G)." w)]
                 [(begin
                    (or (and chomsky
                           (> (length (first (first (qpeek derivs)))) (+ 2 (treelist-length treelist-w))))
                      (> (count-terminals (first (first (qpeek derivs))) (cfg-get-alphabet g)) (treelist-length treelist-w)))
                    )
                  (make-deriv visited (begin (dequeue! derivs)
                                             derivs) g chomsky)]
                 [else 
                  (let* (
                         (fderiv (qpeek derivs))
                         (state (car fderiv))
                         (fnt (get-last-nt (first state)))
                         )
                    (if (false? fnt)
                        (begin 
                        (if (equal? treelist-w (first state))
                            (append* (map (lambda (l)
                                          (if (equal? treelist-w (first l))
                                                        (if (null? l)
                                                            (list EMP)
                                                            (list (list (tlos->symbol (first l))
                                                                        (los->symbol (second l))))
                                                            )
                                                        (list (list (tlos->symbol (first l))
                                                                    (los->symbol (second l))) ARROW)))
                                        (reverse fderiv)
                                        ))
                            (make-deriv visited (begin (dequeue! derivs)
                                             derivs) g chomsky)))
                        (let*
                            ((rls (get-rules fnt g))
                             (rights (map cfg-rule-rhs rls))
                             (new-states (filter (lambda (st) (and (not (hash-ref visited st #f))
                                                                   (check-terminals? (first state))))
                                                 (map (lambda (rght) (list (subst-last-nt (first state) rght)
                                                                           rght))
                                                      rights))
                                         )
                             )
                          (make-deriv (begin
                                        (map (lambda (new-st) (hash-set! visited new-st 1)) new-states)
                                        visited
                                        )
                                      (let [ (new-queue (begin (dequeue! derivs)
                                                               derivs))]
                                        (foldr (lambda (val accum) (enqueue! accum val))
                                               new-queue
                                               (map (lambda (st) (cons st fderiv))
                                                    new-states))
                                        )
                                      g
                                      chomsky))))])]))
  (if (< (length w) 2)
      (format "The word ~s is too short to test." w)
      (let* ( ;; derive using g ONLY IF derivation found with g in CNF
             #;(ng (convert-to-cnf g))
             #;(ng-derivation (make-deriv (list (list (list (cfg-get-start ng)) '() )) 
                                          (list (list (list (list (cfg-get-start ng)) '() )))
                                          ng
                                          true))
             )
        ;(if (string? ng-derivation)
        ;ng-derivation
        (let [(deriv-queue (make-queue 50))]
          (make-deriv
           (make-hash)
           (begin
             (enqueue! deriv-queue (list (list (treelist (cfg-get-start g)) '() )))
             deriv-queue
             )
           #;(enqueue! (list -1 -1) (list (list (list (cfg-get-start g)) '() )))
           g
           false)
          )
        ;)
        )))

(define testcfg (make-unchecked-cfg '(S A B)
                                    '(a b c d)
                                    `(
                                      (S ,ARROW ,EMP)
                                      (S ,ARROW AB)
                                      (A ,ARROW aSb)
                                      (B ,ARROW cBd)
                                      (A ,ARROW ,EMP)
                                      (B ,ARROW ,EMP))
                                    'S
                                    )
  )

;(cfg-derive testcfg '(a a b b c c c d d d))
;(time (cfg-derive-unsafe testcfg '(a a a a a a a a a a a a a a a a b b b b b b b b b b b b b b b b c c c c c c c c c c c c c c c c d d d d d d d d d d d d d d d d) 'right))
(time (cfg-derive-unsafe-rightmost testcfg '(a a a b b b c c c d d d) 'right))
;(time (cfg-derive-unsafe-rightmost testcfg '(a a a a a a a a a a a a b b b b b b b b b b b b c c c c c c c c c c c c d d d d d d d d d d d d) 'right))
;(time (cfg-derive-unsafe testcfg '(a a a a a a a a a a a a a a a a b b b b b b b b b b b b b b b b c c c c c c c c c c c c c c c c d d d d d d d d d d d d d d d d) 'right))
;(unsafe-fx= 1 2 1)
;(fx= 1 1 1)