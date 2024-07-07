#lang racket
(require racket/fixnum
         racket/treelist
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         "../fsm-core/private/string.rkt"
         "circular-queue-treelist.rkt"
         )

(provide cfg-derive-rightmost)

(define (cfg-derive-rightmost g w)

  (define treelist-w (list->treelist w))
  (define (get-last-nt-helper st i)
    (if (fx= i -1)
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
    (if (fx= i -1)
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

  (define (get-starting-terminals-rightmost st)
    (define st-length (treelist-length st))
    (define (get-starting-terminals-rightmost-helper st i)
      (if (fx= i st-length)
          st
          (if (not (member (treelist-ref st (fx- (sub1 (treelist-length st)) i)) (cfg-get-alphabet g)))
              (if (fx= i 0)
                  (treelist)
                  (treelist-sublist st (fx- (treelist-length st) i) (treelist-length st))
                  )
                     
              (get-starting-terminals-rightmost-helper st (add1 i))
              )
          )
      )
    (get-starting-terminals-rightmost-helper st 0)
    )

  (define (get-last-n-terms w n)
    (if (fx= n 0)
        (treelist)
        (treelist-sublist w (fx- (treelist-length w) n) (treelist-length w)))
    )

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let* ((start-terms-st (get-starting-terminals-rightmost st))
           
           (start-terms-w (if (fx> (treelist-length start-terms-st) (treelist-length treelist-w))
                              #f
                              (get-last-n-terms treelist-w (treelist-length start-terms-st))))
           
           )
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))


  (define (treelist-filter-helper pred tl i)
    (if (fx= i -1)
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
    ;(displayln (vector*-length (queue-values derivs)))
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (treelist-length (treelist-filter (lambda (a) (member a sigma)) st)))
    (cond [(qempty? derivs) (format "~s is not in L(G)." w)]
          [(begin
             (or (and chomsky
                      (fx> (length (first (first (qpeek derivs)))) (fx+ 2 (treelist-length treelist-w))))
                 (fx> (count-terminals (first (first (qpeek derivs))) (cfg-get-alphabet g)) (treelist-length treelist-w)))
             )
           (make-deriv visited (dequeue! derivs)
                       g chomsky)]
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
                       (make-deriv visited (dequeue! derivs) g chomsky)))
                 (let*
                     ((rls (get-rules fnt g))
                      ;(test  (displayln (format "first terminals: ~s" (check-terminals? (first state)))))
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
                               (let [(new-queue (dequeue! derivs))]
                                 (foldr (lambda (val accum) (enqueue! accum val))
                                        new-queue
                                        (map (lambda (st) (cons st fderiv))
                                             new-states))
                                 )
                               g
                               chomsky))))]))
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
        (let [(deriv-queue (make-queue))]
          (make-deriv
           (make-hash)
           (enqueue! deriv-queue (list (list (treelist (cfg-get-start g)) '() )))
           g
           false)
          )
        )))