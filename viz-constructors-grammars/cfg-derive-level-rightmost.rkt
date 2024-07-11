#lang racket

(require "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/constants.rkt"
         "circular-queue-treelist.rkt"
         racket/treelist
         "yield-struct.rkt"
         )

(provide cfg-derive-level-rightmost)

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

(define (treelist-insert-list tl i lst)
  (if (empty? lst)
      tl
      (treelist-insert-list (treelist-insert tl i (first lst)) (add1 i) (rest lst))
      )
  )

;; yield is a structure that has
;; pr - processed part of the word
;; nt - nonterminal
;; up - unprocessed part of the word
#;(struct yield (state up) #:transparent)

(define (cfg-derive-level-rightmost g w)
  (define alphabet-ht
    (foldr (lambda (val accum)
                               (begin (hash-set! accum val 1)
                                                        accum))
                             (make-hash)
                             (cfg-get-alphabet g)))

  (define rules-ht
    (foldr (lambda (val accum)
                            (begin
                              (hash-set! accum val (filter (lambda (r) (eq? val (cfg-rule-lhs r))) 
                                                           (cfg-get-the-rules g)))
                              accum))
                          (make-hash)
                          (cfg-get-v g)))

  (define treelist-w (list->treelist w))


  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt-helper st i)
    (if (= i -1)
        #f
        (if (not (hash-ref alphabet-ht (treelist-ref (yield-state st) i) #f))
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
    (if (= n 0)
        (treelist)
        (treelist-sublist w (- (treelist-length w) n) (treelist-length w))))
  
  ; (list (listof symbol)) --> boolean
  (define (check-terminals-right? st)
    (let* ((start-terms-st (get-starting-terminals-rightmost st))
           (start-terms-w (if (> (treelist-length start-terms-st) (treelist-length treelist-w))
                              #f
                              (get-last-n-terms treelist-w (treelist-length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))


  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (define (get-first-nt-helper i)
      (cond [(= i (treelist-length (yield-state st))) #f]
            [(not (hash-ref alphabet-ht (treelist-ref (yield-state st) i) #f)) (treelist-ref (yield-state st) i)]
            [else (get-first-nt-helper (add1 i))]))
    (get-first-nt-helper (yield-up st)))
  
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
    (let* ((start-terms-st (get-starting-terminals-leftmost st))
           (start-terms-w (if (> (treelist-length start-terms-st) (treelist-length treelist-w))
                              #f
                              (get-first-n-terms treelist-w (treelist-length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))

  ; ASSUMPTION: yield has at least one NT in unprocessed field
  ;; (listof symbol) (listof symbol) (listof symbol) -> yield
  ;; Purpose: Replaces the leftmost nonterminal within the unprocessed field of a yield
  (define (subst-last-nt yd rght)
    (define (subst-last-nt-helper i)
      (if (= i -1)
            (subst-last-nt (yield (yield-state yd) (sub1 (treelist-length (yield-state yd)))) rght)
            (if (not (hash-ref alphabet-ht (treelist-ref (yield-state yd) i) #f))
                (if (eq? (first rght) EMP)
                      (struct-copy yield yd
                                   [state (treelist-delete (yield-state yd) i)]
                                   [up (sub1 i)])
                      (struct-copy yield yd
                                   [state (treelist-insert-list (treelist-delete (yield-state yd) i) i rght)]
                                   [up (sub1 i)]))
                ;(treelist-append (treelist-sublist 
                (subst-last-nt-helper (sub1 i)))))
    (subst-last-nt-helper (yield-up yd)))
             

  

  ;; (listof symbol) -> boolean
  ;; Purpose: Checks to see if there are any nonterminals within the given state
  (define (any-nt? state)
    (define (any-nt?-helper i)
      (if (= i (treelist-length state))
          #f
          (if (hash-ref alphabet-ht (treelist-ref state i) #f)
              (any-nt?-helper (add1 i))
              #t)))
    (any-nt?-helper 0))  

  (define (make-deriv visited derivs g)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st)
      (treelist-length (treelist-filter (lambda (a) (hash-ref alphabet-ht a #f)) st)))
    
    (cond [(qempty? derivs) (format "~s is not in L(G)." w)]
          [(> (count-terminals (yield-state (first (first (qpeek derivs)))))
              (treelist-length treelist-w))
           (make-deriv visited (dequeue! derivs) g)]
          [else
           (let* [(current-deriv (qpeek derivs))
                  (current-yield-and-rule (first current-deriv))
                  (current-yield (first current-yield-and-rule))
                  (state (yield-state current-yield))
                  (current-nt (get-last-nt current-yield))]
             (if (false? current-nt)
                 (if (equal? treelist-w state)
                     (reverse (cons (list current-yield (second current-yield-and-rule)) (rest current-deriv)))
                     (if (any-nt? state)
                         (make-deriv visited (enqueue! (dequeue! derivs)
                                                       (cons (list (yield state (sub1 (treelist-length state)))
                                                                   (second current-yield-and-rule))
                                                             current-deriv))
                                     g)
                         (make-deriv visited (dequeue! derivs) g)))
                 (let* [(rls (hash-ref rules-ht current-nt))
                        (rights (map cfg-rule-rhs rls))
                        (new-yields (filter (lambda (st) (and (not (hash-ref visited st #f))
                                                              (check-terminals-right? state)
                                                              (check-terminals-left? state)))
                                            (map (lambda (rght) (list
                                                                 (subst-last-nt current-yield rght)   
                                                                 (list current-nt '-> rght)))
                                                 rights)))]
                   (make-deriv (begin
                                 (map (lambda (new-yd) (hash-set! visited new-yd 1)) new-yields)
                                 visited)
                               (let [(new-derivs (dequeue! derivs))]
                                 (foldr (lambda (val accum) (enqueue! accum val))
                                        new-derivs
                                        (map (lambda (yd) (cons yd current-deriv)) 
                                             new-yields)))
                               g))))]))
  (make-deriv (make-hash) 
              (enqueue! (make-queue) (list (list (yield (treelist (cfg-get-start g)) 0) '() )))
              g))

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

;; (listof symbol) (listof yield) -> w-der
;; Purpose: Converts a yield derivation into the form of a word derivation that the viz expects
(define (convert-yield-deriv-to-word-deriv yield-deriv)
  (remove-duplicates (map (lambda (x) (yield-state (first x))) yield-deriv)))

;(time (cfg-derive-level-rightmost testcfg '(a a b b c c c d d d)))
;(time (cfg-derive-level-rightmost testcfg '(a a a a a a a a a a a a a a a a b b b b b b b b b b b b b b b b c c c c c c c c c c c c c c c c d d d d d d d d d d d d d d d d)))
;(convert-yield-deriv-to-word-deriv (cfg-derive-level-rightmost testcfg '(a a b b c c c d d d)))