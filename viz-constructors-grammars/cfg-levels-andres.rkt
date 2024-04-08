#lang racket

(require "../fsm-gviz/private/lib.rkt"
         rackunit
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/interface.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         "viz.rkt"
         )

(define even-bs-odd-as (make-cfg '(S A B C)
                                 '(a b)
                                 `((S ,ARROW aA)
                                   (S ,ARROW bB)
                                   (S ,ARROW a)
                                   (A ,ARROW aS)
                                   (A ,ARROW bC)
                                   (B ,ARROW aC)
                                   (B ,ARROW bS)
                                   (C ,ARROW aB)
                                   (C ,ARROW bA)
                                   (C ,ARROW b))
                                 'S))

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

;; yield is a strucutre that has
;; pr - processed part of the word
;; nt - nonterminal
;; up - unprocessed part of the word
;; derv-type - Symbol that is either 'level-left or 'level-right
(struct yield (pr nt up derv-type) #:transparent)

;; yield -> (listof symbol)
;; Purpose: Returns the current state held within a yield
(define (get-current-state a-yield)
  (if (eq? 'level-left (yield-derv-type a-yield))
      (append (yield-pr a-yield) (yield-up a-yield))
      (append (yield-up a-yield) (yield-pr a-yield))
      )                             
  )

(define (convert-yield-deriv-to-word-deriv input-word yield-deriv)
  (append-map (lambda (l) (let [
                                (current-word (first l))
                                (current-rule (second l))
                                ]
                            (if (equal? input-word current-word)
                                (if (null? l)
                                    (list EMP)
                                    (list (list (los->symbol current-word) (los->symbol current-rule))))
                                (list (list (los->symbol current-word)
                                            (los->symbol current-rule)) ARROW))
                            )
                )
              (remove-duplicates (map (lambda (x) (list (get-current-state (first x)) (second x))) (reverse yield-deriv)))
              )
  )

;; cfg word (U 'level-left 'level-right) -> yield-derivation-with-rules
;; Purpose: Computes a derivation using a breadth first search style of traversal
;; and returns the rules alongside each step of the derivation
(define (cfg-derive-levels g w derv-type)

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

  ;; symbol CFG -> (Listof CFG-rule)
  ; A CFG-rule is a structure, (CFG-rule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                   (cfg-get-the-rules g)))

  ; ASSUMPTION: yield has at least one NT in unprocessed field
  ;; (listof symbol) (listof symbol) (listof symbol) -> yield
  ;; Purpose: Replaces the leftmost nonterminal within the unprocessed field of a yield
  ;; with a righthand side of a rule
  (define (subst-first-nt new-up new-p rght)
    (if (not (member (first new-up) (cfg-get-alphabet g)))
        (if (eq? (first rght) EMP)
            (yield new-p '() (rest new-up) 'level-left)
            (yield (append (if (list? new-p)
                               new-p
                               (list new-p)
                               )
                           rght) '() (rest new-up) 'level-left)
            )
        (subst-first-nt (rest new-up) (append (if (list? new-p)
                                                  new-p
                                                  (list new-p)
                                                  )
                                              (if (list? (first new-up))
                                                  (first new-up)
                                                  (list (first new-up))
                                                  )
                                              ) rght)
        )
    )

  ; ASSUMPTION: yield has at least one NT in unprocessed field
  ;; (listof symbol) (listof symbol) (listof symbol) -> yield
  ;; Purpose: Replaces the rightmost nonterminal within the unprocessed field of a yield
  ;; with a righthand side of a rule
  (define (subst-last-nt new-up new-p rght)
    (define (subst-last-nt-helper new-up new-p rght)
      (if (not (member (first new-up) (cfg-get-alphabet g)))
          (if (eq? (first rght) EMP)
              (yield (reverse (if (list? new-p)
                                  new-p
                                  (list new-p)
                                  )
                              )
                     '()
                     (reverse (rest new-up))
                     'level-right)
              (yield (reverse (append (if (list? new-p)
                                          new-p
                                          (list new-p)
                                          ) rght)) '() (reverse (rest new-up)) 'level-right)
              )
          (subst-last-nt-helper (rest new-up) (append (if (list? new-p)
                                                          new-p
                                                          (list new-p)
                                                          )
                                                      (if (list? (first new-up))
                                                          (first new-up)
                                                          (list (first new-up))
                                                          )
                                                      ) rght)
          )
      )
    (subst-last-nt-helper (reverse new-up)
                          (reverse new-p)
                          (reverse rght)
                          )
    )

  ;; (listof symbol) -> boolean
  ;; Purpose: Checks to see if there are any nonterminals within the given state
  (define (any-nt? state) (ormap (lambda (x) (not (member x (cfg-get-alphabet g)))) state))

  (define (make-deriv visited derivs g chomsky)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))
    
    (cond [(empty? derivs) (format "~s is not in L(G)." w)]
          [(or (and chomsky
                    (> (length (get-current-state (first (first (first derivs))))) (+ 2 (length w))))
               (> (count-terminals (get-current-state (first (first (first derivs)))) (cfg-get-alphabet g)) (length w)))
           (make-deriv visited (rest derivs) g chomsky)]
          [else
           (let* [
                  (current-deriv (first derivs))
                  (current-yield-and-rule (first current-deriv))
                  (current-yield (first current-yield-and-rule))
                  (state (get-current-state current-yield))
                  (current-nt (if (eq? derv-type 'level-left)
                                  (get-first-nt (yield-up current-yield))
                                  (get-last-nt (yield-up current-yield))
                                  )
                              )
                  ]
             (if (false? current-nt)
                 (if (equal? w state)
                     (reverse current-deriv)
                     (if (any-nt? state)
                         (make-deriv visited (append (rest derivs)
                                                     (list (cons (list (yield '() '() state (yield-derv-type current-yield))
                                                                       (second current-yield-and-rule)
                                                                       )
                                                                 current-deriv)
                                                           )
                                                     )
                                     g
                                     chomsky)
                         (make-deriv visited (rest derivs) g chomsky)
                         )
                     )
                 (let* [
                        (rls (get-rules current-nt g))
                        (rights (map cfg-rule-rhs rls))
                        (new-yields (filter (lambda (st) (and (not (member st visited))
                                                              (check-terminals? state)))
                                            (map (lambda (rght) (list (if (eq? derv-type 'level-left)
                                                                          (subst-first-nt (yield-up current-yield) (yield-pr current-yield) rght)
                                                                          (subst-last-nt (yield-up current-yield) (yield-pr current-yield) rght)
                                                                          )
                                                                      rght)) rights)))
                        ]
                   (make-deriv (append new-yields visited)
                               (append (rest derivs) 
                                       (map (lambda (yd) (cons yd current-deriv)) 
                                            new-yields))
                               g
                               chomsky)
                   )
                 )
             )
           ]
          )
    )
  (if (< (length w) 2)
      (format "The word ~s is too short to test." w)
      (let* ( ;; derive using g ONLY IF derivation found with g in CNF
             (ng (convert-to-cnf g))
             (ng-derivation (make-deriv (list (list (yield '() '() (list (cfg-get-start ng)) derv-type) '() )) 
                                        (list (list (list (yield '() '() (list (cfg-get-start ng)) derv-type) '() )))
                                        ng
                                        true)))
        (if (string? ng-derivation)
            ng-derivation
            (make-deriv (list (list (yield '() '() (list (cfg-get-start g)) derv-type) '() ))
                        (list (list (list (yield '() '() (list (cfg-get-start g)) derv-type) '() )))
                        g
                        false))))
  )

(define (create-rules-levels-leftmost yield-deriv) 

  (cfg-derive-levels numb>numa '(a b b a b b) 'level-left)