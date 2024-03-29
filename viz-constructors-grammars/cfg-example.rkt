#lang racket
(require "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         
         )


(define (cfg-derive1 g w)
  (define (get-first-nt st)
    (cond [(empty? st) #f]
          [(not (member (car st) (cfg-get-alphabet g))) (car st)]
          [else (get-first-nt (cdr st))])
    )

  (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                   (cfg-get-the-rules g)))

  ; ASSUMPTION: state has at least one NT
  (define (subst-first-nt state rght)
    (cond [(not (member (car state) (cfg-get-alphabet g)))
           (if (eq? (car rght) EMP) (cdr state) (append rght (cdr state)))]
          [else (cons (car state) (subst-first-nt (cdr state) rght))]))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (cond 
      [(not (member (car st) (cfg-get-alphabet g))) '()]
      [else (cons (car st) (get-starting-terminals (cdr st)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    ;(println w)
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


  (define (make-deriv visited derivs g chomsky)
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))

    (cond [(empty? derivs) (format "~s is not in L(G)." w)]
          [(or (and chomsky
                    (> (length (first (first (first derivs)))) (+ 2 (length w)))
                    )
               (> (count-terminals (first (first (first derivs))) (cfg-get-alphabet g)) (length w))
               )
           (make-deriv visited (cdr derivs) g chomsky)]
          [else 
           (let* ((fderiv (car derivs))
                  (state (car fderiv))
                  (fnt (get-first-nt (first state)))
                  )
             (if (false? fnt)
                 (if (equal? w (first state))
                     (append-map (lambda (l) (if (equal? w (first l)) 
                                                 (if (null? l)
                                                     (list EMP)
                                                     (list (list (los->symbol (first l)) (los->symbol (second l))))
                                                     )
                                                 (list (list (los->symbol (first l)) (los->symbol (second l))) ARROW)
                                                 )
                                   ) 
                                 (reverse fderiv))
                     (make-deriv visited (cdr derivs) g chomsky))
                 (let*
                     ((rls (get-rules fnt g))
                      (rights (map cfg-rule-rhs rls))
                      (new-states (filter (lambda (st) (and (not (member st visited))
                                                            (check-terminals? (first state)))) 
                                          (map (lambda (rght) (list (subst-first-nt (first state) rght) rght)) rights)
                                          )
                                  )
                      )
                   (make-deriv (append new-states visited)
                               (append (cdr derivs) 
                                       (map (lambda (st) (cons st fderiv)) 
                                            new-states))
                               g
                               chomsky))))]))   
  (if (< (length w) 2)
      (format "The word ~s is too short to test." w)
      (let* ( ;; derive using g ONLY IF derivation found with g in CNF
             (ng (convert-to-cnf g))
             (ng-derivation (make-deriv (list (list (list (cfg-get-start ng)) '() )) 
                                        (list (list (list (list (cfg-get-start ng)) '() )))
                                        ng
                                        true)
                            )
             )
        (if (string? ng-derivation)
            ng-derivation
            (make-deriv (list (list (list (cfg-get-start g)) '() )) 
                        (list (list (list (list (cfg-get-start g)) '() )))
                        g
                        false)
            )
        )
      )
  )






;; Syntactic Categories
;;  S = words such that number of b > number of a
;;  A = words such that number of b >= number of a


;; L =w | win(ab)*AND w has more b than a

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

;; levels is a structure that has
;; list of levels
;; accumulator for all nodes that appear in levels
;; leftmost accumulator (basically just the root node we need to remember)
(struct levels (lol acc lma))

;; lower-first?
;; edge -> Boolean
;; Purpose: To check if the second element is lowercase
(define (lower-first? edge)
  (lower? (second edge)))

;; rename-nodes
;; levels -> levels
;; Purpose: To rename the terminals that reoccur in extracted edges
        
  
                         
  
;; w-der
;; derivation -> derivation-list
;; Purpose: To turn the derivation into a list
(define (w-der rg word)
  (map symbol->fsmlos (filter (λ (x) (not (equal? x '->)))
                              (cfg-derive rg word))))

;; w-der
;; derivation -> derivation-list
;; Purpose: To turn the derivation into a list
(define (w-der1 rg word)
  (map (lambda (state) (list (symbol->fsmlos (first state)) (symbol->fsmlos (second state))))
       (filter (λ (x) (not (equal? x '->)))
               (cfg-derive1 rg word))
       )
  )

;; lower?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is down case
(define (lower? symbol)
  (not (char-upper-case? (string-ref (symbol->string symbol) 0))))


;; list-intersect?
;; (listof symbol) (listof symbol) -> Boolean
;; Purpose: To check if two lists have the same element in them
(define (list-intersect? los1 los2)
  (ormap (λ (symbol) (member symbol los2)) los1))


;; generate-level
;; (listof symbol) (listof symbol) -> level
;; Purpose: To generate levels for intersect lists
(define (generate-level los1 los2)
  (let* [(leftmost (takef los2 lower?))
         (rightmost (take-right los2 (- (length los1) 1)))
         (nonterminal (first (dropf los1 lower?)))
         (new (if (empty? (drop-right los2 (length rightmost)))
                  (list 'ε)
                  (drop (drop-right los2 (length rightmost)) (length leftmost))))]
    (levels (for*/list ([i (list nonterminal)]
                        [j new])
              (list i j)) (list nonterminal) nonterminal)))


(define (rename-nt hashtable nt) (let [
                                       (result (hash-ref hashtable nt #f))
                                       ] 
                                   (if result
                                       (begin (hash-set! hashtable nt (add1 result))
                                              (string->symbol (format "~s~s" nt (add1 result)))
                                              )
                                       (begin (hash-set! hashtable nt 0)
                                              (string->symbol (format "~s0" nt))
                                              )
                                       )
                                   )
  )

(define (nonterminal? symb) (let [
                                  (ascii-val (char->integer (first (string->list (symbol->string symb)))))
                                  ]
                              (and (<= 65 ascii-val)
                                   (>= 90 ascii-val)
                                   )
                              )
  )
                                


(define (find-leftmost-nt-helper a-lst) (if (empty? a-lst)
                                            #f
                                            (if (nonterminal? (first a-lst))
                                                (first a-lst)
                                                (find-leftmost-nt-helper (rest a-lst))
                                                )
                                            )
  )

(define (find-leftmost-nt state) (find-leftmost-nt-helper state))

(define (generate-level1 list-states rules prev-states used-names)
  (if (empty? rules)
      '()
      (local [
              (define renamed-states (map (lambda (st)
                                                           (rename-nt used-names st)
                                            )
                                          (first rules)
                                          )
                )
              (define (new-level start) (map (lambda (st) (list start st))
                                             renamed-states
                                             )
                )
              (define leftmost-nt (find-leftmost-nt list-states))
              ]
        (if (boolean? leftmost-nt)
            (if (empty? prev-states)
                '()
                (let* [
                       (prev-state (first prev-states))
                       (prev-leftmost-nt (find-leftmost-nt prev-state))
                       (updated-states (filter (lambda (x) (not (eq? prev-leftmost-nt x))) prev-state))
                       ]
                  (generate-level1 updated-states rules (rest prev-states) used-names)
                  )
                )
            (cons (new-level leftmost-nt) (generate-level1 renamed-states (rest rules) (cons list-states prev-states) used-names))
            )
        )
      )
  )

(define (move-rule-applications-in-list lst) (if (= (length lst) 1)
                          (list (list (first (first lst)) '() ))
                          (cons (list (first (first lst)) (second (second lst))) (move-rule-applications-in-list (rest lst)))
                          )
  )
(define (list-of-states lst) (map (lambda (x) (first x))  lst))
(define (list-of-rules lst) (map (lambda (x) (second x)) lst))

    
;; create-levels
;; derivation-list -> (listof level)
;; To generate a list of levels from wder
(define (create-levels wd)
  (if (= (length wd) 1)
      empty
      (cons (generate-level (first wd) (second wd)) (create-levels (rest wd)))))

;(list-of-states (move-rule-applications-in-list (w-der1 numb>numa '(a b b))))
;(list-of-rules (move-rule-applications-in-list (w-der1 numb>numa '(a b b))))
(generate-level1 '(S)
                 (list-of-rules (move-rule-applications-in-list (w-der1 numb>numa '(a b b))))
                 '()
                 (make-hash)
                 )
