#lang fsm
(define a2nb2n (make-ndpda '(P Q)
                           '(a b)
                           '(S a b)
                           'P
                           '(Q)
                           '(((P ε ε) (Q (S)))
                             ((Q ε (S)) (Q ε))
                             ((Q ε (S)) (Q (a S b)))
                             ((Q a (a)) (Q ε))
                             ((Q b (b)) (Q ε)))))

(define a2nb2n2 (make-ndpda '(P Q)
                            '(a b)
                            '(S a b c)
                            'P
                            '(Q)
                            '(((P ε ε) (Q (c S)))
                              ((Q ε (c S)) (Q ε))
                              ((Q ε (c S)) (Q (a c S b)))
                              ((Q a (a)) (Q ε))
                              ((Q b (b)) (Q ε)))))

;(check-equal? (sm-apply a2nb2n2 '(a b)) 'accept)
;(check-equal? (sm-apply a2nb2n2 '(a a b b)) 'accept)

(define wcw^r (make-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP) (P ,EMP))
                            ((P a ,EMP) (P (a)))
                            ((P b ,EMP) (P (b)))
                            ((P c ,EMP) (Q ,EMP))
                            ((Q a (a)) (Q ,EMP))
                            ((Q b (b)) (Q ,EMP))
                            ((Q ,EMP ,EMP) (F ,EMP)))))

(define P (make-ndpda '(S F)
                      '(a b)
                      '(c d e)
                      'S
                      '(F)
                      `(((S a ,EMP) (F (c d e)))
                        ((F b (c d e)) (F ,EMP)))))

;(check-equal? (sm-apply P '(a b)) 'accept)

#|
Data Definitions

A stacke is either
  1. EMP
  2. (listof symbol)
Interpretation: The stack elements to pop or push 

An list nonterminal, lnt, is a (list state symbol state).
Interpretation: All words that take the pda from the first state to
                the second state by popping the symbol off the stack.

A nonterminal table, nt-table, is a (listof lnt symbol)

;; A lhs is either:
;;  1. lnt
;;  2. symbol

;; A rhs is either:
;;  1. (list symbol lnt)
;;  2. (list symbol lnt lnt)
;;  3. symbol

A cfg-rl is a (list lhs ARROW rhs)

|#


;; lhs rhs -=> cfg-rl
;; Purpose: Create a cfg-rl
(define (mk-cfg-rl a-lhs a-rhs) (list a-lhs ARROW a-rhs))

;; Tests for mk-cfg-rl
;(check-equal? (mk-cfg-rl 'S 'aSb) (list 'S ARROW 'aSb))
;(check-equal? (mk-cfg-rl '(S Z F) '(a (P Z Q))) (list '(S Z F) ARROW '(a (P Z Q))))

;; state symbol stacke state stacke --> pda-rule
;; Purpose: Build a pda-rule
(define (mk-pda-rule from a pop to push)
  (list (list from a pop) (list to push)))

(define PDA-R1 (mk-pda-rule 'P 'a EMP 'Q EMP))
(define PDA-R2 (mk-pda-rule 'A 'c '(a) 'B '(b)))

;; Tests for mk-pda-rule
;(check-equal? PDA-R1 (list (list 'P 'a EMP) (list 'Q EMP)))
;(check-equal? PDA-R2 (list (list 'A 'c '(a)) (list 'B '(b))))


;; pda-rule --> state
;; Purpose: Extract from state
(define (get-from r) (first (first r)))

;; Tests for get-from
;(check-equal? (get-from PDA-R1) 'P)
;(check-equal? (get-from PDA-R2) 'A)


;; pda-rule --> symbol
;; Purpose: Extract read symbol
(define (get-read r) (second (first r)))

;; Tests for get-read
(check-equal? (get-read PDA-R1) 'a)
(check-equal? (get-read PDA-R2) 'c)

;; pda-rule --> stacke
;; Purpose: Extract pop elements
(define (get-pop r) (third (first r)))

;; Tests for get-pop
(check-equal? (get-pop PDA-R1) EMP)
(check-equal? (get-pop PDA-R2) '(a))


;; pda-rule --> state
;; Purpose: Extract to state
(define (get-to r) (first (second r)))

;; Tests for get-to
(check-equal? (get-to PDA-R1) 'Q)
(check-equal? (get-to PDA-R2) 'B)


;; pda-rule --> stacke
;; Purpose: Extract push elements
(define (get-push r) (second (second r)))

;; Tests for get-push
(check-equal? (get-push PDA-R1) EMP)
(check-equal? (get-push PDA-R2) '(b))

;; (listof pda-rule) (listof state) --> (listof pda-rule)
;; Purpose: Eliminate rules that pop more than two elements
(define (generate-beta<2-rules rules states)
  ;; pda-rule (listof state) --> (listof pda-rule)
  ;; Purpose: Create |beta| = 1 rules for given rule
  (define (convert-beta=1 r states)
    ;; (listof symbol) (listof state) --> (listof pda-rule)
    ;; Purpose: Generate pda rules for given pop list using given states
    (define (gen-intermediate-rules beta sts)
      (if (empty? (rest sts))
          '()
          (cons (mk-pda-rule (first sts) EMP (list (first beta)) (first (rest sts)) EMP)
                (gen-intermediate-rules (rest beta) (rest sts)))))
    (let* [(from (get-from r))
           (read (get-read r))
           (beta (get-pop r))
           (to (get-to r))
           (push (get-push r))
           (new-states (build-list
                        (sub1 (length beta))
                        (λ (i) (generate-symbol 'B (cons 'B states)))))]
      (append (list (mk-pda-rule from EMP (list (first beta)) (first new-states) EMP)
                    (mk-pda-rule (last new-states) read (list (last beta)) to push))
              (gen-intermediate-rules (rest beta) new-states))))
  (let* [(beta>=2-rules (filter (λ (r) (and (not (eq? (get-pop r) EMP))
                                            (>= (length (get-pop r)) 2)))
                                rules))
         (beta<2-rules (filter (λ (r) (not (member r beta>=2-rules)))
                               rules))]
    (append beta<2-rules (append-map
                          (λ (r) (convert-beta=1 r states))
                          beta>=2-rules))))

;; Tests for generate-beta<1-rules

;; (listof pda-rule) --> Boolean
;; Purpose: Determine if at most 1 element is popped by every rule
(define (all-beta<2 L)
  (andmap
   (λ (r) (or (eq? (get-pop r) EMP)
              (= (length (get-pop r)) 1)))
   L))

#|(check-pred
 all-beta<2
 (generate-beta<2-rules (list (list (list 'Q 'a '(a b c))
                                    (list 'R '(z))))
                        '(Q R)))

(check-pred
 all-beta<2
 (generate-beta<2-rules (list (list (list 'Q 'a '(a b c))
                                    (list 'R '(z)))
                              (list (list 'Q EMP EMP)
                                    (list 'R EMP))
                              (list (list 'Q 'b '(i j k l))
                                    (list 'R EMP)))
                        '(Q R))) |#


;; (listof pda-rule) (listof symbols) --> (listof pda-rules)
;; Purpose: Substitute pop nothing rules with pop 1 rules
(define (generate-beta=1-rules rls gamma)
  (let* [(beta=0-rls (filter (λ (r) (eq? (get-pop r) EMP)) rls))
         (beta>0-rls (filter (λ (r) (not (member r beta=0-rls))) rls))]
    (append beta>0-rls
            (for*/list ([r beta=0-rls]
                        [g gamma])
              (list (list (get-from r) (get-read r) (list g))
                    (list (get-to r)
                          (if (eq? (get-push r) EMP)
                              (list g)
                              (append (get-push r) (list g)))))))))

;; Tests for generate-beta=1-rules
#|(check-equal?
 (generate-beta=1-rules `(((P a ,EMP) (Q (a b)))) '(S a b))
 '(((P a (S)) (Q (a b S)))
   ((P a (a)) (Q (a b a)))
   ((P a (b)) (Q (a b b)))))
(check-equal?
 (generate-beta=1-rules `(((P a ,EMP) (Q (a b)))
                          ((P b (b) (Q ,EMP)))
                          ((P c ,EMP) (Q ,EMP)))
                        '(S A a b))
 '(((P b (b) (Q ε)))
   ((P a (S)) (Q (a b S)))
   ((P a (A)) (Q (a b A)))
   ((P a (a)) (Q (a b a)))
   ((P a (b)) (Q (a b b)))
   ((P c (S)) (Q (S)))
   ((P c (A)) (Q (A)))
   ((P c (a)) (Q (a)))
   ((P c (b)) (Q (b))))) |#


;; (listof pda-rule) (listof states) --> (listof pda-rule)
;; Purpose: Substitute rules that push more than 2 elements
(define (generate-theta<=2-rules rls sts)
  ;; (listof pda-rule) (listof state) --> (listof pda-rule)
  ;; Purpose: Generate rules with |theta|<=2 for given rules
  (define (gen-theta<=2-rules theta>2-rules sts)
    ;; pda-rule --> (listof pda-rule)
    ;; Purpose: Generate rules with |theta|<=2 for given rule
    (define (gen-rules r)
      ;; (listof state) (listof symbol) (listof symbol) symbol --> (listof pda-rule)
      ;; Purpose: Generate rules with |theta|<=2 for given push list and state list
      (define (process-sts sts push pop read) 
        (if (= (length sts) 2)
            (list (mk-pda-rule (first sts) read pop (second sts) push))
            (cons (mk-pda-rule (first sts) EMP pop (second sts) (append pop (list (first push))))
                  (process-sts (rest sts) (rest push) pop read))))
      (let* [(from (get-from r))
             (read (get-read r))
             (pop (get-pop r))
             (to (get-to r))
             (push (get-push r))
             (new-states (build-list (sub1 (length push))
                                     (λ (i) (generate-symbol 'T (cons 'T sts)))))
             (rev-push (reverse push))]
        (cons (mk-pda-rule from EMP pop (first new-states) (append pop (list (first rev-push))))
              (process-sts (append new-states (list to)) (rest rev-push) pop read))))
    (append-map gen-rules theta>2-rules))
  (let* [(theta>2-rules (filter
                         (λ (r) (and (not (eq? (second (second r)) EMP))
                                     (> (length (second (second r))) 2)))
                         rls))
         (theta<=2-rules (filter
                          (λ (r) (not (member r theta>2-rules)))
                          rls))]
    (append theta<=2-rules (gen-theta<=2-rules theta>2-rules sts))))

;; Tests
;; (listof pda-rule) --> Boolean
;; Purpose: Determine all rules have |theta|<=2
(define (all-theta<=2 rls)
  (andmap (λ (r) (<= (length (second (second r))) 2)) rls))

#|(check-pred
 all-theta<=2
 (generate-theta<=2-rules `(((P ,EMP (Z)) (Q (S c Z)))) '(Q P)))
(check-pred
 all-theta<=2
 (generate-theta<=2-rules `(((P ,EMP (Z)) (Q (S c Z)))
                            ((A a (Z)) (B (A b B))))
                          '(Q P))) |#

;; (listof pda-rule) --> (listof state)
;; Purpose: Extract the list of states in the given rules
(define (extract-states rls)
  (remove-duplicates
   (append-map (λ (r) (list (first (first r))
                            (first (second r))))
               rls)))

;; Tests for extract-states
#|(check-equal? (extract-states `(((P ,EMP (Z)) (Q (S c Z)))))
              '(P Q))
(check-equal? (extract-states `(((P ,EMP (Z)) (Q (S c Z)))
                                ((P a (Z)) (R (S c Z)))
                                ((Q ,EMP (Z)) (T (S c Z)))))
              '(P Q R T)) |#

;; pda --> pda
;; Purpose: Convert given pda to a simple pda
(define (pda2spda p)
  (let* [(pstates (sm-states p))
         (psigma (sm-sigma p))
         (pgamma (sm-gamma p))
         (pstart (sm-start p))
         (pfinals (sm-finals p))
         (prules (sm-rules p))
         (new-start (generate-symbol 'S pstates))
         (bottom (generate-symbol 'Z pgamma))
         (initr (mk-pda-rule new-start EMP EMP pstart (list bottom)))
         (new-final (generate-symbol 'F pstates))
         (frules (map (λ (s) (mk-pda-rule s EMP (list bottom) new-final EMP))
                      pfinals))
         (beta<2-rules (generate-beta<2-rules prules pstates))
         (beta=1-rules (generate-beta=1-rules beta<2-rules (cons bottom pgamma)))
         (theta<=2-rules (generate-theta<=2-rules beta=1-rules
                                                  (extract-states beta=1-rules)))]
    (make-ndpda (append (list  new-final new-start)
                        (remove-duplicates
                         (cons pstart (extract-states theta<=2-rules))))
                        
                psigma
                (cons bottom pgamma)
                new-start
                (list new-final)
                (cons initr (append theta<=2-rules frules)))))

;; Tests for pda2spda
(define P1 (pda2spda a2nb2n))
(define P2 (pda2spda wcw^r))
(define P3 (pda2spda a2nb2n2))
(define SP (pda2spda P))

#|(check-equal? (sm-testequiv? a2nb2n P1) #t)
(check-equal? (sm-testequiv? a2nb2n P1) #t)
(check-equal? (sm-testequiv? wcw^r P2) #t)
(check-equal? (sm-testequiv? wcw^r P2) #t)
(check-equal? (sm-testequiv? a2nb2n2 P3) #t)
(check-equal? (sm-testequiv? a2nb2n2 P3) #t)
(check-equal? (sm-testequiv? P SP) #t)
(check-equal? (sm-apply SP '(a b)) 'accept) |#


;; pda --> cfg
;; Purpose: Convert given pda to a cfg
(define (pda2cfg P)
  (define (keep-rule? r len)
    (and (not (eq? (get-push r) EMP))
         (= (length (get-push r)) len)))

  (define (get-nt ntl tbl)
    (second (assoc ntl tbl)))
  
  ;; (listof pda-rule) (listof state) --> (listof cfg-rl)
  ;; Purpose: Return production rules for the given |theta|=0 pda rules
  (define (gen-theta=0-prs rls sts)
    (for*/list ([r rls] [s sts])
      (mk-cfg-rl (list (get-from r) (first (get-pop r)) s)
                 (list (get-read r) (list (get-to r) EMP s)))))

  ;; (listof pda-rule) (listof state) --> (listof cfg-rl)
  ;; Purpose: Return production rules for the given |theta|=1 pda rules
  (define (gen-theta=1-prs rls sts)
    (for*/list ([r rls] [s sts])
      (mk-cfg-rl (list (get-from r) (first (get-pop r)) s)
                 (list (get-read r) (list (get-to r) (first (get-push r)) s)))))

  ;; (listof pda-rule) (listof state) --> (listof cfg-rl)
  ;; Purpose: Return production rules for the given |theta|=2 pda rules
  (define (gen-theta=2-prs rls sts)
    (for*/list ([r rls] [s1 sts] [s2 sts])
      (mk-cfg-rl (list (get-from r) (first (get-pop r)) s2)
                   (list (get-read r)
                         (list (get-to r) (first (get-push r)) s1)
                         (list s1 (second (get-push r)) s2)))))
  
  ;; (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) --> (listof lnt)
  ;; Purpose: Extract the lnts in the given cfg-rl
  (define (extract-lnts theta=0-prs theta=1-prs theta=2-prs self-prs)
    (remove-duplicates
     (append 
      (append-map (λ (pr) (list (first pr) (second (third pr))))
                  (append theta=0-prs theta=1-prs))
      (append-map (λ (pr) (list (first pr)
                                (second (third pr))
                                (third (third pr))))
                  theta=2-prs)
      (map first self-prs))))

  ;; cfg-rl (listof (list lnt nt)) (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) --> (listof cfg-rule)
  ;; Purpose: Convert all given cfg-rl to a cfg-rule using the given association table
  (define (make-cfg-rules startr st-tbl theta=0-prs theta=1-prs theta=2-prs self-prs)
    (cons (mk-cfg-rl (first startr) (get-nt (third startr) st-tbl))
          (append
           (map (λ (rl) (mk-cfg-rl (get-nt (first rl) st-tbl) (third rl)))
                self-prs)
           (map (λ (rl)
                  (mk-cfg-rl (get-nt (first rl) st-tbl)
                             (if (not (eq? (first (third rl)) EMP))
                                 (los->symbol  (list (first (third rl))
                                                     (get-nt (second (third rl)) st-tbl)))
                                 (get-nt (second (third rl)) st-tbl))))
                (append theta=0-prs theta=1-prs))
           (map (λ (rl) (mk-cfg-rl (get-nt (first rl) st-tbl)
                                   (if (not (eq? (first (third rl)) EMP))
                                       (los->symbol (list
                                                     (first (third rl))
                                                     (get-nt (second (third rl)) st-tbl)
                                                     (get-nt (third (third rl)) st-tbl)))
                                       (los->symbol (list
                                                     (get-nt (second (third rl)) st-tbl)
                                                     (get-nt (third (third rl)) st-tbl))))))
                theta=2-prs))))
  (let* [(p (pda2spda P))
         (pstates (sm-states p)) (psigma (sm-sigma p))   (pgamma (sm-gamma p))
         (pstart (sm-start p))   (pfinals (sm-finals p)) (prules (sm-rules p))
         (start (generate-symbol 'S '(S)))
         (bottom (first (filter (λ (s) (not (member s (sm-gamma P)))) pgamma)))
         (startr (mk-cfg-rl start (list (sm-start P) bottom (first pfinals))))
         (pstates-nostart (remove pstart pstates))
         (prules-nostartrls (filter (λ (r) (not (eq? (first (first r)) pstart)))
                                    prules))
         (theta=0-prs (gen-theta=0-prs (filter (λ (r) (eq? (get-push r) EMP))
                                               prules-nostartrls)
                                       pstates-nostart))
         (theta=1-prs (gen-theta=1-prs (filter (λ (r) (keep-rule? r 1)) prules-nostartrls)
                                       pstates-nostart))
         (theta=2-prs (gen-theta=2-prs (filter (λ (r) (keep-rule? r 2)) prules-nostartrls)
                                       pstates-nostart))
         (self-prs (map (λ (s) (mk-cfg-rl (list s EMP s)  EMP)) pstates))
         (st-list (cons (third startr)
                        (extract-lnts theta=0-prs theta=1-prs theta=2-prs self-prs))) 
         (st-tbl (map (λ (lnt) (list lnt (generate-symbol 'G '(G)))) st-list))
         (new-rls (make-cfg-rules startr st-tbl theta=0-prs theta=1-prs theta=2-prs self-prs))]
    (make-cfg (cons start (map second st-tbl)) psigma new-rls start)))

(define a2nb2n-grammar (pda2cfg a2nb2n))
(define wcw^r-grammar (pda2cfg wcw^r))
;(define a2nb2n2-grammar (pda2cfg a2nb2n2))
(define P-grammar (pda2cfg P))


#|(check-equal? (last (grammar-derive a2nb2n-grammar '(a b))) 'ab)
(check-equal? (last (grammar-derive a2nb2n-grammar '(a a b b))) 'aabb)
(check-equal? (last (grammar-derive wcw^r-grammar '(a c a))) 'aca)
(check-equal? (last (grammar-derive wcw^r-grammar '(a b c b a))) 'abcba)
;(check-equal? (last (grammar-derive a2nb2n2-grammar '(a b))) 'ab)
;(check-equal? (last (grammar-derive a2nb2n2-grammar '(a a b b))) 'aabb)
(check-equal? (last (grammar-derive P-grammar '(a b))) 'ab) |#




;; Assume grammar produced by pda2cfg
(define (useless-nts G)

  (define (helper rls acc)
    (let [(new-useful-rls
           (filter
            (λ (r) (and (member (first r) acc)
                        (andmap (λ (nts) (member nts acc))
                                (filter (λ (a) (not (member a (cons EMP (grammar-sigma G)))))
                                        (third r)))))
            rls))]
      (if (empty? new-useful-rls)
          acc
          (helper (foldr (λ (r acc) (remove r acc)) rls new-useful-rls)
                  ;(filter
                  ;(λ (s) (not (member s (cons EMP (grammar-sigma G)))))
                  (remove-duplicates
                   (append acc
                           (map first new-useful-rls)
                           (map third new-useful-rls)))))))  ;)
  
  (let [(rls (map (λ (r) (list (first r) ARROW (symbol->fsmlos (third r))))
                  (grammar-rules G)))
        (start-rl (first (filter (λ (r) (eq? (first r) (grammar-start G)))
                                 (grammar-rules G))))]
    (helper rls (list (third start-rl)))))


;(useless-nts a2nb2n-grammar)

(define prls (grammar-rules P-grammar))
(define pstart (grammar-start P-grammar))


;; cfg --> pda
;; Purpose: Transform the given cfg into a pda
(define (cfg2pda G)
  (let [(nts (grammar-nts G))
        (sigma (grammar-sigma G))
        (start (grammar-start G))
        (rules (grammar-rules G))]
    (make-ndpda '(S Q)
                sigma
                (append nts sigma)
                'S
                (list 'Q)
                (append
                 (list (list (list 'S EMP EMP) (list 'Q (list start))))
                 (map (λ (r)
                        (list (list 'Q EMP (list (first r)))
                              (list 'Q
                                    (if (eq? (third r) EMP)
                                        EMP
                                        (symbol->fsmlos (third r))))))
                      rules)
                 (map (λ (a) (list (list 'Q a (list a)) (list 'Q EMP)))
                      sigma)))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chomsky Normal Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nts Definitions
;;  S-0: start nts representing the empty word, if it is in the language
;;  Y-n: terminals that are not in chomsky form
;;  X-n: non-terminals that are not in chomsky form

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove empties

;; cfg -> cfg
;; Purpose: Remove empties
(define (rm-empties cfg)

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is a terminal
  (define (terminal? sym)
    (contains? sym (grammar-sigma cfg)))

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is an non-terminal
  (define (nts? sym)
    (contains? sym (grammar-nts cfg)))

  ;; symbol (listof symbol) -> boolean
  ;; Purpose: Check whether list contains given symbol
  (define (contains? elem list)
    (cond ((empty? list) #f)
          ((equal? elem (car list)) #t)
          (else (contains? elem (cdr list)))))
  
  ;; rules -> (listof nts)
  ;; Purpose: Find nts that can be replaced by empty
  (define (emp-nts r)  
    ;; rhs (listof nts) -> boolean
    ;; Purpose: Check whether righthand-side can be replaced by empty
    (define (leads-to-empty? rhs empties)
      (let ((ts (filter terminal? (symbol->fsmlos rhs))))
        (if (not (empty? ts))
            #f
            (or (eq? EMP rhs)
                (andmap (lambda (x) (contains? x empties)) (symbol->fsmlos rhs))))))  
    ;; nts -> (listof nts)
    ;; Purpose: Check if one nt can be replaced by empty
    (define (get-empties-helper nts empties)
      (map (lambda (z) (car z))
           (filter (lambda (y) (leads-to-empty? (caddr y) empties))
                   (filter (lambda (x) (contains? nts (symbol->fsmlos (caddr x)))) r))))
    ;; (listof symbol) (listof nts) -> (listof nts)
    ;; Purpose: Find nts that can be replaced by empty
    ;; Accumulator Invariants:
    ;;  to-visit = list of nts that still need to be explored
    ;;  empties = list of all nts that can be replaced by empty
    (define (get-empties to-visit empties)
      (let ((new-empties (filter (lambda (x) (not (contains? x empties)))
                                 (append-map (lambda (x) (get-empties-helper x empties)) to-visit))))
        (if (empty? new-empties)
            '()
            (append new-empties (get-empties new-empties
                                             (append new-empties empties))))))
    
    (get-empties '(ε) '()))

  ;; rule -> (listof rule)
  ;; Purpose: Find the list of new combinations for every rule 
  (define (all-combinations rule)
  
    ;; (listof symbol) -> symbol
    ;; Purpose: Convert a list of symbols to one symbol
    (define (list->symbol list)
      ;; (listof symbol) -> string
      ;; Purpose: Convert a list of symbols to one string
      (define (list->symbol-helper l)
        (if (empty? l)
            ""
            (string-append (symbol->string (car l))
                           (list->symbol-helper (cdr l)))))
      (string->symbol (list->symbol-helper list)))
  
    ;; (listof rhs) (listof rhs) -> Boolean
    ;; Purpose: Check if all elements of the first list are in the second list
    (define (all-rules? rhs acc)
      (cond ((empty? rhs) #t)
            ((contains? (car rhs) acc) (all-rules? (cdr rhs) acc))
            (else #f))) 

    ;; rhs nts integer -> (listof integer)
    ;; Purpose: Finds the indices of the given empty nts
    (define (indices rhs empty acc)
      ;; rhs integer -> rhs
      ;; Purpose: Replaces a found empty nts with a dummy string, so that its
      ;;          index is not used again
      (define (replace-emp rhs i)
        (if (= 0 i)
            (cons "dummy" (cdr rhs))
            (cons (car rhs) (replace-emp (cdr rhs) (sub1 i)))))
      (if (= 0 acc)
          '()
          (cons (index-of rhs empty)
                (indices (replace-emp rhs (index-of rhs empty))
                         empty (sub1 acc)))))
  
    ;; (listof integer) integer integer -> (listof integer)
    ;; Purpose: Find the complement of the given list
    (define (complement list num length)
      (cond ((= num length) '())
            ((contains? num list)
             (complement list (add1 num) length))
            (else (cons num (complement list (add1 num) length)))))
    
    ;; rhs (listof (listof integer)) -> (listof rhs)
    ;; Purpose: Finds all combinations for a rhs for a specific empty nts
    (define (new-rhs rhs combs)
      ;; (listof integer) -> rhs
      ;; Purpose: Find the rhs of given list of indices
      (define (extract-indexed-elems is)
        (map (lambda (x)
               (list-ref rhs x))
             (complement is 0 (length rhs))))
      (map (lambda (y)
             (extract-indexed-elems y))
           combs))
    ;; rhs nts -> rhs 
    ;; Purpose: Remove all empty nts one by one
    (define (make-combs-rhs-helper rhs empties)
      (if (empty? rhs)
          '()
          (append (append-map (lambda (x)
                                (new-rhs (car rhs)
                                         (combinations
                                          (indices (car rhs) x (length (filter (lambda (y) (equal? x y)) (car rhs)))))))                          
                              empties)
                  (make-combs-rhs-helper (cdr rhs) empties))))
    ;; (listof rhs) nts (listof rhs) -> (listof rhs)
    ;; Purpose: Find all combinations for a given list of rhs 
    ;; Accumulator invariants:
    ;;  acc = stores the last found list of rhs 
    (define (make-combs-rhs rhs-list empties acc)
      (if (and (all-rules? rhs-list acc)
               (all-rules? acc rhs-list))
          '()
          (let ((new-combs (remove-duplicates (make-combs-rhs-helper rhs-list empties))))
            (append new-combs
                    (make-combs-rhs new-combs empties rhs-list)))))
    ;; Termination argument:
    ;;  The function finds new combinations of rhs by eliminating the
    ;;  empty nts one by one. An accumulator stores the list of rhs
    ;;  combinations found in the last step. Once that lists repeats,
    ;;  there are no new combinations to be found and the function halts.
  
    (let ((rhs (remove-duplicates
                (filter (lambda (x) (not (empty? x)))
                        (make-combs-rhs (list (symbol->fsmlos (caddr rule))) (emp-nts (grammar-rules cfg)) '())))))      
      (map (lambda (x) (list (car rule) '-> (list->symbol x))) rhs)))

  (if (empty? (emp-nts (grammar-rules cfg)))
             cfg
      (let* ((empties (emp-nts (grammar-rules cfg)))
             (new-start (generate-symbol 'S (grammar-nts cfg)))
             (new-rules (remove-duplicates
                         (if (contains? (grammar-start cfg) empties)
                            (cons `(,new-start -> ε) (cons `(,new-start -> ,(grammar-start cfg))
                                                    (filter (lambda (x) (not (eq? EMP (caddr x))))
                                                            (append-map (lambda (x) (all-combinations x)) (grammar-rules cfg)))))
                            (filter (lambda (x) (not (eq? EMP (caddr x))))
                                    (append-map (lambda (x) (all-combinations x)) (grammar-rules cfg))))))
             (new-nts (remove-duplicates (append (map (lambda (x) (car x)) new-rules)
                                                 (filter nts? (append-map symbol->fsmlos (map (lambda (x) (caddr x))
                                                                                              (grammar-rules cfg)))))))
             (new-start (if (contains? (grammar-start cfg) empties)
                            new-start
                            (grammar-start cfg))))   
               (make-cfg new-nts
                  (grammar-sigma cfg)
                  new-rules
                  new-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kettenregeln

;; cfg -> cfg
;; Purpose: Remove Kettenregeln
(define (rm-ketten cfg)

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is a terminal
  (define (terminal? sym)
    (contains? sym (grammar-sigma cfg)))

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is an non-terminal
  (define (nts? sym)
    (contains? sym (grammar-nts cfg)))

  ;; symbol (listof symbol) -> boolean
  ;; Purpose: Check whether list contains given symbol
  (define (contains? elem list)
    (cond ((empty? list) #f)
          ((equal? elem (car list)) #t)
          (else (contains? elem (cdr list)))))
  
  ;; rules -> Boolean
  ;; Purpose: Check if there are any Ketten in the given ruels
  (define (no-more-ketten? rules)
    (empty? (filter (lambda (x) (kette? (caddr x))) rules)))
  
  ;; rhs -> Boolean
  ;; Purpose: Check if given rhs is a Kette
  (define (kette? rhs)
    (and (not (eq? EMP rhs))
         (not (terminal? rhs))
         (eq? 1 (length (symbol->fsmlos rhs)))))
  
  ;; rule -> rules
  ;; Purpose: Find all rules from a given Kette
  (define (kettenregeln rule)
    (let* ((ketten-rules (filter (lambda (x) (eq? (car x) (caddr rule))) (grammar-rules cfg)))
           (new-rules (map (lambda (x) `(,(car rule) ,ARROW ,(caddr x))) ketten-rules)))
      new-rules))
  
  ;; rules -> rules
  ;; Purpose: Traverse a cfg's rules and remove the Ketten
  (define (make-rules rules)
    (cond ((empty? rules) '())
          ((kette? (caddr (car rules))) (append (kettenregeln (car rules)) (make-rules (cdr rules))))
          (else (cons (car rules) (make-rules (cdr rules))))))
  
  ;; rules -> rules
  ;; Purpose: Function to traverse a cfg's rules
  (define (rm-ketten-helper rules)
    (if (no-more-ketten? rules)
        rules
        (rm-ketten-helper (make-rules (remove-duplicates rules)))))
  ;; Termination argument:
  ;;  Function removes all Ketten recursively until there are
  ;;  no more. Then, halts.

  (if (no-more-ketten? (grammar-rules cfg))
             cfg
      (let* ((new-rules (remove-duplicates (rm-ketten-helper (grammar-rules cfg))))
             (new-nts (remove-duplicates (append (map (lambda (x) (car x)) new-rules)
                                                 (filter nts? (append-map symbol->fsmlos (map (lambda (x) (caddr x))
                                                                                              (grammar-rules cfg))))))))
               (make-cfg new-nts
                         (grammar-sigma cfg)
                         new-rules
                  (grammar-start cfg))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chomsky

;; cfg -> cfg
;; Purpose: Convert a given cfg to Chomsky Normal form
(define (chomsky cfg)

  ;; symbol (listof symbol) -> boolean
  ;; Purpose: Check whether list contains given symbol
  (define (contains? elem list)
    (cond ((empty? list) #f)
          ((equal? elem (car list)) #t)
          (else (contains? elem (cdr list)))))

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is a terminal
  (define (terminal? sym)
    (contains? sym (grammar-sigma cfg)))
  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is an non-terminal
  (define (nts? sym)
    (contains? sym (grammar-nts cfg)))

  ;; cfg -> Boolean
  ;; Purpose: Check if cfg is in Chomsky form
  (define (chomsky? cfg)
    ;; rule -> Boolean?
    ;; Purpose: Check whether rule is already in chomsky 
    (define (valid? rule)
      (or (terminal? (caddr rule))
          (equal? EMP (caddr rule))
          (and (= 2 (length (symbol->fsmlos (caddr rule))))
               (nts? (car (symbol->fsmlos (caddr rule))))
               (nts? (cadr (symbol->fsmlos (caddr rule)))))))
    (empty? (filter (lambda (x) (not (valid? x))) (grammar-rules cfg)))) 
  
  ;; cfg -> rules
  ;; Purpose: Helper function for chomsky, makes new rules
  (define (chomsky-helper cfg)

    ;; (listof symbol) -> symbol
    ;; Purpose: Convert a list of symbols to one symbol
    (define (list->symbol list)
      ;; (listof symbol) -> string
      ;; Purpose: Convert a list of symbols to one string
      (define (list->symbol-helper l)
        (if (empty? l)
            ""
            (string-append (symbol->string (car l)) 
                           (list->symbol-helper (cdr l)))))
      (string->symbol (list->symbol-helper list)))

    ;; sigma (listof sigma) integer -> integer
    ;; Purpose: Find the ref of the sigma element 
    (define (counter elem list int)
      (if (equal? elem (car list))
          int
          (counter elem (cdr list) (add1 int))))

    ;; rule -> Boolean?
    ;; Purpose: Check whether rule is already in chomsky 
    (define (valid? rule)
      (or (terminal? (caddr rule))
          (and (= 2 (length (symbol->fsmlos (caddr rule))))
               (nts? (car (symbol->fsmlos (caddr rule))))
               (nts? (cadr (symbol->fsmlos (caddr rule)))))))
    ;; rule -> Boolean?
    ;; Purpose: Check whether rule is not yet in chomsky
    (define (invalid? rule)
      (not (valid? rule)))

    ;; rhs -> rhs
    ;; Purpose: Replace the ts in a rhs
    (define (replace-ts rhs)
      ;; t -> nt
      ;; Purpose: Replace t with a nt
      (define (new-t t)
        (string->symbol (string-append "Y-" (number->string (counter t (grammar-sigma cfg) 0)))))
      (cond ((empty? rhs) '())
            ((terminal? (car rhs)) (cons (new-t (car rhs)) (replace-ts (cdr rhs))))
            (else (cons (car rhs) (replace-ts (cdr rhs))))))

    ;; rule integer -> rules
    ;; Purpose: For a nt rule, make new rules in which rhs is length 2
    (define (new-X rule int)
      ;; rhs integer -> rhs
      ;; Purpose: Make a new rhs
      (define (new-nt rhs int)
        (cons (car rhs) (list (string->symbol (string-append "X-" (number->string int))))))
      ;; rhs integer -> rhs
      ;; Purpose: Make new rhs for all rules
      (define (replace-nts rhs int)
        (if (= 2 (length rhs))
            (list rhs) 
            (cons (new-nt rhs int)
                  (replace-nts (cdr rhs) (add1 int)))))
      ;; rhs integer -> rules
      ;; Purpose: Make new nts that follow chomsky
      (define (nts rhs int)
        (if (= 2 (length rhs))
            '()
            (cons (string->symbol (string-append "X-" (number->string int)))
                  (nts (cdr rhs) (add1 int)))))
      (map (lambda (x y) `(,x -> ,(list->symbol y))) (cons (car rule) (nts (caddr rule) int)) (replace-nts (caddr rule) int)))
    ;; (listof integer) (listof integer) -> (listof integer)
    ;; Purpose: Sums all numbers in list before each number
    ;; Accumulator invariants:
    ;;  c = consumed integers as sum 
    (define (sum-list c uc)
      (if (empty? uc)
          '()
          (cons (+ (foldl + 0 c) (car uc))
                (sum-list (list (+ (foldl + 0 c) (car uc))) (cdr uc)))))
    ;; rules -> (listof integer)
    ;; Purpose: Integers for new nts
    (define (ints rules)
      (if (empty? rules)
          '()
          (sum-list '() (cons 0 (drop-right (map (lambda (x) (if (> (- (length (symbol->fsmlos (caddr x))) 2) 0)
                                                                 (- (length (symbol->fsmlos (caddr x))) 2)
                                                                 0))
                                                 rules) 1)))))

    ;; rules -> rules
    ;; Purpose: Make new rules that follow chomsky
    (define (make-X-rules rules)
      (append-map (lambda (y z) (new-X y z))
                  (map (lambda (x) `(,(car x) -> ,(replace-ts (symbol->fsmlos (caddr x))))) rules)
                  (ints rules)))

    ;; sigma natnum -> rules
    ;; Purpose: Create new rules for each sigma 
    (define (make-Y-rules rules)
      (let ((sigma (remove-duplicates
                    (filter (lambda (y) (terminal? y))
                            (append-map symbol->fsmlos (map (lambda (x) (caddr x)) rules))))))
        (map (lambda (x) `(,(string->symbol (string-append "Y-" (number->string (counter x (grammar-sigma cfg) 0)))) -> ,x)) sigma)))

    (let ((valid (filter (lambda (x) (or (valid? x)
                                         (eq? EMP (caddr x)))) (grammar-rules cfg)))
          (invalid (filter (lambda (x) (and (invalid? x)
                                            (not (eq? EMP (caddr x))))) (grammar-rules cfg))))
      (remove-duplicates (append valid (make-Y-rules invalid) (make-X-rules invalid)))))

  (if (chomsky? cfg)
      cfg
      (let* ((new-cfg ((compose rm-ketten rm-empties) cfg))
             (new-rules (chomsky-helper new-cfg))
             (new-nts (remove-duplicates
                       (append (map (lambda (x) (car x)) new-rules)
                               (filter (lambda (z) (nts? z))
                                       (append-map (lambda (y) (symbol->fsmlos (caddr y))) new-rules))))))
        (make-cfg new-nts
                  (grammar-sigma new-cfg)
                  new-rules 
                  (grammar-start new-cfg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol (listof symbol) -> boolean
;; Purpose: Check whether list contains given symbol
(define (contains? elem list)
  (cond ((empty? list) #f)
        ((equal? elem (car list)) #t)
        (else (contains? elem (cdr list)))))

;; symbol -> boolean
;; Purpose: Check whether symbol is a terminal
(define (terminal? cfg sym)
  (contains? sym (grammar-sigma cfg)))

;; symbol -> boolean
;; Purpose: Check whether symbol is an non-terminal
(define (nts? cfg sym)
  (contains? sym (grammar-nts cfg)))

;.................................................

;; (listof symbol) -> symbol
;; Purpose: Convert a list of symbols to one symbol
(define (list->symbol list)
  ;; (listof symbol) -> string
  ;; Purpose: Convert a list of symbols to one string
  (define (list->symbol-helper l)
    (if (empty? l)
        ""
        (string-append (symbol->string (car l)) 
                       (list->symbol-helper (cdr l)))))
  (string->symbol (list->symbol-helper list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A config is a struct that contains:
;; - a nonterminal in the original grammar (nt)
;; - a unique number to rename the nts in the original grammar (n)
;; - a representation = renamed nt (rep)
;; - renamed rhs of rules for rep (rules)
(define-struct config (nt n rep rules) #:transparent)

;; renamed-config, newnt, rhss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> (listof config)
;; Purpose: Given a cfg, create a list of configs
(define (make-configs cfg)
  (let* ((cfg-rules (grammar-rules cfg)))
    ;; (listof rule) integer -> (listof config)
    ;; Purpose: Convert the nts in the rules to A-n representation
    ;;          without updating the rhss
    ;; Accumulator invariants:
    ;;  n = next natnum for renaming nts
    (define (make-A-rules rules n)
      (if (empty? rules)
          '()
          (let* ((rule (car rules))
                 (same-lhs-rules (filter (lambda (x) (equal? (car rule) (car x))) (cdr rules)))
                 (different-lhs-rules (filter (lambda (x) (not (equal? (car rule) (car x)))) (cdr rules)))
                 (rhss (append (list (caddr rule))
                               (map (lambda (x) (caddr x)) same-lhs-rules)))
                 (new-config (config (car rule) n (string->symbol (string-append "A-" (number->string n))) rhss)))
            (cons new-config
                  (make-A-rules different-lhs-rules (add1 n))))))
    (make-A-rules cfg-rules 0)))

;.................................................

;; cfg (listof config) -> (listof config)
;; Purpose: Convert the right hands sides to be in proper A-notation and split up in lists
(define (convert-rhs-to-config cfg loA)
  ;; config -> (listof (listof symbol))
  ;; Purpose: Create new rhs lists, all elems are in A-notation and split up into sublists
  (define (convert-1config A)
    (let* ((rhs (config-rules A)))
      ;; symbol -> (listof symbol)
      ;; Purpose: Convert each rule on the right hand side
      (define (convert-1rhs elem)
        ;; (listof symbol) -> (listof symbol)
        ;; Purpose: Convert a list of symbols to A-notation
        (define (convert-to-A los)
          (cond ((empty? los) '())
                ((or (terminal? cfg (car los))
                     (equal? EMP (car los))) (cons (car los) (convert-to-A (cdr los))))
                (else
                 (let ((corresponding-A (car (filter (lambda (x) (equal? (car los) (config-nt x))) loA))))
                   (cons (config-rep corresponding-A) (convert-to-A (cdr los)))))))
        (if (or (terminal? cfg elem)
                (equal? EMP elem))
            (list elem)
            (convert-to-A (symbol->fsmlos elem))))
      (map convert-1rhs rhs)))
  (let ((new-rhs (map convert-1config loA)))
    (map (lambda (A rules) (config (config-nt A)
                                   (config-n A)
                                   (config-rep A)
                                   rules))
         loA new-rhs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol -> (listof config)
;; Purpose: Find the respective config for an A-n symbol
(define (get-config rep configs)
  (car (filter (lambda (x) (equal? rep (config-rep x))) configs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions
;; 1. Convert structs one by one
;; 2. Write substitute function
;; 3. Write introduce-new-rules function
;; 4. Combine in new cfg

;; cfg config (listof (listof symbol)) (listof (listof symbol)) (listof config) (listof config) -> (listof config)
;; Purpose: Given one config, convert its rules to Greibach by
;;          substituting and introducing new rules in B-notation
(define (surgery-on-A cfg A rules new-rules new-A As)

  ;; symbol (listof rule) -> (listof (listof rule))
  ;; Purpose: Substitute a given symbol with its rules
  (define (substitute A-to-sub rest-rules)
    (let ((A-to-sub-rules (config-rules (get-config A-to-sub As)))) 
      (map (lambda (x) (append x rest-rules)) A-to-sub-rules)))

  ;; symbol (listof rule) -> (listof config)
  ;; Purpose: Introduce new configs 
  (define (introduce-new-configs A-to-sub rest-rules)
    (let* ((new-nt (string->symbol (string-append "B-" (number->string (config-n (get-config A-to-sub As))))))
           (new-n #f)
           (new-rep (string->symbol (string-append "B-" (number->string (config-n (get-config A-to-sub As))))))
           (new-rules (cons (append rest-rules (list new-rep))
                            (list rest-rules))))
      (list (config new-nt new-n new-rep new-rules))))

  (if (empty? rules)
      (cons (config (config-nt A) (config-n A) (config-rep A) (remove-duplicates new-rules)) new-A)
      (let* ((first-rhs (car (car rules)))
             (i (config-n A)))
        (cond ((or (terminal? cfg first-rhs)
                   (equal? EMP first-rhs)
                   (< i
                      (config-n (get-config first-rhs As))))
               (surgery-on-A cfg
                             A
                             (cdr rules)
                             (append (list (car rules)) new-rules)
                             new-A
                             As))
              ((> i
                  (config-n (get-config (car (car rules)) As)))
               (let ((first-sym (car (car rules)))
                     (rest-syms (cdr (car rules))))
                 (surgery-on-A cfg
                               A
                               (append (cdr rules) (substitute first-sym rest-syms))
                               new-rules
                               new-A
                               As)))
              (else
               (let ((new-rep (list (string->symbol (string-append "B-" (number->string (config-n A)))))))
                 (surgery-on-A cfg
                               A
                               (append (cdr rules) (map (lambda (x) (append x new-rep)) (append (cdr rules) new-rules)))
                               new-rules
                               (append (introduce-new-configs (car (car rules)) (cdr (car rules))) new-A)
                               As)))))))

;.................................................

;; (listof config) -> (listof config)
;; Purpose: Call surgery function
(define (combine-post-surgery-configs cfg configs acc)
  (if (empty? configs)
      (let ((Bconfigs (filter (lambda (x) (not (config-n x))) acc))
            (other (filter (lambda (x) (config-n x)) acc)))
        (append other (reverse Bconfigs)))
      (let ((new-config (surgery-on-A cfg (car configs) (group cfg (car configs) (append acc configs)) '() '() (append acc configs))))
        (combine-post-surgery-configs cfg (cdr configs) (append new-config acc)))))

;.................................................

;; (listof (listof symbol)) -> (listof (listof symbol))
;; Purpose: Group from terminal/empty to lowest j to highest j
(define (group cfg conf As)
  (let* ((rules (config-rules conf))
         (terminal/emp (filter (lambda (x) (or (terminal? cfg (car x))
                                               (equal? EMP (car x)))) rules))
         (other (filter (lambda (x) (not (or (terminal? cfg (car x))
                                             (equal? EMP (car x))))) rules))
         (i=j (filter (lambda (x) (= (config-n conf) (config-n (get-config (car x) As)))) other))
         (other2 (filter (lambda (x) (not (= (config-n conf) (config-n (get-config (car x) As))))) other)))
    (append terminal/emp other2 i=j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) (listof config) -> (listof config)
;; Purpose: Final substitutions
(define (final-subs cfg configs As)

  ;; symbol (listof rule) -> (listof (listof rule))
  ;; Purpose: Substitute a given symbol with its rules
  (define (substitute A-to-sub rest-rules)
    (let ((A-to-sub-rules (config-rules (get-config A-to-sub As)))) 
      (map (lambda (x) (append x rest-rules)) A-to-sub-rules)))

  ;; (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose: Substitute if needed 
  (define (make-new-rules rules)
    (cond ((empty? rules) '())
          ((or (terminal? cfg (car (car rules)))
               (equal? EMP (car (car rules))))
           (cons (car rules) (make-new-rules (cdr rules))))
          (else 
           (append (substitute (car (car rules)) (cdr (car rules)))
                   (make-new-rules (cdr rules))))))

  (if (empty? configs)
      '()
      (let* ((c (car configs))
             (rules (config-rules c))
             (new-rules (make-new-rules rules))
             (new-config (config (config-nt c) (config-n c) (config-rep c) new-rules)))
        (cons new-config
              (final-subs cfg (cdr configs) (append (cdr As) (list new-config))))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) (listof config) -> (listof config)
;; Purpose: Convert back to regular notation
(define (convert-back-to-regular-notation cfg configs As)
  
  ;; symbol -> symbol
  ;; Purpose: Substitute a symbol in A-notation back to regular notation
  (define (sub A-to-sub)
    (config-nt (get-config A-to-sub As)))

  ;; (listof symbol) -> (listof symbol)
  ;; Purpose: Sub each rule independently 
  (define (make-new-rule rule)
    (cond ((empty? rule) '())
          ((or (terminal? cfg (car rule))
               (equal? EMP (car rule)))
           (cons (car rule) (make-new-rule (cdr rule))))
          (else (cons (sub (car rule)) (make-new-rule (cdr rule))))))

  (if (empty? configs)
      '()
      (let* ((c (car configs))
             (rules (config-rules c))
             (new-rules (map make-new-rule rules))
             (new-config (config (config-nt c) (config-n c) (config-rep c) new-rules)))
        (cons new-config
              (convert-back-to-regular-notation cfg (cdr configs) As)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) -> cfg
;; Purpose: Convert a list of configs to a cfg
(define (configs->cfg cfg configs)
  (let* ((new-nts (map config-nt configs))
         (new-rules
          (remove-duplicates
           (append-map (lambda (conf)
                         (map (lambda (rule) `(,(config-nt conf) -> ,(list->symbol rule))) (config-rules conf)))
                       configs))))
    (make-cfg new-nts
              (grammar-sigma cfg)
              new-rules
              (grammar-start cfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> cfg
;; Purpose: Filter out impossible rules from cfg 
(define (filter-rules cfg)

  (define (filter-rules-helper lhs-nts rules acc)
    (let* ((new-rules (filter (lambda (x) (or (terminal? cfg (caddr x))
                                              (equal? EMP (caddr x))
                                              (member (caddr x) lhs-nts)
                                              (and (member (car (symbol->fsmlos (caddr x))) lhs-nts)
                                                   (member (cadr (symbol->fsmlos (caddr x))) lhs-nts)))) rules))
           (new-acc (filter (lambda (x) (not (member x new-rules))) rules))
           (new-lhs (map (lambda (x) (car x)) new-rules)))
      (if (empty? acc)
          new-rules
          (filter-rules-helper new-lhs new-rules new-acc))))
  
  (let* ((nts (grammar-nts cfg))
         (sigma (grammar-sigma cfg))
         (rules (grammar-rules cfg))
         (start (grammar-start cfg))
         (lhs-nts (map (lambda (x) (car x)) rules))
         (new-rules (filter-rules-helper lhs-nts rules rules))
         (new-lhs2 (map (lambda (x) (car x)) new-rules))
         (new-nts (filter (lambda (x) (member x new-lhs2)) nts)))
    (make-cfg new-nts
              sigma
              new-rules
              start)))

#;(define c (make-cfg '(S A B C D E F)
                    '(a b)
                    '((S -> ε)
                      (S -> a)
                      (A -> a)
                      (B -> b)
                      (B -> AB)
                      (S -> BB)
                      (S -> FD)
                      (S -> FA))
                    'S))

;(filter-rules c)


;; cfg -> cfg
;; Purpose: Convert a cfg to Greibach Normal Form
(define (greibach cfg)
  (let* ((chomsky-form (chomsky cfg))
         (chomsky-form-filtered (filter-rules chomsky-form))
         (init-As (convert-rhs-to-config chomsky-form-filtered (make-configs chomsky-form-filtered)))
         (As (final-subs chomsky-form
                         (combine-post-surgery-configs chomsky-form init-As '())
                         (combine-post-surgery-configs chomsky-form init-As '()))))
    (configs->cfg chomsky-form
                  (convert-back-to-regular-notation chomsky-form
                                                    As
                                                    As))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> pda
;; Purpose: Transform the given cfg into a pda
#;(define (cfg->pda G)
  (let [(nts (grammar-nts G))
        (sigma (grammar-sigma G))
        (start (grammar-start G))
        (rules (grammar-rules G))]
    (make-ndpda '(S Q)
                sigma
                (remove-duplicates (cons 'S (append nts sigma)))
                ;(append nts sigma)
                'S
                (list 'Q)
                (append (list (list (list 'S EMP EMP) (list 'Q (list 'S))))
                        (map (λ (r)
                               (list (list 'Q EMP (list (first r)))
                                     (list 'Q (if (eq? (third r) EMP)
                                                  EMP
                                                  (symbol->fsmlos (third r))))))
                             rules)
                        (map (λ (a) (list (list 'Q a (list a)) (list 'Q EMP)))
                             sigma)))))

(define anbn (make-ndpda '(S M F)
                         '(a b)
                         '(a)
                         'S
                         '(F)
                         '(((S ε ε) (M ε))
                           ((S a ε) (S (a)))
                           ((M b (a)) (M ε))
                           ((M ε ε) (F ε)))))

;(sm-graph anbn)
;(greibach (pda2cfg anbn))
;(sm-graph (cfg2pda (greibach (pda2cfg anbn))))


#| (cfg
 '(X-34 X-31 G-3572344 X-30 X-29 G-3572342 X-28 G-3572341 X-15 G-3572339 X-14 G-3572337 X-13 G-3572335 X-12 G-3572316 X-5 S-3572315 X-1 Y-1 Y-0 G-3572343 G-3572321 S)
 '(a b)
 (list
  (cfg-rule 'X-34 '(a X-30 G-3572321))
  (cfg-rule 'X-34 '(a X-34 G-3572321))
  (cfg-rule 'X-34 '(b G-3572321))
  (cfg-rule 'X-31 '(a X-29 G-3572344))
  (cfg-rule 'G-3572344 '(a X-31))
  (cfg-rule 'X-30 '(a X-29 G-3572343))
  (cfg-rule 'X-29 '(a X-29 G-3572342))
  (cfg-rule 'G-3572342 '(a X-29))
  (cfg-rule 'X-28 '(a X-29 G-3572341))
  (cfg-rule 'G-3572341 '(a X-28))
  (cfg-rule 'X-15 '(a X-29 G-3572339))
  (cfg-rule 'G-3572339 '(a X-15))
  (cfg-rule 'X-14 '(a X-29 G-3572337))
  (cfg-rule 'G-3572337 '(a X-14))
  (cfg-rule 'X-13 '(a X-29 G-3572335))
  (cfg-rule 'G-3572335 '(a X-13))
  (cfg-rule 'X-12 '(a X-29 G-3572316))
  (cfg-rule 'G-3572316 '(a G-3572344))
  (cfg-rule 'G-3572316 '(a G-3572343))
  (cfg-rule 'G-3572316 '(a G-3572342))
  (cfg-rule 'G-3572316 '(a X-12))
  (cfg-rule 'X-5 '(a X-29 G-3572316))
  (cfg-rule 'S-3572315 '(a G-3572344))
  (cfg-rule 'S-3572315 '(a G-3572343))
  (cfg-rule 'S-3572315 '(a G-3572342))
  (cfg-rule 'S-3572315 '(a X-5))
  (cfg-rule 'X-1 '(a X-29 G-3572316))
  (cfg-rule 'Y-1 '(b))
  (cfg-rule 'Y-0 '(a))
  (cfg-rule 'G-3572343 '(a X-34))
  (cfg-rule 'G-3572343 '(a X-30))
  (cfg-rule 'G-3572343 '(b))
  (cfg-rule 'G-3572321 '(b))
  (cfg-rule 'S '(a G-3572344))
  (cfg-rule 'S '(a G-3572343))
  (cfg-rule 'S '(a G-3572342))
  (cfg-rule 'S '(a X-1))
  (cfg-rule 'S '(ε)))
 'S) |#


(define p (make-ndpda '(S)
                      '(a)
                      '()
                      'S
                      '(S)
                      '(((S a ε) (S ε)))))

(sm-graph p)
(sm-graph (cfg2pda (pda2cfg p)))
(sm-graph (cfg2pda (greibach (pda2cfg p))))
(greibach (pda2cfg p))
 
(pda2cfg p)
(sm-graph (cfg2pda (pda2cfg p)))
(sm-showtransitions (cfg2pda (pda2cfg p)) '(a))

(sm-graph (cfg2pda (greibach (pda2cfg p))))
(sm-showtransitions (cfg2pda (greibach (pda2cfg p))) '(a))
(sm-graph (pda2spda p))
(sm-graph (cfg2pda (pda2cfg (pda2spda p))))
(sm-showtransitions (cfg2pda (pda2cfg (pda2spda p))) '(a))


(define p2 (make-ndpda '(X S F)
                       '(a)
                       '(a Z)
                       'X
                       '(F)
                       `(((X ,EMP ,EMP) (S (Z)))
                         ((S a (Z)) (S (Z)))
                         ((S ,EMP (Z)) (F ,EMP)))))
(sm-graph p2)

(define p3 (make-ndpda '(X S F)
                       '(a)
                       '(a Z)
                       'X
                       '(F)
                       `(((X ,EMP ,EMP) (S (Z)))
                         ((S a (a)) (S ,EMP))
                         ((S ,EMP (Z)) (S (a Z)))
                         ((S ,EMP (Z)) (F ,EMP)))))
(sm-graph p3)

(sm-showtransitions p2 '(a))
(sm-showtransitions p3 '(a))


(sm-graph a2nb2n2)
(sm-graph (pda2spda a2nb2n2))

(define p4 (make-ndpda '(S P Q F A B C D E G H I J K L M N O R)
                       '(a b c)
                       '(a b c S Z)
                       'S
                       '(F)
                       '(((S ε ε) (P (Z)))

                       
                         ((P ε (c)) (A (c c)))
                         ((A ε (c)) (B (c S)))
                         ((B ε (c)) (Q (c)))
                         
                         ((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (c)))
                         
                         ((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (c)))
                         
                         ((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (c)))
                         
                         ((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (c)))


                         ((Q b (b)) (Q ε))
                         ((Q a (a)) (Q ε))

                         ((Q ε (Z)) (F ε))

                         ((Q ε (c)) (L ε))
                         ((L ε (S)) (Q ε))

                         ((Q ε (c)) (M ε))
                         ((M ε (S)) (N (S b)))
                         ((N ε (S)) (O (S S)))
                         ((O ε (S)) (R (S c)))
                         ((R ε (S)) (Q (a)))
                         
                         )))
                       
(sm-graph p4)

(sm-showtransitions (pda2spda a2nb2n2) '(a b))
(sm-showtransitions p4 '(a b))

#;(define p5 (make-ndpda '(S P Q F A B C D E G H I J K L M N O R)
                       '(a b c)
                       '(a b c S Z)
                       'S
                       '(F)
                       '(((S ε ε) (P (Z)))

                       
                         ((P ε (c)) (A (c c)))
                         ((A ε (c)) (B (c S)))
                         ((B ε (c)) (Q (c)))
                         
                         ((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (c)))
                         
                         ((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (c)))
                         
                         ((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (c)))
                         
                         ((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (c)))


                         ((Q b (b)) (Q ε))
                         ((Q a (a)) (Q ε))

                         ((Q ε (Z)) (F ε))

                         ((Q ε (c)) (L ε))
                         ((L ε (S)) (Q ε))

                         ((Q ε (c)) (M ε))
                         ((M ε (S)) (N (S b)))
                         ((N ε (S)) (O (S S)))
                         ((O ε (S)) (R (S c)))
                         ((R ε (S)) (Q (a)))
                         
                         )))




(define p5 (make-ndpda '(S P Q F A B C D E G H I J K L M N O R)
                       '(a b c)
                       '(a b c S Z)
                       'S
                       '(F)
                       '(((S ε ε) (P (Z)))

                       
                         ((P ε (c)) (A (c c)))
                         ((A ε (c)) (B (c S)))
                         ((B ε (c)) (Q (c)))
                         
                         ((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (c)))
                         
                         ((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (c)))
                         
                         ((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (c)))
                         
                         ((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (c)))  


                         ((Q b (b)) (Q ε))
                         ((Q a (a)) (Q ε))

                         ((Q ε (Z)) (F ε))

                         ((Q ε (c)) (L ε))
                         ((L ε (S)) (Q ε))

                         ((Q ε (c)) (M ε))
                         ((M ε (S)) (N (S b)))
                         ((N ε (S)) (O (S S)))
                         ((O ε (S)) (R (S c)))
                         ((R ε (S)) (Q (a)))
                         
                         )))

(sm-showtransitions p5 '(a b))

(sm-graph anbn)
(sm-graph (cfg2pda (pda2cfg anbn)))
(sm-graph (cfg2pda (greibach (pda2cfg anbn))))
(sm-graph (pda2spda anbn))

(define anbn2 (make-ndpda '(S Q)
                          '(a b)
                          '(a b Z A)
                          'S
                          '(Q)
                          '(((S ε ε) (Q (Z)))
                            ((Q ε (Z)) (Q ε))
                            ((Q a (a)) (Q ε))
                            ((Q b (b)) (Q ε))
                            ((Q ε (A)) (Q (b)))
                            ((Q ε (Z)) (Q (a Z A)))
                            ((Q ε (Z)) (Q (a A))))))

(sm-graph anbn)
(sm-graph anbn2)
(sm-showtransitions anbn2 '(a b))


;; L = {w | w in (a b)* AND  w has more b than a}
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(define numb>numa-pda (cfg2pda numb>numa))

(define inf-pda (make-ndpda '(S Q)
                            '(a b)
                            '(a b Z A)
                            'S
                            '(Q)
                            '(((S ε ε) (Q (Z)))
                              ((Q ε (Z)) (Q ε))
                              ((Q a (a)) (Q ε))
                              ((Q b (b)) (Q ε))

                              ((Q ε (Z)) (Q (b)))

                             ((Q ε (Z)) (Q (b A)))
                             ((Q ε (Z)) (Q (b A b A)))
                             
                             ((Q ε (Z)) (Q (b A a A b A)))
                             ((Q ε (Z)) (Q (a A b A b A)))

                             ((Q ε (Z)) (Q (b A b A a A b A)))
                             ((Q ε (Z)) (Q (b A a A b A b A)))


                             ((Q ε (A)) (Q ε))
                             ((Q ε (A)) (Q (b)))

                             ((Q ε (A)) (Q (b A)))
                             ((Q ε (A)) (Q (b A b A)))
                             
                             ((Q ε (A)) (Q (b A a A b A)))
                             ((Q ε (A)) (Q (a A b A b A)))

                             ((Q ε (A)) (Q (b A b A a A b A)))
                             ((Q ε (A)) (Q (b A a A b A b A)))
                             


                         #|     b A
                              A b A a A
                              A a A b A
                              b A a A
                              a A b A
                              a A b A a A
                              b A a A b A |#
                              
    
                           

                              
                           #|   ((Q ε (Z)) (Q (b Z)))
                              ((Q ε (Z)) (Q (b a Z)))
                              ((Q ε (Z)) (Q (a b Z))) |#
                              
                          #|    ((Q ε (Z)) (Q (b A)))
                              ((Q ε (Z)) (Q (a Z A)))
                              ((Q ε (Z)) (Q (a A Z)))
                              ((Q ε (A)) (Q (b A)))
                              ((Q ε (A)) (Q (b Z))) |#
                              
                              )))

(sm-graph numb>numa-pda)
(sm-graph inf-pda)
(sm-apply inf-pda '(b b a))




#;(define p5 (make-ndpda '(S P Q F A B C D E G H I J K L M N O R)
                       '(a b c)
                       '(a b c S Z)
                       'S
                       '(F)
                       '(((S ε ε) (P (Z)))

                       
                         ((P ε (c)) (A (c c)))
                         ((A ε (c)) (B (c S)))
                         ((B ε (c)) (Q (c)))
                         
                         ((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (c)))
                         
                         ((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (c)))
                         
                         ((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (c)))
                         
                         ((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (c)))


                         ((Q b (b)) (Q ε))
                         ((Q a (a)) (Q ε))

                         ((Q ε (Z)) (F ε))

                         ((Q ε (c)) (L ε))
                         ((L ε (S)) (Q ε))

                         ((Q ε (c)) (M ε))
                         ((M ε (S)) (N (S b)))
                         ((N ε (S)) (O (S S)))
                         ((O ε (S)) (R (S c)))
                         ((R ε (S)) (Q (a)))
                         
                         )))




(define p52 (make-ndpda '(S P Q F L M)
                       '(a b)
                       '(a b C S Z)
                       'S
                       '(F)
                       '(((S ε ε) (P (Z)))

                       
                         #|((P ε (c)) (A (c c)))
                         ((A ε (c)) (B (c S)))
                         ((B ε (c)) (Q (c)))|#
                         
                         #|((P ε (b)) (C (b b)))
                         ((C ε (b)) (D (b S)))
                         ((D ε (b)) (Q (c)))|#
                         
                         #|((P ε (a)) (E (a a)))
                         ((E ε (a)) (G (a S)))
                         ((G ε (a)) (Q (c)))|#
                         ;((P ε (a)) (Q (a a)))
                         
                         #|((P ε (S)) (H (S S)))
                         ((H ε (S)) (I (S S)))
                         ((I ε (S)) (Q (c)))|#
                         ;((P ε (S)) (Q (c S S)))
                         
                         #|((P ε (Z)) (J (Z Z)))
                         ((J ε (Z)) (K (Z S)))
                         ((K ε (Z)) (Q (c)))|#  
                         ;((P ε (Z)) (Q (c S Z)))

                         ((Q b (b)) (Q ε))
                         ((Q a (a)) (Q ε))

                         ((Q ε (Z)) (F ε))

                         ((Q ε (C)) (L ε))
                         ((L ε (S)) (Q ε))
                         
                         ((P ε (Z)) (Q (a C S b Z)))
                         ((Q ε (C)) (M ε))
                         ((M ε (S)) (Q (a C S b)))

                         ;((Q ε (c)) (M ε))
                         ;((M ε (S)) (N (S b)))
                         ;((N ε (S)) (O (S S)))
                         ;((O ε (S)) (R (S c)))
                         ;((R ε (S)) (Q (a)))
                         
                         )))

(sm-graph p5)
(sm-graph p52)
(sm-showtransitions p52 '(a a b b))
;; a^n b^n


;; inf one
(sm-graph numb>numa-pda)
(sm-graph (pda2spda numb>numa-pda))


(define nanb (make-ndpda '(S F Q R)
                         '(a b)
                         '(a b S Z A)
                         'S
                         '(F)
                         '(((S ε ε) (R (Z)))
                           ((Q ε (Z)) (F ε))
                           ((Q a (a)) (Q ε))
                           ((Q b (b)) (Q ε))
                           ((Q ε (A)) (Q (b A)))
                           ((Q ε (A)) (Q ε))
                           ((Q ε (S)) (Q (b)))
                                          
                           ((R ε (Z)) (Q (b Z)))
                           
                           ;((S ε (Z)) (Q (A b A Z)))
                           ((R ε (Z)) (Q (b A b A Z)))
                           ((R ε (Z)) (Q (b A Z)))
                           
                           ((R ε (Z)) (Q (a A b A b A Z)))
                           ((R ε (Z)) (Q (b A a A b A Z)))

                           ((Q ε (A)) (Q (a A b A)))
                           ((Q ε (A)) (Q (b A a A))))))
                           



(sm-graph nanb)
(sm-showtransitions nanb '(b a b b))

(sm-showtransitions nanb '(b a))
(sm-showtransitions nanb '(a a a a))
;(sm-showtransitions numb>numa-pda '(a a a a))
;(sm-showtransitions (pda2spda numb>numa-pda) '(a a a a))                         


                           




