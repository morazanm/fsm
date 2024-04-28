#lang fsm
(provide pda2cfg pda2spda)

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

(define D (pda2spda (grammar->sm numb>numa)))








