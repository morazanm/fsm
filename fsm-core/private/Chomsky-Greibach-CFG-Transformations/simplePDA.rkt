#lang fsm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pda -> simple pda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple pda:
;;  Always pops 1 element
;;  Always pushed at most 2 elements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> pda
;; Purpose: Transform the given cfg into a pda
(define (cfg->pda G)
  (let [(nts (grammar-nts G))
        (sigma (grammar-sigma G))
        (start (grammar-start G))
        (rules (grammar-rules G))]
    (make-ndpda '(S Q)
                sigma
                (append nts sigma)
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

;.................................................

;; Syntactic Categories
;; S = words that start with n a and end with n b

;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S))

;; pda 
(define a2nb2n-pda (cfg->pda a2nb2n))

;.................................................

;; States
;;  S: ci is empty and stack is empty
;;  P: ci = stack^r AND c not in ci
;;  Q: ci = (append w (list 'c) v) AND w = stack^r v^r
;;  F: stack = '() AND ci = (append w (list c) w^r)

;; L = wcw^r | w in (a b)*
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

;; Tests for wcw^r
(check-equal? (sm-apply wcw^r '(a)) 'reject)
(check-equal? (sm-apply wcw^r '(a c)) 'reject)
(check-equal? (sm-apply wcw^r '(b c a)) 'reject)
(check-equal? (sm-apply wcw^r '(a a b c b a b)) 'reject)
(check-equal? (sm-apply wcw^r '(c)) 'accept)
(check-equal? (sm-apply wcw^r '(a c a)) 'accept)
(check-equal? (sm-apply wcw^r '(a b b b c b b b a)) 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A stack element, stacke, is either
;; 1. EMP
;; 2. (listof symbol)

;.................................................
;; function to make a pda rule

;; state symbol stacke state stacke -> pda-rule
;; Purpose: Build a pda-rule
(define (mk-pda-rule from r pop to push)
  (list (list from r pop) (list to push)))

;; Tests for mk-pda-rule
(define PDA-R1 (mk-pda-rule 'P 'a EMP 'Q EMP))
(define PDA-R2 (mk-pda-rule 'A 'c '(a) 'B '(b)))
(check-equal? PDA-R1 (list (list 'P 'a EMP) (list 'Q EMP)))
(check-equal? PDA-R2 (list (list 'A 'c '(a)) (list 'B '(b))))

;.................................................
;; selectors

;; pda-rule -> state
;; Purpose: Extract from state
(define (get-from r) (first (first r)))

;; Tests for get-from
(check-equal? (get-from PDA-R1) 'P)
(check-equal? (get-from PDA-R2) 'A)

;; pda-rule -> symbol
;; Purpose: Extract read symbol
(define (get-read r) (second (first r)))

;; Tests for get-read
(check-equal? (get-read PDA-R1) 'a)
(check-equal? (get-read PDA-R2) 'c)

;; pda-rule -> stack
;; Purpose: Extract pop elements
(define (get-pop r) (third (first r)))

;; Tests for get-pop
(check-equal? (get-pop PDA-R1) EMP)
(check-equal? (get-pop PDA-R2) '(a))

;; pda-rule -> state
;; Purpose: Extract to state
(define (get-to r) (first (second r)))

;; Tests for get-to
(check-equal? (get-to PDA-R1) 'Q)
(check-equal? (get-to PDA-R2) 'B)

;; pda-rule -> stacke
;; Purpose: Extract push elements
(define (get-push r) (second (second r)))

;; Tests for get-push
(check-equal? (get-push PDA-R1) EMP)
(check-equal? (get-push PDA-R2) '(b))

;.................................................
;; extract states from pda rule

;; (listof pda-rule)→(listof state)
;; Purpose: Extract states in the given rules
(define (extract-states rls)
  (remove-duplicates
   (append-map
    (λ (r) (list (first (first r)) (first (second r)))) rls)))

;; Tests for extract-states
(check-equal? (extract-states `(((P ,EMP (Z)) (Q (S c Z))))) '
              (P Q))
(check-equal? (extract-states `(((P ,EMP (Z)) (Q (S c Z)))
                                ((P a (Z)) (R (S c Z)))
                                ((Q ,EMP (Z)) (T (S c Z)))))
              '(P Q R T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof pda-rule) (listof states) -> (listof pda-rule)
;; Purpose: Replace rules that push more than 2 elements
(define (generate-theta<=2-rules rls sts)
  ;; (listof pda-rule) (listof state) -> (listof pda-rule)
  ;; Purpose: Generate rules with |theta|<=2 for given rules
  (define (gen-theta<=2-rules theta>2-rules sts)
    ;; pda-rule -> (listof pda-rule)
    ;; Purpose: Generate |theta|<=2 rules for given rule
    (define (gen-rules r)
      ;; (listof state) (listof symbol) (listof symbol) symbol -> (listof pda-rule)
      ;; Purpose: Generate |theta|<=2 rules for given push and state lists
      (define (process-sts sts push pop read)
        (if (= (length sts) 2)
            (list (mk-pda-rule (first sts) read pop (second sts) push))
            (cons (mk-pda-rule (first sts) EMP pop (second sts)
                               (append pop (list (first push))))
                  (process-sts (rest sts) (rest push) pop read))))
      (let* ((from (get-from r)) (read (get-read r))
                                 (pop (get-pop r))
                                 (to (get-to r))
                                 (push (get-push r))
                                 (new-states (build-list
                                              (sub1 (length push))
                                              (λ (i) (generate-symbol 'T (cons 'T sts)))))
                                 (rev-push (reverse push)))
        (cons (mk-pda-rule from EMP pop (first new-states)
                           (append pop (list (first rev-push))))
              (process-sts (append new-states (list to))
                           (rest rev-push)
                           pop
                           read))))
    (append-map gen-rules theta>2-rules))
  (let* ((theta>2-rules (filter
                         (λ (r) (and (not (eq? (second (second r)) EMP))
                                     (> (length (second (second r))) 2)))
                         rls))
         (theta<=2-rules (filter (λ (r) (not (member r theta>2-rules)))
                                 rls)))
    (append theta<=2-rules (gen-theta<=2-rules theta>2-rules sts))))

;................................................

;; (listof pda-rule) -> Boolean
;; Purpose: Determine all rules have |theta|<=2
(define (all-theta<=2 rls)
  (andmap (λ (r) (<= (length (second (second r))) 2)) rls))

;; Tests
(check-pred all-theta<=2
            (generate-theta<=2-rules '(((P ,EMP (Z)) (Q (S c Z)))) '(Q P)))
(check-pred all-theta<=2
            (generate-theta<=2-rules '(((P ,EMP (Z)) (Q (S c Z)))
                                       ((A a (Z)) (B (A b B)))) '(Q P)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof pda-rule) (listof symbols) -> (listof pda-rules)
;; Purpose: Substitute pop nothing rules with pop 1 rules
(define (generate-beta=1-rules rls gamma)
  (let* ((beta=0-rls (filter (λ (r) (eq? (get-pop r) EMP)) rls))
         (beta>0-rls (filter (λ (r) (not (member r beta=0-rls))) rls)))
    (append beta>0-rls
            (for*/list ([r beta=0-rls]
                        [g gamma])
              (list (list (get-from r) (get-read r) (list g))
                    (list (get-to r)
                          (if (eq? (get-push r) EMP)
                              (list g)
                              (append (get-push r) (list g)))))))))

;................................................                       

;; Tests for generate-beta=1-rules
(check-equal? (generate-beta=1-rules `(((P a ,EMP) (Q (a b)))) '(S a b))
              '(((P a (S)) (Q (a b S)))
                ((P a (a)) (Q (a b a)))
                ((P a (b)) (Q (a b b)))))
(check-equal? (generate-beta=1-rules `(((P a ,EMP) (Q (a b)))
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
                ((P c (b)) (Q (b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (listof pda-rule) (listof state) -> (listof pda-rule)
;; Purpose: Eliminate rules that pop more than two elements
(define (generate-beta<2-rules rules states)
  ;; pda-rule (listof state) -> (listof pda-rule)
  ;; Purpose: Create |beta| = 1 rules for given rule
  (define (convert-beta=1 r states)
    ;; (listof symbol) (listof state) -> (listof pda-rule)
    ;; Purpose: Generate pda rules for given pop list using given states
    (define (gen-intermediate-rules beta sts)
      (if (empty? (rest sts))
          '()
          (cons (mk-pda-rule (first sts) EMP (list (first beta))
                             (first (rest sts)) EMP)
                (gen-intermediate-rules (rest beta) (rest sts)))))
    (let* ((from (get-from r))
           (read (get-read r))
           (to (get-to r))
           (beta (get-pop r))
           (push (get-push r))
           (new-states (build-list
                        (sub1 (length beta))
                        (λ (i) (generate-symbol 'B (cons 'B states))))))
      (append (list
               (mk-pda-rule from EMP (list (first beta)) (first new-states) EMP)
               (mk-pda-rule (last new-states) read (list (last beta)) to push))
              (gen-intermediate-rules (rest beta) new-states))))
  (let* ((beta>=2-rules (filter (λ (r) (and (not (eq? (get-pop r) EMP)) (>= (length (get-pop r)) 2)))
                                rules))
         (beta<2-rules (filter (λ (r) (not (member r beta>=2-rules))) rules)))
    (append beta<2-rules (append-map (λ (r) (convert-beta=1 r states)) beta>=2-rules))))

;................................................                       

;; (listof pda-rule) -> Boolean
;; Purpose: Determine if at most 1 element is popped by every rule
(define (all-beta<2 L)
  (andmap (λ (r) (or (eq? (get-pop r) EMP) (= (length (get-pop r)) 1))) L))

;; Tests for generate-beta<1-rules
(check-pred all-beta<2
            (generate-beta<2-rules (list (list (list 'Q 'a '(a b c)) (list 'R '(z))))
                                   '(Q R)))
(check-pred all-beta<2
            (generate-beta<2-rules (list (list (list 'Q 'a '(a b c)) (list 'R '(z)))
                                         (list (list 'Q EMP EMP) (list 'R EMP))
                                         (list (list 'Q 'b '(i j k l)) (list 'R EMP)))
                                   '(Q R)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pda -> pda
;; Purpose: Convert given pda to a simple pda
(define (pda->spda p)
  (let* ((pstates (sm-states p))
         (psigma (sm-sigma p))
         (pgamma (sm-gamma p))
         (pstart  (sm-start p))         
         (pfinals (sm-finals p))
         (prules (sm-rules p))

         (new-start (generate-symbol 'S pstates))
         (new-final (generate-symbol 'F pstates))

         (bottom (generate-symbol 'Z pgamma))
         (initr (mk-pda-rule new-start EMP EMP pstart (list bottom)))
         (finalr (map (lambda (s) (mk-pda-rule s EMP (list bottom) new-final EMP)) pfinals))

         (beta<2-rules (generate-beta<2-rules prules pstates))
         (beta=1-rules (generate-beta=1-rules beta<2-rules (cons bottom pgamma)))
         (theta<=2-rules (generate-theta<=2-rules beta=1-rules (extract-states beta=1-rules))))

    (make-ndpda (append (list new-final new-start)
                        (remove-duplicates
                         (cons pstart (extract-states theta<=2-rules))))
                psigma
                (cons bottom pgamma)
                new-start
                (list new-final)
                (cons initr (append theta<=2-rules
                                    finalr)))))

;.................................................

;; Tests for pda2spda
(define P1 (pda->spda a2nb2n-pda))
(define P2 (pda->spda wcw^r))
(check-equal? (sm-testequiv? a2nb2n-pda P1) #t)
(check-equal? (sm-testequiv? a2nb2n-pda P1) #t)
(check-equal? (sm-testequiv? wcw^r P2)  #t)
(check-equal? (sm-testequiv? wcw^r P2)  #t)

(sm-graph (cfg->pda a2nb2n))
(sm-graph P1)
(sm-graph wcw^r)
(sm-graph P2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









































