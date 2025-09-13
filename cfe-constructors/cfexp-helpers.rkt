#lang racket

(require "../fsm-core/private/cfg-struct.rkt"
         "../fsm-core/private/pda.rkt" ;(except-in "../fsm-core/private/pda.rkt" pda->spda)
         "../fsm-core/private/misc.rkt"
         "../fsm-core/private/constants.rkt")

(provide pda2cfg pda->spda)

(define (mk-cfg-rl a-lhs a-rhs)
  (list a-lhs ARROW a-rhs))


;;pda -> cfg
;;Purpose converts the given cfg to a pda
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
  (let* [(p (pda->spda P))
         (pstates (pda-getstates p))
         (psigma (pda-getalphabet p))
         (pgamma (pda-getgamma p))
         (pstart (pda-getstart p))
         (pfinals (pda-getfinals p))
         (prules (pda-getrules p))
         (start (generate-symbol 'S '(S)))
         (bottom (first (filter (λ (s) (not (member s (pda-getgamma P)))) pgamma)))
         (startr (mk-cfg-rl start (list (pda-getstart P) bottom (first pfinals))))
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
    (make-unchecked-cfg (cons start (map second st-tbl)) psigma new-rls start)))



; ndpda -> ndpda
  ; convert the given pda into a simple pda
;; state symbol stacke state stacke --> pda-rule
;; Purpose: Build a pda-rule
(define (mk-pda-rule from a pop to push)
  (list (list from a pop) (list to push)))

;; pda-rule --> state
;; Purpose: Extract from state
(define (get-from r) (first (first r)))

;; pda-rule --> symbol
;; Purpose: Extract read symbol
(define (get-read r) (second (first r)))

;; pda-rule --> stacke
;; Purpose: Extract pop elements
(define (get-pop r) (third (first r)))

;; pda-rule --> state
;; Purpose: Extract to state
(define (get-to r) (first (second r)))

;; pda-rule --> stacke
;; Purpose: Extract push elements
(define (get-push r) (second (second r)))

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


;; (listof pda-rule) --> (listof state)
;; Purpose: Extract the list of states in the given rules
(define (extract-states rls)
  (remove-duplicates
   (append-map (λ (r) (list (first (first r))
                            (first (second r))))
               rls)))

;; pda --> pda
;; Purpose: Convert given pda to a simple pda
(define (pda->spda p)
  (let* [(pstates (pda-getstates p))
         (psigma (pda-getalphabet p))
         (pgamma (pda-getgamma p))
         (pstart (pda-getstart p))
         (pfinals (pda-getfinals p))
         (prules (pda-getrules p))
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
    (make-unchecked-ndpda (append (list  new-final new-start)
                                  (remove-duplicates
                                   (cons pstart (extract-states theta<=2-rules))))
                        
                          psigma
                          (cons bottom pgamma)
                          new-start
                          (list new-final)
                          (cons initr (append theta<=2-rules frules)))))