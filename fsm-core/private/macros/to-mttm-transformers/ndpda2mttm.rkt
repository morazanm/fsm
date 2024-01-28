#lang fsm

(require "mtape-tm.rkt" "pda2simple-pda.rkt")


;; tape 0: input word, such that `(,BLANK) if input word is empty
;; tape 1: stack, such that (top-most..bottomost) and empty stack is `(,BLANK)

#|
A pda is simple if all transition rules have the following
structure:

  ((Q a β) (P θ)), such that Q != S, β ∈Γ, ∧ |θ|≤2

1. Always pops the topmost stack element

2. Pushes zero, one, or two elements onto the stack

3. No restrictions on pda's start state,because whenever a
   pda starts the stack is empty

4. Only one rule from the pda's start state that reads and pops nothing and
   that pushes the bottom of the stack symbol

5. Only one rule into final state that reads and pushes nothing and that
   pops the bottom of the stack symbol

|#

;; ndpda --> mttm
;; Purpose: Convert given pda to a mttm
(define (ndpda->mttm M)
  (define SM (pda2spda M))

  (let* [(sts (sm-states SM))
         (sigma (sm-sigma SM))
         (gamma (sm-gamma SM))
         (start (sm-start SM))
         (finals (sm-finals SM))
         (rules (sm-rules SM))
         (acc-st (generate-symbol 'Y sts))
         (rej-st (generate-symbol 'N sts))
         (start-mt (generate-symbol 'S sts))
         (mt-finals (list acc-st rej-st))
         (mt-sts (append (list start-mt) sts mt-finals))]
    (make-mttm mt-sts
               sigma
               start
               mt-finals
               (append
                ;; The transformation of the only pda start state rule. Anything
                ;; may be read, nothing is popped, and only bottom of the stack
                ;; symbol is pushed (i.e., written to T1).
                (let [(S-rule (first (filter (λ (r) (eq? (caar r) start)) rules)))]
                  (for/list [(a (cons BLANK sigma))]
                    (list (list (caar S-rule) (list a BLANK))
                          (list (caadr S-rule) (list a (car (cadadr S-rule)))))))
                ;; The transformation of the only pda final state rule. The mttm
                ;; must read a blank (i.e., pda read everything) and bottom of
                ;; the stack symbol is popped (overwrittem with blank to leave
                ;; nothing on T1).
                (let [(F-rule (first (filter (λ (r) (eq? (caadr r) (first finals)))
                                             rules)))]
                  (list (list (list (caar F-rule) (list BLANK (car (caddar F-rule))))
                              (list (caadr F-rule) (list BLANK BLANK)))
                        (list (list (first finals) (list BLANK BLANK))
                              (list acc-st (list BLANK BLANK)))))
                ;; The transformation of all other rules.
                (let [(other-rules (filter (λ (r) (and (not (eq? (caar r) start))
                                                       (not (eq? (caadr r) (first finals)))))
                                           rules))]
                  (append-map
                   (λ (r)
                     (let [(fromst (caar r))
                           (read (cadar r))
                           (pop (caddar r))
                           (tost (caadr r))
                           (push (cadadr r))]
                       (cond [(eq? push EMP)
                              (if (not (eq? read EMP))
                                  (let* [(ist (generate-symbol 'I '(I)))]
                                    (list (list (list fromst (list read (first pop)))
                                                      (list ist (list read BLANK)))
                                          (list (list ist (list read BLANK))
                                                (list tost (list RIGHT LEFT)))))
                                  (let [(ist (generate-symbol 'I '(I)))]
                                    (append-map
                                     (λ (a);; do not move right on T0 if pda reads nothing
                                       (list (list (list fromst (list a (first pop)))
                                                   (list ist (list a BLANK)))
                                             (list (list ist (list a BLANK))
                                                   (list tost (list a LEFT)))))
                                     (cons BLANK sigma))))]
                             [(eq? (length push) 1)
                              (if (not (eq? read EMP))
                                  (list (list fromst (list read (first pop)))
                                        (list tost (list RIGHT (first push))))
                                  (map
                                   (λ (a) ;; do not move right on T0 if pda reads nothing
                                     (list (list fromst (list a (first pop)))
                                           (list tost (list a (first push)))))
                                   (cons BLANK sigma)))]
                             [(not (eq? read EMP)) ;; pda pushes two elements and reads something
                              (let [(v1 (first push))
                                    (v2 (first (rest push)))
                                    (ist1 (generate-symbol 'I '(I)))
                                    (ist2 (generate-symbol 'I '(I)))]
                                (list (list (list fromst (list read (first pop)))
                                            (list ist1 (list read v2)))
                                      (list (list ist1 (list read v2))
                                            (list ist2 (list read RIGHT)))
                                      (list (list ist2 (list read BLANK))
                                            (list tost (list RIGHT v1)))))]
                             [else ;; pda pushes two elements and reads nothing                                
                              (let [(v1 (first push))
                                    (v2 (first (rest push)))
                                    (ist1 (generate-symbol 'I '(I)))
                                    (ist2 (generate-symbol 'I '(I)))]
                                (append-map
                                 (λ (a)
                                   (list (list (list fromst (list a (first pop)))
                                               (list ist1 (list a v2)))
                                         (list (list ist1 (list a v2))
                                               (list ist2 (list a RIGHT)))
                                         (list (list ist2 (list a BLANK))
                                               (list tost (list a v1)))))
                                 (cons BLANK sigma)))])))
                   other-rules)))
               2
               acc-st)))
                                

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

(check-equal? (sm-apply a2nb2n '(a a b)) 'reject)
(check-equal? (sm-apply a2nb2n '(a a b b b)) 'reject)
(check-equal? (sm-apply a2nb2n '(b a)) 'reject)
(check-equal? (sm-apply a2nb2n '()) 'accept)
(check-equal? (sm-apply a2nb2n '(a b)) 'accept)
(check-equal? (sm-apply a2nb2n '(a a b b)) 'accept)

(define a2nb2n-mt (ndpda->mttm a2nb2n))

(check-equal? (sm-apply a2nb2n-mt '(a a b)) 'reject)
(check-equal? (sm-apply a2nb2n-mt '(a a b b b)) 'reject)
(check-equal? (sm-apply a2nb2n-mt '(b a)) 'reject)
(check-equal? (sm-apply a2nb2n-mt '()) 'accept)
(check-equal? (sm-apply a2nb2n-mt '(a b)) 'accept)
(check-equal? (sm-apply a2nb2n-mt '(a a b b)) 'accept)