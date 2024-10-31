#lang fsm

(require "mtape-tm.rkt")

;; ndfa --> mttm
;; Purpose: Convert given ndfa into an mttm
(define (ndfa->mttm M)
  (let* [(sts (sm-states M))
         (sigma (sm-sigma M))
         (start (sm-start M))
         (finals (sm-finals M))
         (rules (sm-rules M))
         (acc-st (generate-symbol 'Y sts))
         (rej-st (generate-symbol 'N sts))
         (mt-finals (list acc-st rej-st))
         (mt-sts (append sts mt-finals))]
    (make-mttm mt-sts
               sigma
               start
               (list acc-st rej-st)
               (remove-duplicates  ;; needed, for example, if ndfa only has e-trans out of its start state
                (append
                 ;; if start state is a final state then on BLANK
                 ;; move to accept. Otherwise, move to reject.
                 ;; This properly processes an '() given to a ndfa.
                 (if (member start finals)
                     (list (list (list start `(,BLANK))
                                 (list acc-st `(,BLANK))))
                     (list (list (list start `(,BLANK))
                                 (list rej-st `(,BLANK)))))
                 ;; From every ndfa final state move to the mttm's
                 ;; final state on a blank. Reading a blank in a
                 ;; ndfa final state means that the ndfa has read
                 ;; accepts and so should the mttm.
                 (map (λ (f) (list (list f (list BLANK))
                                   (list acc-st (list BLANK))))
                      finals)
                 ;; These rules have the mttm reject when none of the
                 ;; ndfa's rules apply. 
                 (for*/list [(s sts)
                             (a sigma)]
                   (list (list s (list a))
                         (list rej-st (list RIGHT))))
                 ;; If the mttm reads a blank in a ndfa non-final
                 ;; state then the dfa has read all the input and
                 ;; rejects. These rules have the mttm also reject.
                 (map (λ (s) (list (list s (list BLANK))
                                   (list rej-st (list BLANK))))
                      (filter (λ (s) (not (member s finals))) sts))
                 ;; These are all the ndfa's rules transformed into
                 ;; mttm rules. For ndfa e-transitions, the mttm
                 ;; does not move the head right, does not mutate the
                 ;; the tape, and only changes state (no matter what
                 ;; is read on tape 0). Otherwise, it moves
                 ;; the head right on tape 0.
                 (let* [(e-trans (filter (λ (r) (eq? (second r) EMP)) rules))
                        (non-e-trans (filter (λ (r) (not (eq? (second r) EMP))) rules))]
                   (append 
                    (map (λ (r) (list (list (first r) (list (second r)))
                                      (list (third r) (list RIGHT))))
                         non-e-trans)
                    (for*/list [(e-rule e-trans)
                                (a (cons BLANK sigma))] ;; need BLANK for e-trans out of the ndfa's start state
                      (list (list (first e-rule) (list a))
                            (list (third e-rule) (list a))))))))
               1
               acc-st)))

;; L = {w | a not in w OR b not in w OR c not in w}
(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C)
                                        '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))

;; Tests for AT-LEAST-ONE-MISSING
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(a b c)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b b a b c b a)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b a c)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '()) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(a)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(c)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(c c a a)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(b b c b b b)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING '(a a a b b b)) 'accept)

(define AT-LEAST-ONE-MISSING-mt (ndfa->mttm AT-LEAST-ONE-MISSING))

;; Tests for AT-LEAST-ONE-MISSING-mt
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(a b c)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(b b a b c b a)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(b a c)) 'reject)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '()) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(a)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(b)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(c)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(c c a a)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(b b c b b b)) 'accept)
(check-equal? (sm-apply AT-LEAST-ONE-MISSING-mt '(a a a b b b)) 'accept)


;; L = {ε} U aa* U ab*
(define LNDFA (make-ndfa '(S A B F)
                         '(a b)
                         'S
                         '(A B F)
                         `((S a A)
                           (S a B)
                           (S ,EMP F)
                           (A b A)
                           (B a B))))

;; Tests for LNDFA
(check-equal? (sm-apply LNDFA '(a b a)) 'reject)
(check-equal? (sm-apply LNDFA '(b b b b b)) 'reject)
(check-equal? (sm-apply LNDFA '(a b b b b a a a)) 'reject)
(check-equal? (sm-apply LNDFA '()) 'accept)
(check-equal? (sm-apply LNDFA '(a)) 'accept)
(check-equal? (sm-apply LNDFA '(a a a a)) 'accept)
(check-equal? (sm-apply LNDFA '(a b b)) 'accept)

(define LNDFA-mt (ndfa->mttm LNDFA))

;; Tests for LNDFA-mt
(check-equal? (sm-apply LNDFA-mt '(a b a)) 'reject)
(check-equal? (sm-apply LNDFA-mt '(b b b b b)) 'reject)
(check-equal? (sm-apply LNDFA-mt '(a b b b b a a a)) 'reject)
(check-equal? (sm-apply LNDFA-mt '()) 'accept)
(check-equal? (sm-apply LNDFA-mt '(a)) 'accept)
(check-equal? (sm-apply LNDFA-mt '(a a a a)) 'accept)
(check-equal? (sm-apply LNDFA-mt '(a b b)) 'accept)


