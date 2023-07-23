#lang fsm

(require "mtape-tm.rkt")

;; dfa --> mttm
;; Purpose: Convert given dfa into an mttm
(define (dfa->mttm M)
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
               (append ;; if start state is a final state then on BLANK
                       ;; move to accept. Otherwise, move to reject.
                       ;; This properly processes an '() given to a dfa.
                       (if (member start finals)
                           (list (list (list start `(,BLANK))
                                       (list acc-st `(,BLANK))))
                           (list (list (list start `(,BLANK))
                                       (list rej-st `(,BLANK)))))
                       ;; From every dfa final state move to the mttm's
                       ;; final state on a blank. Reading a blank in a
                       ;; dfa final state means that the dfa has read
                       ;; accepts and so should the mttm.
                       (map (λ (f) (list (list f (list BLANK))
                                         (list acc-st (list BLANK))))
                            finals)
                       ;; These rules have the mttm reject when none of the
                       ;; dfa's rules apply. If a (s a X) rule exists in the
                       ;; dfa then the mttm will be nondeterministic.
                       (for*/list [(s sts)
                                   (a sigma)]
                         (list (list s (list a))
                               (list rej-st (list RIGHT))))
                       ;; If the mttm reads a blank in a dfa non-final
                       ;; state then the dfa has read all the input and
                       ;; rejects. These rules have the mttm also reject.
                       (map (λ (s) (list (list s (list BLANK))
                                         (list rej-st (list BLANK))))
                            (filter (λ (s) (not (member s finals))) sts))
                       ;; These are all the dfa's rules transformed into
                       ;; mttm rules.
                       (map (λ (r) (list (list (first r) (list (second r)))
                                         (list (third r) (list RIGHT))))
                            rules))
               1
               acc-st)))

;; L(M) = ab*
(define M (make-dfa `(C S F ,DEAD)
                    '(a b)
                    'S
                    '(F)
                    `((S a F)
                      (S b ,DEAD)
                      (F a ,DEAD)
                      (F b F)
                      (C a ,DEAD)
                      (,DEAD a ,DEAD)
                      (,DEAD b ,DEAD))
                    'no-dead))

(check-equal? (sm-apply M '()) 'reject)
(check-equal? (sm-apply M '(b b b)) 'reject)
(check-equal? (sm-apply M '(a b b b a b)) 'reject)
(check-equal? (sm-apply M '(a b)) 'accept)
(check-equal? (sm-apply M '(a b b b b)) 'accept)

(define M-mt (dfa->mttm M))

(check-equal? (sm-apply M-mt '()) 'reject)
(check-equal? (sm-apply M-mt '(b b b)) 'reject)
(check-equal? (sm-apply M-mt '(a b b b a b)) 'reject)
(check-equal? (sm-apply M-mt '(a b)) 'accept)
(check-equal? (sm-apply M-mt '(a b b b b)) 'accept)