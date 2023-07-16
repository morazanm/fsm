#lang fsm

(require "mtape-tm.rkt")


(define (dfa->mttm M)
  (let* [(sts (sm-states M))
         (sigma (sm-sigma M))
         (start (sm-start M))
         (finals (sm-finals M))
         (rules (sm-rules M))
         (acc-st (generate-symbol 'Y sts))
         (rej-st (generate-symbol 'N sts))
         (mt-finals (list acc-st))]
    (make-mttm sts
               sigma
               start
               (list acc-st rej-st)
               (append 
                       (map (λ (f) (list (list f (list BLANK))
                                         (list acc-st (list BLANK))))
                            finals)
                       (for*/list [(s sts)
                                   (a sigma)]
                         (list (list s (list a))
                               (list rej-st (list RIGHT))))
                       (map (λ (s) (list (list s BLANK)
                                         (list rej-st BLANK)))
                            sts)
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