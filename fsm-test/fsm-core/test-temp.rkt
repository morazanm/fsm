#lang racket
(require   "../../fsm-core/private/fsa.rkt"
           "../../main.rkt")



(module+ test
  (require rackunit)
  
  (define dfa-a* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q a Q))))
  (define dfa-b* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q b Q))))
  (define ndfa-inter-a*-b* (sm-intersection dfa-a* dfa-b*))
  (sm-finals ndfa-inter-a*-b*)

  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a b a a b)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (b b b)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a a a)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a b)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a a a b ))'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' ()) 'accept)

  )