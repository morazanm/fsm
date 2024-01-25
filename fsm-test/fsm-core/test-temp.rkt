#lang racket
(require   "../../fsm-core/private/fsa.rkt"
           "../../main.rkt" rackunit)



;(module+ test
;  (require rackunit)

  ; L = a*
  (define dfa-a* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q a Q))))

  (check-equal? (sm-apply dfa-a* ' (a b a a b)) 'reject)
  (check-equal? (sm-apply dfa-a* '(b b a b )) 'reject)
  (check-equal? (sm-apply dfa-a* '(b b b b b )) 'reject)
  (check-equal? (sm-apply dfa-a* '(b a a a a a)) 'reject)
  (check-equal? (sm-apply dfa-a* '(a a a a a a)) 'accept)
  (check-equal? (sm-apply dfa-a* '( a )) 'accept)
  (check-equal? (sm-apply dfa-a* '()) 'accept)

  ; L = b*
  (define dfa-b* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q b Q))))

  (check-equal? (sm-apply dfa-b* '(a a a a a a)) 'reject)
  (check-equal? (sm-apply dfa-b* ' (a b a a b)) 'reject)
  (check-equal? (sm-apply dfa-b* '(b b a b )) 'reject)
  (check-equal? (sm-apply dfa-b* '(b b b b b)) 'accept)
  (check-equal? (sm-apply dfa-b* '(b)) 'accept)
  (check-equal? (sm-apply dfa-b* '()) 'accept)

  (define dfa-renamests-a* (sm-rename-states (sm-states dfa-a*) dfa-a*)) 

  (check-equal? (sm-apply dfa-renamests-a* ' (a b a a b)) 'reject)
  (check-equal? (sm-apply dfa-renamests-a* '(b b a b )) 'reject)
  (check-equal? (sm-apply dfa-renamests-a* '(b b b b b )) 'reject)
  (check-equal? (sm-apply dfa-renamests-a* '(b a a a a a)) 'reject)
  (check-equal? (sm-apply dfa-renamests-a* '(a a a a a a)) 'accept)
  (check-equal? (sm-apply dfa-renamests-a* '( a )) 'accept)
  (check-equal? (sm-apply dfa-renamests-a* '()) 'accept)

  (define dfa-renamests-b* (sm-rename-states (sm-states dfa-b*) dfa-b*))  

  (check-equal? (sm-apply dfa-renamests-b* '(a a a a a a)) 'reject)
  (check-equal? (sm-apply dfa-renamests-b* ' (a b a a b)) 'reject)
  (check-equal? (sm-apply dfa-renamests-b* '(b b a b )) 'reject)
  (check-equal? (sm-apply dfa-renamests-b* '(b b b b b)) 'accept)
  (check-equal? (sm-apply dfa-renamests-b* '(b)) 'accept)
  (check-equal? (sm-apply dfa-renamests-b* '()) 'accept)

  ; L = a* U b*
  (define dfa-a*Unionb* (sm-union dfa-a* dfa-b*))

  (check-equal? (sm-apply dfa-a*Unionb* '(b a)) 'reject)
  (check-equal? (sm-apply dfa-a*Unionb* '(a a b a)) 'reject)
  (check-equal? (sm-apply dfa-a*Unionb* '(b b a b)) 'reject)
  (check-equal? (sm-apply dfa-a*Unionb* '()) 'accept)
  (check-equal? (sm-apply dfa-a*Unionb* '( a )) 'accept)
  (check-equal? (sm-apply dfa-a*Unionb* '( b )) 'accept)
  (check-equal? (sm-apply dfa-a*Unionb* '(a a a a )) 'accept)
  (check-equal? (sm-apply dfa-a*Unionb* '(b b b b b)) 'accept)
  (check-equal? (sm-apply dfa-a*Unionb* '(a a a b b b b)) 'reject)

  (define dfa-showtransitions-a*Unionb* (sm-showtransitions dfa-a*Unionb* '(a a a a))) 

  (define dfa-renamests-a*Unionb* (sm-rename-states (sm-states dfa-a*Unionb*)dfa-a*Unionb*))

  (check-equal? (sm-apply dfa-renamests-a*Unionb* '(b a)) 'reject)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '(a a b a)) 'reject)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '(b b a b)) 'reject)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '()) 'accept)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '( a )) 'accept)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '( b )) 'accept)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '(a a a a )) 'accept)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '(b b b b b)) 'accept)
  (check-equal? (sm-apply dfa-renamests-a*Unionb* '(a a a b b b b)) 'reject)
  
  
  (define ndfa-inter-a*-b* (sm-intersection dfa-a* dfa-b*))
  (sm-finals ndfa-inter-a*-b*)
  (sm-graph ndfa-inter-a*-b*)

  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a b a a b)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (b b b)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a a a)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a b)) 'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' (a a a b ))'reject)
  (check-equal? (sm-apply ndfa-inter-a*-b* ' ()) 'accept)

 ; )