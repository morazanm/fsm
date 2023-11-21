#lang fsm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = ab*
;; Name: ab*
;; Σ: '(a b)
(define ab* (make-dfa '(S F)
                      '(a b)
                      'S
                      '(F)
                      '((S a F) 
                        (F b F))))

;.................................................

;; Tests 
(check-equal? (sm-apply ab* '()) 'reject)
(check-equal? (sm-apply ab* '(b)) 'reject)
(check-equal? (sm-apply ab* '(a)) 'accept)
(check-equal? (sm-apply ab* '(a b b)) 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



