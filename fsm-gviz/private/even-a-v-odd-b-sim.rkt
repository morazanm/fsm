#lang fsm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = even a* v odd b*
;; Name: even-a-v-odd-b
;; Σ: '(a b)
(define even-a-v-odd-b (make-ndfa '(S A B C D)
                                  '(a b)
                                  'S
                                  '(A D)
                                  '((S ε A)
                                    (S ε C)
                                    (A a B)
                                    (B a A)
                                    (C b D)
                                    (D b C))))

;.................................................

;; Tests 
(check-equal? (sm-apply even-a-v-odd-b '(b b)) 'reject)
(check-equal? (sm-apply even-a-v-odd-b '(a)) 'reject)
(check-equal? (sm-apply even-a-v-odd-b '()) 'accept)
(check-equal? (sm-apply even-a-v-odd-b '(b)) 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;