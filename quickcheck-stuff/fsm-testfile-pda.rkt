#lang fsm

(require racket/list
         rackunit
         "sm-test-invs-pda.rkt"
         )


(define aˆnbˆn (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))


;; L = wcwˆR
;; State Documentation
;; S: ci = a b* AND stack = ciˆR
;; F: ci = xycyˆR AND stack = xˆR
(define dwcwˆr (make-ndpda '(S F)
'(a b c)
'(a b)
'S
'(F)
`(((S a ,EMP) (S (a)))
((S b ,EMP) (S (b)))
((S c ,EMP) (F ,EMP))
((F a (a)) (F ,EMP))
((F b (b)) (F ,EMP)))))
(check-equal? (sm-apply dwcwˆr '()) 'reject)
(check-equal? (sm-apply dwcwˆr '(a b b a)) 'reject)
(check-equal? (sm-apply dwcwˆr '(a a)) 'reject)
(check-equal? (sm-apply dwcwˆr '(a b b c b b a b)) 'reject)
(check-equal? (sm-apply dwcwˆr '(a c a)) 'accept)
(check-equal? (sm-apply dwcwˆr '(b a c a b)) 'accept)
(check-equal? (sm-apply dwcwˆr '(b b a c a b b)) 'accept)

(sm-graph dwcwˆr)