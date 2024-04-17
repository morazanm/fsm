#lang fsm

(require "pda2cfg-v2-simple-pda.rkt")
(provide cfg2pda)

;; Sample cfg

;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S ,ARROW ,EMP)
                           (S ,ARROW aSb))
                         'S))

;; Tests for a2nb2n
(check-equal? (grammar-derive a2nb2n '(b b b))
              "(b b b) is not in L(G).")
(check-equal? (grammar-derive a2nb2n '(a b a))
              "(a b a) is not in L(G).")
(check-equal? (grammar-derive a2nb2n '(a b))
              '(S -> aSb -> ab))
(check-equal? (grammar-derive a2nb2n '(a a a b b b))
              '(S -> aSb -> aaSbb -> aaaSbbb -> aaabbb))

;; L = {w | w in (a b)* AND  w has more b than a}
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

; Tests for numb>numa
(check-equal? (grammar-derive numb>numa '(a b))
              "(a b) is not in L(G).")
(check-equal? (grammar-derive numb>numa '(a b a))
              "(a b a) is not in L(G).")
(check-equal? (grammar-derive numb>numa '(a a a a a))
              "(a a a a a) is not in L(G).")
(check-equal? (grammar-derive numb>numa '(b b b))
              '(S -> AbA -> bA -> bbA -> bbbA -> bbb))
#|
(check-equal? (grammar-derive numb>numa '(b b a b a a b))
              '(S -> AbA -> AbAaAbA -> bAaAbA -> bAbAaAaAbA
                  -> bAbAaAbAaAaAbA -> bbAaAbAaAaAbA -> bbaAbAaAaAbA
                  -> bbabAaAaAbA -> bbabaAaAbA -> bbabaaAbA
                  -> bbabaabA -> bbabaab))
(check-equal? (grammar-derive numb>numa '(a a a b b b b))
              '(S -> AbA -> AaAbAbA -> aAbAbA -> aAaAbAbAbA
                  -> aaAbAbAbA -> aaAaAbAbAbAbA -> aaaAbAbAbAbA
                  -> aaabAbAbAbA -> aaabbAbAbA -> aaabbbAbA
                  ->  aaabbbbA -> aaabbbb))
|#

;; cfg --> pda
;; Purpose: Transform the given cfg into a pda
(define (cfg2pda G)
  (let [(nts (grammar-nts G))
        (sigma (grammar-sigma G))
        (start (grammar-start G))
        (rules (grammar-rules G))]
    (make-ndpda '(S Q)
                sigma
                (append nts sigma)
                'S
                (list 'Q)
                (append
                 (list (list (list 'S EMP EMP) (list 'Q (list start))))
                 (map (λ (r)
                        (list (list 'Q EMP (list (first r)))
                              (list 'Q
                                    (if (eq? (third r) EMP)
                                        EMP
                                        (symbol->fsmlos (third r))))))
                      rules)
                 (map (λ (a) (list (list 'Q a (list a)) (list 'Q EMP)))
                      sigma)))))

;; Tests

(define a2nb2n-pda (cfg2pda a2nb2n))
(define numb>numa-pda (cfg2pda numb>numa))

(check-equal? (sm-apply a2nb2n-pda '(b b)) 'reject)
(check-equal? (sm-apply a2nb2n-pda '(a a b b a b)) 'reject)
(check-equal? (sm-apply a2nb2n-pda '()) 'accept)
(check-equal? (sm-apply a2nb2n-pda '(a a a b b b)) 'accept)

(check-equal? (sm-apply numb>numa-pda '(b b b)) 'accept)
(check-equal? (sm-apply numb>numa-pda '(b b a)) 'accept)
;(check-equal? (sm-apply numb>numa-pda '(b b a b b)) 'accept)
;(check-equal? (sm-apply numb>numa-pda '(a b b a b)) 'accept) ;; ~3 mins

(define a2nb2b-pda (cfg2pda a2nb2n))
(define a2nb2n-cfg (pda2cfg a2nb2b-pda))

(check-equal? (sm-apply a2nb2b-pda '(a b)) 'accept)
(check-equal? (sm-apply a2nb2b-pda '(a a b b)) 'accept)
(check-equal? (last (grammar-derive a2nb2n-cfg '(a b))) 'ab)
(check-equal? (last (grammar-derive a2nb2n-cfg '(a a b b))) 'aabb)

(define a2nb2b-pda2 (cfg2pda (pda2cfg a2nb2b-pda)))

(check-equal? (sm-apply a2nb2b-pda2 '(a b)) 'accept)
(check-equal? (sm-apply a2nb2b-pda2 '(a a b b)) 'accept)

(define a2nb2b-pda3 (grammar->sm (pda2cfg a2nb2b-pda)))

(check-equal? (sm-apply a2nb2b-pda3 '(a b)) 'accept)
(check-equal? (sm-apply a2nb2b-pda3 '(a a b b)) 'accept)
