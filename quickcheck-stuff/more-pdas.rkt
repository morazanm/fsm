#lang fsm

;; (a^nb^n)U(b^na^n)

;; State documentation 
;; S: ci = empty AND stack = empty, start state 
;; A: ci = a^n AND stack = a^n
;; B: ci = a^nb^k AND (length stack) = n + k
;; C: ci = b^n AND stack = a^n
;; D: ci = b^na^k AND (length stack) = n + k
;; E: ci = (a^nb^n)U(b^na^n) AND stack = empty, final state

;; the stack is a (listof a)

(define a^nb^nUb^na^n (make-ndpda '(S A B C D E)
                                  '(a b)
                                  '(a)
                                  'S
                                  '(E)
                                  `(((S ,EMP ,EMP) (A ,EMP))
                                    ((S ,EMP ,EMP) (C ,EMP))
                                    ((A a ,EMP) (A (a)))
                                    ((A ,EMP ,EMP) (B ,EMP))
                                    ((B b (a)) (B ,EMP))
                                    ((B ,EMP ,EMP) (E ,EMP))
                                    ((C b ,EMP) (C (a)))
                                    ((C ,EMP ,EMP) (D ,EMP))
                                    ((D a (a)) (D ,EMP))
                                    ((D ,EMP ,EMP) (E ,EMP))
                                    )))

;; tests for (a^nb^n)U(b^na^n)
(check-accept? a^nb^nUb^na^n '() '(a b) '(b a) '(a a b b) '(b b a a) '(a a a a b b b b) '(b b b b a a a a))
(check-reject? a^nb^nUb^na^n '(a) '(b) '(b b a) '(a a b) '(a b b) '(b a a) '(a a b b b b) '(b b b a a a a)) 


;; invariants for a^nb^nUb^na^n

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
(define (S-INV-a^nb^nUb^na^n ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for s-inv
(check-equal? (S-INV-a^nb^nUb^na^n '() '()) #t)
(check-equal? (S-INV-a^nb^nUb^na^n '(a) '()) #f)
(check-equal? (S-INV-a^nb^nUb^na^n '(b) '(a)) #f)
(check-equal? (S-INV-a^nb^nUb^na^n '(a b) '()) #f)
(check-equal? (S-INV-a^nb^nUb^na^n '(b a) '(a)) #f)
(check-equal? (S-INV-a^nb^nUb^na^n '(b b a) '(b)) #f)
(check-equal? (S-INV-a^nb^nUb^na^n '(a b b) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
(define (A-INV-a^nb^nUb^na^n ci stack)
  (and (equal? ci stack)
       (andmap (λ (x) (eq? 'a x)) ci)
       (andmap (λ (x) (eq? 'a x)) stack)))

;; tests for a-inv
(check-equal? (A-INV-a^nb^nUb^na^n '() '()) #t)
(check-equal? (A-INV-a^nb^nUb^na^n '(a) '(a)) #t)
(check-equal? (A-INV-a^nb^nUb^na^n '(a a a) '(a a a)) #t)
(check-equal? (A-INV-a^nb^nUb^na^n '(a a a a a a) '(a a a a a a)) #t)
(check-equal? (A-INV-a^nb^nUb^na^n '(a) '()) #f)
(check-equal? (A-INV-a^nb^nUb^na^n '(b) '(a)) #f)
(check-equal? (A-INV-a^nb^nUb^na^n '(a b) '()) #f)
(check-equal? (A-INV-a^nb^nUb^na^n '(b a) '(a)) #f)
(check-equal? (A-INV-a^nb^nUb^na^n '(b b a) '(b)) #f)
(check-equal? (A-INV-a^nb^nUb^na^n '(a b b) '()) #f)




;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
(define (B-INV-a^nb^nUb^na^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (drop ci (length As))])
  (and (= (length As) (+ (length Bs) (length stack)))
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for b-inv
(check-equal? (B-INV-a^nb^nUb^na^n '() '()) #t)
(check-equal? (B-INV-a^nb^nUb^na^n '(a a a) '(a a a)) #t)
(check-equal? (B-INV-a^nb^nUb^na^n '(a a a a a b) '(a a a a)) #t)
(check-equal? (B-INV-a^nb^nUb^na^n '(a a b) '(a)) #t)
(check-equal? (B-INV-a^nb^nUb^na^n '(a a a b b b) '()) #t)
(check-equal? (B-INV-a^nb^nUb^na^n '(a) '()) #f)
(check-equal? (B-INV-a^nb^nUb^na^n '(b) '(a)) #f)
(check-equal? (B-INV-a^nb^nUb^na^n '(a b b) '()) #f)
(check-equal? (B-INV-a^nb^nUb^na^n '(b a) '(a)) #f)
(check-equal? (B-INV-a^nb^nUb^na^n '(b b a) '(b)) #f)
(check-equal? (B-INV-a^nb^nUb^na^n '(a b b) '()) #f)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
(define (C-INV-a^nb^nUb^na^n ci stack)
  (and (andmap (λ (x) (eq? 'b x)) ci)
       (andmap (λ (x) (eq? 'a x)) stack)))

;; tests for c-inv
(check-equal? (C-INV-a^nb^nUb^na^n '() '()) #t)
(check-equal? (C-INV-a^nb^nUb^na^n '(b b b) '(a a a)) #t)
(check-equal? (C-INV-a^nb^nUb^na^n '(b) '(a)) #t)
(check-equal? (C-INV-a^nb^nUb^na^n '(b b b b a) '(a a a a)) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(a a b) '(a)) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(a a a b b b) '()) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(a) '()) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(a b b) '()) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(b a) '(a)) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(b b a) '(b)) #f)
(check-equal? (C-INV-a^nb^nUb^na^n '(a b b) '()) #f)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in D
(define (D-INV-a^nb^nUb^na^n ci stack)
  (let* ([Bs (takef ci (λ (x) (eq? 'b x)))]
         [As (drop ci (length Bs))])
  (and (= (length Bs) (+ (length As) (length stack)))
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for D-inv
(check-equal? (D-INV-a^nb^nUb^na^n '() '()) #t)
(check-equal? (D-INV-a^nb^nUb^na^n '(b b b) '(a a a)) #t)
(check-equal? (D-INV-a^nb^nUb^na^n '(b b b b b a) '(a a a a)) #t)
(check-equal? (D-INV-a^nb^nUb^na^n '(b b a) '(a)) #t)
(check-equal? (D-INV-a^nb^nUb^na^n '(b b b a a a) '()) #t)
(check-equal? (D-INV-a^nb^nUb^na^n '(b) '()) #f)
(check-equal? (D-INV-a^nb^nUb^na^n '(a) '(a)) #f)
(check-equal? (D-INV-a^nb^nUb^na^n '(a b b) '()) #f)
(check-equal? (D-INV-a^nb^nUb^na^n '(b a) '(a)) #f)
(check-equal? (D-INV-a^nb^nUb^na^n '(b b a) '(b)) #f)
(check-equal? (D-INV-a^nb^nUb^na^n '(a b b) '()) #f)


;; E: ci = (a^nb^n)U(b^na^n) AND stack = empty, final state



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
(define (E-INV-a^nb^nUb^na^n ci stack)
  (let* ([first-As (takef ci (λ (x) (eq? 'a x)))]
         [first-Bs (takef ci (λ (x) (eq? 'b x)))]
         [As (drop ci (length first-Bs))]
         [Bs (drop ci (length first-As))])
    (and (empty? stack)
         (or (and (equal? ci (append first-As Bs))
                  (= (length first-As) (length Bs)))
             (and (equal? ci (append first-Bs As))
                  (= (length first-Bs) (length As)))))))

;; tests for E-inv
(check-equal? (E-INV-a^nb^nUb^na^n '() '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(a a a b b b) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(a a a a a b b b b b) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(a a b b) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(a a a b b b) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(b b b a a a) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(b b b b b a a a a a) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(b b a a) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(b b b a a a) '()) #t)
(check-equal? (E-INV-a^nb^nUb^na^n '(a) '()) #f)
(check-equal? (E-INV-a^nb^nUb^na^n '(b) '(a)) #f)
(check-equal? (E-INV-a^nb^nUb^na^n '(a b b) '()) #f)
(check-equal? (E-INV-a^nb^nUb^na^n '(b a) '(a)) #f)
(check-equal? (E-INV-a^nb^nUb^na^n '(b b a) '(b)) #f)
(check-equal? (E-INV-a^nb^nUb^na^n '(a b b) '()) #f)





(define LOI-a^nb^nUb^na^n (list (list 'S S-INV-a^nb^nUb^na^n)
                                (list 'A A-INV-a^nb^nUb^na^n)
                                (list 'B B-INV-a^nb^nUb^na^n)
                                (list 'C C-INV-a^nb^nUb^na^n)
                                (list 'D D-INV-a^nb^nUb^na^n)
                                (list 'E E-INV-a^nb^nUb^na^n)))
                                



;; a^nbaba^n

;; State documentation
;; S: ci = empty AND stack = empty, starting state
;; A: ci = a^n = stack
;; B: ci = a^nb AND stack = a^n
;; C: ci = a^nba AND stack = a^n
;; D: ci = a^nbab AND stack = a^n
;; E: ci = a^nbaba^k AND stack = a^n-k
;; F: ci = a^nbaba^n AND stack = empty, final state
;; a stack is a (listof a)

(define a^nbaba^n (make-ndpda '(S A B C D E F)
                              '(a b)
                              '(a)
                              'S
                              '(F)
                              `(((S ,EMP ,EMP) (A ,EMP))
                                ((A a ,EMP) (A (a)))
                                ((A b ,EMP) (B ,EMP))
                                ((B a ,EMP) (C ,EMP))
                                ((C b ,EMP) (D ,EMP))
                                ((D ,EMP ,EMP) (E ,EMP))
                                ((E a (a)) (E ,EMP))
                                ((E ,EMP ,EMP) (F ,EMP)))))


;; tests for a^nbaba^n
(check-accept? a^nbaba^n '(a b a b a) '(b a b) '(a a b a b a a) '(a a a b a b a a a) '(a a a a b a b a a a a))
(check-reject? a^nbaba^n '() '(b) '(a) '(a b b b b a b a) '(a b a b) '(a a b b a b a a) '(a a a a b b a b a a a))



;; invariants for a^nbaba^n

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
(define (S-INV-a^nbaba^n ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for s-inv
(check-equal? (S-INV-a^nbaba^n '() '()) #t)
(check-equal? (S-INV-a^nbaba^n '(a) '()) #f)
(check-equal? (S-INV-a^nbaba^n '(b) '()) #f)
(check-equal? (S-INV-a^nbaba^n '() '(a)) #f)
(check-equal? (S-INV-a^nbaba^n '(a b a b a) '()) #f)
(check-equal? (S-INV-a^nbaba^n '(b a b) '()) #f)
(check-equal? (S-INV-a^nbaba^n '(a a a b a b a a a) '()) #f)
(check-equal? (S-INV-a^nbaba^n '(a a b a b a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
(define (A-INV-a^nbaba^n ci stack)
  (and (equal? ci stack)
       (andmap (λ (x) (eq? 'a x)) ci)
       (andmap (λ (x) (eq? 'a x)) stack)))

;; tests for a-inv
(check-equal? (A-INV-a^nbaba^n '() '()) #t)
(check-equal? (A-INV-a^nbaba^n '(a) '(a)) #t)
(check-equal? (A-INV-a^nbaba^n '(a a) '(a a)) #t)
(check-equal? (A-INV-a^nbaba^n'(a a a) '(a a a)) #t)
(check-equal? (A-INV-a^nbaba^n '(a b a b a) '()) #f)
(check-equal? (A-INV-a^nbaba^n '(a b a b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
(define (B-INV-a^nbaba^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (and (equal? (append As B) ci)
       (equal? As stack)
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for b-inv
(check-equal? (B-INV-a^nbaba^n '() '()) #t)
(check-equal? (B-INV-a^nbaba^n '(a) '(a)) #t)
(check-equal? (B-INV-a^nbaba^n '(a b) '(a)) #t)
(check-equal? (B-INV-a^nbaba^n'(a a b) '(a a)) #t)
(check-equal? (B-INV-a^nbaba^n '(a b a b a) '()) #f)
(check-equal? (B-INV-a^nbaba^n '(a b a b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
(define (C-INV-a^nbaba^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (length As)) (λ (x) (eq? 'b x)))]
         [A (takef (drop ci (+ (length B) (length As))) (λ (x) (eq? 'a x)))])
  (and (equal? (append As B A) ci)
       (equal? As stack)
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for c-inv
(check-equal? (C-INV-a^nbaba^n '() '()) #t)
(check-equal? (C-INV-a^nbaba^n '(a b) '(a)) #t)
(check-equal? (C-INV-a^nbaba^n '(a b a) '(a)) #t)
(check-equal? (C-INV-a^nbaba^n'(a a b a) '(a a)) #t)
(check-equal? (C-INV-a^nbaba^n '(a b a b a) '()) #f)
(check-equal? (C-INV-a^nbaba^n '(a b a b b) '(a)) #f)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in D
(define (D-INV-a^nbaba^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (length As)) (λ (x) (eq? 'b x)))]
         [A (takef (drop ci (+ (length B) (length As))) (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (+ (length A) (length B) (length As))) (λ (x) (eq? 'b x)))])
  (and (equal? (append As B A B) ci)
       (equal? As stack)
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for d-inv
(check-equal? (D-INV-a^nbaba^n '() '()) #t)
(check-equal? (D-INV-a^nbaba^n '(b a b) '()) #t)
(check-equal? (D-INV-a^nbaba^n '(a b a b) '(a)) #t)
(check-equal? (D-INV-a^nbaba^n'(a a b a b) '(a a)) #t)
(check-equal? (D-INV-a^nbaba^n '(a b a b a) '()) #f)
(check-equal? (D-INV-a^nbaba^n '(a b a b b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
(define (E-INV-a^nbaba^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (length As)) (λ (x) (eq? 'b x)))]
         [A (takef (drop ci (+ (length B) (length As))) (λ (x) (eq? 'a x)))]
         [B-second (takef (drop ci (+ (length A) (length B) (length As))) (λ (x) (eq? 'b x)))]
         [As-after (takef (drop ci (+ (length B-second) (length A) (length B) (length As))) (λ (x) (eq? 'a x)))])
  (and (equal? (append As B A B As-after) ci)
       (= (- (length As) (length As-after)) (length stack))
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for e-inv
(check-equal? (E-INV-a^nbaba^n '() '()) #t)
(check-equal? (E-INV-a^nbaba^n '(b a b) '()) #t)
(check-equal? (E-INV-a^nbaba^n '(a b a b) '(a)) #t)
(check-equal? (E-INV-a^nbaba^n'(a a b a b a) '(a)) #t)
(check-equal? (E-INV-a^nbaba^n '(a b a b a a) '()) #f)
(check-equal? (E-INV-a^nbaba^n '(a b a b b) '(a)) #f)






;; F: ci = a^nbaba^n AND stack = empty, final state
;; a stack is a (listof a)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in F
(define (F-INV-a^nbaba^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (length As)) (λ (x) (eq? 'b x)))]
         [A (takef (drop ci (+ (length B) (length As))) (λ (x) (eq? 'a x)))]
         [B-second (takef (drop ci (+ (length A) (length B) (length As))) (λ (x) (eq? 'b x)))]
         [As-after (takef (drop ci (+ (length B-second) (length A) (length B) (length As))) (λ (x) (eq? 'a x)))])
  (and (equal? (append As B A B As-after) ci)
       (empty? stack)
       (equal? As As-after))))

;; tests for f-inv
(check-equal? (F-INV-a^nbaba^n '() '()) #t)
(check-equal? (F-INV-a^nbaba^n '(b a b) '()) #t)
(check-equal? (F-INV-a^nbaba^n '(a b a b a) '()) #t)
(check-equal? (F-INV-a^nbaba^n'(a a b a b a a) '()) #t)
(check-equal? (F-INV-a^nbaba^n '(a b a b a a) '()) #f)
(check-equal? (F-INV-a^nbaba^n '(a b a b b) '(a)) #f)




;; a^mca^mcaba^nba^n, m, n >= 1

;; State Documentaion:
;; S: ci = empty AND stack = empty, start state
;; A: ci = a^m AND stack = a^m
;; B: ci = a^mc AND stack = a^m
;; C: ci = a^mca^k AND stack = a^m-k
;; D: ci = a^mca^mc AND stack = empty
;; E: ci = a^mca^mca AND stack = empty
;; F: ci = a^mca^mcab AND stack = empty
;; G: ci = a^mca^mcaba^n AND stack = a^n
;; H: ci = a^mca^mcaba^nb AND stack = a^n
;; I: ci = a^mca^mcaba^nba^l AND stack = a^n-l
;; J: ci = a^mca^mcaba^nba^n AND stack = empty, final state

;; stack is a (listof a)
(define a^mca^mcaba^nba^n (make-ndpda '(S A B C D E F G H I J)
                                      '(a b c)
                                      '(a)
                                      'S
                                      '(J)
                                      `(((S a ,EMP) (A ,EMP))
                                        ((A a ,EMP) (A (a)))
                                        ((A c ,EMP) (B ,EMP))
                                        ((B a ,EMP) (C ,EMP))
                                        ((C a (a)) (C ,EMP))
                                        ((C c ,EMP) (D ,EMP))
                                        ((D a ,EMP) (E ,EMP))
                                        ((E b ,EMP) (F ,EMP))
                                        ((F a ,EMP) (G ,EMP))
                                        ((G a ,EMP) (G (a)))
                                        ((G b ,EMP) (H ,EMP))
                                        ((H a ,EMP) (I ,EMP))
                                        ((I a (a)) (I ,EMP))
                                        ((I ,EMP ,EMP) (J ,EMP))
                                        )))

 
;; tests for a^mca^mcaba^nba^n
(check-accept? a^mca^mcaba^nba^n '(a c a c a b a b a) '(a a c a a c a b a b a) '(a a a c a a a c a b a b a) '(a a c a a c a b a a a a b a a a a))
(check-reject? a^mca^mcaba^nba^n '(c c a b a b a) '(a a c a a c a b b) '(a a a a c a b a b a) '(a a a c a a c a b a a a a b a a a a))

;; invariants for a^mca^mcaba^nba^n

;; invariants for a^nbaba^n

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
(define (S-INV-a^mca^mcaba^nba^n ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for s-inv
(check-equal? (S-INV-a^mca^mcaba^nba^n '() '()) #t)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(a c) '(a)) #f)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(b) '()) #f)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(a c a c) '(a)) #f)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(a b a b a) '()) #f)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(b a b) '()) #f)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(a a a b a b a a a) '()) #f)
(check-equal? (S-INV-a^mca^mcaba^nba^n '(a a b a b a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
(define (A-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let ([As (takef (rest ci) (λ (x) (eq? 'a x)))])
         (and (eq? (first ci) 'a)
              (equal? ci (cons (first ci) As))
              (equal? As stack)
              (andmap (λ (x) (eq? 'a x)) ci)
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for a-inv
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a) '()) #t)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a) '(a)) #t)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a a a) '(a a a)) #t)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a b a) '()) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a a a) '()) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
(define (B-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (drop ci (length As))])
         (and (eq? (first ci) 'a)
              (equal? ci (append As C))
              (equal? As stack)
              (andmap (λ (x) (eq? 'c x)) C)
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for b-inv
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a c) '(a)) #t)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a a c) '(a a)) #t)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a a a a c) '(a a a a)) #t)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (B-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a b a c) '()) #f)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a a a a c c c b) '()) #f)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (B-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
(define (C-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second))
              (= (length stack) (- (length As) (length As-second)))
              (equal? C '(c))
              (andmap (λ (x) (eq? 'c x)) C)
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for c-inv
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a c a) '()) #t)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a a c a) '(a)) #t)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a a a a c a a) '(a a)) #t)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (C-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a a c a a) '(a)) #f)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a b a c) '()) #f)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a a a a c c c b) '()) #f)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (C-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in D
(define (D-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))])
         (and (equal? ci (append As C As-second C-second))
              (empty? stack)
              (equal? As As-second)
              (equal? C '(c))
              (equal? C-second '(c))
              (andmap (λ (x) (eq? 'a x)) stack)))))


;; tests for d-inv
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a c a c) '()) #t)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a a c a a c) '()) #t)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c) '()) #t)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (D-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a a c a a) '(a)) #f)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a b a c) '()) #f)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a a a a c c c b) '()) #f)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (D-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
(define (E-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))]
               [A (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second C-second A))
              (empty? stack)
              (equal? As As-second)
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (andmap (λ (x) (eq? 'a x)) A)
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for e-inv
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a c a c a) '()) #t)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a a c a a c a) '()) #t)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a) '()) #t)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (E-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a a c a a) '(a)) #f)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a b a c) '()) #f)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a a a a c c c b) '()) #f)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (E-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)


;; F: ci = a^mca^mcab AND stack = empty

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in F
(define (F-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))]
               [A (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second))) (λ (x) (eq? 'a x)))]
               [B (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) (λ (x) (eq? 'b x)))])
         (and (equal? ci (append As C As-second C-second A B))
              (empty? stack)
              (equal? As As-second)
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (andmap (λ (x) (eq? 'b x)) B)
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for f-inv
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a c a c a b) '()) #t)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a a c a a c a b) '()) #t)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b) '()) #t)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a a c c a a c c ) '()) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a b a c a) '()) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a a a a c c c b) '()) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)
(check-equal? (F-INV-a^mca^mcaba^nba^n '(a c a c c b) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in G
(define (G-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))]
               [A (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second))) (λ (x) (eq? 'a x)))]
               [B (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) (λ (x) (eq? 'b x)))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second C-second A B As-third))
              (equal? stack As-third)
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (andmap (λ (x) (eq? 'b x)) B)
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for g-inv
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a c a c a b) '()) #t)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a c a a c a b a) '(a)) #t)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a) '(a a)) #t)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a b) '(a a)) #f)
(check-equal? (G-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a c a a c c b a) '(a)) #f)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a b a c a) '()) #f)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a a a c c c b) '()) #f)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)



;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in H
(define (H-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))]
               [A (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second))) (λ (x) (eq? 'a x)))]
               [B (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) (λ (x) (eq? 'b x)))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))]
               [B-second (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third))) (λ (x) (eq? 'b x)))])
         (and (equal? ci (append As C As-second C-second A B As-third B-second))
              (equal? stack As-third)
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (equal? B-second '(b))
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for h-inv
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a c a c a b a b) '(a)) #t)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b) '(a)) #t)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b) '(a a)) #t)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a b) '(a a b)) #f)
(check-equal? (H-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a c a a c c b b a) '(a)) #f)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a b a c b a) '()) #f)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a a a c  b c c b) '()) #f)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in I
(define (I-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))]
               [A (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second))) (λ (x) (eq? 'a x)))]
               [B (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) (λ (x) (eq? 'b x)))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))]
               [B-second (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third))) (λ (x) (eq? 'b x)))]
               [As-fourth (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third) (length B-second))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second C-second A B As-third B-second As-fourth))
              (= (length stack) (- (length As-third) (length As-fourth)))
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (equal? B-second '(b))
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for i-inv
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a c a c a b a b) '(a)) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b) '(a)) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b a) '(a)) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a c a c a b a b a) '()) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b a) '()) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b a a) '()) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a b) '(a a b)) #f)
(check-equal? (I-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a c a a c c b b a) '(a)) #f)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a b a c b a) '()) #f)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a a a c  b c c b) '()) #f)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)

;; J: ci = a^mca^mcaba^nba^n AND stack = empty, final state

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in J
(define (J-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (takef (drop ci (length As)) (λ (x) (eq? 'c x)))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (takef (drop ci (+ (length As) (length C) (length As-second))) (λ (x) (eq? 'c x)))]
               [A (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second))) (λ (x) (eq? 'a x)))]
               [B (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) (λ (x) (eq? 'b x)))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))]
               [B-second (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third))) (λ (x) (eq? 'b x)))]
               [As-fourth (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third) (length B-second))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second C-second A B As-third B-second As-fourth))
              (empty? stack)
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (equal? B-second '(b))
              (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for j-inv
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a c a c a b a b a) '()) #t)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b a) '()) #t)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b a a) '()) #t)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a c a c a b a b a) '()) #t)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b a) '()) #t)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b a a) '()) #t)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a b) '(a a b)) #f)
(check-equal? (J-INV-a^mca^mcaba^nba^n '() '()) #f)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a c a a c c b b a) '(a)) #f)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a b a c b a) '()) #f)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a a a c  b c c b) '()) #f)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (J-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)


;; ((a^n b^n)U(b^n a^n))U ((c^m d^m)U(d^m c^m))


(define lots-of-equals (make-ndpda '(S A B C D E F G H)
                                   '(a b c d)
                                   '(a b)
                                   'S
                                   '(F H)
                                   `(((S ,EMP ,EMP) (A ,EMP))
                                     ((S ,EMP ,EMP) (C ,EMP))
                                     ((A a ,EMP) (A (a)))
                                     ((A ,EMP ,EMP) (B ,EMP))
                                     ((B b (a)) (B ,EMP))
                                     ((B ,EMP ,EMP) (E ,EMP))
                                     ((B ,EMP ,EMP) (G ,EMP))
                                     ((C b ,EMP) (C (a)))
                                     ((C ,EMP ,EMP) (D ,EMP))
                                     ((D a (a)) (D ,EMP))
                                     ((D ,EMP ,EMP) (E ,EMP))
                                     ((D ,EMP ,EMP) (G ,EMP))
                                     ((E c ,EMP) (E (b)))
                                     ((E ,EMP ,EMP) (F ,EMP))
                                     ((F d (b)) (F ,EMP))
                                     ((G d ,EMP) (G (a)))
                                     ((G ,EMP ,EMP) (H ,EMP))
                                     ((H c (b)) (H ,EMP)))))
                                     



;; (a^n b^n)*


;; state documentation
;; S: ci = empty = stack
;; A: ci = (a^nb^n)*a^m AND stack = a^m
;; B: ci = (a^nb^n)*a^mb^l AND stack = a^m-l
;; C: ci = (a^nb^n)* AND stack = empty
(define a^nb^n* (make-ndpda '(S A B C)
                            '(a b)
                            '(a)
                            'S
                            '(S C)
                            `(((S ,EMP ,EMP) (A ,EMP))
                              ((A a ,EMP) (A (a)))
                              ((A ,EMP ,EMP) (B ,EMP))
                              ((B b (a)) (B ,EMP))
                              ((B ,EMP ,EMP) (A ,EMP))
                              ((B ,EMP ,EMP) (C ,EMP)))))
;; tests for a^nb^n*
(check-accept? a^nb^n* '() '(a b) '(a b a b) '(a b a b a b) '(a a b b a b) '(a a b b a a a b b b a b a a b b))
(check-reject? a^nb^n* '(a) '(b) '(a a) '(b b) '(a b b) '(a a b))


;; invariants for a^nb^n*

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
(define (S-INV-a^nb^n* ci stack)
  (and (empty? stack)
       (empty? ci)))

;; tests for s-inv
(check-equal? (S-INV-a^nb^n* '() '()) #t)
(check-equal? (S-INV-a^nb^n* '(a) '(a)) #f)
(check-equal? (S-INV-a^nb^n* '(a a) '(a a)) #f)
(check-equal? (S-INV-a^nb^n* '(b) '()) #f)
       

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
(define (A-INV-a^nb^n* ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (if (empty? Bs)
      (equal? stack As)
      (and (= (length As) (length Bs))
           (A-INV-a^nb^n* (drop ci (+ (length As) (length Bs))) stack)))))

;; tests for a-inv
(check-equal? (A-INV-a^nb^n* '() '()) #t)
(check-equal? (A-INV-a^nb^n* '(a) '(a)) #t)
(check-equal? (A-INV-a^nb^n* '(a a b b a) '(a)) #t)
(check-equal? (A-INV-a^nb^n* '(a b a a) '(a a)) #t)
(check-equal? (A-INV-a^nb^n* '(b) '(a)) #f)
(check-equal? (A-INV-a^nb^n* '(a a b) '(a)) #f)
(check-equal? (A-INV-a^nb^n* '(a a ) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
(define (B-INV-a^nb^n* ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (if (empty? (drop ci (+ (length Bs) (length As))))
      (= (length As) (+ (length Bs) (length stack)))
      (and (= (length As) (length Bs))
           (A-INV-a^nb^n* (drop ci (+ (length As) (length Bs))) stack)))))

;; tests for b-inv
(check-equal? (B-INV-a^nb^n* '(a b) '()) #t)
(check-equal? (B-INV-a^nb^n* '(a a b b) '()) #t)
(check-equal? (B-INV-a^nb^n* '(a a b) '(a)) #t)
(check-equal? (B-INV-a^nb^n* '(a a a b b b a a) '(a a)) #t)
(check-equal? (B-INV-a^nb^n* '(b) '(a)) #f)
(check-equal? (B-INV-a^nb^n* '(a a b b) '(a)) #f)
(check-equal? (B-INV-a^nb^n* '(a a) '(a)) #f)

;; C: ci = (a^nb^n)* AND stack = empty


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
(define (C-INV-a^nb^n* ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (if (and (empty? (drop ci (+ (length Bs) (length As))))
           (empty? stack))
      (= (length As) (length Bs))
      (and (= (length As) (length Bs))
           (A-INV-a^nb^n* (drop ci (+ (length As) (length Bs))) stack)))))

;; tests for c-inv
(check-equal? (C-INV-a^nb^n* '(a b) '()) #t)
(check-equal? (C-INV-a^nb^n* '(a a b b) '()) #t)
(check-equal? (C-INV-a^nb^n* '(a a b b a b a b a b) '()) #t)
(check-equal? (C-INV-a^nb^n* '(a a a b b b a b a a b b) '()) #t)
(check-equal? (C-INV-a^nb^n* '(b) '(a)) #f)
(check-equal? (C-INV-a^nb^n* '(a a b b) '(a)) #f)
(check-equal? (C-INV-a^nb^n* '(a a) '(a)) #f)









;;(a^nb^ma^nUb^na^mb^n)a
