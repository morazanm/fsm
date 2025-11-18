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


;; E: ci = a^nbaba^k AND stack = a^n-k

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
(define (E-INV-a^nbaba^n ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (length As)) (λ (x) (eq? 'b x)))]
         [A (takef (drop ci (+ (length B) (length As))) (λ (x) (eq? 'a x)))]
         [B (takef (drop ci (+ (length A) (length B) (length As))) (λ (x) (eq? 'b x)))]
         [As-after (takef (drop ci (+ (length A) (length B) (length As))) (λ (x) (eq? 'a x)))])
  (and (equal? (append As B A B As-after) ci)
       (= (- (length As) (length As-after)) (length stack))
       (andmap (λ (x) (eq? 'a x)) stack))))

;; tests for e-inv
(check-equal? (E-INV-a^nbaba^n '() '()) #t)
(check-equal? (E-INV-a^nbaba^n '(b a b) '()) #t)
(check-equal? (E-INV-a^nbaba^n '(a b a b) '(a)) #t)
(check-equal? (E-INV-a^nbaba^n'(a a b a b a) '(a)) #t)
(check-equal? (E-INV-a^nbaba^n '(a b a b a) '()) #f)
(check-equal? (E-INV-a^nbaba^n '(a b a b b) '(a)) #f)

;; F: ci = a^nbaba^n AND stack = empty, final state
;; a stack is a (listof a)
