#lang racket
(require racket/list
         rackunit
         "sm-test-invs-pda.rkt"
         "../fsm-core/private/pda.rkt"
         "../fsm-core/private/sm-apply.rkt"
         "../fsm-core/private/constants.rkt"
         "../sm-graph.rkt"
         )


;                                      
;                                      
;                                      
;                                      
;    ;;;;;;  ;;;;;;     ;;;            
;     ;    ;  ;    ;     ;;            
;     ;    ;  ;     ;   ;  ;    ;;;; ; 
;     ;    ;  ;     ;   ;  ;   ;    ;; 
;     ;    ;  ;     ;   ;  ;   ;       
;     ;;;;;   ;     ;  ;;;;;;   ;;;;;  
;     ;       ;     ;  ;    ;        ; 
;     ;       ;    ;  ;      ; ;     ; 
;    ;;;;;   ;;;;;;  ;;;    ;;;;;;;;;  
;                                      
;                                      
;                                      
;                                      
;                                      



;; L = {aˆnbˆn | n >= 0}
;; States
;; S ci = (listof a) = stack, start state
;; M ci = (append (listof a) (listof b)) AND
;;        (length ci as) = (length stack) + (length ci bs)
;; F ci = (append (listof a) (listof b)) and all as and bs matched,
;; final state
;; The stack is a (listof a)
(define aˆnbˆn (make-unchecked-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))
;; Tests for aˆnbˆn
(check-equal? (sm-apply aˆnbˆn '(a)) 'reject)
(check-equal? (sm-apply aˆnbˆn '(b b)) 'reject)
(check-equal? (sm-apply aˆnbˆn '(a b b)) 'reject)
(check-equal? (sm-apply aˆnbˆn '(a b a a b b)) 'reject)
(check-equal? (sm-apply aˆnbˆn '()) 'accept)
(check-equal? (sm-apply aˆnbˆn '(a a b b)) 'accept)


;; Invariants for aˆnbˆn

;; word stack → Boolean
;; Purpose: Determine if the given ci and stack are the
;;          same (listof a)
(define (S-INV-aˆnbˆn ci stck)
  (and (= (length ci) (length stck))
       (andmap (λ (i g) (and (eq? i 'a) (eq? g 'a))) ci stck)))

;; Tests for S-INV-aˆnbˆn
(check-equal? (S-INV-aˆnbˆn '() '(a a)) #f)
(check-equal? (S-INV-aˆnbˆn '(a) '()) #f)
(check-equal? (S-INV-aˆnbˆn '(b b b) '(b b b)) #f)
(check-equal? (S-INV-aˆnbˆn '() '()) #t)
(check-equal? (S-INV-aˆnbˆn '(a a a) '(a a a)) #t)


;; word stack → Boolean
;; Purpose: Determine if ci = EMP or a+b+ AND the stack
;;          only contains a AND |ci as| = |stack| + |ci bs|
(define (M-INV-aˆnbˆn ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (equal? (append as bs) ci)
         (andmap (λ (s) (eq? s 'a)) stck)
         (= (length as) (+ (length bs) (length stck))))))

;; Tests for M-INV-aˆnbˆn
(check-equal? (M-INV-aˆnbˆn '(a a b) '(a a)) #f)
(check-equal? (M-INV-aˆnbˆn '(a) '()) #f)
(check-equal? (M-INV-aˆnbˆn '(a a a b) '(a a a)) #f)
(check-equal? (M-INV-aˆnbˆn '(a a a b) '(a)) #f)
(check-equal? (M-INV-aˆnbˆn '() '()) #t)
(check-equal? (M-INV-aˆnbˆn '(a) '(a)) #t)
(check-equal? (M-INV-aˆnbˆn '(a b) '()) #t)
(check-equal? (M-INV-aˆnbˆn '(a a a b b) '(a)) #t)



;; word stack → Boolean
;; Purpose: Determine if ci = a^nb^n and stack is empty
(define (F-INV-aˆnbˆn ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (empty? stck)
         (equal? (append as bs) ci)
         (= (length as) (length bs)))))

;; Tests for F-INV-aˆnbˆn
(check-equal? (F-INV-aˆnbˆn '(a a b) '()) #f)
(check-equal? (F-INV-aˆnbˆn '(a) '()) #f)
(check-equal? (F-INV-aˆnbˆn '(a a a b) '(a a a)) #f)
(check-equal? (F-INV-aˆnbˆn '() '()) #t)
(check-equal? (F-INV-aˆnbˆn '(a b) '()) #t)
(check-equal? (F-INV-aˆnbˆn '(a a b b) '()) #t)


(define LOI-aˆnbˆn (list (list 'S S-INV-aˆnbˆn) (list 'M M-INV-aˆnbˆn)
                         (list 'F F-INV-aˆnbˆn)))



                                           
;; L = wcwˆR | w in (a b)*
;; States
;; S ci is empty and stack is empty
;; P ci = stackˆR AND c not in ci
;; Q ci = (append w (list c) v) AND
;; w = stackˆR vˆR
;; F stack = () AND ci = (append w (list c) wˆR)
(define wcwˆr (make-unchecked-ndpda '(S P Q F)
                          '(a b c)
                          '(a b)
                          'S
                          '(F)
                          `(((S ,EMP ,EMP) (P ,EMP))
                            ((P a ,EMP) (P (a)))
                            ((P b ,EMP) (P (b)))
                            ((P c ,EMP) (Q ,EMP))
                            ((Q a (a)) (Q ,EMP))
                            ((Q b (b)) (Q ,EMP))
                            ((Q ,EMP ,EMP) (F ,EMP)))))

;; Tests for wcwˆr
(check-equal? (sm-apply wcwˆr '(a)) 'reject)
(check-equal? (sm-apply wcwˆr '(a c)) 'reject)
(check-equal? (sm-apply wcwˆr '(b c a)) 'reject)
(check-equal? (sm-apply wcwˆr '(a a b c b a b)) 'reject)
(check-equal? (sm-apply wcwˆr '(c)) 'accept)
(check-equal? (sm-apply wcwˆr '(a c a)) 'accept)
(check-equal? (sm-apply wcwˆr '(a b b b c b b b a)) 'accept)


;; invariants for wcwˆr

;; word stack → Boolean
;; Purpose: Determine in the given word and stack are empty
(define (S-INV-wcwˆr ci s) (and (empty? ci) (empty? s)))

;; Tests for S-INV-wcwˆr
(check-equal? (S-INV-wcwˆr '() '(a a)) #f)
(check-equal? (S-INV-wcwˆr '(a c a) '()) #f)
(check-equal? (S-INV-wcwˆr '(a c a) '(b b)) #f)
(check-equal? (S-INV-wcwˆr '() '()) #t)

;; word stack → Boolean
;; Purpose: Determine if the given ci is the reverse of
;; the given stack AND c is not in ci
(define (P-INV-wcwˆr ci s)
  (and (equal? ci (reverse s))
       (not (member 'c ci))))

;; Tests for P-INV-wcwˆr
(check-equal? (P-INV-wcwˆr '(a c a) '(a c a)) #f)
(check-equal? (P-INV-wcwˆr '(a a) '(a b)) #f)
(check-equal? (P-INV-wcwˆr '() '()) #t)
(check-equal? (P-INV-wcwˆr '(a b) '(b a)) #t)
(check-equal? (P-INV-wcwˆr '(a b a a) '(a a b a)) #t)


;; word stack → Boolean
;; Purpose: Determine if ci=s^Rv^Rcv
(define (Q-INV-wcwˆr ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))
         (v (if (member 'c ci)
                (drop ci (add1 (length w)))
                '()))]
    (and (equal? ci (append w (list 'c) v))
         (equal? w (append (reverse s) (reverse v))))))

;; Tests for Q-INV-wcwˆr
(check-equal? (Q-INV-wcwˆr '(a a) '()) #f)
(check-equal? (Q-INV-wcwˆr '(b b c a) '(b a)) #f)
(check-equal? (Q-INV-wcwˆr '(c) '()) #t)
(check-equal? (Q-INV-wcwˆr '(b a c) '(a b)) #t)
(check-equal? (Q-INV-wcwˆr '(a b c b) '(a)) #t)
(check-equal? (Q-INV-wcwˆr '(a b b c b) '(b a)) #t)


;; word stack → Boolean
;; Purpose: Determine if ci=s^Rv^Rcv AND stack is empty
(define (F-INV-wcwˆr ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))]
    (and (empty? s)
         (equal? ci (append w (list 'c) (reverse w))))))

;; Tests for F-INV-wcwˆr
(check-equal? (F-INV-wcwˆr '() '()) #f)
(check-equal? (F-INV-wcwˆr '(b b) '()) #f)
(check-equal? (F-INV-wcwˆr '(b a c) '(b a)) #f)
(check-equal? (F-INV-wcwˆr '(c) '()) #t)
(check-equal? (F-INV-wcwˆr '(b a c a b) '()) #t)
(check-equal? (F-INV-wcwˆr '(a b b c b b a) '()) #t)




(define LOI-wcwˆr (list (list 'S S-INV-wcwˆr) (list 'P P-INV-wcwˆr)
                        (list 'Q Q-INV-wcwˆr) (list 'F F-INV-wcwˆr)))

  


;; Let Σ = {a b}. Design and implement a pda for L = {w | w has an
;; equal number of as and bs}. Follow all the steps of the design recipe.

;; L = {w | w has an equal number of as and bs}
;; States
;;    S: stack contains the number of bs that have been read - the number of as that have been read
;;       OR stack containst the number of as that have been read - the number of bs that have been read
;; The stack is a (listof (a*b*))


(define equal-as-bs (make-unchecked-ndpda '(S)
                                '(a b)
                                '(a b)
                                'S
                                '(S)
                                `(((S a ,EMP) (S (a)))
                                  ((S b ,EMP) (S (b)))
                                  ((S a (b)) (S ,EMP))
                                  ((S b (a)) (S ,EMP)))))



;; Let Σ = {a b}. Design and implement a pda for L = {w | w is a
;; palindrome}. Follow all the steps of the design recipe.
;; (a palindrome is a word that can be spelled the same forward and backwards

;; basically ww^R

;; States
;;   S: stack is empty AND ci is empty ,start state and final state
;;   A: ci = the stack reversed (w^R
;;   B: ci = (append w m v) AND w = stack^R m v^R
;;   C: stack = empty AND ci is a palindrome (append w w^R), final state 
(define palindrome-pda (make-unchecked-ndpda '(S A B C)
                                   '(a b)
                                   '(a b)
                                   'S
                                   '(C)
                                   `(((S ,EMP ,EMP) (A ,EMP))
                                     ((A a ,EMP) (A (a)))
                                     ((A b ,EMP) (A (b)))
                                     ((A a ,EMP) (B,EMP))
                                     ((A b ,EMP) (B ,EMP))
                                     ((A ,EMP ,EMP) (B ,EMP))
                                     ((B a (a)) (B ,EMP))
                                     ((B b (b)) (B ,EMP))
                                     ((B ,EMP ,EMP) (C ,EMP)))))

;; tests for palindrome-pda
(check-equal? (sm-apply palindrome-pda '()) 'accept)
(check-equal? (sm-apply palindrome-pda '(a)) 'accept)
(check-equal? (sm-apply palindrome-pda '(a b a)) 'accept)
(check-equal? (sm-apply palindrome-pda '(b a b)) 'accept)
(check-equal? (sm-apply palindrome-pda '(a a b b a a)) 'accept)
(check-equal? (sm-apply palindrome-pda '(a b)) 'reject)
(check-equal? (sm-apply palindrome-pda '(b a)) 'reject)
(check-equal? (sm-apply palindrome-pda '(a b a a)) 'reject)

;; invariants for palindrome-pda

;; invariants also take in the stack now

;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in S
;;          (empty stack AND empty word)
(define (S-INV-PALINDROME-PDA ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for S-INV-PALINDROME-PDA
(check-equal? (S-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (S-INV-PALINDROME-PDA '(a) '()) #f)
(check-equal? (S-INV-PALINDROME-PDA '() '(b)) #f)
(check-equal? (S-INV-PALINDROME-PDA '(a) '(a)) #f)
(check-equal? (S-INV-PALINDROME-PDA '(b b) '(b b)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in A
;;          (ci = stack reversed)
(define (A-INV-PALINDROME-PDA ci stack)
  (equal? ci (reverse stack)))

;; tests for A-INV-PALINDROME-PDA
(check-equal? (A-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (A-INV-PALINDROME-PDA '(a) '(a)) #t)
(check-equal? (A-INV-PALINDROME-PDA '(a b b a) '(a b b a)) #t)
(check-equal? (A-INV-PALINDROME-PDA '(b b) '(b b)) #t)
(check-equal? (A-INV-PALINDROME-PDA '(b a) '(b a)) #f)
(check-equal? (A-INV-PALINDROME-PDA '(a b) '(a b)) #f)
(check-equal? (A-INV-PALINDROME-PDA '(a a a a) '(b b)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;          (ci = (append w v) AND w = stack^R v^R)
(define (B-INV-PALINDROME-PDA ci stack)    
  (or (empty? ci)
      (and (<= (length stack) (length ci))
           (local [(define w (if (even? (length (append ci stack)))
                                 (take ci (/ (length (append ci stack)) 2))         
                                 (take ci (floor (/ (length (append ci stack)) 2)))))
                   
                   (define v (if (even? (length (append ci stack)))
                                 (drop ci (length w))
                                 (drop ci (add1 (length w)))))]
             (equal? w (append (reverse stack) (reverse v)))))))

;; tests for B-INV-PALINDROME-PDA
(check-equal? (B-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a b) '(a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a b b) '(a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(b b a) '(b b)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a b b a b) '(b a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(a a a) '(a a)) #t)
(check-equal? (B-INV-PALINDROME-PDA '(b) '(b a)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(a a) '(b b)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(b a) '(b b)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(a a) '(b a)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(b) '(b b)) #f)
(check-equal? (B-INV-PALINDROME-PDA '(b b) '(b b b)) #f)


               
;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in C
;;          (stack = empty AND ci = ww^R)
(define (C-INV-PALINDROME-PDA ci stack)
  (and (empty? stack)
       (or (empty? ci)
           (equal? ci (reverse ci)))))

;; tests for C-INV-PALINDROME-PDA
(check-equal? (C-INV-PALINDROME-PDA '() '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a a b a a) '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a a a a a a) '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a a a a a) '()) #t)
(check-equal? (C-INV-PALINDROME-PDA '(a b a b) '()) #f)
(check-equal? (C-INV-PALINDROME-PDA '(a a a b) '(a a b)) #f)


(define LOI-palindrome-pda (list (list 'S S-INV-PALINDROME-PDA)
                                 (list 'A A-INV-PALINDROME-PDA)
                                 (list 'B B-INV-PALINDROME-PDA)
                                 (list 'C C-INV-PALINDROME-PDA)))

;; PROOF for palindrome-pda

;; The state invariants hold when M accepts w
#|

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. This
guarentees that ci = '() and stack = '(). After using the transition rule,
ci = '() and stack = '(). A-INV holds since the consumed input is the reverse
of the stack. Thus, A-INV holds after the transition rule.

((A a ,EMP) (A (a))): By inductive hypothesis, A-INV holds. This
guarentees that ci is the reverse of the stack. After using the transition rule,
we read an a and push an a onto the stack, making the stack the
reverse of the consumed input, and with this, A-INV holds.
Thus, A-INV holds after the transition rule is used. 

((A b ,EMP) (A (b))): By inductive hypothesis, A-INV holds. This guareentees
that the ci is the reverse of the stack. After using the transition rule, we read
a b and push a b onto the stack. With this, the stack is the reverse of the
consumed input, making A-INV hold. Thus, A-INV holds after the transition
rule is used. 

((A a ,EMP) (B,EMP)): By inductive hypothesis, A-INV holds. This
guarentees that the ci is the reverse of the stack. After using the
transition rule, an a is read and nothing is pushed and popped onto
the stack. B-INV holds since ci = (append w m v) AND w = stack^R m v^R.
In this case, w is the reverse of the stack and v = '(), and m = a, making the
B-INV hold. Thus, B-INV holds after the transition rule. 

((A b ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. This
guarentees that the ci is the reverse of the stack. After using the
transition rule, a b is read and nothing is pushed and popped onto
the stack. B-INV holds since ci = (append w m v) AND w = stack^R m v^R.
ci = (stack^R v) is the reverse of the stack and v = '(), with v being
the letters that were matched with the stack and m = b. Thus, B-INV holds
after the transition rule.

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis , A-INV holds. This
guarentees that the ci = stack^R. After using this transition rule,
the ci = stack^R and the stack remains the same since nothing is pushed
or popped. w = stack^R m v^R and ci = (append w m v), with m = '() and v = '(). 
Thus, B-INV holds after the transition rule is used. 

((B a (a)) (B ,EMP)): By inductive hypothesis, B-INV holds. This
guareentees that ci = (append w m v) AND w = stack^R m v^R. After
using this transition rule, ci = (append w m v) AND w = stack^R m v^R.
Thus, B-INV holds after the transition rule. 

((B b (b)) (B ,EMP)): By inductive hypothesis, B-INV holds. This
guareentees that ci = (append w m v) AND w = stack^R m v^R. After
this transition rule, ci = (append w m v) AND w = stack^R m v^R. Thus,
B-INV holds after the transition rule. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. This
guareentees that ci = (append w m v) AND w = stack^R m v^R. After
this transition rule, ci = (append w m v) AND w = stack^R m v^R. Since
C is a final state, the stack must be empty for the word to be accepted,
and for the word to be accepted, it must be a palindrone. Thus,
C-INV holds after the transition rule. 


|#



;; Let Σ = {a b}. Design and implement a pda for
;; L = {aibj |i≤j≤2i}. Follow all the steps of the design recipe

;; States: 
;;   S: ci = empty AND stack = empty, start and final state
;;   A: ci = a^i AND stack = a^k AND i <= k <= 2i
;;   B: ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i
;;   C: ci = a^ib^j where i≤j≤2i AND stack = empty, final state

(define AiBj (make-unchecked-ndpda '(S A B C)
                         '(a b)
                         '(a)
                         'S
                         '(C)
                         `(((S a ,EMP) (A (a)))
                           ((S a ,EMP) (A (a a)))
                           ((S ,EMP ,EMP) (A ,EMP))
                           ((A a ,EMP) (A (a)))
                           ((A a ,EMP) (A (a a)))
                           ((A b (a)) (B ,EMP))
                           ((A ,EMP ,EMP) (B ,EMP))
                           ((B b (a)) (B ,EMP))
                           ((B ,EMP ,EMP) (C ,EMP))
                           )))
                         

;; Tests for AiBj
(check-equal? (sm-apply AiBj '(a b)) 'accept)
(check-equal? (sm-apply AiBj '(a a b b b)) 'accept)
(check-equal? (sm-apply AiBj '()) 'accept)
(check-equal? (sm-apply AiBj '(a a a b b b)) 'accept)
(check-equal? (sm-apply AiBj '(a b b)) 'accept)
(check-equal? (sm-apply AiBj '(a a a a a a b)) 'reject)
(check-equal? (sm-apply AiBj '(a b b b)) 'reject)
(check-equal? (sm-apply AiBj '(a)) 'reject)
(check-equal? (sm-apply AiBj '(b)) 'reject)
(check-equal? (sm-apply AiBj '(a a)) 'reject)
(check-equal? (sm-apply AiBj '(a a b)) 'reject)


;; invariants for AiBj

;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in S
;;          (empty stack AND empty word)
(define (S-INV-AiBj ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for S-INV-AiBj
(check-equal? (S-INV-AiBj '() '()) #t)
(check-equal? (S-INV-AiBj '(a) '(a a)) #f)
(check-equal? (S-INV-AiBj '(b) '()) #f)
(check-equal? (S-INV-AiBj '(a a a a) '(a a a a a a a a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in A
;;          ci = a^i AND stack = a^k AND i <= k <= 2i
(define (A-INV-AiBj ci stack)
  (and (andmap (λ (x) (eq? 'a x)) ci)
       (<= (length ci) (length stack) (* 2 (length ci)))))

;; tests for A-INV-AiBj
(check-equal? (A-INV-AiBj '(a) '(a a)) #t)
(check-equal? (A-INV-AiBj '(a a) '(a a a a)) #t)
(check-equal? (A-INV-AiBj '(a a a) '(a a a a a a)) #t)
(check-equal? (A-INV-AiBj '(a a a a) '(a a a a a a a a)) #t)
(check-equal? (A-INV-AiBj '() '()) #t)
(check-equal? (A-INV-AiBj '() '(a)) #f)
(check-equal? (A-INV-AiBj '(a b) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;          (ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i)
(define (B-INV-AiBj ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (equal? (append As Bs) ci)
         (<= (length As) (+ (length Bs) (length stack)) (* 2 (length As)))
         )))

;; tests for B-INV-AiBj
(check-equal? (B-INV-AiBj '(a a b b) '()) #t)
(check-equal? (B-INV-AiBj '(a b) '(a)) #t)
(check-equal? (B-INV-AiBj '(a a a b b b) '(a a a)) #t)
(check-equal? (B-INV-AiBj '(a a b b b) '(a)) #t)
(check-equal? (B-INV-AiBj '(a a b b) '(a a)) #t)
(check-equal? (B-INV-AiBj '(a a b) '(a a a a)) #f)
(check-equal? (B-INV-AiBj '(a a b b b b b b) '()) #f)
(check-equal? (B-INV-AiBj '(a b b b b b b b b b) '()) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in C
;;          (ci = a^ib^j where i≤j≤2i AND stack = empty)
(define (C-INV-AiBj ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (<= (length As) (length Bs) (* 2 (length As)))
         (equal? ci (append As Bs))
         (empty? stack))))

;; tests for C-INV-AiBj
(check-equal? (C-INV-AiBj '(a b) '()) #t)
(check-equal? (C-INV-AiBj '(a b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b b b) '()) #t)
(check-equal? (C-INV-AiBj '(a a b b b) '()) #t)
(check-equal? (C-INV-AiBj '(a b b b) '()) #f)
(check-equal? (C-INV-AiBj '() '()) #t)
(check-equal? (C-INV-AiBj '(a b b) '(a)) #f)
(check-equal? (C-INV-AiBj '(a b b) '(a a)) #f)


(define LOI-AiBj (list (list 'S S-INV-AiBj) (list 'A A-INV-AiBj)
                       (list 'B B-INV-AiBj) (list 'C C-INV-AiBj)))


;; PROOF

;; The state invariants hold when M accepts w
#|

When M starts, S-INV holds because ci = '() and s '().
This establishes the base case.
 

Proof invariants hold after each transition that consumes input:

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. This
guareentees that ci = '() and s = '(). After using this rule, ci = '() = a^i
and stack = '() = a^k. A-holds, because i <= k <= 2i.

((S a ,EMP) (A (a))): By inductive hypothesis, S-INV holds.
This guarantees that ci = '() and s = '(). After using this
rule, ci = '(a) = a^i and s = '(a) = a^k. A-INV holds, because ci = a^i
AND stack = '() = a^k. Observe that i <= k <= 2i. Thus, A-INV holds. 

((S a ,EMP) (A (a a))): By inductive hypothesis, S-INV holds. S-INV
guarantees that ci = '() and s = '(). After using this rule, ci = '(a) = a^i
and stack = '(a a) = a^k. A-INV holds, because ci = a^i AND stack = a^k.
Observe that i <= k <= 2i. Thus, A-INV holds. 

((A a ,EMP) (A (a))): By inductive hypothesis, A-INV holds. A-INV
guarantees that ci = a^i AND stack = a^k AND i <= k <= 2i. By reading
an a and pushing an a to the stack, we have ci = '(a^i a) = a^i+1 AND
stack = '(a^k a) = a^k+1. Observe that i+1 <= k+1 <= 2(i+1). 

((A a ,EMP) (A (a a))): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^i AND stack = a^k AND i <= k <= 2i. By reading
an a and pushing 2 as to the stack, we have ci = '(a^i a) AND stack = '(a^k a a)\
= a^k+2. Observe that i+1 <= k+2 <= 2(i+1).

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^i AND stack = a^k AND i <= k <= 2i. Observe that
ci = a^ib^0 = a^ib^j. Since i <= 0 + k <= 2i, we have that i <= j+k <= 2i.
Thus, B-INV holds.

((A b (a)) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that  ci = a^i = a^ib^0 AND stack = a^k AND i <= k <= 2i. By reading
a b and popping an a from the stack, we have ci = a^ib^1 = a^ib^j+1 AND
stack = a^k-1. Since i <= 1 + k-1 <= 2i, we have that i <= j+k <= 2i.
Thus, B-INV holds.

((B b (a)) (B ,EMP)): By inductive hypothesis, B-INV holds. B-INV guarentees that
ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i. By reading a b and popping an a
from the stack, we have ci = a^ib^j+1 AND stack = a^k-1. Since i <= j+1 + k-1 <= 2i,
we have that i <= j+k <= 2i. Thus B-INV holds. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. B-INV guarentees that
ci = a^ib^j AND stack = a^k AND i <= j+k <= 2i. Reading nothing and not changing the
stack means that ci = a^ib^j after the transition. Recall that M is nondetermintistic
and uses such transtion only to move to C (the final state) and accept. This means
that s must be empty and, therefore ci = a^ib^j, where i <= j <= 2i. Thus, C-INV holds. 



PROVING L = L(M)

w∈L ⇔ w∈L(M)

(⇒) Assume w∈L. This means that w = a^ib^j, where i <= j <= 2i. Given that state
invariantsalways hold, the following computation takes place:
(S a^ib^j EMP) -|ˆ∗ (A b^j a^j) -| (B b^k a^k) -|ˆ∗ (C EMP EMP)
Therefore, w∈L(M).

(⇐) Assume w∈L(M). This means that M halts in C, a final state,
with an empty stack having consumed w. Given that the state invariants
always hold, we may conclude that w = a^ib^j, where i <= j <= 2i. Therefore, w∈L


w∈/L ⇔ w∈/L(M)
Proof

(⇒) Assume w∈/L. This means w ≠ aibj, where i <= j <= 2i. Given that
the state invariant predicates always hold, there is no computation
that has M consume w and end in C with an empty stack. Therefore, w∈/L(M).

(⇐) Assume w∈/L(M). This means that M cannot transition into C with an
empty stack having consumed w. Given that the state invariants always hold,
this means that w ≠ aibj, where i <= j <= 2i. Thus, w∈/L.

|#




;; Let Σ = {a b}. Design and implement a pda for L = {a^nb^ma^n |n,m≥0}
;; States:
;;   S: ci = a* = a^n AND stack = a^n, start state
;;   A: ci = a^nb* = a^nb^m AND stack = a^n
;;   B: ci = a^nb^ma^k AND stack = a^n-k, final state
(define A^nB^mA^n (make-unchecked-ndpda '(S A B)
                              '(a b)
                              '(a)
                              'S
                              '(B)
                              `(((S a ,EMP) (S (a)))
                                ((S ,EMP ,EMP) (A ,EMP))
                                ((S b ,EMP) (A ,EMP))
                                ((A b ,EMP) (A ,EMP))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((A a (a)) (B ,EMP))
                                ((B a (a)) (B ,EMP)))))

;; tests for A^nB^mA^n
(check-equal? (sm-apply A^nB^mA^n '()) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a b a)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(b)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a a b b a a)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a b b b b b b b a)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(b b b b b b b b b)) 'accept)
(check-equal? (sm-apply A^nB^mA^n '(a a a a b b a a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a a a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(b a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a b)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a a a a a a a b a)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a b a b)) 'reject)
(check-equal? (sm-apply A^nB^mA^n '(a a b a a a)) 'reject)

;; invariants for A^nB^mA^n

;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in S
;;   S: ci = a* = a^n AND stack = a^n, start state
(define (S-INV-A^nB^mA^n ci stack)
  (and (andmap (λ (x) (eq? 'a x)) ci)
       (andmap (λ (x) (eq? 'a x)) stack)
       (= (length stack) (length ci))))

;; tests for S-INV-A^nB^mA^n
(check-equal? (S-INV-A^nB^mA^n '() '()) #t)
(check-equal? (S-INV-A^nB^mA^n '(a) '(a)) #t)
(check-equal? (S-INV-A^nB^mA^n '(a a a) '(a a a)) #t)
(check-equal? (S-INV-A^nB^mA^n '(a a a a) '(a a a a)) #t)
(check-equal? (S-INV-A^nB^mA^n '(a) '()) #f)
(check-equal? (S-INV-A^nB^mA^n '(b) '(a)) #f)
(check-equal? (S-INV-A^nB^mA^n '(a) '(a a)) #f)
(check-equal? (S-INV-A^nB^mA^n '(a b a) '()) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in A
;;   A: ci = a^nb* = a^nb^m AND stack = a^n
(define (A-INV-A^nB^mA^n ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (equal? As stack)
         (equal?
          (append As Bs) ci))))

;; tests for A-INV-A^nB^mA^n
(check-equal? (A-INV-A^nB^mA^n '(b) '()) #t)
(check-equal? (A-INV-A^nB^mA^n '(a) '(a)) #t)
(check-equal? (A-INV-A^nB^mA^n '(a b b) '(a)) #t)
(check-equal? (A-INV-A^nB^mA^n '(a a a b b b b b) '(a a a)) #t)
(check-equal? (A-INV-A^nB^mA^n '(b b b b b b b b) '()) #t)
(check-equal? (A-INV-A^nB^mA^n '(a a a a a a a b) '(a a a a a a a)) #t)
(check-equal? (A-INV-A^nB^mA^n '() '()) #t)
(check-equal? (A-INV-A^nB^mA^n '(a b a) '(a a)) #f)
(check-equal? (A-INV-A^nB^mA^n '(a b a b) '()) #f)
(check-equal? (A-INV-A^nB^mA^n '(a) '(a a)) #f)
(check-equal? (A-INV-A^nB^mA^n '(a b b a a a) '(a)) #f)
(check-equal? (A-INV-A^nB^mA^n '(a a b b b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: To determine if the given word and stack belongs in B
;;    B: ci = a^nb^ma^k AND stack = a^n-k
(define (B-INV-A^nB^mA^n ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (As-after-Bs (takef (drop ci (length (append As Bs))) (λ (x) (eq? x 'a))))
         (stack-all-as? (andmap (λ (x) (eq? 'a x)) stack))]
    (or (and (equal? ci As)
             (even? (length As))
             stack-all-as?) 
        (and (= (length stack) (- (length As) (length As-after-Bs)))
             stack-all-as?
             (equal? (append As Bs As-after-Bs) ci)))))

;; tests for B-INV-A^nB^mA^n
(check-equal? (B-INV-A^nB^mA^n '() '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(b) '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(a b a) '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(a a) '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(a a a a) '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(a a b b a a) '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(a a a a a b b b a a a a a) '()) #t)
(check-equal? (B-INV-A^nB^mA^n '(a) '()) #f)
(check-equal? (B-INV-A^nB^mA^n '(a a a) '()) #f)
(check-equal? (B-INV-A^nB^mA^n '(b b a) '()) #f)
(check-equal? (B-INV-A^nB^mA^n '(a a b) '()) #f)
(check-equal? (B-INV-A^nB^mA^n '(a a a a a) '()) #f)
(check-equal? (B-INV-A^nB^mA^n '(b a b a) '()) #f)


(define LOI-A^nB^mA^n (list (list 'S S-INV-A^nB^mA^n) (list 'A A-INV-A^nB^mA^n)
                            (list 'B B-INV-A^nB^mA^n)))


;; PROOF FOR A^nB^mA^n


;; PROOF

;; The state invariants hold when M accepts w
#|

Proof invariants hold after each transition that consumes input:

When M starts, S-INV holds because ci = '() and s = '().
This establishes the base case.

((S a ,EMP) (S (a))): By inductive hypothesis, S-INV holds.
S-INV guareentees that ci = a* = a^n AND stack = a^n. After
using this rule, ci = a* = a^n+1 AND stack = a^n+1. Observe that
ci = a* = a^n+1 AND stack = a^n+1. Thus, S-INV holds. 

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds.
S-INV guarentees that ci = a* = a^n AND stack = a^n. After using this
rule, ci = a* = a^n AND stack = a^n. A-INV holds since ci = a^nb^0
AND stack = a^n.

((S b ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. S-INV
guarentees that ci = a* = a^n AND stack = a^n. After using this rule,
ci = a^nb^1 and stack = a^n. Observe that ci = a^nb^1 = a^nb* = a^nb^m
and stack = a^n. Thus, A-INV holds. 

((A b ,EMP) (A ,EMP)): By inductive hypothesis A-INV holds. A-INV
guarentees that ci = a^nb* = a^nb^m AND stack = a^n. After using this rule,
ci = a^nb^m+1 and stack = a^n. Observe that we have ci = a^nb^m+1 =
a^nb* and stack = a^n. Thus, A-INV holds. 

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^nb* = a^nb^m and stack = a^n. After using this
rule, ci = a^nb^m AND stack = a^n. Observe that ci = a^nb^ma^0 = a^nb^ma^k
AND stack = a^n-k = a^n-0. Thus, B-INV holds. 

((A a (a)) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^nb* = a^nb^m AND stack = a^n. After using this
rule, ci = a^nb^ma^1 = a^nb^ma^k AND stack = a^n-k. Thus, B-INV holds. 

((B a (a)) (B ,EMP))))): By inductive hypothesis, B-INV holds. B-INV
guarentees that ci = a^nb^ma^k AND stack = a^n-k. After using this rule,
ci = a^nb^ma^k+1 AND stack = a^n-(k+1). Thus, B-INV holds. 


PROVING L = L(M)

w∈L ⇔ w∈L(M)

(⇒) Assume w∈L. This means that w = a^nb^ma^n, where n, m => 0.
Given that state invariants always hold, the following computation
takes place:
(S a^nb^ma^n '()) |-* (S b^ma^n a^n) |-* (A a^n a^n) |-* (B ,EMP ,EMP)
Therefore, w∈L(M).

(⇐) Assume w∈L(M). This means that M halts in B, a final state,
with an empty stack having consumed w. Given that the state invariants
always hold, we may conclude that w = a^nb^ma^n, where n, m => 0.
Therefore, w∈L


w∈/L ⇔ w∈/L(M)
Proof

(⇒) Assume w∈/L. This means w ≠ a^nb^ma^n, where n,m => 0. Given that
the state invariant predicates always hold, there is no computation
that has M consume w and end in B with an empty stack. Therefore, w∈/L(M).

(⇐) Assume w∈/L(M). This means that M cannot transition into B with an
empty stack having consumed w. Given that the state invariants always hold,
this means that w ≠ a^nb^ma^n, where n,m => 0. nThus, w∈/L.

|#



;; Let Σ = {a b c d}. Design and implement a pda for
;;   L={a^mb^nc^pd^q:m,n,p,q≥0 ∧ m+n=p+q}. Follow all the steps of
;;   the design recipe
;; States:
;;  S: ci = a* = a^m AND stack = a^m, start state
;;  A: ci = a^mb* = a^mb^n AND stack = a^m+n
;;  B: ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p
;;  C: ci = a^mb^nc^pd* = a^mb^nc^pd^q AND stack = a^m+n-p-q, final state
(define a^mb^nc^pd^q (make-unchecked-ndpda '(S A B C)
                                 '(a b c d)
                                 '(a)
                                 'S
                                 '(C)
                                 `(((S a ,EMP) (S (a)))
                                   ((S ,EMP ,EMP) (A ,EMP))
                                   ((A b ,EMP) (A (a)))
                                   ((A ,EMP ,EMP) (B ,EMP))
                                   ((B c (a)) (B ,EMP))
                                   ((B ,EMP ,EMP) (C ,EMP))
                                   ((C d (a)) (C ,EMP)))))

;; tests for a^mb^nc^pd^q
(check-equal? (sm-apply a^mb^nc^pd^q '()) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a b c d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(b b d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(b b c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a b c d d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a b b d d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(b b b c d d)) 'accept)
(check-equal? (sm-apply a^mb^nc^pd^q '(a)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(b)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(c)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(d)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a a c)) 'reject)
(check-equal? (sm-apply a^mb^nc^pd^q '(a a a a b b c d)) 'reject)

;; invariants for a^mb^nc^pd^q                                   

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
;;  (ci = a* = a^m AND stack = a^m)
(define (S-INV-a^mb^nc^pd^q ci stack)
  (and (andmap (λ (x) (eq? x 'a)) ci)
       (andmap (λ (x) (eq? x 'a)) stack)
       (equal? ci stack)))

;; tests for S-INV-a^mb^nc^pd^q
(check-equal? (S-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a) '(a a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a a a) '(a a a a a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a a a a a a) '(a a a a a a a a)) #t)
(check-equal? (S-INV-a^mb^nc^pd^q '(a) '()) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '() '(a)) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a) '(a)) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a) '(a a)) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a) '()) #f)
(check-equal? (S-INV-a^mb^nc^pd^q '(a a a a) '(a a a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
;;  (ci = a^mb* = a^mb^n AND stack = a^m+n)
(define (A-INV-a^mb^nc^pd^q ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))]
    (and (equal? (append As Bs) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= (length stack) (length ci)))))
  
  
;; tests for A-INV-a^mb^nc^pd^q
(check-equal? (A-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(b) '(a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a b) '(a a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a a a b b) '(a a a a a)) #t)
(check-equal? (A-INV-a^mb^nc^pd^q '(a) '()) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(b) '()) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(a a) '(a)) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(b b) '()) #f)
(check-equal? (A-INV-a^mb^nc^pd^q '(a b a) '(a a a)) #f)

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
;;  (ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p)
(define (B-INV-a^mb^nc^pd^q ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? x 'c))))]
    (and (equal? (append As Bs Cs) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= (length stack) (- (+ (length As) (length Bs)) (length Cs))))))
  
  
;; tests for B-INV-a^mb^nc^pd^q
(check-equal? (B-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(b) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a c) '()) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(b c) '()) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a b c) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(a b b) '(a a a)) #t)
(check-equal? (B-INV-a^mb^nc^pd^q '(c) '(a)) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(a d) '()) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(b d) '()) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(a c c) '()) #f)
(check-equal? (B-INV-a^mb^nc^pd^q '(b b c) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
;;  (ci = a^mb^nc^pd* = a^mb^nc^pd^q AND stack = a^m+n-p-q)
(define (C-INV-a^mb^nc^pd^q ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? x 'b))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? x 'c))))
         (Ds (takef (drop ci (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? x 'd))))]
    (and (equal? (append As Bs Cs Ds) ci)
         (andmap (λ (x) (eq? x 'a)) stack)
         (= (length stack) (- (+ (length As) (length Bs)) (length Cs) (length Ds))))))
  
  
;; tests for C-INV-a^mb^nc^pd^q
(check-equal? (C-INV-a^mb^nc^pd^q '() '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a c) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a d) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(b c) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(b d) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a) '(a)) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a a a d) '(a a)) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a a b c c d) '()) #t)
(check-equal? (C-INV-a^mb^nc^pd^q '(a) '(a a)) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(b b b) '(a a a a)) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(a b c c c) '()) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(a a a a d) '(a)) #f)
(check-equal? (C-INV-a^mb^nc^pd^q '(d c b a) '()) #f)


(define LOI-a^mb^nc^pd^q (list (list 'S S-INV-a^mb^nc^pd^q)
                               (list 'A A-INV-a^mb^nc^pd^q)
                               (list 'B B-INV-a^mb^nc^pd^q)
                               (list 'C C-INV-a^mb^nc^pd^q)))

;; proof for a^mb^nc^pd^q

;; PROOF

;; The state invariants hold when M accepts w
#|

Proof invariants hold after each transition:

When M starts, S-INV holds because ci = '() and s = '().
This establishes the base case.

((S a ,EMP) (S (a))): By inductive hypothesis, S-INV holds. S-INV
guarentees that ci = a* = a^m AND stack = a^m. After using this rule,
ci = a^m+1 = a* AND stack = a^m+1. Thus, S-INV holds.

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds. S-INV
guarentees that ci = a* = a^m AND stack = a^m. After using this rule,
ci = a^m = a^mb^0 = a^mb^n AND stack = a^m = a^m+n. Thus, A-INV holds.

((A b ,EMP) (A (a))): By inductive hypothesis, A-INV holds. A-inv
guarentees that ci = a^mb* = a^mb^n AND stack = a^m+n. After using
this rule, ci = a^mb^n+1 = a^mb* AND stack = a^m+n+1. Thus,
A-INV holds.

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds. A-INV
guarentees that ci = a^mb* = a^mb^n AND stack = a^m+n. After using this
rule, ci = a^mb^n = a^mb^nc^0 = a^mb^nc^p AND stack = a^m+n-p.
Thus, B-INV holds. 

((B c (a)) (B ,EMP)): By inductive hypothesis, B-INV holds. B-INV
guarentees that ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p. After
using this rule, ci = a^mb^nc^p+1 AND stack = a^m+n-(p+1). Thus, B-INV
holds. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. B-INV
guarentees that ci = a^mb^nc* = a^mb^nc^p AND stack = a^m+n-p. After using
this rule, ci = a^mb^nc^pd^0 = a^mb^nc^pd^q AND stack = a^m+n-p-q. Thus,
C-INV holds. 

((C d (a)) (C ,EMP)): By inductive hypothesis, C-INV holds. C-INV guarentees
that ci = a^mb^nc^pd* = a^mb^nc^pd^q AND stack = a^m+n-p-q. After using
this rule, ci = a^mb^nc^pd^q+1 AND stack = a^m+n-p-(q+1). Thus, C-INV holds. 

|#


#|
 word in language & word is in language of the machine

w∈L -> w∈L(M):

Assume that w∈L. This means that w = a^mb^nc^pd^q, where m+n = p+q.
Given that the state invariants always hold, the following computation
takes place:

(S a^mb^nc^pd^q '()) |-* (S b^nc^pd^q a^m) |- (A c^pd^q a^m+n)
|-* (B d^q a^m+n-p) |-* (C '() a^m+n-p-q)

Therefore, w∈L(M).

w∈L(M) -> w∈L:

Assume w∈L(M). This means that M halts in C, a final state, with an empty
stack and the word being entirely consumed. Given that the state invariants
always hold, we can conclude that w = a^mb^nc^pd^q, where m+n= p+q. Thus,
can can conclude w∈L.


|#







;; Let Σ={abc}. Design and implement a pda for
;; L = {a^mb^nc^p : m,n,p≥0 ∧ (m = n ∨ n = p)}
;; Follow all the steps of the design recipe

;; States:
;;  S: ci = '() AND stack = '(), start state
;;  A: ci = a^m AND stack = a^m
;;  B: ci = a^mb^n AND stack a^m-n
;;  C: ci = a^mb^nc^p AND stack = a^m-n, final state
;;  D: ci = a^m AND stack = '()
;;  E: ci = a^mb^n AND stack = a^n
;;  F: ci = a^mb^nc^p AND stack = a^n-p, final state
(define a^mb^nc^p (make-unchecked-ndpda '(S A B C D E F)
                              '(a b c)
                              '(a)
                              'S
                              '(C F)
                              `(((S ,EMP ,EMP) (A ,EMP))
                                ((S ,EMP ,EMP) (D ,EMP))
                                ((A a ,EMP) (A (a)))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((B b (a)) (B ,EMP))
                                ((B ,EMP ,EMP) (C ,EMP))
                                ((C c ,EMP) (C ,EMP))
                                ((D a ,EMP) (D ,EMP))
                                ((D ,EMP ,EMP) (E ,EMP))
                                ((E b ,EMP) (E (a)))
                                ((E ,EMP ,EMP) (F ,EMP))
                                ((F c (a)) (F ,EMP)))))

;; tests for a^mb^nc^p
(check-equal? (sm-apply a^mb^nc^p '()) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a b)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(b c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a b c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a a b b c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a b b c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a a a b b b)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(b b b c c c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(c)) 'accept)
(check-equal? (sm-apply a^mb^nc^p '(a c)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(b)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(b b b)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(a a b c c)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(a b b b c c)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(a a a a a a b)) 'reject)
(check-equal? (sm-apply a^mb^nc^p '(b b b b b b c)) 'reject)


;; invariants for a^mb^nc^p

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in S
;;  (ci = '() AND stack = '())
(define (S-INV-a^mb^nc^p ci stack)
  (and (empty? ci)
       (empty? stack)))

;; tests for S-INV-a^mb^nc^p
(check-equal? (S-INV-a^mb^nc^p '() '()) #t)
(check-equal? (S-INV-a^mb^nc^p '(a a) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(a a a) '(a)) #f)
(check-equal? (S-INV-a^mb^nc^p '(a b c) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(b) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(c) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(a b) '()) #f)
(check-equal? (S-INV-a^mb^nc^p '(b c) '()) #f)
 

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
;;  (ci = a^m AND stack = a^m)
(define (A-INV-a^mb^nc^p ci stack)
  (and (andmap (λ (x) (eq? x 'a)) ci)
       (andmap (λ (x) (eq? x 'a)) stack)
       (= (length ci) (length stack))))

;; tests for A-INV-a^mb^nc^p
(check-equal? (A-INV-a^mb^nc^p '() '()) #t)
(check-equal? (A-INV-a^mb^nc^p '(a) '(a)) #t)
(check-equal? (A-INV-a^mb^nc^p '(a a a a) '(a a a a)) #t)
(check-equal? (A-INV-a^mb^nc^p '(a a a a a) '(a a a a a)) #t)
(check-equal? (A-INV-a^mb^nc^p '(b) '()) #f)
(check-equal? (A-INV-a^mb^nc^p '(c) '()) #f)
(check-equal? (A-INV-a^mb^nc^p '(a b) '(a)) #f)
(check-equal? (A-INV-a^mb^nc^p '(a b b) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
;;  (ci = a^mb^n AND stack a^m-n)
(define (B-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))]
    (and (andmap (λ (x) (eq? x 'a)) stack)
         (equal? ci (append As Bs))
         (= (length stack) (- (length As) (length Bs))))))

;; tests for B-INV-a^mb^nc^p
(check-equal? (B-INV-a^mb^nc^p '() '()) #t)
(check-equal? (B-INV-a^mb^nc^p '(a) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^p '(a b) '()) #t)
(check-equal? (B-INV-a^mb^nc^p '(a a b) '(a)) #t)
(check-equal? (B-INV-a^mb^nc^p '(a a b b) '()) #t)
(check-equal? (B-INV-a^mb^nc^p '(b) '()) #f)
(check-equal? (B-INV-a^mb^nc^p '(a) '(a a)) #f)
(check-equal? (B-INV-a^mb^nc^p '(b) '(a)) #f)
(check-equal? (B-INV-a^mb^nc^p '(a a b b b) '()) #f)
(check-equal? (B-INV-a^mb^nc^p '(b b b a) '()) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
;;  (ci = a^mb^nc^p AND stack = a^m-n)
(define (C-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? 'c x))))]
    (and (andmap (λ (x) (eq? x 'a)) stack)
         (equal? ci (append As Bs Cs))
         (= (length stack) (- (length As) (length Bs))))))

;; tests for C-INV-a^mb^nc^p
(check-equal? (C-INV-a^mb^nc^p '() '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(c) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a b) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a b c) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a a b b c c) '()) #t)
(check-equal? (C-INV-a^mb^nc^p '(a c) '()) #f)
(check-equal? (C-INV-a^mb^nc^p '(b c) '()) #f)
(check-equal? (C-INV-a^mb^nc^p '(b a) '()) #f)
(check-equal? (C-INV-a^mb^nc^p '(a b) '(a)) #f)
(check-equal? (C-INV-a^mb^nc^p '(a a a a a b b b b b c c) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in D
;;  (ci = a^m AND stack = '())
(define (D-INV-a^mb^nc^p ci stack)
  (and (andmap (λ (x) (eq? x 'a)) ci)
       (empty? stack)))

;; tests for D-INV-a^mb^nc^p
(check-equal? (D-INV-a^mb^nc^p '() '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a) '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a a a a) '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a a a) '()) #t)
(check-equal? (D-INV-a^mb^nc^p '(a b c) '()) #f)
(check-equal? (D-INV-a^mb^nc^p '(a b) '()) #f)
(check-equal? (D-INV-a^mb^nc^p '(a a a a b) '()) #f)
(check-equal? (D-INV-a^mb^nc^p '(a) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
;;  (ci = a^mb^n AND stack = a^n)
(define (E-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))]
    (and (andmap (λ (x) (eq? x 'a)) stack)
         (equal? ci (append As Bs))
         (= (length stack) (length Bs)))))

;; tests for E-INV-a^mb^nc^p
(check-equal? (E-INV-a^mb^nc^p '() '()) #t)
(check-equal? (E-INV-a^mb^nc^p '(b) '(a)) #t)
(check-equal? (E-INV-a^mb^nc^p '(a b) '(a)) #t)
(check-equal? (E-INV-a^mb^nc^p '(b b b b) '(a a a a)) #t)
(check-equal? (E-INV-a^mb^nc^p '(a) '()) #t)
(check-equal? (E-INV-a^mb^nc^p '(a) '(a)) #f)
(check-equal? (E-INV-a^mb^nc^p '(b b) '(a)) #f)
(check-equal? (E-INV-a^mb^nc^p '(a b c) '()) #f)
(check-equal? (E-INV-a^mb^nc^p '(b b b c) '(a a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in E
;;  (ci = a^mb^nc^p AND stack = a^n-p)
(define (F-INV-a^mb^nc^p ci stack)
  (let* [(As (takef ci (λ (x) (eq? x 'a))))
         (Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x))))
         (Cs (takef (drop ci (+ (length As) (length Bs))) (λ (x) (eq? 'c x))))]
    (and (andmap (λ (x) (eq? x 'a)) stack)
         (equal? ci (append As Bs Cs))
         (= (length stack) (- (length Bs) (length Cs))))))

;; tests for F-INV-a^mb^nc^p
(check-equal? (F-INV-a^mb^nc^p '() '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(a) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(b c) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(b) '(a)) #t)
(check-equal? (F-INV-a^mb^nc^p '(a b c) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(a b b c c) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(a a a) '()) #t)
(check-equal? (F-INV-a^mb^nc^p '(b b b b c c) '(a a)) #t)
(check-equal? (F-INV-a^mb^nc^p '(a b c c) '()) #f)
(check-equal? (F-INV-a^mb^nc^p '(a a b b c) '()) #f)
(check-equal? (F-INV-a^mb^nc^p '(a a a b c) '(a)) #f)
(check-equal? (F-INV-a^mb^nc^p '(b b b b b c) '(a a a a a a a a)) #f)
(check-equal? (F-INV-a^mb^nc^p '(a b b b b) '()) #f)


(define LOI-a^mb^nc^p (list (list 'S S-INV-a^mb^nc^p)
                            (list 'A A-INV-a^mb^nc^p)
                            (list 'B B-INV-a^mb^nc^p)
                            (list 'C C-INV-a^mb^nc^p)
                            (list 'D D-INV-a^mb^nc^p)
                            (list 'E E-INV-a^mb^nc^p)
                            (list 'F F-INV-a^mb^nc^p)))
                            


;; PROOF




;; PROOF

;; The state invariants hold when M accepts w
#|

Proof invariants hold after each transition that consumes input:

When M starts, S-INV holds because ci = '() and s = '().
This establishes the base case.

((S ,EMP ,EMP) (A ,EMP)): By inductive hypothesis, S-INV holds.
S-INV guareentees that ci = '() AND stack = '(). After using this rule,
ci = '() AND stack = '(). Observe that ci = a^0 = a^m AND stack = a^m.
Thus, A-INV holds. 

((S ,EMP ,EMP) (D ,EMP)): By inductive hypothesis, S-INV holds.
S-INV guareentees that ci = '() AND stack = '(). After using this rule,
ci = '() AND stack = '(). Observe that ci = a^0 = a^m AND stack = '().
Thus, D-INV holds. 

((A a ,EMP) (A (a))): By inductive hypothesis, A-INV holds.
A-INV guareentees that ci = a^m AND stack = a^m. After using this
rule, ci = a^m+1 AND stack = a^m+1. Thus, A-INV holds.

((A ,EMP ,EMP) (B ,EMP)): By inductive hypothesis, A-INV holds.
A-INV guareentees that ci = a^m AND stack = a^m. After using this
rule, ci = a^m = a^mb^0 = a^mb^n AND stack = a^m = a^m-n.
Thus, B-INV holds. 

((B b (a)) (B ,EMP)): By inductive hypothesis, B-INV holds.
B-INV guareentees that ci = a^mb^n AND stack = a^m-n. After using
this rule, ci = a^mb^n+1 AND stack = a^m-(n+1). Thus, B-INV holds. 

((B ,EMP ,EMP) (C ,EMP)): By inductive hypothesis, B-INV holds. 
B-INV guareentees that ci = a^mb^n AND stack = a^m-n. After using
this rule, ci = a^mb^nc^0 = a^mb^nc^p AND stack = a^m-n. Thus,
C-INV holds. 

((C c ,EMP) (C ,EMP)): By inductive hypothesis, C-INV holds.
C-INV guareentees that ci = a^mb^nc^p AND stack = a^m-n. After
using this rule, ci = a^mb^nc^p+1 AND stack = a^m-n. Thus,
C-INV holds.

((D a ,EMP) (D ,EMP)): By inductive hypothesis, D-INV holds.
D-INV guareentees that ci = a^m AND stack = '(). After using this
rule, ci = a^m+1 AND stack = '(). Thus, D-INV holds. 

((D ,EMP ,EMP) (E ,EMP)): By inductive hypothesis, D-INV holds.
D-INV guareentees that ci = a^m AND stack = '(). After using this
rule, ci = a^mb^0 = a^mb^n AND stack = a^n. Thus, E-INV holds. 

((E b ,EMP) (E (a))): By inductive hypothesis, E-INV holds.
E-INV guareentees that ci = a^mb^n AND stack = a^n. After using
this rule, ci = a^mb^n+1 AND stack = a^n+1. Thus, E-INV holds.

((E ,EMP ,EMP) (F ,EMP)): By inductive hypothesis, E-INV holds.
E-INV guareentees that ci = a^mb^n AND stack = a^n. After using
this rule, ci = a^mb^nc^0 = a^mb^nc^p AND stack = a^n-p.
Thus F-INV holds.

((F c (a)) (F ,EMP)): BY inductive hypothesis, F-INV holds.
F-INV guareentees that ci = a^mb^nc^p AND stack = a^n-p.
After using this rule, ci= a^mb^nc^p+1 AND stack = a^n-(p+1).
Thus F-INV holds. 


PROVING L = L(M)

w∈L ⇔ w∈L(M)

(⇒) Assume w∈L. This means that w = a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p).
Given that state invariants always hold, this means that M consumes all its input in
either C or F. Since C and F are final states, w∈L(M).

(⇐) Assume w∈L(M). This means that M halts in C or F, final states,
with an empty stack having consumed w. Given that the state invariants
always hold, we may conclude that w = a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p).
Therefore, w∈L


w∈/L ⇔ w∈/L(M)
Proof

(⇒) Assume w∈/L. This means w ≠ a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p).
Given that the state invariant predicates always hold, there is no computation
that has M consume w and end in C or F with an empty stack. Therefore, w∈/L(M).

(⇐) Assume w∈/L(M). This means that M cannot transition into C or F with an
empty stack having consumed w. Given that the state invariants always hold,
this means that w ≠ a^mb^nc^p, where m,n,p≥0 ∧ (m = n ∨ n = p). Thus, w∈/L.

|#























;; a^mca^mcaba^nba^n, m, n >= 0

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
(define a^mca^mcaba^nba^n (make-unchecked-ndpda '(S A B C D E F G H I J)
                                      '(a b c)
                                      '(a b)
                                      'S
                                      '(J)
                                      `(((S ,EMP ,EMP) (A ,EMP))
                                        ((A a ,EMP) (A (a)))
                                        ((A c ,EMP) (B ,EMP))
                                        ((B ,EMP ,EMP) (C ,EMP))
                                        ((C a (a)) (C ,EMP))
                                        ((C c ,EMP) (D ,EMP))
                                        ((D a ,EMP) (E ,EMP))
                                        ((E b ,EMP) (F ,EMP))
                                        ((F ,EMP ,EMP) (G ,EMP))
                                        ((G a ,EMP) (G (b)))
                                        ((G b ,EMP) (H ,EMP))
                                        ((H ,EMP ,EMP) (I ,EMP))
                                        ((I a (b)) (I ,EMP))
                                        ((I ,EMP ,EMP) (J ,EMP))
                                        )))

 
;; tests for a^mca^mcaba^nba^n
;(check-accept? a^mca^mcaba^nba^n '(a c a c a b a b a) '(a a c a a c a b a b a) '(a a a c a a a c a b a b a) '(a a c a a c a b a a a a b a a a a))
;(check-reject? a^mca^mcaba^nba^n '(c c a b a b a) '(a a c a a c a b b) '(a a a a c a b a b a) '(a a a c a a c a b a a a a b a a a a))

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
  (or (and (empty? ci)
           (empty? stack))
      (let ([As (takef ci (λ (x) (eq? 'a x)))])
        (and (equal? As stack)
             (andmap (λ (x) (eq? 'a x)) ci)
             (andmap (λ (x) (eq? 'a x)) stack)))))

;; tests for a-inv
(check-equal? (A-INV-a^mca^mcaba^nba^n '() '()) #t)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a) '(a)) #t)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a a) '(a a a)) #t)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a b) '(a)) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a b a) '()) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a a a) '()) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a a a b b) '(a a)) #f)
(check-equal? (A-INV-a^mca^mcaba^nba^n '(a b b b a c a) '()) #f)

;; B: ci = a^mc AND stack = a^m

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
(define (B-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (drop ci (length As))])
         (and (equal? ci (append As C))
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


;; C: ci = a^mca^k AND stack = a^m-k

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in C
(define (C-INV-a^mca^mcaba^nba^n ci stack)
  (and (not (empty? ci))
       (let*  ([As (takef ci (λ (x) (eq? 'a x)))]
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))])
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))]
               [A (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second))) 1))])
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))]
               [A (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second))) 1))]
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))]
               [A (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second))) 1))]
               [B (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) 1))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second C-second A B As-third))
              (= (length stack)(length  As-third))
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (andmap (λ (x) (eq? 'b x)) B)
              (andmap (λ (x) (eq? 'b x)) stack)))))

;; tests for g-inv
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a c a c a b) '()) #t)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a c a a c a b a) '(b)) #t)
(check-equal? (G-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a) '(b b)) #t)
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))]
               [A (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second))) 1))]
               [B (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) 1))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))]
               [B-second (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third))) (λ (x) (eq? 'b x)))])
         (and (equal? ci (append As C As-second C-second A B As-third B-second))
              (= (length stack) (length As-third))
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (equal? B-second '(b))
              (andmap (λ (x) (eq? 'b x)) stack)))))

;; tests for h-inv
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a c a c a b a b) '(b)) #t)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b) '(b)) #t)
(check-equal? (H-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b) '(b b)) #t)
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))]
               [A (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second))) 1))]
               [B (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) 1))]
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
              (andmap (λ (x) (eq? 'b x)) stack)))))

;; tests for i-inv
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a c a c a b a b) '(b)) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a c a a c a b a b) '(b)) #t)
(check-equal? (I-INV-a^mca^mcaba^nba^n '(a a a a c a a a a c a b a a b a) '(b)) #t)
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
               [C (if (empty? (drop ci (length As)))
                      '()
                      (take (drop ci (length As)) 1))]
               [As-second (takef (drop ci (+ (length As) (length C))) (λ (x) (eq? 'a x)))]
               [C-second (if (empty? (drop ci (+ (length As) (length C) (length As-second))))
                             '()
                             (take (drop ci (+ (length As) (length C) (length As-second))) 1))]
               [A (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second))) 1))]
               [B (if (empty? (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))))
                      '()                      
                      (take (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A))) 1))]
               [As-third (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B))) (λ (x) (eq? 'a x)))]
               [B-second (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third))) (λ (x) (eq? 'b x)))]
               [As-fourth (takef (drop ci (+ (length As) (length C) (length As-second) (length C-second) (length A) (length B) (length As-third) (length B-second))) (λ (x) (eq? 'a x)))])
         (and (equal? ci (append As C As-second C-second A B As-third B-second As-fourth))
              (empty? stack)
              (equal? C '(c))
              (equal? C-second '(c))
              (equal? A '(a))
              (equal? B '(b))
              (equal? B-second '(b))))))

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




(define LOI-new (list (list 'S S-INV-a^mca^mcaba^nba^n)
                      (list 'A A-INV-a^mca^mcaba^nba^n)
                      (list 'B B-INV-a^mca^mcaba^nba^n)
                      (list 'C C-INV-a^mca^mcaba^nba^n)
                      (list 'D D-INV-a^mca^mcaba^nba^n)
                      (list 'E E-INV-a^mca^mcaba^nba^n)
                      (list 'F F-INV-a^mca^mcaba^nba^n)
                      (list 'G G-INV-a^mca^mcaba^nba^n)
                      (list 'H H-INV-a^mca^mcaba^nba^n)
                      (list 'I I-INV-a^mca^mcaba^nba^n)
                      (list 'J J-INV-a^mca^mcaba^nba^n)))






;; A: ci = (a^nb^n)*a^m AND stack = a^m
;; B: ci = (a^nb^n)*a^mb^l AND stack = a^m-l
;; C: ci = (a^nb^n)* AND stack = empty, final state
;; D: ci = (a^nb^n)*a^m AND stack = b^n
;; E: ci = (a^nb^n)*a^mb^l AND stack =b^m-l


;; state documentation
;; S: ci = (a^nb^n)* AND stack = empty, starting and final state
;; A: ci = (a^nb^n)*a^n AND stack = ba^n
;; B: ci = (a^nb^n)*a^mb^l AND stack = ba^n-l
(define a^nb^n* (make-unchecked-ndpda '(S A B)
                            '(a b)
                            '(a b)
                            'S
                            '(S)
                            `(((S ,EMP ,EMP) (A (b)))
                              ((A a ,EMP) (A (a)))
                              ((A ,EMP ,EMP) (B ,EMP))
                              ((B b (a)) (B ,EMP))
                              ((B ,EMP (b)) (S ,EMP)))))
;; tests for a^nb^n*
(check-equal? (sm-apply a^nb^n* '()) 'accept)
(check-equal? (sm-apply a^nb^n* '(a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a b a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a b a b a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a a b b a b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a a b b a a a b b b a b a a b b)) 'accept)
(check-equal? (sm-apply a^nb^n* '(a)) 'reject)
(check-equal? (sm-apply a^nb^n* '(b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a a)) 'reject)
(check-equal? (sm-apply a^nb^n* '(b b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a b b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a a b)) 'reject)
(check-equal? (sm-apply a^nb^n* '(a a a a a b b a a a b b b a b b b b)) 'reject)

;; invariants for a^nb^n*

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in s
(define (S-INV-a^nb^n* ci stack)
  (let* ([As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (if (and (empty? (drop ci (+ (length Bs) (length As))))
           (empty? stack))
      (= (length As) (length Bs))
      (and (= (length As) (length Bs))
           (empty? stack)
           (S-INV-a^nb^n* (drop ci (+ (length As) (length Bs))) stack)))))

;; tests for s-inv
(check-equal? (S-INV-a^nb^n* '(a b) '()) #t)
(check-equal? (S-INV-a^nb^n* '(a a b b) '()) #t)
(check-equal? (S-INV-a^nb^n* '(a a b b a b a b a b) '()) #t)
(check-equal? (S-INV-a^nb^n* '(a a a b b b a b a a b b) '()) #t)
(check-equal? (S-INV-a^nb^n* '(b) '(a)) #f)
(check-equal? (S-INV-a^nb^n* '(a a b b) '(a)) #f)
(check-equal? (S-INV-a^nb^n* '(a a) '(a)) #f)
       

;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in A
(define (A-INV-a^nb^n* ci stack)
  (let* ([B-stack (takef-right stack (λ (x) (eq? 'b x)))]
         [As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (if (empty? Bs)
      (equal? stack (append As B-stack))
      (and (= (length As) (length Bs))
           (A-INV-a^nb^n* (drop ci (+ (length As) (length Bs))) stack)))))

;; tests for a-inv
(check-equal? (A-INV-a^nb^n* '() '(b)) #t)
(check-equal? (A-INV-a^nb^n* '(a) '(a b)) #t)
(check-equal? (A-INV-a^nb^n* '(a a b b a) '(a b)) #t)
(check-equal? (A-INV-a^nb^n* '(a b a a) '(a a b)) #t)
(check-equal? (A-INV-a^nb^n* '(b) '(a)) #f)
(check-equal? (A-INV-a^nb^n* '(a a b) '(a)) #f)
(check-equal? (A-INV-a^nb^n* '(a a ) '(a)) #f)


;; word stack -> Boolean
;; Purpose: Determines if the given word and stack belongs in B
(define (B-INV-a^nb^n* ci stack)
  (let* ([B-stack (takef-right stack (λ (x) (eq? 'b x)))]
         [As (takef ci (λ (x) (eq? 'a x)))]
         [Bs (takef (drop ci (length As)) (λ (x) (eq? 'b x)))])
  (if (empty? (drop ci (+ (length Bs) (length As))))
      (= (length As) (+ (length Bs) (- (length stack) (length B-stack))))
      (and (= (length As) (length Bs))
           (B-INV-a^nb^n* (drop ci (+ (length As) (length Bs))) stack)))))

;; tests for b-inv
(check-equal? (B-INV-a^nb^n* '(a b) '(b)) #t)
(check-equal? (B-INV-a^nb^n* '(a a b b) '(b)) #t)
(check-equal? (B-INV-a^nb^n* '(a a b) '(a b)) #t)
(check-equal? (B-INV-a^nb^n* '(a a a b b b a a) '(a a b)) #t)
(check-equal? (B-INV-a^nb^n* '(b) '(a)) #f)
(check-equal? (B-INV-a^nb^n* '(a a b b) '(a)) #f)
(check-equal? (B-INV-a^nb^n* '(a a) '(a)) #f)







(define LOI-a^nb^n* (list (list 'S S-INV-a^nb^n*)
                          (list 'A A-INV-a^nb^n*)
                          (list 'B B-INV-a^nb^n*)
                          ))

















;;Let Σ = {a b}. Design and implement a pda for L = {w | w has 3
;; times as many as than b} 












