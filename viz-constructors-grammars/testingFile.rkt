#lang racket

(require "../fsm-core/interface.rkt"
         rackunit
         "../fsm-gui/interface.rkt"
         )
;; L = w | w has more as than bs
;; S:
;; the stack contains an arbitrary number of unmatched a's
;; the number of a's in ci >= number of b's in ci
;; the size of the stack is less than or equal to the number of a's in ci
;; the stack does not contain bs
;; A:
;; The stack contains an arbitrary number of unmatched a's
;; The number of a's in ci > number b's in ci
;; The size of the stack is less than or equal to the number of a's in ci
;; B:
;; The stack contains an arbitrary number of unmatched a's followed by 1
;; or more unmatched b's on the stack
;; The size of the stack is less than or equal to the size of c
(define numa>numb (make-ndpda '(S A B)
                              '(a b)
                              '(a b)
                              'S
                              '(A)
                              `(((S a ,EMP)  (A (a)))
                                ((S b ,EMP)  (B (b)))
                                ((A a ,EMP)  (A (a)))
                                ((A b (a a)) (A (a)))
                                ((A b (a))   (S ,EMP))
                                ((A ,EMP (a))   (A ()))
                                ((B b ,EMP)  (B (b)))
                                ((B a (b b)) (B (b)))
                                ((B a (b))   (S ,EMP)))))

(check-equal? (sm-apply numa>numb '()) 'reject)
(check-equal? (sm-apply numa>numb '(a b b)) 'reject)
(check-equal? (sm-apply numa>numb '(a b b a)) 'reject)
(check-equal? (sm-apply numa>numb '(a)) 'accept)
(check-equal? (sm-apply numa>numb '(b b a b a a a a)) 'accept)
(check-equal? (sm-apply numa>numb '(a a b b b a a)) 'accept)

;; word symbol --> (listof symbol)
;; Purpose: Extract the xs from the given word
(define (extract-xs w x) (filter (λ (s) (eq? s x)) w))

(check-equal? (extract-xs '(b b b) 'a) '())
(check-equal? (extract-xs '(b a a b b) 'b) '(b b b))
(check-equal? (extract-xs '(b a b a b b) 'a) '(a a))

;; word stack --> Boolean
;; S:
;; the stack contains an arbitrary number of unmatched a's
;; the number of a's in ci >= number of b's in ci
;; the size of the stack is less than or equal to the number of a's in ci
;; the stack does not contain bs
(define (S-INV-numa>numb ci stack)
  (and (andmap (λ (s) (eq? s 'a)) stack)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (and (>= (length as) (length bs))
              (<= (length stack) (length as))))))

(check-equal? (S-INV-numa>numb '() '(a a)) #f)
(check-equal? (S-INV-numa>numb '() '(b)) #f)
(check-equal? (S-INV-numa>numb '(a a b) '(a)) #t)
(check-equal? (S-INV-numa>numb '() '()) #t)
(check-equal? (S-INV-numa>numb '(a b b a) '()) #t)


;; word stack --> Boolean
;; A:
;; The stack contains an arbitrary number of unmatched a's
;; The number of a's in ci > number b's in ci
;; The size of the stack is less than or equal to the number of a's in ci
(define (A-INV-numa>numb ci stack)
  (and (andmap (λ (s) (eq? s 'a)) stack)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (and (> (- (length as) (length bs)) 0)
              (<= (length stack) (length as))))))

(check-equal? (A-INV-numa>numb '() '()) #f)
(check-equal? (A-INV-numa>numb '(a) '(a a a)) #f)
(check-equal? (A-INV-numa>numb '(a a b b a) '(a a)) #t)
(check-equal? (A-INV-numa>numb '(a b a) '(a)) #t)
(check-equal? (A-INV-numa>numb '(b a a b a a) '(a a)) #t)

;; word stack --> Boolean
;; B:
;; The stack contains an arbitrary number of unmatched a's followed by 1
;; or more unmatched b's on the stack
;; The size of the stack is less than or equal to the size of ci
(define (B-INV-numa>numb ci stack)
  (and (not (empty? stack))
       (<= (length stack) (length ci))
       (let* ((sbs (takef stack (λ (s) (eq? s 'b))))
              (sas (takef (drop stack (length sbs)) (λ (s) (eq? s 'a)))))
         (and (>= (length sbs) 1)
              (equal? stack (append sbs sas))))))

(check-equal? (B-INV-numa>numb '() '()) #f)
(check-equal? (B-INV-numa>numb '(a a a) '(b b b b)) #f)
(check-equal? (B-INV-numa>numb '(b b a b) '(b b)) #t)
(check-equal? (B-INV-numa>numb '(a a b b b b a b b) '(b b b)) #t)

;; L = {aˆnbˆn | n >= 0}
;; States
;; S ci = (listof a) = stack, start state
;; M ci = (append (listof a) (listof b)) AND
;; (length ci as) = (length stack) + (length ci bs)
;; F ci = (append (listof a) (listof b)) and all as and bs matched,
;; final state
;; The stack is a (listof a)
(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                            ((S a ,EMP) (S (a)))
                            ((M b (a)) (M ,EMP))
                            ((M ,EMP ,EMP) (F ,EMP)))))
;; Tests for aˆnbˆn
(check-equal? (sm-apply a^nb^n '(a)) 'reject)
(check-equal? (sm-apply a^nb^n '(b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a b b)) 'reject)
(check-equal? (sm-apply a^nb^n '(a b a a b b)) 'reject)
(check-equal? (sm-apply a^nb^n '()) 'accept)
(check-equal? (sm-apply a^nb^n '(a a b b)) 'accept)

;; word stack → Boolean
;; Purpose: Determine if the given ci and stack are the
;; same (listof a)
(define (S-INV-a^nb^n ci stck)
  (and (= (length ci) (length stck))
       (andmap (λ (i g) (and (eq? i 'a) (eq? g 'a))) ci stck)))
;; Tests for S-INV
(check-equal? (S-INV-a^nb^n '() '(a a)) #f)
(check-equal? (S-INV-a^nb^n '(a) '()) #f)
(check-equal? (S-INV-a^nb^n '(b b b) '(b b b)) #f)
(check-equal? (S-INV-a^nb^n '() '()) #t)
(check-equal? (S-INV-a^nb^n '(a a a) '(a a a)) #t)

;; word stack → Boolean
;; Purpose: Determine if ci = EMP or a+b+ AND the stack
;; only contains a AND |ci as| = |stack| + |ci bs|
(define (M-INV-a^nb^n ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (equal? (append as bs) ci)
         (andmap (λ (s) (eq? s 'a)) stck)
         (= (length as) (+ (length bs) (length stck))))))

;; Tests for M-INV
(check-equal? (M-INV-a^nb^n '(a a b) '(a a)) #f)
(check-equal? (M-INV-a^nb^n '(a) '()) #f)
(check-equal? (M-INV-a^nb^n '(a a a b) '(a a a)) #f)
(check-equal? (M-INV-a^nb^n '(a a a b) '(a)) #f)
(check-equal? (M-INV-a^nb^n '() '()) #t)
(check-equal? (M-INV-a^nb^n '(a) '(a)) #t)
(check-equal? (M-INV-a^nb^n '(a b) '()) #t)
(check-equal? (M-INV-a^nb^n '(a a a b b) '(a)) #t)

;; word stack → Boolean
;; Purpose: Determine if ci = a^nb^n and stack is empty
(define (F-INV-a^nb^n ci stck)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as))
                    (λ (s) (eq? s 'b))))]
    (and (empty? stck)
         (equal? (append as bs) ci)
         (= (length as) (length bs)))))
;; Tests for F-INV
(check-equal? (F-INV-a^nb^n '(a a b) '()) #f)
(check-equal? (F-INV-a^nb^n '(a) '()) #f)
(check-equal? (F-INV-a^nb^n '(a a a b) '(a a a)) #f)
(check-equal? (F-INV-a^nb^n '() '()) #t)
(check-equal? (F-INV-a^nb^n '(a b) '()) #t)
(check-equal? (F-INV-a^nb^n '(a a b b) '()) #t)

;; L = wcwˆR | w in (a b)*
;; States
;; S ci is empty and stack is empty
;; P ci = stackˆR AND c not in ci
;; Q ci = (append w (list c) v) AND
;; w = stackˆR vˆR
;; F stack = () AND ci = (append w (list c) wˆR)
(define wcw^r (make-ndpda '(S P Q F)
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
(check-equal? (sm-apply wcw^r '(a)) 'reject)
(check-equal? (sm-apply wcw^r '(a c)) 'reject)
(check-equal? (sm-apply wcw^r '(b c a)) 'reject)
(check-equal? (sm-apply wcw^r '(a a b c b a b)) 'reject)
(check-equal? (sm-apply wcw^r '(c)) 'accept)
(check-equal? (sm-apply wcw^r '(a c a)) 'accept)
(check-equal? (sm-apply wcw^r '(a b b b c b b b a)) 'accept)

;; word stack → Boolean
;; Purpose: Determine in the given word and stack are empty
(define (S-INV-wcw^r ci s) (and (empty? ci) (empty? s)))
;; Tests for S-INV
(check-equal? (S-INV-wcw^r '() '(a a)) #f)
(check-equal? (S-INV-wcw^r '(a c a) '()) #f)
(check-equal? (S-INV-wcw^r '(a c a) '(b b)) #f)
(check-equal? (S-INV-wcw^r '() '()) #t)

;; word stack → Boolean
;; Purpose: Determine if the given ci is the reverse of
;; the given stack AND c is not in ci
(define (P-INV-wcw^r ci s)
  (and (equal? ci (reverse s)) (not (member 'c ci))))
;; Tests for P-INV
(check-equal? (P-INV-wcw^r '(a c a) '(a c a)) #f)
(check-equal? (P-INV-wcw^r '(a a) '(a b)) #f)
(check-equal? (P-INV-wcw^r '() '()) #t)
(check-equal? (P-INV-wcw^r '(a b) '(b a)) #t)
(check-equal? (P-INV-wcw^r '(a b a a) '(a a b a)) #t)

;; word stack → Boolean
;; Purpose: Determine if ci=s^Rv^Rcv
(define (Q-INV-wcw^r ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))
         (v (if (member 'c ci)
                (drop ci (add1 (length w)))
                '()))]
    (and (equal? ci (append w (list 'c) v))
         (equal? w (append (reverse s) (reverse v))))))
;; Tests for Q-INV
(check-equal? (Q-INV-wcw^r '(a a) '()) #f)
(check-equal? (Q-INV-wcw^r '(b b c a) '(b a)) #f)
(check-equal? (Q-INV-wcw^r '(c) '()) #t)
(check-equal? (Q-INV-wcw^r '(b a c) '(a b)) #t)
(check-equal? (Q-INV-wcw^r '(a b c b) '(a)) #t)
(check-equal? (Q-INV-wcw^r '(a b b c b) '(b a)) #t)

;; word stack → Boolean
;; Purpose: Determine if ci=s^Rv^Rcv AND stack is empty
(define (F-INV-wcw^r ci s)
  (let* [(w (takef ci (λ (s) (not (eq? s 'c)))))]
    (and (empty? s)
         (equal? ci (append w (list 'c) (reverse w))))))
;; Tests for F-INV
(check-equal? (F-INV-wcw^r '() '()) #f)
(check-equal? (F-INV-wcw^r '(b b) '()) #f)
(check-equal? (F-INV-wcw^r '(b a c) '(b a)) #f)
(check-equal? (F-INV-wcw^r '(c) '()) #t)
(check-equal? (F-INV-wcw^r '(b a c a b) '()) #t)
(check-equal? (F-INV-wcw^r '(a b b c b b a) '()) #t)

#;(sm-visualize numa>numb
              (list 'S S-INV-numa>numb) (list 'A A-INV-numa>numb) (list 'B B-INV-numa>numb)
              )
#;(sm-visualize a^nb^n
              (list 'S S-INV-a^nb^n) (list 'M M-INV-a^nb^n) (list 'F F-INV-a^nb^n)
              )
(sm-visualize wcw^r
              (list 'S S-INV-wcw^r) (list 'P P-INV-wcw^r) (list 'Q Q-INV-wcw^r) (list 'F F-INV-wcw^r)
              )