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
(define (S-INV ci stack)
  (and (andmap (λ (s) (eq? s 'a)) stack)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (and (>= (length as) (length bs))
              (<= (length stack) (length as))))))

(check-equal? (S-INV '() '(a a)) #f)
(check-equal? (S-INV '() '(b)) #f)
(check-equal? (S-INV '(a a b) '(a)) #t)
(check-equal? (S-INV '() '()) #t)
(check-equal? (S-INV '(a b b a) '()) #t)


;; word stack --> Boolean
;; A:
;; The stack contains an arbitrary number of unmatched a's
;; The number of a's in ci > number b's in ci
;; The size of the stack is less than or equal to the number of a's in ci
(define (A-INV ci stack)
  (and (andmap (λ (s) (eq? s 'a)) stack)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (and (> (- (length as) (length bs)) 0)
              (<= (length stack) (length as))))))

(check-equal? (A-INV '() '()) #f)
(check-equal? (A-INV '(a) '(a a a)) #f)
(check-equal? (A-INV '(a a b b a) '(a a)) #t)
(check-equal? (A-INV '(a b a) '(a)) #t)
(check-equal? (A-INV '(b a a b a a) '(a a)) #t)

;; word stack --> Boolean
;; B:
;; The stack contains an arbitrary number of unmatched a's followed by 1
;; or more unmatched b's on the stack
;; The size of the stack is less than or equal to the size of ci
(define (B-INV ci stack)
  (and (not (empty? stack))
       (<= (length stack) (length ci))
       (let* ((sbs (takef stack (λ (s) (eq? s 'b))))
              (sas (takef (drop stack (length sbs)) (λ (s) (eq? s 'a)))))
         (and (>= (length sbs) 1)
              (equal? stack (append sbs sas))))))

(check-equal? (B-INV '() '()) #f)
(check-equal? (B-INV '(a a a) '(b b b b)) #f)
(check-equal? (B-INV '(b b a b) '(b b)) #t)
(check-equal? (B-INV '(a a b b b b a b b) '(b b b)) #t)

(sm-visualize numa>numb
              (list 'S S-INV) (list 'A A-INV) (list 'B B-INV)
              )
#;'(
    ((S (a a a) ()) ())
    ((A (a a) (a)) ((S a ε) (A (a))))
    ((A (a) (a a)) ((A a ε) (A (a))))
    ((A () (a a a)) ((A a ε) (A (a))))
    ((A () (a a)) ((A ε (a)) (A ε)))
    ((A () (a)) ((A ε (a)) (A ε)))
    ((A () ()) ((A ε (a)) (A ε)))
    )