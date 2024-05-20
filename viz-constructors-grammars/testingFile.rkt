#lang racket

(require "../fsm-core/interface.rkt"
         rackunit
         "../fsm-gui/interface.rkt"
         )
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
                                ((A ,EMP (a))   (A ,EMP))
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

;; S = ci = {a b}* AND numa(ci) = numb(ci) AND stack = ε, start
;; word stack --> Boolean
(define (S-INV ci stack)
  (and (andmap (λ (s) (or (eq? s 'a) (eq? s 'b))) ci)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (= (length as) (length bs)))
       (empty? stack)))

(check-equal? (S-INV '() '(a a)) #f)
(check-equal? (S-INV '() '(b)) #f)
(check-equal? (S-INV '(a a b) '(a)) #f)
(check-equal? (S-INV '() '()) #t)
(check-equal? (S-INV '(a b b a) '()) #t)


;; A = ci ={a b}* AND numa(ci) > numb(ci) AND stack = unmatched as in ci, final
;; word stack --> Boolean
(define (A-INV ci stack)
  (and (andmap (λ (s) (or (eq? s 'a) (eq? s 'b))) ci)
       (andmap (λ (s) (eq? s 'a)) stack)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (and (> (- (length as) (length bs)) 0)
              (= (length stack) (- (length as) (length bs)))))))

(check-equal? (A-INV '() '()) #f)
(check-equal? (A-INV '(a a) '(a)) #f)
(check-equal? (A-INV '(a b a) '(a)) #t)
(check-equal? (A-INV '(b a a b a a) '(a a)) #t)

;; B = ci ={a b}* AND numb(ci) > numa(ci) AND stack = b+ AND AND stack = unmatched bs in ci
;; word stack --> Boolean
(define (B-INV ci stack)
  (and (andmap (λ (s) (or (eq? s 'a) (eq? s 'b))) ci)
       (not (empty? stack))
       (andmap (λ (s) (eq? s 'b)) stack)
       (let ((as (extract-xs ci 'a))
             (bs (extract-xs ci 'b)))
         (and (> (- (length bs) (length as)) 0)
              (= (length stack) (- (length bs) (length as)))))))

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