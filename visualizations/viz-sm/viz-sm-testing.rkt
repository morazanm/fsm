#lang racket

(require "old-viz-pda.rkt"
         "viz-ndfa.rkt"
         "../../fsm-core/interface.rkt")


;; L = {w | EMP U aa* U ab*}
;; State Documentation (ci = consumed input)
;; S: ci is empty, starting state
;; A: ci = aa*, final state
;; B: ci = ab*, final state
;; F: ci = EMP, final state
(define emp-aa-ab (make-ndfa '(S A B F)
                             '(a b)
                             'S
                             '(A B F)
                             `((S a A) (S a B) (S ,EMP F)
                                       (A a A)
                                       (B b B))))
;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (S-INV a-word)
  (empty? a-word))

;; word -> Boolean
;; Purpose: To determine whether ci = aa*
(define (A-INV ci)
  (and (not (empty? ci))
       (andmap (λ (w) (eq? w 'a)) ci)))

;; word -> Boolean
;; Purpose: To determine whether ci = ab*
(define (B-INV ci)
  (and (not (empty? ci))
       (eq? (first ci) 'a)
       (andmap (λ (w) (eq? w 'b)) (rest ci))))

;;word -> Boolean
;;Purpose: To determine whether ci = emp
(define (F-INV ci)
  (empty? ci))

;(ndfa-viz emp-aa-ab '(a a a a a a a))
#;(ndfa-viz emp-aa-ab '(a a a a a a a)
            (list 'S S-INV)
            (list 'A A-INV)
            (list 'B B-INV)
            (list 'F F-INV)
            #:add-dead #t)

;; L = {a^ib^j | i ≤ j ≤ 2i}
;; State Documentation (ci = consumed input)
;; S: ci = a* AND stack = b^(2*|ci|)
;; H: ci = a^ib^j AND stack = b* AND i <= j+|stack| <= 2i		
(define a^ib^j-student
  (make-ndpda '(S H)
              '(a b)
              '(b)
              'S
              '(H)
              `(((S ,EMP ,EMP) (H ,EMP))
                ((S a ,EMP) (S (b b)))
                ((H b (b b)) (H ,EMP))
                ((H ,EMP (b)) (H ,EMP)))))

;; ci stack -> boolean
;; Purpose: Determines if S's role holds
(define (PDA-S-INV ci stack)
  (and (andmap (λ (w) (eq? w 'b)) stack)
       (andmap (λ (w) (eq? w 'a)) ci)
       (= (* 2 (length ci)) (length stack))))


;; ci stack -> boolean
;; Purpose: Determines if H's role holds
(define (PDA-H-INV ci stack)
  (let* [(as (takef ci (λ (s) (eq? s 'a))))
         (bs (takef (drop ci (length as)) (λ (s) (eq? s 'b))))]
    (and (equal? ci (append as bs))
         (andmap (λ (s) (eq? s 'b)) stack)
         (<= (length as) (+ (length bs) (length stack))
             (* 2 (length as))))))


;; L = {w | |b∈w| > |a∈w|}
;; Syntactic Categories
;; S: numb b's > numb a's
;; A: numb b's >= numb a's
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(define pd-numb>numa (grammar->sm numb>numa))

#;(pda-viz a^ib^j-student '(a a a b b) (list 'S PDA-S-INV)
           (list 'H PDA-H-INV))
;(pda-viz pd-numb>numa '(a b) #:max-cmps 5)