#lang racket

(require "sm-viz.rkt"
         "viz-ndfa.rkt"
         "old-viz-pda.rkt"
         "../../fsm-core/interface.rkt")


(define DNA-SEQUENCE (make-dfa '(K H F M I D B S R) ;C)
                               '(a t c g)
                               'K
                               '(K F I B R)
                               `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                         (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                         (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                         (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (DNA-K-INV a-word)
  (empty? a-word))

;;word -> boolean
;;Purpose: Determines if the given word has more a's than t's
(define (DNA-H-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of a's and t's
(define (DNA-F-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more t's than a's
(define (DNA-M-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-t num-a)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of t's and a's
(define (DNA-I-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more c's than g's
(define (DNA-D-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-c num-g)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of c's and g's
(define (DNA-B-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has more g's than c's
(define (DNA-S-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of g's and c's
(define (DNA-R-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

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
                             `((S a A) (S a B) (S ,EMP F) (A a A) (B b B))))
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

;(sm-viz emp-aa-ab '(a b b b b a b b))
;(sm-viz emp-aa-ab '(a b b b b a b b) #:add-dead #t)
#;(sm-viz emp-aa-ab '(a a a a a a a)
            (list 'S S-INV)
            (list 'A A-INV)
            (list 'B B-INV)
            (list 'F F-INV))

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
              `(((S ,EMP ,EMP) (H ,EMP)) ((S a ,EMP) (S (b b)))
                ((H b (b b)) (H ,EMP)) ((H ,EMP (b)) (H ,EMP)))))

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

#;(sm-viz a^ib^j-student '(a a a b b) (list 'S PDA-S-INV)
           (list 'H PDA-H-INV))
;(sm-viz pd-numb>numa '(a b) #:max-cmps 5)