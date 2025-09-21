#lang fsm

;(define ADD-CSG (make-csg '(S I J K L M N)
;                          '(b i j)
;                          `((S ,ARROW Ib)
;                            (I ,ARROW iIi)
;                            (I ,ARROW bJb)
;                            (J ,ARROW jJ)
;                            (J ,ARROW jJj)
;                            (J ,ARROW K)
;                            (jK ,ARROW Kj)
;                            (bKj ,ARROW biL)
;                            (Lj ,ARROW jL)
;                            (Lb ,ARROW bL)
;                            (Li ,ARROW iL)
;                            (Lb ,ARROW Mib)
;                            (iM ,ARROW Mi)
;                            (ibM ,ARROW ib)
;                            (bM ,ARROW Mb)
;                            (jM ,ARROW Nj)
;                            (jN ,ARROW Nj)
;                            (iNj ,ARROW iiL))
;                          'S))

#|
L = {AbBbAB | A,B in i*}
Syntactic Categories
   S: generates words in i^nbi^mbi^n+m
   A: generates an i for an argument and a promise, I,  
      to generate a matching i for the result
  IE: generates i for the result

Invariant:
  A in  yield ==> i,I,A in first num or i,I,A in second num
  A nin yield AND E in  yield ==> |i| before 2nd b = |i| after second b + |I| before second b
  E nin yield ==> |i| before 2nd b = |i| after second b
|#
(define ADD-CSG2 (make-csg '(S A E I)
                           '(b i)
                           `((S ,ARROW AbAbE)
                             (A ,ARROW ,EMP)
                             (A ,ARROW iIA)
                             (Ii ,ARROW iI)
                             (Ib ,ARROW bI)
                             (IE ,ARROW Ei)
                             (E ,ARROW ,EMP))
                           'S))

;; Tests
(check-derive? ADD-CSG2 '(b b) '(b i b i) '(i b b i)
               '(i i b i b i i i)
               '(i i b i i b i i i i)
               #;'(i i b i i i b i i i i i))


;(grammar-viz ADD-CSG2 '(i i b i b i i i))

(define (get-A yield)
  (takef yield (λ (s) (not (eq? s 'b)))))

(define (get-B yield)
  (get-A (drop yield (add1 (length (get-A yield))))))

(define (get-res yield)
  (if (member 'E yield)
      (drop yield (+ (length (get-A yield)) (length (get-B yield)) 3))
      (drop yield (+ (length (get-A yield)) (length (get-B yield)) 2))))

(define (number-i-before-2ndb yield)
  (count (λ (s) (eq? s 'i)) (append (get-A yield) (get-B yield))))

(define (number-i-after-2ndb yield)
  (length (get-res yield)))

(define (number-I-before-2ndb yield)
  (count (λ (s) (eq? s 'I)) (append (get-A yield) (get-B yield))))


(define (ADD-CSG2-INV yield)
  (let [(yield (filter (λ (s) (not (eq? s EMP))) yield))]
    (or (empty? yield)
        (and (implies (member 'A yield)
                      (and (andmap (λ (s) (or (eq? s 'i) (eq? s 'I) (eq? s 'A)))
                                   (get-A yield))
                           (andmap (λ (s) (or (eq? s 'i) (eq? s 'I) (eq? s 'A)))
                                   (get-B yield))))
             (implies (and (not (member 'A yield)) (member 'E yield))
                      (= (number-i-before-2ndb yield)
                         (+ (number-i-after-2ndb yield)
                            (number-I-before-2ndb yield))))
             (implies (not (member 'E yield))
                      (= (number-i-before-2ndb yield)
                         (number-i-after-2ndb yield)))))))

;(grammar-viz ADD-CSG2 '(i i b i b i i i) ADD-CSG2-INV)


