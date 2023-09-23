#lang fsm

(require "constructors.rkt")

#|
STATE DOCUMENTATION (i is head's position, w is the input word, and xs is all x on tape)
  S: i = 1 AND tape[i] = BLANK
  A: i > 2 AND tape[2..i-1] = a^+
  B: i > 3 AND tape[2..i-1] = a^+b^+
  C: i > 4 AND tape[2..i-1] = a^+b^+c^+
  D: i >= 1 AND w = x^na*x^nb*x^nc*
  E: i > 1 AND w = x^naa*x^nbb*x^ncc*
  F: i > 1 AND w = x^n+1aa*x^nbbb*x^nccc*
  G: i > 3 AND w = x^n+1aa*x^n+1bb*x^nccc*
  H: i > 1 AND w = x*bx*c and |xs|%3 = 1 and |xs1b| = 2|xs2c|, where xs1b = x*b, xs2c = x*c. and w = (append xs1b xs2c)
  I: i > 2 AND w = x*c and |xs|%3 = 2
  J: i = 2 AND tape[i-1] = BLANK
  K: i > 3 AND w = xxxx^* and |xs|%3 = 0 and tape[i] = x and i = |w| + 1    ;;;
  L: i > 4 AND w = xxxx^* and |xs|%3 = 0 and i = |w| + 2    ;;;
  Y: i >= 1 AND w = x* and |xs|%3 = 0 and tape[i] = BLANK ;;;
|#

;; L = a^n b^n c^n
;; PRE: tape = `(,LM ,BLANK w) ∧ i = 1, where w∈{a b c}∗
;; Σ = {a b c x}
(define anbncn (make-tm2 '(S A B C D E F G H I J K L Y)
                         '(a b c x)
                         `(((S ,BLANK) (J ,RIGHT))
                           ((J ,BLANK) (Y ,BLANK))
                           ((J d) (A OVER))
                           ((A a) (A ,RIGHT))
                           ((A b) (B ,RIGHT))
                           ((B b) (B ,RIGHT))
                           ((B c) (C ,RIGHT))
                           ((C c) (C ,RIGHT))
                           ((C ,BLANK) (D ,LEFT))
                           ((D a) (D ,LEFT))
                           ((D b) (D ,LEFT))
                           ((D c) (D ,LEFT))
                           ((D x) (D ,LEFT))
                           ((D ,BLANK) (E ,RIGHT))
                           ((E x) (E ,RIGHT))
                           ((E a) (F x))
                           ((E a) (H x))
                           ((F a) (F ,RIGHT))
                           ((F b) (G x))
                           ((F x) (F ,RIGHT))
                           ((G b) (G ,RIGHT))
                           ((G x) (G ,RIGHT))
                           ((G c) (D x))
                           ((H x) (H ,RIGHT))
                           ((H b) (I x))
                           ((I x) (I ,RIGHT))
                           ((I c) (K x))            
                           ((K x) (L ,RIGHT))  
                           ((L ,BLANK) (Y ,BLANK)))     
                         'S
                         '(Y)
                         'Y))

(check-equal? (sm-apply anbncn `(,LM ,BLANK a a) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK b b b) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK b a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a c b) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b b b c c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a b c c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b b c c a b c) 1) 'reject)
(check-equal? (sm-apply anbncn `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a b c) 1) 'accept)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a b b c c) 1) 'accept)
(check-equal? (sm-apply anbncn `(,LM ,BLANK a a a b b b c c c) 1) 'accept)

;; word symbol --> word
;; Purpose: Return the subword at the front of the given word that only contains the given symbol
(define (front-symbs w s)
  (takef w (λ (a) (eq? a s))))

(check-equal? (front-symbs '(a a a c b) 'c) '())
(check-equal? (front-symbs '(a a a c b) 'a) '(a a a))

;; tape natnum --> Boolean
;; Purpose: Determine head in position 1 and tape[i]=BLANK
(define (S-INV t i)
  (and (= i 1) (eq? (list-ref t i) BLANK)))

(check-equal? (S-INV `(,LM ,BLANK  a b c) 0) #f)
(check-equal? (S-INV `(,LM a b c c) 1) #f)
(check-equal? (S-INV `(,LM ,BLANK  a b c) 1) #t)
(check-equal? (S-INV `(,LM ,BLANK  a a b b c) 1) #t)

;; tape natnum --> Boolean
;; Purpose: Determine that head's position is 2 and tape[1] = BLANK
(define (J-INV tape i)
  (and (= i 2) (eq? (list-ref tape (sub1 i)) BLANK)))

(check-equal? (J-INV `(,LM ,BLANK a b c) 1) #f)
(check-equal? (J-INV `(,LM a a a) 1) #f)
(check-equal? (J-INV `(,LM ,BLANK a b c) 2) #t)
(check-equal? (J-INV `(,LM ,BLANK b b b) 2) #t)

;; tape natnum --> Boolean
;; Purpose: Determine head in position > 1 and tape[2..i-1]=a*
(define (A-INV t i)
  (and (> i 2)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))]
         (equal? as w))))

(check-equal? (A-INV `(,LM ,BLANK  a b c) 0) #f)
(check-equal? (A-INV `(,LM ,BLANK ,BLANK ,BLANK) 2) #f)
(check-equal? (A-INV `(,LM ,BLANK  a b a b c) 4) #f)
(check-equal? (A-INV `(,LM ,BLANK  a b c) 3) #t)
(check-equal? (A-INV `(,LM ,BLANK  a a a b b c) 5) #t)

;; tape natnum --> Boolean
;; Purpose: Determine head in position > 2 and tape[2..i-1]=a^+b^+
(define (B-INV t i)
  (and (> i 3)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))
              (w-as (drop w (length as)))
              (bs (front-symbs w-as 'b))]
         (and (equal? w (append as bs))
              (not (empty? as))
              (not (empty? bs))))))

(check-equal? (B-INV `(,LM ,BLANK  a b c) 0) #f)
(check-equal? (B-INV `(,LM ,BLANK  a b b c c) 6) #f)
(check-equal? (B-INV `(,LM ,BLANK  a a b b c c) 6) #t)
(check-equal? (B-INV `(,LM ,BLANK  a b b c) 5) #t)
(check-equal? (B-INV `(,LM ,BLANK  a a b b b c c) 7) #t)

;; tape natnum --> Boolean
;; Purpose: Determine head in position > 3 and tape[2..i-1]=a^+b^+c^+
(define (C-INV t i)
  (and (> i 4)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs  w 'a))
              (w-as (drop w (length as)))
              (bs (front-symbs  w-as 'b))
              (w-asbs (drop w-as (length bs)))
              (cs (front-symbs w-asbs 'c))]
         (and (equal? w (append as bs cs))
              (not (empty? as))
              (not (empty? bs))
              (not (empty? cs))))))

(check-equal? (C-INV `(,LM ,BLANK  a b b c c) 5) #f)
(check-equal? (C-INV `(,LM ,BLANK  a b b) 4) #f)
(check-equal? (C-INV `(,LM ,BLANK  a b b c c) 6) #t)
(check-equal? (C-INV `(,LM ,BLANK  a b c ,BLANK) 5) #t)

;; tape natnum --> Boolean
;; Purpose: Determine that head position is >= 1 and that input word =x^na^+x^nb^+x^nc^+
(define (D-INV t i)
  (and (>= i 1)
       (let* [(w (takef (drop t 2) (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 0)
              (= (length xs1) (length xs2) (length xs3))))))

(check-equal? (D-INV `(,LM ,BLANK b b b c c) 4) #f)
(check-equal? (D-INV `(,LM ,BLANK a b b) 2) #f)
(check-equal? (D-INV `(,LM ,BLANK a b c) 1) #t)
(check-equal? (D-INV `(,LM ,BLANK a a b b c c) 7) #t)

;; tape natnum --> Boolean
;; Purpose: Determine head in position > 1 and tape=x^na^+x^nb^+x^nc^+
(define (E-INV t i)
  (and (> i 1)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 0)
              (= (length xs1) (length xs2) (length xs3))))))

(check-equal? (E-INV `(,LM ,BLANK x a a x b b x c c ,BLANK) 1) #f)
(check-equal? (E-INV `(,LM ,BLANK x a a x b c c ,BLANK) 2) #f)
(check-equal? (E-INV `(,LM ,BLANK x a a x b b x c c ,BLANK) 4) #t)
(check-equal? (E-INV `(,LM ,BLANK a a b b c c ,BLANK) 3) #t)

;; tape natnum --> Boolean
;; Purpose: Determine head in position > 1 and tape=x^n+1a^+x^nbb^+*x^ncc^+
(define (F-INV t i)
  (and (> i 1) ;; is 2 when first a is replaced
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 1)
              (> (length cs) 1)
              (= (sub1 (length xs1)) (length xs2) (length xs3))))))

(check-equal? (F-INV `(,LM ,BLANK x a a x b b c c c ,BLANK) 3) #f)
(check-equal? (F-INV `(,LM ,BLANK x a b b c c ,BLANK) 1) #f)
(check-equal? (F-INV `(,LM ,BLANK x a a b b b c c c ,BLANK) 5) #t)
(check-equal? (F-INV `(,LM ,BLANK x a b b c c ,BLANK) 3) #t)

;; tape natnum --> Boolean
;; Purpose: Determine head in position > 3 and tape=x^n+1a^+x^n+1b^+x^ncc^+
(define (G-INV t i)
  (and (> i 3)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 1)
              (= (sub1 (length xs1)) (sub1 (length xs2)) (length xs3))))))

(check-equal? (G-INV `(,LM ,BLANK x a a x x b c c c ,BLANK) 3) #f)
(check-equal? (G-INV `(,LM ,BLANK x a a x b b x c c ,BLANK) 5) #f)
(check-equal? (G-INV `(,LM ,BLANK x a a x b b c c c ,BLANK) 5) #t)
(check-equal? (G-INV `(,LM ,BLANK x x a x x b x c c ,BLANK) 8) #t)

;; tape natnum --> Boolean
;; Purpose: Determine input word = x^+bx^+c and |xs|%3 = 1 and |x^+b| = 2*|x^+c|
(define (H-INV t i)
  (and (> i 1)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (b (front-symbs w-xs1 'b))
              (w-xs1b (drop w-xs1 (length b)))
              (xs2 (front-symbs w-xs1b 'x))
              (w-xs1bxs2 (drop w-xs1b (length xs2)))
              (c (front-symbs w-xs1bxs2 'c))]
         (and (equal? w (append xs1 b xs2 c))
              (= (add1 (length xs1)) (* 2 (add1 (length xs2))))
              (= (length b) 1)
              (= (length c) 1)
              (= (remainder (length (append xs1 xs2)) 3) 1)))))

(check-equal? (H-INV `(,LM ,BLANK a b c ,BLANK) 2) #f)
(check-equal? (H-INV `(,LM ,BLANK x x c ,BLANK) 4) #f)
(check-equal? (H-INV `(,LM ,BLANK x b c ,BLANK) 2) #t)
(check-equal? (H-INV `(,LM ,BLANK x x x b x c ,BLANK) 3) #t)

;; tape natnum --> Boolean
;; Purpose: Determine input word = xx(xxx)*c and |xs|%3 = 2
(define (I-INV t i)
  (and (> i 2)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (c (front-symbs w-xs1 'c))]
         (and (equal? w (append xs1 c))
              (= (length c) 1)
              (= (remainder (length xs1) 3) 2)))))

(check-equal? (I-INV `(,LM ,BLANK a b c ,BLANK) 2) #f)
(check-equal? (I-INV `(,LM ,BLANK x c ,BLANK) 3) #f)
(check-equal? (I-INV `(,LM ,BLANK x x c ,BLANK) 4) #t)
(check-equal? (I-INV `(,LM ,BLANK x x x x x c ,BLANK) 7) #t)


;; tape natnum --> Boolean
;; Purpose: w = xxxx* and |xs|%3 = 0 and tape[i] = x
(define (K-INV t i)
  (let [(w (drop-right (drop t 2) 1))]
    (and (> i 3)
         (eq? (list-ref t i) 'x)
         (andmap (λ (s) (eq? s 'x)) w)
         (>= (length w) 3)
         (= (remainder (length w) 3) 0)
         (= i (add1 (length w))))))

(check-equal? (K-INV `(,LM ,BLANK a b c ,BLANK) 3) #f)
(check-equal? (K-INV `(,LM ,BLANK x x c ,BLANK) 3) #f)
(check-equal? (K-INV `(,LM ,BLANK x x x ,BLANK) 4) #t)
(check-equal? (K-INV `(,LM ,BLANK x x x x x x ,BLANK) 7) #t)


;; tape natnum --> Boolean
;; Purpose: Determine that w = xxxx* and |xs|%3 = 0 and i = |w| + 2
(define (L-INV t i)
  (let [(w (drop-right (drop t 2) 1))]
    (and (> i 4)
         (andmap (λ (s) (eq? s 'x)) w)
         (>= (length w) 3)
         (= (remainder (length w) 3) 0)
         (= i (+ (length w) 2)))))

(check-equal? (L-INV `(,LM ,BLANK a ,BLANK) 3) #f)
(check-equal? (L-INV `(,LM ,BLANK x x c ,BLANK) 5) #f)
(check-equal? (L-INV `(,LM ,BLANK x x x ,BLANK) 5) #t)
(check-equal? (L-INV `(,LM ,BLANK x x x x x x ,BLANK) 8) #t)



;; tape natnum --> Boolean
;; Purpose: Determine input word = x* and |xs|%3 = 0
(define (Y-INV t i)
  (let* [(w (drop-right (drop t 2) 1))]
    (and (eq? (list-ref t i) BLANK) ;;;
         (andmap (λ (s) (eq? s 'x)) w)
         (= (remainder (length w) 3) 0))))

(check-equal? (Y-INV `(,LM ,BLANK x x c ,BLANK) 3) #f)
(check-equal? (Y-INV `(,LM ,BLANK a b c ,BLANK) 3) #f)
(check-equal? (Y-INV `(,LM ,BLANK ,BLANK) 2) #t)
(check-equal? (Y-INV `(,LM ,BLANK x x x x x x ,BLANK) 8) #t) ;;;

#;(sm-visualize anbncn
                (list 'S S-INV) (list 'A A-INV) (list 'B B-INV)
                (list 'C C-INV) (list 'D D-INV) (list 'E E-INV)
                (list 'F F-INV) (list 'G G-INV) (list 'H H-INV)
                (list 'I I-INV) (list 'J J-INV) (list 'K K-INV)
                (list 'L L-INV) (list 'Y Y-INV))