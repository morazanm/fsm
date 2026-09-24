#lang racket/base

(require
  racket/list
  "../../fsm-core/private/tm.rkt" 
  "../../fsm-core/private/constants.rkt"
  "../../fsm-core/private/mtape-tm.rkt"
  "sm-viz.rkt")

;; State Documentation:
;; S: i = 1 AND tape[1] = BLANK
;; A: i = 2 AND tape[i-1] = BLANK
;; B: i > 2 AND tape[2..i-1] = (aUb)+ AND |xs| = 0 AND tape[1] = BLANK
;; C: i > 0 AND |xs| = 0 AND tape[1] = BLANK
;; D: i = 2 AND tape[i-1] = BLANK AND |xs| = 0
;; E: i >= 2 AND tape[2] = x AND tape[1] = BLANK AND |xs| = 1 AND |as|%2 = 1 AND |bs|%2 = 0
;; F: i > 0 AND tape[2] = x AND |xs| => 2 AND |xs|%2 = 0 AND tape[1] = BLANK AND |bs|%2 = 0 AND |as|%2 = 0
;; G: i >= 2 AND tape[2] = x AND tape[1] = BLANK AND |xs| = 1 AND |bs|%2 = 1 AND |as|%2 = 0
;; H: i > 1 AND tape[2] = x AND |xs|%2 = 0 AND |as|%2 = 0 AND |bs|%2 = 0 AND tape[1] = BLANK
;; N: i > 2 AND tape[2..i] = xx+ AND |xs|%2 = 1 AND |as|%2 = 1 AND |bs|%2 = 0 AND tape[1] = BLANK
;; I: i > 3 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 1 AND |bs|%2 = 0 AND tape[1] = BLANK
;; J: i > 4 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 1 AND |bs|%2 = 0 AND tape[i-1] = x AND tape[1] = BLANK
;; O: i > 2 AND tape[2..i] = xx+ AND |xs|%2 = 1 AND |as|%2 = 0 AND |bs|%2 = 1 AND tape[1] = BLANK
;; K: i > 3 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 0 AND |bs|%2 = 1 AND tape[1] = BLANK
;; L: i > 4 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 0 AND |bs|%2 = 1 AND tape[i-1] = x AND tape[1] = BLANK
;; M: i > 1 AND w = x+ AND |xs|%2 = 0 AND |as| = 0 AND |bs| = 0 AND tape[1] = BLANK
;; Y: i > 1 and w = x* AND |xs|%2 = 0 AND |as| = 0 AND |bs| = 0 AND tape[i] = BLANK AND tape[1] = BLANK

;; PRE: tape = `(,LM ,BLANK w w) AND i = 1
(define ww (make-unchecked-tm
            '(S A D E F G H I J K M N O P Y)
            '(a b x)
            `(((S ,BLANK) (A ,RIGHT))
              ((A a) (D a))
              ((A b) (D b))
              ((A ,BLANK) (Y ,BLANK))
              ((D a) (E x))
              ((D b) (G x))
              ((E x) (E ,RIGHT))
              ((E a) (E ,RIGHT))
              ((E b) (E ,RIGHT))
              ((E a) (F x))
              ((F a) (F ,LEFT))
              ((F b) (F ,LEFT))
              ((F x) (F ,LEFT))
              ((F ,BLANK) (N ,RIGHT))
              ((F ,BLANK) (H ,RIGHT))
              ((G x) (G ,RIGHT))
              ((G a) (G ,RIGHT))
              ((G b) (G ,RIGHT))
              ((G b) (F x))
              ((H x) (H ,RIGHT))
              ((H a) (O x))
              ((H b) (P x))
              ((I a) (I ,RIGHT))
              ((I b) (I ,RIGHT))
              ((I x) (J ,RIGHT))
              ((J x) (J ,RIGHT))
              ((J a) (F x))
              ((K a) (K ,RIGHT))
              ((K b) (K ,RIGHT))
              ((K x) (M ,RIGHT))
              ((M x) (M ,RIGHT))
              ((M b) (F x))
              ((N x) (N ,RIGHT))
              ((N ,BLANK) (Y ,BLANK))
              ((O x) (I ,RIGHT))
              ((P x) (K ,RIGHT))
              )
            'S
            '(Y)
            'Y))

;; invariants for L = ww


;; tape = @ _ x^nu x^nu AND 1<=i<=2+2n+|u|-1

(define (F-INV-ww t i)
  (let* [(xn (takef (drop t 2) (λ (symb) (eq? symb 'x))))
         (length-xn (length xn))
         (u (takef (drop t (+ length-xn 2)) (λ (symb) (not (eq? 'x symb)))))]
    (and (or (equal? t (append '(@ _) xn))
             (equal? t (append '(@ _) xn u xn u)))
         (<= 1 i (+ 1 (* 2 length-xn) (length u))))))

;; tape natnum --> Boolean
;; Purpose: Determine head in position 1 and tape[i]=BLANK
(define (S-INV-ww t i)
  (and (= i 1) (eq? (list-ref t i) BLANK)))

;; tape natnum --> Boolean
;; Purpose: Determine head in position 2 AND tape[i-1] = BLANK
(define (A-INV-ww t i)
  (and (= i 2) (eq? (list-ref t (sub1 i)) BLANK)))


;; tape natnum --> Boolean
;; Purpose: Determine head in position greater than 2 AND tape[2..i-1] = (aUb)+ AND |xs| = 0 
(define (B-INV-ww t i)
  (and (> i 2)
       (eq? BLANK (list-ref t 1))
       (empty? (filter (λ (symb) (eq? 'x symb)) t))
       (let* [(w2->i-1 (take (drop t 2) (- i 2)))
              (onlyas&bs (filter (λ (symb) (or (eq? 'a symb)
                                               (eq? 'b symb)))
                                 w2->i-1))]
         (equal? w2->i-1 onlyas&bs))))

;; tape natnum --> Boolean
;; Purpose: Determine head in position greater than 2 AND tape[2..i-1] = (aUb)+ AND |xs| = 0 
(define (C-INV-ww t i)
  (and (> i 0)
       (eq? BLANK (list-ref t 1))
       (empty? (filter (λ (symb) (eq? 'x symb)) t))))

;; tape natnum --> Boolean
;; Purpose: Determine head in position 2 AND tape[i-1] = BLANK
(define (D-INV-ww t i)
  (and (= i 2)
       (eq? BLANK (list-ref t (sub1 i)))))


;; tape natnum --> Boolean
;; Purpose: Determine head in position 2 or greater AND tape[2] = x AND tape[1] = BLANK AND |xs| = 1 AND |as|%2 = 1 AND |bs|%2 = 0
(define (E-INV-ww t i)
  (and (>= i 2)
       (eq? BLANK (list-ref t 1))
       (eq? 'x (list-ref t 2))
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 0)))

;; tape natnum --> Boolean
;; Purpose: Determine head in position is 2 AND tape[2] = x AND tape[i-1] = BLANK AND |xs| = 1 AND |bs|%2 = 1 AND |as|%2 = 0
(define (G-INV-ww t i)
  (and (>= i 2)
       (eq? BLANK (list-ref t 1))
       (eq? 'x (list-ref t 2))
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 1)))


;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater i > 1 AND tape[2] = x AND |xs|%2 = 0 AND |as|%2 = 0 AND |bs|%2 = 0 AND tape[1] = BLANK
(define (H-INV-ww t i)
  (and (> i 1)
       (eq? BLANK (list-ref t 1))
       (eq? 'x (list-ref t 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 0)))

;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 2 AND tape[2..i] = x AND |xs|%2 = 1 
(define (N-INV-ww t i)
  (let* [(w2->i (take (drop t 2) (- i 1)))]
    (and (>= i 2)
         (andmap (λ (x) (eq? x 'x)) (takef (drop t 2) (λ (x) (eq? x 'x))))      
         (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 0))))


;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 3 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 1 AND |bs|%2 = 0 AND tape[1] = BLANK
(define (I-INV-ww t i)
  (and (> i 3)
       (eq? BLANK (list-ref t 1))
       (let* [(w2->i (take (drop t 2) (- 3 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 0)))

;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 4 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 1 AND |bs|%2 = 0 AND tape[i-1] = x AND tape[1] = BLANK
(define (J-INV-ww t i)
  (and (> i 4)
       (eq? BLANK (list-ref t 1))
       (eq? 'x (list-ref t (sub1 i)))
       (let* [(w2->i (take (drop t 2) (- 3 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 0)))


;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 2 AND tape[2..i] = xx+ AND |xs|%2 = 1 AND |as|%2 = 0 AND |bs|%2 = 1 AND tape[1] = BLANK
(define (O-INV-ww t i)
  (and (> i 2)
       (eq? BLANK (list-ref t 1))
       (let* [(w2->i (take (drop t 2) (- i 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (>= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 0)))


;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 3 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 0 AND |bs|%2 = 1 AND tape[1] = BLANK
(define (K-INV-ww t i)
  (and (> i 3)
       (eq? BLANK (list-ref t 1))
       (let* [(w2->i (take (drop t 2) (- 3 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 1)))


;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 4 AND tape[2..3] = xx AND |xs|%2 = 1 AND |as|%2 = 0 AND |bs|%2 = 1 AND tape[i-1] = x AND tape[1] = BLANK
(define (L-INV-ww t i)
  (and (> i 4)
       (eq? BLANK (list-ref t 1))
       (eq? 'x (list-ref t (sub1 i)))
       (let* [(w2->i (take (drop t 2) (- 3 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 1)))

(define (P-INV-ww t i)
  (and (> i 2)
       (eq? BLANK (list-ref t 1))
       (let* [(w2->i (take (drop t 2) (- i 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (>= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 1)))

;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 1 AND w = x+ AND |xs|%2 = 0 AND |as| = 0 AND |bs| = 0 AND tape[1] = BLANK
(define (M-INV-ww t i)
  (and (> i 4)
       (eq? BLANK (list-ref t 1))
       (eq? 'x (list-ref t (sub1 i)))
       (let* [(w2->i (take (drop t 2) (- 3 1)))]
         (equal? w2->i (filter (λ (symb) (eq? 'x symb)) w2->i))
         (= (length (filter (λ (symb) (eq? 'x symb)) w2->i)) 2))
       (>= (length (filter (λ (symb) (eq? symb 'x)) t)) 2)
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 1)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 1)))


;; tape natnum --> Boolean
;; Purpose: Determine head in position is greater than 1 and w = x* AND |xs|%2 = 0 AND |as| = 0 AND |bs| = 0 AND tape[i] = BLANK AND tape[1] = BLANK
(define (Y-INV-ww t i)
  (and (> i 1)
       (eq? BLANK (list-ref t 1))
       (eq? BLANK (list-ref t i))
       (let* [(ww (drop t 2))]
         (equal? ww (filter (λ (symb) (eq? 'x symb)) ww))
         (>= (length (filter (λ (symb) (eq? 'x symb)) ww)) 0))
       (= (remainder (length (filter (λ (symb) (eq? symb 'x)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'a)) t)) 2) 0)
       (= (remainder (length (filter (λ (symb) (eq? symb 'b)) t)) 2) 0)))

(sm-viz ww '(@ _ a a b a a b) #:head-pos 1 (list 'S S-INV-ww)
          (list 'A A-INV-ww)
          (list 'D D-INV-ww)
          (list 'E E-INV-ww)
          (list 'F F-INV-ww)
          (list 'G G-INV-ww)
          (list 'H H-INV-ww)
          (list 'I I-INV-ww)
          (list 'J J-INV-ww)
          (list 'K K-INV-ww)
          (list 'M M-INV-ww)
          (list 'N N-INV-ww)
          (list 'O O-INV-ww)
          (list 'P P-INV-ww)
          (list 'Y Y-INV-ww))


;;Pre-Condition: '(LM BLANK w) AND t0h = 1 AND tape 1 is empty AND t1h = 0
;;compute f(w) = ww
(define copy (make-unchecked-mttm '(K H T F E B W D M)
                      '(a b)
                      'K
                      '(M)
                      (list
                       (list (list 'K (list BLANK BLANK)) ;;<--- start
                             (list 'H (list RIGHT RIGHT))) 
                       (list (list 'H (list 'a BLANK)) ;;<---- PHASE 1: read a in w 
                             (list 'T (list 'a 'a)))
                       (list (list 'T (list 'a 'a))
                             (list 'H (list RIGHT RIGHT)))
                       (list (list 'H (list 'b BLANK)) ;;<---- PHASE 1: read b in w
                             (list 'F (list 'b 'b)))
                       (list (list 'F (list 'b 'b))
                             (list 'H (list RIGHT RIGHT)))
                       (list (list 'H (list BLANK BLANK)) ;;<--- PHASE 2: Go to beginning of t1
                             (list 'E (list BLANK LEFT)))
                       (list (list 'E (list BLANK 'a))
                             (list 'E (list BLANK LEFT)))
                       (list (list 'E (list BLANK 'b))
                             (list 'E (list BLANK LEFT)))
                       (list (list 'E (list BLANK BLANK)) ;;<---- PHASE 3: read w on t1 AND write w on t0
                             (list 'W (list BLANK RIGHT)))
                       (list (list 'W (list BLANK 'a))
                             (list 'D (list 'a 'a)))
                       (list (list 'D (list 'a 'a))
                             (list 'W (list RIGHT RIGHT)))
                       (list (list 'W (list BLANK 'b))
                             (list 'B (list 'b 'b)))
                       (list (list 'B (list 'b 'b))
                             (list 'W (list RIGHT RIGHT)))
                       (list (list 'W (list BLANK BLANK))
                             (list 'M (list BLANK BLANK)))
                       )        
                      2))

;;(listof tape-configs) -> boolean
;;Purpose: Determine if K-inv holds
(define (k-inv tape-config)
  (let [(t0h (car (car tape-config)))
        (t0 (cadr (car tape-config)))
        (t1h (car (cadr tape-config)))
        (t1 (cadr (cadr tape-config)))]
    (and (= t0h 1) (= t1h 0)
         (eq? (list-ref t0 t0h) BLANK)
         (equal? t1 (list BLANK)))))

;;(listof tape-configs) -> boolean
;;Purpose: Determine if H-inv holds
(define (h-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (readt0 (take (drop t0 2) (- t0h 2)))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (= (length readt0) (- t1h 1))
         (equal? readt0 readt1))))


;;(listof tape-configs) -> boolean
;;Purpose: Determine if T-inv holds
(define (t-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (readt0 (take (drop t0 2) (- t0h 2)))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (eq? (list-ref t0 t0h) 'a)
         (= (length readt0) (- t1h 1))
         (equal? readt0 readt1))))

;;(listof tape-configs) -> boolean
;;Purpose: Determine if F-inv holds
(define (f-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (readt0 (take (drop t0 2) (- t0h 2)))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (eq? (list-ref t0 t0h) 'b)
         (= (length readt0) (- t1h 1))
         (equal? readt0 readt1))))

;;(listof tape-configs) -> boolean
;;Purpose: Determine if E-inv holds
(define (e-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (readt0 (take (drop t0 1) t0h))]
    (and (>= t0h 2)
         (eq? (list-ref t0 t0h) BLANK)
         (equal? readt0 t1))))

(define (w-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (writet0 (take (drop t0 (length t1)) (- t0h (length t1))))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (equal? writet0 readt1))))

(define (b-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (writet0 (take (drop t0 (length t1)) (- t0h (length t1))))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (eq? (list-ref t1 t1h) 'b)
         (equal? writet0 readt1))))

(define (d-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (writet0 (take (drop t0 (length t1)) (- t0h (length t1))))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (eq? (list-ref t1 t1h) 'a)
         (equal? writet0 readt1))))

(define (m-inv tape-config)
  (let* [(t0h (car (car tape-config)))
         (t0 (cadr (car tape-config)))
         (t1h (car (cadr tape-config)))
         (t1 (cadr (cadr tape-config)))
         (readt0 (take (drop t0 2) (- t0h 2)))
         (readt1 (take (drop t1 1) (- t1h 1)))]
    (and (>= t0h 2) (>= t1h 1)
         (eq? (list-ref t0 t0h) BLANK)
         (eq? (list-ref t1 t1h) BLANK)
         (equal? readt0 (append readt1 readt1)))))

(sm-viz copy `(,LM ,BLANK a b b a b) #:head-pos 1 (list 'K k-inv) 
        (list 'H h-inv)
        (list 'T t-inv)
        (list 'F f-inv)
        (list 'E e-inv)
        (list 'B b-inv)
        (list 'W w-inv)
        (list 'D d-inv)
        (list 'M m-inv))