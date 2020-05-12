#lang racket

(require "main.rkt")
(require racket/stream)


;;---- DFA ----
(define a* (make-dfa '(S F)     ;; the states
                     '(a b)     ;; the input alphabet
                     'S         ;; the staring state
                     '(F)       ;; the set of final states
                     '((S a F)  ;; the transition functions
                       (F a F)
                       (F b F))))
(define pda-numa=numb (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M ,EMP ,EMP) (F ,EMP))
                                    ((M a ,EMP) (M (a)))
                                    ((M b ,EMP) (M (b)))
                                    ((M a (b)) (M ,EMP))
                                    ((M b (a)) (M ,EMP)))))
#|
;;---- NDFA ----
;; valid input: aaabbb 
(define P (make-ndpda '(S F)
                      '(a b)
                      '(c)
                      'S
                      '(F)
                      `(((S ,EMP ,EMP) (F ,EMP))
                        ((F a ,EMP) (F (c)))
                        ((F b (c)) (F ,EMP)))))

;;---- PDA ----
(define pda-numa=numb (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M ,EMP ,EMP) (F ,EMP))
                                    ((M a ,EMP) (M (a)))
                                    ((M b ,EMP) (M (b)))
                                    ((M a (b)) (M ,EMP))
                                    ((M b (a)) (M ,EMP)))))
;; valid input: aabcbaa
#|
(define pda-wcw^r (make-ndpda '(S M N F)
                              '(a b c)
                              '(a b)
                              'S
                              '(F)
                              `(((S ,EMP ,EMP) (M ,EMP))
                                ((M a ,EMP) (M (a)))
                                ((M b ,EMP) (M (b)))
                                ((M c ,EMP) (N ,EMP))
                                ((N a (a)) (N ,EMP))
                                ((N b (b)) (N ,EMP))
                                ((N ,EMP ,EMP) (F ,EMP)))))
|#
;;---- TM ----

;; machine input tape-pos (optional)
;; (sm-showtransitions Ma `(,LM b b b b) 2)
; write "a" on tape
(define Ma2 (make-tm '(S H)                 ;the states
                     `(a b)             ;the alphabet
                     `(((S a) (H a))        ;the transition relation
                       ((S b) (H a))
                       ((S ,BLANK) (H a)))
                     'S                     ;the starting state
                     '(H)))                 ;the halting states


; write "a" on tape
(define Ma (make-tm '(S H)
                    `(a b ,LM)
                    `(((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S
                    '(H)))



(define S2-INV (lambda (x y) #t))
(define H-INV (lambda (tape posn)
                (eq? 'a (list-ref tape posn))))

;;(sm-visualize Ma (list 'S S2-INV) (list 'H H-INV))











;; ----- TM language recognizer -----

(define Alla (make-tm '(S Y N)
                      `(a b ,LM)
                      `(((S a) (S ,RIGHT))
                        ((S b) (N b))
                        ((S ,BLANK) (Y ,BLANK)))
                      'S
                      '(Y N)
                      'Y))


;; precondition: 1) head is at 1
;;               2) tape input only contains a's b's c's (does not contain z)
(define a^nb^nc^n (make-tm '(S B C D E Y N)
                           '(a b c z)
                           `(((S a) (B z))
                             ((S b) (N b))
                             ((S c) (N c))
                             ((S ,BLANK) (Y ,BLANK))
                             ((S z) (N z))
                             ((E z) (E ,RIGHT))
                             ((E ,BLANK) (Y ,BLANK))
                             ((E a) (N a))
                             ((E b) (N b))
                             ((E c) (N c))
                             ((B a) (B ,RIGHT))
                             ((B b) (C z))
                             ((B c) (N c))
                             ((B ,BLANK) (N ,BLANK))
                             ((B z) (B ,RIGHT))
                             ((C a) (N a))
                             ((C b) (C ,RIGHT))
                             ((C c) (D z))
                             ((C ,BLANK) (N ,BLANK))
                             ((C z) (C ,RIGHT))
                             ((D a) (S a))
                             ((D b) (D ,LEFT))
                             ((D c) (D ,LEFT))
                             ((D ,BLANK) (N ,BLANK))
                             ((D z) (D ,LEFT))
                             ((D ,LM) (E R)))
                           'S
                           '(Y N)
                           'Y))
|#
;; z => a is is read
;; x => b is read
;; z => y is read
(define a^nb^nc^n2 (make-tm '(S B C D E Y N)
                            '(a b c z x y)
                            `(((S a) (B z))
                              ((S b) (N b))
                              ((S c) (N c))
                              ((S ,BLANK) (Y ,BLANK))
                              ((S z) (N z))
                              ((S x) (N x))
                              ((S y) (N y))
                            
                              ((E z) (E ,RIGHT))
                              ((E x) (E ,RIGHT))
                              ((E y) (E ,RIGHT))
                              ((E ,BLANK) (Y ,BLANK))
                              ((E a) (N a))
                              ((E b) (N b))
                              ((E c) (N c))

                              ((B a) (B ,RIGHT))
                              ((B b) (C x))
                              ((B c) (N c))
                              ((B ,BLANK) (N ,BLANK))
                              ((B z) (B ,RIGHT))
                              ((B x) (B ,RIGHT))
                              ((B y) (B ,RIGHT))

                              ((C a) (N a))
                              ((C b) (C ,RIGHT))
                              ((C c) (D y))
                              ((C ,BLANK) (N ,BLANK))
                              ((C z) (C ,RIGHT))
                              ((C x) (C ,RIGHT))
                              ((C y) (C ,RIGHT))

                              ((D a) (S a))
                              ((D b) (D ,LEFT))
                              ((D c) (D ,LEFT))
                              ((D ,BLANK) (N ,BLANK))
                              ((D z) (D ,LEFT))
                              ((D x) (D ,LEFT))
                              ((D y) (D ,LEFT))
                              ((D ,LM) (E R)))
                            'S
                            '(Y N)
                            'Y))



#|
;; gets the number of z's in the tape
(define get-num-of-x (lambda (tape symbol)
                       (length (filter (lambda (ele)
                                         (eq? ele symbol)) tape))))

(define implies (lambda (a c)
                  (or (not a) c)))

;; takewhile stream-list boolean-procedure
;; Purpose: Contines to take from the stream until the boolean procedure is met
(define (takewhile list proc)
  (letrec(
          (takewhile-eval (lambda (list proc) 
                            (cond
                              [(empty? list) '()]
                              [(proc (stream-first list))
                               (stream-cons (stream-first list)
                                            (takewhile-eval (stream-rest list) proc))]
                              [else '()]))))
    (stream->list (takewhile-eval list proc))))

;; dropwhile stream-list boolean-procedure
;; Purpose: Continues to srop elements from the stream until the boolean procedure conditional is true
(define (dropwhile list proc)
  (letrec(
          (dropwhile-eval (lambda (list proc)
                            (cond
                              [(empty? list)'()]
                              [(proc (stream-first list)) (dropwhile-eval (stream-rest list) proc)]
                              [else list]))))
    (stream->list (dropwhile-eval list proc))))


;;num zs before first a = num zs between last a and first b = num zs between last b and first c

;; The number of z's is divisiable by 3
;; The number of z's before the first a is less or equal to the number of z's
(define S-INV (lambda (tape posn)
                (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                               (equal? input 'y)
                                                               (equal? input 'z)))
                                           tape)))
                  (and (equal? 0 (modulo (length list-of-xyz) 3))
                       (or
                        (let ((num-of-a (get-num-of-x tape 'a)))
                          (implies (or (eq? (list-ref tape posn) 'c)
                                       (eq? (list-ref tape posn) 'b))
                                   (or (< num-of-a
                                          (get-num-of-x tape 'b))
                                       (< num-of-a
                                          (get-num-of-x tape 'c)))))
                      
                        (implies (equal? (list-ref tape posn) BLANK)
                                 (equal? tape `(,LM ,BLANK))))))))
                     

;; the number of z's is one bigger then number of x's and number of y's
(define B-INV (lambda (tape posn)
                (let ((num-z (get-num-of-x tape 'z)))
                  (and (> num-z (get-num-of-x tape 'x))
                       (> num-z (get-num-of-x tape 'y))))))

;; the number of x's before the first b is one more then the number of x's after the first b
(define C-INV (lambda (tape posn)
                (let ((num-x (get-num-of-x tape 'x)))
                  (and (= num-x (get-num-of-x tape 'z))
                       (> num-x (get-num-of-x tape 'y))))))

;; the number of y's before the first c is one more then the number of y's after the first c
(define D-INV (lambda (tape posn)
                (let ((num-y (get-num-of-x tape 'y)))
                  (and (= num-y (get-num-of-x tape 'z))
                       (= num-y (get-num-of-x tape 'x))))))


(define E-INV (lambda (tape posn)
                (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                               (equal? input 'y)
                                                               (equal? input 'z)))
                                           tape)))
                  (= (modulo (length list-of-xyz) 3) 0))))
                 

(define N-INV (lambda (tape posn)
                (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                               (equal? input 'y)
                                                               (equal? input 'z)))
                                           tape)))
                  (not (= (length list-of-xyz)
                          (sub1 (length tape)))))))

(define Y-INV (lambda (tape posn)
                (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                               (equal? input 'y)
                                                               (equal? input 'z)))
                                           tape)))
                  (and
                   (= (length list-of-xyz) (- (length tape) 2))
                   (= (modulo (length list-of-xyz) 3) 0)))))





(define LB (make-tm '(S H)
                    `(a b)
                    `(((S a) (S ,LEFT))
                      ((S b) (S ,LEFT))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))

|#
(sm-visualize a*)







