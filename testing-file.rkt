#lang racket

(require "main.rkt")

;;---- DFA ----
(define a* (make-dfa '(S F)     ;; the states
                     '(a b)     ;; the input alphabet
                     'S         ;; the staring state
                     '(F)       ;; the set of final states
                     '((S a F)  ;; the transition functions
                       (F a F)
                       (F b F))))
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


;; gets the number of z's in the tape
(define get-num-of-x (lambda (tape symbol)
                       (length (filter (lambda (ele)
                                         (eq? ele symbol)) tape))))

(define implies (lambda (a c)
                  (or (not a) c)))
                  

;; The number of z's is divisiable by 3
(define S-INV (lambda (tape posn)
                (and (equal? 0 ((get-num-of-x tape 'z) . modulo . 3))
                     (or
                      (let ((num-of-a (get-num-of-x tape 'a)))
                        (implies (or (eq? (list-ref tape posn) 'c)
                                     (eq? (list-ref tape posn) 'b))
                                 (or (< num-of-a
                                        (get-num-of-x tape 'b))
                                     (< num-of-a
                                        (get-num-of-x tape 'c)))))
                      
                      (implies (equal? (list-ref tape posn) BLANK)
                               (equal? tape `(,LM ,BLANK)))))))
                     

;; The number of z's on the tape is greater then 1
(define B-INV (lambda (tape posn)
                (= (modulo (get-num-of-x tape 'z) 3) 1)))

(define C-INV (lambda (tape posn)
                (= (modulo (get-num-of-x tape 'z) 3) 2)))

(define D-INV (lambda (tape posn)
                (= (modulo (get-num-of-x tape 'z) 3) 0)))

(define E-INV (lambda (tape posn)
                (and
                 (= (modulo (get-num-of-x tape 'z) 3) 0)
                 #t)))
                 

(define N-INV (lambda (tape posn)
                (not (= (get-num-of-x tape 'z)
                        (sub1 (length tape))))))

(define Y-INV (lambda (tape posn)
                (let ((num-z (get-num-of-x tape 'z)))
                  (and
                   (= num-z (sub1 (length tape)))
                   (= (modulo num-z 3) 0)))))
                  
#|

(sm-visualize  a^nb^nc^n
               (list 'S S-INV)
               (list 'B B-INV)
               (list 'C C-INV)
               (list 'D D-INV)
               (list 'N N-INV)
               (list 'Y Y-INV))
|#

(sm-visualize pda-wcw^r)