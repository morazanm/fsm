#lang racket

(require (for-syntax syntax/parse syntax/to-string)
         syntax/to-string
         "../main.rkt")


(define a-aUb*
  (make-ndfa '(S F)     ;; the states
             '(a b)     ;; the input alphabet
             'S         ;; the staring state
             '(F)       ;; the set of final states
             '((S a F)  ;; the transition functions
               (F a F)
               (F b F))
             'nodead))

(define tmp
  (make-ndfa '(S A F)     
             '(a b)     
             'S        
             '(F)       
             '((S a F)
               (F a F)
               (S b A)
               (A a F)
               (A b A))
             'nodead))

(define-invariants-for a-aUb*
  (define-invariant-for S (ci) (empty? ci))
  (define-invariant-for F (ci)
    (define ans (and (not (empty? ci))
                     (eq? (first ci) 'a)
                     (andmap (λ (s) (or (eq? s 'a) (eq? s 'b)))
                             (rest ci))))
    ans))


;(sm-visualize a-aUb* (list 'S SS-INV) (list 'F FF-INV))
(sm-visualize! a-aUb*)

#;(sm-visualize3 a-aUb*
                 (list 'F (inv->string!
                           (lambda (ci)
                             (and (not (empty? ci))
                                  (eq? (first ci) 'a)
                                  (andmap (λ (s) (or (eq? s 'a) (eq? s 'b)))
                                          (rest ci))))))
                 (list 'S (inv->string! (lambda (ci) (empty? ci)))))

;;---- DFA ----
(define a*
  (make-dfa '(S F)     ;; the states
            '(a b)     ;; the input alphabet
            'S         ;; the staring state
            '(F)       ;; the set of final states
            '((S a F)  ;; the transition functions
              (F a F)
              (F b F))
            'nodead))

#;(sm-visualize2 a* (list 'S (lambda (v) true))
                 (list 'F (lambda (v) false)))

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


#;(sm-visualize!! pda-numa=numb
                  (S -> PDA-S-INV)
                  (F -> PDA-F-INV)
                  (M -> PDA-M-INV))
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

;; z => a is is read
;; x => b is read
;; z => y is read
|#

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




(define-invariants-for a^nb^nc^n2
  ;; gets the number of z's in the tape
  (define get-num-of-x (lambda (tape symbol)
                         (length (filter (lambda (ele)
                                           (eq? ele symbol)) tape))))

  (define implies (lambda (a c)
                    (or (not a) c)))
  
  ;; The number of z's is divisiable by 3
  ;; The number of z's before the first a is less or equal to the number of z's
  (define-invariant-for S (S tape posn)
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
                     (equal? tape `(,LM ,BLANK)))))))

  ;; the number of z's is one bigger then number of x's and number of y's
  (define-invariant-for B (tape posn)
    (let ((num-z (get-num-of-x tape 'z)))
      (and (> num-z (get-num-of-x tape 'x))
           (> num-z (get-num-of-x tape 'y)))))

  ;; the number of x's before the first b is one more then the number of x's after the first b
  (define-invariant-for C (tape posn)
    (let ((num-x (get-num-of-x tape 'x)))
      (and (= num-x (get-num-of-x tape 'z))
           (> num-x (get-num-of-x tape 'y)))))

  ;; the number of y's before the first c is one more then the number of y's after the first c
  (define-invariant-for D (tape posn)
    (let ((num-y (get-num-of-x tape 'y)))
      (and (= num-y (get-num-of-x tape 'z))
           (= num-y (get-num-of-x tape 'x)))))


  (define-invariant-for E (tape posn)
    (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                   (equal? input 'y)
                                                   (equal? input 'z)))
                               tape)))
      (= (modulo (length list-of-xyz) 3) 0)))
                 

  (define-invariant-for N (tape posn)
    (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                   (equal? input 'y)
                                                   (equal? input 'z)))
                               tape)))
      (not (= (length list-of-xyz)
              (sub1 (length tape))))))

  (define-invariant-for Y (tape posn)
    (let ((list-of-xyz (filter (lambda (input) (or (equal? input 'x)
                                                   (equal? input 'y)
                                                   (equal? input 'z)))
                               tape)))
      (and
       (= (length list-of-xyz) (- (length tape) 2))
       (= (modulo (length list-of-xyz) 3) 0)))))


(sm-visualize! a^nb^nc^n2)
               
#|
(define LB (make-tm '(S H)
                    `(a b)
                    `(((S a) (S ,LEFT))
                      ((S b) (S ,LEFT))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))



;(sm-visualize a*)





(define XX
  (make-tm
   '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Q11 Q12)
   `(x y i c)
   `(((Q0 i) (Q0 ,RIGHT)) ;WE ARE IN Q0, WE SEE AN I HEAD RIGHT AND RETURN TO Q0
     ((Q0 c) (Q1 ,RIGHT)) ;THE CENTRAL MULTIPLICATION SYMBOL C HAS BEEN SEEN, HEAD TO Q1
     
     ((Q1 i) (Q1 ,RIGHT)) ; NOW IN Q1, CONTINUE RIGHT ON EVERY I
     ((Q1 ,BLANK) (Q1 c)) ;ONCE WE HAVE ENCOUNTERED A BLANK, WE KNOW WE ARE AT THE END. OVERWRITE THE BLANK WITH A C
     ((Q1 c) (Q2 ,LEFT)) ;NOW THAT THE C HAS BEEN WRITTEN IN, HEAD TO STATE Q2 AND PROGRESS LEFT ALONG THE TAPE

 

     ((Q2 i) (Q2 ,LEFT)) ; WHEN WE ARE IN Q2, MOVE LEFT EVERY I
     ((Q2 c) (Q3 ,RIGHT));IN THE EVENT THAT WE ONCE AGAIN ENCOUNTER A C, MOVE RIGHT ALONG THE TAPE

 

     ((Q3 x) (Q3 ,RIGHT)); IF WE SEE AN X IN Q3, MOVE RIGHT ALONG THE TAPE
     ((Q3 i) (Q4 x)) ; IF WE SEE AN I ON THE TAPE, HEAD TO Q4 AND OVERWRITE WITH X
     ;((Q3 i) (Q4 ,LEFT))
     ((Q3 c) (Q3 ,BLANK)) ;IF WE SEE C HEAD TO HALTING STATE Q12
     ((Q3 ,BLANK) (Q12 ,RIGHT)) ;;;;
     
     ((Q4 x) (Q4 ,LEFT)) ;IF WE SEE AN X, MOVE LEFT ALONG THE TAPE
     ((Q4 c) (Q5 ,LEFT)) ;IF WE SEE A C, MOVE RIGHT ALONG THE TAPE

 

     ((Q5 y) (Q5 ,LEFT)) ;WHEN WE SEE A Y, MOVE LEFT ALONG THE TAPE
     ((Q5 i) (Q6 y)) ;WHEN WE SEE AN I IN Q5, MOVE TO Q6
     ;((Q5 i) (Q6 ,RIGHT))
     ((Q5 ,LM) (Q11 ,RIGHT)) ;;;;
     
     ((Q6 y) (Q6 ,RIGHT)) ;WHEN WE SEE A Y IN Q6, HEAD TO Q6 AND RIGHT ON THE TAPE
     ((Q6 c) (Q7 ,RIGHT))

 

     ((Q7 i) (Q7 ,RIGHT)) ;SEE AN I, GO RIGHT AND BACK TO Q7
     ((Q7 x) (Q7 ,RIGHT)) ;SEE AN X, GO RIGHT AND BACK TO Q7
     ((Q7 c) (Q8 ,RIGHT)) ;SEE A C, HEAD TO Q8 AND RIGHT ON TAPE

 

     ((Q8 i) (Q8 ,RIGHT)) ; SEE AN I, HEAD BACK TO Q8 AND RIGHT ON TAPE
     ((Q8 ,BLANK) (Q9 i)) ;SEE A BLANK HEAD TO Q9
     ;((Q8 ,BLANK) (Q9 ,LEFT))

 

     ((Q9 i) (Q9 ,LEFT)) ;SEE AN I HEAD BACK TO Q9 AND LEFT ON TAPE
     ((Q9 c) (Q10 ,LEFT))

 

     ((Q10 i) (Q10 ,LEFT)) ;SEE AN I, HEAD BACK TO Q10 AND LEFT ON TAPE
     ((Q10 x) (Q10 ,LEFT)) ;SEE AN X, HEAD BACK TO Q10 AND LEFT ON TAPE
     ((Q10 c) (Q5 ,LEFT))

 

     ((Q11 y) (Q11 i)) ;SEE A Y, HEAD BACK TO Q11 AND OVERWRITE WITH I
     ((Q11 i) (Q11 ,RIGHT)) ;SEE AN I HEAD BACK TO Q11 AND RIGHT ALONG THE TAPE
     ((Q11 c) (Q3 ,RIGHT))
     
     )
   'Q0
   '(Q12)))

;;(define x(sm-graph XX))
#|
(define FSM (make-ndfa '(F S M)
                       '(f s m)
                       'F
                       '(M)
                       '((F s S)
                         (S m M)
                         (M f F)) 'nodead))


(define (S-INV ci) (empty? ci))

(define (A-INV ci) (empty? ci))

(define (B-INV ci) (and (= (length ci) 1) (eq? (car ci) 'a)))

(define (C-INV ci)
  (and (>= (length ci) 1) (andmap (λ (s) (eq? s 'a)) ci)))

(define (D-INV ci) (empty? ci))

(define (E-INV ci)
  (and (= (length ci) 1) (eq? (first ci) 'a)))

(define (F-INV ci)
  (and (>= (length ci) 1)
       (eq? (first ci) 'a)
       (andmap (λ (s) (eq? s 'b))
               (rest ci))))


(define M (make-ndfa '(S A B C D E F)
                     '(a b)
                     'S
                     '(C F)
                     `((S ,EMP A)
                       (S ,EMP D)
                       (A a B)
                       (B ,EMP C)
                       (C a C)
                       (D a E)
                       (E ,EMP F)
                       (F b F))
                     'no-dead
                     ))

(sm-visualize M (list 'S S-INV) (list 'A A-INV) (list 'B B-INV) (list 'D D-INV) (list 'E E-INV) (list 'F F-INV))

|#


(define C90 (make-dfa
             (quote (D C B A))
             (quote (a b))
             (quote A)
             (quote (B))
             (quote ((A b C) (A a B)))))

(define (S-INV ci) (empty? ci))

(define (A-INV ci) (empty? ci))

(define (B-INV ci) (and (= (length ci) 1) (eq? (car ci) 'a)))

(define (C-INV ci)
  (and (>= (length ci) 1) (andmap (λ (s) (eq? s 'a)) ci)))

(define (D-INV ci) "Josh")

(define (E-INV ci)
  (and (= (length ci) 1) (eq? (first ci) 'a)))

(define (F-INV ci)
  (and (>= (length ci) 1)
       (eq? (first ci) 'a)
       (andmap (λ (s) (eq? s 'b))
               (rest ci))))


(define M (make-ndfa '(S A B C D E F)
                     '(a b)
                     'S
                     '(C F)
                     `((S ,EMP A)
                       (S ,EMP D)
                       (A a B)
                       (B ,EMP C)
                       (C a C)
                       (D a E)
                       (E ,EMP F)
                       (F b F))
                     'no-dead
                     ))
|#
#|
(sm-visualize M (list 'S S-INV)
              (list 'A A-INV)
              (list 'B B-INV)
              (list 'C C-INV)
              (list 'D D-INV)
              (list 'E E-INV)
              (list 'F F-INV))
|#

(define ONE-MISSING
  (make-dfa '(S A B C D E F)
            '(a b c)
            'S
            '(S A B C D E F)
            '((S a A)   
              (S b B)
              (S c C)
              (A a A)
              (A b D)
              (A c E)
              (B a D)
              (B b B)
              (B c F)
              (C a E)
              (C b F)
              (C c C)
              (D a D)
              (D b D)
              (E a E)
              (E c E)
              (F b F)
              (F c F))))

;;(sm-visualize a* (list 'S (lambda(v) #t)) (list 'F (lambda(v) 5)))



;;(sm-visualize XX)

(define pda=2ba
  (make-ndpda '(S M1 F)
              '(a b)
              '(a b)
              'S
              '(F)
              `(((S ,EMP ,EMP) (M1 ,EMP))
                ((M1 a ,EMP) (M1 (a a)))
                ((M1 b ,EMP) (M1 (b)))
                ((M1 a (b)) (M1 (a)))
                ((M1 a (b b)) (M1 ,EMP))
                ((M1 b (a)) (M1 ,EMP))
                ((M1 ,EMP ,EMP) (F ,EMP)))))

;(sm-visualize a*)

