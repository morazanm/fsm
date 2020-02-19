#lang racket

(require "main.rkt")

;; TEST MACHINES BELOW
(define INIT-STATES '(A B C D))
(define INIT-START 'A)
(define INIT-FINALS '(C D))
(define INIT-RULES (list `(A ,EMP B) '(B b A) '(A c C) '(C b D)))
(define INIT-SIGMA '(a b c b))
(define INIT-CURRENT 'A)
(define INIT-ALPHA `(a b c))

;; NDFA
(define M1 (make-ndfa INIT-STATES INIT-ALPHA INIT-START INIT-FINALS INIT-RULES))

;; DFA
(define a* (make-dfa '(S F)     ;; the states
                     '(a b)     ;; the input alphabet
                     'S         ;; the staring state
                     '(F)       ;; the set of final states
                     '((S a F)  ;; the transition functions
                       (F a F)
                       (F b F))))

(define M3 (make-dfa '(A B C D F)
                     '(a b)
                     'A
                     '(F)
                     '((A a A) (A b B) (B a C) (B b B) (F a F)
                               (C a A) (C b D) (D a F) (D b B)
                               (F b F))))

;; valid input: aaabbb 
(define P (make-ndpda '(S F)
                     '(a b)
                     '(c)
                     'S
                     '(F)
                     `(((S ,EMP ,EMP) (F ,EMP))
                       ((F a ,EMP) (F (c)))
                       ((F b (c)) (F ,EMP)))))

;; TODO TEST
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


;; machine input tape-pos (optional)
;; (sm-showtransitions Ma `(,LM b b b b) 2)
; write "a" on tape
(define Ma (make-tm '(S H)                  ;the states
                    `(a b ,LM)              ;the alphabet
                    `(((S ,LM) (S ,RIGHT))  ;the transition relation
                      ((S a) (H a))
                      ((S b) (H a))
                      ((S ,BLANK) (H a)))
                    'S                      ;the starting state
                    '(H)))                  ;the halting states


(sm-showtransitions Ma `(,LM b b b b) 2)
;;(sm-visualize pda-wcw^r)
;;(sm-visualize 'dfa)

;;(sm-visualize Ma)
