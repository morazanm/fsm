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

(define (S-INV tape pos) #true)
(define (H-INV tape pos) (eq? 'a (list-ref tape pos)))
;;(sm-visualize Ma (list 'S S-INV) (list 'H H-INV))



;; ----- TM language recognizer -----

;; input '(a a a a)
(define Alla (make-tm '(S Y N)
                      `(a b ,LM)
                      `(((S a) (S ,RIGHT))
                        ((S b) (N b))
                        ((S ,BLANK) (Y ,BLANK)))
                      'S
                      '(Y N)
                      'Y))

(sm-visualize 'tm-language-recognizer)