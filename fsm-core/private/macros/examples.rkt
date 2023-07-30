#lang fsm
(require "constructors.rkt"
         "../constants.rkt"
         ;"../fsa.rkt"
         ;"../tm.rkt"
         #;"../../../main.rkt")
(local-require test-engine/racket-tests)

(define dfa-temp (make-dfa2
                  '(A B)
                  '(a b)
                  'A
                  '(B)
                  (list '(A b A)
                        '(A a B)
                        '(B b A))
                  #:accepts (list '(b b b b a))
                  #:rejects (list '(a a a a b))))

(check-error
 (make-dfa2
  `(A B)
  '(a b)
  'A
  '(A)
  (list '(A a A)
        '(B a A)
        '(B b B)
        '(A b B))
  #f
  #:accepts (list '(a a a b) '(a a))
  #:rejects (list '(b b b b) '(b b))
  )
 "Does not accept the predicted value:  ((a a a b))")

(define wcw^r (make-ndpda2 '(S P Q F)
                           '(a b c)
                           '(a b)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (P ,EMP))
                             ((P a ,EMP) (P (a)))
                             ((P b ,EMP) (P (b)))
                             ((P c ,EMP) (Q ,EMP))
                             ((Q a (a)) (Q ,EMP))
                             ((Q b (b)) (Q ,EMP))
                             ((Q ,EMP ,EMP) (F ,EMP)))
                           #:accepts (list '(c))
                           #:rejects (list '(a a a b b))
                           ))

(define a^nb^n (make-ndpda '(S M F)
                           '(a b)
                           '(a)
                           'S
                           '(F)
                           `(((S ,EMP ,EMP) (M ,EMP))
                             ((S a ,EMP) (S (a)))
                             ((M b (a)) (M ,EMP))
                             ((M ,EMP ,EMP) (F ,EMP)))))

(define ndfa-temp (make-ndfa2
                  '(A B)
                  '(a b)
                  'A
                  '(B)
                  `((A b A)
                    (A a B)
                    (B ,EMP A))
                  #:accepts (list '(b b b b a))
                  #:rejects (list '(a a a a b))))

(define tm-temp (make-tm2
                  '(S Y N)
                    `(a b)
                    `(((S a) (S ,RIGHT))
                      ((S b) (N b))
                      ((S ,BLANK) (Y ,BLANK)))
                    'S
                    '(Y N)
                    'Y
                  #:accepts (list '(a a a) '(a a))
                  #:rejects (list '(b b b b) '(b b))
                  ))
(dfa-temp '(a a a a))
(sm-apply tm-temp`(,LM a a a b a a))

(define EQABC (make-mttm2 '(S Y N C D E F G)
                         '(a b c)
                         'S
                         '(Y N)
                         (list ;; read all blanks and move all R
                          (list (list 'S (list BLANK BLANK BLANK BLANK))
                                (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
                          ;; read a on t0, copy to t1 and then move R on t0 and t1
                          (list (list 'C (list 'a BLANK BLANK BLANK))
                                (list 'D (list 'a 'a BLANK BLANK)))
                          (list (list 'D (list 'a 'a BLANK BLANK))
                                (list 'C (list RIGHT RIGHT BLANK BLANK)))
                          ;; read b on t0, copy to t2 and then move R on t0 and t2
                          (list (list 'C (list 'b BLANK BLANK BLANK))
                                (list 'E (list 'b BLANK 'b BLANK)))
                          (list (list 'E (list 'b BLANK 'b BLANK))
                                (list 'C (list RIGHT BLANK RIGHT BLANK)))
                          ;; read c on t0, copy to t3 and then move R on t0 and t3
                          (list (list 'C (list 'c BLANK BLANK BLANK))
                                (list 'F (list 'c BLANK BLANK 'c)))
                          (list (list 'F (list 'c BLANK BLANK 'c))
                                (list 'C (list RIGHT BLANK BLANK RIGHT)))
                          ;; read BLANK on t0, move L on t1, t2 and t3
                          (list (list 'C (list BLANK BLANK BLANK BLANK))
                                (list 'G (list BLANK LEFT LEFT LEFT)))
                          ;; read BLANK on all tapes, move to Y
                          (list (list 'G (list BLANK BLANK BLANK BLANK))
                                (list 'Y (list BLANK BLANK BLANK BLANK)))
                          ;; read a, b, c on t1, t2, and t3 them move L on t1, t2, t3
                          (list (list 'G (list BLANK 'a 'b 'c))
                                (list 'G (list BLANK LEFT LEFT LEFT)))
                          ;; too many of at least 1 letter
                          (list (list 'G (list BLANK BLANK 'b 'c))
                                (list 'N (list BLANK BLANK 'b 'c)))
                          (list (list 'G (list BLANK 'a BLANK 'c))
                                (list 'N (list BLANK 'a BLANK 'c)))
                          (list (list 'G (list BLANK 'a 'b BLANK))
                                (list 'N (list BLANK 'a 'b BLANK)))
                          (list (list 'G (list BLANK BLANK BLANK 'c))
                                (list 'N (list BLANK BLANK BLANK 'c)))
                          (list (list 'G (list BLANK BLANK 'b BLANK))
                                (list 'N (list BLANK BLANK 'b BLANK)))
                          (list (list 'G (list BLANK 'a BLANK BLANK))
                                (list 'N (list BLANK 'a BLANK BLANK))))
                         4
                         'Y))
