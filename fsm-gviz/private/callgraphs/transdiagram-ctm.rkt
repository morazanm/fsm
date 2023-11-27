#lang fsm
(require 2htdp/image)
(require "../lib.rkt" "cg-defs.rkt")

(provide computation-edges)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move-right machine

;; PRE:  tape = (LM w) AND i=k≥0, where w in {a b BLANK}*
;; POST: tape = (LM w) AND i=k+1
(define R (make-tm '(S F)
                   '(a b d)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S d) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))

;.................................................
;; Move-left machine 

;; PRE:  tape = (LM w) AND i=k≥1, where w in {a b BLANK}*
;; POST: tape = (LM w) AND i=k-1
(define L (make-tm '(S H)
                   '(a b d)
                   `(((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
                     ((S d) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))

;.................................................
;; Halt-machine

;; PRE:  tape = (LM w), where w in {a b BLANK}*
;; POST: tape = (LM w)
(define HALT (make-tm '(S)
                      '(a b)
                      '()
                      'S
                      '(S)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=a
(define Wa (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H a))
                      ((S b) (H a))
                      ((S d) (H a))
                      ((S ,BLANK) (H a)))
                    'S
                    '(H)))

;; Tests for Wa
(check-equal? (last (sm-showtransitions Wa `(,LM b) 1))
              `(H 1 (,LM a)))
(check-equal? (last (sm-showtransitions Wa `(,LM b a ,BLANK a) 3))
              `(H 3 (,LM b a a a)))

;.................................................
;; Write b

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=b
(define Wb (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H b))
                      ((S b) (H b))
                      ((S d) (H b))
                      ((S ,BLANK) (H b)))
                    'S
                    '(H)))

;; Tests for Wb
(check-equal? (last (sm-showtransitions Wb `(,LM ,BLANK ,BLANK) 2))
              `(H 2 (,LM ,BLANK b)))
(check-equal? (last (sm-showtransitions Wb `(,LM b b b b) 3))
              `(H 3 (,LM b b b b)))

;.................................................
;; Write BLANK

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=BLANK
(define WB (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H ,BLANK))
                      ((S b) (H ,BLANK))
                      ((S d) (H ,BLANK))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))

;; Tests for WB
(check-equal? (last (sm-showtransitions WB `(,LM a a) 1))
              `(H 1 (,LM ,BLANK a)))
(check-equal? (last (sm-showtransitions WB `(,LM a b b b) 3))
              `(H 3 (,LM a b ,BLANK b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move right twice

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*
(define RR (combine-tms (list R R) '(a b d)))

;; Tests for RR
(check-equal? (ctm-run RR `(,LM b a a) 1)
              `(F 3 (,LM b a a)))
(check-equal? (ctm-run RR `(,LM a a a) 2)
              `(F 4 (,LM a a a ,BLANK)))
(check-equal? (ctm-run RR `(,LM a b b a) 3)
              `(F 5 (,LM a b b a ,BLANK)))
(check-equal? (ctm-run RR `(,LM b) 1)
              `(F 3 (,LM b ,BLANK ,BLANK)))

;.................................................
;; Find right BLANK 

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
;; POST: tape = (LM w) AND i>k AND tape[i] = BLANK AND tape[k+1..i-1] /= BLANK
(define FBR (combine-tms
             (list 0
                   R
                   (cons BRANCH
                         (list (list 'a (list GOTO 0))
                               (list 'b (list GOTO 0))
                               (list 'd (list GOTO 0))
                               (list BLANK (list GOTO 10))))
                   10)
             (list 'a 'b 'd)))

;; Tests for FBR
(check-equal? (ctm-run FBR `(,LM ,BLANK) 1)
              `(F 2 (,LM ,BLANK ,BLANK)))
(check-equal? (ctm-run FBR `(,LM a a b b b ,BLANK a a) 2)
              `(F 6 (,LM a a b b b ,BLANK a a)))
(check-equal? (ctm-run FBR `(,LM ,BLANK ,BLANK b b) 3)
              `(F 5 (,LM ,BLANK ,BLANK b b ,BLANK)))

;.................................................
;; Find left BLANK

;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)∗
;;              AND there exists j<i such that tape[j]=BLANK
;; POST: tape = (LM w) AND i<k AND tape[i]=BLANK AND tape[i+1..|w|] != BLANK
(define FBL (combine-tms
             (list 0
                   L
                   (cons BRANCH
                         (list (list 'a (list GOTO 0))
                               (list 'b (list GOTO 0))
                               (list 'd (list GOTO 0))
                               (list BLANK (list GOTO 1))
                               (list LM (list GOTO 0))))
                   1)
             (list 'a 'b 'd)))

;; Tests for FBL
(check-equal? (ctm-run FBL `(,LM ,BLANK a a b) 4)
              `(H 1 (,LM ,BLANK a a b)))
(check-equal? (ctm-run FBL `(,LM a ,BLANK a b ,BLANK b a b b) 8)
              `(H 5 (,LM a ,BLANK a b ,BLANK b a b b)))

;.................................................
;; write twice to the right

;; PRE:  tape = (LM a1 ... ak ... an) AND i=k>0 AND aj∈{a b BLANK}
;; POST: tape = (LM a1 ... ak ak ak ... an) AND i=k+2
(define WTWICER (combine-tms `(((VAR s)
                                ,R
                                s
                                ,R
                                s))
                             (list 'a 'b LM)))

;; Tests for WTWICER
(check-equal? (ctm-run WTWICER `(,LM a ,BLANK) 1)
              `(h 3 (,LM a a a)))
(check-equal? (ctm-run WTWICER `(,LM b a a a ,BLANK) 1)
              `(h 3 (,LM b b b a _)))
(check-equal? (ctm-run WTWICER `(,LM a b ,BLANK ,BLANK a) 2)
              `(h 4 (,LM a b b b a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computing with Turing machines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f(a b) = a + b

;; A natural number in unary notation (nn) is either:
;; 1. (BLANK)
;; 2. (d nn)

;; numd(x) = number of ds in x
;; States:
;; S: i = 1 and tape = (LM BLANK a BLANK b)
;; A: i <= numd(a) + 2 and tape = (LM BLANK a BLANK b)
;; B: i <= numd(a) + numd(b) + 3 and tape = (LM BLANK a d b)
;; C: i = numd(a) + numd(b) + 2 and tape = (LM BLANK a d b)
;; D: i = numd(a) + numd(b) + 2 and tape = (LM BLANK a b)
;; E: i = numd(a) + numd(b) + 1 and tape = (LM BLANK a b)
;; G: i = numd(a) + numd(b) + 2 and tape = (LM BLANK)
;; F: i = 3                      if a = b = 0
;;      = numd(a) + numd(b) + 2  otherwise and tape = (LM BLANK a b)

;; PRE:  tape = (LM BLANK a BLANK b) AND i = i
;; POST: tape = (LM BLANK a b) AND
;;       i = 3                      if a = b = 0
;;         = numd(a) + numd(b) + 2  otherwise, where numd(x) = nr of ds in x
(define ADD (make-tm '(S A B C D E F G)
                     '(d)
                     `(((S ,BLANK) (A ,RIGHT))
                       ((A d) (A ,RIGHT))
                       ((A ,BLANK) (B d))
                       ((B d) (B ,RIGHT))
                       ((B ,BLANK) (C ,LEFT))
                       ((C d) (D ,BLANK))
                       ((D ,BLANK) (E ,LEFT))
                       ((E d) (F ,RIGHT))
                       ((E ,BLANK) (G ,RIGHT))
                       ((G ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))

;; Tests for ADD
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK ,BLANK ,BLANK) 1))
              `(F 3 (,LM ,BLANK ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK ,BLANK d d d ,BLANK) 1))
              `(F 5 (,LM ,BLANK d d d ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK d d ,BLANK ,BLANK) 1))
              `(F 4 (,LM ,BLANK d d ,BLANK ,BLANK)))
(check-equal? (last (sm-showtransitions ADD `(,LM ,BLANK d d ,BLANK d d d) 1))
              `(F 7 (,LM ,BLANK d d d d d ,BLANK ,BLANK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; f(a b) = a - b

;; A natural number in unary notation (nn) is:
;; 1. (d nn)

;; numd(y) = number of ds in y
;; States:
;; S: i = 1 and tape = (@ _ d* _ d* x)
;; A: i <= numd(a) + 2 and tape = (@ _ d* _ d* x)
;; B: numd(a) + 3 <= i <= numd(a) + numd(b) + 3 and
;;    tape = (@ _ d* _* d* x) v tape = (@ _ (d*-1) _ _* _ (d*-1) x)
;; C: numd(a) + 3 <= i <= numd(a) + numd(b) + 3 and tape = (@ _ d* _* _ (d*-1) x)
;; F: i = 3                      if a = b = 0
;;      = numd(a) + numd(b) + 3  otherwise and tape = (@ _ d* _* _)

;; PRE:  tape = (@ _ a _ b x) AND i = i
;; POST: tape = (@ _ (a-b) _* _) AND
;;       i = 3                      if a = b = 0
;;         = numd(a) + numd(b) + 3  otherwise, where numd(y) = nr of ds in y
(define SUB (make-tm '(S A B C F)
                     '(d x)
                     `(((S ,BLANK) (A ,RIGHT))
                       ((A d) (A ,RIGHT))
                       ((A ,BLANK) (B ,RIGHT))
                       ((B ,BLANK) (B ,RIGHT))
                       ((B d) (C ,BLANK))
                       ((C ,BLANK) (C ,LEFT))
                       ((C d) (B ,BLANK))
                       ((B x) (F ,BLANK)))
                     'S
                     '(F)))

;.................................................

;(sm-graph SUB)
;(sm-visualize SUB)

;.................................................

;; Tests for SUB
(check-equal? (last (sm-showtransitions SUB '(@ _ _ x) 1))
              '(F 3 (@ _ _ _)))
(check-equal? (last (sm-showtransitions SUB '(@ _ d d d _ x) 1))
              '(F 6 (@ _ d d d _ _)))
(check-equal? (last (sm-showtransitions SUB '(@ _ d d _ d x) 1))
              '(F 6 (@ _ d _ _ _ _)))
(check-equal? (last (sm-showtransitions SUB '(@ _ d d d d _ d d x) 1))
              '(F 9 (@ _ d d _ _ _ _ _ _)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy(w) = w BLANK w

;; A ctmd is a list defined as follows:
;; 1. empty list
;; 2. (cons m ctmd), where m is either a tm or a ctmd
;; 3. (cons LABEL ctmd)
;; 4. (cons (list GOTO LABEL) ctmd)
;; 5. (cons (BRANCH (list (list symbol (list GOTO LABEL)))) ctmd)
;; 6. (cons ((VAR symbol) ctmd) ctmd)
;; 7. (cons variable ctmd)

;; PRE:  tape = (LM BLANK w BLANK) and head on blank after w
;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w 
(define COPY (combine-tms
              (list FBL
                    0
                    R
                    (cons BRANCH (list (list BLANK (list GOTO 2))
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))
                                       (list 'd (list GOTO 1))))
                    1
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))
                                       (list 'd (list GOTO 4))))
                    3
                    RR
                    (list GOTO 5)
                    4
                    R
                    (list GOTO 5)
                    5)
              '(a b d)))

;; Tests for COPY
(check-equal? (rest (ctm-run COPY `(,LM ,BLANK ,BLANK ,BLANK) 3))
              `(5 (,LM ,BLANK ,BLANK ,BLANK ,BLANK ,BLANK)))
(check-equal? (rest (ctm-run COPY `(,LM ,BLANK a b b ,BLANK) 5))
              `(9 (,LM ,BLANK a b b ,BLANK a b b ,BLANK)))
#;(check-equal? (rest (ctm-run COPY `(,LM ,BLANK b b b b ,BLANK)) 5)
                `(11 (,LM ,BLANK b b b b ,BLANK b b b b ,BLANK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and implement a ctm that shifts its input word one space to the right.

;; Name: w->_w
;; Σ = {a b}

;; PRE:  tape = (@ w) and i = 1
;; POST: tape = (@ _ w) and i = |w|+2
(define warrow_w (combine-tms
               (list FBR
                     0
                     L
                     (cons BRANCH (list (list 'a (list GOTO 1))
                                        (list 'b (list GOTO 1))
                                        (list 'd (list GOTO 1))
                                        (list '@ (list GOTO 3))
                                        (list '_ (list GOTO 3))))
                     1
                     (list (list VAR 'x)
                           R
                           'x
                           L
                           WB
                           (list GOTO 0))
                     3
                     FBR)
               '(a b d)))

;.................................................
#|             
;; Tests for w->_w
(check-equal? (rest (ctm-run w->_w '(@ a b a) 1))
              '(5 (@ _ a b a _)))
(check-equal? (rest (ctm-run w->_w '(@ _ _) 1))
              '(2 (@ _ _)))
(check-equal? (rest (ctm-run w->_w '(@ a b b a) 1))
              '(6 (@ _ a b b a _)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and implement a ctm that shifts its input word one space to the left.

;; Name: _w->w
;; Σ = {a b}

;; PRE:  tape = (@ _ w) and i = 1
;; POST: tape = (@ w _ _) and i = |w|+2
(define _w->w (combine-tms
               (list 0
                     R
                     (cons BRANCH (list (list 'a (list GOTO 1))
                                        (list 'b (list GOTO 1))
                                        (list 'd (list GOTO 1))
                                        (list '_ (list GOTO 3))))
                     1
                     (list (list VAR 'x)
                           L
                           'x
                           R
                           WB
                           (list GOTO 0))
                     3)
               '(a b d)))

;.................................................
               
;; Tests for _w->w
(check-equal? (rest (ctm-run _w->w '(@ _ a b a) 1))
              '(5 (@ a b a _ _)))
(check-equal? (rest (ctm-run _w->w '(@ _ _) 1))
              '(2 (@ _ _)))
(check-equal? (rest (ctm-run _w->w '(@ _ a b b a) 1))
              '(6 (@ a b b a _ _)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctm to compute mult(a b) = a * b

;; A given number is represented in nr of ds. A d can be part of the first number or of the second number of the equation.
;; In our proof we distinguish between both as follows:
;; 1. ad = a d that is in the first number of the equation 
;; 2. bd = a d that is in the second number of the equation

;; PRE:  tape = '(@ _ ad* _ bd*) and i = 1
;; POST: tape = '(@ _ (_^|ad*|) _ _ (ad* * bd*) _) and i = |ad*| + |ad* * bd*| + 4
(define MULT (combine-tms
              (list R
                    (cons BRANCH (list (list 'd (list GOTO 0))
                                       (list '_ (list GOTO 3))))
                    0
                    WB
                    R
                    (cons BRANCH (list (list 'd (list GOTO 1))
                                       (list '_ (list GOTO 2))))
                    1
                    WB
                    FBR
                    FBR
                    COPY
                    FBL
                    FBL
                    FBL
                    (list GOTO 0)
                    2
                    FBR
                    (list GOTO 4)
                    3
                    R
                    6
                    WB
                    R
                    (cons BRANCH (list (list '_ (list GOTO 4))
                                       (list 'd (list GOTO 6))))
                    4
                    FBL
                    warrow_w
                    FBR)           
              '(d)))

;.................................................
                  
;; Tests for MULT
(check-equal? (rest (ctm-run MULT '(@ _ d _ d d) 1))
              '(7 (@ _ _ _ _ d d _)))
(check-equal? (rest (ctm-run MULT '(@ _ d d d _ d d) 1))
              '(13 (@ _ _ _ _ _ _ d d d d d d _)))
(check-equal? (rest (ctm-run MULT '(@ _ d d d _ d) 1))
              '(10 (@ _ _ _ _ _ _ d d d _)))
(check-equal? (rest (ctm-run MULT '(@ _ d d d d _ d d) 1))
              '(16 (@ _ _ _ _ _ _ _ d d d d d d d d _)))
(check-equal? (rest (ctm-run MULT '(@ _ d d _ d d) 1))
              '(10 (@ _ _ _ _ _ d d d d _)))
(check-equal? (rest (ctm-run MULT '(@ _ _) 1))
              '(5 (@ _ _ _ _ _)))
(check-equal? (rest (ctm-run MULT '(@ _ _ d d _) 1))
              '(6 (@ _ _ _ _ _ _)))
(check-equal? (rest (ctm-run MULT '(@ _ d _ _) 1))
              '(5 (@ _ _ _ _ _)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sm-graph 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample ctm as list
(define COPYL
  '(FBL
    0
    R
    (cons BRANCH (list (list BLANK (list GOTO 2))
                       (list 'a (list GOTO 1))
                       (list 'b (list GOTO 1))
                       (list 'd (list GOTO 1))))
    1
    (list (list VAR 'k)
          WB
          FBR
          FBR
          'k
          FBL
          FBL
          'k
          (list GOTO 0))
    2
    FBR
    L
    (cons BRANCH (list (list BLANK (list GOTO 3))
                       (list 'a (list GOTO 4))
                       (list 'b (list GOTO 4))
                       (list 'd (list GOTO 4))))
    3
    RR
    (list GOTO 5)
    4
    R
    (list GOTO 5)
    5))

;.................................................
;; dot-nodes

;; Sample expressions
(define COPY-NODES
  '(("FBL0" ((color "forestgreen") (shape "rectangle") (label "FBL")))
    ;; 0
    ("R1" ((color "goldenrod1") (shape "diamond") (label "R")))
    ;; 1
    ("WB2" ((color "black") (shape "rectangle") (label "WB")))
    ("FBR3" ((color "black") (shape "rectangle") (label "FBR")))
    ("FBR4" ((color "black") (shape "rectangle") (label "FBR")))
    ("k5" ((color "black") (shape "rectangle") (label "k")))
    ("FBL6" ((color "black") (shape "rectangle") (label "FBL")))
    ("FBL7" ((color "black") (shape "rectangle") (label "FBL")))
    ("k8" ((color "black") (shape "rectangle") (label "k")))
    ;; 2
    ("FBR9" ((color "black") (shape "rectangle") (label "FBR")))
    ("L10" ((color "goldenrod1") (shape "diamond") (label "L")))
    ;; 3
    ("RR11" ((color "black") (shape "rectangle") (label "RR")))
    ;; 4
    ("R12" ((color "black") (shape "rectangle") (label "R")))))

;.................................................

;; (listof ctm) -> (listof node)
;; Purpose: Given a ctm, create a list of nodes in dot format
#;(define (dot-nodes ctm)
  ;; (listof symbol) integer -> (listof integer)
  ;; Purpose: Depending on a list's length, create a list of integers following that length
  (define (int-for-var list int)
    (if (null? list)
        '()
        (cons (+ 1 int)
              (int-for-var (cdr list) (+ 1 int)))))  
  ;; (listof ctm) integer -> (listof node)
  ;; Purpose: Helper function to dot-nodes
  (define (new-node ctm int)
    (cond [(null? ctm) '()]
          [(symbol? ctm)
           (list (list (string-append (symbol->string ctm) (number->string int))
                       `((color "black") (shape "rectangle") (label ,(symbol->string ctm)))))]              
          [(or (number? (car ctm))
               (and (pair? (car ctm))
                    (or (equal? (cadr (car ctm)) 'GOTO)
                        (equal? (cadr (car ctm)) 'BRANCH)))) (new-node (cdr ctm) int)]
          [(and (pair? (car ctm))
                (equal? (cadr (cadr (car ctm))) VAR))
           (append (append-map (lambda (x x2) (new-node (if (pair? x)
                                                            (cdr x)
                                                            x)
                                                        x2))
                               (drop-right (cddr (car ctm)) 1)
                               (int-for-var (drop-right (cddr (car ctm)) 1) (- int 1)))
                   (new-node (cdr ctm) (+ int (length (drop-right (cddr (car ctm)) 1)))))]
          [else 
           (let* [(a-node
                   (string-append (symbol->string (car ctm)) (number->string int)))
                  (a-color
                   (cond [(= int 0) "forestgreen"]
                         [(and (> (length ctm) 1)
                               (pair? (cadr ctm))
                               (equal? (cadr (cadr ctm)) 'BRANCH)) "goldenrod1"]
                         [else "black"]))
                  (a-shape
                   (cond [(and (> (length ctm) 1)
                               (pair? (cadr ctm))
                               (equal? (cadr (cadr ctm)) 'BRANCH)) "diamond"]
                         [else "rectangle"]))
                  (a-label
                   (symbol->string (car ctm)))]
             (cons (list a-node
                         `((color ,a-color) (shape ,a-shape) (label ,a-label)))
                   (new-node (cdr ctm) (+ int 1))))]))
  (new-node ctm 0))
(define (dot-nodes ctm)
  ;; (listof symbol) integer -> (listof integer)
  ;; Purpose: Depending on a list's length, create a list of integers following that length
  (define (int-for-var list int)
    (if (null? list)
        '()
        (cons (+ 1 int)
              (int-for-var (cdr list) (+ 1 int)))))  
  ;; (listof ctm) integer -> (listof node)
  ;; Purpose: Helper function to dot-nodes
  ;; Accumulator invariant:
  ;; 
  (define (new-node ctm int label-acc)
    (cond [(null? ctm) '()]
          [(symbol? ctm)
           (list (list (string-append (symbol->string ctm) (number->string int))
                       `((color "black") (shape "rectangle") (label ,(symbol->string ctm)))))]
          [(and (number? (car ctm))
                (member (car ctm) label-acc))
           '()]
          [(number? (car ctm))
           (new-node (cdr ctm) int (cons (car ctm) label-acc))]
          [(and (pair? (car ctm))
                    (or (equal? (cadr (car ctm)) 'GOTO)
                        (equal? (cadr (car ctm)) 'BRANCH))) (new-node (cdr ctm) int label-acc)]
          [(and (pair? (car ctm))
                (equal? (cadr (cadr (car ctm))) VAR))
           (append (append-map (lambda (x x2) (new-node (if (pair? x)
                                                            (cdr x)
                                                            x)
                                                        x2
                                                        label-acc))
                               (drop-right (cddr (car ctm)) 1)
                               (int-for-var (drop-right (cddr (car ctm)) 1) (- int 1)))
                   (new-node (cdr ctm) (+ int (length (drop-right (cddr (car ctm)) 1))) label-acc))]
          [else 
           (let* [(a-node
                   (string-append (symbol->string (car ctm)) (number->string int)))
                  (a-color
                   (cond [(= int 0) "forestgreen"]
                         [(and (> (length ctm) 1)
                               (pair? (cadr ctm))
                               (equal? (cadr (cadr ctm)) 'BRANCH)) "goldenrod1"]
                         [else "black"]))
                  (a-shape
                   (cond [(and (> (length ctm) 1)
                               (pair? (cadr ctm))
                               (equal? (cadr (cadr ctm)) 'BRANCH)) "diamond"]
                         [else "rectangle"]))
                  (a-label
                   (symbol->string (car ctm)))]
             (cons (list a-node
                         `((color ,a-color) (shape ,a-shape) (label ,a-label)))
                   (new-node (cdr ctm) (+ int 1) label-acc)))]))
  (new-node ctm 0 '()))

;.................................................

;; Tests for dot-nodes
(check-equal? (dot-nodes COPYL) COPY-NODES)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dot-edges

;; Sample expressions
(define COPY-EDGES
  '(("FBL0" "R1" ((label "") (style "solid")))
    ;; 0
    ("R1" "FBR9" ((label BLANK) (style "dashed")))
    ("R1" "WB2" ((label a) (style "dashed")))
    ("R1" "WB2" ((label b) (style "dashed")))
    ("R1" "WB2" ((label d) (style "dashed")))
    ;; 1
    ("WB2" "FBR3" ((label "") (style "solid")))
    ("FBR3" "FBR4" ((label "") (style "solid")))
    ("FBR4" "k5" ((label "") (style "solid")))
    ("k5" "FBL6" ((label "") (style "solid")))
    ("FBL6" "FBL7" ((label "") (style "solid")))
    ("FBL7" "k8" ((label "") (style "solid")))
    ("k8" "R1" ((label "") (style "solid")))
    ;; 2
    ("FBR9" "L10" ((label "") (style "solid")))
    ("L10" "RR11" ((label BLANK) (style "dashed")))
    ("L10" "R12" ((label a) (style "dashed")))
    ("L10" "R12" ((label b) (style "dashed")))
    ("L10" "R12" ((label d) (style "dashed")))))

;.................................................

;; (listof ctm) -> (listof edge)
;; Purpose: Given a ctm, create a list of edges in dot format
;; Accumulator Invariants:
;;  acc = given ctm list
(define (dot-edges ctm)
  ;; integer (listof ctm) -> (listof ctm)
  ;; Purpose: Given a label, return the rest of the given ctm list (acc) after reaching that label
  (define (goto-label label ctm)
    (if (equal? label (car ctm))
        (cdr ctm)    
        (goto-label label (cdr ctm))))
  ;; integer (listof ctm) integer -> integer
  ;; Purpose: Given a label, traverse a ctm list to find the number of the first tm after that label
  ;; Accumulator invariants:
  ;;  int = starts with 0 and counts all tms traversed 
  (define (goto-int label ctm int)
    (cond [(equal? label (car ctm)) int]
          [(number? (car ctm)) (goto-int label (cdr ctm) int)]
          [(symbol? (car ctm)) (goto-int label (cdr ctm) (+ 1 int))]
          [(equal? (cadr (car ctm)) 'BRANCH) (goto-int label (cdr ctm) int)]
          [(equal? (cadr (car ctm)) 'GOTO) (goto-int label (cdr ctm) int)]
          [(equal? (cadr (cadr (car ctm))) 'VAR) (goto-int label (cdr ctm) (+ int (- (length (car ctm)) 3)))])) 
  ;; (listof symbol) (listof ctm) integer -> (listof edge)
  ;; Purpose: Given a var list, the full ctm list, and the current integer count, create the necessary edges 
  (define (find-var-edges vars acc int)
    ;; (listof symbol) integer -> (listof integer)
    ;; Purpose: Depending on a list's length, create a list of integers starting at the given int and ending at the given list's length
    (define (int-for-var list int)
      (if (null? list)
          '()
          (cons (+ 1 int)
                (int-for-var (cdr list) (+ 1 int))))) 
    (append (append-map (lambda (x x2 x3) (list (list (string-append (symbol->string (if (pair? x)
                                                                                         (car (cdr x))
                                                                                         x))
                                                                     (number->string x3))
                                                      (string-append (symbol->string (if (pair? x2)
                                                                                         (car (cdr x2))
                                                                                         x2))
                                                                     (number->string (+ 1 x3)))
                                                      '((label "") (style "solid")))))
                        (drop (drop-right vars 2) 2)
                        (drop (drop-right vars 1) 3)
                        (int-for-var (drop (drop-right vars 2) 2) (- int 1)))
            (list (list (string-append (symbol->string (if (pair? (last (drop-right vars 1)))
                                                           (car (cdr (last (drop-right vars 1))))
                                                           (last (drop-right vars 1))))
                                       (number->string (+ int (length (drop (drop-right vars 2) 2)))))
                        (string-append (symbol->string (car (goto-label (caddr (last vars)) acc)))
                                       (number->string (goto-int (caddr (last vars)) acc 0)))
                        '((label "") (style "solid"))))))
  ;; (listof ctm) (listof ctm) integer -> string      
  ;; Purpose: Given a ctm, the full accumulator ctm, and an integer find the destination state
  (define (find-to-state ctm acc int)
    (cond [(symbol? (cadr ctm))
           (string-append (symbol->string (cadr ctm)) (number->string (+ 1 int)))]
          [(number? (cadr ctm))
           (find-to-state (cdr ctm) acc int)]
          [(equal? 'BRANCH (cadr (cadr ctm)))
           (find-branch-to-state (cadr ctm) acc int)]
          [(equal? 'GOTO (cadr (cadr ctm)))
           (string-append (symbol->string (car (goto-label (caddr (cadr ctm)) acc))) (number->string (goto-int (caddr (cadr ctm)) acc int)))]
          [else
           (symbol->string (caddr (cadr ctm)))]))
  ;; (listof ctm) (listof ctm) integer -> string      
  ;; Purpose: Given a ctm, the full accumulator ctm, and an integer find the destination state in the case of branches
  (define (find-branch-to-state ctm acc int)
    (cond [(symbol? (car ctm))
           (symbol->string (car ctm))]
          [(number? (car ctm))
           (find-branch-to-state (cdr ctm) acc int)]
          [(equal? 'BRANCH (cadr (car ctm)))
           (find-branch-to-state (cadr ctm) acc int)]
          [(equal? 'GOTO (cadr (car ctm)))
           (find-branch-to-state (goto-label (caddr (car ctm)) acc) acc int)]
          [else
           (symbol->string (caddr (car ctm)))]))
  ;; string (listof ctm) (listof ctm) integer -> (listof edge)
  ;; Purpose: Given the source state, the branch, the full accumulated ctm and an integer, make a list of edges from the branch
  (define (find-branch-edges from-state branch acc int)
    (cond [(null? branch) '()]
          [(equal? (car branch) 'list) (find-branch-edges from-state (filter (lambda (x) (not (equal? (caddr (caddr x)) (last acc)))) (cdr branch)) acc int)]
          [else 
           (append (list (list from-state
                               (string-append (find-branch-to-state 
                                               (goto-label (caddr (caddr (car branch))) acc)
                                               acc int)
                                              (number->string (goto-int (caddr (caddr (car branch))) acc 0)))
                               `((label ,(if (pair? (cadr (car branch)))
                                             (car (cdr (cadr (car branch))))
                                             (cadr (car branch))))
                                 (style "dashed"))))
                   (find-branch-edges from-state (cdr branch) acc int))]))
                   ;(new-edge (cdr (goto-label (caddr (caddr (car branch))) acc)) acc (goto-int (caddr (caddr (car branch))) acc 0) label-acc))]))
  ;; (listof ctm) (listof ctm) integer -> (listof edge)
  ;; Purpose: Given a ctm list and an accumulator that contains the initial given ctm list, and an integer, create the list of edges
  (define (new-edge ctm acc int label-acc)
    (if (empty? ctm)
        '()
        (cond [(and (symbol? (car ctm))
                    (= (length ctm) 1))
               '()]
              [(and (symbol? (car ctm))
                    (pair? (cadr ctm))
                    (equal? 'BRANCH (cadr (cadr ctm))))
              ;(let ((labels (filter number? (caddr (cadr ctm)))))
               (append (find-branch-edges (string-append (symbol->string (car ctm))
                                                         (number->string int))
                                          (caddr (cadr ctm)) acc int)
                     ;(map (lambda (x) (new-edge (goto-label x acc) (goto-int x acc 0))) labels)
                       (new-edge (cdr ctm) acc (+ 1 int) label-acc))]         
              #;[(symbol? (car ctm))
               (if (and (pair? (cadr ctm))
                        (equal? 'GOTO (cadr (cadr ctm)))
                        (null? (goto-label (caddr (cadr ctm)) acc)))
                   '()
                   (cons (list (string-append (symbol->string (car ctm)) (number->string int))
                               (find-to-state ctm acc int)
                               '((label "") (style "solid")))                
                         (new-edge (cdr ctm) acc (+ 1 int) label-acc)))]
              [(and (symbol? (car ctm))
                    (pair? (cadr ctm))
                    (equal? 'GOTO (cadr (cadr ctm)))
                    (not (null? (goto-label (caddr (cadr ctm)) acc))))

               (cons (list (string-append (symbol->string (car ctm)) (number->string int))
                               (string-append (symbol->string (car (goto-label (caddr (cadr ctm)) acc))) (number->string (goto-int (caddr (cadr ctm)) acc 0)))
                               '((label "") (style "solid"))) 
                ;(new-edge (goto-label (caddr (cadr ctm)) acc) acc (goto-int (caddr (cadr ctm)) acc 0) label-acc)
                       (new-edge (cdr (goto-label (caddr (cadr ctm)) acc)) acc (add1 (goto-int (caddr (cadr ctm)) acc 0)) label-acc))]
                       ;(new-edge (cdr ctm) acc (+ 1 int) label-acc))]
              
              [(symbol? (car ctm))
               (if (and (pair? (cadr ctm))
                        (equal? 'GOTO (cadr (cadr ctm)))
                        (null? (goto-label (caddr (cadr ctm)) acc)))
                   '()
                   (cons (list (string-append (symbol->string (car ctm)) (number->string int))
                               (find-to-state ctm acc int)
                               '((label "") (style "solid")))                
                         (new-edge (cdr ctm) acc (+ 1 int) label-acc)))]
              [(and (number? (car ctm))
                    (< 1 (count (lambda (x) (= (car ctm) x)) label-acc))) '()]
              [(number? (car ctm)) (new-edge (cdr ctm) acc int (cons (car ctm) label-acc))]
              [(equal? 'BRANCH (cadr (car ctm))) (new-edge (cdr ctm) acc int label-acc)]
              #;[(equal? 'GOTO (cadr (car ctm)))
               (new-edge (goto-label (caddr (car ctm)) acc) acc int label-acc)]
              [(equal? 'GOTO (cadr (car ctm)))
               (new-edge (goto-label (caddr (car ctm)) acc) acc int label-acc)]
              [(equal? 'VAR (cadr (cadr (car ctm))))
               (append (find-var-edges (car ctm) acc int)
                       (new-edge (cdr ctm) acc (+ int (- (length (car ctm)) 3)) label-acc))])))
  (new-edge ctm ctm 0 '()))
#;(define (dot-edges ctm)
  ;; integer (listof ctm) -> (listof ctm)
  ;; Purpose: Given a label, return the rest of the given ctm list (acc) after reaching that label
  (define (goto-label label ctm)
    (if (equal? label (car ctm))
        (cdr ctm)    
        (goto-label label (cdr ctm))))
  ;; integer (listof ctm) integer -> integer
  ;; Purpose: Given a label, traverse a ctm list to find the number of the first tm after that label
  ;; Accumulator invariants:
  ;;  int = starts with 0 and counts all tms traversed 
  (define (goto-int label ctm int)
    (cond [(equal? label (car ctm)) int]
          [(number? (car ctm)) (goto-int label (cdr ctm) int)]
          [(symbol? (car ctm)) (goto-int label (cdr ctm) (+ 1 int))]
          [(equal? (cadr (car ctm)) 'BRANCH) (goto-int label (cdr ctm) int)]
          [(equal? (cadr (car ctm)) 'GOTO) (goto-int label (cdr ctm) int)]
          [(equal? (cadr (cadr (car ctm))) 'VAR) (goto-int label (cdr ctm) (+ int (- (length (car ctm)) 3)))])) 
  ;; (listof symbol) (listof ctm) integer -> (listof edge)
  ;; Purpose: Given a var list, the full ctm list, and the current integer count, create the necessary edges 
  (define (find-var-edges vars acc int)
    ;; (listof symbol) integer -> (listof integer)
    ;; Purpose: Depending on a list's length, create a list of integers starting at the given int and ending at the given list's length
    (define (int-for-var list int)
      (if (null? list)
          '()
          (cons (+ 1 int)
                (int-for-var (cdr list) (+ 1 int))))) 
    (append (append-map (lambda (x x2 x3) (list (list (string-append (symbol->string (if (pair? x)
                                                                                         (car (cdr x))
                                                                                         x))
                                                                     (number->string x3))
                                                      (string-append (symbol->string (if (pair? x2)
                                                                                         (car (cdr x2))
                                                                                         x2))
                                                                     (number->string (+ 1 x3)))
                                                      '((label "") (style "solid")))))
                        (drop (drop-right vars 2) 2)
                        (drop (drop-right vars 1) 3)
                        (int-for-var (drop (drop-right vars 2) 2) (- int 1)))
            (list (list (string-append (symbol->string (if (pair? (last (drop-right vars 1)))
                                                           (car (cdr (last (drop-right vars 1))))
                                                           (last (drop-right vars 1))))
                                       (number->string (+ int (length (drop (drop-right vars 2) 2)))))
                        (string-append (symbol->string (car (goto-label (caddr (last vars)) acc)))
                                       (number->string (goto-int (caddr (last vars)) acc 0)))
                        '((label "") (style "solid"))))))
  ;; (listof ctm) (listof ctm) integer -> string      
  ;; Purpose: Given a ctm, the full accumulator ctm, and an integer find the destination state
  (define (find-to-state ctm acc int)
    (cond [(symbol? (cadr ctm))
           (string-append (symbol->string (cadr ctm)) (number->string (+ 1 int)))]
          [(number? (cadr ctm))
           (find-to-state (cdr ctm) acc int)]
          [(equal? 'BRANCH (cadr (cadr ctm)))
           (find-branch-to-state (cadr ctm) acc int)]
          [(equal? 'GOTO (cadr (cadr ctm)))
           (find-to-state (goto-label (caddr (cadr ctm)) acc) acc int)]
          [else
           (symbol->string (caddr (cadr ctm)))]))
  ;; (listof ctm) (listof ctm) integer -> string      
  ;; Purpose: Given a ctm, the full accumulator ctm, and an integer find the destination state in the case of branches
  (define (find-branch-to-state ctm acc int)
    (cond [(symbol? (car ctm))
           (symbol->string (car ctm))]
          [(number? (car ctm))
           (find-branch-to-state (cdr ctm) acc int)]
          [(equal? 'BRANCH (cadr (car ctm)))
           (find-branch-to-state (cadr ctm) acc int)]
          [(equal? 'GOTO (cadr (car ctm)))
           (find-branch-to-state (goto-label (caddr (car ctm)) acc) acc int)]
          [else
           (symbol->string (caddr (car ctm)))]))
  ;; string (listof ctm) (listof ctm) integer -> (listof edge)
  ;; Purpose: Given the source state, the branch, the full accumulated ctm and an integer, make a list of edges from the branch
  (define (find-branch-edges from-state branch acc int)
    (cond [(null? branch) '()]
          [(equal? (car branch) 'list) (find-branch-edges from-state (filter (lambda (x) (not (equal? (caddr (caddr x)) (last acc)))) (cdr branch)) acc int)]
          [else 
           (append (list (list from-state
                               (string-append (find-branch-to-state 
                                               (goto-label (caddr (caddr (car branch))) acc)
                                               acc int)
                                              (number->string (goto-int (caddr (caddr (car branch))) acc 0)))
                               `((label ,(if (pair? (cadr (car branch)))
                                             (car (cdr (cadr (car branch))))
                                             (cadr (car branch))))
                                 (style "dashed"))))
                   (find-branch-edges from-state (cdr branch) acc int))]))
  ;; (listof ctm) (listof ctm) integer -> (listof edge)
  ;; Purpose: Given a ctm list and an accumulator that contains the initial given ctm list, and an integer, create the list of edges
  (define (new-edge ctm acc int label-acc)
    (if (empty? ctm)
        '()
        (cond [(and (symbol? (car ctm))
                    (pair? (cadr ctm))
                    (equal? 'BRANCH (cadr (cadr ctm))))
               (append (find-branch-edges (string-append (symbol->string (car ctm))
                                                         (number->string int))
                                          (caddr (cadr ctm)) acc int)
                       (new-edge (cdr ctm) acc (+ 1 int) label-acc))]         
              [(symbol? (car ctm))
               (if (and (pair? (cadr ctm))
                        (equal? 'GOTO (cadr (cadr ctm)))
                        (null? (goto-label (caddr (cadr ctm)) acc)))
                   '()
                   (cons (list (string-append (symbol->string (car ctm)) (number->string int))
                               (find-to-state ctm acc int)
                               '((label "") (style "solid")))                
                         (new-edge (cdr ctm) acc (+ 1 int) label-acc)))]
              #;[(and (number? (car ctm))
                    (< 30 (count (lambda (x) (= (car ctm) x)) label-acc)))
               '()]
              [(number? (car ctm)) new-edge (cdr ctm) acc int (cons (car ctm) label-acc)]
              [(equal? 'BRANCH (cadr (car ctm))) (new-edge (cdr ctm) acc int label-acc)]
              [(equal? 'GOTO (cadr (car ctm)))
               (new-edge (goto-label (caddr (car ctm)) acc) acc int label-acc)]
              [(equal? 'VAR (cadr (cadr (car ctm))))
               (append (find-var-edges (car ctm) acc int)
                       (new-edge (cdr ctm) acc (+ int (- (length (car ctm)) 3)) label-acc))])))
  (new-edge ctm ctm 0 '()))

;.................................................

;; Tests for dot-edges
(check-equal? (dot-edges COPYL) COPY-EDGES)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .dot files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transition-diagram-ctm

;; fsm word [integer] -> image
;; Purpose: Given a ctm as list, create a .png file from a .dot file, and return a bitmap
;; Color blindness:
;;  0 - default colors
;;  1 - Deuteranopia
(define (transition-diagram-ctm ctm)
  (define fname "fsm")
  ;; image
  ;; Purpose: Store a graph image 
  (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'fontsize 13)))
  (begin
    (set! cgraph
          (foldl
           (lambda (a-node a-graph)
             (let* [(state (string->symbol (first a-node)))
                    (color (second (first (second a-node))))
                    (shape (second (second (second a-node))))
                    (label (second (third (second a-node))))]
               (add-node a-graph state #:atb (hash 'color color 'shape shape 'label label)))) 
           cgraph   
           (dot-nodes ctm)))
    (set! cgraph
          (foldl
           (lambda (a-trans a-graph)
             (let* [(state1 (string->symbol (first a-trans)))
                    (state2(string->symbol (second a-trans)))
                    (label (if (symbol? (second (first (third a-trans))))
                               (symbol->string (second (first (third a-trans))))
                               (second (first (third a-trans)))))
                    (style (second (second (third a-trans))))] 
               (add-edge a-graph label state1 state2 #:atb (hash 'style style))))
           cgraph
           (dot-edges ctm)))
    (let [(res (graph->bitmap cgraph))]
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define FBRL '(0
               R
               (cons BRANCH
                     (list (list 'a (list GOTO 0))
                           (list 'b (list GOTO 0))
                           (list 'd (list GOTO 0))
                           (list BLANK (list GOTO 10))))
               10))
(define FBLL '(0
               L
               (cons BRANCH
                     (list (list 'a (list GOTO 0))
                           (list 'b (list GOTO 0))
                           (list 'd (list GOTO 0))
                           (list BLANK (list GOTO 1))
                           (list LM (list GOTO 0))))
               1))
(define TWICERL '(0
                  R
                  (cons BRANCH (list (list 'a (list GOTO 1))
                                     (list 'b (list GOTO 1))
                                     (list 'd (list GOTO 1))
                                     (list '_ (list GOTO 3))))
                  1
                  (list (list VAR 'x)
                        L
                        'x
                        R
                        WB
                        (list GOTO 0))
                  3))
#;(define MULTL '(R
                (cons BRANCH (list (list 'd (list GOTO 0))
                                   (list '_ (list GOTO 3))))
                0
                WB
                R
                (cons BRANCH (list (list 'd (list GOTO 1))
                                   (list '_ (list GOTO 2))))
                1
                WB
                FBR
                FBR
                COPY
                FBL
                FBL
                FBL
                (list GOTO 0)
                2
                FBR
                (list GOTO 4)
                3
                R
                (list GOTO 5)
                5
                (cons BRANCH (list (list '_ (list GOTO 4))
                                   (list 'd
                                         ;WB
                                         ;R
                                         (list GOTO 5))))
                4
                FBL
                warrow_w
                FBR))

(define MULTL '(R
                (cons BRANCH (list (list 'd (list GOTO 0))
                                   (list '_ (list GOTO 3))))
                0
                WB
                R
                (cons BRANCH (list (list 'd (list GOTO 1))
                                   (list '_ (list GOTO 2))))
                1
                WB
                FBR
                FBR
                COPY
                FBL
                FBL
                FBL
                (list GOTO 0)
                2
                FBR
                (list GOTO 4)
                3
                R
                (list GOTO 5)
                5
                (cons BRANCH (list (list '_ (list GOTO 4))
                                   (list 'd (list GOTO 6))))
                6
                WB
                R
                (list GOTO 5)
                4
                FBL
                warrow_w
                FBR))

(define  WARROW_L '(FBR
                   0
                   L
                   (cons BRANCH (list (list 'a (list GOTO 1))
                                      (list 'b (list GOTO 1))
                                      (list 'd (list GOTO 1))
                                      (list '@ (list GOTO 3))
                                      (list '_ (list GOTO 3))))
                   1
                   (list (list VAR 'x)
                         R
                         'x
                         L
                         WB
                         (list GOTO 0))
                   3
                   FBR))

(define _WARROWL '(0
                   R
                   (cons BRANCH (list (list 'a (list GOTO 1))
                                      (list 'b (list GOTO 1))
                                      (list 'd (list GOTO 1))
                                      (list '_ (list GOTO 3))))
                   1
                   (list (list VAR 'x)
                         L
                         'x
                         R
                         WB
                         (list GOTO 0))
                   3))

;(transition-diagram-ctm COPYL)
;(transition-diagram-ctm FBRL)
;(transition-diagram-ctm FBLL)
;(transition-diagram-ctm TWICERL)
;(transition-diagram-ctm WARROW_L)
;(transition-diagram-ctm _WARROWL)
;(transition-diagram-ctm MULTL)

;(ctm-run COPY '(_ a b _) 1 #:trace #t)

#|(define (trace-of-edges trace edges)
  (if (empty? trace)
      '()
      (append (filter (lambda (x) (option x|#


#;'(("FBL0" "R1" ((label "") (style "solid")))
  ("R1" "FBR9" ((label BLANK) (style "dashed")))
  ("R1" "WB2" ((label a) (style "dashed")))
  ("R1" "WB2" ((label b) (style "dashed")))
  ("R1" "WB2" ((label d) (style "dashed")))
  ("WB2" "FBR3" ((label "") (style "solid")))
  ("FBR3" "FBR4" ((label "") (style "solid")))
  ("FBR4" "k5" ((label "") (style "solid")))
  ("k5" "FBL6" ((label "") (style "solid")))
  ("FBL6" "FBL7" ((label "") (style "solid")))
  ("FBL7" "k8" ((label "") (style "solid")))
  ("k8" "R1" ((label "") (style "solid")))
  ("FBR9" "L10" ((label "") (style "solid")))
  ("L10" "RR11" ((label BLANK) (style "dashed")))
  ("L10" "R12" ((label a) (style "dashed")))
  ("L10" "R12" ((label b) (style "dashed")))
  ("L10" "R12" ((label d) (style "dashed"))))

#;'(FBL
  0
  R
  (cons BRANCH (list (list BLANK (list GOTO 2)) (list 'a (list GOTO 1)) (list 'b (list GOTO 1)) (list 'd (list GOTO 1))))
  1
  (list (list VAR 'k) WB FBR FBR 'k FBL FBL 'k (list GOTO 0))
  2
  FBR
  L
  (cons BRANCH (list (list BLANK (list GOTO 3)) (list 'a (list GOTO 4)) (list 'b (list GOTO 4)) (list 'd (list GOTO 4))))
  3
  RR
  (list GOTO 5)
  4
  R
  (list GOTO 5)
  5)

#;(define COPY-TRACE
'((tmconfig 'H 0 '(_ a b _))
 (tmconfig 'F 1 '(_ a b _))
 '(BRANCH a)
 '(GOTO 1)
 '(VAR k a)
 (tmconfig 'H 1 '(_ _ b _))
 (tmconfig 'F 3 '(_ _ b _))
 (tmconfig 'F 4 '(_ _ b _ _))
 (tmconfig 'h 4 '(_ _ b _ a))
 (tmconfig 'H 3 '(_ _ b _ a))
 (tmconfig 'H 1 '(_ _ b _ a))
 (tmconfig 'h 1 '(_ a b _ a))
 '(GOTO 0)
 (tmconfig 'F 2 '(_ a b _ a))
 '(BRANCH b)
 '(GOTO 1)
 '(VAR k b)
 (tmconfig 'H 2 '(_ a _ _ a))
 (tmconfig 'F 3 '(_ a _ _ a))
 (tmconfig 'F 5 '(_ a _ _ a _))
 (tmconfig 'h 5 '(_ a _ _ a b))
 (tmconfig 'H 3 '(_ a _ _ a b))
 (tmconfig 'H 2 '(_ a _ _ a b))
 (tmconfig 'h 2 '(_ a b _ a b))
 '(GOTO 0)
 (tmconfig 'F 3 '(_ a b _ a b))
 '(BRANCH _)
 '(GOTO 2)
 (tmconfig 'F 6 '(_ a b _ a b _))
 (tmconfig 'H 5 '(_ a b _ a b _))
 '(BRANCH b)
 '(GOTO 4)
 (tmconfig 'F 6 '(_ a b _ a b _))
 '(GOTO 5)
 (tmconfig 'F 6 '(_ a b _ a b _))))

(define COPYL2
  '(FBL
    0
    R
    (cons BRANCH (list (list _ (list GOTO 2)) (list 'a (list GOTO 1)) (list 'b (list GOTO 1)) (list 'd (list GOTO 1))))
    1
    (list (list VAR 'k) WB FBR FBR 'k FBL FBL 'k (list GOTO 0))
    2
    FBR
    L
    (cons BRANCH (list (list _ (list GOTO 3)) (list 'a (list GOTO 4)) (list 'b (list GOTO 4)) (list 'd (list GOTO 4))))
    3
    RR
    (list GOTO 5)
    4
    R
    (list GOTO 5)
    5))

(define COPY-EDGES2
  '(("FBL0" "R1" ((label "") (style "solid")))
    ("R1" "FBR9" ((label _) (style "dashed")))
    ("R1" "WB2" ((label a) (style "dashed")))
    ("R1" "WB2" ((label b) (style "dashed")))
    ("R1" "WB2" ((label d) (style "dashed")))
    ("WB2" "FBR3" ((label "") (style "solid")))
    ("FBR3" "FBR4" ((label "") (style "solid")))
    ("FBR4" "k5" ((label "") (style "solid")))
    ("k5" "FBL6" ((label "") (style "solid")))
    ("FBL6" "FBL7" ((label "") (style "solid")))
    ("FBL7" "k8" ((label "") (style "solid")))
    ("k8" "R1" ((label "") (style "solid")))
    ("FBR9" "L10" ((label "") (style "solid")))
    ("L10" "RR11" ((label _) (style "dashed")))
    ("L10" "R12" ((label a) (style "dashed")))
    ("L10" "R12" ((label b) (style "dashed")))
    ("L10" "R12" ((label d) (style "dashed")))))


;; ctm (listof ctm) tape int -> (listof edge)
;; Purpose: Given a ctm, a ctm list, a tape, and a head position, returns the edges traversed in the computation
(define (computation-edges ctm ctmlist tape head) 
  ;; (listof trace) (listof edge) string -> (listof edge)
  ;; Purpose: Returns the traced edges in order
  ;; Accumulator invariant:
  ;;  stored-val = stores the destination state, which is the source state of the following edge
  ;;  edges = list of all edges
  (define (follow-trace trace edges stored-val)
    (cond [(or (empty? trace)
               (empty? (cdr trace))
               (equal? stored-val "")) '()]
          [(and (struct? (car trace))
                (struct? (cadr trace)))
           (let ((new-edge (filter (lambda (x) (equal? (car x) stored-val)) edges)))
             (append new-edge
                     (follow-trace (cdr trace) edges (if (empty? new-edge)
                                                         ""
                                                         (cadr (car new-edge))))))]
          [(and (struct? (car trace))
                (equal? 'BRANCH (car (cadr trace))))
           (let ((new-edge (filter (lambda (x) (and (equal? (car x) stored-val)
                                                    (equal? (cadr (car (caddr x))) (cadr (cadr trace))))) edges)))
             (append new-edge
                     (follow-trace (cdr trace) edges (if (empty? new-edge)
                                                         ""
                                                         (cadr (car new-edge))))))]
          [(struct? (car trace))
           (let ((new-edge (filter (lambda (x) (equal? (car x) stored-val)) edges)))
             (append new-edge
                     (follow-trace (cdr trace) edges (if (empty? new-edge)
                                                         ""
                                                         (cadr (car new-edge))))))]
          [(or (equal? 'GOTO (car (car trace)))
               (equal? 'BRANCH (car (car trace)))
               (equal? 'VAR (car (car trace))))
           (follow-trace (cdr trace) edges stored-val)]))
  (follow-trace (cdr (ctm-run ctm tape head #:trace #t)) (dot-edges ctmlist) (car (car (dot-edges ctmlist)))))


(define COPY-COMPUTATION
  '(("FBL0" "R1" ((label "") (style "solid")))
    ("R1" "WB2" ((label a) (style "dashed")))
    ("WB2" "FBR3" ((label "") (style "solid")))
    ("FBR3" "FBR4" ((label "") (style "solid")))
    ("FBR4" "k5" ((label "") (style "solid")))
    ("k5" "FBL6" ((label "") (style "solid")))
    ("FBL6" "FBL7" ((label "") (style "solid")))
    ("FBL7" "k8" ((label "") (style "solid")))
    ("k8" "R1" ((label "") (style "solid")))
    ("R1" "WB2" ((label b) (style "dashed")))
    ("WB2" "FBR3" ((label "") (style "solid")))
    ("FBR3" "FBR4" ((label "") (style "solid")))
    ("FBR4" "k5" ((label "") (style "solid")))
    ("k5" "FBL6" ((label "") (style "solid")))
    ("FBL6" "FBL7" ((label "") (style "solid")))
    ("FBL7" "k8" ((label "") (style "solid")))
    ("k8" "R1" ((label "") (style "solid")))
    ("R1" "FBR9" ((label _) (style "dashed")))
    ("FBR9" "L10" ((label "") (style "solid")))
    ("L10" "R12" ((label b) (style "dashed")))))

;(follow-trace COPY-TRACE COPY-EDGES2 "FBL0")
;(computation-edges COPY COPYL '(_ a b) 1)

(check-equal? (computation-edges COPY COPYL2 '(_ a b) 1) COPY-COMPUTATION)
 


                                          
