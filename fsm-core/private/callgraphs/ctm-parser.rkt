#lang fsm
(require eopl)
(require 2htdp/image)
(require "../lib.rkt" "cg-defs.rkt")

(define-datatype expression expression?
  (ctmd-exp
   (ctmd pair?))
  (tm-exp
   (sym symbol?)
   (int integer?)
   (next-tm list? #;expression?))
  (label-exp
   (int integer?))
  (branch-exp
   (branches pair?))
  (goto-exp
   (label expression?))
  (var-exp
   (var symbol?)
   (tms list?)
   (goto expression?))
  (end-exp))

#;(define (find-next-tm tm acclist acclist2)
  (cond ((null? acclist)
      '())
      ((symbol? (car acclist))
       (car acclist))
      ((pair? (car acclist))
       (cond ((equal? 'BRANCH (car (car acclist)))
              ())))
      (else (find-next-tm tm (cdr acclist)))))

(define (branch-exp? exp)
  (cases expression exp
    (branch-exp (b) #t)
    (else #f)))

(define (tm-exp? exp)
  (cases expression exp
    (tm-exp (s i n) #t)
    (else #f)))

(define (tost-name exp)
  (cases expression exp
    (tm-exp (s i n) s)
    (else '())))

;; any -> throws error
;; Purpose: Throw an error in case of invalid syntax
(define (report-invalid-ctm-syntax datum)
  (error (format "Expected a ctmd, given: ~a" datum)))

;; ctmd -> exp
;; Purpose: Parse the ctmd
(define (parse-ctmd datum int acclist)
  (cond ((symbol? datum)
         (tm-exp
          datum
          int
          '()
          #;(find-next-tm datum acclist)))
        ((number? datum)
         (label-exp datum))
        ((pair? datum)
         (cond ((eqv? (car datum) 'BRANCH)
                (branch-exp
                 (cadr datum)))
               ((eqv? (car datum) 'GOTO)
                (goto-exp
                 (label-exp (cadr datum))))
               ((eqv? (car (car datum)) 'VAR)
                (var-exp
                 (if (pair? (cadr (car datum)))
                     (car (cdr (cadr (car datum))))
                     (cadr (car datum)))

                 (drop-right (cdr datum) 1)
                 (parse-ctmd (last datum) int (last datum))))))
        (else (report-invalid-ctm-syntax datum))))

(define (get-int lab)
  (cases expression lab
    (label-exp (int) int)
    (goto-exp (label) (get-int label))
    (else (error "expected label-exp, given: ~a" lab))))

(define (find-goto lab l2)
  (if (null? l2)
      '()
      (cases expression (car l2)
        (label-exp (int)
                   (if (= int (if (integer? lab)
                                  lab
                                  (get-int lab)))
                       (cdr l2)
                       (find-goto lab (cdr l2))))
        (else (find-goto lab (cdr l2))))))

(define (find-next-tm l l2)
  (if (null? l)
      '()
      (cases expression (car l)
        (tm-exp (sym int next-tm)
                (string-append (symbol->string sym) (number->string int)))
        (branch-exp (branches)
                    branches)
        (goto-exp (label)
                  (find-next-tm (find-goto label l2) l2))
        (var-exp (var tms goto)
                 (string-append (symbol->string (car tms)) "var0"))
        (else (find-next-tm (cdr l) l2)))))
         
(define (p l l2)
  (if (null? l)
      '()
      (cases expression (car l)
        (tm-exp (sym int next-tm)
                (cons (tm-exp sym int (list (find-next-tm (cdr l) l2)))
                      (p (cdr l) l2)))
        (var-exp (var tms goto)
                 (cases expression goto
                   (tm-exp (sym int next)
                           (cons (var-exp var tms (tm-exp sym int (list (find-next-tm (cdr l) l2))))
                                 (p (cdr l) l2)))
                   (else (cons (car l) (p (cdr l) l2)))))
        (else (cons (car l)
                    (p (cdr l) l2))))))




;; exp -> (listof node)
;; Purpose: Create one node
(define (node exp first-int)
  (cases expression exp
    (tm-exp (sym int next-tm)
            (let* [(a-node
                    (string-append (symbol->string sym) (number->string int)))
                   (a-color
                    (cond [(= int first-int) "forestgreen"]
                          [(pair? (car next-tm)) "goldenrod1"]                           
                          [else "black"]))
                   (a-shape
                    (cond [(pair? (car next-tm)) "diamond"]                           
                          [else "rectangle"]))
                   (a-label
                    (symbol->string sym))]
              (list a-node `((color ,a-color) (shape ,a-shape) (label ,a-label)))))
    (var-exp (var tms goto)
             (map (lambda (x i) (list (string-append (symbol->string (if (pair? x)
                                                                         (car (cdr x))
                                                                         x))
                                                     "var" (number->string i))
                                      `((color "black") (shape "square") (label ,(if (pair? x)
                                                                                     (car (cdr x))
                                                                                     x)))))
                  tms
                  (make-int-list tms 0)))
    (else '())))

(define (branch-edges fromst branch l l2)
  (let ((tost (find-next-tm (find-goto (cadr (cadr branch)) l2) l2))
        (a-label (symbol->string (if (not (symbol? (car branch)))
                                     (car (cdr (car branch)))
                                     (car branch)))))
    (list fromst tost
          `((label ,a-label) (style "dashed")))))

;; exp -> (listof edge)
;; Purpose: Create one edge
(define (edge exp l l2)
  (cases expression exp
    (tm-exp (sym int next-tm)
            (cond ((null? (car next-tm))
                   '())
                  ((pair? (car next-tm))
                   (filter (lambda (y)
                             (not (null? (cadr y))))
                    (map (lambda (x) (branch-edges (string-append (symbol->string sym) (number->string int))
                                                  x l l2))
                        (car next-tm))))
                  (else
                   (let* [(fromst
                           (string-append (symbol->string sym) (number->string int)))
                          (tost
                           (car next-tm))
                          (a-label "")
                          (a-style "solid")]
                     (list fromst tost
                           `((label ,a-label) (style ,a-style)))))))
    (var-exp (var tms goto)
             (append
              (map (lambda (x x2 int1 int2)
                       (list (string-append (symbol->string (if (symbol? x)
                                                                x
                                                                (car (cdr x))))
                                            "var" (number->string int1))
                             (string-append (symbol->string (if (symbol? x2)
                                                                x2
                                                                (car (cdr x2))))
                                            "var" (number->string int2))
                             `((label ,(format "var = ~a" var)) (style "solid"))))
                     (drop-right tms 1)
                     (cdr tms)
                     (make-int-list (cdr tms) 0)
                     (make-int-list (cdr tms) 1)) 
              (list (list (string-append (symbol->string (if (symbol? (last tms))
                                                             (last tms)
                                                             (car (cdr (last tms)))))
                                         "var" (number->string (- (length tms) 1)))
                          (find-next-tm (find-goto goto l2) l2)
                          '((label "") (style "solid"))))))
    (else '())))
    

(define COPYL2
  '(list FBL
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

;; list -> list
;; Purpose: Filter given list to not include 'list
(define (filter-list l)
  (cond ((null? l)
         '())
        ((pair? (car l))
         (cons (filter-list (car l)) (filter-list (cdr l))))
        ((or (equal? (car l) 'list)
             (equal? (car l) 'cons))
         (filter-list (cdr l)))
        (else (cons (car l) (filter-list (cdr l))))))

;; program -> expval
;; Purpose: Evaluate the given program
#;(define (value-of-program pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (empty-env)))))

(define (make-int-list list acc)
  (if (null? list)
      '()
      (cons acc
            (make-int-list (cdr list) (add1 acc)))))




;; ctmd -> program
;; Purpose: Parse the given ctmd
(define (parse-program list)
  (ctmd-exp
   (let ((parsed-list
          (map (lambda (l i)
                 (parse-ctmd l i (filter-list list)))
               (filter-list list)
               (make-int-list (filter-list list) 0))))
     (p parsed-list parsed-list))))
   
(define (find-first-int l int)
  (if (tm-exp? (car l))
      int
      (find-first-int (cdr l) (add1 int))))
      

(parse-program COPYL2)

(define (dot-nodes exp)
  (cases expression exp
    (ctmd-exp (l)
              (map (lambda (x) (node x (find-first-int l 0))) l))
    (else '())))

(define (dot-edges exp)
  (cases expression exp
    (ctmd-exp (l)
              (map (lambda (x) (edge x l l)) l))
    (else '())))


(define (clean-list l)
  (cond ((null? l)
         '())
        ((null? (car l))
         (clean-list (cdr l)))
        ((and (pair? (car l))
              (pair? (car (car l))))
         (append (map (lambda (x) x) (car l))
                 (clean-list (cdr l))))
        (else (cons (car l)
                    (clean-list (cdr l))))))

(clean-list (dot-edges
             (ctmd-exp
              (list
               (tm-exp 'FBL 0 '("R2"))
               (label-exp 0)
               (tm-exp 'R 2 '(((_ (GOTO 2)) ('a (GOTO 1)) ('b (GOTO 1)) ('d (GOTO 1)))))
               (branch-exp '((_ (GOTO 2)) ('a (GOTO 1)) ('b (GOTO 1)) ('d (GOTO 1))))
               (label-exp 1)
               (var-exp 'k '(WB FBR FBR 'k FBL FBL 'k) (goto-exp (label-exp 0)))
               (label-exp 2)
               (tm-exp 'FBR 7 '("L8"))
               (tm-exp 'L 8 '(((_ (GOTO 3)) ('a (GOTO 4)) ('b (GOTO 4)) ('d (GOTO 4)))))
               (branch-exp '((_ (GOTO 3)) ('a (GOTO 4)) ('b (GOTO 4)) ('d (GOTO 4))))
               (label-exp 3)
               (tm-exp 'RR 11 '(()))
               (goto-exp (label-exp 5))
               (label-exp 4)
               (tm-exp 'R 14 '(()))
               (goto-exp (label-exp 5))
               (label-exp 5)))))

;; fsm word -> image
;; Purpose: Given a ctm as list, create a .png file from a .dot file, and return a bitmap
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
           (clean-list (dot-nodes (parse-program ctm)))))
    (set! cgraph
            (foldl
             (lambda (a-trans a-graph)
               (let* [(state1 (string->symbol (first a-trans)))
                      (state2 (string->symbol (second a-trans)))
                      (label (if (symbol? (second (first (third a-trans))))
                                 (symbol->string (second (first (third a-trans))))
                                 (second (first (third a-trans)))))
                      (style (second (second (third a-trans))))] 
                 (add-edge a-graph label state1 state2 #:atb (hash 'style style))))
             cgraph
             (clean-list (dot-edges (parse-program ctm)))))
    (let [(res (graph->bitmap cgraph))]
      res)))

(transition-diagram-ctm COPYL2)
























(define FBRL '(0
               R
               (cons BRANCH
                     (list (list 'a (list GOTO 0))
                           (list 'b (list GOTO 0))
                           (list 'd (list GOTO 0))
                           (list BLANK (list GOTO 10))))
               10))

(clean-list (dot-nodes (parse-program FBRL)))
(clean-list (dot-edges (parse-program FBRL)))

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

(define MULT-ACC
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


(transition-diagram-ctm COPYL2)
(transition-diagram-ctm FBRL)
(transition-diagram-ctm FBLL)
(transition-diagram-ctm TWICERL)
(transition-diagram-ctm WARROW_L)
(transition-diagram-ctm _WARROWL)
(transition-diagram-ctm MULTL)


#;(define swap (combine-tms (list (list (list VAR 'i)
                                      R
                                      (list (list VAR 'j)
                                            'i
                                            L
                                            'j)))
                          '(a b)))

#;(define SWAPL '(list (list (list VAR 'i)
                           R
                           (list (list VAR 'j)
                                 i
                                 L
                                 j))))

'(list (list (list VAR 'i)
             R
             (list (list VAR 'j)
                   i
                   L
                   j)))

;(transition-diagram-ctm SWAPL)



;(filter-list SWAPL)















