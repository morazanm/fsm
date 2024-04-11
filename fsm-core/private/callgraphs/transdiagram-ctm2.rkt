#lang fsm
(require eopl)
(require 2htdp/image)
(require "../../../fsm-gviz/private/lib.rkt" "cg-defs.rkt")
(provide computation-edges transition-diagram-ctm dot-nodes dot-edges clean-list parse-program)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; datatype

(define-datatype expression expression?
  (ctmd-exp
   (ctmd pair?))
  (tm-exp
   (sym symbol?)
   (int integer?)
   (next-tm list?))
  (label-exp
   (int integer?))
  (branch-exp
   (branches pair?))
  (goto-exp
   (label expression?))
  (var-exp
   (var symbol?)
   (tm string?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

;; expression -> boolean
;; Purpose: Determine whether exp is a tm-exp
(define (tm-exp? exp)
  (cases expression exp
    (tm-exp (s i n) #t)
    (else #f)))

(define (var-exp? exp)
  (cases expression exp
    (var-exp (v t) #t)
    (else #f)))

(define (branch-exp? exp)
  (cases expression exp
    (branch-exp (b) #t)
    (else #f)))

(define (label-exp? exp)
  (cases expression exp
    (label-exp (b) #t)
    (else #f)))

(define (branch-list exp)
  (cases expression exp
    (branch-exp (l) l)
    (else (error "not a branch"))))

;; any -> throws error
;; Purpose: Throw an error in case of invalid syntax
(define (report-invalid-ctm-syntax datum)
  (error (format "Expected a ctmd, given: ~a" datum)))

;; any -> int throws error
;; Purpose: Get integer from label or goto
(define (get-int lab)
  (cases expression lab
    (label-exp (int) int)
    (goto-exp (label) (get-int label))
    (else (error "expected label-exp, given: ~a" lab))))

;; integer label -> list
;; Purpose: Return rest of ctmd list after goto
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

;; list list -> string list
;; Purpose: Find the next turing machine(s)
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
        (else (find-next-tm (cdr l) l2)))))

;; symbol list list list -> list
;; Purpose: Find the branch edges
(define (branch-edges fromst branch l l2)
  (let ((tost (find-next-tm (find-goto (cadr (cadr branch)) l2) l2))
        (a-label (symbol->string (if (not (symbol? (car branch)))
                                     (car (cdr (car branch)))
                                     (car branch)))))
    (list fromst tost
          `((label ,a-label) (style "dashed") (color "black") (headlabel "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser

;; ctmd -> exp
;; Purpose: Parse the ctmd
(define (parse-ctmd datum int acclist)
  (cond ((symbol? datum)
         (tm-exp
          datum
          int
          '())) ;; empty when initialized
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
                 "")))) ;; empty when initialized
        (else (report-invalid-ctm-syntax datum))))

;.................................................

;; list list -> list
;; Purpose: Take as input the parsed list and complete the
;;          empty parts for tm-exps and var-exps
(define (p l l2)
  (if (null? l)
      '()
      (cases expression (car l)
        (tm-exp (sym int next-tm)
                (cons (tm-exp sym int (list (find-next-tm (cdr l) l2)))
                      (p (cdr l) l2)))
        (var-exp (var tm)
                 (cons (var-exp var (find-next-tm (cdr l) l2))
                       (p (cdr l) l2)))
        (else (cons (car l)
                    (p (cdr l) l2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; exp integer -> (listof node)
;; Purpose: Create one node
(define (node exp first-int)
  (cases expression exp
    (tm-exp (sym int next-tm)
            (let* [(a-node
                    (string-append (symbol->string sym) (number->string int)))
                   (a-color
                    (cond [(and (integer? first-int)
                                (= int first-int)) "forestgreen"]
                          [(pair? (car next-tm)) "goldenrod1"]                           
                          [else "black"]))
                   (a-shape
                    (cond [(pair? (car next-tm)) "diamond"]                           
                          [else "rectangle"]))
                   (a-label
                    (symbol->string sym))]
              (list a-node `((color ,a-color) (shape ,a-shape) (label ,a-label)))))
    #;(var-exp (v tm)
             (list tm `((color "black") (shape "rectangle") (label ,(string-append tm "\n" (format "var=~a" v))))))
    (else '())))

;.................................................

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
                           `((label ,a-label) (style ,a-style) (color "black") (headlabel "")))))))
    (var-exp (var tm)
             (list tm tm
                   `((label "") (style "solid") (color "white") (headlabel ,(format "var=~a" var)))))
    (else '())))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more helpers

;; list -> list
;; Purpose: Filter given list to not include 'list or 'cons or ()
(define (filter-list l)
  (cond ((null? l)
         '())
        ((pair? (car l))
         (cons (filter-list (car l)) (filter-list (cdr l))))
        ((or (equal? (car l) 'list)
             (equal? (car l) 'cons))
         (filter-list (cdr l)))
        (else (cons (car l) (filter-list (cdr l))))))

;; list -> list
;; Purpose: Adjust the list to make the var sequence part of it
(define (adjust-var l)
  (cond ((null? l) '())
        ((and (pair? (car l))
              (pair? (car (car l)))
              (equal? 'VAR (car (car (car l)))))
         (cons (list (car (car l)) (cadr (car l)))
               (append (adjust-var (cdr (car l)))
                       (adjust-var (cdr l)))))
        (else (cons (car l) (adjust-var (cdr l)))))) 

;; list list -> list
;; Purpose: Create a list of consecutive integers matching the ctmd list length
(define (make-int-list list acc)
  (if (null? list)
      '()
      (cons acc
            (make-int-list (cdr list) (add1 acc)))))

;; list int -> int
;; Purpose: Find the first tm's integer to find the start tm
(define (find-first-int l int)
  (if (tm-exp? (car l))
      int
      (find-first-int (cdr l) (add1 int))))

;; list -> exp
;; Purpose: Find the first exp that is not a label
(define (first-elem l)
  (cond ((empty? l) (error "invalid ctm syntax: add machines to syntax"))
        ((label-exp? (car l)) (first-elem (cdr l)))
        (else (car l))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ctmd -> program
;; Purpose: Parse the given ctmd
(define (parse-program list)
  (ctmd-exp
   (let ((parsed-list
          (map (lambda (l i)
                 (parse-ctmd l i (adjust-var (filter-list list))))
               (adjust-var (filter-list list))
               (make-int-list (adjust-var (filter-list list)) 0))))
     (p parsed-list parsed-list))))
   
;.................................................

;; expression -> list
;; Purpose: Create the list of nodes
(define (dot-nodes exp)
  (cases expression exp
    (ctmd-exp (l)
              (if (branch-exp? (first-elem l))
                  (append (list (list "dummy" '((color "forestgreen") (shape "diamond") (label ""))))
                          (map (lambda (x) (node x "branch-start")) l))
                  (map (lambda (x) (node x (find-first-int l 0))) l)))
    (else '())))

;; expression -> list
;; Purpose: Create the list of edges
(define (dot-edges exp)
  (cases expression exp
    (ctmd-exp (l)
              (if (branch-exp? (first-elem l))
                  (append (map (lambda (x) (branch-edges "dummy" x l l)) (branch-list (first-elem l)))   
                          (map (lambda (x) (edge x l l)) l))
              (map (lambda (x) (edge x l l)) l)))
    (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list -> list
;; Purpose: Clean the list to not include () or nested list
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                    (label (cond ((equal? "_" (second (first (third a-trans)))) "BLANK")
                                 ((symbol? (second (first (third a-trans))))
                                  (symbol->string (second (first (third a-trans)))))
                                 (else 
                                  (second (first (third a-trans))))))
                    (style (second (second (third a-trans))))
                    (color (second (third (third a-trans))))
                    (headlabel (second (fourth (third a-trans))))] 
               (add-edge a-graph label state1 state2 #:atb (hash 'style style 'color color 'headlabel headlabel))))
           cgraph
           (clean-list (dot-edges (parse-program ctm)))))
    (let [(res (graph->bitmap cgraph))]
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examples

(define COPYL2
  '(list FBL
         0
         R
         (cons BRANCH (list (list _ (list GOTO 2)) (list 'a (list GOTO 1)) (list 'b (list GOTO 1))))
         1
         (list (list VAR k) WB FBR FBR k FBL FBL k (list GOTO 0))
         2
         FBR
         L
         (cons BRANCH (list (list _ (list GOTO 3)) (list 'a (list GOTO 4)) (list 'b (list GOTO 4))))
         3
         RR
         (list GOTO 5)
         4
         R
         (list GOTO 5)
         5))

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
                  (list (list VAR x)
                        L
                        x
                        R
                        WB
                        (list GOTO 0))
                  3))

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

(define WARROW_L '(FBR
                    0
                    L
                    (cons BRANCH (list (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))
                                       (list 'd (list GOTO 1))
                                       (list '@ (list GOTO 3))
                                       (list '_ (list GOTO 3))))
                    1
                    (list (list VAR x)
                          R
                          x
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
                   (list (list VAR x)
                         L
                         x
                         R
                         WB
                         (list GOTO 0))
                   3))

(define SWAP
  (combine-tms
   (list (list (list VAR 'i)
               'R
               (list (list VAR 'j)
                     'i
                     'L
                     'j)))
   '(a b)))

(define SWAPL (list (list (list VAR 'i)
                             'R
                             (list (list VAR 'j)
                                   'i
                                   'L
                                   'j))))

#|
(transition-diagram-ctm COPYL2)
(transition-diagram-ctm FBRL)
(transition-diagram-ctm FBLL)
(transition-diagram-ctm TWICERL)
(transition-diagram-ctm WARROW_L)
(transition-diagram-ctm _WARROWL)
(transition-diagram-ctm MULTL)
(transition-diagram-ctm SWAPL)
|#

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
                                                    (if (equal? (cadr (cadr trace)) '_)
                                                        (or (equal? (cadr (car (caddr x))) "_")
                                                            (equal? (cadr (car (caddr x))) "BLANK"))
                                                        (equal? (string->symbol (cadr (car (caddr x)))) (cadr (cadr trace)))))) edges)))
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
  (follow-trace (cdr (ctm-run ctm tape head #:trace #t))
                (filter (lambda (x) (not (equal? "white" (cadr (caddr (caddr x)))))) (clean-list (dot-edges (parse-program ctmlist))))
                (car (car (clean-list (dot-edges (parse-program ctmlist)))))))


;(transition-diagram-ctm COPYL2)
;(transition-diagram-ctm SWAPL)


(define C
  '(list 7 8 (cons BRANCH (list (list _ (list GOTO 2)) (list 'a (list GOTO 1)) (list 'b (list GOTO 1))))
         1
         (list (list VAR k) WB FBR FBR k FBL FBL k (list GOTO 0))
         2
         FBR
         L
         (cons BRANCH (list (list _ (list GOTO 3)) (list 'a (list GOTO 4)) (list 'b (list GOTO 4))))
         3
         RR
         (list GOTO 5)
         4
         R
         (list GOTO 5)
         5))

(define A '(1 2 3))


(define ADDL '(list FBL2
                    SHIFTL
                    LFT
                    (cons BRANCH (list (list BLANK (list GOTO 20))
                                       (list i (list GOTO 5))))
                    5
                    RGHT
                    20))
