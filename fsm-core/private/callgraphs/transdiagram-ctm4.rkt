#lang racket
(require "../../../fsm-gviz/private/lib.rkt" "../tm.rkt" "cg-defs.rkt")
(provide computation-edges transition-diagram-ctm dot-nodes dot-edges clean-list parse-program)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structs

(struct ctmd-exp (ctmd) #:transparent)
(struct tm-exp (sym int next-tm) #:transparent)
(struct label-exp (int) #:transparent)
(struct branch-exp (branches) #:transparent)
(struct goto-exp (label) #:transparent)
(struct var-exp (var tm) #:transparent)
(struct expression (exp) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

;; expression -> list throws error
(define (branch-list exp)
  (cond ((branch-exp? exp) (branch-exp-branches exp))
        (else (error "not a branch"))))

;; any -> throws error
;; Purpose: Throw an error in case of invalid syntax
(define (report-invalid-ctm-syntax datum)
  (error (format "Expected a ctmd, given: ~a" datum)))

;; any -> int throws error
;; Purpose: Get integer from label or goto
(define (get-int lab)
  (cond ((label-exp? lab) (label-exp-int lab))
        ((goto-exp? lab) (get-int (goto-exp-label lab)))
        (else (error "expected label-exp, given: ~a" lab))))

;; integer label -> list
;; Purpose: Return rest of ctmd list after goto
(define (find-goto lab l2)
  (if (null? l2)
      '()
      (cond ((label-exp? (car l2))
             (if (= (label-exp-int (car l2)) (if (integer? lab)
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
      (cond ((tm-exp? (car l))
             (string-append (symbol->string (tm-exp-sym (car l))) (number->string (tm-exp-int (car l)))))
            ((branch-exp? (car l))
             (branch-exp-branches (car l)))
            ((goto-exp? (car l))
             (find-next-tm (find-goto (goto-exp-label (car l)) l2) l2))
            (else (find-next-tm (cdr l) l2)))))

;; symbol list list list -> list
;; Purpose: Find the branch edges
(define (branch-edges fromst branch l l2)
  (displayln branch)
  (let ((tost (find-next-tm (find-goto (cadr (cadr branch)) l2) l2))
        (a-label (symbol->string (if (not (symbol? (car branch)))
                                     (car (cdr (car branch)))
                                     (car branch)))))
    (list fromst tost
          `((label ,a-label) (style "dashed") (color "black") (headlabel "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser

;; ctmd integer list -> exp
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
         (cond ((equal? (car datum) 'BRANCH)
                (if (pair? (car (cadr datum)))
                    (branch-exp
                     (cadr datum))
                    (branch-exp
                     (car (list (cdr datum))))))
               ((equal? (car datum) 'GOTO)
                (goto-exp
                 (label-exp (cadr datum))))
               ((equal? (car (car datum)) 'VAR)
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
      (cond ((tm-exp? (car l)) 
             (cons (tm-exp (tm-exp-sym (car l)) (tm-exp-int (car l)) (list (find-next-tm (cdr l) l2)))
                   (p (cdr l) l2)))
            ((var-exp? (car l)) 
             (cons (var-exp (var-exp-var (car l)) (find-next-tm (cdr l) l2))
                   (p (cdr l) l2)))
            (else (cons (car l)
                        (p (cdr l) l2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; exp integer -> (listof node)
;; Purpose: Create one node
(define (node exp first-int)
  (cond ((tm-exp? exp) 
         (let* ((a-node
                 (string-append (symbol->string (tm-exp-sym exp)) (number->string (tm-exp-int exp))))
                (a-color
                 (cond [(and (integer? first-int)
                             (= (tm-exp-int exp) first-int)) "forestgreen"]
                       [(pair? (car (tm-exp-next-tm exp))) "goldenrod1"]                           
                       [else "black"]))
                (a-shape
                 (cond [(pair? (car (tm-exp-next-tm exp))) "diamond"]                           
                       [else "rectangle"]))
                (a-label
                 (symbol->string (tm-exp-sym exp))))
           (list a-node `((color ,a-color) (shape ,a-shape) (label ,a-label)))))
        (else '())))

;.................................................

;; exp -> (listof edge)
;; Purpose: Create one edge
(define (edge exp l l2)
  (cond ((tm-exp? exp) 
         (cond ((null? (car (tm-exp-next-tm exp)))
                '())
               ((pair? (car (tm-exp-next-tm exp)))
                (filter (lambda (y)
                          (not (null? (cadr y))))
                        (map (lambda (x) (branch-edges (string-append (symbol->string (tm-exp-sym exp)) (number->string (tm-exp-int exp)))
                                                       x l l2))
                             (car (tm-exp-next-tm exp)))))
               (else
                (let* ((fromst
                        (string-append (symbol->string (tm-exp-sym exp)) (number->string (tm-exp-int exp))))
                       (tost
                        (car (tm-exp-next-tm exp)))
                       (a-label "")
                       (a-style "solid"))
                  (list fromst tost
                        `((label ,a-label) (style ,a-style) (color "black") (headlabel "")))))))
        ((var-exp? exp) 
         (list (var-exp-tm exp) (var-exp-tm exp)
               `((label "") (style "solid") (color "white") (headlabel ,(format "var=~a" (var-exp-var exp))))))
        (else '())))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more helpers

;; list -> list
;; Purpose: Filter given list to not include 'list or 'cons or ()
(define (filter-list l)
  (cond ((null? l)
         '())
        ((and (pair? (car l))
              (or (equal? (car (car l)) 'quote)
                  (equal? (car (car l)) 'quasiquote)
                  (equal? (car (car l)) 'unquote))) (cons (car (filter-list (cdr (car l)))) (filter-list (cdr l))))
        ((pair? (car l))
         (cons (filter-list (car l)) (filter-list (cdr l))))
        ((or (equal? (car l) 'list)
             (equal? (car l) 'cons))
         (filter-list (cdr l)))
        (else (cons (car l) (filter-list (cdr l))))))

;; list -> list
;; Purpose: Get the branches in the correct form
#;(define (new-branch-list l acc)
  ;; branch -> branch
  ;; Purpose: Fix branch
  (define (helper b)
    (cond ((= 1 (length (filter (lambda (x) (not (pair? x))) b)))
           (helper2 (cdr b)))
          (( ))))
  
  (cond ((null? l)
         '())
        ((and (pair? (car l))
              (equal? 'BRANCH (car (car l))))
         (helper (car l)))
        (else (cons (car l) (new-branch-list (cdr l) acc)))))

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
  (cond ((ctmd-exp? exp)
         (let ((l (ctmd-exp-ctmd exp)))
           (if (branch-exp? (first-elem l))
               (append (list (list "dummy" '((color "forestgreen") (shape "diamond") (label ""))))
                       (map (lambda (x) (node x "branch-start")) l))
               (map (lambda (x) (node x (find-first-int l 0))) l))))
        (else '())))

;; expression -> list
;; Purpose: Create the list of edges
(define (dot-edges exp)
  (cond ((ctmd-exp? exp)
         (let ((l (ctmd-exp-ctmd exp)))
           (if (branch-exp? (first-elem l))
               (append (map (lambda (x) (branch-edges "dummy" x l l)) (branch-list (first-elem l)))   
                       (map (lambda (x) (edge x l l)) l))
               (map (lambda (x) (edge x l l)) l))))
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

;; ctm -> image
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
;; trace

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
  (follow-trace (cdr (ctm-apply ctm tape head #t))
                (filter (lambda (x) (not (equal? "white" (cadr (caddr (caddr x)))))) (clean-list (dot-edges (parse-program ctmlist))))
                (car (car (clean-list (dot-edges (parse-program ctmlist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

#;(define SWAP
  (combine-tms
   (list (list (list VAR 'i)
               'R
               (list (list VAR 'j)
                     'i
                     'L
                     'j)))
   '(a b)))

(define SWAPL '(list (list (list VAR 'i)
                             'R
                             (list (list VAR 'j)
                                   'i
                                   'L
                                   'j)))) 


#|(transition-diagram-ctm COPYL2)
(transition-diagram-ctm FBRL)
(transition-diagram-ctm FBLL)
(transition-diagram-ctm TWICERL)
(transition-diagram-ctm WARROW_L)
(transition-diagram-ctm _WARROWL)
(transition-diagram-ctm MULTL)
(transition-diagram-ctm SWAPL)




(transition-diagram-ctm COPYL2)
(transition-diagram-ctm SWAPL)|#


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

(define Cwrong
  '(list 7 8 (cons BRANCH (list (list _ (list GOTO 2)) (list 'a (list GOTO 1)) (list 'b (list GOTO 1))))
         1
         (list (list VAR k) WB FBR FBR 'k FBL FBL 'k (list GOTO 0))
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

;(transition-diagram-ctm Cwrong) 

(define ADDL2 '(list R
                   `(,BRANCH (,BLANK (GOTO 100))
                             (d (GOTO 200)))
                   100 ;; a=0
                   R
                   R
                   `(,BRANCH (,BLANK (GOTO 500)) ;; a=b=0
                             (d (GOTO 300)))
                   200 ;; a!=0
                   FBR
                   Wd
                   FBR
                   L
                   WB
                   `(,GOTO 1000)
                   300 ;; a=0 and b!=0
                   L
                   SHL
                   FBL
                   SHL
                   `(,GOTO 1000)
                   500 ;; a=0 and b=0
                   L
                   `(,GOTO 1000)
                   1000))

;(transition-diagram-ctm ADDL)
(transition-diagram-ctm ADDL2) 

;; PRE:  tape = '(@ _ ad* _ bd*) and i = 1
;; POST: tape = '(@ _ (_^|ad*|) _ _ (ad* * bd*) _) and i = |ad*| + |ad* * bd*| + 4
#;(define MULTL2 '(list R
                    (cons BRANCH (list (list 'd WB (list GOTO 0))
                                       (list '_ R (list GOTO 3))))
                    0
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
                    (cons BRANCH (list (list '_ (list GOTO 4))
                                       (list 'd
                                             WB
                                             R
                                             (list GOTO 3))))
                    4
                    FBL
                    w->_w
                    FBR))

;(transition-diagram-ctm MULTL2)


(define ADDL3 '(list FBR
                     Wd
                     FBR
                     L
                     WB
                     L
                     (list BRANCH
                           (list BLANK (list GOTO 3))
                           (list 'd (list GOTO 4)))
                     
                     (list GOTO 5)
                     3
                     R
                     R
                     R
                     (list BRANCH
                           (list BLANK (list GOTO 6))
                           (list 'd (list GOTO 7)))
                     (list GOTO 5)
                     4
                     R
                     (list GOTO 5)
                     6
                     L
                     (list GOTO 5)
                     7
                     L
                     SHL
                     FBL
                     SHL
                     (list GOTO 5)
                     5

                     ))

(define ADDL4 '(list FBR
                     Wd
                     FBR
                     L
                     WB
                     L
                     (list BRANCH
                           (list BLANK R R R (list BRANCH (list BLANK L)
                                                   (list 'd L SHL FBL SHL)))
                           (list 'd R))))


(filter-list ADDL2)
;(filter-list ADDL3)
(filter-list ADDL4)

;; (listof number) -> number
;; Purpose: Generate a random number between 0 and 100 that is not already in numlist
(define (random2 numlist)
  (let ((n (random 1000)))
    (if (member n numlist)
        (random2 numlist)
        n)))

;; branch -> boolean
;; Purpose: Check if branch is in correct form
(define (correct? b)
  (and (pair? (cadr b))
       (equal? 'GOTO (car (cadr b)))))
  
;; branch -> boolean
;; Purpose: Check if branch has goto
(define (has-goto? b)
  (and (pair? (last b))
       (equal? 'GOTO (car (last b)))))


(define (transform-b2 b new-nums initnum)
  (cond ((null? b) (list initnum))
        ((correct? (car b))
         (transform-b2 (cdr b) new-nums initnum))
        ((has-goto? (car b))
         (cons (cons (car new-nums) (cdr (car b))) (transform-b2 (cdr b) (cdr new-nums) initnum)))
        (else
         (append (append (cons (car new-nums) (cdr (car b))) `((GOTO ,initnum))) (transform-b2 (cdr b) (cdr new-nums) initnum)))))


;; branches (listof number) number -> list
;; Purpose: Add branches ang gotos to list 
(define (transform-b branches nums initnum)

  
  (define (helper2 b2)

    
  (define (helper b)
    (cond ((null? b) '())
          ((correct? (car b))
           (cons (car b) (helper (cdr b))))
          #;((has-goto? (car b))
           (cons (cons (car (car b)) `((last (car b)))) (helper (cdr b))))
          (else
           (cons (cons (car (car b)) `((GOTO ,(random2 (cons initnum nums))))) (helper (cdr b))))))

    (let* ((new-branches (helper b2))
           (new-nums (append (map (lambda (x) (cadr (cadr x))) new-branches))))
      (cons (cons 'BRANCH new-branches) (cons `(GOTO ,initnum) (transform-b2 branches new-nums initnum)))))

  (helper2 branches))

        
;; list -> list
;; Purpose: Get the branches in the correct form
(define (new-branch-list l acc)

  ;; branches -> branch
  ;; Purpose: Fix branch
  (define (helper b)
    (cond ((andmap correct? b)
           (cons (car l) (new-branch-list (cdr l) acc)))
          (else
           (let* ((new (transform-b b (filter number? acc) (random2 (filter number? acc))))
                  (new-nums (filter number? new)))
             (append new (new-branch-list (cdr l) new-nums))))))
          
  (cond ((null? l) 
         '())
        ((and (pair? (car l))
              (equal? 'BRANCH (car (car l))))
         (helper (cdr (car l))))
        (else (cons (car l) (new-branch-list (cdr l) acc)))))

(new-branch-list (filter-list ADDL4) (filter-list ADDL4))
(filter-list ADDL3)
;(transition-diagram-ctm ADDL3)
(new-branch-list (new-branch-list (filter-list ADDL4) (filter-list ADDL4)) (new-branch-list (filter-list ADDL4) (filter-list ADDL4)))

(define (correct-b? b)
  (andmap correct? (cdr b)))

(define (new-branch-list2 l)
  (let ((branches (filter (lambda (x) (and (pair? x)
                                           (equal? 'BRANCH (car x)))) l)))
  (if (andmap (lambda (x) (correct-b? x)) branches)
      l
      (new-branch-list2 (new-branch-list l l)))))


(transition-diagram-ctm (new-branch-list2 (filter-list ADDL4)))


   