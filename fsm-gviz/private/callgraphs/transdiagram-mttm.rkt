#lang fsm
(require 2htdp/image)
(require "../lib.rkt" "cg-defs.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multitape-tm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; L = {w | w has equal number of as, bs, and cs}
;; PRE: (@ _ w) AND t0h = 1 AND tapes 1-3 are empty AND t1h-t3h = 0

;; Conditions and States:

;; S: tape 0 = (@ _ w) AND t0h = 1
;;    tape 1-3 = (_) AND t1h = t2h = t3h = 0
;;    starting state

;; C: tape 0 = (@ _ w) AND t0h >= 2
;;    tape 1 = (_ a* _) AND num a = num a in tape0[2..t0h-1] AND
;;             t1h = num a in tape1[2..t0h-1] + 1 AND
;;    tape 2 = (_ b* _) AND num b = num b in tape2[2..t0h-1] AND
;;             t2h = num b in tape2[2..t0h-1] + 1 AND tape1[t2h] = BLANKtape1[t1h] = BLANK
;;    tape 3 = (_ c* _) AND num c = num c in tape3[2..t0h-1] AND
;;             t3h = num c in tape3[2..t0h-1] + 1 AND tape1[t3h] = BLANK

;; PRE: (@ _ w) AND i = 1
(define EQABC
  (make-mttm '(S Y N C D E F G)
             '(a b c)
             'S
             '(Y N)
             (list
              (list (list 'S (list BLANK BLANK BLANK BLANK))
                    (list 'C (list RIGHT RIGHT RIGHT RIGHT)))
              (list (list 'C (list 'a BLANK BLANK BLANK))
                    (list 'D (list 'a 'a BLANK BLANK)))
              (list (list 'D (list 'a 'a BLANK BLANK))
                    (list 'C (list RIGHT RIGHT BLANK BLANK)))
              (list (list 'C (list 'b BLANK BLANK BLANK))
                    (list 'E (list 'b BLANK 'b BLANK)))
              (list (list 'E (list 'b BLANK 'b BLANK))
                    (list 'C (list RIGHT BLANK RIGHT BLANK)))
              (list (list 'C (list 'c BLANK BLANK BLANK))
                    (list 'F (list 'c BLANK BLANK 'c)))
              (list (list 'F (list 'c BLANK BLANK 'c))
                    (list 'C (list RIGHT BLANK BLANK RIGHT)))
              (list (list 'C (list BLANK BLANK BLANK BLANK))
                    (list 'G (list BLANK LEFT LEFT LEFT)))
              (list (list 'G (list BLANK BLANK BLANK BLANK))
                    (list 'Y (list BLANK BLANK BLANK BLANK)))
              (list (list 'G (list BLANK 'a 'b 'c))
                    (list 'G (list BLANK LEFT LEFT LEFT)))
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

;.................................................

;; Tests for EQABC
(check-equal? (sm-apply EQABC `(,LM ,BLANK a a b b a c c) 1) 'reject)
(check-equal? (sm-apply EQABC `(,LM ,BLANK a a a) 1) 'reject)
(check-equal? (sm-apply EQABC `(,LM ,BLANK c c a b b) 1) 'reject)
(check-equal? (sm-apply EQABC `(,LM ,BLANK) 1) 'accept)
(check-equal? (sm-apply EQABC `(,LM ,BLANK a c c b a b) 1) 'accept)
(check-equal? (sm-apply EQABC `(,LM ,BLANK c c c a b b a a c b a b b c a) 1) 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design and implement an mttm to compute f(w) = ww.

;; States:
;; S: tape 0 = (@ _ w) and t0h = 1 and tape0[t0h] = BLANK
;;    tape 1 = (_) and t1h = 0 and tape1[t1h] = BLANK,
;;    starting state
;; C: tape 0 = (@ _ w) and t0h >= 2
;;    tape 1 = (_ (avb)*) v (_ (avb)* _) and 1 <= t1h <= nr of (avb)s in tape 1 + 1 and
;;             tape1[t1h] = BLANK and tape0[2..t0h-1] = tape1[1..t1h-1]
;; D: tape 0 = (@ _ w) and t0h >= 2 and tape0[t0h] = a
;;    tape 1 = (_ (avb)* _) and t1h = nr of (avb)s in tape 1 and tape1[t1h] = a and 
;;             tape0[2..t0h-1] = tape1[1..t1h-1]
;; G: tape 0 = (@ _ w _) and t0h = |w+2| and tape0[t0h] = BLANK
;;    tape 1 = (_ w _) and 0 <= t1h <= |w| and tape1[t1h] = elem in w v BLANK
;; A: tape 0 = (@ _ w (avb)* _) v (@ _ w (avb)*) and t0h >= |w+2| and tape0[t0h] = elem in w v BLANK
;;    tape 1 = (_ (avbv_)* _) and 1 <= t1h <= |w|+|w|+2 and tape1[t1h] = elem in w v BLANK and
;;             tape1[t0h..|tape1|] + tape0[|w+2|..|tape0|] = w
;; Y: tape 0 = (@ _ w w _) and t0h = |w|+|w|+2 and tape0[t0h] = BLANK
;;    tape 1 = (_*) and nr of BLANKs = |w|+2 and t1h = |w|+1 and tape1[t1h] = BLANK,
;;    accepting state

;; L = f(w) = ww
;; Σ = {a b}
;; PRE: (@ _ w) AND t0h = 1 AND tape 1 is empty AND t1h = 0
(define ww
  (make-mttm '(S A C D G Y)
             '(a b)
             'S
             '(Y)
             (list
              (list (list 'S (list BLANK BLANK))
                    (list 'C (list RIGHT RIGHT)))
              (list (list 'C (list 'a BLANK))
                    (list 'D (list 'a 'a)))
              (list (list 'D (list 'a 'a))
                    (list 'C (list RIGHT RIGHT)))
              (list (list 'C (list 'b BLANK))
                    (list 'D (list 'b 'b)))
              (list (list 'D (list 'b 'b))
                    (list 'C (list RIGHT RIGHT)))
              (list (list 'C (list BLANK BLANK))
                    (list 'G (list BLANK LEFT)))
              (list (list 'G (list BLANK 'a))
                    (list 'G (list BLANK LEFT)))
              (list (list 'G (list BLANK 'b))
                    (list 'G (list BLANK LEFT)))           
              (list (list 'G (list BLANK BLANK))
                    (list 'A (list BLANK RIGHT)))
              (list (list 'A (list BLANK 'a))
                    (list 'A (list 'a BLANK)))
              (list (list 'A (list 'a BLANK))
                    (list 'A (list RIGHT RIGHT)))
              (list (list 'A (list BLANK 'b))
                    (list 'A (list 'b BLANK)))
              (list (list 'A (list 'b BLANK))
                    (list 'A (list RIGHT RIGHT)))              
              (list (list 'A (list BLANK BLANK))
                    (list 'Y (list BLANK BLANK))))
             2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M -> (listof node)
;; Purpose: Generate a list of nodes
(define (dot-nodes M)
  (let* ((start-state (sm-start M))
         (final-states (sm-finals M))
         (rest-states (filter (lambda (x) (not (member x (append (list start-state) final-states)))) (sm-states M))))
    (append
     (list (list start-state `((color "forestgreen") (shape "circle") (label ,start-state))))
     (map (lambda (x) (list x `((color "black") (shape "doublecircle") (label ,x)))) final-states)
     (map (lambda (x) (list x `((color "black") (shape "circle") (label ,x)))) rest-states))))

;.................................................

#;(define (dot-edges M)
  ;; (listof trans) -> (listof edge)
  ;; Purpose: Convert one transition into edges
  (define (edge l)
    (let* ((fromst (car (car l)))
           (tost (car (cadr l)))
           (labels (map (lambda (x y) (string-append "[" (symbol->string x) " " (symbol->string y) "]")) (cadr (car l)) (cadr (cadr l)))))
      (append-map (lambda (z) (list (list fromst tost `((fontsize 15) (label ,z))))) labels)))
  ;(remove-duplicates (append-map edge (sm-rules M))))
  (append-map edge (sm-rules M)))

;; list -> string
;; Purpose: Convert given list to a string
(define (list->string2 l)
  (if (empty? l)
      ""
      (string-append (symbol->string (car l))
                     (if (not (empty? (cdr l))) " " "")
                     (list->string2 (cdr l)))))

;; M -> (listof edge)
;; Purpose: Generate a list of edges
(define (dot-edges M)
  ;; (listof trans) -> (listof edge)
  ;; Purpose: Convert one transition into edges
  (define (edge l)
    (let* ((fromst (car (car l)))
           (tost (car (cadr l)))
           (read (cadr (car l)))
           (action (cadr (cadr l)))
           (labell (string-append "[(" (list->string2 read) ")(" (list->string2 action) ")]")))
      (list (list fromst tost `((fontsize 15) (label ,labell))))))
  (append-map edge (sm-rules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fsm word -> image
;; Purpose: Given a mttm as list, create a .png file from a .dot file, and return a bitmap
(define (transition-diagram-mttm M)
  (define fname "fsm")
  ;; (listof string) -> string 
  ;; creates a string where each value in the list is on a new line
  (define (one-rule-per-line rules)
    (string-join rules "\n"))
  ;; image
  ;; Purpose: Store a graph image 
  (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'fontsize 13)
                               #:fmtrs (formatters (hash) (hash) (hash 'label one-rule-per-line))))
  (begin
    (set! cgraph
          (foldl
           (lambda (a-node a-graph)
             (let* [(state (first a-node))
                    (color (second (first (second a-node))))
                    (shape (second (second (second a-node))))
                    (label (second (third (second a-node))))]
               (add-node a-graph state #:atb (hash 'color color 'shape shape 'label label)))) 
           cgraph   
           (dot-nodes M)))
    (set! cgraph
          (foldl
           (lambda (a-trans a-graph)
             (let* [(state1 (first a-trans))
                    (state2 (second a-trans))
                    (label (second (second (third a-trans))))] 
               (add-edge a-graph label state1 state2)))
           cgraph
           (dot-edges M)))
    (let [(res (graph->bitmap cgraph))]
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(transition-diagram-mttm EQABC)
(transition-diagram-mttm ww)



