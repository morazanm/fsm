#lang fsm
(require 2htdp/image)
(require  "../../../fsm-gviz/private/lib.rkt" "cg-defs.rkt" "mk-cg-edges-function.rkt")
(provide make-mttm-cg-edges dot-nodes-mttm dot-trans-mttm computation-diagram-mttm)
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

;; Tests for ww
(check-equal? (cadr (sm-apply ww `(,LM ,BLANK) 1))
              `(2 (,LM ,BLANK ,BLANK)))
(check-equal? (cadr (sm-apply ww `(,LM ,BLANK a a a) 1))
              `(8 (,LM ,BLANK a a a a a a ,BLANK)))
(check-equal? (cadr (sm-apply ww `(,LM ,BLANK a b) 1))
              `(6 (,LM ,BLANK a b a b ,BLANK)))
(check-equal? (cadr (sm-apply ww `(,LM ,BLANK) 1))
              `(2 (,LM ,BLANK ,BLANK)))
(check-equal? (cadr (sm-apply ww `(,LM ,BLANK a b a b a b) 1))
              `(14 (,LM ,BLANK a b a b a b a b a b a b ,BLANK)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-mttm-cg-edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine word natnum natnum -> (listof mttm-Edge)
;; Purpose: Given a machine, a word, and a head position, returns a list of tm-Edges
(define (make-mttm-cg-edges M word head threshold)
  ;; (listof mttm-Edge) (listof mttm-stuci) (listof mttm-stuci) -> (listof mttm-stuci)
  ;; Purpose: Given edges and a list of stucis, will create
  ;;          a new list of stucis that contains the reached
  ;;          states and their respective tapes
  ;; Accumulator Invariants:
  ;;  visited  = list of mttm-stucis that have already been explored 
  (define (add-to-stucis edges to-visit visited)  
    ;; (listof mttm-Edge) mttm-stuci -> (listof mttm-stuci)
    ;; Purpose: Given edges and a single mttm-stuci, creates all
    ;;          new mttm-stucis associated with the transitions
    ;;          following the given tm-stuci
    (define (add-to-stucis-helper edges stuci)
      ;; action natnum -> natnum
      ;; Purpose: Find new head position given an action
      (define (new-head a h)
        (cond [(eq? a 'R) (add1 h)]
              [(eq? a 'L) (sub1 h)]
              [else h]))
      ;; action (mlistof X) natnum -> (mlistof X)
      ;; Purpose: Create a new tape
      (define (new-tape a t h)
        ;; (mlistof X) natnum -> X
        ;; Purpose: Find element right from i on given mttm tape
        (define (mttm-tape-right-i tape i)
          (cond [(empty? tape) '_]
                [(= i -1) (mcar tape)]
                [else (mttm-tape-right-i (mcdr tape) (sub1 i))]))
        ;; -> tape
        ;; Purpose: Stores a copy of the mttm-stuci's tape
        (define mtape (void)) 
        (cond [(eq? a 'R)
               (if (eq? (mttm-tape-right-i t h) '_)
                   (add-blank t)
                   t)]
              [(eq? a 'L)
               t]
              [else
               (begin
                 ;; tape = '(@ x0 x1 ... xn) and mtape = (void)
                 (set! mtape (create-tape-copy t))
                 ;; tape = '(@ x0 x1 ... xn) and mtape = '(@ x0 x1 ... xi=m ... xn) 
                 (mcons-set-i! mtape h a)
                 ;; mtape = '(@ x0 x1 ... xi=(mttm-Edge-action edge) ... xn) 
                 mtape)]))
      ;; mttm-Edge -> mttm-stuci
      ;; Purpose: Given an edge, creates a new mttm-stuci
      (define (new-stuci edge)
        (let* [(new-state
                (mttm-Edge-tost edge))
               (new-tapes
                (map (lambda (a t h) (new-tape a t h)) (mttm-Edge-actions edge) (mttm-stuci-tapes stuci) (mttm-stuci-heads stuci)))
               (new-heads
                (map (lambda (a h) (new-head a h)) (mttm-Edge-actions edge) (mttm-stuci-heads stuci)))
               (new-cl
                (add1 (mttm-stuci-cl stuci)))]
          (mttm-stuci new-state new-tapes new-heads new-cl)))
      (map (lambda (e) (new-stuci e))
           (filter (lambda (e) (and (eq? (mttm-stuci-state stuci) (mttm-Edge-fromst e))
                                    (equal? (tapes-at-i stuci) (mttm-Edge-reads e))))
                   edges)))
    (cut-off
     (begin ;(displayln edges)
            ;(displayln to-visit)
            ;(displayln visited)
     (if (empty? to-visit)
         '()
         (let* [(new-stucis (add-to-stucis-helper edges (first to-visit)))]
           (if (or (empty? new-stucis)
                   (ormap (lambda (s) 
                            (ormap (lambda (s2) (mttm-stucis-equal? s s2)) 
                                   new-stucis))
                          visited))
               (add-to-stucis edges (rest to-visit) visited)
               (append new-stucis
                       (add-to-stucis edges (rest to-visit)
                                      (append new-stucis
                                              visited)))))))
     threshold)) 
  ;; mttm-stuci -> (listof mttm-Edge)
  ;; Purpose: Given a mttm-stuci, creates all possible edges for that
  ;;          mttm-stuci
  (define (add-to-edges stuci)
    (begin (displayln stuci)
    
    (let* [(new-rules
            (filter (lambda (e) (and (eq? (mttm-rule-fromst e) (mttm-stuci-state stuci))
                                     (equal? (mttm-rule-reads e) (tapes-at-i stuci))))
                    (sm-rules M)))]
      (displayln new-rules)
      ;; (mlistof X) natnum -> X
      ;; Purpose: Find element at i on given mttm tape
      (define (mttm-tape-at-i tape i)
        (cond [(empty? tape) '_]
              [(= i 0) (mcar tape)]
              [else (mttm-tape-at-i (mcdr tape) (sub1 i))]))
      ;; (mlistof X) natnum -> X
      ;; Purpose: Find element left from i on given mttm tape
      (define (mttm-tape-left-i tape i)
        (cond [(empty? tape) '_]
              [(= i 1) (mcar tape)]
              [else (mttm-tape-left-i (mcdr tape) (sub1 i))]))
      ;; (mlistof X) natnum -> X
      ;; Purpose: Find element right from i on given mttm tape
      (define (mttm-tape-right-i tape i)
        (cond [(empty? tape) '_]
              [(= i -1) (mcar tape)]
              [else (mttm-tape-right-i (mcdr tape) (sub1 i))]))
      ;; (mlistof X) natnum action -> (listof X)
      ;; Purpose: Create list of tape elemts for future rule
      (define (future-rule-reads tapes heads actions)
        ;; (mlistof X) natnum action -> X
        ;; Purpose: Find elemenet on left, right, or at tape head
        (define (future-rule-reads-helper t h a)
          (cond [(eq? a 'R) (mttm-tape-right-i t h)]
                [(eq? a 'L) (mttm-tape-left-i t h)]
                [else a]))       
        (map (lambda (x y z) (future-rule-reads-helper x y z)) tapes heads actions))     
      ;; rule -> (listof rule)
      ;; Purpose: Given one rule, finds all future rules that can be used after using given rule
      (define (future-rules rule)
        (filter (lambda (r) (and (eq? (mttm-rule-tost rule) (tm-rule-fromst r))
                                 (equal? (future-rule-reads (mttm-stuci-tapes stuci)
                                                         (mttm-stuci-heads stuci)
                                                         (mttm-rule-actions rule)) (mttm-rule-reads r))))
                (sm-rules M)))
      ;; rule -> Boolean
      ;; Purpose: Given a rule, determines whether the machine halts
      (define (machine-halts? rule)
        (empty? (future-rules rule)))
      ;; rule -> tm-Edge
      ;; Purpose: Given a machine rule, determines which mttm-Edge subtype to use
      ;; How: If the machine halts in the state, we use a mttm-spedge.
      ;;      If the machine does not halt, and the threshold is reached, we use a mttm-cutoff-edge.
      ;;      Otherwise, we use a mttm-edge.
      (define (mttm-Edge rule)
        (cond [(machine-halts? rule)
               (mttm-spedge (mttm-rule-fromst rule)
                            (mttm-rule-reads rule)
                            (mttm-rule-tost rule)
                            (mttm-rule-actions rule))] 
              [(= (mttm-stuci-cl stuci) (- threshold 1))
               (mttm-cutoff-edge (mttm-rule-fromst rule)
                                 (mttm-rule-reads rule)
                                 (mttm-rule-tost rule)
                                 (mttm-rule-actions rule))]
              [else (mttm-edge (mttm-rule-fromst rule)
                               (mttm-rule-reads rule)
                               (mttm-rule-tost rule)
                               (mttm-rule-actions rule))]))
      (if (empty? new-rules)
          '()
          (map (lambda (e) (mttm-Edge e)) new-rules)))))
  ;; (listof mttm-stuci) (listof mttm-stuci) -> (listof mttm-Edge)
  ;; Purpose: Given lists of to-visit and visited mttm-stucis, appends all possible
  ;;          edges in the machine
  ;; How: It starts with one start mttm-stuci with the tape, head position, and cl, and 
  ;;      and finds all edges associated with that stuci. Then, the function
  ;;      takes the new generated edges to find the reached mttm-stucis with adjusted
  ;;      components. Next, it recursively appends the old edges
  ;;      to newly found edges associated with the new mttm-stucis and so on, until
  ;;      the function halts. 
  ;; Accumulator Invariants:
  ;;  to-visit = list of mttm-stucis that still need to be explored
  ;;  visited  = list of all mttm-stucis ever explored 
  (define (mttm-computation-tree->cg-edges to-visit visited)
    (let* [(new-edges (append-map (lambda (s) (add-to-edges s)) to-visit))
           (new-stucis (add-to-stucis (remove-duplicates new-edges) to-visit visited))]
      (if (empty? new-stucis)
          new-edges
          (append
           new-edges
           (mttm-computation-tree->cg-edges new-stucis (append to-visit visited))))))
  ;; Termination Argument:
  ;;  Given a start mttm-stuci, the function will generate the edges associated with
  ;;  that tm-stuci, and then the new tm-stucis associated with the new edges, and so on.
  ;;  The functions add-to-edges and add-to-stucis are mutually recursive. However,
  ;;  at some point the machine haltsand  all possible transitions will have
  ;;  been found. tm-stucis cannot be examined repeatedly, which does not avoid an infinite loop.
  ;;  That is why there is a cutoff threshold, that also halts the function. Once there are no
  ;;  edges left to create new tm-stucis, the function appends these last set of edges
  ;;  one more time, and halts.    
  ;; (listof mttm-Edge) -> (listof mttm-Edge)
  ;; Purpose: Given the finished list of edges, extracts the unnecessary transitions
  ;;          if the machine accepts the word. Only the edges are left that are
  ;;          essential for the word to get accepted.
  (define (mttm-remove-redundant-edges-on-accept edges)
    ;; (listof sm-showtransitions) -> (listof mttm-Edge)
    ;; Purpose: Given sm-showtransitions, makes a list of edges according to that one accepting computation
    #;(define (edges-on-accept st)
      (let* [(fromst (first (first st)))]      
        (if (or (and (eq? (sm-type M) 'tm-language-recognizer)
                     (eq? fromst (sm-accept M)))
                (and (eq? (sm-type M) 'tm)
                     (member fromst (sm-finals M))))
            '()
            (let* [(tost (first (second st)))
                   (head-from (second (first st)))
                   (head-to (second (second st)))
                   (read (list-ref (third (first st)) head-from))
                   (action (cond [(eq? head-from head-to)
                                  (list-ref (third (second st)) head-to)]
                                 [(< head-from head-to) 'R]
                                 [else 'L]))]
              (if (or (and (eq? (sm-type M) 'tm-language-recognizer)
                           (eq? tost (sm-accept M)))
                      (and (eq? (sm-type M) 'tm)
                           (member tost (sm-finals M))))
                  (cons (tm-spedge fromst read tost action)
                        (edges-on-accept (rest st)))
                  (cons (tm-edge fromst read tost action)
                        (edges-on-accept (rest st))))))))
    #;(if (or (and (eq? (sm-type M) 'tm-language-recognizer)
                 (ormap (lambda (r) (eq? (tm-Edge-tost r) (sm-accept M)))
                        (append (filter tm-spedge? edges)
                                (filter tm-cutoff-spedge? edges))))
            (and (eq? (sm-type M) 'tm)
                 (ormap (lambda (r) (member (tm-Edge-tost r) (sm-finals M)))
                        (append (filter tm-spedge? edges)
                                (filter tm-cutoff-spedge? edges)))))
        (remove-duplicates (edges-on-accept (sm-showtransitions M (cons '@ word) head)))
        edges) edges)
  ;; (listof mttm-Edge) -> (listof mttm-Edge)
  ;; Purpose: Given a list of edges removes duplicates. The order of prioritization from
  ;;          highest to lowest is: mttm-cutoff-spedge, both mttm-spedge and mttm-cutoff-edge, mttm-edge
  (define (mttm-remove-duplicate-Edges Edges)
(let* [(only-mttm-edges (filter mttm-edge? Edges))
           (only-mttm-spedges (filter mttm-spedge? Edges))
           (only-mttm-cutoff-edges (filter mttm-cutoff-edge? Edges))
           (only-mttm-cutoff-spedges (filter mttm-cutoff-spedge? Edges))
           (new-mttm-edges
            (filter
             (lambda (r) (not (ormap (lambda (r2) (mttm-Edges-equal? r r2))
                                     (append only-mttm-cutoff-edges
                                             only-mttm-spedges
                                             only-mttm-cutoff-spedges))))
             only-mttm-edges))
           (new-mttm-spedges
            (filter
             (lambda (r) (not (ormap (lambda (r2) (mttm-Edges-equal? r r2))
                                     (append only-mttm-cutoff-spedges
                                             only-mttm-cutoff-edges))))
             only-mttm-spedges))
           (new-mttm-cutoff-edges
            (filter
             (lambda (r) (not (ormap (lambda (r2) (mttm-Edges-equal? r r2))
                                     (append only-mttm-cutoff-spedges
                                             only-mttm-spedges))))
             only-mttm-cutoff-edges))
           (new-mttm-cutoff-spedges
            (remove-duplicates
             (append only-mttm-cutoff-spedges
                     (map
                      (lambda (E)
                        (mttm-cutoff-spedge (mttm-Edge-fromst E)
                                            (mttm-Edge-reads E)
                                            (mttm-Edge-tost E)
                                            (mttm-Edge-actions E)))
                      (filter
                       (lambda (r) (ormap (lambda (r2) (mttm-Edges-equal? r r2))
                                          only-mttm-spedges))
                       only-mttm-cutoff-edges)))))]
      (append
       new-mttm-edges
       new-mttm-spedges
       new-mttm-cutoff-edges
       new-mttm-cutoff-spedges)))
  ;; (listof tm-stuci) natnum -> (listof tm-stuci)
  ;; Purpose: Given a list of tm-stucis and a threshold, filters out tm-stucis that reach given threshold
  (define (cut-off stucis threshold)
    (filter (lambda (s) (< (mttm-stuci-cl s) threshold)) stucis))
  ;; natnum -> (listof natnum)
  ;; Purpose: Given the number of tapes, create a list of 0s
  (define (rest-heads nr)
    (if (= nr 1)
        '()
        (append (list 0)
                (rest-heads (sub1 nr)))))
  ;; natnum -> (listof tapes)
  ;; Purpose: Given the number of tapes, create a list of tapess
  (define (rest-tapes nr)
    (if (= nr 1)
        '()
        (append (list (mcons '_ '()))
                (rest-tapes (sub1 nr)))))
   (mk-cg-edges-function
     mttm-computation-tree->cg-edges
    (mttm-stuci (sm-start M)
                (append (list (create-tape word)) (rest-tapes (sm-numtapes M)))
                (append (list head) (rest-heads (sm-numtapes M)))
                0)
     mttm-remove-redundant-edges-on-accept
     mttm-remove-duplicate-Edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dot-nodes-mttm

;; M word (listof mttm-Edge) optargs -> (listof node)
;; Purpose: Given a machine and a word, creates a list of
;;          nodes in dot format
;; Color blindness:
;;  'default - default colors
;;  'deut - Deuteranopia
(define (dot-nodes-mttm M word new-rules color-blindness)
  (let* [(start-state (sm-start M))
         (edge-states (remove-duplicates (append (map (lambda (r) (mttm-Edge-fromst r)) new-rules)
                                                 (map (lambda (r) (mttm-Edge-tost r)) new-rules))))
         (all-states
          (if (empty? edge-states)
              (list (sm-start M))
              edge-states))
         (end-states
          (remove-duplicates
           (append (map (lambda (r) (mttm-Edge-tost r))
                        (filter mttm-spedge? new-rules))
                   (map (lambda (r) (mttm-Edge-tost r))
                        (filter mttm-cutoff-spedge? new-rules)))))
         (cutoff-states
          (remove-duplicates
           (append (map (lambda (r) (mttm-Edge-tost r))
                        (filter mttm-cutoff-edge? new-rules))
                   (map (lambda (r) (mttm-Edge-tost r))
                        (filter mttm-cutoff-spedge? new-rules)))))
         (final-states (sm-finals M))]
    ;; (listof state) -> (listof nods)
    ;; Purpose: Given a list of all states in a machine after a word
    ;;          has been consumed, returns a list of nodes 
    (define (new-node los)
      (if (empty? los)
          '()
          (let* [(a-color
                  (cond [(= 1 (length all-states)) "crimson"] 
                        [(member (first los) end-states) "crimson"]
                        [(and (eq? color-blindness 'default)
                              (eq? (first los) start-state)) "forestgreen"]
                        [(and (eq? color-blindness 'deut)
                              (eq? (first los) start-state)) "dodgerblue"]
                        [else "black"]))
                 (a-shape
                  (cond [(member (first los) final-states) "doublecircle"]
                        [else "circle"]))
                 (a-label
                  (symbol->string (first los)))
                 (a-fontcolor
                  (cond [(or (and (eq? (first los) start-state)
                                  (member (first los) end-states))
                             (= 1 (length all-states)))
                         (cond [(eq? color-blindness 'default) "forestgreen"]
                               [(eq? color-blindness 'deut) "dodgerblue"])]
                        [else "black"]))
                 (a-fillcolor
                  (if (member (first los) cutoff-states)
                      "gold"
                      "white"))]
            (cons (list (first los) `((color ,a-color) (shape ,a-shape) (label ,a-label) (fontcolor ,a-fontcolor) (fillcolor ,a-fillcolor))) 
                  (new-node (rest los))))))
    (new-node all-states)))

;.................................................

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
(define (dot-trans-mttm M new-rules)
  ;; (listof trans) -> (listof edge)
  ;; Purpose: Convert one transition into edges
  (define (edge l)
    (let* ((fromst (mttm-Edge-fromst l))
           (tost (mttm-Edge-tost l))
           (reads (mttm-Edge-reads l))
           (actions (mttm-Edge-actions l))
           (labell (string-append "[(" (list->string2 reads) ")(" (list->string2 actions) ")]")))
      (list fromst tost `((fontsize 15) (label ,labell)))))
  (map edge new-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .dot files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computation-diagram-tm

;; (listof string) -> string 
;; creates a string where each value in the list is on a new line
(define (one-rule-per-line rules)
  (string-join rules "\n"))

(define graph-formatters (formatters
                          (hash)                            ; graph level formatters
                          (hash)                            ; node level formatters
                          (hash 'label one-rule-per-line))) ; edge level formatters

;.................................................

;; Optional Arguments is either:
;; 1. (list symbol)         -> contains symbol for color blindness
;; 2. (list integer)        -> contains number for cut-off threshold
;; 3. (list symbol integer) -> contains symbol for color blindness and number for cut-off threshold

;; mttm word natnum [optargs] -> image
;; Purpose: Given a machine, a word, and a head position, creates a .png file from
;;          a .dot file, and returns a bitmap.
;; Color blindness:
;;  'default - default colors
;;  'deut - Deuteranopia
;; Threshold:
;;  8 - default 
;;  2 - min 
(define (computation-diagram-mttm M word head . optargs)
  (define fname "fsm")
  ;; Definitions for Threshold
  (define DEFAULT-THRESH 100)
  (define MIN-THRESH 2)
  (define threshold
    (cond [(empty? optargs) DEFAULT-THRESH]
          [(and (integer? (first optargs))
                (>= (first optargs) MIN-THRESH))
           (first optargs)]
          [(and (> (length optargs) 1)
                (integer? (second optargs))
                (>= (second optargs) MIN-THRESH))
           (second optargs)]
          [else DEFAULT-THRESH]))
  ;; Definitions for Color Blindness
  (define DEFAULT-COLOR 'default)
  (define DEUTERANOPIA 'deut)
  (define COLOR-LIST (list DEFAULT-COLOR DEUTERANOPIA))
  (define color-blindness 
    (cond [(empty? optargs) DEFAULT-COLOR]
          [(and (symbol? (first optargs))
                (member (first optargs) COLOR-LIST))
           (first optargs)]
          [(and (> (length optargs) 1)
                (symbol? (second optargs))
                (member (second optargs) COLOR-LIST))
           (second optargs)]
          [else DEFAULT-COLOR]))
  (let* [(new-rules (make-mttm-cg-edges M word head threshold))]
    ;; word -> word
    ;; Purpose: If neccessary, adds a left-end marker
    (define (add-LM w)
      (cond [(null? w) '(@)]
            [(eq? LM (car w)) w]
            [else (cons LM w)]))
    ;; image
    ;; Purpose: Stores a computation graph image 
    (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'label (cond [(and (eq? (sm-type M) 'mttm)
                                                                                      (not (empty? (append (filter mttm-cutoff-edge? new-rules)
                                                                                                           (filter mttm-cutoff-spedge? new-rules)))))
                                                                                 (format "All computations on '~a cut off at threshold ~a" (add-LM word) threshold)]
                                                                                [(and (eq? (sm-type M) 'mttm-language-recognizer)
                                                                                      (not (empty? (append (filter mttm-cutoff-edge? new-rules)
                                                                                                           (filter mttm-cutoff-spedge? new-rules)))))
                                                                                 (format "All computations on '~a cut off at threshold ~a" (add-LM word) threshold)]
                                                                                [(and (eq? (sm-type M) 'mttm)
                                                                                      (ormap (lambda (e) (member (mttm-Edge-tost e) (sm-finals M)))
                                                                                             (append (filter mttm-spedge? new-rules)
                                                                                                     (filter mttm-cutoff-spedge? new-rules))))
                                                                                 (format "Machine reaches a halting state on: '~a" (add-LM word))]
                                                                                [(eq? (sm-type M) 'mttm)
                                                                                 (format "Machine fails to reach a halting state on: '~a" (add-LM word))]
                                                                                [(and (eq? (sm-type M) 'mttm-language-recognizer)
                                                                                      (ormap (lambda (r) (eq? (mttm-Edge-tost r) (sm-accept M)))
                                                                                             (append (filter mttm-spedge? new-rules)
                                                                                                     (filter mttm-cutoff-spedge? new-rules))))
                                                                                 (format "Machine accepts on: '~a" (add-LM word))]
                                                                                [else
                                                                                 (format "Machine rejects on: '~a" (add-LM word))])
                                                     'fontsize 13)
                                 #:fmtrs (formatters (hash) (hash) (hash 'label one-rule-per-line)))) 
    (begin
      (set! cgraph
            (foldl
             (lambda (a-node a-graph)
               (let* [(state (first a-node))
                      (color (second (first (second a-node))))
                      (shape (second (second (second a-node))))
                      (label (second (third (second a-node)))) 
                      (fontcolor (second (fourth (second a-node))))
                      (style "filled")
                      (fillcolor (second (fifth (second a-node))))]
                 (add-node a-graph state #:atb (hash 'color color 'shape shape 'label label 'fontcolor fontcolor 'style style 'fillcolor fillcolor)))) 
             cgraph   
             (dot-nodes-mttm M word new-rules color-blindness)))
      (set! cgraph
            (foldl
             (lambda (a-trans a-graph)
               (let* [(state1 (first a-trans))
                      (state2 (second a-trans))
                      (fontsize (second (first (third a-trans))))
                      (label (second (second (third a-trans))))]
                 (add-edge a-graph label state1 state2 #:atb (hash 'fontsize fontsize))))
             cgraph
             (dot-trans-mttm M new-rules)))
      (let [(res (graph->bitmap cgraph))] 
          res))))

;.................................................

(make-mttm-cg-edges ww '(@ _ a) 1 100)
(make-mttm-cg-edges ww '(@ _ a b c) 1 100)

(make-mttm-cg-edges EQABC '(@ _ a b c) 1 100)
(make-mttm-cg-edges EQABC '(@ _ a b b) 1 100)

(computation-diagram-mttm ww '(_ a) 1)
(sm-graph EQABC)
(computation-diagram-mttm EQABC '(@ _ a b c) 1)
(computation-diagram-mttm EQABC '(@ _ a b b) 1)


