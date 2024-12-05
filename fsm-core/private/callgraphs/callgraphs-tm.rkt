#lang racket/base
(require "../../../fsm-gviz/private/lib.rkt"
         "cg-defs.rkt"
         "mk-cg-edges-function.rkt"
         "../sm-getters.rkt"
         "../sm-apply.rkt"
         racket/list
         racket/string)
(provide make-tm-cg-edges dot-nodes-tm dot-trans-tm computation-diagram-tm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-tm-cg-edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine word natnum natnum -> (listof tm-Edge)
;; Purpose: Given a machine, a word, and a head position, returns a list of tm-Edges
(define (make-tm-cg-edges M word head threshold)
  ;; (listof tm-Edge) (listof tm-stuci) (listof tm-stuci) -> (listof tm-stuci)
  ;; Purpose: Given edges and a list of stucis, will create
  ;;          a new list of stucis that contains the reached
  ;;          states and their respective tapes
  ;; Accumulator Invariants:
  ;;  visited  = list of tm-stucis that have already been explored 
  (define (add-to-stucis edges to-visit visited)  
    ;; (listof tm-Edge) tm-stuci -> (listof tm-stuci)
    ;; Purpose: Given edges and a single tm-stuci, creates all
    ;;          new tm-stucis associated with the transitions
    ;;          following the given tm-stuci
    (define (add-to-stucis-helper edges stuci)
      ;; tm-Edge -> tm-stuci
      ;; Purpose: Given an edge, creates a new tm-stuci
      (define (new-stuci edge)
        ;; -> tape
        ;; Purpose: Stores a copy of the tm-stuci's tape
        (define mtape (void)) 
        (let* [(new-state
                (tm-Edge-tost edge))
               (new-tape
                (cond [(eq? (tm-Edge-action edge) 'R)
                       (if (eq? (tape-right-i stuci) '_)
                           (add-blank (tm-stuci-tape stuci))
                           (tm-stuci-tape stuci))]
                      [(eq? (tm-Edge-action edge) 'L)
                       (tm-stuci-tape stuci)]
                      [else
                       (begin
                         ;; tape = '(@ x0 x1 ... xn) and mtape = (void)
                         (set! mtape (create-tape-copy (tm-stuci-tape stuci)))
                         ;; tape = '(@ x0 x1 ... xn) and mtape = '(@ x0 x1 ... xi=m ... xn) 
                         (mcons-set-i! mtape (tm-stuci-head stuci) (tm-Edge-action edge))
                         ;; mtape = '(@ x0 x1 ... xi=(tm-Edge-action edge) ... xn) 
                         mtape)]))
               (new-head
                (cond [(eq? (tm-Edge-action edge) 'R) (add1 (tm-stuci-head stuci))]
                      [(eq? (tm-Edge-action edge) 'L) (sub1 (tm-stuci-head stuci))]
                      [else (tm-stuci-head stuci)]))
               (new-cl
                (add1 (tm-stuci-cl stuci)))]
          (tm-stuci new-state new-tape new-head new-cl)))
      (map (lambda (e) (new-stuci e))
           (filter (lambda (e) (and (eq? (tm-stuci-state stuci) (tm-Edge-fromst e))
                                    (eq? (tape-at-i stuci) (tm-Edge-read e))))
                   edges)))
    (cut-off
     (if (empty? to-visit)
         '()
         (let* [(new-stucis (add-to-stucis-helper edges (first to-visit)))]
           (if (or (empty? new-stucis)
                   (ormap (lambda (s) 
                            (ormap (lambda (s2) (tm-stucis-equal? s s2)) 
                                   new-stucis))
                          visited))
               (add-to-stucis edges (rest to-visit) visited)
               (append new-stucis
                       (add-to-stucis edges (rest to-visit)
                                      (append new-stucis
                                              visited))))))
     threshold)) 
  ;; tm-stuci -> (listof tm-Edge)
  ;; Purpose: Given a tm-stuci, creates all possible edges for that
  ;;          tm-stuci
  (define (add-to-edges stuci)
    (let* [(new-rules
            (filter (lambda (e) (and (eq? (tm-rule-fromst e) (tm-stuci-state stuci))
                                     (eq? (tm-rule-read e) (tape-at-i stuci))))
                    (sm-rules M)))]
      ;; rule -> (listof rule)
      ;; Purpose: Given one rule, finds all future rules that can be used after using given rule
      (define (future-rules rule)
        (filter (lambda (r) (and (eq? (tm-rule-tost rule) (tm-rule-fromst r))
                                 (cond [(eq? (tm-rule-action rule) 'R) (eq? (tape-right-i stuci) (tm-rule-read r))]
                                       [(eq? (tm-rule-action rule) 'L) (eq? (tape-left-i stuci) (tm-rule-read r))]
                                       [else (eq? (tm-rule-action rule) (tm-rule-read r))])))
                (sm-rules M)))
      ;; rule -> Boolean
      ;; Purpose: Given a rule, determines whether the machine halts
      (define (machine-halts? rule)
        (empty? (future-rules rule)))
      ;; rule -> tm-Edge
      ;; Purpose: Given a machine rule, determines which tm-Edge subtype to use
      ;; How: If the machine halts in the state, we use a tm-spedge.
      ;;      If the machine does not halt, and the threshold is reached, we use a tm-cutoff-edge.
      ;;      Otherwise, we use a tm-edge.
      (define (tm-Edge rule)
        (cond [(machine-halts? rule)
               (tm-spedge (tm-rule-fromst rule)
                          (tm-rule-read rule)
                          (tm-rule-tost rule)
                          (tm-rule-action rule))] 
              [(= (tm-stuci-cl stuci) (- threshold 1))
               (tm-cutoff-edge (tm-rule-fromst rule)
                               (tm-rule-read rule)
                               (tm-rule-tost rule)
                               (tm-rule-action rule))]
              [else (tm-edge (tm-rule-fromst rule)
                             (tm-rule-read rule)
                             (tm-rule-tost rule)
                             (tm-rule-action rule))]))
      (if (empty? new-rules)
          '()
          (map (lambda (e) (tm-Edge e)) new-rules))))
  ;; (listof tm-stuci) (listof tm-stuci) -> (listof tm-Edge)
  ;; Purpose: Given lists of to-visit and visited tm-stucis, appends all possible
  ;;          edges in the machine
  ;; How: It starts with one start tm-stuci with the tape, head position, and cl, and 
  ;;      and finds all edges associated with that stuci. Then, the function
  ;;      takes the new generated edges to find the reached tm-stucis with adjusted
  ;;      components. Next, it recursively appends the old edges
  ;;      to newly found edges associated with the new tm-stucis and so on, until
  ;;      the function halts. 
  ;; Accumulator Invariants:
  ;;  to-visit = list of tm-stucis that still need to be explored
  ;;  visited  = list of all tm-stucis ever explored 
  (define (tm-computation-tree->cg-edges to-visit visited)
    (let* [(new-edges (append-map (lambda (s) (add-to-edges s)) to-visit))
           (new-stucis (add-to-stucis (remove-duplicates new-edges) to-visit visited))]
      (if (empty? new-stucis)
          new-edges
          (append
           new-edges
           (tm-computation-tree->cg-edges new-stucis (append to-visit visited))))))
  ;; Termination Argument:
  ;;  Given a start tm-stuci, the function will generate the edges associated with
  ;;  that tm-stuci, and then the new tm-stucis associated with the new edges, and so on.
  ;;  The functions add-to-edges and add-to-stucis are mutually recursive. However,
  ;;  at some point the machine haltsand  all possible transitions will have
  ;;  been found. tm-stucis cannot be examined repeatedly, which does not avoid an infinite loop.
  ;;  That is why there is a cutoff threshold, that also halts the function. Once there are no
  ;;  edges left to create new tm-stucis, the function appends these last set of edges
  ;;  one more time, and halts.    
  ;; (listof tm-Edge) -> (listof tm-Edge)
  ;; Purpose: Given the finished list of edges, extracts the unnecessary transitions
  ;;          if the machine accepts the word. Only the edges are left that are
  ;;          essential for the word to get accepted.
  (define (tm-remove-redundant-edges-on-accept edges)
    ;; (listof sm-showtransitions) -> (listof tm-Edge)
    ;; Purpose: Given sm-showtransitions, makes a list of edges according to that one accepting computation
    (define (edges-on-accept st)
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
    (if (or (and (eq? (sm-type M) 'tm-language-recognizer)
                 (ormap (lambda (r) (eq? (tm-Edge-tost r) (sm-accept M)))
                        (append (filter tm-spedge? edges)
                                (filter tm-cutoff-spedge? edges))))
            (and (eq? (sm-type M) 'tm)
                 (ormap (lambda (r) (member (tm-Edge-tost r) (sm-finals M)))
                        (append (filter tm-spedge? edges)
                                (filter tm-cutoff-spedge? edges)))))
        (remove-duplicates (edges-on-accept (sm-showtransitions M (cons '@ word) head)))
        edges))
  ;; (listof tm-Edge) -> (listof tm-Edge)
  ;; Purpose: Given a list of edges removes duplicates. The order of prioritization from
  ;;          highest to lowest is: tm-cutoff-spedge, both tm-spedge and tm-cutoff-edge, tm-edge
  (define (tm-remove-duplicate-Edges Edges)
    (let* [(only-tm-edges (filter tm-edge? Edges))
           (only-tm-spedges (filter tm-spedge? Edges))
           (only-tm-cutoff-edges (filter tm-cutoff-edge? Edges))
           (only-tm-cutoff-spedges (filter tm-cutoff-spedge? Edges))
           (new-tm-edges
            (filter
             (lambda (r) (not (ormap (lambda (r2) (tm-Edges-equal? r r2))
                                     (append only-tm-cutoff-edges
                                             only-tm-spedges
                                             only-tm-cutoff-spedges))))
             only-tm-edges))
           (new-tm-spedges
            (filter
             (lambda (r) (not (ormap (lambda (r2) (tm-Edges-equal? r r2))
                                     (append only-tm-cutoff-spedges
                                             only-tm-cutoff-edges))))
             only-tm-spedges))
           (new-tm-cutoff-edges
            (filter
             (lambda (r) (not (ormap (lambda (r2) (tm-Edges-equal? r r2))
                                     (append only-tm-cutoff-spedges
                                             only-tm-spedges))))
             only-tm-cutoff-edges))
           (new-tm-cutoff-spedges
            (remove-duplicates
             (append only-tm-cutoff-spedges
                     (map
                      (lambda (E)
                        (tm-cutoff-spedge (tm-Edge-fromst E)
                                          (tm-Edge-read E)
                                          (tm-Edge-tost E)
                                          (tm-Edge-action E)))
                      (filter
                       (lambda (r) (ormap (lambda (r2) (tm-Edges-equal? r r2))
                                          only-tm-spedges))
                       only-tm-cutoff-edges)))))]
      (append
       new-tm-edges
       new-tm-spedges
       new-tm-cutoff-edges
       new-tm-cutoff-spedges)))
  ;; (listof tm-stuci) natnum -> (listof tm-stuci)
  ;; Purpose: Given a list of tm-stucis and a threshold, filters out tm-stucis that reach given threshold
  (define (cut-off stucis threshold)
    (filter (lambda (s) (< (tm-stuci-cl s) threshold)) stucis))
   (mk-cg-edges-function
     tm-computation-tree->cg-edges
    (tm-stuci (sm-start M) (create-tape word) head 0)
     tm-remove-redundant-edges-on-accept
     tm-remove-duplicate-Edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dot-nodes-tm

;; M word (listof tm-Edge) optargs -> (listof node)
;; Purpose: Given a machine and a word, creates a list of
;;          nodes in dot format
;; Color blindness:
;;  'default - default colors
;;  'deut - Deuteranopia
(define (dot-nodes-tm M word new-rules color-blindness)
  (let* [(start-state (sm-start M))
         (edge-states (remove-duplicates (append (map (lambda (r) (tm-Edge-fromst r)) new-rules)
                                                (map (lambda (r) (tm-Edge-tost r)) new-rules))))
         (all-states
          (if (empty? edge-states)
              (list (sm-start M))
              edge-states))
         (end-states
          (remove-duplicates
           (append (map (lambda (r) (tm-Edge-tost r))
                        (filter tm-spedge? new-rules))
                   (map (lambda (r) (tm-Edge-tost r))
                        (filter tm-cutoff-spedge? new-rules)))))
         (cutoff-states
          (remove-duplicates
           (append (map (lambda (r) (tm-Edge-tost r))
                        (filter tm-cutoff-edge? new-rules))
                   (map (lambda (r) (tm-Edge-tost r))
                        (filter tm-cutoff-spedge? new-rules)))))
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
                  (cond [(and (eq? (sm-type M) 'tm-language-recognizer)
                              (eq? (first los) (sm-accept M))) "doubleoctagon"]
                        [(member (first los) final-states) "doublecircle"]
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

;; (listof tm-Edge) -> (listof trans)
;; Purpose: Given a machine's edges, creates a list of
;;          transitions in dot format
(define (dot-trans-tm new-rules)
  ;; (listof tm-Edge) -> (listof trans)
  ;; Purpose: Given a list of edges, creates
  ;;          a list of transitions
  (define (all-trans loes)
    ;; (listof component) -> (listof trans)
    ;; Purpose: Given a list of components within an edge returns a transition
    (define (new-trans loc)      
      (list (tm-Edge-fromst loc) (tm-Edge-tost loc) `((fontsize 15) (label ,(string-append "[" (symbol->string (tm-Edge-read loc)) " " (symbol->string (tm-Edge-action loc)) "]")))))
    (if (empty? loes)
        '()
        (cons (new-trans (first loes))
              (all-trans (rest loes)))))
  (all-trans new-rules))

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

;; tm word natnum [optargs] -> image
;; Purpose: Given a machine, a word, and a head position, creates a .png file from
;;          a .dot file, and returns a bitmap.
;; Color blindness:
;;  'default - default colors
;;  'deut - Deuteranopia
;; Threshold:
;;  8 - default 
;;  2 - min 
(define (computation-diagram-tm M word head . optargs)
  (define fname "fsm")
  ;; Definitions for Threshold
  (define DEFAULT-THRESH 8)
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
  (let* [(new-rules (make-tm-cg-edges M word head threshold))]
    ;; image
    ;; Purpose: Stores a computation graph image 
    (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'label (cond [(and (eq? (sm-type M) 'tm)
                                                                                      (not (empty? (append (filter tm-cutoff-edge? new-rules)
                                                                                                           (filter tm-cutoff-spedge? new-rules)))))
                                                                                 (format "All computations on '~a cut off at threshold ~a" word threshold)]
                                                                                [(and (eq? (sm-type M) 'tm-language-recognizer)
                                                                                      (not (empty? (append (filter tm-cutoff-edge? new-rules)
                                                                                                           (filter tm-cutoff-spedge? new-rules)))))
                                                                                 (format "All computations on '~a cut off at threshold ~a" word threshold)]
                                                                                [(and (eq? (sm-type M) 'tm)
                                                                                      (ormap (lambda (e) (member (tm-Edge-tost e) (sm-finals M)))
                                                                                             (append (filter tm-spedge? new-rules)
                                                                                                     (filter tm-cutoff-spedge? new-rules))))
                                                                                 (format "Machine reaches a halting state on: '~a" word)]
                                                                                [(eq? (sm-type M) 'tm)
                                                                                 (format "Machine fails to reach a halting state on: '~a" word)]
                                                                                [(and (eq? (sm-type M) 'tm-language-recognizer)
                                                                                      (ormap (lambda (r) (eq? (tm-Edge-tost r) (sm-accept M)))
                                                                                             (append (filter tm-spedge? new-rules)
                                                                                                     (filter tm-cutoff-spedge? new-rules))))
                                                                                 (format "Machine accepts on: '~a" word)]
                                                                                [else
                                                                                 (format "Machine rejects on: '~a" word)])
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
             (dot-nodes-tm M word new-rules color-blindness)))
      (set! cgraph
            (foldl
             (lambda (a-trans a-graph)
               (let* [(state1 (first a-trans))
                      (state2 (second a-trans))
                      (fontsize (second (first (third a-trans))))
                      (label (second (second (third a-trans))))]
                 (add-edge a-graph label state1 state2 #:atb (hash 'fontsize fontsize))))
             cgraph
             (dot-trans-tm new-rules)))
      (let [(res (graph->bitmap cgraph))] 
          res))))

;.................................................







