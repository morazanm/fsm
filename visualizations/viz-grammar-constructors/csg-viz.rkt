#lang racket/base

(require "../../fsm-gviz/private/lib.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/skew-binomial-heap.rkt"
         "grammar-viz.rkt"
         racket/treelist
         racket/list
         racket/set)

(provide csg-viz)

(define HEDGE-COLOR 'skyblue)

(define YIELD-COLOR 'violet)

(define FAILED-INV-COLOR 'red)
(define INV-HELD-COLOR 'green)

(define FONT-SIZE 12)
(define HEXAGON-COLOR 'black)

(struct rule-with-metrics (rule lhs-len num-change-nts num-change-len))
(struct comp-step (word len num-nts lhs-applied rhs-applied idx seq-num))

;; csg word -> Derivation with rules
;; Creates a derivation with the rule applied besides each step
(define (csg-derive-edited g w)
  (define generated-derivs (mutable-set))
  (define nts-set (list->seteq (csg-getv g)))
  (define word-len (length w))
  (define rules-with-lengths
    (map (lambda (rule)
           (rule-with-metrics rule
                              (length (csg-rule-lhs rule))
                              (- (count (lambda (symb) (set-member? nts-set symb))
                                        (csg-rule-rhs rule))
                                 (count (lambda (symb) (set-member? nts-set symb))
                                        (csg-rule-lhs rule)))
                              (- (length (filter (lambda (x) (not (eq? EMP x))) (csg-rule-rhs rule)))
                                 (length (csg-rule-lhs rule)))))
         (csg-getrules g)))

  ;; word natnum natnum word -> word
  (define (subst-in-list word start-idx tosub-len tosub)
    (define end-idx (+ start-idx tosub-len))
    (define (end-word idx word)
      (if (= idx end-idx)
          (append tosub word)
          (end-word (add1 idx) (cdr word))))
    (define (begin-word idx word)
      (if (= idx start-idx)
          (end-word idx word)
          (cons (car word) (begin-word (add1 idx) (cdr word)))))
    (begin-word 0 word))

  ;; comp-step -> (Listof comp-step)
  ;; Applies all possible rules to inputted yield, returns list of new yields to process
  (define (apply-one-step curr num-steps)
    (define yield-len (comp-step-len curr))
    (define (apply-one-rule rules-left word idx stop-idx seq-num)
      (if (null? rules-left)
          '()
          (if (= idx stop-idx)
              (if (null? (cdr rules-left))
                  '()
                  (apply-one-rule (cdr rules-left)
                                  (comp-step-word curr)
                                  0
                                  (min yield-len
                                       (add1 (- (comp-step-len curr) (rule-with-metrics-lhs-len (cadr rules-left)))))
                                  seq-num))
              (if (> (rule-with-metrics-lhs-len (car rules-left))
                     (length word))
                  (apply-one-rule (cdr rules-left)
                                  (comp-step-word curr)
                                  0
                                  (min yield-len
                                       (add1 (- (comp-step-len curr) (rule-with-metrics-lhs-len (cadr rules-left)))))
                                  seq-num)
                  (if (equal? (csg-rule-lhs (rule-with-metrics-rule (car rules-left)))
                              (take word (rule-with-metrics-lhs-len (car rules-left))))
                      (cons (comp-step (subst-in-list (comp-step-word curr)
                                                      idx
                                                      (rule-with-metrics-lhs-len (car rules-left))
                                                      (if (equal?
                                                           (csg-rule-rhs (rule-with-metrics-rule (car rules-left)))
                                                           (list EMP))
                                                          '()
                                                          (csg-rule-rhs (rule-with-metrics-rule (car rules-left)))))
                                       (+ (comp-step-len curr)
                                          (rule-with-metrics-num-change-len (car rules-left)))
                                       (+ (comp-step-num-nts curr)
                                          (rule-with-metrics-num-change-nts (car rules-left)))
                                       (csg-rule-lhs (rule-with-metrics-rule (car rules-left)))
                                       (csg-rule-rhs (rule-with-metrics-rule (car rules-left)))
                                       idx
                                       (add1 seq-num))
                            (apply-one-rule rules-left
                                            (cdr word)
                                            (add1 idx)
                                            stop-idx
                                            (add1 seq-num)))
                      (apply-one-rule rules-left (cdr word) (add1 idx) stop-idx seq-num))
                  )
                  )))
    (apply-one-rule rules-with-lengths
                    (comp-step-word curr)
                    0
                    (min yield-len
                         (add1 (- (comp-step-len curr)
                             (rule-with-metrics-lhs-len (car rules-with-lengths)))))
                    num-steps))
  
  ;; (Listof comp-step) (Listof comp-step) -> Boolean
  ;; Sorts the list
  (define (comparator x y)
    (if (= (comp-step-len (car x)) (comp-step-len (car y)))
        (< (comp-step-seq-num (car x)) (comp-step-seq-num (car y)))
        (< (comp-step-len (car x)) (comp-step-len (car y)))))
  
  ;; (Listof comp-step) -> Derivation
  (define (bfs-deriv tovisit num-steps)
    (define (find-deriving-word new-steps filtered-new-steps)
      (if (null? new-steps)
          (bfs-deriv (foldr (lambda (val accum)
                              (heap-insert val accum comparator))
                            (delete-min/max tovisit comparator)
                            filtered-new-steps)
                     (add1 num-steps))
          (if (= 0 (comp-step-num-nts (caar new-steps)))
              (if (and (= (comp-step-len (caar new-steps)) word-len)
                       (equal? w (comp-step-word (caar new-steps))))
                  (car new-steps)
                  (find-deriving-word (rest new-steps) filtered-new-steps))
              (find-deriving-word (rest new-steps) (cons (first new-steps) filtered-new-steps)))))
    (if (root-empty? tovisit)
        '()
        (let ([firstpath (find-min/max tovisit)])
          (find-deriving-word (for/list ([step (in-list (apply-one-step (car firstpath) num-steps))]
                                         #:when (not (set-member? generated-derivs (comp-step-word step))))
                                (set-add! generated-derivs (comp-step-word step))
                                (cons step firstpath)) '()))))

  (let ([result (reverse (bfs-deriv (heap comparator
                                          (list (comp-step (list (csg-getstart g)) 1 1 '() '() '() 0))) 0))])
    (if (null? result)
        (format "~s is not in L(G)." w)
        (append-map
         (lambda (l)
           (if (equal? w (comp-step-word l))
               (if (null? l)
                   (list (list EMP))
                   (list
                    (list (los->symbol (comp-step-word l))
                          (los->symbol (comp-step-lhs-applied l))
                          (los->symbol (comp-step-rhs-applied l))
                          (comp-step-idx l))))
               (list
                (list (los->symbol (comp-step-word l))
                      (los->symbol (comp-step-lhs-applied l))
                      (los->symbol (comp-step-rhs-applied l))
                      (comp-step-idx l)))))
         result))))

;; symbol -> symbol
;; Just removes number renaming that we have to do for graphviz and returns the original symbol
(define (undo-renaming symb)
  (string->symbol (list->string (takef (string->list (symbol->string symb)) (lambda (x) (not (equal? #\_ x)))))))

;; (listof symbols) (listof rules) MutableHashtable
;; Returns the levels for each graph needed to display the derivation
(define (generate-levels curr-state rules used-names)
  (define (find-length lst sublst sublst-len start-idx num-empties)
    (let ([res (take (drop lst start-idx) sublst-len)])
      (if (> (length (filter (lambda (x) (eq? x EMP)) res)) num-empties)
          (find-length lst sublst (add1 sublst-len) start-idx (add1 num-empties))
          (if (equal? sublst (filter (lambda (x) (not (eq? x EMP))) res))
              sublst-len
              (error (format "~s ~s ~s" lst sublst res))))))
  ;; rename-symbols
  ;; symbol (listof symbol) -> (listof symbol)
  ;; Purpose: To rename the symbols in the substituted part of the yield if needed
  (define (rename-symbols nt hashtb)
    (let ([result (hash-ref hashtb nt #f)])
      (if result
          (begin
            (hash-set! hashtb nt (add1 result))
            (string->symbol (format "~s_~s" nt (add1 result))))
          (begin
            (hash-set! hashtb nt 0)
            (string->symbol (format "~s_0" nt))))))
  ;; (listof symbols) (listof rules) MutableHashtable (listof symbol) (listof symbol) (listof edges)
  ;; Returns the levels for each graph needed to display the derivation
  (define (generate-levels-helper curr-state
                                  rules
                                  used-names
                                  hex-nodes
                                  yield-nodes
                                  levels
                                  hedge-nodes)
    (if (null? (car (car rules)))
        (list hex-nodes yield-nodes levels hedge-nodes)
        (let* ([curr-rule (car rules)]
               [idx-of-replaced (caddr curr-rule)]
               [replaced-str-length (find-length (map undo-renaming curr-state)
                                                 (symbol->fsmlos (car curr-rule))
                                                 (length (symbol->fsmlos (car curr-rule)))
                                                 idx-of-replaced
                                                 0)]
               [all-before-to-be-replaced-symbols (take curr-state idx-of-replaced)]
               [to-be-replaced-symbols-plus-tail (drop curr-state idx-of-replaced)]
                  
               [to-be-replaced-symbols (take to-be-replaced-symbols-plus-tail replaced-str-length)]
               [replaced-combined-symbol (rename-symbols (car curr-rule) used-names)]
               [replacement-symbols (map (lambda (x) (rename-symbols x used-names))
                                         (symbol->fsmlos (cadr curr-rule)))]
               [after-replaced-symbols (drop to-be-replaced-symbols-plus-tail replaced-str-length)])
          (generate-levels-helper
           ;; curr-state
           (append all-before-to-be-replaced-symbols replacement-symbols after-replaced-symbols)
           ;; rules
           (cdr rules)
           ;; used-names
           used-names
           ;; hex-nodes
           (cons (remove-duplicates (flatten (cons replaced-combined-symbol hex-nodes)))
                 hex-nodes)
           ;; yield-nodes
           (cons (append all-before-to-be-replaced-symbols replacement-symbols after-replaced-symbols)
                 yield-nodes)
              
           ;; levels
           (let ([not-replaced-edges (filter (lambda (edge) (not (member (cadr edge) to-be-replaced-symbols)))
                                             (if (null? levels) '() (car levels)))]
                 [replaced-edges (if (null? levels)
                                     '()
                                     (remove-duplicates
                                      (map (lambda (edge)
                                             (list (car edge) replaced-combined-symbol))
                                           (filter (lambda (edge) (member (cadr edge) to-be-replaced-symbols))
                                                   (car levels)))))])
             (cons (append (foldr (lambda (val accum)
                                    (cons (list replaced-combined-symbol val) accum))
                                  '()
                                  replacement-symbols)
                           replaced-edges
                           not-replaced-edges)
                   levels))
           ;; hedge-nodes
           (cons (foldr (lambda (val accum)
                          (cons (list replaced-combined-symbol val) accum))
                        '()
                        replacement-symbols)
                 hedge-nodes)
           ))))
  (map reverse (generate-levels-helper curr-state rules used-names (list '()) (list '()) (list '()) (list '()))))

;; deriv-with-rules -> deriv-with-rules
;; Purpose: This is just taking the list received and moving the rules like such:
;; '( ((S) ()) ((AbA) (AbA)) )
;; where the rule applied was next to where it was applied, to:
;; '( ((S) (AbA)) ((AbA) (AaAbA)) )
;; now the rule is next to where it will be applied
(define (move-rule-applications-in-list lst)
  (if (= (length lst) 1)
      (list (list (car (car lst)) '() '() '()))
      (cons (list (car (car lst)) (cadr (cadr lst)) (caddr (cadr lst)) (cadddr (cadr lst)))
            (move-rule-applications-in-list (cdr lst)))))

;; dgrph is a structure that has
;; levels - levels of a graph
;; nodes - all nodes in the graph
;; hex-nodes - hex nodes in the graph
;; yield-nodes - yield nodes in the graph
;; hedges - highlighted edges of the graph
(struct dgrph
  (levels
   nodes
   hex-nodes
   yield-nodes
   hedges))

;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes hex-nodes yield-node invariant)
  
  (let ([invariant-result (if (not (eq? 'NO-INV invariant))
                              (invariant (map undo-renaming yield-node))
                              '())])
    (foldl (λ (state result)
             (add-node result
                       state
                       #:atb (hash 'color (cond
                                          [(member state yield-node) YIELD-COLOR]
                                          [(member state hedge-nodes) HEDGE-COLOR]
                                          [else 'black])
                                   'fillcolor (if (not (eq? 'NO-INV invariant))
                                                  (if (member state yield-node)
                                                      (if invariant-result
                                                          INV-HELD-COLOR
                                                          FAILED-INV-COLOR)
                                                      (if (member state hedge-nodes)
                                                          HEDGE-COLOR
                                                          'black))
                                                  (if (member state hedge-nodes)
                                                      HEDGE-COLOR
                                                      (if (member state yield-node)
                                                          YIELD-COLOR
                                                          'black)))
                                   'style (if (not (eq? 'NO-INV invariant))
                                              (if (member state yield-node)
                                                  'filled
                                                  'solid)
                                              'solid)
                                   'shape (cond
                                            [(member state hex-nodes) 'hexagon]
                                            [else 'circle])
                                   'label (undo-renaming state)
                                   'penwidth (cond
                                               [(member state hedge-nodes) 3.0]
                                               [else 1.0])
                                   'fontcolor 'black
                                   'font "Sans")))
           graph
           lon)))

;; graph (listof edges) -> graph
;; Creates invisible edges so that ordering of the yield nodes is always maintained
(define (make-invisible-edge-graph graph loe)
  (foldl
   (lambda (rule result)
     (add-edge result "" (car rule) (cadr rule) #:atb (hash 'style 'invisible 'arrowhead 'none)))
   graph
   (reverse loe)))

;; make-edge-graph
;; graph (listof level) -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph loe hedges)
  (foldl
   (lambda (rule result)
     (add-edge
      result
      ""
      (car rule)
      (cadr rule)
      #:atb
      (hash 'fontsize FONT-SIZE
            'style 'solid 
            'penwidth (cond
                        [(member rule hedges) 3.0]
                        [else 1.0])
            'color (if (member rule hedges) HEDGE-COLOR 'black))))
   graph
   (reverse loe)))

;; create-dgraphs
;; dgrph (listof dgrph) boolean -> (listof dgrph)
;; Purpose: To create all the dgrphs for graph imgs
(define (create-dgrphs levels hex-nodes yield-nodes hedges)
  (map (lambda (level hex-node yield-node hedge)
         (dgrph
          level
          (extract-nodes level)
          hex-node
          yield-node
          hedge))
       levels hex-nodes yield-nodes hedges))

;; (listof nodes) -> (listof edges)
;; Recursively creates an edge between the first and second element of the list until it empty
;; We use this list of edges to add invisble edges to the graph, guarenteeing ordering of the yield nodes at the bottom of the graph
(define (create-line-of-edges yield-nodes)
  (if (<= (length yield-nodes) 1)
      '()
      (cons (list (car yield-nodes) (cadr yield-nodes))
            (create-line-of-edges (cdr yield-nodes)))))

;; create-graph-structs
;; dgprh -> img
;; Purpose: Creates the final graph structure that will be used to create the images in graphviz
(define (create-graph-structs a-dgrph invariant)
  (make-invisible-edge-graph
   (make-edge-graph
    (make-node-graph
     (create-graph 'dgraph
                   #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in" 'splines "polyline" 'overlap "scale"))
     (dgrph-nodes a-dgrph)
     (extract-nodes (dgrph-hedges a-dgrph))
     (dgrph-hex-nodes a-dgrph)
     (dgrph-yield-nodes a-dgrph)
     invariant)
    (dgrph-levels a-dgrph)
    (dgrph-hedges a-dgrph))
   (create-line-of-edges (dgrph-yield-nodes a-dgrph))))

(define (csg-viz g w #:cpu-cores [cpu-cores #f] . invariant)
  (let* [(derv (csg-derive-edited g w))
         (w-derv (map (lambda (x) (symbol->fsmlos (car x))) derv))
         (moved-stuff (move-rule-applications-in-list derv))
         (moved-rules
          (map (lambda (x) (list (cadr x) (caddr x))) moved-stuff))
         (moved-rules-with-idx (map (lambda (x) (list (cadr x) (caddr x) (cadddr x))) moved-stuff))
         
         
         (rules
          (cons ""
                (foldr (lambda (x accum)
                         (if (null? (car x))
                             '()
                             (append (list (string-append (symbol->string (car x))
                                                          " → "
                                                          (symbol->string (cadr x))))
                                     accum)))
                       '()
                       moved-rules)))
         (renamed (generate-levels (list (csg-getstart g)) moved-rules-with-idx (make-hash)))
         (lod (create-dgrphs (caddr renamed) (car renamed) (cadr renamed) (cadddr renamed)))
         (graphs (map (lambda (x) (create-graph-structs x (if (null? invariant)
                                                              'NO-INV
                                                              (car invariant)))) lod))
         (rank-node-lst (map (lambda (x y) (cons x (map list y)))
                             (second renamed)
                             (first renamed)))
         (removed-dashes-rank-node-lst (map (lambda (x)
                                              (map (lambda (y)
                                                     (map (lambda (z) (los->symbol (remove '- (symbol->list z)))) y)) x))
                                            rank-node-lst))]
    
    (init-viz g
              w
              w-derv 
              rules
              graphs
              'NO-INV
              #:cpu-cores cpu-cores
              #:special-graphs? 'cfg
              #:rank-node-lst removed-dashes-rank-node-lst)))

(define (anbncn-csg-G-INV yield)
  (if (member 'G yield)
      (let ([num-as (length (filter (lambda (x) (equal? x 'A)) yield))]
            [num-bs (length (filter (lambda (x) (equal? x 'B)) yield))]
            [num-cs (length (filter (lambda (x) (equal? x 'C)) yield))])
        (= num-as num-bs num-cs))
      #f))

(define anbncn-csg
  (make-unchecked-csg '(S-1 A B C G H I) 
                      '(a b c) 
                      `((S-1 ,ARROW ABCS-1) 
                        (S-1 ,ARROW G)
                        (BA ,ARROW AB) 
                        (CA ,ARROW AC) 
                        (CB ,ARROW BC)
                        (CG ,ARROW Gc) 
                        (G  ,ARROW H) 
                        (BH ,ARROW Hb) 
                        (H ,ARROW I)
                        (AI ,ARROW Ia) 
                        (I ,ARROW ,EMP)) 
                      'S-1))

;; generates word word-reversed word
;; S - Generates K, G, right hand marker R
;; K - generates word and promises to generate reversed word
;; A - promise to generate a
;; B - promise to generate b
;; L - left hand marker
;; R - right hand marker
;; G - copies in order word next to reversed word


;; Split into more phases?
;; generate first w, then copy it to right. Have invariants for this.

;; K - idx of K <= |w|
;; L - idx of L = |W|
;; G,L - if idx of G > idx of N
;;            elements in (I-idx ... N-idx) == elements in ((L-idx - (N-idx - I-idx)) ... L-idx)
;;            elements in (N-idx ... G-idx) == elements in (0 ... (G-idx - N-idx))
;;            elements in (G-idx ... R-idx) == elements in ((G-idx - N-idx) ... (R-idx - (G-idx - N-idx)))
;;       else
;;            elements in (I-idx ... G-idx) == elements in
;;            elements in (G-idx ... N-idx) == elements in
;;            elements in (N-idx ... R-idx) == elements in

(define w-w^r-w
  (make-unchecked-csg '(S K A B C D L R G I N)
                      '(a b)
                      `((S ,ARROW KGR)
                        (K ,ARROW aKA)
                        (K ,ARROW bKB)
                        (K ,ARROW L)
                        (AIG ,ARROW IaGC)
                        (BIG ,ARROW IbGD)
                        (GCa ,ARROW aGC)
                        (GCb ,ARROW bGC)
                        (GDa ,ARROW aGD)
                        (GDb ,ARROW bGD)
                        (aG ,ARROW Ga)
                        (bG ,ARROW Gb)
                        (GCR ,ARROW GaR)
                        (GDR ,ARROW GbR)
                        (LIG ,ARROW ,EMP)
                        (R ,ARROW ,EMP))
                      'S))

(define ADD-CSG2 (make-unchecked-csg '(S A E I)
                                     '(b i)
                                     `((S ,ARROW AbAbE)
                                       (A ,ARROW ,EMP)
                                       (A ,ARROW iIA)
                                       (Ii ,ARROW iI)
                                       (Ib ,ARROW bI)
                                       (IE ,ARROW Ei)
                                       (E ,ARROW ,EMP))
                                     'S))

(define anbn
  (make-unchecked-csg
   '(S A B)
   '(a b)
   (list (list 'S ARROW 'AaB) (list 'AaA ARROW 'aSb) (list 'AaA ARROW EMP) (list 'B ARROW 'A))
   'S))

(define anbncn
  (make-unchecked-csg
   '(S A B C G H I)
   '(a b c)
   `((S ,ARROW ABCS)
     (S ,ARROW G)
     (BA ,ARROW AB)
     (CA ,ARROW AC)
     (CB ,ARROW BC)
     (CG ,ARROW Gc)
     (BG ,ARROW BH)
     (BH ,ARROW Hb)
     (AH ,ARROW AI)
     (AI ,ARROW Ia)
     (I ,ARROW ,EMP)
     )
   'S))

#;(define ADD-CSG2 (make-unchecked-csg '(S A E I)
                                       '(b i)
                                       `((S ,ARROW AbAbE)
                                         (A ,ARROW ,EMP)
                                         (A ,ARROW iIA)
                                         (Ii ,ARROW iI)
                                         (Ib ,ARROW bI)
                                         (IE ,ARROW Ei)
                                         (E ,ARROW ,EMP))
                                       'S))
(define anbn1
  (make-unchecked-csg '(S A B)
                      '(a b)
                      `((S ,ARROW AAaAAB)
                        (AAaAAA ,ARROW aSb)
                        (AAaAAA ,ARROW ,EMP)
                        (B ,ARROW A))
                      'S))
