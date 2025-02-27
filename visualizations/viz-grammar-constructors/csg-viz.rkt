#lang racket/base

(require "../../fsm-gviz/private/lib.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/misc.rkt"
         "grammar-viz.rkt"
         racket/list
         racket/local)

(provide csg-viz)

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

(define HEDGE-COLOR 'skyblue)

(define YIELD-COLOR 'violet)

(define FAILED-INV-COLOR 'red)
(define INV-HELD-COLOR 'green)

(define FONT-SIZE 12)
(define HEXAGON-COLOR 'black)


;; csg word -> Derivation with rules
;; Creates a derivation with the rule applied besides each step
(define (csg-derive-edited g w)

  ; csg-rule natnum (listof symbol) --> (listof (listof symbol))
  (define (use-csg-rule r str i)

    ; (listof symbol) (listof symbol) natnum --> (listof (listof symbol))
    (define (helper lhs rhs i)
      (cond
        [(< (- (length (first str)) i) (length lhs)) '()]
        [else
         (let* ([subword (sublist (first str) i (length lhs))])
           (cond
             [(equal? lhs subword)
              (if (equal? rhs (list EMP))
                  (cons (list (subst-in-list (first str) i (length lhs) '()) lhs rhs)
                        (helper lhs rhs (+ i 1)))
                  (cons (list (subst-in-list (first str) i (length lhs) rhs) lhs rhs)
                        (helper lhs rhs (+ i 1))))]
             [else (helper lhs rhs (+ i 1))]))]))
    (let ([res (helper (csg-rule-lhs r) (csg-rule-rhs r) i)]) res))

  ; (listof symbol) (listof csg-rule) --> (listof (listof symbol))
  (define (apply-one-step curr rls)
    (cond
      [(null? rls) '()]
      [else (append (use-csg-rule (car rls) curr 0) (apply-one-step curr (cdr rls)))]))

  ; (listof symbol) (listof (listof (listof symbol))) -> (listof (listof symbol))
  (define (bfs-deriv generated-derivations tovisit)
    (define (ins paths)
      (define (insert path sortedpaths)
        (cond
          [(null? sortedpaths) (list path)]
          [(< (length (car (first path))) (length (caar (first sortedpaths))))
           (cons path sortedpaths)]
          [else (cons (car sortedpaths) (insert path (cdr sortedpaths)))]))

      (cond
        [(null? paths) '()]
        [else (insert (car paths) (ins (cdr paths)))]))

    (cond
      [(null? tovisit) '()]
      [else
       (let* ([firstpath (car tovisit)] [current (car firstpath)])
         (cond
           [(equal? w (first current)) firstpath]
           [else
            (let* ([new-words (apply-one-step current (csg-getrules g))]
                   [newstrings (filter (lambda (s) (not (member (first s) generated-derivations)))
                                       new-words)]
                   [new-queue-paths (map (lambda (s) (cons s firstpath)) newstrings)]
                   [newpaths (ins (append (cdr tovisit) new-queue-paths))])
              (bfs-deriv (append newstrings generated-derivations) newpaths))]))]))

  (let* ([res (bfs-deriv '() (list (list (list (list (csg-getstart g)) '() '()))))]
         [result (reverse res)])
    (if (null? result)
        (format "~s is not in L(G)." w)
        (append-map
         (lambda (l)
           (if (equal? w (first l))
               (if (null? l)
                   (list (list EMP))
                   (list
                    (list (los->symbol (first l)) (los->symbol (second l)) (los->symbol (third l)))))
               (list
                (list (los->symbol (first l)) (los->symbol (second l)) (los->symbol (third l))))))
         result))))

;; symbol -> symbol
;; Just removes number renaming that we have to do for graphviz and returns the original symbol
(define (undo-renaming symb)
  (string->symbol (list->string (filter (lambda (x)
                                          (not (or (equal? #\0 x)
                                                   (equal? #\1 x)
                                                   (equal? #\2 x)
                                                   (equal? #\3 x)
                                                   (equal? #\4 x)
                                                   (equal? #\5 x)
                                                   (equal? #\6 x)
                                                   (equal? #\7 x)
                                                   (equal? #\8 x)
                                                   (equal? #\9 x))))
                                        (string->list (symbol->string symb))))))

;; (listof symbols) (listof rules) MutableHashtable
;; Returns the levels for each graph needed to display the derivation
(define (generate-levels curr-state rules used-names)
  (local
    [;; list list -> num
     ;; Purpose: Returns the index of the the begining of wherever the sublst matches in the original lst
     (define (index-of lst sublst)
       (local [(define sublst-length (length sublst))
               (define (index-of-helper lst sublst idx)
                 (if (empty? lst)
                     -1
                     (if (equal? (take lst sublst-length) sublst)
                         idx
                         (index-of-helper (rest lst) sublst (add1 idx)))))]
         (index-of-helper lst sublst 0)))
     ;; rename-symbols
     ;; symbol (listof symbol) -> (listof symbol)
     ;; Purpose: To rename the symbols in the substituted part of the yield if needed
     (define (rename-symbols nt hashtb)
       (let ([result (hash-ref hashtb nt #f)])
         (if result
             (begin
               (hash-set! hashtb nt (add1 result))
               (string->symbol (format "~s~s" nt (add1 result))))
             (begin
               (hash-set! hashtb nt 0)
               (string->symbol (format "~s0" nt))))))
     ;; (listof symbols) (listof rules) MutableHashtable (listof symbol) (listof symbol) (listof edges)
     ;; Returns the levels for each graph needed to display the derivation
     (define (generate-levels-helper curr-state
                                     rules
                                     used-names
                                     hex-nodes
                                     yield-nodes
                                     levels
                                     hedge-nodes)
       (if (empty? (first (first rules)))
           (list hex-nodes yield-nodes levels hedge-nodes)
           (let* ([curr-rule (first rules)]
                  [idx-of-replaced (index-of (map undo-renaming curr-state)
                                             (symbol->fsmlos (first curr-rule)))]
                  [replaced-str-length (string-length (symbol->string (first curr-rule)))]
                  [all-before-to-be-replaced-symbols (take curr-state idx-of-replaced)]
                  [to-be-replaced-symbols-plus-tail (drop curr-state idx-of-replaced)]
                  
                  [to-be-replaced-symbols (take to-be-replaced-symbols-plus-tail replaced-str-length)]
                  [replaced-combined-symbol (rename-symbols (first curr-rule) used-names)]
                  [replacement-symbols (map (lambda (x) (rename-symbols x used-names))
                                            (symbol->fsmlos (second curr-rule)))]
                  [after-replaced-symbols (drop to-be-replaced-symbols-plus-tail replaced-str-length)])
             (generate-levels-helper
              ;; curr-state
              (append all-before-to-be-replaced-symbols replacement-symbols after-replaced-symbols)
              ;; rules
              (rest rules)
              ;; used-names
              used-names
              ;; hex-nodes
              (cons (remove-duplicates (flatten (cons replaced-combined-symbol hex-nodes)))
                    hex-nodes)
              ;; yield-nodes
              (cons (append all-before-to-be-replaced-symbols replacement-symbols after-replaced-symbols)
                    yield-nodes)
              
              ;; levels
              (let ([not-replaced-edges (filter (lambda (edge) (not (member (second edge) to-be-replaced-symbols)))
                                                (if (empty? levels) '() (first levels)))]
                    [replaced-edges (if (empty? levels)
                                        '()
                                        (remove-duplicates
                                         (map (lambda (edge)
                                                (list (first edge) replaced-combined-symbol))
                                              (filter (lambda (edge) (member (second edge) to-be-replaced-symbols))
                                                      (first levels)))))])
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
              ))))]
    (map reverse (generate-levels-helper curr-state rules used-names (list '()) (list '()) (list '()) (list '())))))

;; deriv-with-rules -> deriv-with-rules
;; Purpose: This is just taking the list received and moving the rules like such:
;; '( ((S) ()) ((AbA) (AbA)) )
;; where the rule applied was next to where it was applied, to:
;; '( ((S) (AbA)) ((AbA) (AaAbA)) )
;; now the rule is next to where it will be applied
(define (move-rule-applications-in-list lst)
  (if (= (length lst) 1)
      (list (list (first (first lst)) '() '()))
      (cons (list (first (first lst)) (second (second lst)) (third (second lst)))
            (move-rule-applications-in-list (rest lst)))))

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
                       #:atb (hash 'color (if (member state hedge-nodes)
                                              HEDGE-COLOR
                                              (if (member state yield-node)
                                                  YIELD-COLOR
                                                  'black))
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
     (add-edge result "" (first rule) (second rule) #:atb (hash 'style 'invisible 'arrowhead 'none)))
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
      (first rule)
      (second rule)
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
      (cons (list (first yield-nodes) (second yield-nodes))
            (create-line-of-edges (rest yield-nodes)))))

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
         (w-derv (map (lambda (x) (symbol->fsmlos (first x))) derv))
         (moved-rules
          (map (lambda (x) (list (second x) (third x))) (move-rule-applications-in-list derv)))
         (rules
          (cons ""
                (foldr (lambda (x accum)
                         (if (empty? (first x))
                             '()
                             (append (list (string-append (symbol->string (first x))
                                                          " → "
                                                          (symbol->string (second x))))
                                     accum)))
                       '()
                       moved-rules)))
         (renamed (generate-levels (list (csg-getstart g)) moved-rules (make-hash)))
         (lod (create-dgrphs (third renamed) (first renamed) (second renamed) (fourth renamed)))
         (graphs (map (lambda (x) (create-graph-structs x (if (empty? invariant)
                                                              'NO-INV
                                                              (first invariant)))) lod))]
    (init-viz g
              w
              w-derv 
              rules
              graphs
              'NO-INV
              #:cpu-cores cpu-cores
              #:special-graphs? 'cfg
              #:rank-node-lst (map (lambda (x y) (cons x (map list y)))
                                   (second renamed)
                                   (first renamed)))))

(define (anbncn-csg-INV yield)
  (define (G-INV yield)
    (= (length (filter (lambda (x) (not (eq? 'c x)))
                       (rest (dropf yield (lambda (x) (not (eq? 'G x)))))))
       0))
  
  (define (H-INV yield)
    (let* ([bs-and-cs (rest (dropf yield (lambda (x) (not (eq? 'H x)))))]
          [bs (takef bs-and-cs (lambda (x) (eq? x 'b)))]
          [cs (dropf bs-and-cs (lambda (x) (eq? x 'b)))])
      (andmap (lambda (x) (eq? x 'c)) cs)))
  
  (define (I-INV yield)
    (let* ([as-bs-cs (rest (dropf yield (lambda (x) (not (eq? 'I x)))))]
           [as (takef as-bs-cs (lambda (x) (eq? 'a x)))]
           [bs-and-cs (dropf as-bs-cs (lambda (x) (eq? 'a x)))]
           [bs (takef bs-and-cs (lambda (x) (eq? x 'b)))]
           [cs (dropf bs-and-cs (lambda (x) (eq? x 'b)))])
      (and (andmap (lambda (x) (eq? 'a x)) as)
           (andmap (lambda (x) (eq? 'b x)) bs)
           (andmap (lambda (x) (eq? 'c x)) cs))))
  
  (define (in-lang? yield)
    (let ([num-as (length (filter (lambda (x) (eq? x 'a)) yield))]
          [num-bs (length (filter (lambda (x) (eq? x 'b)) yield))]
          [num-cs (length (filter (lambda (x) (eq? x 'c)) yield))])
      (= num-as num-bs num-cs)))
  
  (define (equal-num-abc? word)
    (= (+ (length (filter (lambda (x) (eq? 'c x)) word))
          (length (filter (lambda (x) (eq? 'a x)) word)))
       (+ (length (filter (lambda (x) (eq? 'B x)) word))
          (length (filter (lambda (x) (eq? 'b x)) word)))
       (+ (length (filter (lambda (x) (eq? 'C x)) word))
          (length (filter (lambda (x) (eq? 'c x)) word)))))
  
  (and (equal-num-abc? yield)
       (cond [(member 'G yield) (G-INV yield)]
             [(member 'H yield) (H-INV yield)]
             [(member 'I yield) (I-INV yield)]
             [else (in-lang? yield)])))

(define anbncn-csg
  (make-unchecked-csg '(S A B C G H I) 
                      '(a b c) 
                      `((S ,ARROW ABCS) 
                        (S ,ARROW G)
                        (BA ,ARROW AB) 
                        (CA ,ARROW AC) 
                        (CB ,ARROW BC)
                        (CG ,ARROW Gc) 
                        (G  ,ARROW H) 
                        (BH ,ARROW Hb) 
                        (H ,ARROW I)
                        (AI ,ARROW Ia) 
                        (I ,ARROW ,EMP)) 
                      'S))


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