#lang racket/base

(require "../../fsm-gviz/private/lib.rkt"
         "../../fsm-gviz/private/parallel.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz.rkt"
         "grammar-viz.rkt"
         "../viz-lib/zipper.rkt"
         rackunit
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

(define FONT-SIZE 12)
(define HEXAGON-COLOR 'black)





;; tree Any -> (U #f tree)
;; Finds the search value within the tree using a depth first search
(define (dfs node search-val)
  (if (equal? (tree-value node) search-val)
      node
      (ormap (lambda (node) (dfs node search-val)) (tree-subtrees node))))

;; A tree has a value and subtrees
;; A value is Any
;; A subtree is a listof Any
(struct tree (value [subtrees #:mutable]) #:transparent)






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
                  [before-replacement (take curr-state idx-of-replaced)]
                  [before-replacement-removed (drop curr-state idx-of-replaced)]
                  [removed (take before-replacement-removed replaced-str-length)]
                  [removed-combined-symbol (rename-symbols (first curr-rule) used-names)]
                  [replacement-symbols (map (lambda (x) (rename-symbols x used-names))
                                            (symbol->fsmlos (second curr-rule)))]
                  [after-removed (drop before-replacement-removed replaced-str-length)])
             (generate-levels-helper
              (append before-replacement replacement-symbols after-removed)
              (rest rules)
              used-names
              (let ()
                (cons (remove-duplicates (flatten (cons removed-combined-symbol hex-nodes)))
                      (cons (remove-duplicates (flatten (cons removed-combined-symbol hex-nodes)))
                            hex-nodes)))
              (let* ([before-replace
                      (cons (append before-replacement (list removed-combined-symbol) after-removed)
                            yield-nodes)]
                     [after-replace (cons (append before-replacement replacement-symbols after-removed)
                                          before-replace)])
                after-replace)
              (let* ([before-replace (foldr (lambda (val accum)
                                              (cons (list val removed-combined-symbol) accum))
                                            '()
                                            removed)]
                     [not-replaced-edges (filter (lambda (edge) (not (member (second edge) removed)))
                                                 (if (empty? levels) '() (first levels)))]
                     [replaced-edges (if (empty? levels)
                                         '()
                                         (remove-duplicates
                                          (map (lambda (edge)
                                                 (list (first edge) removed-combined-symbol))
                                               (filter (lambda (edge) (member (second edge) removed))
                                                       (first levels)))))]
                     [after-replace
                      (cons (append (foldr (lambda (val accum)
                                             (cons (list removed-combined-symbol val) accum))
                                           '()
                                           replacement-symbols)
                                    replaced-edges
                                    not-replaced-edges)
                            (cons (append before-replace (if (empty? levels) '() (first levels)))
                                  levels))]
                     )
                after-replace)
              (let* ([before-replace (cons (foldr (lambda (val accum)
                                                    (cons (list val removed-combined-symbol) accum))
                                                  '()
                                                  removed)
                                           hedge-nodes)]
                     [after-replace (cons (foldr (lambda (val accum)
                                                   (cons (list removed-combined-symbol val) accum))
                                                 '()
                                                 replacement-symbols)
                                          before-replace)])
                after-replace)))))]
    (map reverse (generate-levels-helper curr-state rules used-names '() '() '() '()))))

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
;; up-levels - unprocessed levels
;; p-levels - processed levels
;; nodes - nodes in the graph
;; up-hex-nodes - unprocessed hex nodes
;; p-hex-nodes - processed hex nodes
;; up-yield-nodes - unprocessed yield nodes
;; p-yield-nodes - processed yield nodes
;; hedges - highlighted edges of the graphs
;; up-rules - unprocessed grammar rules
;; p-rules - processed grammar rules
(struct dgrph
  (up-levels p-levels
             nodes
             up-hex-nodes
             p-hex-nodes
             up-yield-nodes
             p-yield-nodes
             up-hedges
             p-hedges
             up-rules
             p-rules))

;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes hex-nodes yield-node)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color (cond
                                          [(member state yield-node) YIELD-COLOR]
                                          [(member state hedge-nodes) HEDGE-COLOR]
                                          [else 'black])
                                 'style 'solid
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
         lon))

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
(define (create-dgrphs a-dgrph lod hex?)
  (if (empty? (dgrph-up-levels a-dgrph))
      (cons a-dgrph lod)
      (let* ([new-up-levels (rest (dgrph-up-levels a-dgrph))]
             [new-ad-levels (cons (first (dgrph-up-levels a-dgrph)) (dgrph-p-levels a-dgrph))]
             [new-nodes (extract-nodes (first (dgrph-up-levels a-dgrph)))]
             [new-up-hex-nodes (rest (dgrph-up-hex-nodes a-dgrph))]
             [new-p-hex-nodes (cons (first (dgrph-up-hex-nodes a-dgrph)) (dgrph-p-hex-nodes a-dgrph))]
             [new-up-yield-nodes (rest (dgrph-up-yield-nodes a-dgrph))]
             [new-p-yield-nodes (cons (first (dgrph-up-yield-nodes a-dgrph))
                                      (dgrph-p-yield-nodes a-dgrph))]
             [new-up-hedges (rest (dgrph-up-hedges a-dgrph))]
             [new-p-hedges (cons (first (dgrph-up-hedges a-dgrph)) (dgrph-p-hedges a-dgrph))])
        (if hex?
            (let ([new-up-rules (rest (dgrph-up-rules a-dgrph))]
                  [new-p-rules (cons (first (dgrph-up-rules a-dgrph)) (dgrph-p-rules a-dgrph))])
              (create-dgrphs (dgrph new-up-levels
                                    new-ad-levels
                                    new-nodes
                                    new-up-hex-nodes
                                    new-p-hex-nodes
                                    new-up-yield-nodes
                                    new-p-yield-nodes
                                    new-up-hedges
                                    new-p-hedges
                                    new-up-rules
                                    new-p-rules)
                             (cons a-dgrph lod)
                             #t))
            (let ([new-up-rules (dgrph-up-rules a-dgrph)] [new-p-rules (dgrph-p-rules a-dgrph)])
              (create-dgrphs (dgrph new-up-levels
                                    new-ad-levels
                                    new-nodes
                                    new-up-hex-nodes
                                    new-p-hex-nodes
                                    new-up-yield-nodes
                                    new-p-yield-nodes
                                    new-up-hedges
                                    new-p-hedges
                                    new-up-rules
                                    new-p-rules)
                             (cons a-dgrph lod)
                             #f))))))

;; (listof nodes) -> (listof edges)
;; Recursively creates an edge between the first and second element of the list until it empty
;; We use this list of edges to add invisble edges to the graph, guarenteeing ordering of the yield nodes at the bottom of the graph
(define (create-line-of-edges yield-nodes)
  (if (= (length yield-nodes) 1)
      '()
      (cons (list (first yield-nodes) (second yield-nodes))
            (create-line-of-edges (rest yield-nodes)))))

;; create-graph-structs
;; dgprh -> img
;; Purpose: Creates the final graph structure that will be used to create the images in graphviz
(define (create-graph-structs a-dgrph)
  (let* ([nodes (dgrph-nodes a-dgrph)]
         [levels (first (dgrph-p-levels a-dgrph))]
         [hedges (first (dgrph-p-hedges a-dgrph))]
         [hedge-nodes (extract-nodes hedges)]
         [yield-nodes (first (dgrph-p-yield-nodes a-dgrph))]
         [hex-nodes (first (dgrph-p-hex-nodes a-dgrph))])
    (make-invisible-edge-graph
     (make-edge-graph
      (make-node-graph
       (create-graph 'dgraph
                     #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in" 'splines "polyline" 'overlap "scale"))
       nodes
       hedge-nodes
       hex-nodes
       yield-nodes)
      levels
      hedges)
     (create-line-of-edges yield-nodes))))

(define (remove-every-second lst)
  (if (empty? lst)
      lst
      (if (= (length lst) 1)
          (cons (first lst) '())
          (cons (second lst) (remove-every-second (rest (rest lst))))
          )
      )
  )

(define (copy lst)
  (if (empty? lst)
      '()
      (cons (first lst) (cons (first lst) (copy (rest lst))))))

(define (csg-viz g w #:cpu-cores [cpu-cores #f] . invariants)
  (local [(define derv (csg-derive-edited g w))
          (define w-derv (map (lambda (x) (symbol->fsmlos (first x))) derv))
          (define moved-rules
            (map (lambda (x) (list (second x) (third x))) (move-rule-applications-in-list derv)))
          (define rules
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
          (define renamed (generate-levels (list (csg-getstart g)) moved-rules (make-hash)))
          (define test-rules (copy rules))
          (define dgraph
            (dgrph (rest (third renamed))
                   (list (first (third renamed)))
                   '()
                   (rest (first renamed))
                   (list (first (first renamed)))
                   (rest (second renamed))
                   (list (first (second renamed)))
                   (rest (fourth renamed))
                   (list (first (fourth renamed)))
                   (rest test-rules)
                   (list (first test-rules))))
          (define lod (reverse (create-dgrphs dgraph '() #f)))
          (define graphs (map create-graph-structs lod))
          (define test-w-derv (copy w-derv))
          ]
    (init-viz g
              w
              w-derv 
              rules
              (let ([frst (first graphs)]
                    [fourth (second graphs)])
                (cons frst (cons fourth (remove-every-second (drop graphs 2)))))
              'NO-INV
              #:cpu-cores cpu-cores
              #:special-graphs? 'cfg
              #:rank-node-lst (let ([frst (first (second renamed))]
                                    [fourth (second (second renamed))])
                                (map (lambda (x y) (cons x (foldr (lambda (val accum) (cons (list val) accum))
                                                                         '()
                                                                         y)))
                                     (cons frst (cons fourth (remove-every-second (drop (second renamed) 2))))
                                     (let ([res (append (dgrph-up-hex-nodes (first lod)) (dgrph-p-hex-nodes (first lod)))])
                                       (drop-right (cons '() (cons (first res) (remove-every-second res))) 1))
                                       
                                     )
                                ))
    ))

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