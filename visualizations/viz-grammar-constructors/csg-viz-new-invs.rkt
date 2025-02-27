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
  ;; list list -> num
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
                                  hedge-nodes
                                  rules-used)
    (if (empty? (first (first rules)))
        (list hex-nodes yield-nodes levels hedge-nodes rules-used)
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
               [after-replaced-symbols (drop to-be-replaced-symbols-plus-tail replaced-str-length)]
               [new-curr-state (append all-before-to-be-replaced-symbols replacement-symbols after-replaced-symbols)])
          (generate-levels-helper
           ;; curr-state
           new-curr-state
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

           ;; rules-used
           (cons (list curr-rule (list all-before-to-be-replaced-symbols to-be-replaced-symbols after-replaced-symbols) (list all-before-to-be-replaced-symbols replacement-symbols after-replaced-symbols)) rules-used)
           ))))
  (map reverse (generate-levels-helper curr-state rules used-names (list '()) (list '()) (list '()) (list '()) (list (list '() '() '())))))


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
   hedges
   rules))

;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

(define (insert-at lst pos x)
  (define-values (before after) (split-at lst pos))
  (append before (cons x after)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes hex-nodes yield-node rule-used invariant)
  (let ([invariant-result (if (not (eq? 'NO-INV invariant))
                              (if (hash-ref invariant (if (empty? (first rule-used)) '() (insert-at (first rule-used) 1 ARROW)) #f)
                                  ((hash-ref invariant (if (empty? (first rule-used)) '() (insert-at (first rule-used) 1 ARROW)))
                                   (map (lambda (x) (if (empty? x) '() (map undo-renaming x))) (second rule-used))
                                   (map (lambda (x) (if (empty? x) '() (map undo-renaming x))) (third rule-used)))
                                  '())
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
                                                      (if (empty? invariant-result)
                                                          (if (member state hedge-nodes)
                                                              HEDGE-COLOR
                                                              (if (member state yield-node)
                                                                  YIELD-COLOR
                                                                  'black))
                                                          (if invariant-result
                                                              INV-HELD-COLOR
                                                              FAILED-INV-COLOR))
                                                      (if (member state hedge-nodes)
                                                          HEDGE-COLOR
                                                          'black))
                                                  (if (member state hedge-nodes)
                                                      HEDGE-COLOR
                                                      (if (member state yield-node)
                                                          YIELD-COLOR
                                                          'black)))
                                   'style (if (not (eq? 'NO-INV invariant))
                                              (if (empty? invariant-result)
                                                  'solid
                                                  (if (member state yield-node)
                                                      'filled
                                                      'solid))
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
(define (create-dgrphs levels hex-nodes yield-nodes hedges rules)
  (map (lambda (level hex-node yield-node hedge rule)
         (dgrph
          level
          (extract-nodes level)
          hex-node
          yield-node
          hedge
          rule))
       levels hex-nodes yield-nodes hedges rules))

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
     (dgrph-rules a-dgrph)
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
         (lod (create-dgrphs (third renamed) (first renamed) (second renamed) (fourth renamed) (fifth renamed)))
         (inv-hash (foldr (lambda (val accum) (hash-set accum (first val) (second val)))
                          (hash)
                          invariant))
         (graphs (map (lambda (x) (create-graph-structs x (if (empty? invariant)
                                                              'NO-INV
                                                              inv-hash))) lod))]
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



;; Throughout every step |A| + |a| = |B| + |b| = |C| + |c|
;; S - Promise to generate a^n b^n c^n
;; S -> EMP = L = S -> L = EMP
;; S -> K = L = S -> L = K

;; K - Promise to generate an equal number>=1 of A's B's and C's
;; K -> ABCK = L = ( { star A, star B, star C } K ) ->
;;             L = ( { plus A, plus B, plus C } K )
;; K -> G = L = ( { plus A, plus B, plus C } K ) ->
;;          L = ( {plus A, plus B, plus C } G )

;; A - Promise to generate an "a"
;; BA -> AB = L = ( {kleene-plus A's, kleene-plus B's, star C's} {kleene plus A's, star B's, C's} {K or {G star c} or {H star b plus c}}) ->
;;            L = ( {two or more A's, kleene-star B's, C's} {kleene-plus B's, kleene star A's, C's} {K or {G star c} or {H star b plus c}} )

;; CA -> AC = L = ( {kleene-plus A's, kleene-star B's, plus C's} {kleene plus A's, star B's, C's} {K or {G star c}} ) ->
;;            L = ( {two or more A's, kleene-star B's, C's} {kleene-plus C's, kleene star A's, B's} {K or {G star c}} )

;; B - Promise to generate an "b"
;; CB -> BC = L = ( {kleene-plus A's, kleene-star B's, plus C's} {kleene-star A's, plus B's, star C's} {K or {G star c}} ) ->
;;            L = ( {kleene-plus A's, kleene-plus B's, star C's} {kleene-star A's, B's, kleene-plus C's} {K or {G star c}} )

;; G - Generates "c"s
;; BG -> BH = L = ( {kleene-plus A's, plus B's} G {kleene-plus c's} ) ->
;;            L = ( {kleene-plus A's, plus B's} H {kleene-plus c's} )
;; CG -> Gc = L = ( {kleene-plus A's, B's, kleene-plus C's} G {kleene-star c's} ) ->
;;            L = ( {kleene-plus A's, B's, kleene-star C's} G {kleene-plus c's} )

;; H - Generates "b"s
;; BH -> Hb = L = ( {kleene-plus A's, plus B's} H {kleene-star b's, kleene-plus c's} ) ->
;;            L = ( {kleene-plus A's, star B's} H {kleene-plus b's, kleene-plus c's} )
;; AH -> AI = L = ( {kleene-plus A's} H {b^n c^n} ) ->
;;            L = ( {kleene-plus A's} I {b^n c^n} )

;; I - Generates "a"s
;; AI -> Ia = L = ( {kleene-plus A's} I {kleene-star a's b^n c^n} ) ->
;;            L = ( {kleene-star A's} I {kleene-plus a b^n c^n} )
;; I -> EMP = L = ( I {a^n b^n c^n} ) ->
;;            L = ( a^n b^n c^n )

(define anbncn
  (make-unchecked-csg
   '(S K A B C G H I)
   '(a b c)
   `((S ,ARROW ,EMP)
     (S ,ARROW K)
     (K ,ARROW ABCK)
     (K ,ARROW G)
     (BA ,ARROW AB)
     (CA ,ARROW AC)
     (CB ,ARROW BC)
     (CG ,ARROW Gc)
     (BG ,ARROW BH)
     (BH ,ARROW Hb)
     (AH ,ARROW AI)
     (AI ,ARROW Ia)
     (I ,ARROW ,EMP))
   'S))

(define (equal-num-abc? word)
  (equal? (+ (length (filter (lambda (x) (eq? 'A x)) word))
             (length (filter (lambda (x) (eq? 'a x)) word)))
          (+ (length (filter (lambda (x) (eq? 'B x)) word))
             (length (filter (lambda (x) (eq? 'b x)) word)))
          (+ (length (filter (lambda (x) (eq? 'C x)) word))
             (length (filter (lambda (x) (eq? 'c x)) word)))))

(define (StoEMP pre-cond post-cond)
  (and (and (empty? (first pre-cond)) (equal? '(S) (second pre-cond)) (empty? (third pre-cond)))
       (and (empty? (first post-cond)) (equal? (list EMP) (second post-cond)) (empty? (third post-cond)))))

(define (StoK pre-cond post-cond)
  (and (and (empty? (first pre-cond)) (equal? '(S) (second pre-cond)) (empty? (third pre-cond)))
       (and (empty? (first post-cond)) (equal? '(K) (second post-cond)) (empty? (third post-cond)))))

(define (kleene-star-ABC word)
  (if (empty? word)
      #t
      (and (and (eq? 'A (first word)) (eq? 'B (second word)) (eq? 'C (third word))) (kleene-star-ABC (drop word 3)))))

(define (kleene-plus-ABC word)
  (if (< (length word) 3)
      #f
      (and (and (eq? 'A (first word)) (eq? 'B (second word)) (eq? 'C (third word))) (kleene-star-ABC (drop word 3)))))

(define (KtoABCK pre-cond post-cond)
  (and (and (kleene-star-ABC (first pre-cond)) (equal? '(K) (second pre-cond)) (empty? (third pre-cond)))
       (let* ([new-LHS-of-k (append (first post-cond) (drop-right (second post-cond) 1))])
         (and (kleene-plus-ABC new-LHS-of-k) (equal? '(A B C K) (second post-cond)) (empty? (third post-cond))))))

(define (kleene-plus-A-plus-B-plus-C word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 0)
       (> (length (filter (lambda (x) (eq? 'B x)) word)) 0)
       (> (length (filter (lambda (x) (eq? 'C x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)
                                               (eq? 'C x)))) word)) 0)))

(define (KtoG pre-cond post-cond)
  (and (and (kleene-plus-A-plus-B-plus-C (first pre-cond)) (equal? '(K) (second pre-cond)) (empty? (third pre-cond)))
       (and (kleene-plus-A-plus-B-plus-C (first post-cond)) (equal? '(G) (second post-cond)) (empty? (third post-cond)))))

(define (kleene-plus-A-star-B-star-C word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)
                                               (eq? 'C x)))) word)) 0)))

(define (kleene-star-A-star-B-star-C word)
  (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                          (eq? 'B x)
                                          (eq? 'C x)))) word)) 0))

(define (two-or-more-A-star-B-star-C word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 1)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)
                                               (eq? 'C x)))) word)) 0)))
(define (kleene-star-c word)
  (= (length (filter (lambda (x) (not (or (eq? 'c x)))) word)) 0))

(define (G-star-c word)
  (and (equal? 'G (first word))
       (kleene-star-c (rest word))))

(define (KorG-star-c word)
  (or (equal? '(K) word)
      (G-star-c word)))

(define (H-star-b-plus-c word)
  (and (equal? 'H (first word))
       (let* ([len (length (filter (lambda (x) (eq? 'b x)) (rest word)))]
              [bs (take (rest word) len)]
              [cs (drop (rest word) len)])
         (and (andmap (lambda (x) (eq? 'b x)) bs)
              (andmap (lambda (x) (eq? 'c x)) cs)))))

(define (KorG-star-c-or-H-star-b-plus-c word)
  (or (equal? '(K) word)
      (G-star-c word)
      (H-star-b-plus-c word)))

(define (kleene-star-A-star-B-plus-C word)
  (and (> (length (filter (lambda (x) (eq? 'C x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)
                                               (eq? 'C x)))) word)) 0)))

(define (kleene-star-A-plus-B-star-C word)
  (and (> (length (filter (lambda (x) (eq? 'B x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)
                                               (eq? 'C x)))) word)) 0)))

(define (BAtoAB pre-cond post-cond)
  (and (and (kleene-plus-A-star-B-star-C (first pre-cond))
            (equal? '(B A) (second pre-cond))
            (kleene-star-A-star-B-star-C (takef (third pre-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                      (eq? 'G x)
                                                                                      (eq? 'H x))))))
            (KorG-star-c-or-H-star-b-plus-c (dropf (third pre-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                         (eq? 'G x)
                                                                                         (eq? 'H x)))))))
       (let ([new-LHS (append (first post-cond) (list (first (second post-cond))))]
             [new-RHS (append (list (second (second post-cond))) (takef (third post-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                                               (eq? 'G x)
                                                                                                               (eq? 'H x))))))])
         (and (two-or-more-A-star-B-star-C new-LHS)
              (equal? '(A B) (second post-cond))
              (kleene-star-A-plus-B-star-C new-RHS)
              (KorG-star-c-or-H-star-b-plus-c (dropf (third post-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                            (eq? 'G x)
                                                                                            (eq? 'H x))))))))))
              
(define (CAtoAC pre-cond post-cond)
  (and (and (kleene-plus-A-star-B-star-C (first pre-cond))
            (equal? '(C A) (second pre-cond))
            (kleene-star-A-star-B-star-C (takef (third pre-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                      (eq? 'G x))))))
            (KorG-star-c (dropf (third pre-cond) (lambda (x) (not (or (eq? 'K x)
                                                                      (eq? 'G x)))))))
       (let ([new-LHS (append (first post-cond) (list (first (second post-cond))))]
             [new-RHS (append (list (second (second post-cond))) (takef (third post-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                                               (eq? 'G x))))))])
         (and (two-or-more-A-star-B-star-C new-LHS)
              (equal? '(A C) (second post-cond))
              (kleene-star-A-star-B-plus-C new-RHS)
              (KorG-star-c (dropf (third post-cond) (lambda (x) (not (or (eq? 'K x)
                                                                         (eq? 'G x))))))))))

(define (kleene-plus-A-plus-B-star-C word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 0)
       (> (length (filter (lambda (x) (eq? 'B x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)
                                               (eq? 'C x)))) word)) 0)))

(define (CBtoBC pre-cond post-cond)
  (and (and (kleene-plus-A-star-B-star-C (first pre-cond))
            (equal? '(C B) (second pre-cond))
            (kleene-star-A-star-B-star-C (takef (third pre-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                      (eq? 'G x))))))
            (KorG-star-c (dropf (third pre-cond) (lambda (x) (not (or (eq? 'K x)
                                                                      (eq? 'G x)))))))
       (let ([new-LHS (append (first post-cond) (list (first (second post-cond))))]
             [new-RHS (append (list (second (second post-cond))) (takef (third post-cond) (lambda (x) (not (or (eq? 'K x)
                                                                                                               (eq? 'G x))))))])
         (and (kleene-plus-A-plus-B-star-C new-LHS)
              (equal? '(B C) (second post-cond))
              (kleene-star-A-star-B-plus-C new-RHS)
              (KorG-star-c (dropf (third post-cond) (lambda (x) (not (or (eq? 'K x)
                                                                         (eq? 'G x))))))))))

(define (kleene-plus-A-plus-B word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 0)
       (> (length (filter (lambda (x) (eq? 'B x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)))) word)) 0)))

(define (kleene-plus-c word)
  (and (> (length (filter (lambda (x) (eq? 'c x)) word)) 0)
       (= (length (filter (lambda (x) (not (eq? 'c x))) word)) 0)))

(define (BGtoBH pre-cond post-cond)
  (and (let ([new-LHS (append (first pre-cond) (list (first (second pre-cond))))])
         (and (kleene-plus-A-plus-B new-LHS) (equal? '(B G) (second pre-cond)) (kleene-plus-c (third pre-cond))))
       (let ([new-LHS (append (first post-cond) (list (first (second post-cond))))])
         (and (kleene-plus-A-plus-B new-LHS) (equal? '(B H) (second post-cond)) (kleene-plus-c (third post-cond))))))

(define (CGtoGc pre-cond post-cond)
  (and (and (kleene-plus-A-plus-B-star-C (first pre-cond)) (equal? '(C G) (second pre-cond)) (kleene-star-c (third pre-cond)))
       (let ([new-RHS (append (list (second (second post-cond))) (third post-cond))])
         (and (kleene-plus-A-plus-B-star-C (first post-cond)) (equal? '(G c) (second post-cond)) (kleene-plus-c new-RHS)))))

(define (b^nc^n word)
  (and (= (length (filter (lambda (x) (eq? 'b x)) word))
          (length (filter (lambda (x) (eq? 'c x)) word)))
       (= (length (filter (lambda (x) (not (or (eq? 'b x)
                                               (eq? 'c x)))) word)) 0)))
(define (kleene-plus-A word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 0)
       (= (length (filter (lambda (x) (not (eq? 'A x))) word)) 0)))

(define (AHtoAI pre-cond post-cond)
  (and (let ([new-LHS (append (first pre-cond) (list (first (second pre-cond))))])
         (and (kleene-plus-A new-LHS) (equal? '(A H) (second pre-cond)) (b^nc^n (third pre-cond))))
       (let ([new-LHS (append (first post-cond) (list (first (second post-cond))))])
         (and (kleene-plus-A new-LHS) (equal? '(A I) (second post-cond)) (b^nc^n (third post-cond))))))

(define (kleene-plus-b-plus-c word)
  (and (> (length (filter (lambda (x) (eq? 'b x)) word)) 0)
       (> (length (filter (lambda (x) (eq? 'c x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'b x)
                                               (eq? 'c x)))) word)) 0)))

(define (kleene-star-b-plus-c word)
  (and (> (length (filter (lambda (x) (eq? 'c x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'b x)
                                               (eq? 'c x)))) word)) 0)))

(define (kleene-plus-A-star-B word)
  (and (> (length (filter (lambda (x) (eq? 'A x)) word)) 0)
       (= (length (filter (lambda (x) (not (or (eq? 'A x)
                                               (eq? 'B x)))) word)) 0)))

(define (BHtoHb pre-cond post-cond)
  (and (let ([new-LHS (append (first pre-cond) (list (first (second pre-cond))))])
         (and (kleene-plus-A-plus-B new-LHS) (equal? '(B H) (second pre-cond)) (kleene-star-b-plus-c (third pre-cond))))
       (let ([new-RHS (append (list (second (second post-cond))) (third post-cond))])
         (and (kleene-plus-A-star-B (first post-cond)) (equal? '(H b) (second post-cond)) (kleene-plus-b-plus-c new-RHS)))))

(define (in-lang? word)
  (let* ([len (/ (length word) 3)]
         [as (take word len)]
         [bs (take (drop word len) len)]
         [cs (take (drop word (* len 2)) len)])
    (and (andmap (lambda (x) (equal? x 'a)) as)
         (andmap (lambda (x) (equal? x 'b)) bs)
         (andmap (lambda (x) (equal? x 'c)) cs))))

(define (ItoEMP pre-cond post-cond)
  (and (and (empty? (first pre-cond)) (equal? '(I) (second pre-cond)) (in-lang? (third pre-cond)))
       (and (empty? (first post-cond)) (equal?  (list EMP) (second post-cond)) (in-lang? (third post-cond)))))

(define (kleene-star-a-b^nc^n word)
  (let* ([as (filter (lambda (x) (equal? 'a x)) word)]
         [dropped-as (drop word (length as))]
         [bs (take dropped-as (/ (length dropped-as) 2))]
         [cs (drop dropped-as (/ (length dropped-as) 2))])
    (if (> (length (filter (lambda (x) (not (or (eq? 'a x) (eq? 'b x) (eq? 'c x)))) word)) 0)
        #f
        (and (andmap (lambda (x) (eq? 'b x)) bs)
             (andmap (lambda (x) (eq? 'c x)) cs)))))

(define (kleene-plus-a-b^nc^n word)
  (let* ([as (filter (lambda (x) (equal? 'a x)) word)]
         [dropped-as (drop word (length as))]
         [bs (take dropped-as (/ (length dropped-as) 2))]
         [cs (drop dropped-as (/ (length dropped-as) 2))])
    (if (or (not (> (length as) 0))
            (> (length (filter (lambda (x) (not (or (eq? 'a x) (eq? 'b x) (eq? 'c x)))) word)) 0))
        #f
        (and (andmap (lambda (x) (eq? 'b x)) bs)
             (andmap (lambda (x) (eq? 'c x)) cs)))))

(define (kleene-star-A word)
  (= (length (filter (lambda (x) (not (eq? 'A x))) word)) 0))

(define (AItoIa pre-cond post-cond)
  (and (let ([new-LHS (append (first pre-cond) (list (first (second pre-cond))))])
         (and (kleene-plus-A new-LHS) (equal? '(A I) (second pre-cond)) (kleene-star-a-b^nc^n (third pre-cond))))
       (let ([new-RHS (append (list (second (second post-cond))) (third post-cond))])
         (and (kleene-star-A (first post-cond)) (equal? '(I a) (second post-cond)) (kleene-plus-a-b^nc^n new-RHS)))))

(csg-viz anbncn '(a a b b c c) (list (list 'S ARROW 'K) StoK)
         (list (list 'S ARROW EMP) StoEMP)
         (list (list 'K ARROW 'ABCK) KtoABCK)
         (list (list 'K ARROW 'G) KtoG)
         (list (list 'BA ARROW 'AB) BAtoAB)
         (list (list 'CA ARROW 'AC) CAtoAC)
         (list (list 'CB ARROW 'BC) CBtoBC)
         (list (list 'CG ARROW 'Gc) CGtoGc)
         (list (list 'BG ARROW 'BH) BGtoBH)
         (list (list 'BH ARROW 'Hb) BHtoHb)
         (list (list 'AH ARROW 'AI) AHtoAI)
         (list (list 'AI ARROW 'Ia) AItoIa)
         (list (list 'I ARROW EMP) ItoEMP))

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