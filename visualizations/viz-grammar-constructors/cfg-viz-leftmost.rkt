#lang racket/base
(require "cfg-derive-leftmost.rkt"
         "../../fsm-gviz/private/lib.rkt"
         "../../fsm-core/private/cfg-struct.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/misc.rkt"
         "grammar-viz.rkt"
         "../viz-lib/zipper.rkt"
         racket/string
         racket/local
         racket/list
         )

;; Symbol -> Symbol
;; Removes all numbers from the symbol (that were originally added for differentiating in graphviz)
(define (undo-renaming symb)
  (string->symbol (list->string (takef (string->list (symbol->string symb)) (lambda (x) (not (equal? #\_ x)))))))


;; rename-symb
;; MutableHashTable Symbol -> Symbol
;; Purpose:  Returns a unique version of the symbol given (via the addition of
;; a previously unused number to the end of it)
(define (rename-symb hashtable nt)
  (let ([result (hash-ref hashtable nt #f)])
    (if result
        (begin
          (hash-set! hashtable nt (add1 result))
          (string->symbol (format "~s_~s" nt (add1 result))))
        (begin
          (hash-set! hashtable nt 0)
          (string->symbol (format "~s_0" nt))))))



;; nonterminal?
;; symbol -> Boolean
;; Purpose: Determines if the first character within the symbol is a uppercase letter,
;; and hence a nonterminal
(define (nonterminal? symb)
  (char-upper-case? (car (string->list (symbol->string symb)))))

;; find-leftmost-nt
;; (listof symbol) -> (U #f Symbol)
;; Purpose: If it exists, returns the leftmost-nt in the state given. Otherwise, returns false
(define (find-leftmost-nt state)
  (if (null? state)
      #f
      (if (list? state)
          (if (nonterminal? (car state)) (car state) (find-leftmost-nt (cdr state)))
          (if (nonterminal? state) state #f))))

;; w-der -> (listof rules)
;; Purpose: This is just taking the list received from the new w-der and new cfg-derive and moving the rules like such:
;; '( ((S) ()) ((AbA) (AbA)) )
;; where the rule applied was next to where it was applied, to:
;; '( ((S) (AbA)) ((AbA) (AaAbA)) )
;; now the rule is next to where it will be applied
(define (move-rule-applications-in-list lst)
  (if (null? (cdr lst))
      (list (list (car (car lst)) '()))
      (cons (list (car (car lst)) (cadr (cadr lst)))
            (move-rule-applications-in-list (cdr lst)))))

(define (create-rules-leftmost w-der)
  (cond
    [(null? (cdr w-der)) '()]
    [else
     (cons (string-append (symbol->string (find-leftmost-nt (car (car w-der))))
                          " → "
                          (string-join (map symbol->string (cadr (car w-der)))))
             (create-rules-leftmost (cdr w-der)))]))

;; levels -> (listof levels)
;; Purpose: creates a list containing the levels used for each graph generated
(define (create-list-of-levels levels)
  (local [;; levels -> (listof levels)
          ;; Purpose: creates a list containing the levels used for each graph generated in reverse
          (define (create-list-of-levels-helper lvls)
            (if (null? lvls) '() (cons lvls (create-list-of-levels-helper (cdr lvls)))))]
    (create-list-of-levels-helper (reverse levels))))


;; generate-levels-list-helper
;; (listof symbol) (listof (listof symbol)) (listof (listof symbol)) MutableHashTable ->
;; (listof symbol) (U #f symbol)) -> (listof (listof (listof symbol)))




;; generate-levels-list
;; (listof symbol) (listof (listof symbol)) (listof (listof symbol)) MutableHashTable (U 'left 'right 'level) ->
;; (listof (listof (listof symbol)))
;; Purpose: Generates levels from rules
(define (generate-levels-list lst-rule-rhs starting-nt)
  (define used-names (make-hash))
  (define (generate-levels-list-helper lst-rule-rhs current-level prev-level)
    ;; The list of generated rules used contains an empty list denoting no more rules, hence
    ;; the need to call "first" first
    (if (null? lst-rule-rhs)
        ;; If theres no more rules to apply than computation is done
        '()
        (let ([current-nt (find-leftmost-nt current-level)])
          (if current-nt
              (let
                [;; Just sticks a number after the symbol itself, uses a hash table to keep track of what numbers
                 ;; were already used for a specific symbol
                 (renamed-states (car lst-rule-rhs) #;(map (lambda (st) (rename-symb used-names st)) (car lst-rule-rhs)))]
                (cons (map (lambda (st) (list current-nt st)) renamed-states)
                      (generate-levels-list-helper (cdr lst-rule-rhs)
                                                   renamed-states
                                                   (cons current-level prev-level)
                                                   )))
              ;; If its fails to find a nonterminal in the current state, attempt to go back up
              ;; the stack to a previous state
              (if (null? prev-level)
                  ;; If there are no more previous states, than the computation is done
                  '()
                  (let ([prev-current-nt (find-leftmost-nt (car prev-level))])
                    ;; Don't reduce the number of rules here since one was not sucessfully applied, only
                    ;; remove the state we popped off the stack
                    (generate-levels-list-helper lst-rule-rhs
                                                 ;; Need to remove the leftmost-nt from the previous state since we just went
                                                 ;; down its respective path
                                                 ;; when we call the function again with it removed, the next nt will be processed
                                                 (filter (lambda (x) (not (eq? prev-current-nt x)))
                                                         (car prev-level))
                                                 (cdr prev-level))))))))
  (generate-levels-list-helper lst-rule-rhs starting-nt '()))

;; list-of-states
;; derivation-with-rules -> (listof rules)
;; Purpose: Separates the list of states from their rule applications in the list generated by w-der
#;(define (list-of-states lst)
  (map (lambda (x) (car x)) lst))

;; Separates the list of rules from their respective states in the list generated by w-der
;; derivation-with-rules -> derivation
(define (list-of-rules lst)
  (if (null? (cdr lst))
      '()
      (cons (cadr (car lst)) (list-of-rules (cdr lst))))
  #;(map (lambda (x) (cadr x)) lst))

(define (accumulate-previous-ranks rank-node-lst accum)
  (if (null? rank-node-lst)
      '()
      (let ([new-accum (append accum (list (car rank-node-lst)))])
        (cons new-accum (accumulate-previous-ranks (cdr rank-node-lst) new-accum)))))


;; A tree has a value and subtrees
;; A value is Any
;; A subtree is a listof Any
(struct tree (value [subtrees #:mutable]) #:transparent)

(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'skyblue)
(define INVARIANT-HOLDS-COLOR 'chartreuse4)
(define INVARIANT-BROKEN-COLOR 'red2)
(define FONT-SIZE 20)
(define TAPE-SIZE 42)

;; dgrph is a structure that has
;; up-levels - unprocessed levels
;; ad-levels - levels added to the graph
;; nodes - nodes in the graph
;; hedges - highlighted edges of the graphs
;; up-rules - unprocessed grammar rules
;; p-rules - processed grammar rules
(struct dgrph (up-levels ad-levels nodes hedges up-rules p-rules up-yield-trees p-yield-trees))

;; tree Any -> (U #f tree)
;; Finds the search value within the tree using a depth first search
(define (dfs node search-val)
  (if (equal? (tree-value node) search-val)
      node
      (ormap (lambda (node) (dfs node search-val)) (tree-subtrees node))))

(define (create-yield-tree levels g-start)
  (foldl (lambda (val accum)
           (begin
             (set-tree-subtrees! (dfs accum (car (car val)))
                                 (map (lambda (edge) (tree (cadr edge) '())) val))
             accum))
         (tree g-start '())
         levels))

(define (get-leftmost-order-helper subtrees)
  (if (empty? subtrees)
      '()
      (append (get-leftmost-order (first subtrees)) (get-leftmost-order-helper (rest subtrees)))))

(define (get-leftmost-order yt)
  (if (empty? (tree-subtrees yt))
      (if (eq? (tree-value yt) EMP) '() (list (tree-value yt)))
      (append (list (tree-value yt)) (get-leftmost-order-helper (tree-subtrees yt)))))


;; tree -> listof Symbol
;; Accumulates all of the leaf nodes in order (producing the yield of the tree)
(define (get-yield subtree)
  ;; lower?
  ;; symbol -> Boolean
  ;; Purpose: Determines if a symbol is down case
  (define (lower? symbol)
    (not (char-upper-case? (string-ref (symbol->string symbol) 0))))
  
  (define (get-yield-helper subtree)
    (foldl (lambda (node yield)
             (cond
               [(equal? (undo-renaming (tree-value node)) EMP) yield]
               [(empty? (tree-subtrees node)) (append yield (list (tree-value node)))]
               [else (append yield (get-yield-helper node))]))
           '()
           (tree-subtrees subtree)))
  (filter (lambda (node) (lower? node))
          (foldl (lambda (node yield)
                   (cond
                     [(equal? (undo-renaming (tree-value node)) EMP) yield]
                     [(empty? (tree-subtrees node)) (append yield (list (tree-value node)))]
                     [else (append yield (get-yield-helper node))]))
                 '()
                 (tree-subtrees subtree))))

;; tree invariant-function -> (U boolean Symbol)
;; Evaluates the invariant function given to us by the user on the current yield
;; generated by its respective nonterminal
(define (invariant-holds? subtree invar-func)
  (invar-func (map undo-renaming (get-yield subtree))))


;; tree (listof Symbol) (listof (list Symbol ((listof Symbol) -> (U boolean Symbol)) -> (U boolean Symbol)
;; Checks all invariants against all of their respective nodes
(define (check-all-invariants tree nonterminals-to-check invariants)
  (local [(define (check-invariant invariant-nt invariant-func)
            (local [(define (find-all-invariant-nodes nts)
                      (if (empty? nts)
                          '()
                          (if (equal? invariant-nt (undo-renaming (first nts)))
                              (cons (first nts) (find-all-invariant-nodes (rest nts)))
                              (find-all-invariant-nodes (rest nts)))))
                    (define (check-all-invariant-nodes nonterminals invar-func broken-nodes)
                      (if (empty? nonterminals)
                          (if (empty? broken-nodes) #t broken-nodes)
                          (if (invariant-holds? (dfs tree (first nonterminals)) invar-func)
                              (check-all-invariant-nodes (rest nonterminals) invar-func broken-nodes)
                              (check-all-invariant-nodes (rest nonterminals)
                                                         invar-func
                                                         (cons (first nonterminals) broken-nodes)))
                          ))]
              (check-all-invariant-nodes (find-all-invariant-nodes nonterminals-to-check)
                                         invariant-func
                                         '())))
          (define (check-all-invariants-helper nonterminals-to-check invariants broken-nodes)
            (if (empty? invariants)
                (if (empty? broken-nodes) '() broken-nodes)
                (let ([result (check-invariant (first (first invariants))
                                               (second (first invariants)))])
                  (if (list? result)
                      (check-all-invariants-helper nonterminals-to-check
                                                   (rest invariants)
                                                   (append result broken-nodes))
                      (check-all-invariants-helper nonterminals-to-check
                                                   (rest invariants)
                                                   broken-nodes)))))]
    (check-all-invariants-helper nonterminals-to-check invariants '())))



;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph
                         lon
                         hedge-nodes
                         yield-node
                         broken-invariants?
                         producing-nodes
                         has-invariant)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color (cond
                                          [(member state hedge-nodes) HEDGE-COLOR]
                                          [(member state yield-node) YIELD-COLOR]
                                          [else 'black])
                                 'style 'filled
                                 'fillcolor (cond
                                              [(not (member (undo-renaming state) has-invariant)) 'white]
                                              [(and (member (undo-renaming state) has-invariant)
                                                    (not (member state producing-nodes)))
                                               'white]
                                              [(and (member (undo-renaming state) has-invariant)
                                                    (member state producing-nodes)
                                                    (member state broken-invariants?))
                                               INVARIANT-BROKEN-COLOR]
                                              [(and (member (undo-renaming state) has-invariant)
                                                    (member state producing-nodes)
                                                    (not (member state broken-invariants?)))
                                               INVARIANT-HOLDS-COLOR])
                                 'shape 'circle
                                 'label (string->symbol (list->string (takef (string->list (symbol->string state))
                                                                                   (lambda (x) (not (equal? #\_ x))))
                                                                                   ))
                                 'fontcolor 'black
                                 'font "Sans"
                                 'penwidth (cond
                                             [(member state hedge-nodes) 3.0]
                                             [(member state yield-node) 3.0]
                                             [else 1.0]))))
         graph
         (reverse lon)))

(define (make-invis-edges graph lvl)
  (if (= (length lvl) 1)
      graph
      (make-invis-edges
       (add-edge graph "" (first lvl) (second lvl) #:atb (hash 'style 'invisible 'arrowhead 'none))
       (rest lvl))))

;; graph (listof edges) -> graph
;; Creates invisible edges so that ordering of the yield nodes is always maintained
(define (make-invisible-edge-graph graph rank-node-lvls)
  (foldr (lambda (lvls accum) (make-invis-edges accum lvls)) graph rank-node-lvls))

;; make-edge-graph
;; graph (listof level) -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph loe hedges)
  (foldl (lambda (rules result)
           (if (empty? (first rules))
               result
               (foldl (lambda (rule result)
                        (add-edge result
                                  ""
                                  (first rule)
                                  (second rule)
                                  #:atb (hash 'fontsize
                                              FONT-SIZE
                                              'style
                                              (if (member rule hedges)
                                                  'dashed
                                                  'solid)
                                              'color
                                              (if (member rule hedges) HEDGE-COLOR 'black))))
                      result
                      rules)))
         graph
         (reverse loe)))

;; create-graph-structs
;; dgprh -> img
;; Purpose: Creates the final graph structure that will be used to create the images in graphviz
(define (create-graph-structs a-dgrph invariants derv-order root-node rank-node-lvls)
  (let* ([nodes (dgrph-nodes a-dgrph)]
         [levels (map reverse (dgrph-ad-levels a-dgrph))]
         [reversed-levels (reverse levels)]
         [hedges (dgrph-hedges a-dgrph)]
         [invariant-nts (map first invariants)]
         [producing-nodes (map (lambda (edge) (first edge)) (append* levels))]
         [invariant-nodes
          (cons root-node
                (append-map
                 (lambda (lvl)
                   (let* ([nodes (map (lambda (edge) (second edge)) lvl)]
                          [invar-nodes (filter (lambda (node) (member node producing-nodes)) nodes)])
                     (cond
                       [(equal? derv-order 'left) (reverse invar-nodes)]
                       [(equal? derv-order 'right) invar-nodes]
                       [else invar-nodes])))
                 levels))]
         [broken-invariant?
          (check-all-invariants (first (dgrph-p-yield-trees a-dgrph)) invariant-nodes invariants)]
         [hedge-nodes (map (λ (x) (if (empty? x) '() (second x))) hedges)]
         [yield-node (map (λ (x) (if (empty? x) '() (first x))) hedges)])
    (make-invisible-edge-graph
     (make-edge-graph
      (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                       nodes
                       hedge-nodes
                       yield-node
                       broken-invariant?
                       producing-nodes
                       invariant-nts)
      reversed-levels
      hedges)
     rank-node-lvls)))

;; dgrph (listof (list nonterminal predicate)) starting-nonterminal derivation-order
;; Purpose: Creates the list of broken invariants for a given dgrph
(define (create-invariant-nodes a-dgrph invariants root-node derv-order)
  (let* (#;[nodes (dgrph-nodes a-dgrph)]
         [levels (map reverse (dgrph-ad-levels a-dgrph))]
         [producing-nodes (map (lambda (edge) (first edge)) (append* levels))]
         [invariant-nodes
          (let ([all-but-starting-nt
                 (append-map
                  (lambda (lvl)
                    (let* ([nodes (map (lambda (edge) (second edge)) lvl)]
                           [invar-nodes (filter (lambda (node) (member node producing-nodes)) nodes)])
                      (cond
                        [(equal? derv-order 'left) (reverse invar-nodes)]
                        [(equal? derv-order 'right) invar-nodes]
                        [(equal? derv-order 'level-left) (reverse invar-nodes)]
                        [(equal? derv-order 'level-right) invar-nodes])))
                  levels)])
            (if (member root-node producing-nodes)
                (cons root-node all-but-starting-nt)
                all-but-starting-nt))]
         [broken-invariants
          (check-all-invariants (first (dgrph-p-yield-trees a-dgrph)) invariant-nodes invariants)])
    broken-invariants))

;; create-dgraphs
;; dgrph (listof dgrph) -> (listof dgrph)
;; Purpose: To create all the dgrphs for graph imgs
(define (create-dgrphs a-dgrph)
  (if (empty? (dgrph-up-levels a-dgrph))
      (cons a-dgrph '())
      (let* ([new-up-levels (rest (dgrph-up-levels a-dgrph))]
             [new-ad-levels (cons (first (dgrph-up-levels a-dgrph)) (dgrph-ad-levels a-dgrph))]
             [new-nodes (extract-nodes new-ad-levels)]
             [new-hedges (first (dgrph-up-levels a-dgrph))]
             [new-up-rules (rest (dgrph-up-rules a-dgrph))]
             [new-p-rules (cons (first (dgrph-up-rules a-dgrph)) (dgrph-p-rules a-dgrph))]
             [new-up-yield-trees (rest (dgrph-up-yield-trees a-dgrph))]
             [new-p-yield-trees (cons (first (dgrph-up-yield-trees a-dgrph))
                                      (dgrph-p-yield-trees a-dgrph))])
        (cons a-dgrph (create-dgrphs (dgrph new-up-levels
                                            new-ad-levels
                                            new-nodes
                                            new-hedges
                                            new-up-rules
                                            new-p-rules
                                            new-up-yield-trees
                                            new-p-yield-trees)
                                     )))))

#;(define (get-ordered-invariant-nodes ordered-nodes invar-nodes)
  (define (get-ordered-invariant-nodes-helper ordered-nodes invar-nodes)
    (filter (lambda (node) (member node invar-nodes)) ordered-nodes))
  (if (empty? ordered-nodes)
      '()
      (cons (get-ordered-invariant-nodes-helper (first ordered-nodes) (first invar-nodes))
            (get-ordered-invariant-nodes (rest ordered-nodes) (rest invar-nodes)))))

(define (w-der-with-rules derivation)
  (map (lambda (state) (list (symbol->fsmlos (car state)) (symbol->fsmlos (cadr state))))
       derivation))

(define (cfg-viz cfg word #:derv-type [derv-type 'left] #:cpu-cores [cpu-cores #f]. invariants)
(let ([derivation (cfg-derive-leftmost cfg word)])
        (if (string? derivation)
            derivation
            (let* ([der-with-fsmlos (map (lambda (state) (list (symbol->fsmlos (car state))
                                                              (symbol->fsmlos (cadr state))))
                                        derivation)]
                   [der-with-fsmlos-moved (move-rule-applications-in-list der-with-fsmlos)]
                   [rules
                    (cons "" (map (lambda (rule) (let ([res (find-leftmost-nt (car rule))])
                                                   (if res
                                                       (string-append (symbol->string (find-leftmost-nt (car rule)))
                                                                      " → "
                                                                      (string-join (map symbol->string (cadr rule))))
                                                       '())))
                                  der-with-fsmlos-moved))]
                   [renamed-rule-rhss (let ([a-hash (make-hash)])
                                        (map (lambda (x) (map (lambda (y) (rename-symb a-hash y)) x))
                                             (list-of-rules der-with-fsmlos-moved)))]
                   [renamed (generate-levels-list
                             renamed-rule-rhss
                             (cfg-get-start cfg))]
                   #;[rank-node-lvls (cons (list (list (cfg-get-start cfg)))
                                         (accumulate-previous-ranks
                                          renamed-rule-rhss
                                          (list (list (cfg-get-start cfg)))))]
                   [yield-trees (map (lambda (x) (create-yield-tree x (cfg-get-start cfg))) (map reverse (create-list-of-levels renamed)))]
                   
                   
                   [lod (create-dgrphs (dgrph renamed
                                              '()
                                              '()
                                              '()
                                              (cdr rules)
                                              (list (car rules))
                                              (map (lambda (x) (car yield-trees)) yield-trees)
                                              (list (tree (cfg-get-start cfg) '()))))
                        #;(reverse (create-dgrphs dgraph '()))]
                   #;[invar-nodes
                    (map (lambda (a-dgrph)
                           (create-invariant-nodes a-dgrph invariants (cfg-get-start cfg) derv-type))
                         lod)]
                   #;[ordered-nodes
                    (reverse (map get-leftmost-order yield-trees))]
                   #;(define (get-ordered-invariant-nodes ordered-nodes invar-nodes)
                     #;(define (get-ordered-invariant-nodes-helper ordered-nodes invar-nodes)
                       (filter (lambda (node) (member node invar-nodes)) ordered-nodes))
                     (if (empty? ordered-nodes)
                         '()
                         (cons (filter (lambda (node) (member node (first invar-nodes))) (first ordered-nodes))
                               #;(get-ordered-invariant-nodes-helper (first ordered-nodes) (first invar-nodes))
                               (get-ordered-invariant-nodes (rest ordered-nodes) (rest invar-nodes)))))
                   [broken-invariants
                    (if (empty? invariants)
                        'NO-INV
                        (list->zipper (cons '()
                                            (for/list ([dgraph (in-list lod)]
                                                       [ordered-nodes (in-list (cons (list (cfg-get-start cfg)) (reverse (map get-leftmost-order yield-trees))))]
                                                       #:do [(define invar-nodes (create-invariant-nodes dgraph invariants (cfg-get-start cfg) derv-type))]
                                                       )
                                              
                                              (reverse (rest (for/list ([inv-nodes (in-list invar-nodes)])
                                                               (undo-renaming (filter (lambda (node) (member node inv-nodes)) ordered-nodes))))))
                                            #;(map (lambda (lst) (map undo-renaming lst))
                                                 (map reverse
                                                      (rest (get-ordered-invariant-nodes
                                                             (cons (list (cfg-get-start cfg)) (reverse (map get-leftmost-order yield-trees)))
                                                             (map (lambda (a-dgrph)
                                                                    (create-invariant-nodes a-dgrph invariants (cfg-get-start cfg) derv-type))
                                                                  lod))))))))]
                   [removed-dashed-rank-node-lvls (map (lambda (x)
                                                         (map (lambda (y)
                                                                (map (lambda (z) (los->symbol (remove '- (symbol->list z)))) y)) x))
                                                       (cons (list (list (cfg-get-start cfg)))
                                                             (accumulate-previous-ranks
                                                              renamed-rule-rhss
                                                              (list (list (cfg-get-start cfg))))))]
                   [graphs (map (lambda (dgrph node-lvls)
                                  (create-graph-structs dgrph
                                                        invariants
                                                        derv-type
                                                        (cfg-get-start cfg)
                                                        node-lvls))
                                lod
                                removed-dashed-rank-node-lvls)])
              (init-viz cfg
                        word
                        (map (lambda (x) (car x)) der-with-fsmlos)
                        rules
                        graphs
                        broken-invariants
                        #:cpu-cores cpu-cores
                        #:special-graphs? 'cfg
                        #:rank-node-lst removed-dashed-rank-node-lvls)))))


(define numb>numa
  (make-unchecked-cfg
   '(S A)
   '(a b)
   `((S ,ARROW b) (S ,ARROW AbA) (A ,ARROW AaAbA) (A ,ARROW AbAaA) (A ,ARROW ,EMP) (A ,ARROW bA))
   'S))


(define palindrome
  (make-unchecked-cfg '(S A)
                      '(a b)
                      '((S -> ε) (S -> aSa) (S -> bSb) (S -> aAa) (S -> bAb) (A -> aS) (A -> bS))
                      'S))

(define (A-INV w)
  (let ([as (filter (lambda (symb) (eq? symb 'a)) w)] [bs (filter (lambda (symb) (eq? symb 'b)) w)])
    (>= (length bs) (length as))))

(define (S-INV w)
  (let ([as (filter (lambda (symb) (eq? symb 'a)) w)]
        [bs (filter (lambda (symb) (eq? symb 'b)) w)]
        [As (filter (lambda (symb) (eq? symb 'A)) w)])
    (> (length bs) (length as))
    ))


#;(cfg-viz palindrome '(a a b a a b a) (list 'A A-INV) (list 'S S-INV))