#lang racket
(require "../fsm-gviz/private/lib.rkt"
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/interface.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         "viz.rkt"
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define even-bs-odd-as (make-cfg '(S A B C)
                                 '(a b)
                                 `((S ,ARROW aA)
                                   (S ,ARROW bB)
                                   (S ,ARROW a)
                                   (A ,ARROW aS)
                                   (A ,ARROW bC)
                                   (B ,ARROW aC)
                                   (B ,ARROW bS)
                                   (C ,ARROW aB)
                                   (C ,ARROW bA)
                                   (C ,ARROW b))
                                 'S))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define FONT-SIZE 20)
(define TAPE-SIZE 42)

;; dgrph is a structure that has
;; up-levels - unprocessed levels
;; ad-levels - levels added to the graph
;; nodes - nodes in the graph
;; hedges - highlighted edges of the graphs
;; up-rules - unprocessed grammar rules
;; p-rules - processed grammar rules
(struct dgrph (up-levels ad-levels nodes hedges up-rules p-rules))


;; nonterminal?
;; symbol -> Boolean
;; Purpose: Determines if the first character within the symbol is a uppercase letter,
;; and hence a nonterminal
(define (nonterminal? symb) (char-upper-case? (first (string->list (symbol->string symb)))))

;; find-leftmost-nt
;; (listof symbol) -> (U #f Symbol)
;; Purpose: If it exists, returns the leftmost-nt in the state given. Otherwise, returns false
(define (find-leftmost-nt state)
  (if (empty? state)
      #f
      (if (list? state)
          (if (nonterminal? (first state))
              (first state)
              (find-leftmost-nt (rest state))
              )
          (if (nonterminal? state)
              state
              #f))))

;; (listof symbol) -> (U #f Symbol)
;; If it exists, returns the leftmost-nt in the state given. Otherwise, returns false
(define (find-rightmost-nt state)
  (if (list? state)
      (find-leftmost-nt (reverse state))
      (find-leftmost-nt (list state))))


;; create-rules
;; (listof symbol) -> (listof string)
;; Purpose: To create the rules for leftmost derivation
(define (create-rules-leftmost w-der)
  (cond [(empty? w-der)
         '()]
        [(= 1 (length w-der))
         '()]
        [else
         (append (list (string-append (symbol->string (find-leftmost-nt (first (first w-der))))
                                      " → "
                                      (string-join (map symbol->string (second (first w-der))))))
                 (create-rules-leftmost (rest w-der)))]))

;; create-rules
;; (listof symbol) -> (listof string)
;; Purpose: To create the rules for rightmost derivation
(define (create-rules-rightmost w-der)
  (cond [(empty? w-der)
         '()]
        [(= 1 (length w-der))
         '()]
        [else
         (append (list (string-append (symbol->string (find-rightmost-nt (first (first w-der))))
                                      " → "
                                      (string-join (map symbol->string (second (first w-der))))))
                 (create-rules-rightmost (rest w-der)))]))

;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes yield-node)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(member state hedge-nodes)
                                      HEDGE-COLOR]
                                     [(member state yield-node)
                                      YIELD-COLOR]
                                     [else 'black])
                        'shape 'circle
                        'label (string->symbol (string (string-ref (symbol->string state) 0)))
                        'fontcolor 'black
                        'font "Sans")))
         graph
         (reverse lon)))

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
                                  #:atb (hash 'fontsize FONT-SIZE
                                              'style 'solid
                                              'color (if (member rule hedges)
                                                         HEDGE-COLOR
                                                         'black))))
                      result
                      rules)))
         graph
         (reverse loe)))

;; create-graph-structs
;; dgprh -> img
;; Purpose: Creates the final graph structure that will be used to create the images in graphviz
(define (create-graph-structs a-dgrph)
  (let* [(nodes (dgrph-nodes a-dgrph))
         (levels (reverse (map reverse (dgrph-ad-levels a-dgrph))))
         (hedges (dgrph-hedges a-dgrph))
         (hedge-nodes (map (λ (x) (if (empty? x)
                                      '()
                                      (second x)))
                           hedges))
         (yield-node (map (λ (x) (if (empty? x)
                                     '()
                                     (first x)))
                          hedges))]
    (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                      nodes hedge-nodes yield-node)
                     levels hedges)))

;; create-dgraphs
;; dgrph (listof dgrph) -> (listof dgrph)
;; Purpose: To create all the dgrphs for graph imgs
(define (create-dgrphs a-dgrph lod)
  (if (empty? (dgrph-up-levels a-dgrph))
      (cons a-dgrph lod)
      (let* [(new-up-levels (rest (dgrph-up-levels a-dgrph)))
             (new-ad-levels (cons (first (dgrph-up-levels a-dgrph))
                                  (dgrph-ad-levels a-dgrph)))
             (new-nodes (extract-nodes new-ad-levels))
             (new-hedges (first (dgrph-up-levels a-dgrph)))
             (new-up-rules (rest (dgrph-up-rules a-dgrph)))
             (new-p-rules (cons (first (dgrph-up-rules a-dgrph))
                                (dgrph-p-rules a-dgrph)))]
        (create-dgrphs
         (dgrph new-up-levels                      
                new-ad-levels
                new-nodes
                new-hedges
                new-up-rules
                new-p-rules)
         (cons a-dgrph lod)))))


;; cfg-derive-with-rule-application
;; cfg word derivation-type -> derivation-with-rules
;; A derivaton-with-rule takes the form of: (Listof (Listof Symbol) OR Symbol) OR String
;; A derivation type is a symbol that is either 'left, 'right, or 'level
(define (cfg-derive-with-rule-application g w derv-type)
  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (cond [(empty? st) #f]
          [(not (member (car st) (cfg-get-alphabet g))) (car st)]
          [else (get-first-nt (cdr st))]
          )
    )
  
  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st) (get-first-nt (reverse st)))

  ;; symbol CFG -> (Listof CFG-rule)
  ; A CFG-rule is a structure, (CFG-rule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                   (cfg-get-the-rules g)))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol)s (listof symbol)s -> (listof symbol)s
  ;; Purpose: Replaces the leftmost nonterminal with a righthand side of a rule
  (define (subst-first-nt state rght)
    (cond [(not (member (car state) (cfg-get-alphabet g)))
           (if (eq? (car rght) EMP)
               (cdr state)
               (append rght (cdr state))
               )
           ]
          [else (cons (car state) (subst-first-nt (cdr state) rght))]))

  ; ASSUMPTION: state has at least one NT
  ;; (listof symbol) (listof symbol) -> (listof symbol)
  ;; Purpose: Replaces the rightmost nonterminal with a righthand side of a rule
  (define (subst-last-nt state rght) (reverse (subst-first-nt (reverse state) (reverse rght))))

  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (cond 
      [(not (member (car st) (cfg-get-alphabet g))) '()]
      [else (cons (car st) (get-starting-terminals (cdr st)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    (cond [(= n 0) '()]
          [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))

  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let* ((start-terms-st (get-starting-terminals st))
           (start-terms-w (if (> (length start-terms-st) (length w))
                              #f
                              (get-first-n-terms w (length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))

  ;; (Listof (List (Listof Symbol) (Listof Symbol))) (Listof (Listof (List (Listof Symbol) (Listof Symbol)))) CFG ->
  ;; (U (Listof (U (Listof Symbol) Symbol)) String))
  (define (make-deriv visited derivs g)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))

    (cond [(eq? derv-type 'left)
           (cond [(empty? derivs) (format "~s is not in L(G)." w)]
                 [(> (count-terminals (first (first (first derivs))) (cfg-get-alphabet g))
                     (length w))
                  (make-deriv visited (cdr derivs) g)]
                 [else 
                  (let* ((fderiv (car derivs))
                         (state (car fderiv))
                         (fnt (get-first-nt (first state))))
                    (if (false? fnt)
                        (if (equal? w (first state))
                            (append-map (lambda (l) (if (equal? w (first l))
                                                        (if (null? l)
                                                            (list EMP)
                                                            (list (list (los->symbol (first l)) (los->symbol (second l)))))
                                                        (list (list (los->symbol (first l))
                                                                    (los->symbol (second l))) ARROW)))
                                        (reverse fderiv))
                            (make-deriv visited (cdr derivs) g))
                        (let*
                            ((rls (get-rules fnt g))
                             (rights (map cfg-rule-rhs rls))
                             (new-states (filter (lambda (st) (and (not (member st visited))
                                                                   (check-terminals? (first state)))) 
                                                 (map (lambda (rght) (list (subst-first-nt (first state) rght)
                                                                           rght)) rights))))
                          (make-deriv (append new-states visited)
                                      (append (cdr derivs) 
                                              (map (lambda (st) (cons st fderiv)) 
                                                   new-states))
                                      g))))]
                 )
           ]
          [(eq? derv-type 'right)
           (cond [(empty? derivs) (format "~s is not in L(G)." w)]
                 [(> (count-terminals (first (first (first derivs))) (cfg-get-alphabet g))
                     (length w))
                  (make-deriv visited (cdr derivs) g)]
                 [else 
                  (let* ((fderiv (car derivs))
                         (state (car fderiv))
                         (fnt (get-last-nt (first state))))
                    (if (false? fnt)
                        (if (equal? w (first state))
                            (append-map (lambda (l) (if (equal? w (first l))
                                                        (if (null? l)
                                                            (list EMP)
                                                            (list (list (los->symbol (first l))
                                                                        (los->symbol (second l))))
                                                            )
                                                        (list (list (los->symbol (first l))
                                                                    (los->symbol (second l))) ARROW)))
                                        (reverse fderiv))
                            (make-deriv visited (cdr derivs) g))
                        (let*
                            ((rls (get-rules fnt g))
                             (rights (map cfg-rule-rhs rls))
                             (new-states (filter (lambda (st) (and (not (member st visited))
                                                                   (check-terminals? (first state)))) 
                                                 (map (lambda (rght) (list (subst-last-nt (first state) rght)
                                                                           rght))
                                                      rights))))
                          (make-deriv (append new-states visited)
                                      (append (cdr derivs) 
                                              (map (lambda (st) (cons st fderiv)) 
                                                   new-states))
                                      g))))])]))   
  (let ((ng-derivation (make-deriv (list (list (list (cfg-get-start g)) '() )) 
                                   (list (list (list (list (cfg-get-start g)) '() )))
                                   g)))
    (if (string? ng-derivation)
        ng-derivation
        (make-deriv (list (list (list (cfg-get-start g)) '() )) 
                    (list (list (list (list (cfg-get-start g)) '() )))
                    g
                    false))))

;; w-der
;; derivation -> derivation-list
;; Purpose: To turn the derivation into a list
(define (w-der-with-rules derivation)
  (map (lambda (state) (list (symbol->fsmlos (first state)) (symbol->fsmlos (second state))))
       (filter (λ (x) (not (equal? x '->)))
               derivation)
       )
  )

;; rename-symb
;; MutableHashTable Symbol -> Symbol
;; Purpose:  Returns a unique version of the symbol given (via the addition of
;; a previously unused number to the end of it)
(define (rename-symb hashtable nt)
  (let [(result (hash-ref hashtable nt #f))] 
    (if result
        (begin (hash-set! hashtable nt (add1 result))
               (string->symbol (format "~s~s" nt (add1 result))))
        (begin (hash-set! hashtable nt 0)
               (string->symbol (format "~s0" nt))))))




;; generate-levels-list-helper
;; (listof symbol) (listof (listof symbol)) (listof (listof symbol)) MutableHashTable ->
;; (listof symbol) (U #f symbol)) -> (listof (listof (listof symbol)))
(define (generate-levels-list-helper current-state rules prev-states used-names find-nt-func)
  ;; The list of generated rules used contains an empty list denoting no more rules, hence
  ;; the need to call "first" first
  (if (empty? (first rules))
      ;; If theres no more rules to apply than computation is done
      '()
      (let [(current-nt (find-nt-func current-state))]
        (if (boolean? current-nt)
            ;; If its fails to find a nonterminal in the current state, attempt to go back up
            ;; the stack to a previous state
            (if (empty? prev-states)
                ;; If there are no more previous states, than the computation is done
                '()
                (let* [(prev-state (first prev-states))
                       (prev-current-nt (find-nt-func prev-state))
                       ;; Need to remove the leftmost-nt from the previous state since we just went
                       ;; down its respective path
                       ;; when we call the function again with it removed, the next nt will be processed
                       (updated-states (filter (lambda (x) (not (eq? prev-current-nt x))) prev-state))
                       ]
                  ;; Don't reduce the number of rules here since one was not sucessfully applied, only
                  ;; remove the state we popped off the stack
                  (generate-levels-list-helper updated-states
                                               rules
                                               (rest prev-states)
                                               used-names
                                               find-nt-func)))
            (local [;; Just sticks a number after the symbol itself, uses a hash table to keep track of what numbers
                    ;; were already used for a specific symbol
                    (define renamed-states (map (lambda (st) (rename-symb used-names st)) (first rules)))
                    
                    ;; Creates a new level by taking the current rule that is meant to be applied at this point of
                    ;; the derivation
                    ;; and creating an edge between each of the elements within and the current nonterminal being
                    ;;processed
                    (define (new-level start) (map (lambda (st) (list start st))
                                                   renamed-states))]
              (cons (new-level current-nt)
                    (generate-levels-list-helper renamed-states
                                                 (rest rules)
                                                 (cons current-state prev-states)
                                                 used-names find-nt-func))
              )))))


;; yield is a strucutre that has
;; pr - processed part of the word
;; nt - nonterminal
;; up - unprocessed part of the word
;; derv-type - Symbol that is either 'level-left or 'level-right
(struct yield (pr nt up derv-type))

;; list any -> list
;; Purpose: Returns the list up until and excluding the value given
(define (take-until lst val)
  (if (eq? (first lst) val)
      '()
      (cons (first lst) (take-until (rest lst) val))
      )
  )

;; generate-levels-list
;; (listof symbol) (listof (listof symbol)) (listof (listof symbol)) MutableHashTable (U 'left 'right 'level) ->
;; (listof (listof (listof symbol)))
;; Purpose: Generates levels from rules
(define (generate-levels-list current-state rules prev-states used-names derv-type)
  (cond [(eq? derv-type 'left)
         (generate-levels-list-helper current-state rules prev-states used-names find-leftmost-nt)]
        [(eq? derv-type 'right)
         (generate-levels-list-helper current-state rules prev-states used-names find-rightmost-nt)]
        )
  )

;; (listof symbol) -> symbol
;; Purpose: Returns the leftmost nonterminal held within the list of symbols
(define (get-leftmost-nt st)
  (cond [(empty? st) #f]
        [(nonterminal? (first st)) (first st)]
        [else (get-leftmost-nt (cdr st))]
        )
  )

;; (listof symbol) -> symbol
;; Purpose: Returns the rightmost nonterminal held within the list of symbols
(define (get-rightmost-nt st) (get-leftmost-nt (reverse st)))

;; yield -> (listof symbol)
;; Purpose: Returns the current state of the word being derived
(define (get-current-state a-yield)
  (if (eq? 'level-left (yield-derv-type a-yield))
      (append (yield-pr a-yield) (yield-up a-yield))
      (append (yield-up a-yield) (yield-pr a-yield))
      )                             
  )

;; (listof symbol) (listof symbol) (listof (listof symbol)) mutable-hashtable ((listof symbol) -> symbol) symbol
;; Purpose: Generates the levels used by graphviz to produce the images for the visualization
(define (generate-levels-bfs-list-helper unprocessed-yield processed-yield rules used-names find-nt-func derv-type)
  (if (empty? rules)
      '()
      (let [(current-nt (find-nt-func unprocessed-yield))]
        (if (false? current-nt)
            (generate-levels-bfs-list-helper processed-yield '() rules used-names find-nt-func derv-type)
            (local [(define renamed-states (map (lambda (st) (rename-symb used-names st)) (first rules)))
                    (define (new-level start) (map (lambda (st) (list start st))
                                                   renamed-states))
                    ]
              (cons (new-level current-nt)
                    (generate-levels-bfs-list-helper
                     (if (eq? derv-type 'level-left)
                         (rest (member current-nt unprocessed-yield))
                         (take-until unprocessed-yield current-nt)
                         )
                     (if (eq? derv-type 'level-left)
                         (append (if (list? processed-yield)
                                     processed-yield
                                     (list processed-yield)
                                     )
                                 (if (list? renamed-states)
                                     renamed-states
                                     (list renamed-states)
                                     )
                                 )
                         (append (if (list? renamed-states)
                                     renamed-states
                                     (list renamed-states)
                                     )
                                 (if (list? processed-yield)
                                     processed-yield
                                     (list processed-yield)
                                     )
                                 )
                         )
                     (rest rules)
                     used-names
                     find-nt-func
                     derv-type
                     )
                    )
                     
              )
            )
        )
      )
  )

;; (listof symbol) (listof symbol) (listof (listof symbol)) mutable-hashtable symbol
;; Purpose: Generates the levels used by graphviz to produce the images for the visualization
(define (generate-levels-bfs-list unprocessed-yield processed-yield rules used-names derv-type)
  (cond [(eq? derv-type 'level-left)
         (generate-levels-bfs-list-helper unprocessed-yield processed-yield rules used-names get-leftmost-nt derv-type)
         ]
        [(eq? derv-type 'level-right)
         (generate-levels-bfs-list-helper unprocessed-yield processed-yield rules used-names get-rightmost-nt derv-type)
         ]
        )
  )

;; w-der -> (listof rules)
;; Purpose: This is just taking the list received from the new w-der and new cfg-derive and moving the rules like such:
;; '( ((S) ()) ((AbA) (AbA)) )
;; where the rule applied was next to where it was applied, to:
;; '( ((S) (AbA)) ((AbA) (AaAbA)) )
;; now the rule is next to where it will be applied
(define (move-rule-applications-in-list lst)
  (if (= (length lst) 1)
      (list (list (first (first lst)) '() ))
      (cons (list (first (first lst)) (second (second lst)))
            (move-rule-applications-in-list (rest lst)))
      ))

;; list-of-states
;; derivation-with-rules -> (listof rules)
;; Purpose: Separates the list of states from their rule applications in the list generated by w-der
(define (list-of-states lst) (map (lambda (x) (first x))  lst))

;; Separates the list of rules from their respective states in the list generated by w-der
;; derivation-with-rules -> derivation
(define (list-of-rules lst) (map (lambda (x) (second x)) lst))

;; (listof symbol) (listof yield) -> w-der
;; Purpose: Converts a yield derivation into the form of a word derivation that the viz expects
(define (convert-yield-deriv-to-word-deriv input-word yield-deriv)
  (remove-duplicates (map (lambda (x) (get-current-state (first x))) yield-deriv))
  )

;; (listof yield) -> (listof symbol)
;; Purpose: Creates a list of rules for a leftmost bfs derivation that are used in the visualization
(define (create-rules-levels-leftmost yield-deriv)
  (cond [(empty? yield-deriv)
         '()]
        [(= 1 (length yield-deriv))
         '()]
        [(not (empty? (yield-up (first (first yield-deriv)))))
         (append (list (string-append (symbol->string (get-leftmost-nt (yield-up (first (first yield-deriv)))))
                                      " → "
                                      (string-join (map symbol->string (second (first yield-deriv))))))
                 (create-rules-levels-leftmost (rest yield-deriv)))
         ]
        [(empty? (yield-up (first (first yield-deriv))))
         (create-rules-levels-leftmost (rest yield-deriv))
         ]
        )
  )

;; (listof yield) -> (listof symbol)
;; Purpose: Creates a list of rules for a rightmost bfs derivation that are used in the visualization
(define (create-rules-levels-rightmost yield-deriv)
  (cond [(empty? yield-deriv)
         '()]
        [(= 1 (length yield-deriv))
         '()]
        [(not (empty? (yield-up (first (first yield-deriv)))))
         (append (list (string-append (symbol->string (get-rightmost-nt (yield-up (first (first yield-deriv)))))
                                      " → "
                                      (string-join (map symbol->string (second (first yield-deriv))))))
                 (create-rules-levels-rightmost (rest yield-deriv)))
         ]
        [(empty? (yield-up (first (first yield-deriv))))
         (create-rules-levels-rightmost (rest yield-deriv))
         ]
        )
  )

;; cfg (listof symbol) symbol -> (listof yield)
;; Purpose: Computes the derivation of the given word using the cfg in a direction given by derv-type
;; either a leftmost or rightmost
(define (cfg-derive-levels g w derv-type)

  ;; (listof symbol) -> Symbol
  ;; Purpose: Returns leftmost nonterminal
  (define (get-first-nt st)
    (cond [(empty? st) #f]
          [(nonterminal? (first st)) (first st)]
          [else (get-first-nt (cdr st))]
          )
    )
  
  ;; (listof symbol) -> symbol
  ;; Purpose: Returns rightmost nonterminal
  (define (get-last-nt st) (get-first-nt (reverse st)))
  
  ; (listof (listof symbol)) --> (listof symbol)
  (define (get-starting-terminals st)
    (cond 
      [(not (member (car st) (cfg-get-alphabet g))) '()]
      [else (cons (car st) (get-starting-terminals (cdr st)))]))

  ; (listof (listof symbol)) natnum --> (listof symbol)
  (define (get-first-n-terms w n)
    (cond [(= n 0) '()]
          [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))
  
  ; (list (listof symbol)) --> boolean
  (define (check-terminals? st)
    (let* ((start-terms-st (get-starting-terminals st))
           (start-terms-w (if (> (length start-terms-st) (length w))
                              #f
                              (get-first-n-terms w (length start-terms-st)))))
      (cond [(false? start-terms-w) #f]
            [else (equal? start-terms-st start-terms-w)])))

  ;; symbol CFG -> (Listof CFG-rule)
  ; A CFG-rule is a structure, (CFG-rule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (define (get-rules nt g)
    (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
            (cfg-get-the-rules g)))

  ; ASSUMPTION: yield has at least one NT in unprocessed field
  ;; (listof symbol) (listof symbol) (listof symbol) -> yield
  ;; Purpose: Replaces the leftmost nonterminal within the unprocessed field of a yield
  ;; with a righthand side of a rule
  (define (subst-first-nt new-up new-p rght)
    (if (not (member (first new-up) (cfg-get-alphabet g)))
        (if (eq? (first rght) EMP)
            (yield new-p '() (rest new-up) 'level-left)
            (yield (append (if (list? new-p)
                               new-p
                               (list new-p)
                               )
                           rght) '() (rest new-up) 'level-left)
            )
        (subst-first-nt (rest new-up) (append (if (list? new-p)
                                                  new-p
                                                  (list new-p)
                                                  )
                                              (if (list? (first new-up))
                                                  (first new-up)
                                                  (list (first new-up))
                                                  )
                                              ) rght)
        )
    )

  ; ASSUMPTION: yield has at least one NT in unprocessed field
  ;; (listof symbol) (listof symbol) (listof symbol) -> yield
  ;; Purpose: Replaces the rightmost nonterminal within the unprocessed field of a yield
  ;; with a righthand side of a rule
  (define (subst-last-nt new-up new-p rght)
    (define (subst-last-nt-helper new-up new-p rght)
      (if (not (member (first new-up) (cfg-get-alphabet g)))
          (if (eq? (first rght) EMP)
              (yield (reverse (if (list? new-p)
                                  new-p
                                  (list new-p)
                                  )
                              )
                     '()
                     (reverse (rest new-up))
                     'level-right)
              (yield (reverse (append (if (list? new-p)
                                          new-p
                                          (list new-p)
                                          ) rght)) '() (reverse (rest new-up)) 'level-right)
              )
          (subst-last-nt-helper (rest new-up) (append (if (list? new-p)
                                                          new-p
                                                          (list new-p)
                                                          )
                                                      (if (list? (first new-up))
                                                          (first new-up)
                                                          (list (first new-up))
                                                          )
                                                      ) rght)
          )
      )
    (subst-last-nt-helper (reverse new-up)
                          (reverse new-p)
                          (reverse rght)
                          )
    )

  ;; (listof symbol) -> boolean
  ;; Purpose: Checks to see if there are any nonterminals within the given state
  (define (any-nt? state) (ormap (lambda (x) (not (member x (cfg-get-alphabet g)))) state))

  (define (make-deriv visited derivs g)
    ;; (Listof Symbol) (Listof Symbol) -> Natural
    (define (count-terminals st sigma)
      (length (filter (lambda (a) (member a sigma)) st)))
    
    (cond [(empty? derivs) (format "~s is not in L(G)." w)]
          [(> (count-terminals (get-current-state (first (first (first derivs))))
                               (cfg-get-alphabet g))
              (length w))
           (make-deriv visited (rest derivs) g)]
          [else
           (let* [(current-deriv (first derivs))
                  (current-yield-and-rule (first current-deriv))
                  (current-yield (first current-yield-and-rule))
                  (state (get-current-state current-yield))
                  (current-nt (if (eq? derv-type 'level-left)
                                  (get-first-nt (yield-up current-yield))
                                  (get-last-nt (yield-up current-yield))
                                  )
                              )
                  ]
             (if (false? current-nt)
                 (if (equal? w state)
                     (reverse (cons (list (yield state '() '() (yield-derv-type current-yield)) (second current-yield-and-rule)) (rest current-deriv)))
                     (if (any-nt? state)
                         (make-deriv visited (append (rest derivs)
                                                     (list (cons (list (yield '() '() state (yield-derv-type current-yield))
                                                                       (second current-yield-and-rule)
                                                                       )
                                                                 current-deriv)
                                                           )
                                                     )
                                     g)
                         (make-deriv visited (rest derivs) g)
                         )
                     )
                 (let* [(rls (get-rules current-nt g))
                        (rights (map cfg-rule-rhs rls))
                        (new-yields (filter (lambda (st) (and (not (member st visited))
                                                              (check-terminals? state)))
                                            (map (lambda (rght) (list (if (eq? derv-type 'level-left)
                                                                          (subst-first-nt (yield-up current-yield) (yield-pr current-yield) rght)
                                                                          (subst-last-nt (yield-up current-yield) (yield-pr current-yield) rght)
                                                                          )
                                                                      rght)) rights)))
                        ]
                   (make-deriv (append new-yields visited)
                               (append (rest derivs) 
                                       (map (lambda (yd) (cons yd current-deriv)) 
                                            new-yields))
                               g)
                   )
                 )
             )
           ]
          )
    )
  (let ((ng-derivation (make-deriv (list (list (yield '() '() (list (cfg-get-start g)) derv-type) '() )) 
                                   (list (list (list (yield '() '() (list (cfg-get-start g)) derv-type) '() )))
                                   g)))
    (if (string? ng-derivation)
        ng-derivation
        (make-deriv (list (list (yield '() '() (list (cfg-get-start g)) derv-type) '() ))
                    (list (list (list (yield '() '() (list (cfg-get-start g)) derv-type) '() )))
                    g)))
  )
         
;; cfg-viz
(define (cfg-viz cfg word derv-type)
  (if (or (eq? derv-type 'left)
          (eq? derv-type 'right)
          )
      (let [(derivation (cfg-derive-with-rule-application cfg word derv-type))]
        (if (string? derivation)
            derivation
            (let* [(der-with-rules (w-der-with-rules derivation))
                   (rules (cons "" (cond [(eq? derv-type 'left)
                                          (create-rules-leftmost (move-rule-applications-in-list der-with-rules))
                                          ]
                                         [(eq? derv-type 'right)
                                          (create-rules-rightmost (move-rule-applications-in-list der-with-rules))])))
                   (w-der (list-of-states der-with-rules))
                   (renamed (generate-levels-list (first (first (first der-with-rules)))
                                                  (list-of-rules (move-rule-applications-in-list der-with-rules))
                                                  '()
                                                  (make-hash)
                                                  derv-type)
                            )
                   (dgraph (dgrph renamed '() '() '() (rest rules) (list (first rules))))
                   (lod (reverse (create-dgrphs dgraph '())))
                   (graphs (map create-graph-structs lod))]
              (run-viz cfg word w-der rules graphs))))
      (let [(derivation (cfg-derive-levels cfg word derv-type))]
        (if (string? derivation)
            derivation
            (let* [(rules (cons "" (cond [(eq? derv-type 'level-left)
                                          (create-rules-levels-leftmost (move-rule-applications-in-list derivation))
                                          ]
                                         [(eq? derv-type 'level-right)
                                          (create-rules-levels-rightmost (move-rule-applications-in-list derivation))
                                          ]
                                         )
                                )
                          )
                   (w-der (convert-yield-deriv-to-word-deriv word derivation))
                   (renamed (generate-levels-bfs-list '(S)
                                                      '()
                                                      (rest (map (lambda (x) (second x))
                                                                 (append (filter (lambda (x) (not (empty? (yield-up (first x)))))
                                                                                 derivation)
                                                                         (list (last derivation))
                                                                         )
                                                                 )
                                                            )
                                                      (make-hash)
                                                      derv-type
                                                      )
                            )
                   (dgraph (dgrph renamed '() '() '() (rest rules) (list (first rules))))
                   (lod (reverse (create-dgrphs dgraph '())))
                   (graphs (map create-graph-structs lod))
                   ]
              (run-viz cfg word w-der rules graphs)
              )
            )
        )
      )
  )

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

(cfg-viz numb>numa '(a b b) 'level-right)
