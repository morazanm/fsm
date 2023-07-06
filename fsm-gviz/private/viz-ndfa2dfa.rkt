#lang fsm

(require "../../fsm-core/interface.rkt" "lib.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)

(define FNAME "fsm")




;;;;;;;;; FUNCTION NDFA->DFA THAT RETURNS A SORTED LIST OF RULES ;;;;;;;;;;;

;; (listof ss) -> ss-name-tbl
;; Purpose: Create a table for ss names
(define (compute-ss-name-tbl super-states)
  (map (λ (ss)
         (if (empty? ss)
             (list ss DEAD)
             (list ss (generate-symbol 'Z '(Z)))))
       super-states))

;; Tests for compute-ss-name-tbl
(check-pred (lambda (tbl)
              (and (list? tbl)
                   (andmap (λ (e) (= (length e) 2)) tbl)
                   (andmap (λ (e) (andmap symbol? (first e)))
                           tbl)
                   (andmap (λ (e) (symbol? (second e))) tbl)))
            (compute-ss-name-tbl '()))

(check-pred (lambda (tbl)
              (and (list? tbl)
                   (andmap (λ (e) (= (length e) 2)) tbl)
                   (andmap (λ (e) (andmap symbol? (first e)))
                           tbl)
                   (andmap (λ (e) (symbol? (second e))) tbl)))
            (compute-ss-name-tbl '((A B) (A B C) () (C))))


;; (listof ss) alphabet emps-tbl (listof ndfa-rule) (listof ss) -> (listof ss-dfa-rule)
;; Purpose: Compute the super state dfa rules
;; Accumulator Invariants:
;; ssts = the super states explored
;; to-search-ssts = the super states that must still be explored
(define (compute-ss-dfa-rules to-search-ssts sigma empties rules ssts)
  ;; ss alphabet (listof ndfa-rule) emps-tbl -> (listof (listof ss))
  ;; Purpose: Compute reachable super states from given super state
  (define (find-reachables ss sigma rules empties)
    (map (λ (st)
           (find-reachables-from-st st sigma rules empties))
         ss))
  ;; state alphabet (listof ndfa-rule) emps-tbl -> (listof ss)
  ;; Purpose: Find the reachable super state from the given state for each element of
  ;; the given alphabet
  (define (find-reachables-from-st st sigma rules empties)
    (map (λ (a)
           (find-reachables-from-st-on-a st a rules empties))
         sigma))
  ;; state symbol (listof ndfa-rule) emps-tbl -> ss
  ;; Purpose: Find the reachable super state from the given state and the given alphabet element
  (define (find-reachables-from-st-on-a st a rules empties)
    (let* [(rls (filter
                 (λ (r)
                   (and (eq? (first r) st) (eq? (second r) a)))
                 rules))
           (to-states (map third rls))]
      (remove-duplicates
       (append-map (λ (st) (extract-empties st empties))
                   to-states))))
  ;; natnum (listof (listof ss)) -> (listof ss)
  ;; Purpose: Return ss of ith (listof state) in each given list element
  (define (get-reachable i reachables)
    (remove-duplicates (append-map
                        (λ (reached) (list-ref reached i))
                        reachables)))
  
  (if (empty? to-search-ssts)
      '()
      (let* [(curr-ss (first to-search-ssts))
             (reachables (find-reachables curr-ss sigma rules empties))
             (to-super-states
              (build-list (length sigma) (λ (i) (get-reachable i reachables))))
             (new-rules (map (λ (sst a) (list curr-ss a sst))
                             to-super-states
                             sigma))]
        (append
         new-rules
         (compute-ss-dfa-rules
          (append (rest to-search-ssts)
                  (filter (λ (ss)
                            (not (member ss (append to-search-ssts ssts))))
                          to-super-states))
          sigma
          empties
          rules
          (cons curr-ss ssts))))))



;; (listof state) rules -> emps-tbl
;; Purpose: Compute empties table for all given states
(define (compute-empties-tbl states rules)
  ;; state (listof state) (listof ndfa-rule) -> (listof ndfa-rule)
  ;; Purpose: Extract empty transitions to non-generated states for the given state
  (define (get-e-trans state gen-states rules)
    (filter (λ (r) (and (eq? (first r) state)
                        (eq? (second r) EMP)
                        (not (member (third r) gen-states))))
            rules))
  ;; (listof state) (listof ndfa-rules) (listof state) -> (listof state)
  ;; Purpose: Compute the empties for the states left to explore in the first given (listof state)
  ;; Accumulator Invariants:
  ;;   to-search = unvisited states reachable by consuming no input
  ;;   visited = visited states reachable by consuming no input
  (define (compute-empties to-search rules visited)
    (if (empty? to-search)
        visited
        (let* [(current (first to-search))
               (current-e-rules
                (get-e-trans current (append to-search visited) rules))]
          (compute-empties (append (rest to-search) (map third current-e-rules))
                           rules
                           (cons current visited)))))
  (map (λ (st) (list st (compute-empties (list st) rules '()))) states))


;; Tests for compute-empties-tbl
(check-equal? (compute-empties-tbl '(X Y Z) `((X ,EMP Y) (Y a Z) (Z ,EMP X)))
              '((X (Y X)) (Y (Y)) (Z (Y X Z))))

(check-equal? (compute-empties-tbl '(W X Y Z)
                                   `((W ,EMP X) (X ,EMP Y) (Y a Z) (Z ,EMP Y) (Z b Z)))
              '((W (Y X W)) (X (Y X)) (Y (Y)) (Z (Y Z))))


;; state emps-tbl -> ss
;; Purpose: Extract the empties of the given state
;; Assume: Given state is in the given list of states
(define (extract-empties st empties)
  (second (first (filter (λ (e) (eq? (first e) st))
                         empties))))

;; Tests for extract-empties
(check-equal? (extract-empties 'A '((S (S B))
                                    (F (F))
                                    (A (A C D))
                                    (C (C))
                                    (D (D))))
              '(A C D))

(check-equal? (extract-empties 'Z '((Z (Z S))
                                    (S ())))
              '(Z S))


;; The convert implementation

;; (listof states) alphabet state (listof state) (listof ndfa-rule) -> dfa rules
;; Purpose: Creates a list of dfa rules for the given ndfa
(define (convert-rules-only states sigma start finals rules)
  (let* [(empties (compute-empties-tbl states rules))
         (ss-dfa-rules
          (compute-ss-dfa-rules (list (extract-empties start empties))
                                sigma
                                empties
                                rules
                                '()))
         (super-states (remove-duplicates
                        (append-map (λ (r) (list (first r) (third r)))
                                    ss-dfa-rules)))
         (ss-name-tbl (compute-ss-name-tbl super-states))]
    ss-dfa-rules
    #;(make-dfa (map (λ (ss) (second (assoc ss ss-name-tbl)))
                     super-states)
                sigma
                (second (assoc (first super-states) ss-name-tbl))
                (map (λ (ss) (second (assoc ss ss-name-tbl)))
                     (filter (λ (ss) (ormap (λ (s) (member s finals)) ss))
                             super-states))
                (map (λ (r) (list (second (assoc (first r) ss-name-tbl))
                                  (second r)
                                  (second (assoc (third r) ss-name-tbl))))
                     ss-dfa-rules)
                'no-dead)))

;; (listof states) alphabet state (listof state) (listof ndfa-rule) -> dfa
;; Purpose: Creates a dfa from the given ndfa
(define (convert-finals-only states sigma start finals rules)
  (let* [(empties (compute-empties-tbl states rules))
         (ss-dfa-rules
          (compute-ss-dfa-rules (list (extract-empties start empties))
                                sigma
                                empties
                                rules
                                '()))
         (super-states (remove-duplicates
                        (append-map (λ (r) (list (first r) (third r)))
                                    ss-dfa-rules)))
         (ss-name-tbl (compute-ss-name-tbl super-states))]
    (filter (λ (ss) (ormap (λ (s) (member s finals)) ss))
            super-states)))


;; ndfa -> dfa rules
;; Purpose: Convert the given ndfa to an equivalent dfa (only outputs rules)
(define (ndfa2dfa-rules-only M)
  (if (eq? (sm-type M) 'dfa)
      M
      (convert-rules-only (sm-states M)
                          (sm-sigma M)
                          (sm-start M)
                          (sm-finals M)
                          (sm-rules M))))

;; ndfa -> dfa
;; Purpose: Convert the given ndfa to an equivalent dfa
(define (ndfa2dfa-finals-only M)
  (if (eq? (sm-type M) 'dfa)
      M
      (convert-finals-only (sm-states M)
                           (sm-sigma M)
                           (sm-start M)
                           (sm-finals M)
                           (sm-rules M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; a world is a structure that consists of
;; cgraph - created dfa graph of a dfa
;; up-edges - unprocessed edges, edges that are yet to be drawn
;; ad-edges - already drawn edges
;; incl-nodes - states that are already drawn on the graph
;; M - a given ndfa
;; hedges - a list of highlighted edges in the ndfa graph
;; fedges - a list of faded edges in the ndfa graph
;; bledges - a list of black/unvisited edges in the ndfa graph

(define-struct world (up-edges ad-edges incl-nodes M hedges fedges bledges))


;; add-included-node
;; (listof states) rule -> (listof states)
;; Purpose: To add a state into the list of states already drawn in the graph
(define (add-included-node los rule)
  (if (member (third rule) los)
      los
      (cons (third rule) los)))

;; remove-included-node
;; (listof states) rule (listof edges)
;; Purpose: To remove a state into the list of states already drawn in the graph
(define (remove-included-node los rule loe)
  (if (ormap (λ (edge) (equal? (third rule) (third edge)))
             loe)
      los
      (remove (third rule) los)))

;; los --> symbol
(define (los2symb los)
  (define (helper los)
    (if (empty? (rest los))
        (symbol->string (first los))
        (string-append (symbol->string (first los))
                       " "
                       (helper (rest los)))))
  (string->symbol (helper los)))


;; contains-final-state?
;; state (listof states) -> Boolean
;; Putpose: To check if the given state is in the list of final states
(define (contains-final-state? sss sm-finals)
  (ormap (λ (s) (equal? sss s)) sm-finals))


;; dfa-node-graph
;; (listof rules) graph (listof states) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (dfa-node-graph cgraph los finals)
  (foldl (λ (state result)  (if  (contains-final-state? state finals)
                                 (add-node
                                  result
                                  (los2symb state)
                                  #:atb (hash 'color (if (equal? state (last los))
                                                         'darkgreen
                                                         'red)
                                              'shape 'doublecircle
                                              'label (if (equal? state '())
                                                         'ds  
                                                         (los2symb state))
                                              'fontcolor 'black))
                                 (add-node
                                  result
                                  (if (equal? state '())
                                      'ds  
                                      (los2symb state))
                                  #:atb (hash 'color (if (equal? state (last los))
                                                         'darkgreen
                                                         'black)
                                              'shape 'circle
                                              'label (if (equal? state '())
                                                         'ds  
                                                         (los2symb state))
                                              'fontcolor 'black))))
         cgraph
         los))


;; dfa-edge-graph
;; (listof rules) -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (dfa-edge-graph cgraph lor added-dfa-edge)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (los2symb (first rule)))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (los2symb (third rule)))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color (if (equal? rule added-dfa-edge)
                                                           'violet
                                                           'black))))
         cgraph
         lor))


  
;; create-dfa-graph
;; (listof rules) -> graph
;; Purpose: To create a dfa graph from a given ndfa
(define (create-dfa-graph lor los finals)
  (if (empty? lor)
      (dfa-edge-graph (dfa-node-graph (create-graph 'dfagraph #:atb (hash 'rankdir "LR")) los finals)
                      lor '())
      (dfa-edge-graph (dfa-node-graph (create-graph 'dfagraph #:atb (hash 'rankdir "LR")) los finals)
                      lor (first lor))))

;; find-empty-transitions
;; (listof state) (listof state) (listof rules) -> (listof rules)
;; Purpose: To compute a list of empty transitions from any of the given states
(define (find-empty-transitions to-visit visited rules)
  (if (empty? to-visit)
      empty
      (let* [(current-state (first to-visit))
             (current-state-empties (filter (λ (rule) (and (eq? (first rule) current-state)
                                                           (eq? (second rule) EMP)))
                                            rules))
             (not-already-visited (filter (λ (cse) (not (member cse (append to-visit visited))))                 
                                          (map third current-state-empties)))]
        (append current-state-empties
                (find-empty-transitions
                 (append (rest to-visit) not-already-visited)
                 (cons current-state visited)
                 rules)))))
                                                              


;; compute-all-hedges
;; ndfa ss -> (listof edges)
;; Purpose: To compute a list of edges that needs to be highlighted in the ndfa graph
(define (compute-all-hedges ndfa-rules to-ss added-dfa-edge)
  (if (empty? to-ss)
      empty
      (let [(new-hedges (if (empty? added-dfa-edge)
                            empty
                            (filter (λ (rule) (and (eq? (second added-dfa-edge)
                                                        (second rule))
                                                   (member (first rule) (first added-dfa-edge))))
                                    ndfa-rules)))]
        (append (find-empty-transitions (list (first to-ss)) empty ndfa-rules)
                new-hedges
                (compute-all-hedges (filter (λ (rule) (not (member rule new-hedges)))
                                            ndfa-rules)
                                    (rest to-ss) added-dfa-edge)))))


;; remove-edges
;; (listof edges) (listof edges) -> (listof edges)
;; Purpose: To remove edges from a list of edges
(define (remove-edges to-remove removing-from)
  (if (empty? removing-from)
      removing-from
      (drop removing-from (length to-remove))))

;; compute-down-fedges
;; (listof rules) (listof edges) -> (listof edges)
;; Purpose: To compute all fedges in down click
(define (compute-down-fedges rules ad-edges)
  (if (empty? ad-edges)
      empty
      (append (compute-all-hedges rules
                                  (third (first ad-edges))
                                  (first ad-edges))
              (compute-down-fedges rules (rest ad-edges)))))

      

;; world key -> world
;; Purpose: To return the next world based on
;; the given key
(define (process-key a-world a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (world-up-edges a-world))
             a-world
             (let* [(new-up-edges (rest (world-up-edges a-world)))
                    (new-ad-edges (cons (first (world-up-edges a-world))
                                        (world-ad-edges a-world)))
                    (new-incl-nodes (add-included-node (world-incl-nodes a-world)
                                                       (first new-ad-edges)))
                    (new-hedges (compute-all-hedges (sm-rules (world-M a-world))
                                                    (third (first new-ad-edges))
                                                    (first new-ad-edges)))
                    (new-fedges (append (world-hedges a-world) (world-fedges a-world)))
                    (new-bledges (remove-edges new-hedges (world-bledges a-world)))]
               (make-world new-up-edges                       
                           new-ad-edges
                           new-incl-nodes
                           (world-M a-world)
                           new-hedges
                           new-fedges
                           new-bledges 
                           )))]
        [(key=? "left" a-key)
         (if (empty? (world-ad-edges a-world))
             a-world
             (let* [(ss-edges (ndfa2dfa-rules-only (world-M a-world)))
                    (super-start-state (first (first ss-edges)))
                    (edge-removed (first (world-ad-edges a-world)))
                    (new-up-edges (cons edge-removed
                                        (world-up-edges a-world)))
                    (new-ad-edges (rest (world-ad-edges a-world)))
                    (new-incl-nodes (remove-included-node (world-incl-nodes a-world)
                                                          edge-removed
                                                          new-ad-edges))
                    (new-hedges (if (empty? new-ad-edges)
                                    (compute-all-hedges (sm-rules (world-M a-world))
                                                        super-start-state '())
                                    (compute-all-hedges (sm-rules (world-M a-world))                                                    
                                                        (third (first new-ad-edges))                                                                                                           
                                                        (first new-ad-edges))))
                    (previous-hedges (world-hedges a-world))
                    (new-bledges (remove-duplicates (append previous-hedges (world-bledges a-world))))
                    (new-fedges (remove-edges previous-hedges (world-fedges a-world)))]
               (make-world new-up-edges                       
                           new-ad-edges
                           new-incl-nodes
                           (world-M a-world)
                           new-hedges
                           new-fedges
                           new-bledges)))]
        [(key=? "down" a-key)
         (let* [(ss-edges (ndfa2dfa-rules-only (world-M a-world)))
                (super-start-state (first (first ss-edges)))                
                (new-up-edges '())
                (new-ad-edges (reverse ss-edges))
                (new-incl-nodes (remove-duplicates
                                 (reverse (cons super-start-state
                                                (reverse (map (λ (edge) (third edge)) new-ad-edges))))))
                (new-hedges (compute-all-hedges (sm-rules (world-M a-world))
                                                (third (first new-ad-edges))
                                                (first new-ad-edges)))
                (new-fedges (append (compute-down-fedges (sm-rules (world-M a-world))
                                                         new-ad-edges)
                                    (compute-all-hedges (sm-rules (world-M a-world))
                                                        super-start-state '())))
                (new-bledges '())]
           (make-world new-up-edges                       
                       new-ad-edges
                       new-incl-nodes
                       (world-M a-world)
                       new-hedges
                       new-fedges
                       new-bledges))]           
        [else a-world]))



(define E-SCENE (empty-scene 1250 600))



;; (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (ndfa-node-graph cgraph M)
  (foldl (λ (state result) (cond [(contains-final-state? state (sm-finals M))
                                  (add-node
                                   result
                                   state
                                   #:atb (hash 'color 'red
                                               'shape 'doublecircle
                                               'label state
                                               'fontcolor 'black))]
                                 [(eq? state (sm-start M))
                                  (add-node
                                   result
                                   state
                                   #:atb (hash 'color 'darkgreen
                                               'shape 'circle
                                               'label state
                                               'fontcolor 'black))]
                                 [else (add-node
                                        result
                                        state
                                        #:atb (hash 'color 'black
                                                    'shape 'circle
                                                    'label state
                                                    'fontcolor 'black))]))
         cgraph
         (sm-states M)))


;; SELL THE LABEL INVARIANT AS A FEATURE
;; for the highlighted edges, the first letter comes from the last edge added to the dfa
;; for the faded edge, the first letter comes from the last dfa edge added that includes the ndfa destination state

;; (listof rules) -> graph
;; Purpose: To create a graph of edges from the given hedges, fedges, and bledges
;; Note: Some edges can be hedges and fedges at the same time, but in this visualization,
;; hedges have a priority
(define (ndfa-edge-graph cgraph hedges fedges bledges)
  (let* [(no-duplicates-fedges (remove-duplicates (filter (λ (fedge) (not (member fedge hedges)))
                                                          fedges)))
         (no-duplicates-bledges (filter (λ (bledge) (not (member bledge (append hedges no-duplicates-fedges))))
                                        bledges))]
    (foldl (λ (rule result) (add-edge result
                                      (second rule)
                                      (first rule)
                                      (third rule)
                                      #:atb (hash 'fontsize 20
                                                  'style 'solid
                                                  'color (cond [(member rule hedges) 'violet]
                                                               [(member rule fedges) 'gray]
                                                               [(member rule bledges) 'black]))))
           cgraph
           (append hedges no-duplicates-fedges no-duplicates-bledges))))

;; make-ndfa-graph
;; world -> graph
;; Purpose: To make an ndfa-graph from the given ndfa
(define (make-ndfa-graph a-world)
  (let* [(ndfa-edge-graph-x (ndfa-edge-graph (ndfa-node-graph (create-graph 'ndfagraph #:atb (hash 'rankdir "LR"))
                                                              (world-M a-world))
                                             (world-hedges a-world)
                                             (world-fedges a-world)
                                             (world-bledges a-world)))]
    ndfa-edge-graph-x))


;; draw-world
;; world -> img
;; Purpose: To draw a world image
(define (draw-world a-world)
  (scale 0.7
         (above (graph->bitmap (make-ndfa-graph a-world) (current-directory) "fsm")
                (overlay
                 (graph->bitmap (create-dfa-graph (world-ad-edges a-world) (world-incl-nodes a-world) (ndfa2dfa-finals-only (world-M a-world))) (current-directory) "fsm")
                 E-SCENE))))

(define aa-ab (make-ndfa `(S A B F)
                         '(a b)
                         'S
                         '(A B)
                         `((S a A)
                           (S a B)
                           (S ,EMP F)
                           (A a A)
                           (B b B))))

;; contains-final-state-run?
;; symbol (listof symbols)
(define (contains-final-state-run? sss sm-finals)
  (ormap (λ (s) (member s sm-finals)) sss))




;; ndfa --> world
(define (run M)
  (let* [(ss-edges (ndfa2dfa-rules-only M))
         (super-start-state (first (first ss-edges)))]
    (big-bang
        (make-world ss-edges
                    '()
                    (list (first (first ss-edges)))
                    M
                    (compute-all-hedges (sm-rules M) super-start-state '())
                    '()
                    (remove (compute-all-hedges (sm-rules M) super-start-state '()) (sm-rules M)))
                
      [on-draw draw-world]
      [on-key process-key]
      [name 'visualization])))

(run aa-ab)

(define EXAMPLE (call-with-values get-display-size empty-scene))
