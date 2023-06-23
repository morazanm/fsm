#lang racket

(require "../../fsm-core/interface.rkt" "lib.rkt")
(require 2htdp/universe rackunit)
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

;; (listof states) alphabet state (listof state) (listof ndfa-rule) -> dfa
;; Purpose: Create a dfa from the given ndfa components
(define (convert states sigma start finals rules)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; a world is a structure that consists of
;; cgraph - created graph
;; up-edges - unprocessed edges, edges that are yet to be drawn
;; ad-edges - already drawn edges
;; incl-nodes - states that are already drawn on the graph
(define-struct world (cgraph up-edges ad-edges incl-nodes))




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


;; (listof rules) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph los)
  (foldl (λ (state result) (add-node result
                                     (if (equal? state '())
                                         'ds  
                                         (los2symb state))
                                     #:atb (hash 'color 'black 'shape 'circle 'label
                                                 (if (equal? state '())
                                                     'ds
                                                     (los2symb state))
                                                 'fontcolor 'black)))
         cgraph
         los))


;; (listof rules) -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph lor)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (los2symb (first rule)))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (los2symb (third rule)))
                                    #:atb (hash 'fontsize 20 'style 'solid)))
         cgraph
         lor))


  

;; (listof rules) -> graph
;; Purpose: To create a dfa graph from a given ndfa
(define (create-cgraph lor los)
  (edge-graph (node-graph (create-graph 'cgraph #:atb (hash 'rankdir "LR")) los)
              lor))
      

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
                                                       (first new-ad-edges)))]
               (make-world (create-cgraph new-ad-edges new-incl-nodes)                       
                           new-up-edges                       
                           new-ad-edges
                           new-incl-nodes)))]
        [(key=? "left" a-key)
         (if (empty? (world-ad-edges a-world))
             a-world
             (let* [(new-up-edges (cons (first (world-ad-edges a-world))
                                        (world-up-edges a-world)))
                    (new-ad-edges (rest (world-ad-edges a-world)))
                    (new-incl-nodes (remove-included-node (world-incl-nodes a-world)
                                                          (first (world-ad-edges a-world))
                                                          new-ad-edges))]
               (make-world (create-cgraph new-ad-edges new-incl-nodes)                       
                           new-up-edges                       
                           new-ad-edges
                           new-incl-nodes)))]
        [else a-world]))

;; ndfa -> dfa
;; Purpose: Convert the given ndfa to an equivalent dfa
(define (ndfa2dfa M)
  (if (eq? (sm-type M) 'dfa)
      M
      (convert (sm-states M)
               (sm-sigma M)
               (sm-start M)
               (sm-finals M)
               (sm-rules M))))

;; contains-final-state?
;; state (listof states) -> Boolean
(define (contains-final-state? sss sm-finals)
  (ormap (λ (s) (equal? sss s)) sm-finals))


(define E-SCENE (empty-scene 700 700))


;; draw-world
;; world -> img
;; Purpose: To draw a world image
(define (draw-world a-world)
  (overlay
   (graph->bitmap (world-cgraph a-world) (current-directory) "fsm")
   E-SCENE))

(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C) '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))


;; ndfa --> world
(define (run M)
  (let* [(ss-edges (ndfa2dfa M))
         (super-start-state (first (first ss-edges)))
         (cgraph (create-graph 'cgraph
                               #:atb (hash 'rankdir "LR")))
         (init-graph (if (contains-final-state? super-start-state (sm-finals M))
                         (add-node
                          cgraph
                          (los2symb super-start-state)
                          #:atb (hash 'color 'red
                                      'shape 'doublecircle
                                      'label (los2symb super-start-state)
                                      'fontcolor 'black))
                         (add-node
                          cgraph
                          (los2symb super-start-state)
                          #:atb (hash 'color 'black
                                      'shape 'circle
                                      'label (los2symb super-start-state)
                                      'fontcolor 'black))))]
    (big-bang
        (make-world init-graph
                    ss-edges
                    '()
                    (list (first (first ss-edges))))
                
      [on-draw draw-world]
      [on-key process-key]
      [name 'visualization])))

(run AT-LEAST-ONE-MISSING)


