#lang racket

(require "../../fsm-core/interface.rkt" "lib.rkt")
(require 2htdp/universe rackunit)


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
;; incl-nodes - nodes that are already drawn on the graph
(define-struct world (cgraph up-edges ad-edges incl-nodes))

 

;; remove-up-edge
;; (listof rules) -> (listof rules)
;; Purpose: To remove an unprocessed rule
(define (remove-up-edge lor)
  (if (empty? lor)
      lor
      (remove (last lor) lor)))

;; add-drawn-edge
;; (listof rules) -> (listof rules)
;; Purpose: To add a drawn edge to the list of rules
(define (add-drawn-edge lor)
  (if (empty? lor)
      lor
      (reverse (cons (search-for-next-edge lor)
                     (reverse (lor))))))

;; add-included-nodes
;; (listof rules) -> (listof nodes)
;; Purpose: To add the nodes that are included in the current state
;; of the graph
(define (add-included-nodes lon a-world)
  (if (empty? lon)
      lon
      (append lon (third (search-for-next-edge (world-ad-edges a-world))))))

;; add-up-edge
;; (listof rules) -> (listof rules)
;; Purpose: To add a up-edge to the list of rules
(define (add-up-edge lor)
  (if (empty? lor)
      lor
      (reverse (cons (search-for-next-edge lor)
                     (reverse (lor)))))) 
      
      
;; remove-drawn-edge
;; (listof rules) -> (listof rules)
;; Purpose: To remove a drawn rule
(define (remove-drawn-edge lor)
  (if (empty? lor)
      lor
      (remove (last lor) lor)))


;; world key -> world
;; Purpose: To return the next world based on
;; the given key
(define (process-key a-world a-key)
  (cond [(key=? "right" a-key)
         (make-world (add-cgraph-img ...)
                     (remove-up-edge (world-up-edges a-world))
                     (add-drawn-edge (world-ad-edges a-world))
                     (add-included-nodes (world-incl-nodes a-world)))]
        [(key=? "left" a-key)
         (make-world (remove-cgraph-img ...)
                     (add-up-edge (world-up-edges a-world))
                     (remove-drawn-edge (world-ad-edges a-world))
                     (remove-included-nodes (world-incl-nodes a-world)))]
        [else a-world]))



;; ndfa --> world
(define (run M)
  (let* [(ss-edges (ndfa2dfa M))
         (super-start-state (first (first ss-edges)))]
    (big-bang
        (make-world (add-node
                     (create-graph 'cgraph #:atb (hash 'rankdir "LR"))
                     super-start-state
                     (if (contains-final-state? super-start-state (sm-finals M))
                         #:atb (hash 'color 'red
                                     'shape 'doublecircle
                                     'label (los2symb super-start-state)
                                     'fontcolor 'black)
                         #:atb (hash 'color 'black 'shape 'circle 'label
                                     (if (equal? (first rule) '())
                                         'ds
                                         (los2symb (first rule)))
                                     'fontcolor 'black)))
                    ss-edges
                    '()
                    (list (first (first ss-edges))))
                
      [on-draw draw-world]
      [on-key process-key]
      [name visualization])))
