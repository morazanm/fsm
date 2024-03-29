#lang racket

(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "definitions-viz.rkt"
         "run-viz.rkt"
         "../fsm-core/interface.rkt")

(provide ndfa2dfa-viz)

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



;; (listof state) rules -> emps-tbl
;; Purpose: Compute empties table for all given states
(define (compute-empties-tbl states rules)  
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
    ss-dfa-rules))

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
(define (compute-ss-edges M)
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

(define E-SCENE (empty-scene 1250 600))
(define E-SCENE-TOOLS (overlay (beside (above (above (triangle 30 'solid 'black)
                                                     (rectangle 10 30 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Restart the visualization" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rectangle 30 10 'solid 'black)
                                                      (rotate 270 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Move one step forward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rotate 90 (triangle 30 'solid 'black))
                                                      (rectangle 30 10 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Move one step backward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (above (rectangle 10 30 'solid 'black)
                                                     (rotate 180 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Complete the visualization" 18 'black))
                                       )
                               (empty-scene 1250 100)))

;; etc (edges-to-track) is a structure that consists of
;; up-edges - unprocessed edges, edges that are yet to be drawn
;; ad-edges - already drawn edges
;; incl-nodes - states that are already drawn on the graph
;; M - a given ndfa
;; hedges - a list of highlighted edges in the ndfa graph
;; fedges - a list of faded edges in the ndfa graph
;; bledges - a list of black/unvisited edges in the ndfa graph

(define-struct etc (up-edges ad-edges incl-nodes M hedges fedges bledges))

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
(define (dfa-node-graph cgraph los finals edge)
  (foldl (λ (state result)  (if  (contains-final-state? state finals)
                                 (add-node
                                  result
                                  (los2symb state)
                                  #:atb (hash 'color (if (equal? state (last los))
                                                         'darkgreen
                                                         'black)
                                              'shape 'doublecircle
                                              'style (cond [(empty? edge)
                                                            'solid]
                                                           [(equal? state (third edge))
                                                            'bold]
                                                           [else 'solid])        
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
                                              'style (cond [(empty? edge)
                                                            'solid]
                                                           [(equal? state (third edge))
                                                            'bold]
                                                           [else 'solid])
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
;; (listof rules) -> graph img
;; Purpose: To create a dfa graph from a given ndfa
(define (create-dfa-graph lor los finals)
  (if (empty? lor)
      (graph->bitmap (dfa-edge-graph (dfa-node-graph (create-graph 'dfagraph #:atb (hash 'rankdir "LR")) los finals '())
                                     lor '()))
      (graph->bitmap (dfa-edge-graph (dfa-node-graph (create-graph 'dfagraph #:atb (hash 'rankdir "LR")) los finals (first lor)) 
                                     lor (first lor)))))

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
;; (listof ndfa-rules) superstate  super-state-edge -> (listof edges)
;; Purpose: To compute a list of edges that needs to be highlighted in the ndfa graph
(define (compute-all-hedges ndfa-rules to-ss added-dfa-edge)
  (if (empty? to-ss)
      empty
      (let [(new-hedges
             (if (empty? added-dfa-edge)
                 empty
                 (filter (λ (rule) (and (eq? (second added-dfa-edge)
                                             (second rule))
                                        (member (first rule)
                                                (first added-dfa-edge))))
                         ndfa-rules)))]
        (append (find-empty-transitions (list (first to-ss))
                                        empty
                                        ndfa-rules)
                new-hedges
                (compute-all-hedges (filter (λ (rule) (not (member rule new-hedges)))
                                            ndfa-rules)
                                    (rest to-ss)
                                    added-dfa-edge)))))


;; remove-edges
;; (listof edges) (listof edges) -> (listof edges)
;; Purpose: To remove edges from a list of edges
(define (remove-edges to-remove removing-from)
  (if (empty? to-remove)
      removing-from
      (if (member (first to-remove) removing-from)
          (remove (first to-remove) removing-from)
          (remove-edges (rest to-remove) removing-from))))

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



;; (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (ndfa-node-graph cgraph M hedges)
  (foldl (λ (state result) (cond [(contains-final-state? state (sm-finals M))
                                  (add-node
                                   result
                                   state
                                   #:atb (hash 'color 'black
                                               'shape 'doublecircle
                                               'style (if (ormap (λ (edge) (equal? state (third edge))) hedges)
                                                          'bold
                                                          'solid)
                                               'label state
                                               'fontcolor 'black))]
                                 [(eq? state (sm-start M))
                                  (add-node
                                   result
                                   state
                                   #:atb (hash 'color 'darkgreen
                                               'shape 'circle
                                               'label state
                                               'style (if (ormap (λ (edge) (equal? (second edge) 'ε)) hedges)
                                                          'bold
                                                          'solid)
                                               'fontcolor 'black))]
                                 [else (add-node
                                        result
                                        state
                                        #:atb (hash 'color 'black
                                                    'shape 'circle
                                                    'label state
                                                    'style (if (ormap (λ (edge) (equal? state (third edge))) hedges)
                                                               'bold
                                                               'solid)
                                                    'fontcolor 'black))]))
         cgraph
         (sm-states M)))


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
;; etc -> graph img
;; Purpose: To make an ndfa-graph from the given ndfa
(define (make-ndfa-graph a-etc)
  (let* [(ndfa-edge-graph-x (ndfa-edge-graph (ndfa-node-graph (create-graph 'ndfagraph #:atb (hash 'rankdir "LR"))
                                                              (etc-M a-etc) (etc-hedges a-etc))
                                             (etc-hedges a-etc)
                                             (etc-fedges a-etc)
                                             (etc-bledges a-etc)))]
    (graph->bitmap ndfa-edge-graph-x)))


;; create-etcs
;; etc (listof etc) -> (listof etc)
;; Purpose: To create all the etcs for graph imgs
(define (create-etcs a-etc low)
  (if (empty? (etc-up-edges a-etc))
      (cons a-etc low)
      (let* [(curr-dfa-ss-edge (first (etc-up-edges a-etc)))
             (new-up-edges (rest (etc-up-edges a-etc)))
             (new-ad-edges (cons curr-dfa-ss-edge
                                 (etc-ad-edges a-etc)))
             (new-incl-nodes (add-included-node (etc-incl-nodes a-etc)
                                                curr-dfa-ss-edge))
             (new-hedges (compute-all-hedges (sm-rules (etc-M a-etc))
                                             (third curr-dfa-ss-edge)
                                             curr-dfa-ss-edge))
             (new-fedges (if (empty? new-up-edges)
                             (append new-hedges
                                     (etc-fedges a-etc))
                             (append (etc-hedges a-etc)
                                     (etc-fedges a-etc))))
             (new-bledges (remove-edges new-hedges (etc-bledges a-etc)))]
        (create-etcs
         (make-etc new-up-edges                       
                   new-ad-edges
                   new-incl-nodes
                   (etc-M a-etc)
                   new-hedges
                   new-fedges
                   new-bledges)
         (cons a-etc low)))))

;; create-all-imgs
;; (listof etc) -> (listof img)
;; Purpose: To create all graph imgs
(define (create-all-imgs low)
  (define (resize-img img)
    (let [(e-scn-w (- (image-width E-SCENE) 10))
          (e-scn-h (- (image-height E-SCENE) 10))
          (w (image-width img))
          (h (image-height img))]
      (cond [(and (> w e-scn-w) (> h (/ e-scn-h 2)))
             (resize-image img e-scn-w (/ e-scn-h 2))]
            [(> w e-scn-w) (resize-image img e-scn-w h)]
            [(> h (/ e-scn-h 2)) (resize-image img w (/ e-scn-h 2))]
            [else img])))
  (if (empty? low)
      '()
      (let* [(ndfa-graph (overlay (resize-img
                                   (beside (text "NDFA    " 24 'darkgreen)
                                           (make-ndfa-graph (first low))))
                                  (empty-scene (image-width E-SCENE)
                                               (/ (image-height E-SCENE) 2))))
             (dfa-graph
              (let* [(new-edge (if (empty? (etc-ad-edges (first low)))
                                   '()
                                   (first (etc-ad-edges (first low)))))
                     (edge-str (cond [(empty? new-edge)
                                      "Starting Super State"]
                                     [(or (empty? (third new-edge))
                                          (empty? (first new-edge)))
                                      (string-append "SS Edge Added: "
                                                     "("
                                                     (symbol->string (if (empty? (first new-edge))
                                                                         DEAD
                                                                         (los->symbol (first new-edge))))
                                                     " "
                                                     (symbol->string (second new-edge))
                                                     " "
                                                     (symbol->string (if (empty? (third new-edge))
                                                                         DEAD
                                                                         (los->symbol (third new-edge))))
                                                     ") - no corresponding ndfa transition.")]                                     
                                     [else (string-append "SS Edge Added: "
                                                          "("
                                                          (symbol->string (los->symbol (first new-edge)))
                                                          " "
                                                          (symbol->string (second new-edge))
                                                          " "
                                                          (symbol->string (los->symbol (third new-edge)))
                                                          ")")]))
                     (edge-msg-img (text edge-str 24 'violet))]          
                (overlay (resize-img
                          (above
                           (create-dfa-graph
                            (etc-ad-edges (first low))
                            (etc-incl-nodes (first low))
                            (ndfa2dfa-finals-only (etc-M (first low))))
                           edge-msg-img))
                         (empty-scene (image-width E-SCENE)
                                      (/ (image-height E-SCENE) 2)))))]
        (cons (above ndfa-graph
                     dfa-graph)
              (create-all-imgs (rest low))))))







;; draw-etc
;; etc -> img
;; Purpose: To draw a etc image
(define (draw-etc a-etc)
  (if (empty? (viz-state-pimgs a-etc))
      (above (first (viz-state-upimgs a-etc)) E-SCENE-TOOLS)
      (above (first (viz-state-pimgs a-etc)) E-SCENE-TOOLS)))


;; contains-final-state-run?
;; symbol (listof symbols)
(define (contains-final-state-run? sss sm-finals)
  (ormap (λ (s) (member s sm-finals)) sss))


;; ndfa2dfa-viz 
;; ndfa -> void
(define (ndfa2dfa-viz M)
  (let* [(ss-edges (compute-ss-edges M))
         (super-start-state (compute-empties (list (sm-start M))
                                             (sm-rules M)
                                             '()))
         (init-hedges (compute-all-hedges (sm-rules M) super-start-state '()))
         (etc (make-etc ss-edges
                        '()
                        (list (first (first ss-edges)))
                        M
                        init-hedges
                        '()
                        (remove init-hedges (sm-rules M))))
         (low (reverse (create-etcs etc '())))
         (imgs (create-all-imgs low))]
    (run-viz (viz-state (rest imgs) (list (first imgs)))
             draw-etc
             'ndfa2dfa-viz)))



(define aa-ab (make-ndfa `(S A B F)
                         '(a b)
                         'S
                         '(A B F)
                         `((S a A)
                           (S a B)
                           (S ,EMP F)
                           (A a A)
                           (B b B))))

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

(ndfa2dfa-viz aa-ab)
;(ndfa2dfa-viz AT-LEAST-ONE-MISSING)
