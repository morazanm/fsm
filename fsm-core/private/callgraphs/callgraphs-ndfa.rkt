#lang racket/base
(require "../../../fsm-gviz/private/lib.rkt"
         "../sm-getters.rkt"
         "../misc.rkt"
         "../constants.rkt"
         "cg-defs.rkt"
         "mk-cg-edges-function.rkt"
         "../sm-apply.rkt"
         racket/list)
(provide make-ndfa-cg-edges dot-nodes-fsa dot-trans-fsa computation-diagram-fsa)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-ndfa-cg-edges 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine word -> (listof ndfa-Edge)
;; Purpose: Given a machine and a word, returns a list of edges
;;          including those that lead to a dead state
(define (make-ndfa-cg-edges M word)
  (let* [(my-ds (generate-symbol 'ds (sm-states M)))]
    ;; (listof ndfa-Edge) (listof ndfa-stuci) (listof ndfa-stuci) -> (listof ndfa-stuci)
    ;; Purpose: Given edges and a list of ndfa-stucis, will create
    ;;          a new list of ndfa-stucis that contains the reached
    ;;          states and their respective unconsumed inputs
    ;; Accumulator Invariants:
    ;;  visited = list of all ndfa-stucis explored
    (define (add-to-stucis edges to-visit visited)
      ;; ndfa-stuci -> (listof ndfa-stuci)
      ;; Purpose: Given ndfa-edges and a single ndfa-stuci, creates all
      ;;          new ndfa-stucis associated with the transitions
      ;;          following the given ndfa-stuci
      (define (add-to-stucis-helper stuci)
        (let* [(stuci-empty-ui
                (lambda (r) (ndfa-stuci (ndfa-Edge-tost r)
                                        (ndfa-stuci-ui stuci))))
               (stuci-any-ui
                (lambda (r) (ndfa-stuci (ndfa-Edge-tost r)
                                        (if (eq? (ndfa-Edge-read r)
                                                 (car  (ndfa-stuci-ui stuci)))
                                            (cdr  (ndfa-stuci-ui stuci))
                                            (ndfa-stuci-ui stuci)))))              
               (empty-rules
                (lambda (e) (and (eq? (ndfa-Edge-read e) EMP)
                                 (eq? (ndfa-Edge-fromst e)
                                      (ndfa-stuci-state stuci)))))
               (empty-or-nonempty-rules
                (lambda (e) (and (or (eq? (ndfa-Edge-read e)
                                          (car  (ndfa-stuci-ui stuci)))
                                     (eq? (ndfa-Edge-read e) EMP))
                                 (eq? (ndfa-Edge-fromst e)
                                      (ndfa-stuci-state stuci)))))] 
          (if (null? (ndfa-stuci-ui stuci))
              (map stuci-empty-ui
                   (filter empty-rules
                           edges))
              (map stuci-any-ui
                   (filter empty-or-nonempty-rules
                           edges))))) 
      (if (null? to-visit)
          '()
          (let* [(new-stucis (add-to-stucis-helper (car  to-visit)))]
            (if (null? new-stucis)
                (add-to-stucis edges (cdr  to-visit) visited)
                (append (filter-non-member new-stucis visited)
                        (add-to-stucis edges (cdr  to-visit)
                                       (append new-stucis
                                               visited)))))))

    ;; (listof stuci) (listof stuci) -> (listof stuci)
    ;; Purpose: Filter out stucis already visited
    (define (filter-non-member s v)
      (cond ((null? s) '())
            ((ormap (lambda (x) (ndfa-stucis-equal? (car s) x)) v)
             (filter-non-member (cdr s) v))
            (else (cons (car s) (filter-non-member (cdr s) v)))))
    ;; ndfa-stuci -> (listof ndfa-Edge)
    ;; Purpose: Given an ndfa-stuci, creates all possible edges for that
    ;;          ndfa-stuci
    (define (add-to-edges stuci)
      (let* [(empty-rules
              (lambda (r) (and (eq? EMP (ndfa-rule-read r))
                               (eq? (ndfa-stuci-state stuci) (ndfa-rule-fromst r)))))
             (nonempty-rules
              (lambda (r) (and (eq? (ndfa-stuci-state stuci) (ndfa-rule-fromst r))
                               (eq? (car  (ndfa-stuci-ui stuci)) (ndfa-rule-read r)))))
             (empty-or-nonempty-rules
              (lambda (r) (or (and (eq? (ndfa-stuci-state stuci) (ndfa-rule-fromst r))
                                   (eq? (car  (ndfa-stuci-ui stuci)) (ndfa-rule-read r)))
                              (and (eq? (ndfa-stuci-state stuci) (ndfa-rule-fromst r))
                                   (eq? EMP (ndfa-rule-read r))))))
             (empty-trans?
              (lambda (e) (eq? EMP e)))]
        (if (null? (ndfa-stuci-ui stuci))
            (map (λ (r)
                   (ndfa-spedge (ndfa-rule-fromst r)
                                (ndfa-rule-read r)
                                (ndfa-rule-tost r)))
                 (filter empty-rules
                         (sm-rules M)))
            (let* [(new-rules
                    (if (= 1 (length (ndfa-stuci-ui stuci)))
                        (append (map (λ (r)
                                       (ndfa-spedge (ndfa-rule-fromst r)
                                                    (ndfa-rule-read r)
                                                    (ndfa-rule-tost r)))
                                     (filter nonempty-rules
                                             (sm-rules M)))
                                (map (λ (r)
                                       (ndfa-edge (ndfa-rule-fromst r)
                                                  (ndfa-rule-read r)
                                                  (ndfa-rule-tost r))) 
                                     (filter empty-rules
                                             (sm-rules M))))
                        (map (λ (r)
                               (ndfa-edge (ndfa-rule-fromst r)
                                          (ndfa-rule-read r)
                                          (ndfa-rule-tost r))) 
                             (filter empty-or-nonempty-rules                     
                                     (sm-rules M)))))]
              (cond [(null? new-rules)
                     (list (ndfa-spedge (ndfa-stuci-state stuci)
                                        (car  (ndfa-stuci-ui stuci))
                                        my-ds))]
                    [(andmap empty-trans? (map ndfa-Edge-read new-rules))
                     (cons (ndfa-spedge (ndfa-stuci-state stuci)
                                        (car  (ndfa-stuci-ui stuci))
                                        my-ds)
                           new-rules)]
                    [else new-rules])))))
    ;; (listof ndfa-stuci) (listof ndfa-stuci) -> (listof ndfa-Edge)
    ;; Purpose: Return an exhaustive list of edges and 
    ;;          spedges by traversing the computation tree
    ;;          using BFS
    ;; Accumulator Invariants:
    ;;  to-visit = list of unexplored stucis 
    ;;  visited  = list of explored stucis
    (define (ndfa-computation-tree->cg-edges to-visit visited)
      (let* [(new-edges
              (remove-duplicates
               (append-map (lambda (s) (add-to-edges s)) 
                           to-visit)))
             (new-stucis 
              (add-to-stucis new-edges
                             to-visit visited))]
        (if (null? new-stucis)
            new-edges
            (append new-edges
                    (ndfa-computation-tree->cg-edges 
                     new-stucis 
                     (append to-visit visited))))))
    ;; Termination Argument:
    ;;  Given a start ndfa-stuci, the function will generate the edges associated with
    ;;  that ndfa-stuci, and then the new ndfa-stucis associated with the new edges, and so on.
    ;;  The functions add-to-edges and add-to-stucis are mutually recursive. However,
    ;;  at some point the unconsumed input will be empty, and all possible edges will have
    ;;  been found. ndfa-stucis cannot be examined repeatedly, which avoids an infinite loop. Once there are no
    ;;  edges left to create new ndfa-stucis, the function appends these last set of edges
    ;;  one more time, and halts.
    ;; (listof ndfa-Edges) -> (listof ndfa-Edges)
    ;; Purpose: Given a list of edges removes duplicates. If there is an ndfa-edge that is also
    ;;          an ndfa-spedge, it removes said ndfa-edge.
    (define (ndfa-remove-duplicate-Edges edges)
      (let* [(only-ndfa-edges (filter ndfa-edge? edges))
             (only-ndfa-spedges (filter ndfa-spedge? edges))
             (new-ndfa-edges
              (filter
               (lambda (r) (not (ormap (lambda (r2) (ndfa-Edges-equal? r r2))
                                       only-ndfa-spedges)))
               only-ndfa-edges))]
        (append
         new-ndfa-edges
         only-ndfa-spedges))) 
    ;; (listof ndfa-Edge) -> (listof ndfa-Edge)
    ;; Purpose: Given the finished list of edges, extracts the unnecessary transitions
    ;;          if the machine accepts the word. Only the edges are left that are
    ;;          essential for the word to get accepted.
    (define (ndfa-remove-redundant-edges-on-accept edges)
      ;; (listof sm-showtransition) -> (listof ndfa-Edge)
      ;; Purpose: Given a list of sm-showtransition, finds all needed edges. The last transition becomes an ndfa-spedge.
      (define (make-showtrans-rules showtrans)  
        (cond [(= (length showtrans) 1) '()]
              [else
               (let* [(fromst (cadr (car  showtrans)))
                      (tost (cadr (cadr showtrans)))
                      (read-inpt (cond [(null? (car  (car  showtrans))) EMP]
                                       [(equal? (car  (car  showtrans)) (car  (cadr showtrans))) EMP]
                                       [else (car  (car  (car  showtrans)))]))]
                 (if (= (length showtrans) 2)
                     (cons (ndfa-spedge fromst read-inpt tost)
                           (make-showtrans-rules (cdr  showtrans)))
                     (cons (ndfa-edge fromst read-inpt tost)
                           (make-showtrans-rules (cdr  showtrans)))))]))
      (if (equal? (sm-apply M word) 'reject)
          edges  
          (let* [(showtrans (sm-showtransitions M word))
                 (showtrans-drop-type (take showtrans (- (length showtrans) 1)))
                 (valid-states (map cadr showtrans-drop-type))]
            (remove-duplicates
             (map (lambda (r) (if (and (ndfa-spedge? r)
                                       (not (eq? (ndfa-Edge-tost r) (last valid-states))))
                                  (ndfa-edge (ndfa-Edge-fromst r)
                                             (ndfa-Edge-read r)
                                             (ndfa-Edge-tost r))
                                  r))
                  (filter
                   (lambda (r) (ormap (lambda (r2) (ndfa-Edges-equal? r r2))
                                      (make-showtrans-rules showtrans-drop-type)))
                   edges))))))  
    (mk-cg-edges-function
     ndfa-computation-tree->cg-edges
     (ndfa-stuci (sm-start M) word)
     ndfa-remove-redundant-edges-on-accept
     ndfa-remove-duplicate-Edges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M (listof ndfa-Edge) integer -> (listof node)
;; Purpose: Given a machine and a list of edges, creates a list of
;;          nodes in dot format. The integer determines the color setting. 
;; Color blindness:
;;  0 - default colors
;;  1 - Deuteranopia
(define (dot-nodes-fsa M new-rules cb)
  (let* [(start-state (sm-start M))
         (edge-states (remove-duplicates
                       (append (map (lambda (r) (ndfa-Edge-fromst r))
                                    new-rules)
                               (map (lambda (r)  (ndfa-Edge-tost r))
                                    new-rules))))
         (all-states
          (if (null? edge-states)
              (list (sm-start M))
              edge-states))
         (end-states
          (remove-duplicates
           (map ndfa-spedge-tost
                (filter ndfa-spedge? new-rules))))
         (final-states (sm-finals M))]
    ;; (listof state) -> (listof node)
    ;; Purpose: Given a list of all states in a machine after a word
    ;;          has been consumed, returns a list of nodes 
    (define (new-node los)
      (if (null? los)
          '()
          (let* [(a-color
                  (cond [(= 1 (length all-states)) "crimson"]
                        [(member (car  los) end-states) "crimson"]
                        [(and (or (null? cb)
                                  (eq? 'default (car  cb)))
                              (eq? (ndfa-rule-fromst los) start-state)) "forestgreen"]
                        [(and (not (null? cb))
                              (eq? 'deut (car  cb))
                              (eq? (ndfa-rule-fromst los) start-state)) "dodgerblue"]
                        [else "black"]))
                 (a-shape
                  (cond [(member (ndfa-rule-fromst los) final-states) "doublecircle"]
                        [else "circle"]))
                 (a-label
                  (symbol->string (ndfa-rule-fromst los)))
                 (a-fontcolor
                  (cond [(or (and (eq? (ndfa-rule-fromst los) start-state)
                                  (member (ndfa-rule-fromst los) end-states))
                             (= 1 (length all-states)))
                         (cond [(or (null? cb)
                                    (eq? 'default (car  cb))) "forestgreen"]
                               [(eq? 'deut (car  cb)) "dodgerblue"])]
                        [else "black"]))]
            (cons (list (ndfa-rule-fromst los) `((color ,a-color) (shape ,a-shape) (label ,a-label) (fontcolor ,a-fontcolor))) 
                  (new-node (cdr  los))))))
    (new-node all-states)))

;.................................................

;; (listof ndfa-Edge) -> (listof trans)
;; Purpose: Given a machine's edges, creates a list of
;;          transitions in dot format
(define (dot-trans-fsa new-rules)
  ;; (listof ndfa-Edge) -> (listof trans)
  ;; Purpose: Given a list of edges, creates
  ;;          a list of transitions
  (define (all-trans loes)
    ;; (listof component) -> (listof trans)
    ;; Purpose: Given a list of components within an edge or
    ;;          spedge, returns a transition
    (define (new-trans from r to)      
      (list from to `((fontsize 15) (label ,(symbol->string r)))))
    (if (null? loes)
        '()
        (cons (new-trans (ndfa-Edge-fromst (car  loes))
                         (ndfa-Edge-read (car loes))
                         (ndfa-Edge-tost (car  loes)))
              (all-trans (cdr  loes)))))
  (all-trans new-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .dot files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computation-diagram-fsa

;; fsm word [integer] -> image
;; Purpose: Given a machine and a word, creates a .png file from
;;          a .dot file, and returns a bitmap.
;; Color blindness:
;;  0 - default colors
;;  1 - Deuteranopia
(define (computation-diagram-fsa M word . cb)
  (define fname "fsm")
  ;; image
  ;; Purpose: Stores a graph image 
  (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'label (format "Machine ~as: '~a" (sm-apply M word) 
                                                                                word)
                                                   'fontsize 13)))   
  (let* [(new-rules (make-ndfa-cg-edges M word))]
    (begin
      (set! cgraph
            (foldl
             (lambda (a-node a-graph)
               (let* [(state (car  a-node))
                      (color (cadr (car  (cadr a-node))))
                      (shape (cadr (cadr (cadr a-node))))
                      (label (cadr (caddr (cadr a-node)))) 
                      (fontcolor (cadr (cadddr (cadr a-node))))]
                 (add-node a-graph state #:atb (hash 'color color 'shape shape 'fontcolor fontcolor)))) 
             cgraph   
             (dot-nodes-fsa M new-rules cb)))
      (set! cgraph
            (foldl
             (lambda (a-trans a-graph)
               (let* [(state1 (car  a-trans))
                      (state2 (cadr a-trans))
                      (fontsize (cadr (car  (caddr a-trans))))
                      (label (string->symbol (cadr (cadr (caddr a-trans)))))
                      (style (if (member state2 (sm-states M))
                                 "solid"
                                 "dashed"))] 
                 (add-edge a-graph label state1 state2 #:atb (hash 'fontsize fontsize 'style style))))
             cgraph
             (dot-trans-fsa new-rules)))
      (let [(res (graph->bitmap cgraph))]
        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




             
