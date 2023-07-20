#lang fsm

(require "../../fsm-core/interface.rkt" "lib.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)

(define FNAME "fsm")

;; L = ab*
(define ab* (make-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-ndfa '(Z H B C D F)
                            '(a b)
                            'Z
                            '(F)
                            `((Z a H)
                              (Z a B)
                              (H a D)
                              (D ,EMP F)
                              (B a C)
                              (C b F)
                              (F b F))))
;; L = aab*
(define aab* (make-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))
;; L = a*
(define a* (make-dfa '(S D)
                     '(a b)
                     'S
                     '(S)
                     '((S a S)
                       (S b D)
                       (D a D)
                       (D b D))
                     'no-dead))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; regexp alphabet → ndfa
;; Purpose: Build an ndfa for the given regexp
(define (regexp->ndfa e sigma)
  (let* [(simple-tbl (map
                      (λ (a)
                        (let [(S (generate-symbol 'S '(S)))
                              (A (generate-symbol 'A '(A)))]
                          (list a (make-ndfa (list S A)
                                             sigma
                                             S
                                             (list A)
                                             (list (list S a A))))))
                      (cons EMP sigma)))]
    (cond [(empty-regexp? e) (second (assoc EMP simple-tbl))]
          [(singleton-regexp? e)
           (second (assoc (string->symbol (singleton-regexp-a e))
                          simple-tbl))]
          [(concat-regexp? e)
           (sm-concat (regexp->ndfa (concat-regexp-r1 e) sigma)
                       (regexp->ndfa (concat-regexp-r2 e) sigma))]
          [(union-regexp? e)
           (sm-union (regexp->ndfa (union-regexp-r1 e) sigma)
                      (regexp->ndfa (union-regexp-r2 e) sigma))]
          [else (sm-kleenestar (regexp->ndfa (kleenestar-regexp-r1 e) sigma))])))



;; (listof edge) → regexp
;; Purpose: Collapse the given edges into a regexp
(define (collapse-edges loe) (cond [(empty? loe) '()]
                                   [(empty? (rest loe)) (second (first loe))]
                                   [else (union-regexp (second (first loe))
                                                       (collapse-edges (rest loe)))]))


;; dgraph → dgraph
;; Purpose: Collapse multiple edges between nodes
;; Accumulator Invariant: g = the unprocessed graph
(define (remove-multiple-edges g)
  (if (empty? g) '()
      (let* [(curr-edge (first g))
             (from-state (first curr-edge))
             (to-state (third curr-edge))
             (to-collapse (filter (λ (e) (and (eq? (first e) from-state)
                                              (eq? (third e) to-state)))
                                  g))
             (remaining-g (filter (λ (e) (not (member e to-collapse))) g))]
        (cons (list from-state (collapse-edges to-collapse) to-state)
              (remove-multiple-edges remaining-g)))))

;; node dgraph → dgraph
;; Purpose: Rip out given state from given graph
(define (rip-out-node n g)
  #;(define d (displayln (format "removing: ~s\n from: ~s\n" s g)))
  (let* [(non (filter (λ (r) (and (not (eq? (third r) n))
                                  (not (eq? (first r) n))))
                      g))
         (into-n (filter (λ (r) (and (eq? (third r) n)
                                     (not (eq? (first r) n))))
                         g))
         (outof-n (filter (λ (r) (and (eq? (first r) n)
                                      (not (eq? (third r) n))))
                          g))
         (self-edges (filter (λ (r) (and (eq? (first r) n)
                                         (eq? (third r) n)))
                             g))]
    (remove-multiple-edges
     (append non
             (if (not (empty? self-edges))
                 (let [(self-edge (first self-edges))]
                   (append-map
                    (λ (into-edge)
                      (map (λ (outof-edge)
                             (list (first into-edge)
                                   (concat-regexp
                                    (second into-edge)
                                    (concat-regexp
                                     (kleenestar-regexp (second self-edge))
                                     (second outof-edge)))
                                   (third outof-edge)))
                           outof-n))
                    into-n))
                 (append-map (λ (into-edge)
                               (map (λ (outof-edge)
                                      (list (first into-edge)
                                            (concat-regexp (second into-edge)
                                                           (second outof-edge))
                                            (third outof-edge)))
                                    outof-n))
                             into-n))))))

;; (listof node) dgraph → dgraph
;; Purpose: Rip out the given nodes from the given graph
;; Assume: Given nodes in given graph and g has no multiple edges
;;         between nodes
(define (rip-out-nodes lon g)
  (foldr (λ (s g) (rip-out-node s g)) g lon))


;; (listof ndfa-rule) → dgraph
;; Purpose: Create a dgraph from the given ndfa
(define (make-dgraph lor)
  (map (λ (r) (if (eq? (second r) EMP)
                  (list (first r) (empty-regexp) (third r))
                  (list (first r)
                        (singleton-regexp (symbol->string (second r)))
                        (third r))))
       lor))



;; ndfa → regexp
;; Purpose: Create a regexp from the given ndfa
;; Assume: The transition diagram of the given machine is a connected
;;         directed graph
(define (ndfa2regexp m)
  (let* [(new-start (generate-symbol 'S (sm-states m)))
         (new-final (generate-symbol 'F (sm-states m)))
         (init-dgraph (make-dgraph
                       (cons (list new-start EMP (sm-start m))
                             (append (map (λ (f) (list f EMP new-final))
                                          (sm-finals m))
                                     (sm-rules m)))))
         (collapsed-dgraph
          (rip-out-nodes (sm-states m) (remove-multiple-edges init-dgraph)))]
    (if (empty? collapsed-dgraph)
        (null-regexp)
        (simplify-regexp (second (first collapsed-dgraph))))))


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

(define A (singleton-regexp "a"))
(define G (singleton-regexp "g"))
(define C (singleton-regexp "c"))
(define T (singleton-regexp "t"))

(define UNION-AGCT (union-regexp (union-regexp (union-regexp A G) C) T))

(define AGCT (kleenestar-regexp UNION-AGCT))

(define AGCT-regexp (regexp->ndfa AGCT '(a g c t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define E-SCENE (empty-scene 1250 600))


;; pimgs is a list of processed graph images
;; upimgs is a list of unprocessed graph images
(struct viz-state (pimgs upimgs))

;; create-nodes
;; graph (listof state) -> graph
;; Purpose: To create a graph of nodes
(define (create-nodes graph los ns nf)
  (let [(states-only (append (list ns nf) los))]
    (foldl (λ (state result)
             (add-node
              result
              state
              #:atb (hash 'color (cond [(eq? state ns) 'green]
                                       [(eq? state nf) 'red]
                                       [else 'black])
                          'shape (if (eq? state nf)
                                     'doublecircle
                                     'circle)
                          'label (if (equal? state '())
                                     'ds  
                                     state)
                          'fontcolor 'black)))
           graph
           states-only)))


;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges graph loe)
  (foldl (λ (rule result)
           ;(let [(ddd (displayln (format "label: ~s\nsimplified: ~s\n" (second rule) (printable-regexp (simplify-regexp (second rule))))))]
           (add-edge result
                     (printable-regexp (simplify-regexp (second rule)))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 20
                                 'style 'solid)))
         graph
         loe))


;; create-graph-img
;; (listof state) dgraph state state -> img
;; Purpose: To create a graph img 
(define (create-graph-img los loe news newf)
  (graph->bitmap
   (create-edges
    (create-nodes (create-graph 'dgraph #:atb (hash 'rankdir "LR")) los news newf)
    loe)
   (current-directory)
   "fsm"))


;; create-graph-imgs
;; ndfa -> listof imgs
;; Purpose: To create a list of graph images
(define (create-graph-imgs M)
  (define new-start (generate-symbol 'S (sm-states M)))
  (define new-final (generate-symbol 'F (sm-states M)))
  (define new-rules (cons (list new-start EMP (sm-start M))
                          (map (λ (fst) (list fst EMP new-final))
                               (sm-finals M))))
  (define (grp-seq to-rip g gseq)
    (if (null? to-rip)
        (cons (create-graph-img (append (list new-start new-final) to-rip)
                                g
                                new-start
                                new-final)
              gseq)
        (let [(new-g (rip-out-node (first to-rip) g))]
          (grp-seq (rest to-rip)
                   new-g
                   (cons (create-graph-img
                          (append (list new-start new-final) to-rip)
                          g
                          new-start
                          new-final) gseq)))))

  (reverse (grp-seq (sm-states M) (make-dgraph (append (sm-rules M) new-rules)) '())))

(define (make-init-graph-img M)
  (let* [(new-start (generate-symbol 'S (sm-states M)))
         (new-final (generate-symbol 'F (sm-states M)))
         (new-rules (cons (list new-start EMP (sm-start M))
                          (map (λ (fst) (list fst EMP new-final))
                               (sm-finals M))))]
    (create-graph-img
     (sm-states M)
     (make-dgraph (append (sm-rules M) new-rules))
     new-start
     new-final)))


;; process-key
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (let* [(new-pimgs (cons (first (viz-state-upimgs a-vs))
                                     (viz-state-pimgs a-vs)))
                    (new-upimgs (rest (viz-state-upimgs a-vs)))]
           
               (viz-state new-pimgs new-upimgs)))]
        [(key=? "left" a-key)
         (if (empty? (viz-state-pimgs a-vs))
             a-vs
             (let* [(new-pimgs (rest (viz-state-pimgs a-vs)))
                    (new-upimgs (cons (first (viz-state-pimgs a-vs))
                                      (viz-state-upimgs a-vs)))]
               (viz-state new-pimgs new-upimgs)))
         ]
        [(key=? "down" a-key)
         (let* [(new-pimgs (append (reverse (viz-state-upimgs a-vs))
                                   (viz-state-pimgs a-vs)))
                (new-upimgs '())]
           (viz-state new-pimgs new-upimgs))
         ]           
        [else a-vs]))


;; draw-img
;; struct -> img
;; Purpose: To draw a graph img
(define (draw-world a-vs)
  (if (empty? (viz-state-pimgs a-vs))
      (overlay
       (first (viz-state-upimgs a-vs)) 
       E-SCENE)
      (overlay
       (first (viz-state-pimgs a-vs)) 
       E-SCENE)))


;; run-function
(define (run M)
  (begin
    (big-bang
        (viz-state (list (make-init-graph-img M))
                   (rest (create-graph-imgs M))) 
      [on-draw draw-world]
      [on-key process-key]
      [name 'visualization]))
  (void))

(run AT-LEAST-ONE-MISSING)


(define AT-LEAST-ONE-MISSING-init-graph-img (make-init-graph-img AT-LEAST-ONE-MISSING))

(define AT-LEAST-ONE-MISSING-seq (create-graph-imgs AT-LEAST-ONE-MISSING))
