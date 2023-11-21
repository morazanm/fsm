#lang fsm

(require "../../../fsm-core/interface.rkt" "../lib.rkt" "../../../fsm-gui/graphViz/main.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)
(require "run-viz.rkt")

(define FNAME "fsm")

;; L = ab*
(define nl (make-ndfa '(S)
                      '(a b)
                      'S
                      '()
                      '()))

(define A (make-ndfa '(S A B C D E F)
                     '(a b x)
                     'S
                     '(D E)
                     '((S a A)
                       (S a F)
                       (S b B)
                       (A a C)
                       (B b C)
                       (C b D)
                       (C a E)
                       (C x C)
                       (F a C))))

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

(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C) '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (A a A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

;; (listof ndfa-rule) → dgraph
;; Purpose: Create a dgraph from the given ndfa (for the ones that are already regexp)
(define (make-dgraph-unions lor)
  (map (λ (r) (if (eq? (second r) EMP)
                  (list (first r) (printable-regexp (empty-regexp)) (third r))
                  (list (first r)
                        (second r)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define E-SCENE (empty-scene 1250 600))


;; img is the graph image
;; state is the state being ripped
(struct image-struct (img state))

;; pimgs is a list of processed graph images
;; upimgs is a list of unprocessed graph images
(struct viz-state (pimgs upimgs))

;; create-nodes
;; graph (listof state) state state -> graph
;; Purpose: To add the given states as nodes to the given graph
;;          using the given ns and nf as, respectively, the new
;;          start and final states.
(define (create-nodes graph los ns nf)
  (let [(states-only (append (list ns nf) los))]
    (foldl (λ (state result)
             (add-node
              result
              state
              #:atb (hash 'color (if (eq? state ns)
                                     'darkgreen
                                     'black)
                          'shape (if (eq? state nf)
                                     'doublecircle
                                     'circle)
                          'label (if (equal? state '())
                                     'ds  
                                     state)
                          'fontcolor 'black
                          'font "Sans")))
           graph
           states-only)))                           

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges graph loe)
  (foldl (λ (rule result)
           (add-edge result
                     (printable-regexp (simplify-regexp (second rule)))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         loe))

;; create-edges-special
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges-special graph loe)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         loe))

;; create-graph-img-special
;; (listof state) dgraph state state -> img
;; Purpose: To create a graph img for the given dgraph using
;;          news as the start state and newf as the final state
(define (create-graph-img-special los loe news newf)
  (graph->bitmap
   (create-edges-special
    (create-nodes
     (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
     los
     news
     newf)
    loe)))


;; create-graph-img
;; (listof state) dgraph state state -> img
;; Purpose: To create a graph img for the given dgraph using
;;          news as the start state and newf as the final state
(define (create-graph-img los loe news newf)
  (graph->bitmap
   (create-edges
    (create-nodes
     (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
     los
     news
     newf)
    loe)))


;; create-graph-imgs
;; ndfa -> (listof img)
;; Purpose: To create a list of images of graphs that build a regular
;; expression from the  given ndfa
(define (create-graph-imgs M)
  (define new-start (generate-symbol 'S (sm-states M)))
  (define new-final (generate-symbol 'F (sm-states M)))
  (define new-rules (cons (list new-start EMP (sm-start M))
                          (map (λ (fst) (list fst EMP new-final))
                               (sm-finals M))))
  (define (grp-seq to-rip g gseq)
    (if (null? to-rip)
        (cons (image-struct (create-graph-img
                             (append (list new-start new-final) to-rip)
                             g
                             new-start
                             new-final)
                            '())
              gseq)
        (let [(new-g (rip-out-node (first to-rip) g))]
          (grp-seq (rest to-rip)
                   new-g
                   (cons (image-struct (create-graph-img
                                        (append (list new-start new-final) to-rip)
                                        g
                                        new-start
                                        new-final)
                                       (first to-rip))
                         gseq)))))

  (reverse (grp-seq (if (not (member DEAD (sm-states M)))
                        (sm-states M)
                        (cons DEAD (remove DEAD (sm-states M))))
                    (make-dgraph (append (sm-rules M) new-rules)) '())))

;; add-edges-to-hash
;; hash (listof edges) -> hash
;; Purpose: To add edges to the hash table
(define (add-edges-to-hash hash loe)
  (let* [(current-hash (begin
                         (if (hash-has-key? hash (list (first (first loe))
                                                       (third (first loe))))                             
                             (hash-set! hash (list (first (first loe))
                                                   (third (first loe)))
                                        (append (list (second (first loe)))
                                                (hash-ref hash (list (first (first loe))
                                                                     (third (first loe))))))
                             (hash-set! hash (list (first (first loe))
                                                   (third (first loe)))
                                        (list (second (first loe)))))
                         hash))]
    (if (= (length loe) 1)
        current-hash
        (add-edges-to-hash current-hash (rest loe)))))
               
               


;; make-unions
;; (listof symbol) -> regexp
;; Purpose: To return a union of all symbols in a list
(define (make-unions los)
  (if (= 1 (length los))
      (symbol->string (first los))
      (string-append (symbol->string (first los)) " U " (make-unions (rest los)))))

;; to-union
;; (listof edges) -> (listof edges with regexp labels)
;; Purpose: To turn all loops on multiple edges into unions
(define (to-union loe)
  (let* [(hash-t (add-edges-to-hash (make-hash) loe))
         (hash-l (reverse (hash->list hash-t)))]
    (map (λ (x) (if (< (length (rest x)) 1)
                    (list (first (first x))
                          (string->symbol (first (rest x)))
                          (second (first x)))
                    (list (first (first x))
                          (string->symbol (make-unions (reverse (rest x))))
                          (second (first x)))))
         hash-l)))
                 

;; make-init-graph-img
;; ndfa -> img
;; Purpose: To create the image of the initial ndfa graph
(define (make-init-graph-img M)
  (let* [(new-start (generate-symbol 'S (sm-states M)))
         (new-final (generate-symbol 'F (sm-states M)))
         (new-rules (cons (list new-start EMP (sm-start M))
                          (map (λ (fst) (list fst EMP new-final))
                               (sm-finals M))))
         (changed-rules (to-union (append (sm-rules M) new-rules)))]
    (image-struct (create-graph-img-special
                   (if (not (member DEAD (sm-states M)))
                       (sm-states M)
                       (cons DEAD (remove DEAD (sm-states M))))
                   (make-dgraph-unions changed-rules) 
                   new-start
                   new-final)
                  (if (not (member DEAD (sm-states M)))
                      (sm-states M)
                      DEAD))))

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.

(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (let* [(new-pimgs (cons (first (viz-state-upimgs a-vs))
                                     (viz-state-pimgs a-vs)))
                    (new-upimgs (rest (viz-state-upimgs a-vs)))]           
               (viz-state new-pimgs new-upimgs)))]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (let* [(new-pimgs (rest (viz-state-pimgs a-vs)))
                    (new-upimgs (cons (first (viz-state-pimgs a-vs))
                                      (viz-state-upimgs a-vs)))]
               (viz-state new-pimgs new-upimgs)))]
        [(key=? "down" a-key)
         (let* [(new-pimgs (append (reverse (viz-state-upimgs a-vs))
                                   (viz-state-pimgs a-vs)))
                (new-upimgs '())]
           (viz-state new-pimgs new-upimgs))]
        [(key=? "up" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (let* [(new-pimgs  (list (first (reverse (viz-state-pimgs a-vs)))))
                    (new-upimgs (append (rest (reverse (viz-state-pimgs a-vs))) (viz-state-upimgs a-vs)))]
               (viz-state new-pimgs new-upimgs)))]
        [else a-vs]))


;; draw-img
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (define graph-img (cond [(empty? (viz-state-pimgs a-vs))
                           (above
                            (image-struct-img (first (viz-state-upimgs a-vs)))
                            (text (format "Starting ndfa") 20 'black))]
                          [(= 1 (length (viz-state-pimgs a-vs)))
                           (above
                            (image-struct-img (first (viz-state-pimgs a-vs)))
                            (text (format "Starting ndfa") 20 'black))]
                          [(= 2 (length (viz-state-pimgs a-vs)))
                           (above
                            (image-struct-img (first (viz-state-pimgs a-vs)))
                            (text (format "Added starting and final state") 20 'black))]
                          [(= 3 (length (viz-state-pimgs a-vs)))
                           (above
                            (image-struct-img (first (viz-state-pimgs a-vs)))
                            (text (format "Ripped node: ~s" (first (image-struct-state
                                                                    (first (rest (viz-state-pimgs a-vs))))))
                                  20
                                  'black))]
                          [else
                           (above
                            (image-struct-img (first (viz-state-pimgs a-vs)))
                            (text (format "Ripped node: ~a" (image-struct-state
                                                             (first (rest (viz-state-pimgs a-vs)))))
                                  20
                                  'black))]))
  (let [(width (image-width graph-img))
        (height (image-height graph-img))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (overlay (resize-image graph-img (image-width E-SCENE) (image-height E-SCENE))
                 E-SCENE)
        (overlay graph-img E-SCENE))))



;; ndfa2regexp-viz
;; ndfa --> (void)
(define (ndfa2regexp-viz M)
  (run-viz (viz-state (list (image-struct (sm-graph M) '()))
                      (cons (make-init-graph-img M) (rest (create-graph-imgs M))))
           draw-world
           process-key
           'ndfa2regexp))


(define aa-ab
  (make-ndfa 
   '(S A B F) 
   '(a b) 
   'S
   '(A B)
   `((S a A)
     (S a B)
     (S ,EMP F)
     (A a A)
     (B b B))))

;(run AT-LEAST-ONE-MISSING)




