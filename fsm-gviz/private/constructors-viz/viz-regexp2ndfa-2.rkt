#lang fsm
(require "../lib.rkt" 
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "definitions-viz-2.rkt"
         "run-viz-2.rkt")
(provide regexp2ndfa-viz)

(define FNAME "fsm")

(define R0 (union-regexp (singleton-regexp "a")
                         (null-regexp)))

(define R1 (union-regexp (singleton-regexp "a")
                         (singleton-regexp "b")))

(define R2 (concat-regexp (singleton-regexp "m") R1))

(define R3 (kleenestar-regexp R2))

(define R4 (kleenestar-regexp (union-regexp R3 R2)))

(define R5 (concat-regexp (singleton-regexp "m") R0))

(define R6 (kleenestar-regexp R5))

(define R7 (union-regexp R0 R5))

(define E-SCENE (empty-scene 1250 600))

;; grph is a (listof img) of graphs used to build an ndfa from regexp
;; edge is an edge that has been expanded to build an ndfa from regexp
(struct gedge (grph edge))

;; dgraph2logedges
;; dgraph edge --> (listof gedges)
;; Purpose: Create a list of digraph-edge structures
(define (dgraph2logedges dgraph edge . issimp?)
  
  ;; digraph --> Boolean
  (define (only-simple-edges? grph)
    (andmap (λ (e) (or (empty-regexp? (second e))
                       (singleton-regexp? (second e))
                       (null-regexp? (second e))))
            grph))

  ;; dgraph --> gedge
  ;; Purpose: To extract the first nonsimple edge in the dgraph
  ;; Assumption: dgraph has a nonsimple edge
  (define (extract-first-nonsimple grph)
    (first (filter (λ (e) (and (not (empty-regexp? (second e)))
                               (not (singleton-regexp? (second e)))))
                   grph)))
  
  ;; dgraph edge (listof dgraph) --> (listof dgraph)
  ;; Purpose: To create a list of dgraphs with  the expanded edge removed
  ;; and replaced with the appropriate edges
  (define (bfs grph edge acc)
    (cond [(only-simple-edges? grph) (cons (gedge grph edge) acc)]
          [(and (not (null? issimp?))
                (first issimp?))
           (cons (gedge grph (void)) acc)]
          [else 
           (let* [(next-edge (extract-first-nonsimple grph))
                  (fromst (first next-edge))
                  (rexp (second next-edge))
                  (tost (third next-edge))]
             (cond [(union-regexp? rexp)
                    (let [(newi1 (generate-symbol 'I '(I)))
                          (newi2 (generate-symbol 'I '(I)))
                          (newi3 (generate-symbol 'I '(I)))
                          (newi4 (generate-symbol 'I '(I)))]
                      (bfs
                       (append (list (list fromst (empty-regexp) newi1)
                                     (list fromst (empty-regexp) newi2)
                                     (list newi1 (union-regexp-r1 rexp) newi3)
                                     (list newi2 (union-regexp-r2 rexp) newi4)
                                     (list newi3 (empty-regexp) tost)
                                     (list newi4 (empty-regexp) tost))
                               (remove next-edge grph))
                       next-edge
                       (cons (gedge grph edge) acc)))]
                   [(concat-regexp? rexp)
                    (let [(istate1 (generate-symbol 'I '(I)))
                          (istate2 (generate-symbol 'I '(I)))]
                      (bfs (append (list (list fromst (concat-regexp-r1 rexp) istate1)
                                         (list istate1 (empty-regexp) istate2)
                                         (list istate2 (concat-regexp-r2 rexp) tost))
                                   (remove next-edge grph))
                           next-edge
                           (cons (gedge grph edge) acc)))]
                   [else
                    (let [(istart1 (generate-symbol 'I '(I)))
                          (istart2 (generate-symbol 'I '(I)))]
                      (bfs
                       (append (list (list fromst (empty-regexp) istart1)
                                     (list istart1 (empty-regexp) tost)
                                     (list istart1 (empty-regexp) istart2)
                                     (list istart2 (kleenestar-regexp-r1 rexp) istart2)
                                     (list istart2 (empty-regexp) tost))
                               (remove next-edge grph))
                       next-edge
                       (cons (gedge grph edge) acc)))]))]))
  (bfs dgraph edge '()))


;; create-nodes
;; graph dgraph edge -> graph
;; Purpose: To add the nodes to the graph
(define (create-nodes graph dgraph edge)
  (define (states-only dgraph)
    (remove-duplicates
     (append-map (λ (e) (list (first e) (third e))) dgraph)))
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state 'S)
                                      (if (and (not (empty? edge))
                                               (not (void? edge))
                                               (eq? (first edge) 'S))
                                          'violet
                                          'darkgreen)]
                                     [(eq? state 'F)
                                      (if (and (not (empty? edge))
                                               (not (void? edge))
                                               (eq? (third edge) 'F))
                                          'violet
                                          'black)]
                                     [(or (eq? state (first edge))
                                          (eq? state (third edge)))
                                      'violet]
                                     [else 'black])                                   
                        'shape (if (eq? state 'F)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         (states-only dgraph)))                         

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges graph dgraph)
  (foldl (λ (rule result)
           (add-edge result
                     (printable-regexp (second rule))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         dgraph))


;; create-graph-img
;; graph edge -> img
;; Purpose: To create a graph img for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graph-img dgraph edge)
  (above
   (graph->bitmap
    (create-edges
     (create-nodes
      (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                        'font "Sans"))
      dgraph
      edge)
     dgraph))
   (cond [(empty? edge) (text "Starting NDFA" 24 'black)]
         [(void? edge) (text "Simplified initial regexp" 24 'black)]
         [else (beside (text (format "Expanded regexp: ~a on edge from state" (printable-regexp (second edge))) 24 'black)
                       (text (format " ~a" (first edge)) 24 'violet)
                       (text (format " to state ") 24 'black)
                       (text (format "~a" (third edge)) 24 'violet))])))


;; create-graph-imgs
;; (listof gedges) -> (listof image)
;; Purpose: To create a list of graph images
(define (create-graph-imgs gedges)
  (if (empty? gedges)
      empty
      (cons (create-graph-img (gedge-grph (first gedges))
                              (gedge-edge (first gedges)))
            (create-graph-imgs (rest gedges)))))


;; create-init-nodes
;; graph dgraph -> graph
;; Purpose: To add the nodes to the graph
(define (create-init-nodes graph)
  (add-node
   (add-node
    graph
    'F
    #:atb (hash 'color 'black                                   
                'shape 'doublecircle
                'fontcolor 'black
                'font "Sans"))
   'S
   #:atb (hash 'color 'darkgreen                                   
               'shape 'circle
               'fontcolor 'black
               'font "Sans")))

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-init-edges graph dgraph)
  (add-edge graph
            (printable-regexp (second dgraph))
            (first dgraph)
            (third dgraph)
            #:atb (hash 'fontsize 14
                        'style 'solid
                        'fontname "Sans"
                        )))



;; create-init-graph
;; list -> img
;; Purpose: To create an initial graph image
(define (create-init-graph a-list)
  (above
   (graph->bitmap
    (create-init-edges
     (create-init-nodes
      (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                        'font "Sans")))
     a-list))
   (text "Starting NDFA" 24 'black)))



;; draw-img
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pimgs a-vs))))
        (height (image-height (first (viz-state-pimgs a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (overlay (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE))
                 E-SCENE)
        (overlay (first (viz-state-pimgs a-vs)) E-SCENE))))

;; regexp2ndfa-viz
;; regexp -> void
(define (regexp2ndfa-viz regexp)
  (let* [(logedges (append
                    (list
                     #;(last (dgraph2logedges
                              (list (list 'S regexp 'F)) 
                              '()))
                     (last (dgraph2logedges
                            (list (list 'S (simplify-regexp regexp) 'F)) 
                            '()
                            #t)))
                    (rest (reverse (dgraph2logedges
                                    (list (list 'S (simplify-regexp regexp) 'F))
                                    '())))))
         (loimgs (create-graph-imgs logedges))]
    (run-viz (viz-state loimgs (list (create-init-graph (list 'S regexp 'F)))) draw-world 'regexp2ndfa)))



