#lang fsm
#;(require test-engine/racket-tests)
(require "../../fsm-core/interface.rkt" "lib.rkt" "../../fsm-gui/graphViz/main.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)

(define FNAME "fsm")

(define R1 (union-regexp (singleton-regexp "a")
                         (singleton-regexp "b")))

(define R2 (concat-regexp (singleton-regexp "m") R1))

(define R3 (kleenestar-regexp R2))

(define E-SCENE (empty-scene 1250 600))

;; dgraph --> ndfa
;; Purpose: Create ndfa from given dgraph
(define (dgraph2lodgraph dgraph)
  
  ;; digraph --> Boolean
  (define (only-simple-edges? grph)
    (andmap (λ (e) (or (empty-regexp? (second e))
                       (singleton-regexp? (second e))))
            grph))

  ;; dgraph --> dedge
  ;; Assumption: dgraph has a nonsimple edge
  (define (extract-first-nonsimple grph)
    (first (filter (λ (e) (and (not (empty-regexp? (second e)))
                               (not (singleton-regexp? (second e)))))
                   grph)))
  
  ;; dgraph (listof dgraph) --> (listof dgraph)
  (define (bfs grph acc)
    #;(displayln (format "graph: ~a\n" (map (λ (e) (format "(~a ~a ~a)"
                                                           (first e)
                                                           (printable-regexp (second e))
                                                           (third e) ))
                                            grph)))
    (if (only-simple-edges? grph)
        (cons grph acc)
        (let* [(edge (extract-first-nonsimple grph))
               (fromst (first edge))
               (rexp (second edge))
               (tost (third edge))
               #;(dd (displayln (format "graph: ~a\n" (map (λ (e) (format "(~a ~a ~a)"
                                                                          (first e)
                                                                          (printable-regexp (second e))
                                                                          (third e) ))
                                                           grph))))
               #;(d (displayln (format "edge: (~a ~a ~a)\n" fromst (printable-regexp rexp) tost)))]
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
                            (remove edge grph))
                                
                    #;(cons (list fromst (union-regexp-r1 rexp) tost)
                            (cons (list fromst (union-regexp-r2 rexp) tost)
                                  (remove edge grph)))
                    (cons grph acc)))]
                [(concat-regexp? rexp)
                 (let [(istate1 (generate-symbol 'I '(I)))
                       (istate2 (generate-symbol 'I '(I)))]
                   (bfs (append (list (list fromst (concat-regexp-r1 rexp) istate1)
                                      (list istate1 (empty-regexp) istate2)
                                      (list istate2 (concat-regexp-r2 rexp) tost))
                                (remove edge grph))
                        (cons grph acc))                   
                   #;(bfs (cons (list fromst (concat-regexp-r1 rexp) istate)
                                (cons (list istate (concat-regexp-r2 rexp) tost)
                                      (remove edge grph)))
                          (cons grph acc)))]
                [else
                 (let [(istart1 (generate-symbol 'I '(I)))
                       (istart2 (generate-symbol 'I '(I)))]
                   (bfs
                    (append (list (list fromst (empty-regexp) istart1)
                                  (list istart1 (empty-regexp) tost)
                                  (list istart1 (empty-regexp) istart2)
                                  (list istart2 (kleenestar-regexp-r1 rexp) istart2)
                                  (list istart2 (empty-regexp) tost))
                            (remove edge grph))
                    (cons grph acc)))
                 #;(let [(istate (generate-symbol 'I '(I)))]
                     (bfs (cons (list fromst (empty-regexp) istate)
                                (cons (list istate (kleenestar-regexp-r1 rexp) istate)
                                      (cons (list istate (empty-regexp) tost)
                                            (remove edge grph))))
                          (cons grph acc)))]))))
  (bfs dgraph '()))

;; updg are unprocessed dgraphs
;; pdg are processed dgraphs
(struct viz-state (updg pdg))

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-updg a-vs))
             a-vs
             (let* [(new-pdg (cons (first (viz-state-updg a-vs))
                                   (viz-state-pdg a-vs)))
                    (new-updg (rest (viz-state-updg a-vs)))]           
               (viz-state new-updg new-pdg)))]
        [(key=? "left" a-key)
         (if (= 1 (length (viz-state-pdg a-vs)))
             a-vs
             (let* [(new-pdg (rest (viz-state-pdg a-vs)))
                    (new-updg (cons (first (viz-state-pdg a-vs))
                                    (viz-state-updg a-vs)))]
               (viz-state new-updg new-pdg)))]
        [(key=? "down" a-key)
         (let* [(new-pdg (append (reverse (viz-state-updg a-vs))
                                 (viz-state-pdg a-vs)))
                (new-updg '())]
           (viz-state new-updg new-pdg))]           
        [else a-vs]))

;; create-nodes
;; graph (listof edge) -> graph
;; Purpose: To add the nodes to the graph
(define (create-nodes graph dgraph)
  (define (states-only dgraph)
    (remove-duplicates
     (append-map (λ (e) (list (first e) (third e))) dgraph))
    #;(remove-duplicates (if (empty? dgraph)
                           empty
                           (flatten (cons (filter (λ (el) (symbol? el)) dgraph)
                                          (states-only (rest dgraph)))))))                    
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color 'black
                        'shape 'circle
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
                     (printable-regexp (simplify-regexp (second rule)))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         dgraph))


;; create-graph-img
;; graph -> img
;; Purpose: To create a graph img for the given dgraph
(define (create-graph-img dgraph)
  (graph->bitmap
   (create-edges
    (create-nodes
     (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) dgraph)
    dgraph)))

;; create-starting-nodes
;; graph (listof edge) -> graph
;; Purpose: To add the nodes to the graph
(define (create-starting-nodes graph dgraph)
  (let [(states (list (first dgraph)(third dgraph)))]                    
    (foldl (λ (state result)
             (add-node
              result
              state
              #:atb (hash 'color (if (eq? state (first dgraph))
                                     'green
                                     'red)                                        
                          'shape (if (eq? state (first dgraph))
                                     'circle
                                     'doublecircle)
                          'label (if (equal? state '())
                                     'ds  
                                     state)
                          'fontcolor 'black
                          'font "Sans")))
           graph
           states)))


;; create-starting-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-starting-edges graph dgraph)
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
         (list dgraph)))


;; create-starting-graph-img
;; graph -> img
;; Purpose: To make starting regular expression graph
(define (create-starting-graph-img dgraph)
  (graph->bitmap
   (create-starting-edges
    (create-starting-nodes
     (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) dgraph)
    dgraph)))


;; draw-img
;; viz-state -> img
;; Purpose: To render the given vis-state
(define (draw-world a-vs)
  (define graph-img (cond [(= 1 (length (viz-state-pdg a-vs)))
                           (above
                            (create-starting-graph-img (first (viz-state-pdg a-vs)))
                            (text "Starting regexp" 20 'black))]
                          [(empty? (viz-state-updg a-vs))
                           (above
                            (create-graph-img (first (viz-state-pdg a-vs)))
                            (text "Resulting ndfa" 20 'black))]
                          [else
                           (create-graph-img (first (viz-state-pdg a-vs)))]))
                            
  (let [(width (image-width graph-img))
        (height (image-height graph-img))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (overlay (resize-image graph-img (image-width E-SCENE) (image-height E-SCENE))
                 E-SCENE)
        (overlay graph-img E-SCENE))))




;; run-function
(define (run regexp)
  (let [(lodgraph (reverse
                   (dgraph2lodgraph
                    (list (list (generate-symbol 'S '(S))
                                regexp
                                (generate-symbol 'F '(F)))))))]
    (begin
      (big-bang
          (viz-state (rest lodgraph) (first lodgraph))
        [on-draw draw-world]
        [on-key process-key]
        [name "FSM: regexp to ndfa visualization"]))
    (void)))





#| NO LONGER ILLUSTRATIVE
Illustrative tests; can't be implemented because of randomness in generating symbols

(check-equal? (dgraph2lodgraph (list (list 'S R1 'F)))
              (list
               (list (list 'S (singleton-regexp "a") 'F) (list 'S (singleton-regexp "b") 'F))
               (list (list 'S (union-regexp (singleton-regexp "a") (singleton-regexp "b")) 'F))))

(check-equal? (dgraph2lodgraph (list (list 'S R2 'F)))
              (list
               (list
                (list 'I-1648402 (singleton-regexp "a") 'F)
                (list 'I-1648402 (singleton-regexp "b") 'F)
                (list 'S (singleton-regexp "m") 'I-1648402))
               (list
                (list 'S (singleton-regexp "m") 'I-1648402)
                (list 'I-1648402 (union-regexp (singleton-regexp "a") (singleton-regexp "b")) 'F))
               (list
                (list
                 'S
                 (concat-regexp
                  (singleton-regexp "m")
                  (union-regexp (singleton-regexp "a") (singleton-regexp "b")))
                 'F))))

(check-equal? (dgraph2lodgraph (list (list 'S R3 'F)))
              (list
               (list
                (list 'I-1637562 (singleton-regexp "a") 'I-1637561)
                (list 'I-1637562 (singleton-regexp "b") 'I-1637561)
                (list 'I-1637561 (singleton-regexp "m") 'I-1637562)
                (list 'S (empty-regexp) 'I-1637561)
                (list 'I-1637561 (empty-regexp) 'F))
               (list
                (list 'I-1637561 (singleton-regexp "m") 'I-1637562)
                (list 'I-1637562 (union-regexp (singleton-regexp "a") (singleton-regexp "b")) 'I-1637561)
                (list 'S (empty-regexp) 'I-1637561)
                (list 'I-1637561 (empty-regexp) 'F))
               (list
                (list 'S (empty-regexp) 'I-1637561)
                (list
                 'I-1637561
                 (concat-regexp
                  (singleton-regexp "m")
                  (union-regexp (singleton-regexp "a") (singleton-regexp "b")))
                 'I-1637561)
                (list 'I-1637561 (empty-regexp) 'F))
               (list
                (list
                 'S
                 (kleenestar-regexp
                  (concat-regexp
                   (singleton-regexp "m")
                   (union-regexp (singleton-regexp "a") (singleton-regexp "b"))))
                 'F))))
|#

#|

(list
 (list
  (list 'I-245828 (empty-regexp) 'I-245829)
  (list 'I-245828 (empty-regexp) 'I-245830)
  (list 'I-245829 (singleton-regexp "a") 'I-245831)
  (list 'I-245830 (singleton-regexp "b") 'I-245832)
  (list 'I-245831 (empty-regexp) 'F)
  (list 'I-245832 (empty-regexp) 'F)
  (list 'S (singleton-regexp "m") 'I-245827)
  (list 'I-245827 (empty-regexp) 'I-245828)
  (list 'I-245826 (empty-regexp) 'S)
  (list 'F (empty-regexp) 'I-245826))
 (list
  (list 'S (singleton-regexp "m") 'I-245827)
  (list 'I-245827 (empty-regexp) 'I-245828)
  (list
   'I-245828
   (union-regexp (singleton-regexp "a") (singleton-regexp "b"))
   'F)
  (list 'I-245826 (empty-regexp) 'S)
  (list 'F (empty-regexp) 'I-245826))
 (list
  (list 'I-245826 (empty-regexp) 'S)
  (list
   'S
   (concat-regexp
    (singleton-regexp "m")
    (union-regexp (singleton-regexp "a") (singleton-regexp "b")))
   'F)
  (list 'F (empty-regexp) 'I-245826))
 (list
  (list
   'S
   (kleenestar-regexp
    (concat-regexp
     (singleton-regexp "m")
     (union-regexp (singleton-regexp "a") (singleton-regexp "b"))))
   'F)))
|#