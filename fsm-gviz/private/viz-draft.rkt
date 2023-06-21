#lang racket

(require "../../fsm-core/interface.rkt" "lib.rkt")
(require 2htdp/universe)


;; a world is a structure that consists of
;; cgraph - created graph
;; up-edges - unprocessed edges, edges that are yet to be drawn
;; ad-edges - already drawn edges
;; incl-nodes - nodes that are already drawn on the graph
(define-struct world (cgraph up-edges ad-edges incl-nodes))

;(define INIT-WORLD (make-world (create-graph 'cgraph #:atb (hash 'rankdir "LR"))
                               
                               
                               

 

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
      (reverse (cons 1;(search-for-next-edge lor)
                     (reverse (lor))))))

;; add-included-nodes
;; (listof rules) -> (listof nodes)
;; Purpose: To add the nodes that are included in the current state
;; of the graph
(define (add-included-nodes lon a-world)
  (if (empty? lon)
      lon
      (append lon (list 1)#;(third (search-for-next-edge (world-ad-edges a-world))))))

;; add-up-edge
;; (listof rules) -> (listof rules)
;; Purpose: To add a up-edge to the list of rules
(define (add-up-edge lor)
  (if (empty? lor)
      lor
      (reverse (cons 1;(search-for-next-edge lor)
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
  