#lang fsm
(require "../../fsm-core/interface.rkt" "lib.rkt" "../../fsm-gui/graphViz/main.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)

(define FNAME "fsm")

;; L = nl
(define nl (make-ndfa '(S)
                      '(a b)
                      'S
                      '()
                      '()))

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

;; INTERSECTION VISUALIZATION

;; make-node-union
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-union graph los s f)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state s) 'green]
                                     [else 'black])
                        'shape (if (member state f)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         los))

;; make-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-edge-union graph M N ns)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid)))
         graph
         (append (list (list ns EMP (sm-start M))
                       (list ns EMP (sm-start N)))
                 (sm-rules M)
                 (sm-rules N))))

;; create-union-graph
;; ndfa ndfa -> dgraph
;; Purpose: To create a graph image for the union
;; Assume: The intersection of the states of the given machines is empty
(define (create-union-graph M N)
  (let* [(new-start (generate-symbol 'S (append (sm-states M) (sm-states N))))
         (new-states (cons new-start
                           (append (sm-states M) (sm-states N))))
         (added-edges (list (list new-start EMP (sm-start M))
                            (list new-start EMP (sm-start N))))
         (new-finals (append (sm-finals M) (sm-finals N)))]
    (make-edge-union (make-node-union
                      (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                      new-states new-start new-finals) M N new-start)))
                    



(define E-SCENE (empty-scene 1250 600))

;; upgi are unprocessed graphs
;; pgi are processed graph images
(struct viz-state (upgi pgi))


;; create-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (create-node-graph graph M)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state (sm-start M)) 'darkgreen]
                                     [else 'black])
                        'shape (if (member state (sm-finals M))
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         (sm-states M)))

;; create-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph graph M)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                )))
         graph
         (sm-rules M)))


;; 1. check if it's dfa or ndfa
;; 2. if it's dfa, proceed normally with the function marco has give
;; 3. if it's ndfa, convert to dfa and then proceed
;; 4. complement of M
;; 5. complement of N
;; 6. rename states of M
;; 7. rename states of N
;; 8. union of those two
;; 9. convert that to dfa
;; 10. complement of that dfa

;; create-graph-img
;; ndfa ndfa -> img
;; Purpose: To create a graph image for the intersection
(define (create-graph-imgs M N)
  (define (complement-fsa ndfa)
    (let* [(new-finals (filter (λ (s) (not (member s (sm-finals ndfa)))) (sm-states ndfa)))]
      (make-dfa (sm-states ndfa)
                (sm-sigma ndfa)
                (sm-start ndfa)
                new-finals (sm-rules ndfa) 'no-dead)))
  (let* [(dfaM (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                    (ndfa->dfa M)) (ndfa->dfa M))))
         (dfaN (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                    (ndfa->dfa N)) (ndfa->dfa N))))
         (cmplM (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                     (sm-complement (ndfa->dfa M)))
                                                  (sm-complement (ndfa->dfa M)))))
         (cmplN (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                     (sm-complement (ndfa->dfa N)))
                                                  (sm-complement (ndfa->dfa N)))))
         (notM (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                     (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa M))))
                                  (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa M)))))
         (notN (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                     (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa N))))
                                  (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa N)))))
         (notMimg (graph->bitmap notM))
         (notNimg (graph->bitmap notN))
         #;(unionMN (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                         (create-union-graph notM notN)))
                                   (create-union-graph notM notN)))
         #;(dfaMN (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                       (ndfa->dfa (sm-union notM notN)))
                                                    (ndfa->dfa (sm-union notM notN)))))
         #;(final-graph (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                             (complement-fsa (ndfa->dfa (sm-union notM notN))))
                                                          (complement-fsa (ndfa->dfa (sm-union notM notN))))))]
    (list dfaM dfaN cmplM cmplN notMimg notNimg #;unionMN #;dfaMN #;final-graph)))
    
                    
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M N)
  (overlay (above (text "First ndfa:" 20 'black)
                  (sm-graph M)
                  (text "Second ndfa:" 20 'black)
                  (sm-graph N))
           E-SCENE))


;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upgi a-vs))
             a-vs
             (viz-state (rest (viz-state-upgi a-vs))
                        (cons (first  (viz-state-upgi a-vs))
                              (viz-state-pgi a-vs))))]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pgi a-vs)) 1)
             a-vs
             (viz-state (cons (first (viz-state-pgi a-vs))
                              (viz-state-upgi a-vs))
                        (rest (viz-state-pgi a-vs))))]           
        [else a-vs]))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (overlay
   (first (viz-state-pgi a-vs))
   E-SCENE))

;; run-function
;; run-function
(define (run M N)
  (begin
    (big-bang
        (viz-state (create-graph-imgs M N) (list (make-init-grph-img M N)))
      [on-draw draw-world]
      [on-key process-key]
      [name "FSM: intersection visualization"]))
  (void))