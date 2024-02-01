#lang racket
(require "../fsm-gviz/private/lib.rkt" 
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../fsm-core/interface.rkt")

(define FNAME "fsm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define even-bs-odd-as (make-rg '(S A B C)
                                '(a b)
                                `((S ,ARROW aA)
                                  (S ,ARROW bB)
                                  (S ,ARROW a)
                                  (A ,ARROW aS)
                                  (A ,ARROW bC)
                                  (B ,ARROW aC)
                                  (B ,ARROW bS)
                                  (C ,ARROW aB)
                                  (C ,ARROW bA)
                                  (C ,ARROW b))
                                'S))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define E-SCENE (empty-scene 600 800))

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



;; viz-state is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
(struct viz-state (upimgs pimgs))


;; create-edges
;; (listof level) <=> (listof (listof edge))
;; (listof symbol) -> (listof (listof edge))
;; Purpose: To create edges of the graph and group them by levels
(define (create-edges wd)
  (cond [(empty? wd)
         '()]
        [(= 1 (length wd))
         (list (list (list (last (first wd))
                           null)))]
        [(= 2 (length wd))
         (list (list (list (last (first wd))
                           (last (second wd)))))]
        [else (append (list (map (λ (x) (list (last (first wd)) x)) (take-right (second wd) 2)))
                      (create-edges (rest wd)))]))

;; rename-edges
;; (listof level) -> (listof level)
;; Purpose: To rename the nonterminals that reoccur in extracted edges
(define (rename-edges exe)
  (define (rnm-lvl lvl acc)
    (cond [(empty? lvl)
           '()]
          [(= 1 (length lvl))
           (list (list (first acc) (second (first lvl))) acc)]
          [(member (second (second lvl)) acc)
           (let [(new-symbol (generate-symbol (second (second lvl)) acc))]
             (list (list (list (first acc)(second (first lvl)))
                         (list (first acc) new-symbol))
                   (cons new-symbol acc)))]
          [else (list (list (list (first acc)(second (first lvl)))
                            (list (first acc) (second (second lvl))))
                      (cons (second (second lvl)) acc))]))
  (define (rnm-lvls exe accum)
    (if (empty? exe)
        '()
        (let* [(new-level-accum (rnm-lvl (first exe) accum))
               (new-level (first new-level-accum))
               (new-accum (second new-level-accum))]
          (cons new-level
                (rnm-lvls (rest exe) new-accum)))))
  (rnm-lvls exe (list (first (first (first exe))))))


;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (map (λ (el) (if (list? (first el))
                   (first (first el))
                   (first el))) loe))

;; make-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph los s f)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(and (eq? state s)
                                           (member state f))
                                      'violet]
                                     [(eq? state s) 'darkgreen]
                                     [(member state f)
                                      'violet] 
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
(define (make-edge-graph graph loe)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid )))
         graph
         loe))



;; create-graph-img
;; ndfa -> img
;; Purpose: To create a graph image for complement
(define (create-graph-img)
  (let* [(new-finals '(T R E W))]
    (overlay (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                                              '(a b c d) 
                                                              'a
                                                              '())
                                             `((T "" b)(T "" R)(T "" a))))
             E-SCENE)))


;; create-graph-imgs
;; (listof level) (listof node) -> (listof image)
;; Purpose: To create a list of graph images built level by level
         
;; rg-viz
;; 
#;(define (rg-viz rg word)
    (let* [ (w-der (map symbol->fsmlos  (filter (λ (x) (not (equal? x '->))) (grammar-derive rg word))))
            (extracted-edges (create-edges w-der))
            (loe (rename-edges extracted-edges))
            (lon (extract-nodes loe))
            (graph-imgs (create-graph-images loe lon))]
      (run-viz (viz-state (rest graph-imgs) (list (first graph-imgs)))
               draw-world 'rg-ctm)))



;; vst --> void
#;(define (run-viz a-vs draw-etc a-name)
    (begin
      (big-bang
          a-vs                
        [on-draw draw-etc]
        [on-key process-key]
        [name a-name]))
    (void))









