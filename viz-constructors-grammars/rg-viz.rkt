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

(define E-SCENE-TOOLS (overlay (above (above (above (triangle 30 'solid 'black)
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
                               (empty-scene 250 800)))



;; viz-state is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
(struct viz-state (upimgs pimgs))


;; dgrph is a structure that has
;; up-levels - unprocessed levels
;; ad-levels - levels added to the graph
;; nodes - nodes in the graph
;; leafs - leaf node in each graph
(struct dgrph (up-levels ad-levels nodes leafs))


;; upper-lower
;; symbol -> symbol
;; Purpose: Converts the CAPS of the symbol
(define (upper-lower symbol)
  (if (char-upper-case? (string-ref (symbol->string symbol) 0))
      (string->symbol (string-downcase (symbol->string symbol)))
      (string->symbol (string-upcase (symbol->string symbol)))))
            
      


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

;; rename-nodes
;; (listof level) -> (listof level)
;; Purpose: To rename the terminals that reoccur in extracted edges
(define (rename-nodes exe)
  (define (rnm-lvl lvl acc)
    (cond [(empty? lvl)
           '()]
          [(and (symbol? (first lvl)) (member (second lvl) acc))
           (let [(new-symbol (generate-symbol (second lvl) acc))]
             (list (list (first lvl) new-symbol) acc))]
          [(and (symbol? (first lvl)) (not (member (second lvl) acc)))
           (list (list (first lvl) (second lvl))
                 (cons (second lvl) acc))]
          [(member (second (first lvl)) acc)
           (let [(new-symbol (generate-symbol (second (first lvl)) acc))]
             (list (list (list (first (first lvl)) new-symbol)
                         (second lvl))
                   (cons new-symbol acc)))]
          [else (list (list (first lvl)
                            (second lvl))
                      (cons (second (first lvl)) acc))]))
  (define (rnm-lvls exe accum)
    (if (empty? exe)
        '()
        (let* [(new-level-accum (rnm-lvl (first exe) accum))
               (new-level (first new-level-accum))
               (new-accum (second new-level-accum))]
          (cons new-level
                (rnm-lvls (rest exe) new-accum)))))
  (rnm-lvls exe (list (second (first (first exe))))))


;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; extract-nodes-by-lvl
;; level (listof node) -> (listof node)
;; Purpose: To extract nodes from the lon that are in the level
(define (extract-nodes-by-lvl lon level)
  (let* [(nil (flatten level))]
    (filter (λ (node) (member node nil)) lon)))

;; make-node-graph
;; graph lon leaf -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon leaf)
  (foldr (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color 'black
                        'shape 'circle
                        'label (if (member (upper-lower state) leaf)
                                   (upper-lower (string->symbol (string (string-ref (symbol->string state) 0))))
                                   (string->symbol (string (string-ref (symbol->string state) 0))))
                        'fontcolor 'black
                        'font "Sans")))
         graph
         lon))  

;; make-edge-graph
;; graph (listof level) -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph loe leaf)
  (let* [(first-foldr (foldr (λ (rule result)
                               (begin
                                 (add-edge result
                                           ""
                                           (first (first rule))
                                           (second (first rule))
                                           #:atb (hash 'fontsize 20
                                                       'style 'solid
                                                       ))) 
                               )
                             graph
                             loe))]
    (foldr (λ (rule result)
             (if (empty? (second rule))
                 result
                 (add-edge result
                           ""
                           (first (second rule))           
                           (second (second rule))
                           #:atb (hash 'fontsize 20
                                       'style 'solid
                                       ))) 
             )
           first-foldr
           loe)))



;; (listof level) -> (listof node)
;; Takes in loe from rg-viz and returns the bottom most leaf nodes
(define (leaf-nodes levels)
  (define (leaf-nodes-helper levels)
    (if (empty? levels)
        '()
        (if (empty? (first levels))
            (leaf-nodes-helper (rest levels))
            (cons (second (first levels)) (leaf-nodes-helper (rest levels)))
            )
        )
    )
  (if (empty? levels)
      '()      
      (last (map leaf-nodes-helper levels))))
           



;; create-graph-img
;; ndfa -> img
;; Purpose: To create a graph image for complement
(define (create-graph-img a-dgrph)
  (let* [(nodes (dgrph-nodes a-dgrph))
         (levels (dgrph-ad-levels a-dgrph))
         (leaf (dgrph-leafs a-dgrph))
         (new-nodes (map (λ (node) (if (member node leaf)
                                       (upper-lower node)
                                       node)) nodes))
         (new-levels (map (λ (level) (cond [(empty? (second level)) level]
                                           [(member (second (second level)) leaf)
                                            (list (list (first (first level))
                                                        (second (first level)))
                                                  (list (first (second level))
                                                        (upper-lower (second (second level)))))]
                                           [(member (second (first level)) leaf)
                                            (list (list (first (first level))
                                                        (upper-lower (second (first level))))
                                                  (list (first (second level))
                                                        (second (second level))))]
                                           [else level])) levels))
                                            
         ]
    (overlay (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                                              nodes leaf)
                                             levels leaf))
             E-SCENE)))


;; create-graph-imgs
;; (listof level) (listof node) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-graph-imgs lod)
  (if (empty? lod)
      '()
      (cons (create-graph-img (first lod)) (create-graph-imgs (rest lod)))))

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (viz-state (rest (viz-state-upimgs a-vs))
                        (cons (first (viz-state-upimgs a-vs))
                              (viz-state-pimgs a-vs))))]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (viz-state (cons (first (viz-state-pimgs a-vs))
                              (viz-state-upimgs a-vs))
                        (rest (viz-state-pimgs a-vs))
                        ))]
        [(key=? "down" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (viz-state '()
                        (append (reverse (viz-state-upimgs a-vs))
                                (viz-state-pimgs a-vs))))]
        [(key=? "up" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                      (viz-state-upimgs a-vs)))
                        (list (first (append (reverse (viz-state-pimgs a-vs))
                                             (viz-state-upimgs a-vs))))))]
        [else a-vs]))

;; resize-image :: image -> int -> int -> image
;; Scales a image to the given dimentions. This solution was adapted from
;; one of the answers found here: https://stackoverflow.com/questions/3008772/how-to-smart-resize-a-displayed-image-to-original-aspect-ratio
(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define resize-width src-width)
  (define resize-height src-height)
  (define aspect (/ resize-width resize-height))
  (define scale (min
                 (/ max-width src-width) ; scale-x
                 (/ max-height src-height))) ;scale-y

  (set! resize-width (* resize-width scale))
  (set! resize-height (* resize-height scale))
  
  (when (> resize-width max-width)
    (set! resize-width max-width)
    (set! resize-height (/ resize-width aspect)))

  (when (> resize-height max-height)
    (set! aspect (/ resize-width resize-height))
    (set! resize-height max-height)
    (set! resize-width (* resize-height aspect)))

  (scale/xy
   (/ resize-width src-width)
   (/ resize-height src-height)
   img))

;; create-dgraphs
;; dgrph (listof dgrph) -> (listof dgrph)
;; Purpose: To create all the dgrphs for graph imgs
(define (create-dgrphs a-dgrph lod)
  (if (empty? (dgrph-up-levels a-dgrph))
      (cons a-dgrph lod)
      (let* [(new-up-levels (rest (dgrph-up-levels a-dgrph)))
             (new-ad-levels (cons (first (dgrph-up-levels a-dgrph))
                                  (dgrph-ad-levels a-dgrph)))
             (new-nodes (extract-nodes new-ad-levels))
             (new-leafs (leaf-nodes (reverse new-ad-levels)))
             ]
        (create-dgrphs
         (dgrph new-up-levels                      
                new-ad-levels
                new-nodes
                new-leafs)
         (cons a-dgrph lod))
        )))

;; create-first-img
;; node -> img
;; Purpose: To create the first graph img
(define (create-first-img node)
  (overlay (graph->bitmap (add-node
                           (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                           node
                           #:atb (hash 'color 'black
                                       'shape 'circle
                                       'label node
                                       'fontcolor 'black
                                       'font "Sans")))
           E-SCENE))

;; draw-img
;; viz-state -> img
;; Purpose: To render the given viz-state
;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pimgs a-vs))))
        (height (image-height (first (viz-state-pimgs a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (beside E-SCENE-TOOLS (resize-image (first (viz-state-pimgs a-vs)) (image-width E-SCENE) (image-height E-SCENE)))
        (beside E-SCENE-TOOLS (first (viz-state-pimgs a-vs))))))

         
;; rg-viz
;; 
(define (rg-viz rg word)
  (let* [ (w-der (map symbol->fsmlos  (filter (λ (x) (not (equal? x '->))) (grammar-derive rg word))))
          (extracted-edges (create-edges w-der))
          (renamed (rename-nodes (rename-edges extracted-edges)))
          (loe (map (λ (el) (if (symbol? (first el))
                                (list el '())
                                el)) renamed))
          (dgraph (dgrph loe '() '() '()))
          (lod (reverse (create-dgrphs dgraph '())))
          (first-img (create-first-img (first (extract-nodes loe))))
          (imgs (cons first-img (rest (create-graph-imgs lod))))]
    (run-viz (viz-state (rest imgs) (list (first imgs)))
             draw-world 'rg-ctm)))



;; vst --> void
(define (run-viz a-vs draw-etc a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-etc]
      [on-key process-key]
      [name a-name]))
  (void))








