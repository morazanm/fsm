#lang racket

(require "../fsm-gviz/private/lib.rkt"
         rackunit
         "../fsm-core/interface.rkt"
         "../fsm-gviz/private/parallel.rkt"
         "viz.rkt"
         )

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

(define FONT-SIZE 20)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)

;; posn is a structure that has
;; x coordinate
;; y coordinate
(struct posn (x y))

;; dgrph is a structure that has
;; up-levels - unprocessed levels
;; ad-levels - levels added to the graph
;; nodes - nodes in the graph
;; hedges - highlighted edges of the graphs
;; up-rules - unprocessed grammar rules
;; p-rules - processed grammar rules
(struct dgrph (up-levels ad-levels nodes hedges up-rules p-rules))


;; upper?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is upper case
(define (upper? symbol)
  (char-upper-case? (string-ref (symbol->string symbol) 0)))


;; lower?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is down case
(define (lower? symbol)
  (not (char-upper-case? (string-ref (symbol->string symbol) 0))))
            
      


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


;; create-rules
;; (listof symbol) -> (listof string)
(define (create-rules w-der)
  (cond [(empty? w-der)
         '()]
        [(= 1 (length w-der))
         '()]
        [(= 2 (length w-der))
         (append (list (string-append (symbol->string (last (first w-der)))
                                      " → "
                                      (symbol->string (last (second w-der)))))
                 (create-rules (rest w-der)))]
        [else (append  (list (string-append (symbol->string (last (first w-der)))
                                            " → "
                                            (string-append (first (map symbol->string (take-right (second w-der) 2)))
                                                           (second (map symbol->string (take-right (second w-der) 2))))))
                       (create-rules (rest w-der)))]))

;; rename-edges
;; (listof level) -> (listof level)
;; Purpose: To rename the nonterminals that reoccur in extracted edges
(define (rename-edges exe)
  (define (rnm-lvl lvl acc)
    (cond [(empty? lvl)'()]
          
          [(= 1 (length lvl))
           (list (list (first acc) (second (first lvl))) acc)]
          
          [(member (second (second lvl)) acc)
           (let [
                 (new-symbol (generate-symbol (second (second lvl)) acc))
                 ]
             (list (list (list (first acc)(second (first lvl)))
                         (list (first acc) new-symbol))
                   (cons new-symbol acc))
             )
           ]
          
          [else (list (list (list (first acc)(second (first lvl)))
                            (list (first acc) (second (second lvl))))
                      (cons (second (second lvl)) acc))]
          )
    )
  (define (rnm-lvls exe accum)
    (if (empty? exe)
        '()
        (let* [
               (new-level-accum (rnm-lvl (first exe) accum))
               (new-level (first new-level-accum))
               (new-accum (second new-level-accum))
               ]
          (cons new-level
                (rnm-lvls (rest exe) new-accum))
          )
        )
    )
  (rnm-lvls exe (list (first (first (first exe)))))
  )

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
  (if (= 1 (length exe))
      exe
      (rnm-lvls exe (list (second (first (first exe)))))))


;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes yield-node)
  (foldl (λ (state result) (add-node
                            result
                            state
                            #:atb (hash 'color (cond [(member state hedge-nodes)
                                                      HEDGE-COLOR]
                                                     [(member state yield-node)
                                                      YIELD-COLOR]
                                                     [else 'black]
                                                     )
                                        'shape 'circle
                                        'label (string->symbol (string (string-ref (symbol->string state) 0)))
                                        'fontcolor 'black
                                        'font "Sans"
                                        )
                            )
           )
         graph
         (reverse lon)
         )
  )  

;; make-edge-graph
;; graph (listof level) -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph loe hedges)
  (let [
        (test (displayln (reverse loe)))
        (first-foldr (foldl (λ (rule result)
                              ;(displayln rule)
                              (if (empty? (first rule))
                                  result
                                  (add-edge result
                                            ""
                                            (first (first rule))
                                            (second (first rule))
                                            #:atb (hash 'fontsize FONT-SIZE
                                                        'style 'solid
                                                        'color (if (member (first rule) hedges)
                                                                   HEDGE-COLOR
                                                                   'black)
                                                        )
                                            )
                                  )
                              )
                            graph
                            (reverse loe)
                            )
                     )
        ]
    (foldl (λ (rule result)
             (if (empty? (second rule))
                 result
                 (add-edge result
                           ""
                           (first (second rule))
                           (second (second rule))
                           #:atb (hash 'fontsize 20
                                       'style 'solid
                                       'color (if (member (first rule) hedges)
                                                  HEDGE-COLOR
                                                  'black)
                                       )
                           )
                 )
             )
           first-foldr
           (reverse loe)
           )
    )
  )

;; create-graph-structs
;; dgprh -> img
;; Purpose: Creates the final graph structure that will be used to create the images in graphviz
(define (create-graph-structs a-dgrph)
  (let* [
         (nodes (append (filter lower? (dgrph-nodes a-dgrph))
                        (filter upper? (dgrph-nodes a-dgrph))
                        )
                )
         (levels (reverse (map reverse (dgrph-ad-levels a-dgrph))))
         (hedges (dgrph-hedges a-dgrph))
         (hedge-nodes (map (λ (x) (if (empty? x)
                                      '()
                                      (second x))) hedges)
                      )
         (yield-node (map (λ (x) (if (empty? x)
                                     '()
                                     (first x))) hedges))
         ]
    (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                      nodes hedge-nodes yield-node)
                     levels hedges)
    )
  )

;; create-dgraphs
;; dgrph (listof dgrph) -> (listof dgrph)
;; Purpose: To create all the dgrphs for graph imgs
(define (create-dgrphs a-dgrph lod)
  (if (empty? (dgrph-up-levels a-dgrph))
      (cons a-dgrph lod)
      (let* [(new-up-levels (rest (dgrph-up-levels a-dgrph)))
             (new-ad-levels (cons (first (dgrph-up-levels a-dgrph))
                                  (dgrph-ad-levels a-dgrph))
                            )
             (new-nodes (extract-nodes new-ad-levels))
             (new-hedges (first (dgrph-up-levels a-dgrph)))
             (new-up-rules (rest (dgrph-up-rules a-dgrph)))
             (new-p-rules (cons (first (dgrph-up-rules a-dgrph))
                                (dgrph-p-rules a-dgrph)))
             ]
        (create-dgrphs
         (dgrph new-up-levels                      
                new-ad-levels
                new-nodes
                new-hedges
                new-up-rules
                new-p-rules
                )
         (cons a-dgrph lod))
        )
      )
  )
    
(define (rg-viz rg word #:cpu-cores [cpu-cores #f])
  (if (string? (grammar-derive rg word))
      (grammar-derive rg word)
      (let* [ (w-der (map symbol->fsmlos (filter (λ (x) (not (equal? x '->)))
                                                 (grammar-derive rg word))))
              (rules (cons "" (create-rules w-der)))
              (extracted-edges (create-edges w-der))
              (renamed (rename-nodes (rename-edges extracted-edges)))
              (loe (map (λ (el) (if (symbol? (first el))
                                    (list el '())
                                    el)) renamed))
              (dgraph (dgrph loe '() '() '() (rest rules) (list (first rules))))
              (lod (reverse (create-dgrphs dgraph '())))
              (graphs (map create-graph-structs lod))
              ]
        (run-viz rg word w-der rules graphs)
        )
      )
  )

;(rg-viz even-bs-odd-as '(a a a a a b b b b a a a a a a a a a a a b b b b a a a a a a a a a a a b b b b a a a a a a))
 

;(time
;(rg-viz even-bs-odd-as '(a a a a a b b b b a a a a a a a a a a a b b b b a a a a a a a a a a a b b b b a a a a a a a a a a a a b b b b a a a a a a a a a a a b b b b a a a a a a a a a a a b b b b a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a))
;)
;(rg-viz even-bs-odd-as '(a a a a a b b b b b b b b b b b b b b b b))

(rg-viz even-bs-odd-as '(a a a b b))