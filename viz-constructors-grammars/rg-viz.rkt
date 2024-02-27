#lang racket
(require ;racket/logging
         "../fsm-gviz/private/lib.rkt" 
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../fsm-core/interface.rkt"
         math/matrix
         math/array)

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

(define E-SCENE (empty-scene 1200 800))

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

;; posn is a structure that has
;; x coordinate
;; y coordinate
(struct posn (x y))


;; viz-state is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
(struct viz-state (upimgs pimgs curr-img image-posn scale-factor curr-mouse-posn dest-mouse-posn mouse-pressed))


;; dgrph is a structure that has
;; up-levels - unprocessed levels
;; ad-levels - levels added to the graph
;; nodes - nodes in the graph
(struct dgrph (up-levels ad-levels nodes hedges))


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
  (if (= 1 (length exe))
      exe
      (rnm-lvls exe (list (second (first (first exe)))))))


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
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes)
  (foldr (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (if (member state hedge-nodes)
                                   'violet
                                   'black)
                        'shape 'circle
                        'label (string->symbol (string (string-ref (symbol->string state) 0)))
                        'fontcolor 'black
                        'font "Sans")))
         graph
         lon))  

;; make-edge-graph
;; graph (listof level) -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph loe hedges)
  (let* [(first-foldr (foldr (λ (rule result)
                               (if (empty? (first rule))
                                   result
                                   (add-edge result
                                             ""
                                             (first (first rule))
                                             (second (first rule))
                                             #:atb (hash 'fontsize 20
                                                         'style 'solid
                                                         'color (if (member (first rule) hedges)
                                                                    'violet
                                                                    'black)
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
                                       'color (if (member (first rule) hedges)
                                                  'violet
                                                  'black)
                                       )) 
                 ))
           first-foldr
           loe)))



;; create-graph-img
;; ndfa -> img
;; Purpose: To create a graph image for complement
(define (create-graph-img a-dgrph)
  (let* [(nodes (append (filter lower? (dgrph-nodes a-dgrph))
                        (filter upper? (dgrph-nodes a-dgrph))))
         (levels (reverse (map reverse (dgrph-ad-levels a-dgrph))))
         (hedges (dgrph-hedges a-dgrph))
         (hedge-nodes (map (λ (x) (if (empty? x)
                                      '()
                                      (second x))) (dgrph-hedges a-dgrph)))
         ]
    (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                                     nodes hedge-nodes) 
                                    levels hedges))))


;; create-graph-imgs
;; (listof dgraph) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-graph-imgs lod)
  (if (empty? lod)
      '()
      (cons (create-graph-img (first lod)) (create-graph-imgs (rest lod)))))

;; resize-image :: image -> int -> int -> image
;; Scales a image to the given dimentions
(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define aspect (/ src-width src-height))
  (define scale (min
                 (/ max-width src-width) ; scale-x
                 (/ max-height src-height))) ;scale-y

  (define scaled-width (* src-width scale))
  (define scaled-height (* src-height scale))

  (cond [(and (> scaled-width max-width)
              (<= scaled-height max-height)
              )
         (scale/xy
          (/ max-width src-width)
          (/ (/ scaled-width aspect) src-height)
          img)
         ]
        [(and (<= scaled-width max-width)
              (> scaled-height max-height)
              )
         (let ([scaled-aspect (/ scaled-width scaled-height)])
           (scale/xy
            (/ (* scaled-height scaled-aspect) src-width)
            (/ max-height src-height)
            img)
           )
         ]
        [(and (> scaled-width max-width)
              (> scaled-height max-height)
              )
         (let* (
                [new-scaled-height (/ max-width aspect)]
                [scaled-aspect (/ max-width new-scaled-height)]
                )
           (scale/xy
            (/ (* max-height scaled-aspect) src-width)
            (/ max-height src-height)
            img)
           )
         ]
        [(and (<= scaled-width max-width)
              (<= scaled-height max-height)
              )
         (scale/xy
          (/ scaled-width src-width)
          (/ scaled-height src-height)
          img)
         ]
        )
  )

(define (make-viz-img img img-posn scale-factor) (beside E-SCENE-TOOLS (place-image (scale scale-factor img)
                                                                                    (posn-x img-posn)
                                                                                    (posn-y img-posn)
                                                                                    E-SCENE))
  )
#;(define (make-zoom-out-coord a-vs) (local [
                                       ;; Determines the movement of the mouse that occured since the last tick
                                       (define x-diff (* 0.1 (- (- (posn-x (viz-state-curr-mouse-posn a-vs)) 250) 600)))
                                       (define y-diff (* 0.1 (- (posn-y (viz-state-curr-mouse-posn a-vs)) 400)))

                                       (define new-img-x (- (* 0.9 (posn-x (viz-state-image-posn a-vs))) x-diff))
                                       (define new-img-y (- (* 0.9 (posn-y (viz-state-image-posn a-vs))) y-diff))
                                       ]
                                 (posn new-img-x new-img-y)
                                 )
  )
(define (make-zoom-out-coord a-vs) (local [
                                       ;; Determines the movement of the mouse that occured since the last tick
                                       (define x-diff (* 0.1 (- (posn-x (viz-state-curr-mouse-posn a-vs)) 250)))
                                       (define y-diff (* 0.1 (posn-y (viz-state-curr-mouse-posn a-vs))))
                                       (define new-img-x (- (* 0.9 (posn-x (viz-state-image-posn a-vs))) x-diff))
                                       (define new-img-y (- (* 0.9 (posn-y (viz-state-image-posn a-vs)))  y-diff))
                                       ]
                                 (posn new-img-x new-img-y)
                                 )
  )
(define (make-zoom-in-coord a-vs) (local [
                                       ;; Determines the movement of the mouse that occured since the last tick
                                       (define x-diff (* 0.1 (- (posn-x (viz-state-curr-mouse-posn a-vs)) 250)))
                                       (define y-diff (* 0.1 (posn-y (viz-state-curr-mouse-posn a-vs))))

                                       (define new-img-x (-  (* 1.1 (posn-x (viz-state-image-posn a-vs))) x-diff))
                                       (define new-img-y (-  (* 1.1 (posn-y (viz-state-image-posn a-vs))) y-diff))
                                       ]
                                 (posn new-img-x new-img-y)
                                 )
  )

(define (zoom-again a-vs scale-diff) (local [
                                             (define first-translate (matrix
                                                                      [
                                                                       [1 0 (- (posn-x (viz-state-curr-mouse-posn a-vs)) 600)]
                                                                       [0 1 (- (posn-y (viz-state-curr-mouse-posn a-vs)) 400)]
                                                                       [0 0 1]
                                                                       ]
                                                                      )
                                               )
                                             (define first-scale (matrix
                                                                  [
                                                                   [scale-diff 0 0]
                                                                   [0 scale-diff 0]
                                                                   [0 0 1]
                                                                   ]
                                                                  )
                                               )
                                             (define second-translate (matrix
                                                                       [
                                                                        [1 0 (* -1 (- (posn-x (viz-state-curr-mouse-posn a-vs)) 600))]
                                                                        [0 1 (* -1 (- (posn-y (viz-state-curr-mouse-posn a-vs)) 600))]
                                                                        [0 0 1]
                                                                        ]
                                                                       )
                                               )
                                             (define final-result (matrix* (matrix* (matrix* first-translate first-scale) second-translate) (matrix [
                                                                                                                                                     [(- (posn-x (viz-state-image-posn a-vs)) 600)]
                                                                                                                                                     [(- (posn-y (viz-state-image-posn a-vs)) 400)]
                                                                                                                                                     [1]
                                                                                                                                                     ]
                                                                                                                                                    )
                                                                           )
                                               )
                                             ]
                                       #;(begin (println (format "~s ~s" (posn-x second-translate) (posn-y second-translate)))
                                              (println (format "~s" scale-diff))
                                              (println (format "~s ~s" (posn-x (viz-state-image-posn a-vs)) (posn-y (viz-state-image-posn a-vs))))
                                              (println (format "~s ~s" (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-curr-mouse-posn a-vs))))
                                              second-translate
                                              )
                                       (begin ;(println (format "~s" (matrix* (matrix* first-translate first-scale) second-translate)))
                                              (println (format "~s" (viz-state-scale-factor a-vs)))
                                              (posn (+ (matrix-ref final-result 0 0) 10) (matrix-ref final-result 1 0))
                                              )
                                       )
  )
;; TODO fix these hard coded center points
;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (let ([new-pimgs (cons (first (viz-state-upimgs a-vs))
                              (viz-state-pimgs a-vs))])
               (viz-state (rest (viz-state-upimgs a-vs))
                          new-pimgs
                          (make-viz-img (first new-pimgs) (posn 600 400) 1)
                          (posn 600 400)
                          1
                          (viz-state-curr-mouse-posn a-vs)
                          (viz-state-dest-mouse-posn a-vs)
                          (viz-state-mouse-pressed a-vs)
                          ))
             )]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (let ([new-pimgs (rest (viz-state-pimgs a-vs))])
                    (viz-state (cons (first (viz-state-pimgs a-vs))
                              (viz-state-upimgs a-vs))
                        new-pimgs
                        (make-viz-img (first new-pimgs) (posn 600 400) 1)
                        (posn 600 400)
                        1
                        (viz-state-curr-mouse-posn a-vs)
                        (viz-state-dest-mouse-posn a-vs)
                        (viz-state-mouse-pressed a-vs)
                        ))
             )]
        [(key=? "down" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (let ([new-pimgs (append (reverse (viz-state-upimgs a-vs))
                                (viz-state-pimgs a-vs))])
                    (viz-state '()
                               new-pimgs
                               (make-viz-img (first new-pimgs) (posn 600 400) 1)
                               (posn 600 400)
                               1
                               (viz-state-curr-mouse-posn a-vs)
                               (viz-state-dest-mouse-posn a-vs)
                               (viz-state-mouse-pressed a-vs)
                               )
               )
             )]
        [(key=? "up" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (let ([new-pimgs (list (first (append (reverse (viz-state-pimgs a-vs))
                                             (viz-state-upimgs a-vs))))])
                    (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                      (viz-state-upimgs a-vs)))
                        new-pimgs
                        (make-viz-img (first new-pimgs) (posn 600 400) 1)
                        (posn 600 400)
                        1
                        (viz-state-curr-mouse-posn a-vs)
                        (viz-state-dest-mouse-posn a-vs)
                        (viz-state-mouse-pressed a-vs)
                        )
               )
             )]
        [(key=? "w" a-key)
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (make-viz-img (first (viz-state-pimgs a-vs))
                                  #;(zoom-again a-vs (+ 1 (- (* (viz-state-scale-factor a-vs) 1.1) (viz-state-scale-factor a-vs))))
                                  (let ([zoom-posn (zoom-again a-vs (/ (- (* (viz-state-scale-factor a-vs) 1.025) (viz-state-scale-factor a-vs)) (viz-state-scale-factor a-vs) ))])
                                    (posn (+ 600 (posn-x zoom-posn)) (+ (posn-y zoom-posn) 400))
                                    )
                                  (* (viz-state-scale-factor a-vs) 1.1)
                                  )
                    (let ([zoom-posn (zoom-again a-vs (/ (- (* (viz-state-scale-factor a-vs) 1.025) (viz-state-scale-factor a-vs)) (viz-state-scale-factor a-vs) ))])
                                    (posn (+ 600 (posn-x zoom-posn)) (+ (posn-y zoom-posn) 400))
                                    )
                    #;(zoom-again a-vs (+ 1 (- (* (viz-state-scale-factor a-vs) 1.1) (viz-state-scale-factor a-vs))))
                    (* (viz-state-scale-factor a-vs) 1.1)
                    (viz-state-curr-mouse-posn a-vs)
                    (viz-state-dest-mouse-posn a-vs)
                    (viz-state-mouse-pressed a-vs)
                    )
         ]
        [(key=? "s" a-key)
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (make-viz-img (first (viz-state-pimgs a-vs)) (make-zoom-out-coord a-vs) (* (viz-state-scale-factor a-vs) 0.9))
                    (make-zoom-out-coord a-vs)
                    (* (viz-state-scale-factor a-vs) 0.9)
                    (viz-state-curr-mouse-posn a-vs)
                    (viz-state-dest-mouse-posn a-vs)
                    (viz-state-mouse-pressed a-vs)
                    )
         ]
        [else a-vs]
        )
  )

;; viz-state int int MouseEvent
;; Updates viz-state as to whether the mouse is currently being pressed while on the visualization
(define (process-mouse a-vs x y mouse-event)
  (cond [(string=? mouse-event "button-down")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-img a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #t)
         ]
        [(string=? mouse-event "button-up")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-img a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #f)
         ]
        ;; Want to keep the mouse updating while it is being dragged
        [(string=? mouse-event "drag")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-img a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #t)
         ]
                                                   
        ;; Can happen in both clicked and unclicked states so leave it in whatever it was
        [(string=? mouse-event "move")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-img a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    (viz-state-mouse-pressed a-vs)
                    )
         ]

        ;; This one is ambigious, think its better to leave as whatever it already was
        [(string=? mouse-event "enter")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-img a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    (viz-state-mouse-pressed a-vs)
                    )
         ]

        ;; Stop updating if the mouse leaves the visualization screen
        [(string=? mouse-event "leave")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-img a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #f)
         ]
        [else a-vs]
        )
  )

;; viz-state
;; Updates the position of the image displayed based on the movement of the mouse
(define (process-tick a-vs)
  (local [
          ;; Determines the movement of the mouse that occured since the last tick
          (define x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-dest-mouse-posn a-vs))))
          (define y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-dest-mouse-posn a-vs))))

          (define new-img-x (- (posn-x (viz-state-image-posn a-vs)) x-diff))
          (define new-img-y (- (posn-y (viz-state-image-posn a-vs)) y-diff))
          ]
    (if (viz-state-mouse-pressed a-vs)
        (viz-state (viz-state-upimgs a-vs)
                   (viz-state-pimgs a-vs)
                   (make-viz-img (first (viz-state-pimgs a-vs)) (posn new-img-x new-img-y) (viz-state-scale-factor a-vs))
                   (posn new-img-x new-img-y)
                   (viz-state-scale-factor a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-mouse-pressed a-vs))
        (viz-state (viz-state-upimgs a-vs)
                   (viz-state-pimgs a-vs)
                   (viz-state-curr-img a-vs)
                   (viz-state-image-posn a-vs)
                   (viz-state-scale-factor a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-mouse-pressed a-vs))
        )
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
                                  (dgrph-ad-levels a-dgrph)))
             (new-nodes (extract-nodes new-ad-levels))
             (new-hedges (first (dgrph-up-levels a-dgrph)))
             ]
        (create-dgrphs
         (dgrph new-up-levels                      
                new-ad-levels
                new-nodes
                new-hedges
                )
         (cons a-dgrph lod))
        )))

;; create-first-img
;; node -> img
;; Purpose: To create the first graph img
(define (create-first-img node)
  (graph->bitmap (add-node
                  (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                  node
                  #:atb (hash 'color 'black
                              'shape 'circle
                              'label node
                              'fontcolor 'black
                              'font "Sans"))))


;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs) (viz-state-curr-img a-vs))
#;(define (draw-world a-vs)
  (beside E-SCENE-TOOLS (place-image (first (viz-state-pimgs a-vs))
                                     (posn-x (viz-state-image-posn a-vs))
                                     (posn-y (viz-state-image-posn a-vs))
                                     E-SCENE)))

         
;; rg-viz
;; TODO fix hard coded center for posn
(define (rg-viz rg word)
  (if (string? (grammar-derive rg word))
      (grammar-derive rg word)
      (let* [ (w-der (map symbol->fsmlos  (filter (λ (x) (not (equal? x '->)))
                                                  (grammar-derive rg word))))
              (extracted-edges (create-edges w-der))
              (renamed (rename-nodes (rename-edges extracted-edges)))
              (loe (map (λ (el) (if (symbol? (first el))
                                    (list el '())
                                    el)) renamed))
              (dgraph (dgrph loe '() '() '()))
              (lod (reverse (create-dgrphs dgraph '())))
              (first-img (create-first-img (first (extract-nodes loe))))
              (imgs (cons first-img (rest (create-graph-imgs lod))))]
        (run-viz (viz-state (rest imgs)
                            (list (first imgs))
                            (beside E-SCENE-TOOLS (place-image (first imgs)
                                                               600
                                                               400
                                                               E-SCENE))
                            (posn 600 400)
                            1
                            (posn 0 0)
                            (posn 0 0)
                            #f)
                 draw-world 'rg-ctm))))



;; vst --> void
(define (run-viz a-vs draw-etc a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-etc]
      [on-key process-key]
      [on-mouse process-mouse]
      [on-tick process-tick]
      [name a-name]))
  (void))



; (rg-viz even-bs-odd-as '(a a a a a b b b b a a a a a a))





