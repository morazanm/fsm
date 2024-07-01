#lang racket
(require (for-syntax syntax/parse
                     racket/base)
         2htdp/universe
         2htdp/image
         "../fsm-core/interface.rkt"
         "../fsm-core/private/regular-grammar.rkt"
         math/matrix
         math/array
         "../fsm-gviz/private/parallel.rkt"
         "../fsm-gviz/private/lib.rkt"
         "zipper.rkt"
         "bounding-limits.rkt"
         "viz-state.rkt"
         "default-viz-functions.rkt"
         )

(provide run-viz)
;(provide create-imsg-process-key viz-state viz-state-prev-mouse-posn viz-state-curr-mouse-posn viz-state)


(define-syntax (create-imsg-process-key stx)
  (syntax-parse stx
    #:literals (list)
    [(_ '((key func)...))
     #'(lambda (key-pressed a-imsgs)
         (cond [(key=? key key-pressed) (func a-imsgs)]...
               [else a-imsgs]
               ))]
    [(_ (list (~or* '(key func) (list key func))...))
     #'(lambda (key-pressed a-imsgs)
         (cond [(key=? key key-pressed) (func a-imsgs)]...
               [else a-imsgs]
               ))]))

(define-syntax (create-imsg-process-tick stx)
  (syntax-parse stx
    #:literals (list)
    [(_ '((b-limit func)...))
     #'(lambda (a-vs)
         (let [updated-vs (struct-copy viz-state a-vs
                                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs)) (func updated-vs)]...
                 [else updated-vs]
                 )
           )
         )]
    [(_ (list (~or* '(b-limit func) (list b-limit func))...))
     #'(lambda (a-vs)
         (let [updated-vs (struct-copy viz-state a-vs
                                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs)) (func updated-vs)]...
                 [else updated-vs]
                 )
           )
         )]))

(define-syntax (create-instructions-process-tick stx)
  (syntax-parse stx
    #:literals (list)
    [(_ '((b-limit func)...))
     #'(lambda (a-vs)
         (let [updated-vs (struct-copy viz-state a-vs
                                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs)) (buffer-held-click updated-vs func)]...
                 [else updated-vs]
                 )
           )
         )]
    [(_ (list (~or* '(b-limit func) (list b-limit func))...))
     #'(lambda (a-vs)
         (let [updated-vs (struct-copy viz-state a-vs
                                       [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
           (cond [(within-bounding-limits? b-limit (viz-state-prev-mouse-posn updated-vs)) (buffer-held-click updated-vs func)]...
                 [else updated-vs]
                 )
           )
         )]))



;(define E-SCENE (empty-scene E-SCENE-WIDTH E-SCENE-HEIGHT))
;(define E-SCENE-CENTER (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
;(define PERCENT-BORDER-GAP 0.9)
;(define HEDGE-COLOR 'violet)
;(define YIELD-COLOR 'orange)
;(define DEFAULT-ZOOM 1)
;(define DEFAULT-ZOOM-FLOOR 1)
;(define DEFAULT-ZOOM-CAP 2)
;(define ZOOM-INCREASE 1.1)
;(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))
;(define NODE-SIZE 50)
;(define FONT-SIZE 20)
;(define TAPE-SIZE 42)
(define TICK-RATE 1/60)
;(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))



;; An informative messages struct is a structure that has:
;; go-next func
;; go-prev func
;; go-to-end func
;; go-to-begin func
;; draw-component func
;; mouse-click func
;;     need to normalize the coordinates by adjusting the actual mouse coordinates depending on where the informative messages are in the viz
;; dictionary or list containing:
;;     (list key func)

;; need to look into macros for creating the process-key function, realistically the only way to achieve the level of abstraction needed for sm-viz


;; instructions struct is a struct that contains the following:
;; image (cache it) OR could possibly change with sm-viz? also a draw function?
;; mouse-click func
;;     need to normalize the coordinates by adjusting the actual mouse coordinates depending on where the informative messages are in the viz
;; dictionary or list containing:
;;     (list key func)



;; viz-state is a structure that has
;; imgs - graph images
;; curr-image - current image to be drawn to the screen
;; image-posn - position of the graph image
;; scale-factor - mulitplicative factor used to scale the image while zooming
;; scale-factor-cap - maximum value for scale-factor
;; scale-factor-floor - minimum value for scale-factor
;; curr-mouse-posn - position of the mouse
;; dest-mouse-posn - position where the mouse is dragged
;; mouse-pressed - boolean indicating whether the mouse is pressed
;; rules - production rules
;; yield - derivation yields
;; input-word - The word given by the user to visualize
;; word-img-offset - an index into the tape img used to display a subset of the word that will fit on the screen
;; word-img-offset-cap - the largest maximum value of word-img-offset
;; scroll-accum - save the amount that the user has scrolled the instructions (allows smooth scrolling while allowing the discrete movements of the tape)
;; click-buffer - an accumulator that prevents a function from being spammed when the user holds down a mouse click
#;(struct viz-state (imgs curr-image image-posn
                        ;; physical-screen-width physical-screen-height
                        ;; scale to screen size and finally stop having windows being too big or small?
                        ;; seems mostly harmless since we could default back to original values if shell commands fail for any reason
                        scale-factor scale-factor-cap scale-factor-floor
                        prev-mouse-posn curr-mouse-posn mouse-pressed
                        
                        ;rules yield input-word word-img-offset word-img-offset-cap
                        ;scroll-accum broken-invariants
                        
                        click-buffer

                        informative-messages instructions-graphic
                        ))

;; create-graph-imgs
;; (listof dgraph) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-graph-imgs graphs #:cpu-cores [cpu-cores #f])
  (if (empty? graphs)
      '()
      (if (not cpu-cores)
          (parallel-graphs->bitmap-thunks graphs)
          (parallel-graphs->bitmap-thunks graphs #:cpu-cores cpu-cores)
          )))

;; create-graph-imgs
;; (listof dgraph) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-special-graph-imgs graphs #:cpu-cores [cpu-cores #f] #:rank-node-lst [rank-node-lst '()])
  (if (empty? graphs)
      '()
      (if (not cpu-cores)
          (parallel-special-graphs->bitmap-thunks graphs rank-node-lst)
          (parallel-special-graphs->bitmap-thunks graphs rank-node-lst #:cpu-cores cpu-cores)
          )))





;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
#;(define (process-key a-vs a-key)
  #;(let [(updated-vs (struct-copy viz-state a-vs
                                 [informative-messages ((informative-messages-process-key (viz-state-informative-messages a-vs))
                                                        (viz-state-informative-messages a-vs) a-key)]))]
    (cond [(key=? "right" a-key) (right-key-pressed updated-vs)]
          [(key=? "left" a-key) (left-key-pressed updated-vs)]
          [(key=? "down" a-key) (down-key-pressed updated-vs)]
          [(key=? "up" a-key) (up-key-pressed updated-vs)]
          [(key=? "w" a-key) (w-key-pressed updated-vs)]
          [(key=? "s" a-key) (s-key-pressed updated-vs)]
          #;[(key=? "a" a-key) (a-key-pressed a-vs)]
          #;[(key=? "d" a-key) (d-key-pressed a-vs)]
          [(key=? "r" a-key) (r-key-pressed updated-vs)]
          [(key=? "f" a-key) (f-key-pressed updated-vs)]
          [(key=? "e" a-key) (e-key-pressed updated-vs)]
          [(key=? "wheel-down" a-key) (w-key-pressed updated-vs)]
          [(key=? "wheel-up" a-key) (s-key-pressed updated-vs)]
          [else updated-vs])
    )
  )

;; viz-state int int MouseEvent
;; Updates viz-state as to whether the mouse is currently being pressed while on the visualization
(define (process-mouse a-vs x y mouse-event)
  (cond [(string=? mouse-event "button-down")
         (struct-copy viz-state a-vs
                      [curr-mouse-posn (posn x y)]
                      [mouse-pressed #t])]
        [(string=? mouse-event "button-up")
         (struct-copy viz-state a-vs
                      [curr-mouse-posn (posn x y)]
                      [mouse-pressed #f]
                      [click-buffer 0])]
        ;; Want to keep the mouse updating while it is being dragged
        [(string=? mouse-event "drag")
         (struct-copy viz-state a-vs
                      [curr-mouse-posn (posn x y)]
                      [mouse-pressed #t])]           
        ;; Can happen in both clicked and unclicked states so leave it in whatever it was
        [(string=? mouse-event "move")
         (struct-copy viz-state a-vs
                      [curr-mouse-posn (posn x y)])]
        ;; This one is ambigious, think its better to leave as whatever it already was
        [(string=? mouse-event "enter")
         (struct-copy viz-state a-vs
                      [curr-mouse-posn (posn x y)])]
        ;; Stop updating if the mouse leaves the visualization screen
        [(string=? mouse-event "leave")
         (struct-copy viz-state a-vs
                      [curr-mouse-posn (posn x y)]
                      [mouse-pressed #f])]
        [else a-vs]))



;; viz-state
;; Updates the position of the image displayed based on the movement of the mouse
#;(define (process-tick a-vs)
  (if (viz-state-mouse-pressed a-vs)
      (cond [(within-bounding-limits? E-SCENE-BOUNDING-LIMITS (viz-state-prev-mouse-posn a-vs))
             (let* [;; Determines the movement of the mouse that occured since the last tick
                    (x-diff (- (posn-x (viz-state-curr-mouse-posn a-vs)) (posn-x (viz-state-prev-mouse-posn a-vs))))
                    (y-diff (- (posn-y (viz-state-curr-mouse-posn a-vs)) (posn-y (viz-state-prev-mouse-posn a-vs))))
                    (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
                    (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))
                    (new-img-posn (posn (+ (posn-x (viz-state-image-posn a-vs)) x-diff) (+ (posn-y (viz-state-image-posn a-vs)) y-diff)))
                    (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
                    (viewport-lims (calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs)))
         
                    ;(scroll-dimensions RULE-YIELD-DIMS)
                    ]
               (cond [(within-bounding-limits? viewport-lims new-img-posn)
                      (struct-copy viz-state a-vs
                                   [image-posn new-img-posn]
                                   [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                     [(outside-x-axis-bounding-limits? viewport-lims new-img-posn)
                      (struct-copy viz-state a-vs
                                   [image-posn (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)]
                                   [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                     [(outside-y-axis-bounding-limits? viewport-lims new-img-posn)
                      (struct-copy viz-state a-vs
                                   [image-posn (posn new-img-x (posn-y (viz-state-image-posn a-vs)))]
                                   [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])]
                     [(outside-x-and-y-axis-bounding-limits? viewport-lims new-img-posn)
                      (struct-copy viz-state a-vs
                                   [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)])])
               )
             ]
            [(within-bounding-limits? (informative-messages-bounding-limits (viz-state-informative-messages a-vs))
                                      (viz-state-prev-mouse-posn a-vs))
             ((informative-messages-process-tick a-vs) a-vs)
             ]
            [(within-bounding-limits? (instructions-graphic-bounding-limits (viz-state-instructions-graphic a-vs))
                                      (viz-state-prev-mouse-posn a-vs))
             ((instructions-graphic-process-tick a-vs) a-vs)
             ]
            #|
              [(within-bounding-limits? scroll-dimensions (viz-state-prev-mouse-posn a-vs))
               (let [(new-scroll-accum (+ (viz-state-scroll-accum a-vs) x-diff))]
                 (cond [(and (>= (viz-state-word-img-offset-cap a-vs) (viz-state-word-img-offset a-vs))
                             (<= (quotient new-scroll-accum 25) -1))
                        (struct-copy viz-state a-vs
                                     [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]
                                     [word-img-offset (+ (viz-state-word-img-offset a-vs) 1)]
                                     [scroll-accum 0])]
                       [(and (> (viz-state-word-img-offset a-vs) 0)
                             (>= (quotient new-scroll-accum 25) 1))
                        (struct-copy viz-state a-vs
                                     [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]
                                     [word-img-offset (- (viz-state-word-img-offset a-vs) 1)]
                                     [scroll-accum 0])]
                       [else (struct-copy viz-state a-vs
                                          [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]
                                          [scroll-accum new-scroll-accum])]))]
              [(within-bounding-limits? ARROW-UP-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs up-key-pressed)]
              [(within-bounding-limits? ARROW-RIGHT-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs right-key-pressed)]
              [(within-bounding-limits? ARROW-LEFT-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs left-key-pressed)]
              [(within-bounding-limits? ARROW-DOWN-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs down-key-pressed)]
              [(within-bounding-limits? W-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (w-key-pressed a-vs)]
              [(within-bounding-limits? S-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (s-key-pressed a-vs)]
              [(within-bounding-limits? A-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs a-key-pressed)]
              [(within-bounding-limits? D-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs d-key-pressed)]
              [(within-bounding-limits? R-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs r-key-pressed)]
              [(within-bounding-limits? E-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs e-key-pressed)]
              [(within-bounding-limits? F-KEY-DIMS (viz-state-prev-mouse-posn a-vs))
               (buffer-held-click a-vs f-key-pressed)]
              |#
            [else a-vs])
      (struct-copy viz-state a-vs
                   [prev-mouse-posn (viz-state-curr-mouse-posn a-vs)]))
    
  )

;; create-first-img
;; node -> img
;; Purpose: To create the first graph img
(define (create-first-img node)
  (lambda () (graph->bitmap (add-node
                             (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                             node
                             #:atb (hash 'color 'black
                                         'shape 'circle
                                         'label node
                                         'fontcolor 'black
                                         'font "Sans")))))

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
#;(define (draw-world a-vs)
  (let [(PARSE-TREE-IMG (place-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)) 
                                     (posn-x (viz-state-image-posn a-vs))
                                     (posn-y (viz-state-image-posn a-vs))
                                     (rectangle E-SCENE-WIDTH E-SCENE-HEIGHT 'outline 'white)
                                     ))]
    (above PARSE-TREE-IMG
           ((informative-messages-draw-component (viz-state-informative-messages a-vs)) (informative-messages-component-state (viz-state-informative-messages a-vs)))
           (instructions-graphic-img (viz-state-instructions-graphic a-vs))
           ;(create-instructions-and-tools a-vs)
           )))

;; vst --> void
(define (viz a-vs draw-world process-key process-tick a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-world]
      [on-key process-key]
      [on-mouse process-mouse]
      [on-tick process-tick TICK-RATE]
      [name a-name]))
  (void))

;; word rules (listof gviz-graph) (list Symbol (word -> Boolean))
;;   (U #f int) Boolean (listof (listof Symbol)) -> void
;; Purpose: Creates all the values needed to start the visualization
;; ASSUMPTION: Last two arguments are only used for context sensitive grammars
;; special-graphs - Boolean that lets us know we are using graphs meant for csgs
;; rank-node-lst - List of list of symbols that are in a specific order so that they are forced to be
;; in said order and positioned at the same level
(define (run-viz graphs first-img first-img-coord DEFAULT-ZOOM DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR
                 imsg-struct instructions-struct draw-world process-key process-tick
                 
                #:cpu-cores [cpu-cores #f] #:special-graphs? [special-graphs? #f] #:rank-node-lst [rank-node-lst '()])
  (let* [;(first-img (create-first-img (first (first w-der)))) ;; The first img is always special case
         (imgs (if special-graphs?
                   (cons first-img (rest (create-special-graph-imgs graphs #:cpu-cores cpu-cores #:rank-node-lst rank-node-lst)))
                   (cons first-img (rest (create-graph-imgs graphs #:cpu-cores cpu-cores)))))]
    (viz (viz-state (list->zipper imgs)
                    ((first imgs))
                    first-img-coord
                    DEFAULT-ZOOM
                    DEFAULT-ZOOM-CAP
                    DEFAULT-ZOOM-FLOOR
                    (posn 0 0)
                    (posn 0 0)
                    #f
                    #|
                    (list->zipper rules)
                    (list->zipper w-der)
                    word
                    0
                    (let [(offset-cap (- (length word) TAPE-SIZE))]
                      (if (> 0 offset-cap)
                          0
                          offset-cap))
                    0
                    broken-invariants
                    |#
                    0
                    imsg-struct
                    instructions-struct
                    )
         draw-world process-key process-tick 'grammar-viz)))
#;(key-functions process-key mouse-functions process-tick draw-component component-state bounding-limits)


