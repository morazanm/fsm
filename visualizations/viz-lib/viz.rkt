#lang racket
(require 2htdp/universe
         "../../fsm-gviz/private/parallel.rkt"
         "../../fsm-gviz/private/lib.rkt"
         racket/async-channel
         "vector-zipper.rkt"
         "bounding-limits.rkt"
         "viz-state.rkt"
         2htdp/image)

(provide run-viz)

(define TICK-RATE 1/60)

;; create-graph-imgs
;; (listof dgraph) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-graph-imgs graphs #:graph-type [graph-type 'rg] #:rank-node-lst [rank-node-lst '()] #:cpu-cores [cpu-cores #f])
  (if (empty? graphs)
      '()
      (if (not cpu-cores)
          (streaming-parallel-graphs->bitmap-thunks graphs #:graph-type graph-type #:rank-node-lst rank-node-lst)
          (streaming-parallel-graphs->bitmap-thunks graphs #:graph-type graph-type #:rank-node-lst rank-node-lst #:cpu-cores cpu-cores)
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

(define (create-cfg-graph-imgs graphs #:cpu-cores [cpu-cores #f] #:rank-node-lst [rank-node-lst '()])
  (if (empty? graphs)
      '()
      (if (not cpu-cores)
          (parallel-cfg-graphs->bitmap-thunks graphs rank-node-lst)
          (parallel-cfg-graphs->bitmap-thunks graphs rank-node-lst #:cpu-cores cpu-cores)
          )))

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

;; vst --> void
(define (viz a-vs draw-world process-key process-tick a-name)
  (begin
    (collect-garbage 'major)
    (big-bang
        a-vs                
      [on-draw draw-world]
      [on-key process-key]
      [on-mouse process-mouse]
      [on-tick process-tick TICK-RATE]
      [name a-name]))
  (void))

(define (load-image new-img)
  (if (list? new-img)
      (force (delay/thread (thunk (apply above (map (lambda (img) (img)) (force new-img))) #;(async-channel-put chnnl (apply above (map (lambda (img) (img)) (force new-img))))
                                  )))
      (force (delay/thread ((force new-img)) #;(async-channel-put chnnl ((force new-img)))
                           ))
      )
  )

;; word rules (listof gviz-graph) (list Symbol (word -> Boolean))
;;   (U #f int) Boolean (listof (listof Symbol)) -> void
;; Purpose: Creates all the values needed to start the visualization
;; ASSUMPTION: Last two arguments are only used for context sensitive grammars
;; special-graphs - Boolean that lets us know we are using graphs meant for csgs
;; rank-node-lst - List of list of symbols that are in a specific order so that they are forced to be
;; in said order and positioned at the same level
(define (run-viz graphs first-img first-img-coord DEFAULT-ZOOM DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR
                 imsg-struct instructions-struct draw-world process-key process-tick name
                 
                 #:cpu-cores [cpu-cores #f] #:special-graphs? [special-graphs? #f] #:rank-node-lst [rank-node-lst '()])
  (let* [(imgs (cond [(eq? special-graphs? 'cfg) (let ([res (create-graph-imgs graphs #:rank-node-lst rank-node-lst #:graph-type 'cfg #:cpu-cores cpu-cores)])
                                                   (vector-set! res 0 first-img)
                                                   res
                                                   )
                                                 #;(cons first-img (rest (create-cfg-graph-imgs graphs #:cpu-cores cpu-cores #:rank-node-lst rank-node-lst)))]
                     [(eq? special-graphs? 'csg) (let ([res (create-graph-imgs graphs #:rank-node-lst rank-node-lst #:graph-type 'csg #:cpu-cores cpu-cores)])
                                                   (vector-set! res 0 first-img)
                                                   res
                                                   )
                                                 #;(cons first-img (rest (create-special-graph-imgs graphs #:cpu-cores cpu-cores #:rank-node-lst rank-node-lst)))]
                     [else (let ([res (create-graph-imgs graphs #:cpu-cores cpu-cores)])
                             (vector-set! res 0 first-img)
                             res
                             )]))]
    (viz (viz-state (vector->vector-zipper imgs)
                    'BEGIN
                    ((vector-ref imgs 0))
                    (load-image (vector-ref imgs 1))
                    ((vector-ref imgs 0))
                    ((force (vector-ref imgs (sub1 (vector-length imgs)))))
                    first-img-coord
                    DEFAULT-ZOOM
                    DEFAULT-ZOOM-CAP
                    DEFAULT-ZOOM-FLOOR
                    (posn 0 0)
                    (posn 0 0)
                    #f
                    0
                    imsg-struct
                    instructions-struct)
         draw-world process-key process-tick name)))
#;(key-functions process-key mouse-functions process-tick draw-component component-state bounding-limits)


