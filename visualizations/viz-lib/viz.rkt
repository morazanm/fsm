#lang racket/base
(require "../2htdp/universe.rkt"
         "../../fsm-gviz/private/parallel.rkt"
         "vector-zipper.rkt"
         "bounding-limits.rkt"
         "viz-state.rkt"
         "../2htdp/image.rkt"
         racket/list
         racket/promise
         "resize-viz-image.rkt"
         )

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

;; vst --> void
(define (viz a-vs draw-world process-key process-tick a-name)
  (big-bang
      a-vs                
    [on-draw draw-world]
    [on-key process-key]
    [on-mouse process-mouse]
    [on-tick process-tick TICK-RATE]
    [name a-name])
  (void))

;; word rules (listof gviz-graph) (list Symbol (word -> Boolean))
;;   (U #f int) Boolean (listof (listof Symbol)) -> void
;; Purpose: Creates all the values needed to start the visualization
;; ASSUMPTION: Last two arguments are only used for context sensitive grammars
;; special-graphs - Boolean that lets us know we are using graphs meant for csgs
;; rank-node-lst - List of list of symbols that are in a specific order so that they are forced to be
;; in said order and positioned at the same level
(define (run-viz graphs first-img first-img-coord E-SCENE-WIDTH E-SCENE-HEIGHT PERCENT-BORDER-GAP
                 DEFAULT-ZOOM DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR
                 imsg-struct instructions-struct draw-world process-key process-tick name
                 
                 #:cpu-cores [cpu-cores #f] #:special-graphs? [special-graphs? 'rg] #:rank-node-lst [rank-node-lst '()])
  (define (load-image new-img)
    (if (list? new-img)
        (force (delay/thread (lambda () (apply above (map (lambda (img) ((force img))) new-img)))))
        (force (delay/thread ((force new-img))))
        )
    )
  (let* [(imgs (let ([res (create-graph-imgs graphs
                                             #:rank-node-lst rank-node-lst
                                             #:graph-type special-graphs?
                                             #:cpu-cores cpu-cores)])
                 (vector-set! res 0 first-img)
                 res))
         (curr-img (if (list? (vector-ref imgs (sub1 (vector-length imgs))))
                       (above ((force (first (vector-ref imgs (sub1 (vector-length imgs)))))) ((force (second (vector-ref imgs (sub1 (vector-length imgs)))))))
                       ((force (vector-ref imgs (sub1 (vector-length imgs)))))
                       ))
         (new-floor (find-new-floor curr-img
                                    (* E-SCENE-WIDTH PERCENT-BORDER-GAP)
                                    (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))]
    (viz (viz-state (vector->vector-zipper imgs)
                    'BEGIN
                    ((vector-ref imgs 0))
                    (if (= (vector-length imgs) 1)
                        'DNE
                        (load-image (vector-ref imgs 1)))
                    ((vector-ref imgs 0))
                    (if (list? (vector-ref imgs (sub1 (vector-length imgs))))
                        (above ((force (first (vector-ref imgs (sub1 (vector-length imgs)))))) ((force (second (vector-ref imgs (sub1 (vector-length imgs)))))))
                        ((force (vector-ref imgs (sub1 (vector-length imgs)))))
                        )
                    ((vector-ref imgs 0))
                    first-img-coord
                    DEFAULT-ZOOM
                    DEFAULT-ZOOM-CAP
                    new-floor
                    (posn 0 0)
                    (posn 0 0)
                    #f
                    0
                    imsg-struct
                    instructions-struct)
         draw-world process-key process-tick name)))