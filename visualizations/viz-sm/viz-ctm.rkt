#lang racket/base

(require "../../fsm-gviz/private/lib.rkt"
         "../../fsm-core/private/tm.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/callgraphs/transdiagram-ctm6.rkt"
         "../../visualizations/viz-lib/zipper.rkt"
         "../../visualizations/viz-lib/viz-constants.rkt"
         "../../visualizations/viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../../visualizations/viz-lib/bounding-limits.rkt"
         "../../visualizations/viz-lib/default-viz-function-generators.rkt"
         "../../visualizations/viz-lib/viz-macros.rkt"
         "../../visualizations/viz-lib/viz-state.rkt"
         "../../visualizations/viz-lib/viz.rkt"
         racket/list
         racket/function)

(provide ctm-viz)

(define FNAME "fsm")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER FONT-SIZE
                                               (list (list ARROW-UP-KEY "Restart")
                                                     (list ARROW-RIGHT-KEY "Forward")
                                                     (list ARROW-LEFT-KEY "Backward")
                                                     (list ARROW-DOWN-KEY "Finish")
                                                     (list CURSOR "Hold to drag")
                                                     (list W-KEY "Zoom in")
                                                     (list S-KEY "Zoom out")
                                                     (list R-KEY "Min zoom")
                                                     (list E-KEY "Mid zoom")
                                                     (list F-KEY "Max zoom")
                                                     (list A-KEY "Tape left")
                                                     (list D-KEY "Tape right"))))

(define TAPE-SIZE 20)

(define beginning-imsg-img
  (let* [(tape '(A B C))
         (start-index 0)
         (head-pos 0)]
    (define (make-tape-img loi start-index)
      (if (empty? (rest loi))
          (above (first loi)
                 (square 5 'solid 'white)
                 (text (number->string start-index) 10 'black))
          (beside (above (first loi)
                         (square 5 'solid 'white)
                         (text (number->string start-index) 10 'black))
                  (make-tape-img (rest loi) (add1 start-index)))))
    (let [(letter-imgs (build-list TAPE-SIZE
                                   (λ (i) (if (< (+ start-index i) (length tape))
                                              (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                             24
                                                             (if (= i (- head-pos start-index))
                                                                 'red
                                                                 'black))
                                                       (overlay (square 50 'solid 'white)
                                                                (square (add1 50) 'solid 'black)))
                                              (overlay (square 50 'solid 'white)
                                                       (square (add1 50) 'solid 'black))))))]
      (above (text "The machine halts" 20 'purple)
             (square 30 'solid 'white)
             (make-tape-img letter-imgs start-index)
             (square 30 'solid 'white)
             ))))

(define E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height beginning-imsg-img)
                          (image-height E-SCENE-TOOLS)))
(define MIDDLE-E-SCENE (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))

(define RULE-YIELD-DIMS
  (bounding-limits 0
                   (image-width beginning-imsg-img)
                   E-SCENE-HEIGHT
                   (+ E-SCENE-HEIGHT (image-height beginning-imsg-img))))

(create-bounding-limits E-SCENE-WIDTH E-SCENE-HEIGHT (image-width E-SCENE-TOOLS) RULE-YIELD-DIMS FONT-SIZE LETTER-KEY-WIDTH-BUFFER INS-TOOLS-BUFFER
                        ((ARROW-UP-KEY "Restart")
                         (ARROW-RIGHT-KEY "Forward")
                         (ARROW-LEFT-KEY "Backward")
                         (ARROW-DOWN-KEY "Finish")
                         (CURSOR "Hold to drag")
                         (W-KEY "Zoom in")
                         (S-KEY "Zoom out")
                         (R-KEY "Min zoom")
                         (E-KEY "Mid zoom")
                         (F-KEY "Max zoom")
                         (A-KEY "Tape left")
                         (D-KEY "Tape right")))

;; imsg-struct
(struct imsg-struct (tapes vars))


;; graph is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
(struct graph (upimgs pimgs))

;; tapelist is a structure that has
;; utape which is unprocessed tape images
;; ptape which is processed tape image
(struct tapelist (utape ptape))

;; var is a structure that has
;; uvar which is unprocessed labels
;; pvar which is processed labels
(struct var (uvar pvar))

;; viz-state is a structure that has
;; graph which is a structure
;; tape which is a list that contains all elements
;; needed to create a tape image
;; tapeimg is the image of the current tape
;(struct viz-state (graph tapelist tapeimg var))

;; create-nodes
;; graph (listof node) -> graph
;; Purpose: To add the nodes to the graph
(define (create-nodes dgraph lon edge)
  (foldl (λ (state result)
           (add-node
            result
            (string->symbol (first state))
            #:atb (hash 'color (second (first (second state)))
                        'style (if (equal? (first state) (first edge))
                                   'bold
                                   'solid)
                        'shape (second (second (second state)))
                        'label (second (third (second state)))
                        'fontcolor 'black
                        'font "Sans")))
         dgraph
         lon))                         

;; create-edges
;; graph (listof edge) edge -> graph
;; Purpose: To create graph of edges
(define (create-edges dgraph loe edge)
  (foldl (λ (rule result) (add-edge result
                                    (if (equal? (second (first (third rule))) '_)
                                        'BLANK
                                        (second (first (third rule))))
                                    (string->symbol (first rule))
                                    (string->symbol (second rule))
                                    #:atb (hash 'fontsize 14
                                                'style (second (second (third rule)))
                                                'color (cond [(and (equal? (first rule) (first edge))
                                                                   (equal? (second rule) (second edge)))
                                                              'dodgerblue2]
                                                             [(equal? (second (third (third rule))) "white")
                                                              'white]    
                                                             [else 'black])
                                                'headlabel (second (fourth (third rule)))
                                                )))
         dgraph
         loe))


(define (right-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-end? (imsg-struct-tapes
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-next (imsg-struct-tapes
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-end? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-next (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))

(define (left-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-begin? (imsg-struct-tapes
                                                                         (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-prev (imsg-struct-tapes
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-begin? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-prev (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))

(define (up-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-begin? (imsg-struct-tapes
                                                                         (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-to-begin (imsg-struct-tapes
                                                                        (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-begin? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-to-begin (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))


(define (down-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (if (zipper-at-end? (imsg-struct-tapes
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                      (imsg-struct-tapes
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))
                                                      (zipper-to-end (imsg-struct-tapes
                                                                      (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
                                                      )
                                                  ]
                                           [vars (if (zipper-at-end? (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                                     (zipper-to-end (imsg-struct-vars (informative-messages-component-state (viz-state-informative-messages a-vs))))
                                                     )
                                                 ]
                                           )])]))


(define (d-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (zipper-set (imsg-struct-tapes
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))
                                                              (list (first (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs)))))
                                                                    (add1 (second (zipper-current (imsg-struct-tapes
                                                                                                   (informative-messages-component-state
                                                                                                    (viz-state-informative-messages a-vs))))))
                                                                    (third (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs))))))
                                                              )
                                                  ]
                                           )])]))

(define (a-key-pressed a-vs)
  (struct-copy viz-state
               a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-struct
                                           (informative-messages-component-state (viz-state-informative-messages a-vs))
                                           [tapes (zipper-set (imsg-struct-tapes
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))
                                                              (list (first (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs)))))
                                                                    (if (< 0 (second (zipper-current (imsg-struct-tapes
                                                                                                      (informative-messages-component-state
                                                                                                       (viz-state-informative-messages a-vs))))))
                                                                        (sub1 (second (zipper-current (imsg-struct-tapes
                                                                                                       (informative-messages-component-state
                                                                                                        (viz-state-informative-messages a-vs))))))
                                                                        (second (zipper-current (imsg-struct-tapes
                                                                                                 (informative-messages-component-state
                                                                                                  (viz-state-informative-messages a-vs))))))
                                                                    (third (zipper-current (imsg-struct-tapes
                                                                                            (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs)))))))
                                                  ]
                                           )])]))

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.


;; list2string
;; list -> string
;; Purpose: To convert all elements of the list into a string
(define (list2string a-list)
  (if (empty? a-list)
      (rectangle 0.1 40 'outline 'black)
      (beside (overlay (text (format "~s" (first a-list)) 30 'black)
                       (rectangle 36 40 'outline 'black))
              (list2string (rest a-list)))))

;; draw-imsg
;; list number number var -> image
;; Purpose: To make a informative message image
(define (draw-imsg a-tape)
  (let* [(tape (first (zipper-current (imsg-struct-tapes a-tape))))
         (start-index (second (zipper-current (imsg-struct-tapes a-tape))))
         (head-pos (third (zipper-current (imsg-struct-tapes a-tape))))
         (var (zipper-current (imsg-struct-vars a-tape)))]
    (define (make-tape-img loi start-index)
      (if (empty? (rest loi))
          (above (first loi)
                 (square 5 'solid 'white)
                 (text (number->string start-index) 10 'black))
          (beside (above (first loi)
                         (square 5 'solid 'white)
                         (text (number->string start-index) 10 'black))
                  (make-tape-img (rest loi) (add1 start-index)))))
    (let [(letter-imgs (build-list TAPE-SIZE
                                   (λ (i) (if (< (+ start-index i) (length tape))
                                              (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                             24
                                                             (if (= i (- head-pos start-index))
                                                                 'red
                                                                 'black))
                                                       (overlay (square 50 'solid 'white)
                                                                (square (add1 50) 'solid 'black)))
                                              (overlay (square 50 'solid 'white)
                                                       (square (add1 50) 'solid 'black))))))]
      (if (zipper-at-end? (imsg-struct-tapes a-tape))
          (above (text "The machine halts" 20 'purple)
                 (square 30 'solid 'white)
                 (make-tape-img letter-imgs start-index)
                 (square 30 'solid 'white)
                 )
          (above var
                 (square 30 'solid 'white)
                 (make-tape-img letter-imgs start-index)
                 (square 30 'solid 'white)
                 )
          )
      )))


;; create-graph-img
;; (listof edge) (listof node) edge -> img
;; Purpose: To create a graph img for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graphic loe lon edge)
  (create-edges
   (create-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                      'font "Sans"))
    lon edge)
   loe edge))


;; create-tape
;; (listof trace) -> (listof (listof tape))
;; Purpose: To create a list of lists of tapes
(define (create-tape lot)
  (if (empty? lot)
      empty
      (cons (list (tmconfig-tape (first lot))
                  (if (> (length (tmconfig-tape (first lot))) TAPE-SIZE)
                      (- (length (tmconfig-tape (first lot))) TAPE-SIZE)
                      0)
                  (tmconfig-index (first lot)))
            (create-tape (rest lot)))))

;; create-graph-imgs
;; (listof edge) (listof node) (listof edge) -> (listof image)
;; Purpose: To create a list of transition diagram images
(define (create-graphics loe lon comp-edges)
  (if (empty? comp-edges)
      empty
      (cons (create-graphic loe lon (first comp-edges))
            (create-graphics loe lon (rest comp-edges)))))

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

;; extract-labels
;; loe -> lol
;; Purpose: To extract a list of labels from edges
(define (extract-labels loe)
  (if (empty? loe)
      '()
      (cons (first (third (first loe)))
            (extract-labels (rest loe)))))

;; loe -> loe
;; Purpose: To fix the blank label in computation edges
(define (fix-blank-label loe)
  (map (λ (rule) (if (equal? (second (first (third rule))) "_")
                     (list (first rule) (second rule)
                           (list '(label "BLANK")
                                 (second (third rule))
                                 (third (third rule))
                                 (fourth (third rule))))
                     rule)) loe))


;; references
;; (listof trace) accum -> (listof number)
;; Purpose: To extract the listrefs of VARS in the trace
(define (references lot accum)
  (if (> accum (sub1 (length lot)))
      empty
      (if (not (tmconfig? (list-ref lot accum)))
          (cons accum (references lot (add1 accum)))
          (references lot (add1 accum)))))



;; remove-configs
;; (listof num) (listof configs) -> (listof configs)
;; Purpose: To remove tmconfigs in the place of vars
(define (remove-configs refs configs)
  (if (empty? refs)
      configs
      (remove (list-ref configs (first refs)) (remove-configs (rest refs) configs))))

(define viz-go-next (go-next E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-prev (go-prev E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-begin (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-end (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))


(define viz-zoom-in (zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-zoom-out (zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-out (max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-in (max-zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-reset-zoom (reset-zoom E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))

;; ctm-viz
;; ctm a-list (listof symbol) number -> void
(define (ctm-viz ctm ctm-list tape head)
  (let* [(ce (fix-blank-label (computation-edges ctm ctm-list tape head)))
         (last-node (second (last ce)))
         (comp-edges (append (list (list "dummy-edge" "edge-dummy" (list '(label "dummy")'(style "dummy") '(color "dummy") '(headlabel "dummy"))))
                             ce
                             (list (list last-node "edge-dummy" (list '(label "dummy") '(style "dummy")'(color "dummy") '(headlabel "dummy"))))))
         (loedges (fix-blank-label (clean-list (dot-edges (parse-program ctm-list)))))
         (lonodes (clean-list (dot-nodes (parse-program ctm-list))))
         (tmconfigs (filter (λ (x) (tmconfig? x)) (ctm-apply ctm tape head #t)))
         (all-vars (map (λ (x) (third x))
                        (filter (λ (x) (and (not (tmconfig? x))
                                            (equal? 'VAR (first x)))) (ctm-apply ctm tape head #t))))
         (tmc-var (filter (λ (x) (or (tmconfig? x)
                                     (equal? 'VAR (first x)))) (ctm-apply ctm tape head #t)))
         (refs (map (λ (x) (sub1 x)) (references tmc-var 0)))
         (tmconf-clean (remove-configs refs tmc-var))
         (varimgs (append (map (λ (var) (if (tmconfig? var)
                                            (text "" 20 'black)
                                            (text (format "~a = ~a" (second var)(third var)) 20 'black))) tmconf-clean)))
         (lographs (create-graphics loedges lonodes comp-edges))
         (tapes (imsg-struct (list->zipper (drop-right (create-tape tmconfigs) 1)) (list->zipper (drop-right varimgs 1))))
         (lovars (extract-labels comp-edges))]
    (run-viz lographs
             (lambda () (graph->bitmap (first lographs)))
             MIDDLE-E-SCENE
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (imsg-struct (list->zipper (drop-right (create-tape tmconfigs) 1)) (list->zipper (drop-right varimgs 1)))
                                   (bounding-limits 0 0 0 0)
                                   )
             (instructions-graphic
              E-SCENE-TOOLS
              (bounding-limits 0 0 0 0))
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key ["right" viz-go-next right-key-pressed]
                                     ["left" viz-go-prev left-key-pressed]
                                     ["up" viz-go-to-begin up-key-pressed]
                                     ["down" viz-go-to-end down-key-pressed]
                                     ["w" viz-zoom-in identity]
                                     ["s" viz-zoom-out identity]
                                     ["r" viz-max-zoom-out identity]
                                     ["f" viz-max-zoom-in identity]
                                     ["e" viz-reset-zoom identity]
                                     ["wheel-down" viz-zoom-in identity]
                                     ["wheel-up" viz-zoom-out identity]
                                     ["a" identity a-key-pressed]
                                     ["d" identity d-key-pressed])
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ()
                                      ([ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [W-KEY-DIMS viz-zoom-in identity]
                                        [S-KEY-DIMS viz-zoom-out identity]
                                        [R-KEY-DIMS viz-max-zoom-out identity]
                                        [E-KEY-DIMS viz-reset-zoom identity]
                                        [F-KEY-DIMS viz-max-zoom-in identity]
                                        [A-KEY-DIMS identity a-key-pressed]
                                        [D-KEY-DIMS identity d-key-pressed]))
             'ctm-viz)))


