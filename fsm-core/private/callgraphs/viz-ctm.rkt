#lang racket

(require "../../../fsm-gviz/private/lib.rkt"
         ;"../../../main.rkt"
         "../tm.rkt"
         "../constants.rkt"
         "../../../fsm-core/private/configuration.rkt"
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "transdiagram-ctm2.rkt")

(provide ctm-viz)

(define FNAME "fsm")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define E-SCENE (empty-scene 1250 600))
(define TAPE-SIZE 20)
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
                                       (square 40 'solid 'white)
                                       (above (text "S - scroll tape left one space" 18 'black)
                                              (square 10 'solid 'white)
                                              (text "D - scroll tape right one space" 18 'black))
                                       )
                               (empty-scene 1250 100)))


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
(struct viz-state (graph tapelist tapeimg var))

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

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization on step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (or (empty? (graph-upimgs (viz-state-graph a-vs)))
                 (empty? (tapelist-utape (viz-state-tapelist a-vs))))
             a-vs
             (let* [(new-utape (rest (tapelist-utape (viz-state-tapelist a-vs))))
                    (new-ptape (cons (first (tapelist-utape (viz-state-tapelist a-vs)))
                                     (tapelist-ptape (viz-state-tapelist a-vs))))
                    (message (above (make-tape-img (first (first new-ptape))
                                                   (second (first new-ptape))
                                                   (third (first new-ptape)))
                                    (square 30 'solid 'white)
                                    (text "The machine halts" 20 'purple)))
                    (new-uvar (rest (var-uvar (viz-state-var a-vs))))
                    (new-pvar (cons (first (var-uvar (viz-state-var a-vs)))
                                    (var-pvar (viz-state-var a-vs))))]
               (if (= 1 (length (graph-upimgs (viz-state-graph a-vs))))
                   (viz-state (graph (rest (graph-upimgs (viz-state-graph a-vs)))
                                     (cons (first (graph-upimgs (viz-state-graph a-vs)))
                                           (graph-pimgs (viz-state-graph a-vs))))
                              (tapelist new-utape
                                        new-ptape)
                              message
                              (if (empty? (var-uvar (viz-state-var a-vs)))
                                  (viz-state-var a-vs)                                  
                                  (var (rest (var-uvar (viz-state-var a-vs)))
                                       (cons (first (var-uvar (viz-state-var a-vs)))
                                             (var-pvar (viz-state-var a-vs))))))
                   (viz-state (graph (rest (graph-upimgs (viz-state-graph a-vs)))
                                     (cons (first (graph-upimgs (viz-state-graph a-vs)))
                                           (graph-pimgs (viz-state-graph a-vs))))
                              (tapelist new-utape
                                        new-ptape)
                              (above (make-tape-img (first (first new-ptape))
                                                    (second (first new-ptape))
                                                    (third (first new-ptape)))
                                     (square 30 'solid 'white)
                                     (first new-pvar)
                                     ) 
                              (if (empty? (var-uvar (viz-state-var a-vs)))
                                  (viz-state-var a-vs) 
                                  (var new-uvar
                                       new-pvar))))
               ))]
        [(key=? "left" a-key)
         (if (or (= (length (graph-pimgs (viz-state-graph a-vs))) 1)
                 (= (length (tapelist-ptape (viz-state-tapelist a-vs))) 0))
             a-vs
             (let* [(new-utape (cons (first (tapelist-ptape (viz-state-tapelist a-vs)))
                                     (tapelist-utape (viz-state-tapelist a-vs))))
                    (new-ptape (rest (tapelist-ptape (viz-state-tapelist a-vs))))
                    (new-pvar (rest (var-pvar (viz-state-var a-vs))))
                    (new-uvar (cons (first (var-pvar (viz-state-var a-vs)))
                                    (var-uvar (viz-state-var a-vs))))]
               (viz-state (graph (cons (first (graph-pimgs (viz-state-graph a-vs)))
                                       (graph-upimgs (viz-state-graph a-vs)))                             
                                 (rest (graph-pimgs (viz-state-graph a-vs))))
                          (tapelist new-utape
                                    new-ptape)
                          (above (make-tape-img (first (first new-ptape))
                                                (second (first new-ptape))
                                                (third (first new-ptape)))
                                 (square 30 'solid 'white)
                                 (if (= 1 (length (var-pvar (viz-state-var a-vs))))
                                     (first new-uvar)
                                     (first new-pvar)
                                     )
                                 )
                          (if (= 1 (length (var-pvar (viz-state-var a-vs))))
                              (viz-state-var a-vs) 
                              (var new-uvar                             
                                   new-pvar)))))]
        [(key=? "down" a-key)
         (if (empty? (graph-upimgs (viz-state-graph a-vs)))
             a-vs
             (let* [(new-utape '())
                    (new-ptape (append (reverse (tapelist-utape (viz-state-tapelist a-vs)))
                                       (tapelist-ptape (viz-state-tapelist a-vs))))]
               (viz-state (graph '()
                                 (append (reverse (graph-upimgs (viz-state-graph a-vs)))
                                         (graph-pimgs (viz-state-graph a-vs))))
                          (tapelist new-utape
                                    new-ptape)
                          (above (make-tape-img (first (first new-ptape))
                                                (second (first new-ptape))
                                                (third (first new-ptape)))
                                 (square 30 'solid 'white)
                                 (text "The machine halts" 20 'purple))
                          (var (list (first (append (reverse (var-uvar (viz-state-var a-vs)))
                                                    (var-pvar (viz-state-var a-vs)))))
                               (rest (append (reverse (var-uvar (viz-state-var a-vs)))
                                             (var-pvar (viz-state-var a-vs))))))))]
        [(key=? "up" a-key)
         (if (= (length (graph-pimgs (viz-state-graph a-vs))) 1)
             a-vs
             (let* [(new-utape (rest (append (reverse (tapelist-ptape (viz-state-tapelist a-vs)))
                                             (tapelist-utape (viz-state-tapelist a-vs)))))
                    (new-ptape (list (first (append (reverse (tapelist-ptape (viz-state-tapelist a-vs)))
                                                    (tapelist-utape (viz-state-tapelist a-vs))))))]
               (viz-state (graph (rest (append (reverse (graph-pimgs (viz-state-graph a-vs)))
                                               (graph-upimgs (viz-state-graph a-vs))))
                                 (list (first (append (reverse (graph-pimgs (viz-state-graph a-vs)))
                                                      (graph-upimgs (viz-state-graph a-vs))))))
                          (tapelist new-utape
                                    new-ptape)
                          (above (make-tape-img (first (first new-ptape))
                                                (second (first new-ptape))
                                                (third (first new-ptape)))
                                 (square 30 'solid 'white)
                                 (first (var-pvar (viz-state-var a-vs)))
                                 )
                          (var (rest (append (reverse (var-pvar (viz-state-var a-vs)))
                                             (var-uvar (viz-state-var a-vs))))
                               (list (first (append (reverse (var-pvar (viz-state-var a-vs)))
                                                    (var-uvar (viz-state-var a-vs)))))))))]
        [(key=? "d" a-key)
         (if (and (> (length (first (first (tapelist-ptape (viz-state-tapelist a-vs))))) TAPE-SIZE)
                  (not (= (+ (second (first (tapelist-ptape (viz-state-tapelist a-vs)))) TAPE-SIZE)
                          (length (first (first (tapelist-ptape (viz-state-tapelist a-vs))))))))           
             (let* [(new-tape (list (first (first (tapelist-ptape (viz-state-tapelist a-vs))))
                                    (add1 (second (first (tapelist-ptape (viz-state-tapelist a-vs)))))
                                    (third (first (tapelist-ptape (viz-state-tapelist a-vs))))))
                    (new-ptape (cons new-tape (rest (tapelist-ptape (viz-state-tapelist a-vs)))))
                    (new-tapelist (tapelist (tapelist-utape (viz-state-tapelist a-vs))
                                            new-ptape))
                    (message (above (make-tape-img (first (first new-ptape))
                                                   (second (first new-ptape))
                                                   (third (first new-ptape)))
                                    (square 30 'solid 'white)
                                    (text "The machine halts" 20 'purple)))
                    ]
               (viz-state (viz-state-graph a-vs)
                          new-tapelist
                          message
                          (viz-state-var a-vs)
                          ))
             a-vs)]
        
        [(key=? "s" a-key)
         (if (= (second (first (tapelist-ptape (viz-state-tapelist a-vs)))) 0)
             a-vs
             (let* [(new-tape (list (first (first (tapelist-ptape (viz-state-tapelist a-vs))))
                                    (sub1 (second (first (tapelist-ptape (viz-state-tapelist a-vs)))))
                                    (third (first (tapelist-ptape (viz-state-tapelist a-vs))))))
                    (new-ptape (cons new-tape (rest (tapelist-ptape (viz-state-tapelist a-vs)))))
                    (new-tapelist (tapelist (tapelist-utape (viz-state-tapelist a-vs))
                                            new-ptape))
                    ]
               (viz-state (viz-state-graph a-vs)
                          new-tapelist
                          (if (empty? (graph-upimgs (viz-state-graph a-vs)))
                              (above (make-tape-img (first (first (tapelist-ptape new-tapelist)))
                                                    (second (first (tapelist-ptape new-tapelist)))
                                                    (third (first (tapelist-ptape new-tapelist))))
                                     (square 30 'solid 'white)
                                     (text "The machine halts" 20 'purple))
                              (above (make-tape-img (first (first (tapelist-ptape (viz-state-tapelist a-vs))))
                                                    (sub1 (second (first (tapelist-ptape (viz-state-tapelist a-vs)))))
                                                    (third (first (tapelist-ptape (viz-state-tapelist a-vs)))))
                                     (square 30 'solid 'white)
                                     (second (first (var-pvar (viz-state-var a-vs))))
                                     ))
                          (viz-state-var a-vs))))]
        
        [else a-vs]))

;; list2string
;; list -> string
;; Purpose: To convert all elements of the list into a string
(define (list2string a-list)
  (if (empty? a-list)
      (rectangle 0.1 40 'outline 'black)
      (beside (overlay (text (format "~s" (first a-list)) 30 'black)
                       (rectangle 36 40 'outline 'black))
              (list2string (rest a-list)))))

;; make-tape
;; list number number -> image
;; Purpose: To make a tape
(define (make-tape-img tape start-index head-pos)
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
    (make-tape-img letter-imgs start-index)))


;; create-graph-img
;; (listof edge) (listof node) edge -> img
;; Purpose: To create a graph img for the given dgraph
;; with the labeled edge that has been expanded
(define (create-graph-img loe lon edge)
  (graph->bitmap
   (create-edges
    (create-nodes
     (create-graph 'dgraph #:atb (hash 'rankdir "LR"
                                       'font "Sans"))
     lon edge)
    loe edge))
  )


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
(define (create-graph-imgs loe lon comp-edges)
  (if (empty? comp-edges)
      empty
      (cons (create-graph-img loe lon (first comp-edges))
            (create-graph-imgs loe lon (rest comp-edges)))))

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


;; draw-img
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (if (or (> (image-width (first (graph-pimgs (viz-state-graph a-vs))))
             (- (image-width E-SCENE) 50))
          (> (image-height (first (graph-pimgs (viz-state-graph a-vs))))
             (- (image-height E-SCENE) 50)))
      (above (overlay
              (above (resize-image (first (graph-pimgs (viz-state-graph a-vs)))
                                   (- (- (image-width E-SCENE) 50) 20)
                                   (- (- (image-height E-SCENE) 50) 20))
                     (square 20 'solid 'white)
                     (viz-state-tapeimg a-vs))
              E-SCENE)
             E-SCENE-TOOLS)  
      (above (overlay
              (above (first (graph-pimgs (viz-state-graph a-vs)))
                     (square 20 'solid 'white)
                     (viz-state-tapeimg a-vs))
              E-SCENE)
             E-SCENE-TOOLS)))

;; ctm-viz
;; ctm a-list (listof symbol) number -> void
(define (ctm-viz ctm ctm-list tape head)
  (let* [(ce (fix-blank-label (computation-edges ctm ctm-list tape head))
             )
         (last-node (second (last ce)))
         (comp-edges (append (list (list "dummy-edge" "edge-dummy" (list '(label "dummy")'(style "dummy") '(color "dummy") '(headlabel "dummy"))))
                             ce
                             (list (list last-node "edge-dummy" (list '(label "dummy") '(style "dummy")'(color "dummy") '(headlabel "dummy"))))
                             )
                     )
         (loedges (fix-blank-label (clean-list (dot-edges (parse-program ctm-list))))
                  )
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
                                    
         (loimgs (create-graph-imgs loedges lonodes comp-edges))
         (tapes (create-tape tmconfigs))
         (lovars (extract-labels comp-edges))
         (tapeimg (above (make-tape-img (first (first tapes)) (second (first tapes)) (third (first tapes)))
                         (square 30 'solid 'white)
                         (first varimgs)
                         ))
         ]
    (run-viz (viz-state (graph (rest loimgs) (list (first loimgs)))
                        (tapelist (rest tapes) (list (first tapes)))
                        tapeimg
                        (var (rest varimgs) (list (first varimgs))))
             draw-world 'viz-ctm)))



;; vst --> void
(define (run-viz a-vs draw-etc a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-etc]
      [on-key process-key]
      [name a-name]))
  (void))


