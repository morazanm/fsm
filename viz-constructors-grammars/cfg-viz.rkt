#lang racket
(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         (except-in "../fsm-core/interface.rkt"
                    make-cfg
                    )
         ;"../fsm-core/private/constants.rkt"
         math/matrix
         math/array
         "../fsm-core/private/cfg.rkt"
         )

(define FNAME "fsm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define even-bs-odd-as (make-cfg '(S A B C)
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

;; posn is a structure that has
;; x coordinate
;; y coordinate
(struct posn (x y))


(define E-SCENE-WIDTH 1250)
(define E-SCENE-HEIGHT 500)
(define E-SCENE (empty-scene E-SCENE-WIDTH E-SCENE-HEIGHT))
(define E-SCENE-CENTER (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define PERCENT-BORDER-GAP 0.9)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'orange)
(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))
(define NODE-SIZE 50)

;; viz-state is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
;; curr-image - current image to be drawn to the screen
;; image-posn - position of the graph image
;; scale-factor - mulitplicative factor used to scale the image while zooming
;; scale-factor-cap - maximum value for scale-factor
;; scale-factor-floor - minimum value for scale-factor
;; curr-mouse-posn - position of the mouse
;; dest-mouse-posn - position where the mouse is dragged
;; mouse-pressed - boolean indicating whether the mouse is pressed
;; up-dgraph - unprocessed dgraphs
;; p-dgraph - processed dgraphs
;; up-yield - unprocessed yield
;; p-yield - processed yield
(struct viz-state (upimgs pimgs curr-image image-posn
                          scale-factor scale-factor-cap scale-factor-floor
                          curr-mouse-posn dest-mouse-posn mouse-pressed
                          up-dgraph p-dgraph up-yield p-yield
                          input-word word-img-offset word-img-offset-cap)
  )

(define S-KEY (bitmap/file "./keyboard_key_s.png"))
(define W-KEY (bitmap/file "./keyboard_key_w.png"))
(define R-KEY (bitmap/file "./keyboard_key_r.png"))
(define F-KEY (bitmap/file "./keyboard_key_f.png"))
(define E-KEY (bitmap/file "./keyboard_key_e.png"))
(define ARROW-RIGHT-KEY (bitmap/file "./keyboard_key_right.png"))
(define ARROW-LEFT-KEY (bitmap/file "./keyboard_key_left.png"))
(define ARROW-UP-KEY (bitmap/file "./keyboard_key_up.png"))
(define ARROW-DOWN-KEY (bitmap/file "./keyboard_key_down.png"))

(define cursor (let (
                     (cursor-rect (let (
                                        (inner-white (rectangle 5
                                                                17.5
                                                                'solid
                                                                'white)
                                                     )
                                        (outer-black (rectangle 9
                                                                20
                                                                'solid
                                                                'black)
                                                     )
                                        (white-triangle-infill (rectangle 9 5 'solid 'white))
                                        )
                                    (above white-triangle-infill
                                           (overlay/xy inner-white
                                                       -2
                                                       0
                                                       outer-black
                                                       )
                                           )
                                    )
                                  )
                     (cursor-tri (let
                                     (
                                      (inner-white (overlay/align/offset "right"
                                                                         "middle"
                                                                         (rotate 250
                                                                                 (overlay/align/offset "middle"
                                                                                                       "bottom"
                                                                                                       (triangle/aas 30
                                                                                                                     30
                                                                                                                     44
                                                                                                                     'solid
                                                                                                                     'white
                                                                                                                     )
                                                                                                       0
                                                                                                       3
                                                                                                       (triangle/aas 30
                                                                                                                     30
                                                                                                                     48
                                                                                                                     'solid
                                                                                                                     'black
                                                                                                                     )
                                                                                                       )
                                                                                 )
                                                                         -2
                                                                         -1
                                                                         (triangle/aas 38.94
                                                                                       70.54
                                                                                       74
                                                                                       'solid
                                                                                       'white
                                                                                       )
                                                                         )
                                                   )
                                      (outer-black (overlay/align/offset "right"
                                                                         "middle"
                                                                         (rotate 250
                                                                                 (triangle/aas 30
                                                                                               30
                                                                                               60
                                                                                               'solid
                                                                                               'white
                                                                                               )
                                                                                 )
                                                                         -1
                                                                         -1
                                                                         (triangle/sss 60
                                                                                       90
                                                                                       90
                                                                                       'solid
                                                                                       'black
                                                                                       )
                                                                         )
                                                   )
                                      )
                                   (scale 0.5
                                          (rotate 310
                                                  (overlay/xy inner-white
                                                              -9
                                                              -3
                                                              outer-black
                                                              )
                                                  )
                                          )
                                   )
                          
                                 )
                     )
                 (overlay/xy (rotate 25 cursor-rect)
                             -7
                             -26
                             cursor-tri
                             )
                 )
  )


(define E-SCENE-TOOLS
  (let (
        (ARROW (above (triangle 30 'solid 'black)
                      (rectangle 10 30 'solid 'black))
               )
        )
    (beside/align "bottom"
                  (above ARROW-UP-KEY
                         (square 20 'solid 'white)
                         (text "Restart" 18 'black))
                  (square 40 'solid 'white)
                  (above ARROW-RIGHT-KEY
                         (square 20 'solid 'white)
                         (text "Forward" 18 'black))
                  (square 40 'solid 'white)
                  (above ARROW-LEFT-KEY
                         (square 20 'solid 'white)
                         (text "Backward" 18 'black))
                  (square 40 'solid 'white)
                  (above ARROW-DOWN-KEY
                         (square 20 'solid 'white)
                         (text "Finish" 18 'black))
                  (square 40 'solid 'white)
                  (above cursor
                         (square 20 'solid 'white)
                         (text "Hold to drag" 18 'black))
                  (square 40 'solid 'white)
                  (beside (above/align "middle" W-KEY (square 20 'solid 'white) (text "Zoom in" 18 'black))
                          (square 20 'solid 'white)
                          (above/align "middle"  S-KEY (square 20 'solid 'white) (text "Zoom out" 18 'black))
                          (square 20 'solid 'white)
                          (above/align "middle" R-KEY (square 20 'solid 'white) (text "Min zoom" 18 'black))
                          (square 20 'solid 'white)
                          (above/align "middle" E-KEY (square 20 'solid 'white) (text "Mid zoom" 18 'black))
                          (square 20 'solid 'white)
                          (above/align "middle" F-KEY (square 20 'solid 'white) (text "Max zoom" 18 'black))
                          )
                  )
    )
  )

(define FONT-SIZE 20)


(struct rule-yield-dims (min-x max-x min-y max-y))



(define RULE-YIELD-DIMS (let [
                              (DREV (text "Deriving: " FONT-SIZE 'black))
                              (YIELD (text "Current Yield: " FONT-SIZE 'black))
                              (RULE-USED (text "The rule used: " FONT-SIZE 'black))
                              ]
                          (rule-yield-dims (max (image-width DREV)
                                                (image-width YIELD)
                                                (image-width RULE-USED)
                                                )
                                           (* E-SCENE-WIDTH 0.9)
                                           (+ E-SCENE-HEIGHT (image-height RULE-USED))
                                           (+ E-SCENE-HEIGHT (image-height (above RULE-USED DREV YIELD)))
                                           )
                          )
  )

(define (calculate-word-offset-cap derive-word-img scrolling-dims) (if (> 0 (- (- E-SCENE-WIDTH (rule-yield-dims-min-x scrolling-dims)) (image-width derive-word-img)))
                                                                       0
                                                                       (- (image-width derive-word-img) (- E-SCENE-WIDTH (rule-yield-dims-min-x scrolling-dims)))
                                                                       )
  )

#;(define (create-input-word img char-list) (if (and (> (* 0.9 E-SCENE-WIDTH) (image-width img))
                                                     (not (empty? char-list))
                                                     )
                                                (create-input-word (beside img (text (string-append (symbol->string (first char-list)) " ") FONT-SIZE 'black)) (rest char-list))
                                                img
                                                )
    )


#|
(define (create-yield-word yield htable) (let*
                                             [
                                              (char-list (if (list? yield)
                                                             (map symbol->string yield)
                                                             (list (symbol->string yield))
                                                             )
                                                         )
                                              (width-of-img (calculate-img-width char-list htable))
                                              ]
                                           (if (> (+ width-of-img (image-width DREV)) (* E-SCENE-WIDTH 0.9))
                                               (
                                               (create-yield-word-helper (text "" FONT-SIZE 'black) char-list)
                                               )
                                           )
  )
|#

#;(define (create-char-widths input-word) (let [
                                                (input-word-dups-removed (remove-duplicates input-word))
                                                ]
                                            (foldl (lambda (word-char htable) (hash-set htable word-char (image-width (text (symbol->string word-char) FONT-SIZE 'black)))) (hash) input-word-dups-removed)
                                            )
    )

#;(define (calculate-img-width input-word htable) (foldl (lambda (word-char accum) (+ accum (hash-ref htable word-char))) 0 input-word))

;; This needs to be based on size of e-scene
(define TAPE-SIZE 42)
(define (make-tape-img tape start-index)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        #;(above (first loi)
                 (square 5 'solid 'white)
                 (text (number->string start-index) 10 'black))
        (beside (first loi)
                #;(above (first loi)
                         (square 5 'solid 'white)
                         (text (number->string start-index) 10 'black))
                (make-tape-img (rest loi) (add1 start-index)))
        )
    )
  (let [(letter-imgs (build-list TAPE-SIZE
                                 (λ (i) (if (< (+ start-index i) (length tape))
                                            (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                           24
                                                           'black)
                                                     (overlay (square 25 'solid 'white)
                                                              (square (add1 25) 'solid 'white)))
                                            (overlay (square 25 'solid 'white)
                                                     (square (add1 25) 'solid 'white))))))
        ]
    (make-tape-img letter-imgs start-index)
    )
  )

(define (create-instructions a-vs) (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                           (local [
                                                   (define DREV (text "Deriving: " FONT-SIZE 'black))
                                                   (define YIELD (text "Current Yield: " FONT-SIZE 'black))
                                                   #;(define (create-char-widths input-word) (let [
                                                                                                   (input-word-dups-removed (remove-duplicates input-word))
                                                                                                   ]
                                                                                               (foldl (lambda (word-char htable) (hash-set htable word-char (image-width (text (symbol->string word-char) FONT-SIZE 'black)))) (hash) input-word-dups-removed)
                                                                                               )
                                                       )
                                                   ;; needs to include both terminals AND non terminals it seems
                                                   #;(define char-widths-htable (hash-set (create-char-widths (viz-state-input-word a-vs)) 'S (image-width (text "S" FONT-SIZE 'black))))
                                                   #;(define (calculate-img-width input-word) (foldl (lambda (word-char accum) (+ accum (hash-ref char-widths-htable word-char))) 0 input-word))
                                                   #;(define (create-yield-word-helper img char-list) (if (empty? char-list)
                                                                                                          img
                                                                                                          (create-yield-word-helper (beside img (text (string-append (first char-list) " ") FONT-SIZE 'black)) (rest char-list))
                                                                                                          )
                                                       )
                                                   #;(define (create-yield-word yield) (let*
                                                                                           [
                                                                                            (char-list (if (list? yield)
                                                                                                           (map symbol->string yield)
                                                                                                           (list yield)
                                                                                                           )
                                                                                                       )
                                                                                            (width-of-img (calculate-img-width char-list))
                                                                                            ]
                                                                                         (if (> (+ width-of-img (image-width DREV)) (* E-SCENE-WIDTH 0.9))
                                                                                             ;; some crop image shit here
                                                                                             (crop (viz-state-word-img-offset a-vs) 0 (* E-SCENE-WIDTH 0.9) (image-height YIELD-WORD) YIELD-WORD)
                                                                                             (create-yield-word-helper (text "" FONT-SIZE 'black) char-list)
                                                                                             )
                                                                                         )
                                                       )
                                                   #;(define (create-input-word input-word) (let* [
                                                                                                   (width-of-img (calculate-img-width input-word))
                                                                                                   ]
                                                                                              (if (< (* E-SCENE-WIDTH 0.9) (+ width-of-img (image-width DREV)))
                                                                                                  (crop (viz-state-word-img-offset a-vs) 0 (* E-SCENE-WIDTH 0.9) (image-height INPUT-WORD) INPUT-WORD)
                                                                                                  INPUT-WORD
                                                                                                  )
                                                                                              )
                                                       )
                                                   (define INPUT-WORD (make-tape-img (viz-state-input-word a-vs) (if (length (viz-state-input-word a-vs))
                                                                                                                     (viz-state-word-img-offset a-vs)
                                                                                                                     0
                                                                                                                     )
                                                                                     )
                                                     #;(create-input-word (text "(" FONT-SIZE 'black) (viz-state-input-word a-vs))
                                                     )
                                                                                    
                                                  
                                                   ;; Need to calculate how lacfge the image is going to be before its made, probablematic for sure
                                                   #;(YIELD-WORD 
                                                      (text (format "~a" (first (viz-state-p-yield a-vs))) FONT-SIZE 'black)
                                                      )
                                                   (define YIELD-WORD (let [
                                                                            (normalized-p-yield (if (list? (first (viz-state-p-yield a-vs)))
                                                                                                    (first (viz-state-p-yield a-vs))
                                                                                                    (list (first (viz-state-p-yield a-vs)))
                                                                                                    )
                                                                                                )
                                                                            ]
                                                                        (make-tape-img normalized-p-yield (if (> (length normalized-p-yield) TAPE-SIZE)
                                                                                                              (viz-state-word-img-offset a-vs)
                                                                                                              0
                                                                                                              )
                                                                                       )
                                                                        )
                                                     #;(create-yield-word (first (viz-state-p-yield a-vs))
                                                                          )
                                                     )
                                                  
                                                  
                                                   #;(CROPPED-YIELD-WORD (if (or (boolean? YIELD-WORD)
                                                                                 (< (* E-SCENE-WIDTH 0.9) (+ (image-width YIELD-WORD) (image-width YIELD)))
                                                                                 )
                                                                             ;; Can't just crop, need to 
                                                                             (crop (viz-state-word-img-offset a-vs) 0 (* E-SCENE-WIDTH 0.9) (image-height YIELD-WORD) YIELD-WORD)
                                                                             YIELD-WORD
                                                                             )
                                                                         )
                                                   #;(YIELD-IMG (beside YIELD CROPPED-YIELD-WORD))

                                                  
                                                  
                                                   #;(INPUT-WORD (with-handlers ([exn:fail? (lambda (exn) #f)])
                                                                   (text (format "~a" (viz-state-input-word a-vs)) FONT-SIZE 'black)
                                                                   )
                                                                 )
                                                  
                                                   ;; TODO test this to see if first p-yield is the full thing or just one character
                                                   ;; its just one character
                                                   #;(test (begin
                                                             (displayln (viz-state-input-word a-vs))
                                                             1
                                                             )
                                                           )
                                                   #;(CROPPED-INPUT-WORD (if (or (boolean? INPUT-WORD)
                                                                                 (< (* E-SCENE-WIDTH 0.9) (+ (image-width INPUT-WORD) (image-width DREV)))
                                                                                 )
                                                                             (crop (viz-state-word-img-offset a-vs) 0 (* E-SCENE-WIDTH 0.9) (image-height INPUT-WORD) INPUT-WORD)
                                                                             INPUT-WORD
                                                                             )
                                                                         )
                                                   #;(DREV-IMG (beside DREV CROPPED-INPUT-WORD))

                                                   (define RULE-USED (if (equal? "" (first (dgrph-p-rules (first (viz-state-p-dgraph a-vs)))))
                                                                         ;; Use white so its invisible, makes it so the words dont shift (using an empty string would make the words shift)
                                                                         (text "The rule used: " FONT-SIZE 'white)
                                                                         (text "The rule used: " FONT-SIZE 'black)
                                                                         )
                                                     )
                                                   (define RULE-USED-WORD (if (equal? "" (first (dgrph-p-rules (first (viz-state-p-dgraph a-vs)))))
                                                                              (text "" FONT-SIZE 'white)
                                                                              (beside (text (format "~a" (substring (first (dgrph-p-rules (first (viz-state-p-dgraph a-vs)))) 0 1)) FONT-SIZE YIELD-COLOR)
                                                                                      (text (format " ~a" (substring (first (dgrph-p-rules (first (viz-state-p-dgraph a-vs)))) 1)) FONT-SIZE HEDGE-COLOR)
                                                                                      )
                                                                              )
                                                     )
                                              
                                                   (define RULE-YIELD-DREV-LABELS (above/align "right" RULE-USED DREV YIELD))
                                                   (define WORDS (above/align "left" RULE-USED-WORD INPUT-WORD YIELD-WORD))
                                                   ]
                                             (beside RULE-YIELD-DREV-LABELS WORDS)
                                             )
                                        
                                           )
  )

(define (create-instructions-and-tools a-vs) (above (create-instructions a-vs) (square 30 'solid 'white) E-SCENE-TOOLS))




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

;; list-intersect?
;; (listof symbol) (listof symbol) -> Boolean
;; Purpose: To check if two lists have the same element in them
(define (list-intersect? los1 los2)
  (ormap (λ (symbol) (member symbol los2)) los1))

;; rename-levels
;; (listof level) -> (listof level)
;; Purpose: To rename the terminals and nonterminals that reoccur in extracted edges






#;(define (rename-levels exe)
  (define (rename-level level accum)
    (if (empty? level)
        empty
        (let* [(dd (display (format "~s" new-edge)))]
          (cons new-edge
                (rename-level (rest level)(append new-edge accum))))))
  (define (rnm-lvls exe accum)
    (if (empty? exe)
        '()
        (let* [(new-level (rename-level (first exe) accum))
               (new-accum (append (flatten new-level) accum))]
          (cons new-level
                (rnm-lvls (rest exe) new-accum)))))
  (if (empty? exe)
      empty
      (rnm-lvls exe (first exe))))


      
;; generate-level
;; (listof symbol) (listof symbol) -> level
;; Purpose: To generate levels for intersect lists
(define (generate-level los1 los2)
  (let* [(leftmost (takef los2 lower?))
         (rightmost (take-right los2 (- (length los1) 1)))
         (nonterminal (if (empty? (drop los2 (length leftmost)))
                          empty
                          (first (drop los2 (length leftmost)))))
         (new (if (empty? (drop-right los2 (length rightmost)))
                  (list 'ε)
                  (drop (drop-right los2 (length rightmost)) (length leftmost))))]
    (for*/list ([i (list nonterminal)]
                [j new])
      (list i j))))


;; create-levels
;; derivation-list -> (listof level)
;; To generate a list of levels from wder
(define (create-levels wd)
  (if (= (length wd) 1)
      empty
      (cons (generate-level (first wd) (second wd)) (create-levels (rest wd)))))


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


;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

#|
;; extract-nodes-by-lvl
;; level (listof node) -> (listof node)
;; Purpose: To extract nodes from the lon that are in the level
(define (extract-nodes-by-lvl lon level)
  (let* [(nil (flatten level))]
    (filter (λ (node) (member node nil)) lon)))
|#

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
  (let [(first-foldr (foldl (λ (rule result)
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
             (if (= 1 (length rule))
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



;; create-graph-img
;; ndfa -> img
;; Purpose: To create a graph image for complement
(define (create-graph-img a-dgrph)
  (let* [
         (nodes (dgrph-nodes a-dgrph))
         (levels (reverse (map reverse (dgrph-ad-levels a-dgrph))))
         (hedges (dgrph-hedges a-dgrph))
         (hedge-nodes (map (λ (x) (if (empty? x)
                                      empty
                                      (second x))) hedges)
                      )
         (yield-node (map (λ (x) (if (empty? x)
                                     empty
                                     (first x))) hedges))
         ]
    (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                      nodes hedge-nodes yield-node)
                     levels hedges)
    )
  )



(define TEST-SAVE-DIR "/Users/tijanaminic/Documents/Andres stuff/")
;(define SCRIPT-LOCATION "/home/sora/Documents/demo-folder/create-image-p.sh")

(define (while-func cond thnk) (if (cond)
                                   (thnk)
                                   '()
                                   )
  )

(define (collect-images accum cap)
  (if (> accum cap)
      '()
      (cons (thunk (bitmap/file (string->path (format "~adot~s.png" TEST-SAVE-DIR accum))))
            (collect-images (add1 accum) cap)
            )
      )
  )

(define (parallelize-shell func args) (let* [
                                             (system-os (system-type 'os))
                                             (cpu-cores (cond [(eq? system-os 'unix) (begin
                                                                                       (define p (process "grep -c '^processor' /proc/cpuinfo"))
                                                                                       (define event-result (string->number (sync (read-line-evt (first p)))))
                                                                                       event-result
                                                                                       )
                                                                                     ]
                                                              [(eq? system-os 'windows) ]
                                                              [(eq? system-os 'macos) (begin
                                                                                        (define p (process "sysctl -n hw.ncpu"))
                                                                                        (define event-result (string->number (sync (read-line-evt (first p)))))
                                                                                        event-result
                                                                                        )]
                                                              [(eq? system-os 'macosx) (begin
                                                                                         (define p (process "sysctl -n hw.ncpu"))
                                                                                         (define event-result (string->number (sync (read-line-evt (first p)))))
                                                                                         event-result
                                                                                         )]
                                                              )
                                                        )
                                             (cpu-cores-avail (make-semaphore cpu-cores))
                                             ]
                                        ;(map (lambda (args) (call-with-semaphore cpu-cores-avail func args)) args)
                                        (for/list ([a args])
                                          (semaphore-wait cpu-cores-avail)
                                          (define shell-process (func a))
                                          (thread (lambda () (let [
                                                                   (result (sync (read-line-evt (first shell-process))))
                                                                   ]
                                                               (if (= 0 (string->number result))
                                                                   ;; This is thrown away, just doing this for the error check
                                                                   result
                                                                   (error (format "Graphviz produced an error while compiling the graphs: ~a" result))
                                                                   )
                                                               )
                                                    (close-input-port (first shell-process))
                                                    (close-output-port (second shell-process))
                                                    (close-input-port (fourth shell-process))
                                                    (semaphore-post cpu-cores-avail)
                                                    )
                                                  )
                                          )
                                        )
  )

(define (parallel-dot graphs) (let* [
                                     (graphs-length (length graphs))
                                     (list-dot-files (for/list ([i (range 1 (add1 graphs-length))])
                                                       (format "~adot~s" TEST-SAVE-DIR i)
                                                       )
                                                     )
                                     ]
                                (begin
                                  ;; need to foldl here
                                  (displayln "Regular time: ")
                                  (time (foldl (lambda (value accum) (begin (graph->dot value (string->path TEST-SAVE-DIR) (format "dot~s" accum))
                                                                            (add1 accum)
                                                                            )
                                                 )
                                               1
                                               graphs
                                               )
                                        )
                                  (displayln "New time: ")
                                  (time (foldl (lambda (value accum) (begin (graph->dot value (string->path TEST-SAVE-DIR) (format "dot~s" accum))
                                                                            (add1 accum)
                                                                            )
                                                 )
                                               1
                                               graphs
                                               )
                                        )
                                  ;; Need to choose a different method of echo for windows machines
                                  #;(define processes (map (lambda (dot-path) (process (format "~a -T~s ~s -o ~s; echo $?"
                                                                                               ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
                                                                                               ;; using the absolute path to the executable. For unknown reasons this does not
                                                                                               ;; work on Windows so we will still use the PATH to call the dot executable
                                                                                               "/run/current-system/sw/bin/dot"
                                                                                               'png
                                                                                               (string-append dot-path ".dot")
                                                                                               (string-append dot-path ".png")
                                                                                               )
                                                                                       )
                                                             )
                                                           list-dot-files)
                                      )

                                  ;;cfg, leftmost traversal is preorder, rightmost is a root right left, level traversal is just a breadth first search
                                  #;(map (lambda (event) (let [
                                                               (result (sync event))
                                                               ]
                                                           (if (= 0 (string->number result))
                                                               result
                                                               (error (format "Graphviz produced an error while compiling the graphs: ~a" result))
                                                               )
                                                           )
                                           )
                                         (map (lambda (process) (read-line-evt (first process))) processes)
                                         )
                                  ;(while-func (lambda () (andmap (lambda (process) (eq? 'done-ok ((last process) 'status))) processes)) (lambda () (sleep 0.5)))
                                  ;(sync 
                                   
                                  #;(map (lambda (process) (begin (close-input-port (first process))
                                                                  (close-output-port (second process))
                                                                  (close-input-port (fourth process))
                                                                  )
                                           )
                                         processes
                                         )

                                  (define (make-process file-path) (process (format "~a -T~s ~s -o ~s; echo $?"
                                                                                    ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
                                                                                    ;; using the absolute path to the executable. For unknown reasons this does not
                                                                                    ;; work on Windows so we will still use the PATH to call the dot executable
                                                                                    "/opt/homebrew/bin/dot"
                                                                                    'png
                                                                                    (string-append file-path ".dot")
                                                                                    (string-append file-path ".png")
                                                                                    )
                                                                            )
                                    )
                                  (parallelize-shell make-process list-dot-files)
                                  (collect-images 1 graphs-length)
                                  )
                                )
  )

;; create-graph-imgs
;; (listof dgraph) -> (listof image)
;; Purpose: To create a list of graph images built level by level
(define (create-graph-imgs lod)
  (if (empty? lod)
      '()
      (parallel-dot (map create-graph-img lod))
      )
  )

; resize-image :: image -> int -> int -> image
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
         (list (scale/xy
                (/ max-width src-width)
                (/ (/ scaled-width aspect) src-height)
                img)
               (/ max-width src-width)
               (/ (/ scaled-width aspect) src-height)
               )
         ]
        [(and (<= scaled-width max-width)
              (> scaled-height max-height)
              )
         (let ([scaled-aspect (/ scaled-width scaled-height)])
           (list (scale/xy
                  (/ (* scaled-height scaled-aspect) src-width)
                  (/ max-height src-height)
                  img)
                 (/ (* scaled-height scaled-aspect) src-width)
                 (/ max-height src-height)
                 )
           )
         ]
        [(and (> scaled-width max-width)
              (> scaled-height max-height)
              )
         (let* (
                [new-scaled-height (/ max-width aspect)]
                [scaled-aspect (/ max-width new-scaled-height)]
                )
           (list (scale/xy
                  (/ (* max-height scaled-aspect) src-width)
                  (/ max-height src-height)
                  img)
                 (/ (* max-height scaled-aspect) src-width)
                 (/ max-height src-height)
                 )
           )
         ]
        [(and (<= scaled-width max-width)
              (<= scaled-height max-height)
              )
         (list (scale/xy
                (/ scaled-width src-width)
                (/ scaled-height src-height)
                img)
               (/ scaled-width src-width)
               (/ scaled-height src-height)
               )
         ]
        )
  )

(struct viewport-limits (min-x max-x min-y max-y))

;; img num>0 -> viewport-limits
;; Calculates the min and max values of x and y that keep the graph on the screen at all times
(define (calculate-viewport-limits scaled-image scale) (let* [
                                                              (img-width-node-diff (- (/ (image-width scaled-image) 2) (* NODE-SIZE scale)))
                                                              (img-height-node-diff (- (/ (image-height scaled-image) 2) (* NODE-SIZE scale)))
                                                              (scaled-node-size (* NODE-SIZE scale))
                                                              (MIN-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                                                                         (- (* -1 (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH)) (- E-SCENE-WIDTH scaled-node-size) )
                                                                         (* -1 img-width-node-diff)
                                                                         )
                                                                     )
                                                              (MAX-X (if (< E-SCENE-WIDTH (/ (image-width scaled-image) 2))
                                                                         (+ (- (/ (image-width scaled-image) 2) E-SCENE-WIDTH) E-SCENE-WIDTH (- E-SCENE-WIDTH scaled-node-size) )
                                                                         (+ E-SCENE-WIDTH img-width-node-diff)
                                                                         )
                                                                     )
                                                               
                                                              (MIN-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                                                                         (- (* -1 (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT)) (- E-SCENE-HEIGHT scaled-node-size))
                                                                         (* -1 img-height-node-diff)
                                                                         )
                                                                     )
                                                              (MAX-Y (if (< E-SCENE-HEIGHT (/ (image-height scaled-image) 2))
                                                                         (+ (- (/ (image-height scaled-image) 2) E-SCENE-HEIGHT) E-SCENE-HEIGHT (- E-SCENE-HEIGHT scaled-node-size))
                                                                         (+ E-SCENE-HEIGHT img-height-node-diff)
                                                                         )
                                                                     )
                                                              ]
                                                         (viewport-limits MIN-X MAX-X MIN-Y MAX-Y)
                                                         )
  )

;; viz-state viewport-limits img num>0 -> viz-state
;; Returns a new viz-state where if the given image would be out of bounds of its viewport limits
;; It is placed into a position inbounds
(define (reposition-out-of-bounds-img a-vs viewport-lims new-img new-scale)
  (let
      (
       (MIN-X (viewport-limits-min-x viewport-lims))
       (MAX-X (viewport-limits-max-x viewport-lims))
       (MIN-Y (viewport-limits-min-y viewport-lims))
       (MAX-Y (viewport-limits-max-y viewport-lims))
       )
    (cond [(and (> MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MIN-X (posn-y (viz-state-image-posn a-vs)))
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )                           
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (> (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MAX-X (posn-y (viz-state-image-posn a-vs)))
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )                   
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (> MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn (posn-x (viz-state-image-posn a-vs)) MIN-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (> (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn (posn-x (viz-state-image-posn a-vs)) MAX-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          [(and (> MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (> MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MIN-X MIN-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          [(and (> MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (> (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MIN-X MAX-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (> (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (> MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MAX-X MIN-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (> (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (> (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (posn MAX-X MAX-Y)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          [(and (<= MIN-X (posn-x (viz-state-image-posn a-vs)))
                (<= (posn-x (viz-state-image-posn a-vs)) MAX-X)
                (<= MIN-Y (posn-y (viz-state-image-posn a-vs)))
                (<= (posn-y (viz-state-image-posn a-vs)) MAX-Y)
                )
           (viz-state (viz-state-upimgs a-vs)
                      (viz-state-pimgs a-vs)
                      new-img
                      (viz-state-image-posn a-vs)
                      new-scale
                      (viz-state-scale-factor-cap a-vs)
                      (viz-state-scale-factor-floor a-vs)
                      (viz-state-curr-mouse-posn a-vs)
                      (viz-state-dest-mouse-posn a-vs)
                      (viz-state-mouse-pressed a-vs)
                      (viz-state-up-dgraph a-vs)
                      (viz-state-p-dgraph a-vs)
                      (viz-state-up-yield a-vs)
                      (viz-state-p-yield a-vs)
                      (viz-state-input-word a-vs)
                      (viz-state-word-img-offset a-vs)
                      (viz-state-word-img-offset-cap a-vs)
                      )
           ]
          )
    )
  )

;; num num num num boolean num num num (matrix [ [x] [y] [1] ]) -> (matrix [ [transformed-x] [transformed-y] [1] ])
;; Transforms a given a point matrix based on the arguments provided
(define (affine-transform #:x-translate [x-translate 0]
                          #:y-translate [y-translate 0]
                          #:x-scale [x-scale 1]
                          #:y-scale [y-scale 1]
                          #:reflect [reflect #f]
                          #:rotate [rotate 0]
                          #:x-shear [x-shear 0]
                          #:y-shear [y-shear 0]
                          #:point point)
  (let* [
         (reflection (if reflect
                         -1
                         1
                         )
                     )
         (result (matrix* (matrix [
                                   [(* reflection x-scale (cos rotate)) (* x-shear (* -1 (sin rotate))) x-translate ]
                                   [(* (sin rotate) y-shear) (* y-scale (cos rotate)) y-translate]
                                   [0 0 1]
                                   ]
                                  )
                          point
                          )
                 )
         ]
    result
    )
  )

;; img posn num>0 -> matrix x y 1
;; Calculates the transform needed to zoom correctly
(define (zoom-affine-transform img img-posn scale) (let* [
                                                          (transformed-x (* -1 (+ (- (/ E-SCENE-WIDTH 2)
                                                                                     (+ (posn-x img-posn) (/ (image-width img) 2))
                                                                                     )
                                                                                  (/ (image-width img) 2)
                                                                                  )
                                                                            )
                                                                         )
                                                          (transformed-y (* -1 (+ (- (/ E-SCENE-HEIGHT 2)
                                                                                     (+ (posn-y img-posn) (/ (image-height img) 2))
                                                                                     )
                                                                                  (/ (image-height img) 2)
                                                                                  )
                                                                            )
                                                                         )
                                                          ]
                                                     (affine-transform #:x-translate (* -1 transformed-x)
                                                                       #:y-translate (* -1 transformed-y)
                                                                       #:point (affine-transform #:x-scale scale
                                                                                                 #:y-scale scale
                                                                                                 #:point (affine-transform #:x-translate transformed-x
                                                                                                                           #:y-translate transformed-y
                                                                                                                           #:point (matrix [ [0] [0] [1] ])
                                                                                                                           )
                                                                                                 )
                                                                       )
                                                     )
  )

;; viz-state real>0 -> viz-state
;; Returns a a viz-state where zoomed in onto the current graph being displayed
(define (zoom a-vs factor) (let*  [
                                   (new-scale (* factor (viz-state-scale-factor a-vs)))
                                   (scalable? (cond [(eq? factor ZOOM-INCREASE) (> (viz-state-scale-factor-cap a-vs) new-scale)]
                                                    [(eq? factor ZOOM-DECREASE) (< (viz-state-scale-factor-floor a-vs) new-scale)]
                                                    )
                                              )
                                   ]
                             (if scalable?
                                 (let* [
                                        (scaled-image (scale new-scale (viz-state-curr-image a-vs)))
                                        (viewport-lims (calculate-viewport-limits scaled-image new-scale))
                                        (scale-increase (/ new-scale (viz-state-scale-factor a-vs)))
                                        (affine-matrix (zoom-affine-transform (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)) (viz-state-image-posn a-vs) scale-increase))             
                                        ]
                                   (reposition-out-of-bounds-img (viz-state (viz-state-upimgs a-vs)
                                                                            (viz-state-pimgs a-vs)
                                                                            (viz-state-curr-image a-vs)
                                                                            (posn (+ (posn-x (viz-state-image-posn a-vs)) (matrix-ref affine-matrix 0 0)) (+ (posn-y (viz-state-image-posn a-vs)) (matrix-ref affine-matrix 1 0)))
                                                                            new-scale
                                                                            (viz-state-scale-factor-cap a-vs)
                                                                            (viz-state-scale-factor-floor a-vs)
                                                                            (viz-state-curr-mouse-posn a-vs)
                                                                            (viz-state-dest-mouse-posn a-vs)
                                                                            (viz-state-mouse-pressed a-vs)
                                                                            (viz-state-up-dgraph a-vs)
                                                                            (viz-state-p-dgraph a-vs)
                                                                            (viz-state-up-yield a-vs)
                                                                            (viz-state-p-yield a-vs)
                                                                            (viz-state-input-word a-vs)
                                                                            (viz-state-word-img-offset a-vs)
                                                                            (viz-state-word-img-offset-cap a-vs)
                                                                            )
                                                                 viewport-lims
                                                                 (viz-state-curr-image a-vs)
                                                                 new-scale
                                                                 )
                                   )
                                 a-vs
                                 )
             
                             )
  )

;; process-key
;; viz-state key --> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (let* [
                    
                    (new-up-yield (if (empty? (viz-state-up-yield a-vs))
                                      '()
                                      (rest (viz-state-up-yield a-vs))))
                    (new-p-yield (if (empty? (viz-state-up-yield a-vs))
                                     (viz-state-p-yield a-vs)
                                     (cons (first (viz-state-up-yield a-vs))
                                           (viz-state-p-yield a-vs))))
                    (new-pimgs (cons (first (viz-state-upimgs a-vs))
                                     (viz-state-pimgs a-vs)))
                          
                    (new-pimgs-img (
                                    (first new-pimgs)
                                    )
                                   )
                    
                    (curr-pimgs-img (
                                     (first (viz-state-pimgs a-vs))
                                     )
                                    )
                    (new-p-dgraph (cons (first (viz-state-up-dgraph a-vs))
                                        (viz-state-p-dgraph a-vs)))
                    (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
                    
                    (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                                 (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                                 )
                              )
                    
                    (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                                 (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                                 )
                              )
                    
                    (new-offset-cap (calculate-word-offset-cap (text (format "~a" (first (viz-state-input-word a-vs))) FONT-SIZE 'black) RULE-YIELD-DIMS))
                    ]
               (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                       (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                       )
                   (let
                       [
                        (NEW-FLOOR (min (second img-resize) (third img-resize)))
                        ]
                     (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                            (let [
                                  (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                            new-pimgs
                                                            new-pimgs-img     
                                                            (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                            DEFAULT-ZOOM-CAP
                                                            DEFAULT-ZOOM-CAP
                                                            NEW-FLOOR
                                                            (viz-state-curr-mouse-posn a-vs)
                                                            (viz-state-dest-mouse-posn a-vs)
                                                            (viz-state-mouse-pressed a-vs)
                                                            (rest (viz-state-up-dgraph a-vs))
                                                            new-p-dgraph
                                                            new-up-yield
                                                            new-p-yield
                                                            (viz-state-input-word a-vs)
                                                            (viz-state-word-img-offset a-vs)
                                                            (viz-state-word-img-offset-cap a-vs)
                                                            )
                                                 )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                            (let [
                                  (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                            new-pimgs
                                                            new-pimgs-img 
                                                            (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                            NEW-FLOOR
                                                            DEFAULT-ZOOM-CAP
                                                            NEW-FLOOR
                                                            (viz-state-curr-mouse-posn a-vs)
                                                            (viz-state-dest-mouse-posn a-vs)
                                                            (viz-state-mouse-pressed a-vs)
                                                            (rest (viz-state-up-dgraph a-vs))
                                                            new-p-dgraph
                                                            new-up-yield
                                                            new-p-yield
                                                            (viz-state-input-word a-vs)
                                                            (viz-state-word-img-offset a-vs)
                                                            (viz-state-word-img-offset-cap a-vs)
                                                            )
                                                 )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           [else
                            (let [
                                  (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                            new-pimgs
                                                            new-pimgs-img
                                                            (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                            (viz-state-scale-factor a-vs)
                                                            DEFAULT-ZOOM-CAP
                                                            NEW-FLOOR
                                                            (viz-state-curr-mouse-posn a-vs)
                                                            (viz-state-dest-mouse-posn a-vs)
                                                            (viz-state-mouse-pressed a-vs)
                                                            (rest (viz-state-up-dgraph a-vs))
                                                            new-p-dgraph
                                                            new-up-yield
                                                            new-p-yield
                                                            (viz-state-input-word a-vs)
                                                            (viz-state-word-img-offset a-vs)
                                                            (viz-state-word-img-offset-cap a-vs)
                                                            )
                                                 )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           )
                     )
                   (let [
                         (new-viz-state (viz-state (rest (viz-state-upimgs a-vs))
                                                   new-pimgs
                                                   new-pimgs-img   
                                                   (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                   (viz-state-scale-factor a-vs)
                                                   DEFAULT-ZOOM-CAP
                                                   DEFAULT-ZOOM-FLOOR
                                                   (viz-state-curr-mouse-posn a-vs)
                                                   (viz-state-dest-mouse-posn a-vs)
                                                   (viz-state-mouse-pressed a-vs)
                                                   (rest (viz-state-up-dgraph a-vs))
                                                   new-p-dgraph
                                                   new-up-yield
                                                   new-p-yield
                                                   (viz-state-input-word a-vs)
                                                   (viz-state-word-img-offset a-vs)
                                                   (viz-state-word-img-offset-cap a-vs)
                                                   )
                                        )
                         ]
                     (reposition-out-of-bounds-img new-viz-state
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor a-vs)
                                                   )
                     )
                   )
               )
             )
         ]
        [(key=? "left" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (let* [(new-up-yield (cons (first (viz-state-p-yield a-vs))
                                        (viz-state-up-yield a-vs)))
                    (new-p-yield (rest (viz-state-p-yield a-vs)))
                    (new-pimgs (rest (viz-state-pimgs a-vs)))
                    (new-pimgs-img (
                                    (first new-pimgs)
                                    )
                                   )
                    (curr-pimgs-img (
                                     (first (viz-state-pimgs a-vs))
                                     )
                                    )
                    (new-p-dgraph (rest (viz-state-p-dgraph a-vs)))
                    (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
                    (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2) (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
                    (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2) (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)))
                    ]
               (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                       (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                       )
                   (let
                       [
                        (NEW-FLOOR (min (second img-resize) (third img-resize)))
                        ]
                     (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP)
                            (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                           (viz-state-upimgs a-vs))
                                                                     new-pimgs
                                                                     new-pimgs-img    
                                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                                     DEFAULT-ZOOM-CAP
                                                                     DEFAULT-ZOOM-CAP
                                                                     NEW-FLOOR
                                                                     (viz-state-curr-mouse-posn a-vs)
                                                                     (viz-state-dest-mouse-posn a-vs)
                                                                     (viz-state-mouse-pressed a-vs)
                                                                     (cons (first (viz-state-p-dgraph a-vs))
                                                                           (viz-state-up-dgraph a-vs))
                                                                     new-p-dgraph
                                                                     new-up-yield
                                                                     new-p-yield
                                                                     (viz-state-input-word a-vs)
                                                                     (viz-state-word-img-offset a-vs)
                                                                     (viz-state-word-img-offset-cap a-vs)
                                                                     )
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor a-vs)
                                                          )
                            ]
                           [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                            (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                           (viz-state-upimgs a-vs))
                                                                     new-pimgs
                                                                     new-pimgs-img     
                                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                                     NEW-FLOOR
                                                                     DEFAULT-ZOOM-CAP
                                                                     NEW-FLOOR
                                                                     (viz-state-curr-mouse-posn a-vs)
                                                                     (viz-state-dest-mouse-posn a-vs)
                                                                     (viz-state-mouse-pressed a-vs)
                                                                     (cons (first (viz-state-p-dgraph a-vs))
                                                                           (viz-state-up-dgraph a-vs))
                                                                     new-p-dgraph
                                                                     new-up-yield
                                                                     new-p-yield
                                                                     (viz-state-input-word a-vs)
                                                                     (viz-state-word-img-offset a-vs)
                                                                     (viz-state-word-img-offset-cap a-vs)
                                                                     )
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor a-vs)
                                                          )
                            ]
                           [else
                            (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                           (viz-state-upimgs a-vs))
                                                                     new-pimgs
                                                                     new-pimgs-img
                                                                     (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                                     (viz-state-scale-factor a-vs)
                                                                     DEFAULT-ZOOM-CAP
                                                                     NEW-FLOOR
                                                                     (viz-state-curr-mouse-posn a-vs)
                                                                     (viz-state-dest-mouse-posn a-vs)
                                                                     (viz-state-mouse-pressed a-vs)
                                                                     (cons (first (viz-state-p-dgraph a-vs))
                                                                           (viz-state-up-dgraph a-vs))
                                                                     new-p-dgraph
                                                                     new-up-yield
                                                                     new-p-yield
                                                                     (viz-state-input-word a-vs)
                                                                     (viz-state-word-img-offset a-vs)
                                                                     (viz-state-word-img-offset-cap a-vs)
                                                                     )
                                                          (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                          new-pimgs-img
                                                          (viz-state-scale-factor a-vs)
                                                          )
                            ]
                           )
                     )
                   (reposition-out-of-bounds-img (viz-state (cons (first (viz-state-pimgs a-vs))
                                                                  (viz-state-upimgs a-vs))
                                                            new-pimgs
                                                            new-pimgs-img      
                                                            (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                            (viz-state-scale-factor a-vs)
                                                            DEFAULT-ZOOM-CAP
                                                            DEFAULT-ZOOM-FLOOR
                                                            (viz-state-curr-mouse-posn a-vs)
                                                            (viz-state-dest-mouse-posn a-vs)
                                                            (viz-state-mouse-pressed a-vs)
                                                            (cons (first (viz-state-p-dgraph a-vs))
                                                                  (viz-state-up-dgraph a-vs))
                                                            new-p-dgraph
                                                            new-up-yield
                                                            new-p-yield
                                                            (viz-state-input-word a-vs)
                                                            (viz-state-word-img-offset a-vs)
                                                            (viz-state-word-img-offset-cap a-vs)
                                                            )
                                                 (calculate-viewport-limits (scale (viz-state-scale-factor a-vs) new-pimgs-img) (viz-state-scale-factor a-vs))
                                                 new-pimgs-img
                                                 (viz-state-scale-factor a-vs)
                                                 )
                   )
               )
             )
         ]
        [(key=? "down" a-key)
         (if (empty? (viz-state-upimgs a-vs))
             a-vs
             (let* [(new-p-yield (append (reverse (viz-state-up-yield a-vs))
                                         (viz-state-p-yield a-vs)))
                    (new-pimgs (append (reverse (viz-state-upimgs a-vs))
                                       (viz-state-pimgs a-vs)))
                    (new-pimgs-img (
                                    ;force
                                    (first new-pimgs)
                                    )
                                   )
                    (curr-pimgs-img (
                                     ;force
                                     (first (viz-state-pimgs a-vs))
                                     )
                                    )
                    (new-p-dgraph (append (reverse (viz-state-up-dgraph a-vs))
                                          (viz-state-p-dgraph a-vs)))
                    (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
                    (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                                 (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                                 )
                              )
                    (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                                 (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                                 )
                              )
                    ]
               (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                       (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                       )
                   (let
                       [
                        (NEW-FLOOR (min (second img-resize) (third img-resize)))
                        ]
                     (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                            (let [
                                  (new-viz-state  (viz-state '()
                                                             new-pimgs
                                                             new-pimgs-img
                                                             (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                             DEFAULT-ZOOM-CAP
                                                             DEFAULT-ZOOM-CAP
                                                             NEW-FLOOR
                                                             (viz-state-curr-mouse-posn a-vs)
                                                             (viz-state-dest-mouse-posn a-vs)
                                                             (viz-state-mouse-pressed a-vs)
                                                             '()
                                                             new-p-dgraph
                                                             '()
                                                             new-p-yield
                                                             (viz-state-input-word a-vs)
                                                             (viz-state-word-img-offset a-vs)
                                                             (viz-state-word-img-offset-cap a-vs)
                                                             )
                                                  )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                            (let [
                                  (new-viz-state  (viz-state '()
                                                             new-pimgs
                                                             new-pimgs-img
                                                             (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                             NEW-FLOOR
                                                             DEFAULT-ZOOM-CAP
                                                             NEW-FLOOR
                                                             (viz-state-curr-mouse-posn a-vs)
                                                             (viz-state-dest-mouse-posn a-vs)
                                                             (viz-state-mouse-pressed a-vs)
                                                             '()
                                                             new-p-dgraph
                                                             '()
                                                             new-p-yield
                                                             (viz-state-input-word a-vs)
                                                             (viz-state-word-img-offset a-vs)
                                                             (viz-state-word-img-offset-cap a-vs)
                                                             )
                                                  )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           [else (let [
                                       (new-viz-state  (viz-state '()
                                                                  new-pimgs
                                                                  new-pimgs-img
                                                                  (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                                  (viz-state-scale-factor a-vs)
                                                                  DEFAULT-ZOOM-CAP
                                                                  NEW-FLOOR
                                                                  (viz-state-curr-mouse-posn a-vs)
                                                                  (viz-state-dest-mouse-posn a-vs)
                                                                  (viz-state-mouse-pressed a-vs)
                                                                  '()
                                                                  new-p-dgraph
                                                                  '()
                                                                  new-p-yield
                                                                  (viz-state-input-word a-vs)
                                                                  (viz-state-word-img-offset a-vs)
                                                                  (viz-state-word-img-offset-cap a-vs)
                                                                  )
                                                       )
                                       ]
                                   (reposition-out-of-bounds-img new-viz-state
                                                                 (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                                 new-pimgs-img
                                                                 (viz-state-scale-factor new-viz-state)
                                                                 )
                                   )
                                 ]
                           )
                     )
                   (let [
                         (new-viz-state  (viz-state '()
                                                    new-pimgs
                                                    new-pimgs-img
                                                    (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                    (viz-state-scale-factor a-vs)
                                                    DEFAULT-ZOOM-CAP
                                                    DEFAULT-ZOOM-FLOOR
                                                    (viz-state-curr-mouse-posn a-vs)
                                                    (viz-state-dest-mouse-posn a-vs)
                                                    (viz-state-mouse-pressed a-vs)
                                                    '()
                                                    new-p-dgraph
                                                    '()
                                                    new-p-yield
                                                    (viz-state-input-word a-vs)
                                                    (viz-state-word-img-offset a-vs)
                                                    (viz-state-word-img-offset-cap a-vs)
                                                    )
                                         )
                         ]
                     (reposition-out-of-bounds-img new-viz-state
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor new-viz-state)
                                                   )
                     )
                   )
               )
             )
         ]
        [(key=? "up" a-key)
         (if (= (length (viz-state-pimgs a-vs)) 1)
             a-vs
             (let* [(new-up-yield (rest (append (reverse (viz-state-p-yield a-vs))
                                                (viz-state-up-yield a-vs))))
                    (new-p-yield (list (first (append (reverse (viz-state-p-yield a-vs))
                                                      (viz-state-up-yield a-vs)))))
                    (new-pimgs (list (first (append (reverse (viz-state-pimgs a-vs))
                                                    (viz-state-upimgs a-vs)))))
                    (new-pimgs-img (
                                    ;force
                                    (first new-pimgs)
                                    )
                                   )
                    (curr-pimgs-img (
                                     ;force
                                     (first (viz-state-pimgs a-vs))
                                     )
                                    )
                    (new-p-dgraph (list (first (append (reverse (viz-state-p-dgraph a-vs))
                                                       (viz-state-up-dgraph a-vs)))))
                    (img-resize (resize-image new-pimgs-img (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))

                    (growth-x (- (/ (image-width (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                                 (/ (image-width (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                                 )
                              )
                    (growth-y (- (/ (image-height (scale (viz-state-scale-factor a-vs) new-pimgs-img)) 2)
                                 (/ (image-height (scale (viz-state-scale-factor a-vs) curr-pimgs-img)) 2)
                                 )
                              )
                    ]
               (if (or (< E-SCENE-WIDTH (image-width new-pimgs-img))
                       (< E-SCENE-HEIGHT (image-height new-pimgs-img))
                       )
                   (let
                       [
                        (NEW-FLOOR (min (second img-resize) (third img-resize)))
                        ]
                     (cond [(> (viz-state-scale-factor a-vs) DEFAULT-ZOOM-CAP) 
                            (let [
                                  (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                           (viz-state-upimgs a-vs)))
                                                             new-pimgs
                                                             new-pimgs-img
                                                             (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                             DEFAULT-ZOOM-CAP
                                                             DEFAULT-ZOOM-CAP
                                                             NEW-FLOOR
                                                             (viz-state-curr-mouse-posn a-vs)
                                                             (viz-state-dest-mouse-posn a-vs)
                                                             (viz-state-mouse-pressed a-vs)
                                                             (rest (append (reverse (viz-state-p-dgraph a-vs))
                                                                           (viz-state-up-dgraph a-vs)))
                                                             new-p-dgraph
                                                             new-up-yield
                                                             new-p-yield
                                                             (viz-state-input-word a-vs)
                                                             (viz-state-word-img-offset a-vs)
                                                             (viz-state-word-img-offset-cap a-vs)
                                                             )
                                                  )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           [(< (viz-state-scale-factor a-vs) NEW-FLOOR)
                            (let [
                                  (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                           (viz-state-upimgs a-vs)))
                                                             new-pimgs
                                                             new-pimgs-img
                                                             (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                             NEW-FLOOR
                                                             DEFAULT-ZOOM-CAP
                                                             NEW-FLOOR
                                                             (viz-state-curr-mouse-posn a-vs)
                                                             (viz-state-dest-mouse-posn a-vs)
                                                             (viz-state-mouse-pressed a-vs)
                                                             (rest (append (reverse (viz-state-p-dgraph a-vs))
                                                                           (viz-state-up-dgraph a-vs)))
                                                             new-p-dgraph
                                                             new-up-yield
                                                             new-p-yield
                                                             (viz-state-input-word a-vs)
                                                             (viz-state-word-img-offset a-vs)
                                                             (viz-state-word-img-offset-cap a-vs)
                                                             )
                                                  )
                                  ]
                              (reposition-out-of-bounds-img new-viz-state
                                                            (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                            new-pimgs-img
                                                            (viz-state-scale-factor new-viz-state)
                                                            )
                              )
                            ]
                           [else (let [
                                       (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                                (viz-state-upimgs a-vs)))
                                                                  new-pimgs
                                                                  new-pimgs-img
                                                                  (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                                  (viz-state-scale-factor a-vs)
                                                                  DEFAULT-ZOOM-CAP
                                                                  NEW-FLOOR
                                                                  (viz-state-curr-mouse-posn a-vs)
                                                                  (viz-state-dest-mouse-posn a-vs)
                                                                  (viz-state-mouse-pressed a-vs)
                                                                  (rest (append (reverse (viz-state-p-dgraph a-vs))
                                                                                (viz-state-up-dgraph a-vs)))
                                                                  new-p-dgraph
                                                                  new-up-yield
                                                                  new-p-yield
                                                                  (viz-state-input-word a-vs)
                                                                  (viz-state-word-img-offset a-vs)
                                                                  (viz-state-word-img-offset-cap a-vs)
                                                                  )
                                                       )
                                       ]
                                   (reposition-out-of-bounds-img new-viz-state
                                                                 (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                                 new-pimgs-img
                                                                 (viz-state-scale-factor new-viz-state)
                                                                 )
                                   )
                                 ]
                           )
                     )
                   (let [
                         (new-viz-state  (viz-state (rest (append (reverse (viz-state-pimgs a-vs))
                                                                  (viz-state-upimgs a-vs)))
                                                    new-pimgs
                                                    new-pimgs-img
                                                    (posn (+ (posn-x (viz-state-image-posn a-vs)) growth-x) (+ (posn-y (viz-state-image-posn a-vs)) growth-y))
                                                    (viz-state-scale-factor a-vs)
                                                    DEFAULT-ZOOM-CAP
                                                    DEFAULT-ZOOM-FLOOR
                                                    (viz-state-curr-mouse-posn a-vs)
                                                    (viz-state-dest-mouse-posn a-vs)
                                                    (viz-state-mouse-pressed a-vs)
                                                    (rest (append (reverse (viz-state-p-dgraph a-vs))
                                                                  (viz-state-up-dgraph a-vs)))
                                                    new-p-dgraph
                                                    new-up-yield
                                                    new-p-yield
                                                    (viz-state-input-word a-vs)
                                                    (viz-state-word-img-offset a-vs)
                                                    (viz-state-word-img-offset-cap a-vs)
                                                    )
                                         )
                         ]
                     (reposition-out-of-bounds-img new-viz-state
                                                   (calculate-viewport-limits (scale (viz-state-scale-factor new-viz-state) new-pimgs-img) (viz-state-scale-factor new-viz-state))
                                                   new-pimgs-img
                                                   (viz-state-scale-factor new-viz-state)
                                                   )
                     )
                   )
               )
             )
         ]
        [(key=? "w" a-key) (zoom a-vs ZOOM-INCREASE)]
        [(key=? "s" a-key) (zoom a-vs ZOOM-DECREASE)]
        [(key=? "r" a-key) (let [
                                 (img-resize (resize-image (viz-state-curr-image a-vs) (* E-SCENE-WIDTH PERCENT-BORDER-GAP) (* E-SCENE-HEIGHT PERCENT-BORDER-GAP)))
                                 ]
                             (zoom a-vs (/ (min (second img-resize) (third img-resize)) (viz-state-scale-factor a-vs)))
                             )
                           ]
        [(key=? "f" a-key) (zoom a-vs (/ DEFAULT-ZOOM-CAP (viz-state-scale-factor a-vs)))]
        [(key=? "e" a-key) (zoom a-vs (/ (/ DEFAULT-ZOOM-CAP 2) (viz-state-scale-factor a-vs)))]
        [else a-vs]
        )
  )

;; viz-state int int MouseEvent
;; Updates viz-state as to whether the mouse is currently being pressed while on the visualization
(define (process-mouse a-vs x y mouse-event)
  (cond [(string=? mouse-event "button-down")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #t
                    (viz-state-up-dgraph a-vs)
                    (viz-state-p-dgraph a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    )
         ]
        [(string=? mouse-event "button-up")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #f
                    (viz-state-up-dgraph a-vs)
                    (viz-state-p-dgraph a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    )
         ]
        ;; Want to keep the mouse updating while it is being dragged
        [(string=? mouse-event "drag")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #t
                    (viz-state-up-dgraph a-vs)
                    (viz-state-p-dgraph a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    )
         ]
                                                   
        ;; Can happen in both clicked and unclicked states so leave it in whatever it was
        [(string=? mouse-event "move")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    (viz-state-mouse-pressed a-vs)
                    (viz-state-up-dgraph a-vs)
                    (viz-state-p-dgraph a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    )
         ]

        ;; This one is ambigious, think its better to leave as whatever it already was
        [(string=? mouse-event "enter")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    (viz-state-mouse-pressed a-vs)
                    (viz-state-up-dgraph a-vs)
                    (viz-state-p-dgraph a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    )
         ]

        ;; Stop updating if the mouse leaves the visualization screen
        [(string=? mouse-event "leave")
         (viz-state (viz-state-upimgs a-vs)
                    (viz-state-pimgs a-vs)
                    (viz-state-curr-image a-vs)
                    (viz-state-image-posn a-vs)
                    (viz-state-scale-factor a-vs)
                    (viz-state-scale-factor-cap a-vs)
                    (viz-state-scale-factor-floor a-vs)
                    (viz-state-curr-mouse-posn a-vs)
                    (posn x y)
                    #f
                    (viz-state-up-dgraph a-vs)
                    (viz-state-p-dgraph a-vs)
                    (viz-state-up-yield a-vs)
                    (viz-state-p-yield a-vs)
                    (viz-state-input-word a-vs)
                    (viz-state-word-img-offset a-vs)
                    (viz-state-word-img-offset-cap a-vs)
                    )
         ]
        [else a-vs]
        )
  )

;; viz-state
;; Updates the position of the image displayed based on the movement of the mouse
(define (process-tick a-vs)
  (let* [
         ;; Determines the movement of the mouse that occured since the last tick
         (x-diff (- (posn-x (viz-state-dest-mouse-posn a-vs)) (posn-x (viz-state-curr-mouse-posn a-vs))))
         (y-diff (- (posn-y (viz-state-dest-mouse-posn a-vs)) (posn-y (viz-state-curr-mouse-posn a-vs))))

         (new-img-x (+ (posn-x (viz-state-image-posn a-vs)) x-diff))
         (new-img-y (+ (posn-y (viz-state-image-posn a-vs)) y-diff))

         (tools-size (+ (image-height E-SCENE-TOOLS) 175))

         (scaled-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)))
         (viewport-lims (calculate-viewport-limits scaled-image (viz-state-scale-factor a-vs)))

         (MIN-X (viewport-limits-min-x viewport-lims))
         (MAX-X (viewport-limits-max-x viewport-lims))
         (MIN-Y (viewport-limits-min-y viewport-lims))
         (MAX-Y (viewport-limits-max-y viewport-lims))
         (scroll-dimensions RULE-YIELD-DIMS)
         ]
    (if (viz-state-mouse-pressed a-vs)
        (cond [(and (<= 0 (posn-x (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-x (viz-state-curr-mouse-posn a-vs)) E-SCENE-WIDTH)
                    (<= 0 (posn-y (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-y (viz-state-curr-mouse-posn a-vs)) E-SCENE-HEIGHT)
                    )
               (cond [(and (<= MIN-X new-img-x)
                           (<= new-img-x MAX-X)
                           (<= MIN-Y new-img-y)
                           (<= new-img-y MAX-Y)
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (posn new-img-x new-img-y)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-dgraph a-vs)
                                 (viz-state-p-dgraph a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 )
                      ]
                     [(and (or (> MIN-X new-img-x)
                               (> new-img-x MAX-X)
                               )
                           (<= MIN-Y new-img-y)
                           (<= new-img-y MAX-Y)
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (posn (posn-x (viz-state-image-posn a-vs)) new-img-y)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-dgraph a-vs)
                                 (viz-state-p-dgraph a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 )
                      ]
                     [(and (<= MIN-X new-img-x)
                           (<= new-img-x MAX-X)
                           (or (> MIN-Y new-img-y)
                               (> new-img-y MAX-Y)
                               )
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (posn new-img-x (posn-y (viz-state-image-posn a-vs)))
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-dgraph a-vs)
                                 (viz-state-p-dgraph a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 )
                      ]
                     [(and (or (> MIN-X new-img-x)
                               (> new-img-x MAX-X)
                               )
                           (or (> MIN-Y new-img-y)
                               (> new-img-y MAX-Y)
                               )
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (viz-state-image-posn a-vs)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-dgraph a-vs)
                                 (viz-state-p-dgraph a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (viz-state-word-img-offset a-vs)
                                 (viz-state-word-img-offset-cap a-vs)
                                 )
                      ]
                     )
               ]
              [(and (<= (rule-yield-dims-min-x scroll-dimensions) (posn-x (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-x (viz-state-curr-mouse-posn a-vs)) (rule-yield-dims-max-x scroll-dimensions))
                    (<= (rule-yield-dims-min-y scroll-dimensions) (posn-y (viz-state-curr-mouse-posn a-vs)))
                    (<= (posn-y (viz-state-curr-mouse-posn a-vs)) (rule-yield-dims-max-y scroll-dimensions))
                    )
               (cond [(and (>= (viz-state-word-img-offset-cap a-vs) (viz-state-word-img-offset a-vs))
                           (<= (quotient x-diff 25) -1)
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (viz-state-image-posn a-vs)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-dgraph a-vs)
                                 (viz-state-p-dgraph a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (+ (viz-state-word-img-offset a-vs) 1)
                                 (viz-state-word-img-offset-cap a-vs)
                                 )]
                     [(and (> (viz-state-word-img-offset a-vs) 0)
                           (>= (quotient x-diff 25) 1)
                           )
                      (viz-state (viz-state-upimgs a-vs)
                                 (viz-state-pimgs a-vs)
                                 (viz-state-curr-image a-vs)
                                 (viz-state-image-posn a-vs)
                                 (viz-state-scale-factor a-vs)
                                 (viz-state-scale-factor-cap a-vs)
                                 (viz-state-scale-factor-floor a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-dest-mouse-posn a-vs)
                                 (viz-state-mouse-pressed a-vs)
                                 (viz-state-up-dgraph a-vs)
                                 (viz-state-p-dgraph a-vs)
                                 (viz-state-up-yield a-vs)
                                 (viz-state-p-yield a-vs)
                                 (viz-state-input-word a-vs)
                                 (- (viz-state-word-img-offset a-vs) 1)
                                 (viz-state-word-img-offset-cap a-vs)
                                 )
                      ]
                     [else (viz-state (viz-state-upimgs a-vs)
                                      (viz-state-pimgs a-vs)
                                      (viz-state-curr-image a-vs)
                                      (viz-state-image-posn a-vs)
                                      (viz-state-scale-factor a-vs)
                                      (viz-state-scale-factor-cap a-vs)
                                      (viz-state-scale-factor-floor a-vs)
                                      (viz-state-dest-mouse-posn a-vs)
                                      (viz-state-dest-mouse-posn a-vs)
                                      (viz-state-mouse-pressed a-vs)
                                      (viz-state-up-dgraph a-vs)
                                      (viz-state-p-dgraph a-vs)
                                      (viz-state-up-yield a-vs)
                                      (viz-state-p-yield a-vs)
                                      (viz-state-input-word a-vs)
                                      (viz-state-word-img-offset a-vs)
                                      (viz-state-word-img-offset-cap a-vs)
                                      )
                           ]
                     )
               ]
              [else
               (viz-state (viz-state-upimgs a-vs)
                          (viz-state-pimgs a-vs)
                          (viz-state-curr-image a-vs)
                          (viz-state-image-posn a-vs)
                          (viz-state-scale-factor a-vs)
                          (viz-state-scale-factor-cap a-vs)
                          (viz-state-scale-factor-floor a-vs)
                          (viz-state-dest-mouse-posn a-vs)
                          (viz-state-dest-mouse-posn a-vs)
                          (viz-state-mouse-pressed a-vs)
                          (viz-state-up-dgraph a-vs)
                          (viz-state-p-dgraph a-vs)
                          (viz-state-up-yield a-vs)
                          (viz-state-p-yield a-vs)
                          (viz-state-input-word a-vs)
                          (viz-state-word-img-offset a-vs)
                          (viz-state-word-img-offset-cap a-vs)
                          )
               ]
              )
        (viz-state (viz-state-upimgs a-vs)
                   (viz-state-pimgs a-vs)
                   (viz-state-curr-image a-vs)
                   (viz-state-image-posn a-vs)
                   (viz-state-scale-factor a-vs)
                   (viz-state-scale-factor-cap a-vs)
                   (viz-state-scale-factor-floor a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-dest-mouse-posn a-vs)
                   (viz-state-mouse-pressed a-vs)
                   (viz-state-up-dgraph a-vs)
                   (viz-state-p-dgraph a-vs)
                   (viz-state-up-yield a-vs)
                   (viz-state-p-yield a-vs)
                   (viz-state-input-word a-vs)
                   (viz-state-word-img-offset a-vs)
                   (viz-state-word-img-offset-cap a-vs)
                   )
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
        )))

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
                                         'font "Sans"))
                            )
    )
  )

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [
        (PARSE-TREE-IMG (place-image (scale (viz-state-scale-factor a-vs) (viz-state-curr-image a-vs)) 
                                     (posn-x (viz-state-image-posn a-vs))
                                     (posn-y (viz-state-image-posn a-vs))
                                     (rectangle E-SCENE-WIDTH E-SCENE-HEIGHT 'outline 'white)
                                     )
                        )
        ]
    (above PARSE-TREE-IMG (create-instructions-and-tools a-vs))
    )
  )

         
;; cfg-viz
(define (cfg-viz cfg word)
  (if (string? (grammar-derive cfg word))
      (grammar-derive cfg word)
      (let* [ (w-der (map symbol->fsmlos (filter (λ (x) (not (equal? x '->)))
                                                 (grammar-derive cfg word))))
              (rules (cons "" (create-rules w-der)))
              (extracted-edges (create-levels w-der))
              ;(renamed (rename-levels extracted-edges))
              (renamed 1)
              (loe (map (λ (el) (if (symbol? (first el))
                                    (list el '())
                                    el)) renamed))
              (dgraph (dgrph loe '() '() '() (rest rules) (list (first rules))))
              (lod (reverse (create-dgrphs dgraph '())))
              (first-img (create-first-img (first (extract-nodes loe))))
              (imgs (cons first-img (rest (create-graph-imgs lod))))
              #;(test (begin
                        (map (lambda (x) (displayln x)) imgs)
                        )
                      )
              ]
        (run-viz (viz-state (rest imgs)
                            (list (first imgs))
                            (
                             ;force
                             (first imgs)
                             )
                            (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
                            DEFAULT-ZOOM
                            DEFAULT-ZOOM-CAP
                            DEFAULT-ZOOM-FLOOR
                            (posn 0 0)
                            (posn 0 0)
                            #f
                            (rest lod)
                            (list (first lod))
                            (rest w-der)
                            (first w-der)
                            word
                            0
                            (- (length word) TAPE-SIZE)
                            )
                 draw-world 'cfg-ctm))))

(define (cfg-derive1 g w)
    (define (get-first-nt st)
      (cond [(empty? st) #f]
            [(not (member (car st) (cfg-get-alphabet g))) (car st)]
            [else (get-first-nt (cdr st))])
      )
    
    (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                     (cfg-get-the-rules g)))
    
    ; ASSUMPTION: state has at least one NT
    (define (subst-first-nt state rght)
      (cond [(not (member (car state) (cfg-get-alphabet g)))
             (if (eq? (car rght) EMP) (cdr state) (append rght (cdr state)))]
            [else (cons (car state) (subst-first-nt (cdr state) rght))]))
    
    ; (listof (listof symbol)) --> (listof symbol)
    (define (get-starting-terminals st)
      (cond 
        [(not (member (car st) (cfg-get-alphabet g))) '()]
        [else (cons (car st) (get-starting-terminals (cdr st)))]))
    
    ; (listof (listof symbol)) natnum --> (listof symbol)
    (define (get-first-n-terms w n)
      ;(println w)
      (cond [(= n 0) '()]
            [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))
    
    
    ; (list (listof symbol)) --> boolean
    (define (check-terminals? st)
      (let* ((start-terms-st (get-starting-terminals st))
             (start-terms-w (if (> (length start-terms-st) (length w))
                                #f
                                (get-first-n-terms w (length start-terms-st)))))
        (cond [(false? start-terms-w) #f]
              [else (equal? start-terms-st start-terms-w)])))
    
    
    (define (make-deriv visited derivs g chomsky)
      (define (count-terminals st sigma)
        (length (filter (lambda (a) (member a sigma)) st)))
 
      (cond [(empty? derivs) (format "~s is not in L(G)." w)]
            [(or (and chomsky
                      (> (length (first (first (first derivs)))) (+ 2 (length w)))
                      )
                 (> (count-terminals (first (first (first derivs))) (cfg-get-alphabet g)) (length w))
                 )
             (make-deriv visited (cdr derivs) g chomsky)]
            [else 
             (let* ((fderiv (car derivs))
                    (state (car fderiv))
                    (fnt (get-first-nt (first state)))
                    )
               (if (false? fnt)
                   (if (equal? w (first state))
                       (append-map (lambda (l) (if (equal? w (first l)) 
                                                   (if (null? l)
                                                       (list EMP)
                                                       (list (list (los->symbol (first l)) (los->symbol (second l))))
                                                       )
                                                   (list (list (los->symbol (first l)) (los->symbol (second l))) ARROW)
                                                   )
                                     ) 
                                   (reverse fderiv))
                       (make-deriv visited (cdr derivs) g chomsky))
                   (let*
                       ((rls (get-rules fnt g))
                        (rights (map cfg-rule-rhs rls))
                        (new-states (filter (lambda (st) (and (not (member st visited))
                                                              (check-terminals? (first state)))) 
                                            (map (lambda (rght) (list (subst-first-nt (first state) rght) rght)) rights)
                                            )
                                    )
                        )
                     (make-deriv (append new-states visited)
                                 (append (cdr derivs) 
                                         (map (lambda (st) (cons st fderiv)) 
                                              new-states))
                                 g
                                 chomsky))))]))   
    (if (< (length w) 2)
        (format "The word ~s is too short to test." w)
        (let* ( ;; derive using g ONLY IF derivation found with g in CNF
               (ng (convert-to-cnf g))
               (ng-derivation (make-deriv (list (list (list (cfg-get-start ng)) '() )) 
                                          (list (list (list (list (cfg-get-start ng)) '() )))
                                          ng
                                          true)
                              )
               )
          (if (string? ng-derivation)
              ng-derivation
              (make-deriv (list (list (list (cfg-get-start g)) '() )) 
                          (list (list (list (list (cfg-get-start g)) '() )))
                          g
                          false)
              )
          )
        )
  )

;; vst --> void
(define (run-viz a-vs draw-etc a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-etc]
      [on-key process-key]
      [on-mouse process-mouse]
      [on-tick process-tick 1/60]
      [name a-name]))
  (void))

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))
(cfg-derive1 numb>numa '(a b b))