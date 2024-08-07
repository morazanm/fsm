#lang racket

(require 2htdp/image
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../../fsm-core/private/regexp.rkt"
         "../viz-lib/zipper.rkt"
          "../viz-lib/viz-imgs/cursor.rkt"
          "../../sm-graph.rkt"
          "../../fsm-gviz/interface.rkt"
         )
(provide ndfa2regexp-viz)

(define FNAME "fsm")

(define E-SCENE-HEIGHT 560)

(define E-SCENE-TOOLS
  (let [(ARROW (above (triangle 30 'solid 'black) (rectangle 10 30 'solid 'black)))]
    (beside/align "bottom"
                  (above ARROW-UP-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Restart" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above ARROW-RIGHT-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Forward" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above ARROW-LEFT-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Backward" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above ARROW-DOWN-KEY
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Finish" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (above cursor
                         (square HEIGHT-BUFFER 'solid 'white)
                         (text "Hold to drag" (- FONT-SIZE 2) 'black))
                  (square ARROW-KEY-WIDTH-BUFFER 'solid 'white)
                  (beside (above/align "middle" W-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Zoom in" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle"  S-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Zoom out" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle" R-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Min zoom" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle" E-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Mid zoom" (- FONT-SIZE 2) 'black))
                          (square LETTER-KEY-WIDTH-BUFFER 'solid 'white)
                          
                          (above/align "middle" F-KEY (square HEIGHT-BUFFER 'solid 'white) (text "Max zoom" (- FONT-SIZE 2) 'black))
                          )
                  )
    )
  )

(define imsg-img (text "Starting ndfa" FONT-SIZE 'black))

(define RULE-YIELD-DIMS (bounding-limits 0
                                         (image-width imsg-img)
                                         E-SCENE-HEIGHT
                                         (+ E-SCENE-HEIGHT (image-height imsg-img))))

(define ARROW-UP-KEY-DIMS
  (bounding-limits (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Restart" (- FONT-SIZE 2) 'black)) (image-width ARROW-UP-KEY)) 2))
                   (+ (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Restart" (- FONT-SIZE 2) 'black)) (image-width ARROW-UP-KEY)) 2)
                      (image-width ARROW-UP-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-UP-KEY))))

(define ARROW-RIGHT-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Forward" (- FONT-SIZE 2) 'black)) (image-width ARROW-RIGHT-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Forward" (- FONT-SIZE 2) 'black)) (image-width ARROW-RIGHT-KEY)) 2)
                      (image-width ARROW-RIGHT-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-RIGHT-KEY))))

(define ARROW-LEFT-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Backward" (- FONT-SIZE 2) 'black)) (image-width ARROW-LEFT-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Backward" (- FONT-SIZE 2) 'black)) (image-width ARROW-LEFT-KEY)) 2)
                      (image-width ARROW-LEFT-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-LEFT-KEY))))

(define ARROW-DOWN-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Finish" 18 'black)) (image-width ARROW-DOWN-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Finish" (- FONT-SIZE 2) 'black)) (image-width ARROW-DOWN-KEY)) 2)
                      (image-width ARROW-DOWN-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height ARROW-DOWN-KEY))))

(define W-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom in" (- FONT-SIZE 2) 'black)) (image-width W-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom in" (- FONT-SIZE 2) 'black)) (image-width W-KEY)) 2)
                      (image-width W-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height W-KEY))))

(define S-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom out" (- FONT-SIZE 2) 'black)) (image-width S-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Zoom out" (- FONT-SIZE 2) 'black)) (image-width S-KEY)) 2)
                      (image-width S-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height S-KEY))))

(define R-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Min zoom" (- FONT-SIZE 2) 'black)) (image-width R-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black)) (image-width R-KEY)) 2)
                      (image-width R-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height R-KEY))))

(define E-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Mid zoom" (- FONT-SIZE 2) 'black)) (image-width E-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black)) (image-width E-KEY)) 2)
                      (image-width E-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height E-KEY))))

(define F-KEY-DIMS
  (bounding-limits (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Max zoom" (- FONT-SIZE 2) 'black)) (image-width F-KEY)) 2))
                   (+ (image-width (text "Restart" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Forward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Backward" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Finish" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Hold to drag" (- FONT-SIZE 2) 'black))
                      ARROW-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom in" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Zoom out" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      
                      (image-width (text "Min Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (image-width (text "Mid Zoom" (- FONT-SIZE 2) 'black))
                      LETTER-KEY-WIDTH-BUFFER
                      (/ (- E-SCENE-WIDTH (image-width E-SCENE-TOOLS)) 2)
                      (/ (- (image-width (text "Max Zoom" (- FONT-SIZE 2) 'black)) (image-width F-KEY)) 2)
                      (image-width F-KEY))
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER)
                   (+ EXTRA-HEIGHT-FROM-CURSOR
                      E-SCENE-HEIGHT
                      (bounding-limits-height RULE-YIELD-DIMS)
                      INS-TOOLS-BUFFER
                      (image-height F-KEY))))
;; L = ab*
#| (define nl (make-ndfa '(S)
                      '(a b)
                      'S
                      '()
                      '()))

(define A (make-ndfa '(S A B C D E F)
                     '(a b x)
                     'S
                     '(D E)
                     '((S a A)
                       (S a F)
                       (S b B)
                       (A a C)
                       (B b C)
                       (C b D)
                       (C a E)
                       (C x C)
                       (F a C))))

;; L = ab*
(define ab* (make-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-ndfa '(Z H B C D F)
                            '(a b)
                            'Z
                            '(F)
                            `((Z a H)
                              (Z a B)
                              (H a D)
                              (D ,EMP F)
                              (B a C)
                              (C b F)
                              (F b F))))
;; L = aab*
(define aab* (make-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))
;; L = a*
(define a* (make-dfa '(S D)
                     '(a b)
                     'S
                     '(S)
                     '((S a S)
                       (S b D)
                       (D a D)
                       (D b D))
                     'no-dead))

(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C) '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (listof edge) → regexp
;; Purpose: Collapse the given edges into a regexp
(define (collapse-edges loe)
  (cond [(empty? loe) '()]
        [(empty? (rest loe)) (second (first loe))]
        [else (make-unchecked-union (second (first loe))
                                    (collapse-edges (rest loe)))]))


;; dgraph → dgraph
;; Purpose: Collapse multiple edges between nodes
;; Accumulator Invariant: g = the unprocessed graph
(define (remove-multiple-edges g)
  (if (empty? g) '()
      (let* [(curr-edge (first g))
             (from-state (first curr-edge))
             (to-state (third curr-edge))
             (to-collapse (filter (λ (e) (and (eq? (first e) from-state)
                                              (eq? (third e) to-state)))
                                  g))
             (remaining-g (filter (λ (e) (not (member e to-collapse))) g))]
        (cons (list from-state (collapse-edges to-collapse) to-state)
              (remove-multiple-edges remaining-g)))))

;; node dgraph → dgraph
;; Purpose: Rip out given state from given graph
(define (rip-out-node n g)
  (let* [(non (filter (λ (r) (and (not (eq? (third r) n))
                                  (not (eq? (first r) n))))
                      g))
         (into-n (filter (λ (r) (and (eq? (third r) n)
                                     (not (eq? (first r) n))))
                         g))
         (outof-n (filter (λ (r) (and (eq? (first r) n)
                                      (not (eq? (third r) n))))
                          g))
         (self-edges (filter (λ (r) (and (eq? (first r) n)
                                         (eq? (third r) n)))
                             g))]
    (remove-multiple-edges
     (append non
             (if (not (empty? self-edges))
                 (let [(self-edge (first self-edges))]
                   (append-map
                    (λ (into-edge)
                      (map (λ (outof-edge)
                             (list (first into-edge)
                                   (make-unchecked-concat
                                    (second into-edge)
                                    (make-unchecked-concat
                                     (make-unchecked-kleenestar (second self-edge))
                                     (second outof-edge)))
                                   (third outof-edge)))
                           outof-n))
                    into-n))
                 (append-map (λ (into-edge)
                               (map (λ (outof-edge)
                                      (list (first into-edge)
                                            (make-unchecked-concat (second into-edge)
                                                                   (second outof-edge))
                                            (third outof-edge)))
                                    outof-n))
                             into-n))))))

;; (listof node) dgraph → dgraph
;; Purpose: Rip out the given nodes from the given graph
;; Assume: Given nodes in given graph and g has no multiple edges
;;         between nodes
(define (rip-out-nodes lon g)
  (foldr (λ (s g) (rip-out-node s g)) g lon))


;; (listof ndfa-rule) → dgraph
;; Purpose: Create a dgraph from the given ndfa
(define (make-dgraph lor)
  (map (λ (r) (if (eq? (second r) EMP)
                  (list (first r) (empty-regexp) (third r))
                  (list (first r)
                        (singleton-regexp (symbol->string (second r)))
                        (third r))))
       lor))

;; (listof ndfa-rule) → dgraph
;; Purpose: Create a dgraph from the given ndfa (for the ones that are already regexp)
(define (make-dgraph-unions lor)
  (map (λ (r) (if (eq? (second r) EMP)
                  (list (first r) (printable-regexp (empty-regexp)) (third r))
                  (list (first r)
                        (second r)
                        (third r))))
       lor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; graph-struct
;; grph is the graph 
;; inf is the corresponding informative message
(struct graph-struct (grph inf))

;; create-nodes
;; graph (listof state) state state -> graph
;; Purpose: To add the given states as nodes to the given graph
;;          using the given ns and nf as, respectively, the new
;;          start and final states.
(define (create-nodes graph los ns nf)
  (let [(states-only (remove-duplicates (append (list ns nf) los)))]
    (foldl (λ (state result)
             (add-node
              result
              state
              #:atb (hash 'color (if (eq? state ns)
                                     'darkgreen
                                     'black)
                          'shape (if (eq? state nf)
                                     'doublecircle
                                     'circle)
                          'label (if (equal? state '())
                                     'ds  
                                     state)
                          'fontcolor 'black
                          'font "Sans")))
           graph
           states-only)))                           

;; create-edges
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges graph loe)
  (foldl (λ (rule result)
           (add-edge result
                     (printable-regexp (simplify-regexp (second rule)))
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         loe))

;; create-edges-special
;; graph (listof edge) -> graph
;; Purpose: To create graph of edges
(define (create-edges-special graph loe)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'fontsize 14
                                 'style 'solid
                                 'fontname "Sans"
                                 )))
         graph
         loe))

;; create-graph-special
;; (listof state) dgraph state state -> graph
;; Purpose: To create a graph structure for the given dgraph using
;;          news as the start state and newf as the final state
(define (create-graph-special los loe news newf)
  (create-edges-special
   (create-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
    los
    news
    newf)
   loe))


;; create-graphic
;; (listof state) dgraph state state -> graph
;; Purpose: To create a graph structure for the given dgraph using
;;          news as the start state and newf as the final state
(define (create-graphic los loe news newf)
  (create-edges
   (create-nodes
    (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
    los
    news
    newf)
   loe))


;; create-graphs
;; ndfa -> (listof graph)
;; Purpose: To create a list of graph structures that build a regular
;; expression from the  given ndfa
(define (create-graphs M)
  (define new-start (gen-state (sm-states M)))
  (define new-final (gen-state (cons new-start(sm-states M))))
  (define new-rules (cons (list new-start EMP (sm-start M))
                          (map (λ (fst) (list fst EMP new-final))
                               (sm-finals M))))
  (define (create-graphs-helper M)
  (define (grp-seq to-rip g gseq)
    (if (null? to-rip)
        gseq 
        (let [(new-g (rip-out-node (first to-rip) g))]
          (grp-seq (rest to-rip)
                   new-g
                   (cons (graph-struct (create-graphic
                                        (append (list new-start new-final) )
                                        new-g
                                        new-start
                                        new-final)
                                       (text (format "Ripped node: ~a" (first to-rip)) 20 'black))
                         gseq)))
        ))
  (reverse (grp-seq (if (not (member DEAD (sm-states M)))
                        (sm-states M)
                        (cons DEAD (remove DEAD (sm-states M))))
                    (make-dgraph (append (sm-rules M) new-rules)) '()))
    )
  (cons (graph-struct (create-graphic
                       (append (if (not (member DEAD (sm-states M)))
                                   (sm-states M)
                                   (cons DEAD (remove DEAD (sm-states M)))))
                             (make-dgraph (append (sm-rules M) new-rules))
                             new-start
                             new-final)
                            (text "Added starting and final state" 20 'black))
              (create-graphs-helper M))
  )

;; add-edges-to-hash
;; hash (listof edges) -> hash
;; Purpose: To add edges to the hash table
(define (add-edges-to-hash hash loe)
  (let* [(current-hash (begin
                         (if (hash-has-key? hash (list (first (first loe))
                                                       (third (first loe))))                             
                             (hash-set! hash (list (first (first loe))
                                                   (third (first loe)))
                                        (append (list (second (first loe)))
                                                (hash-ref hash (list (first (first loe))
                                                                     (third (first loe))))))
                             (hash-set! hash (list (first (first loe))
                                                   (third (first loe)))
                                        (list (second (first loe)))))
                         hash))]
    (if (= (length loe) 1)
        current-hash
        (add-edges-to-hash current-hash (rest loe)))))
               
               


;; make-unions
;; (listof symbol) -> regexp
;; Purpose: To return a union of all symbols in a list
(define (make-unions los)
  (if (= 1 (length los))
      (symbol->string (first los))
      (string-append (symbol->string (first los)) " U " (make-unions (rest los)))))

;; to-union
;; (listof edges) -> (listof edges with regexp labels)
;; Purpose: To turn all loops on multiple edges into unions
(define (to-union loe)
  (let* [(hash-t (add-edges-to-hash (make-hash) loe))
         (hash-l (reverse (hash->list hash-t)))]
    (map (λ (x) (if (< (length (rest x)) 1)
                    (list (first (first x))
                          (string->symbol (first (rest x)))
                          (second (first x)))
                    (list (first (first x))
                          (string->symbol (make-unions (reverse (rest x))))
                          (second (first x)))))
         hash-l)))
                 

;; make-init-graph
;; ndfa -> img
;; Purpose: To create the structure of the initial ndfa graph
(define (make-init-graph M)
  (let* [(new-start (gen-state (sm-states M)))
         (new-final (gen-state (cons new-start (sm-states M))))
         (new-rules (cons (list new-start EMP (sm-start M))
                          (map (λ (fst) (list fst EMP new-final))
                               (sm-finals M))))
         (changed-rules (to-union (append (sm-rules M) new-rules)))]
    (graph-struct (create-graph-special
                   (if (not (member DEAD (sm-states M)))
                       (sm-states M)
                       (cons DEAD (sm-states M)))
                   (make-dgraph-unions changed-rules) 
                   new-start
                   new-final)
                  (text "Starting ndfa" FONT-SIZE 'black))))

;; imsg-state
;; infs - state of the informative messages
(struct imsg-state (infs))







(define viz-go-next (go-next E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-prev (go-prev E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-begin (go-to-begin E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))
(define viz-go-to-end (go-to-end E-SCENE-WIDTH E-SCENE-HEIGHT NODE-SIZE DEFAULT-ZOOM-CAP DEFAULT-ZOOM-FLOOR PERCENT-BORDER-GAP))


(define viz-zoom-in (zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-zoom-out (zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-out (max-zoom-out E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-max-zoom-in (max-zoom-in E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))
(define viz-reset-zoom (reset-zoom E-SCENE-WIDTH E-SCENE-HEIGHT ZOOM-INCREASE ZOOM-DECREASE NODE-SIZE PERCENT-BORDER-GAP DEFAULT-ZOOM-CAP DEFAULT-ZOOM))

;; draw-imsg
;; imsg -> img
(define (draw-imsg a-imsg)
  (zipper-current (imsg-state-infs a-imsg)))


(define (right-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (imsg-state-infs a-imsg-state))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-state
                                           a-imsg-state
                                           [infs (zipper-next
                                                  (imsg-state-infs
                                                   a-imsg-state)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define (left-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (imsg-state-infs a-imsg-state))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-state
                                           a-imsg-state
                                           [infs (zipper-prev
                                                  (imsg-state-infs
                                                   a-imsg-state)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define (up-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (imsg-state-infs a-imsg-state))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-state
                                           a-imsg-state
                                           [infs (zipper-to-begin
                                                  (imsg-state-infs
                                                   a-imsg-state)
                                                  )
                                                 ]
                                           )])]))
    )
  )

(define (down-key-pressed a-vs)
  (let ([a-imsg-state (informative-messages-component-state
                       (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (imsg-state-infs a-imsg-state))
        a-vs
  (struct-copy viz-state a-vs
               [informative-messages
                (struct-copy informative-messages
                             (viz-state-informative-messages a-vs)
                             [component-state
                              (struct-copy imsg-state
                                           a-imsg-state
                                           [infs (zipper-to-end
                                                  (imsg-state-infs
                                                   a-imsg-state)
                                                  )
                                                 ]
                                           )])]))
    )
  )

;; ndfa2regexp-viz
;; ndfa --> (void)
(define (ndfa2regexp-viz M)
  (run-viz (list* (fsa->graph M 0) (map graph-struct-grph (create-graphs M)))
           (lambda () (sm-graph M))
           MIDDLE-E-SCENE
           DEFAULT-ZOOM
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                 (imsg-state (list->zipper (list* (text "Starting NDFA" FONT-SIZE 'black)
                                                                 (map graph-struct-inf (create-graphs M)))))
                                 (bounding-limits 0 0 0 0)
                                 )
           (instructions-graphic
            E-SCENE-TOOLS
            (bounding-limits 0 0 0 0))
           (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
           (create-viz-process-key (list (list "right" viz-go-next right-key-pressed)
                                         (list "left" viz-go-prev left-key-pressed)
                                         (list "up" viz-go-to-begin up-key-pressed)
                                         (list "down" viz-go-to-end down-key-pressed)
                                         (list "w" viz-zoom-in identity)
                                         (list "s" viz-zoom-out identity)
                                         (list "r" viz-max-zoom-out identity)
                                         (list "f" viz-max-zoom-in identity)
                                         (list "e" viz-reset-zoom identity)
                                         (list "wheel-down" viz-zoom-in identity)
                                         (list "wheel-up" viz-zoom-out identity)
                                         )
                                   )
           (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                    CLICK-BUFFER-SECONDS
                                    (list)
                                    (list (list ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed)
                                          (list ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed)
                                          (list ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed)
                                          (list ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed)
                                          (list W-KEY-DIMS viz-zoom-in identity)
                                          (list S-KEY-DIMS viz-zoom-out identity)
                                          (list R-KEY-DIMS viz-max-zoom-out identity)
                                          (list E-KEY-DIMS viz-reset-zoom identity)
                                          (list F-KEY-DIMS viz-max-zoom-in identity)))
           ))

(define aa-ab
  (make-unchecked-ndfa 
   '(S A B F) 
   '(a b) 
   'S
   '(A B)
   `((S a A)
     (S a B)
     (A a A)
     (B b B)
     (S ,EMP F))))

(ndfa2regexp-viz aa-ab)

;(run AT-LEAST-ONE-MISSING)




