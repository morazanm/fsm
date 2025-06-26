#lang racket
(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../../sm-graph.rkt")
(struct dfa (states alphabet start finals rules no-dead) #:transparent)
(define (unchecked->dfa old-dfa)
  (dfa (fsa-getstates old-dfa)
       (fsa-getalphabet old-dfa)
       (fsa-getstart old-dfa)
       (fsa-getfinals old-dfa)
       (fsa-getrules old-dfa)
       'no-dead))
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
                                                     (list F-KEY "Max zoom"))))

(define imsg-img
  (above (text "Kleenestar of the ndfa" FONT-SIZE 'black)
         (text (format "Generated starting state:") FONT-SIZE 'black)
         (text (format "Added edges:") FONT-SIZE 'black)))

(define E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height imsg-img)
                          (image-height E-SCENE-TOOLS)))
(define MIDDLE-E-SCENE (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))

(define RULE-YIELD-DIMS
  (bounding-limits 0
                   (image-width imsg-img)
                   E-SCENE-HEIGHT
                   (+ E-SCENE-HEIGHT (image-height imsg-img))))

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
                         (F-KEY "Max zoom")))


(define (make-table M)
  (let ([M (unchecked->dfa M)])
    (make-table-helper (dfa-states M) (add1 (length (dfa-states M))))))

(define (make-table-helper states num-rows)
  (build-vector num-rows (λ (row-num)
                           (build-vector num-rows
                                         (λ (col-num) (let ([blank-tile-count (- (length states) (- num-rows row-num))])
                                                        (cond [(= row-num 0) (if (= col-num 0) 'blank (list-ref states (sub1 col-num)))]
                                                              [(= col-num 0) (list-ref states (sub1 row-num))]
                                                              [(<= col-num blank-tile-count) 'blank]
                                                              [else 'black])))))))

(define (look-up-in-table table row column)
  (vector-ref (vector-ref table column) row))


(define (draw-table table)
  (define (draw-table-helper row-amount idx)
    (if (= row-amount idx)
        (make-row (vector-ref table idx) 0)
        (above (make-row (vector-ref table idx) 0)
               (draw-table-helper row-amount (add1 idx)))))
  (define (make-row row idx)
    (define (draw-square sym)
      (let ([base-square-img (overlay (square 40 'solid 'white) (square 45 'solid 'gray))])
        (match sym
          ['blank base-square-img]
          ['black (overlay (square 40 'solid 'black) (square 45 'solid 'gray))]
          ['new-mark (overlay (text "X" 38 'red) base-square-img)]
          ['mark (overlay (text "X" 38 'black) base-square-img)]
          [_ (overlay (text (symbol->string sym) 38 'black) base-square-img)])))
    (if (= (sub1 (vector-length row)) idx)
        (draw-square (vector-ref row idx))
        (beside (draw-square (vector-ref row idx)) (make-row row (add1 idx)))))
  (draw-table-helper (sub1 (vector-length table)) 0))

(define (make-main-graphic M)
  (beside (sm-graph M)
          (square 10 'solid 'white)
          (draw-table (make-table M))))

(define (make-info-messages M)
  (above (text "TBD" 20 'black)
         (text "TBD" 20 'black)))


(define (make-viz-demo M)
  (overlay (above (make-main-graphic M)
                  (square 60 'solid 'white)
                  (make-info-messages M)
                  (square 40 'solid 'white)
                  E-SCENE-TOOLS)
                  (empty-scene E-SCENE-WIDTH E-SCENE-HEIGHT)))

(define EX4 (make-unchecked-dfa '(A B C D E F G)
                      '(0 1)
                      'A
                      '(B C G)
                      '((A 0 B) (A 1 C)
                        (B 0 D) (B 1 E)
                        (C 0 E) (C 1 D)
                        (D 0 G) (D 1 G)
                        (E 0 G) (E 1 G)
                        (F 0 D) (F 1 E)
                        (G 0 G) (G 1 G))
                      'no-dead))


(define EX6 (make-unchecked-dfa '(A B C D E)
                    '(0 1)
                    'A
                    '(E)
                    '((A 0 B) (A 1 C)
                              (B 0 B) (B 1 D)
                              (C 0 B) (C 1 C)
                              (D 0 B) (D 1 E)
                              (E 0 B) (E 1 C))
                    'no-dead))