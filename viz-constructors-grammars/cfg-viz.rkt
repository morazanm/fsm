#lang racket

(require "../fsm-gviz/private/lib.rkt" 
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../fsm-core/interface.rkt")

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

(define E-SCENE (empty-scene 600 800))

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



;; viz-state is a structure that has
;; upimgs - unprocessed graph images
;; pimgs - processed graph images
(struct viz-state (upimgs pimgs))
