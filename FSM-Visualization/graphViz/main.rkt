#lang racket
(require 2htdp/image "../globals.rkt" "../../GraphViz/lib.rkt" "../../GraphViz/render-graph.rkt"
         "../structs/state.rkt")

(provide scaled-graph create-png)

;; width: 1200 - 100 - 200 (-100)
;; height: 600 - 100 - 75
(define h 400)
(define w 700)
(define h-pda 300)
(define w-pda 500)

;;(define img (bitmap "../../graph.png"))
;;(println (format "Img width ~s" (image-width img)))
;;(println (format "Img height ~s" (image-height img)))

(define (scaled-graph img type)
  (cond
    [(< (image-width img) (image-height img)) (scale 1.2 img)]
    [else
     (if (equal? type 'pda)
         (scale/xy (/ w-pda (image-width img)) (/ h-pda (image-height img)) img)
         (scale/xy (/ w (image-width img)) (/ h (image-height img)) img))]))


(define (create-png states start finals rules cur-rule)
  (let ((g (create-graph 'G #:color 0))
        (rule-start (lambda () (case MACHINE-TYPE
                                 [(dfa) (car cur-rule)]
                                 [(ndfa) (car cur-rule)]
                                 [else (caar cur-rule)])))
        (rule-end (lambda () (case MACHINE-TYPE
                               [(dfa) (last cur-rule)]
                               [(ndfa) (last cur-rule)]
                               [else (caadr cur-rule)])))
        (new-states (map (lambda (s) (fsm-state-name s)) states)))
    (begin
      (states->nodes new-states
                     start
                     finals
                     g)
      (rules->edges rules
                    MACHINE-TYPE
                    g
                    (if (or (member 'empty cur-rule) (member 'null cur-rule)) "$NULL" (rule-start))
                    (if (or (member 'empty cur-rule) (member 'null cur-rule)) "$NULL" (rule-end)))
                    (if (or (member 'empty cur-rule) (member 'null cur-rule))
                        (graph->png g)
                        (graph->png g #:rule cur-rule))
                    (scaled-graph (bitmap "../../vizTool.png") MACHINE-TYPE))))
  

  