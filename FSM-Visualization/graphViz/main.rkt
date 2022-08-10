#lang racket
(require 2htdp/image "../globals.rkt" "../../GraphViz/lib.rkt" "../../GraphViz/render-graph.rkt"
         "../structs/state.rkt" "../structs/machine.rkt" "../inv.rkt")

(provide scaled-graph create-png)


(define (scaled-graph img type)
  (let* ([w-with-left-btns (- w 50)]
         [smallest (>= (- w-with-left-btns (image-width img))
                       (- h (image-height img)))]
         [allocated-w (if (eq? 'pda type) (/ w-pda 2)(/ w 2))]
         [allocated-h (if (eq? 'pda type) (/ h-pda 2)(/ h 2))])
    (cond
      [(and (< (image-width img) allocated-w)
            (< (image-height img) allocated-h))
       (define new-img (scale SMALL-IMG-SCALE img))
       ;; if the newly scaled img is larger then the allocated space we will use the old one
       (if (or (> (image-width new-img) allocated-w)
               (> (image-height new-img) allocated-h))
           img
           new-img)]
      [smallest
       (if (equal? type 'pda)
           (scale (floor (/ h-pda (image-height img))) img)
           (scale (floor (/ w-with-left-btns (image-width img))) img))]
      [else
       (if (equal? type 'pda)
           (scale (/ w-pda (image-width img)) img)
           (scale (/ w-with-left-btns (image-width img)) img))])))


(define (create-png machine hasRun? cur-state cur-rule)
  (letrec ((g (create-graph 'G #:color 0))
           (states (machine-state-list machine))
           (start (machine-start-state machine))
           (finals (machine-final-state-list machine))
           (rules (machine-rule-list machine))
           (color (if hasRun? (determin-inv machine cur-state #:graphViz true) #f))
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
                     g
                     #:cur-state cur-state
                     #:color (HIGHLIGHT-NODE color))
      (rules->edges rules
                    MACHINE-TYPE
                    g
                    (if (or (member 'empty cur-rule) (member 'null cur-rule)) "$NULL" (rule-start))
                    (if (or (member 'empty cur-rule) (member 'null cur-rule)) "$NULL" (rule-end))
                    "black")
      (if (or (member 'empty cur-rule) (member 'null cur-rule))
          (scaled-graph (graph->png g) MACHINE-TYPE)
          (scaled-graph (graph->png g #:rule cur-rule) MACHINE-TYPE)))))
;(scaled-graph (bitmap/file (string-append (path->string (current-directory)) "vizTool.png")) MACHINE-TYPE))))
  

