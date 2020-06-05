#lang racket
(require 2htdp/image "../globals.rkt" "../../GraphViz/lib.rkt" "../../GraphViz/render-graph.rkt"
         "../structs/state.rkt" "../structs/machine.rkt" "../inv.rkt")

(provide scaled-graph create-png)


(define (scaled-graph img type)
  (let ([smallest (>= (- w (image-width img))
                      (- h (image-height img)))])
    (cond
      [(and (< (image-width img) (if (eq? 'pda type) (/ w-pda 2)(/ w 2)))
            (< (image-height img) (if (eq? 'pda type) (/ h-pda 2)(/ h 2))))
       (scale SMALL-IMG-SCALE img)]
      [smallest  (if (equal? type 'pda)
                     (scale (/ h-pda (image-height img)) img)
                     (scale (/ h (image-height img)) img))]
      [else (if (equal? type 'pda)
                (scale (/ w-pda (image-width img)) img)
                (scale (/ w (image-width img)) img))])))



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
  

