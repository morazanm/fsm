#lang racket
(require 2htdp/image "../globals.rkt" "../../GraphViz/lib.rkt" "../../GraphViz/render-graph.rkt"
         "../structs/state.rkt")

(provide scaled-graph create-png)





(define (scaled-graph img type)
  (cond
    [(<= (image-width img) (image-height img)) (if (equal? type 'pda)
                                                   (scale (/ h-pda (image-height img)) img)
                                                   (scale (/ h (image-height img)) img))]
    [else (if (equal? type 'pda)
              (scale (/ w-pda (image-width img)) img)
              (scale (/ w (image-width img)) img))]))



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
  

