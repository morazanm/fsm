#lang racket
(require
  2htdp/image
  "../globals.rkt"
  "../../GraphViz/lib.rkt"
  "../../GraphViz/render-graph.rkt"
  "../structs/state.rkt"
  "../structs/machine.rkt"
  "../inv.rkt")

(provide
 world->graph-png)


(define (scaled-graph img type w h)
  (let ([smallest (>= (- w (image-width img))
                      (- h (image-height img)))])
    (cond
      [(and (< (image-width img) (/ w 2))
            (< (image-height img) (/ h 2)))
       (scale SMALL-IMG-SCALE img)]
      [smallest (scale (/ h (image-height img)) img)]
      [else (scale (/ w (image-width img)) img)])))



(define (world->graph-png hasRun? world width height)
  (define machine (get-field machine world))
  (define cur-state (get-field cur-state world))
  (define cur-rule (get-field cur-rule world))
  (define type (get-field type world))
  (define g (create-graph 'G #:color 0))
  (define states (machine-state-list machine))
  (define start (machine-start-state machine))
  (define finals (machine-final-state-list machine))
  (define rules (machine-rule-list machine))
  (define color (if hasRun?
                    (determin-inv machine
                                  cur-state
                                  (get-field tape-position world)
                                  #:graphViz true)
                    #f))
  (define (rule-start) (case type
                         [(dfa) (car cur-rule)]
                         [(ndfa) (car cur-rule)]
                         [else (caar cur-rule)]))
  (define (rule-end) (case type
                       [(dfa) (last cur-rule)]
                       [(ndfa) (last cur-rule)]
                       [else (caadr cur-rule)]))
  (define new-states (map (lambda (s) (fsm-state-name s)) states))
  (begin
    (states->nodes new-states
                   start
                   finals
                   g
                   #:cur-state cur-state
                   #:color (HIGHLIGHT-NODE color))
    (rules->edges rules
                  type
                  g
                  (if (or (member 'empty cur-rule) (member 'null cur-rule)) "$NULL" (rule-start))
                  (if (or (member 'empty cur-rule) (member 'null cur-rule)) "$NULL" (rule-end))
                  "black")
    (if (or (member 'empty cur-rule) (member 'null cur-rule))
        (scaled-graph (graph->png g) type width height)
        (scaled-graph (graph->png g #:rule cur-rule) type width height))))
  

