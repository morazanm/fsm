#lang racket
(require "lib.rkt")

;; structure of a ndfa/dfa rule
(define (dfa/ndfa-rule? r)
  (listof symbol?))

;; structure of a pda/tm rule
(define (pda/tm-rule? r)
  (listof (listof symbol?) (listof symbol?)))

(provide
 (contract-out
  [struct fsa-adapter ([states (listof symbol?)]
                       [start symbol?]
                       [finals (listof symbol?)]
                       [rules (or/c (listof dfa/ndfa-rule?)
                                    (listof pda/tm-rule?))]
                       [type symbol?]
                       [accept (or/c boolean? symbol?)]
                       [cur-state (or/c boolean? symbol?)]
                       [cur-rule (or/c boolean?
                                       dfa/ndfa-rule?
                                       pda/tm-rule?)]
                       [cur-state-color string?])]
  [fsa-adapter->graph (-> fsa-adapter?
                          colorblind-opt?
                          graph?)]))



(define HIGHLIGHT-EDGE (hash
                        'color "#215dde"
                        'fontsize 15))


(struct fsa-adapter (states start finals rules type accept cur-state cur-rule cur-state-color) #:transparent)
(define (is-tm-lang-rec? type) (equal? type 'tm-language-recognizer))


(define (build-color-state-hash color)
  (hash
   'style "filled"
   'fillcolor color
   'shape "circle"))

;; state-type->node-type: symbol -> fsa-adapter -> symbol
;; determins the state type for the given state
(define (state-type->node-type state fsa)
  (cond
    [(and (is-tm-lang-rec? (fsa-adapter-type fsa))
          (equal? state (fsa-adapter-accept fsa))) 'accept]
    [(and (equal? state (fsa-adapter-start fsa))
          (member state (fsa-adapter-finals fsa))) 'startfinal]
    [(equal? state (fsa-adapter-start fsa)) 'start]
    [(member state (fsa-adapter-finals fsa)) 'final]
    [else 'none]))

;; fsa-states->nodes: fsa-adapter -> graph -> graph
;; Adds all the machine states to the graph
(define (fsa-states->nodes fsa graph)
  (define (fsa-state->node state graph)
    (if (equal? state (fsa-adapter-cur-state fsa))
        (add-node graph state (state-type->node-type
                               state
                               fsa
                               #:atb (build-color-state-hash (fsa-adapter-cur-state-color fsa))))
        (add-node graph state (state-type->node-type state fsa))))
  (foldl fsa-state->node graph (fsa-adapter-states fsa)))


;; fsa-rules->edges :: fsa-adapter -> graph -> graph
;; Adds all the machines rules to the graph
(define (fsa-rules->edges fsa graph)
  (define (parse-rule rule)
    (if (not rule)
        (list #f #f)
        (match (fsa-adapter-type fsa)
          [(or 'dfa 'ndfa) (list (first rule) (last rule))]
          [_ (list (caar rule) (caadr rule))])))
  (match-define `(,start-state ,end-state) (parse-rule (fsa-adapter-cur-rule fsa)))
  (define (fsa-rule->edge rule graph)
    (match-define `(,start ,end) (parse-rule rule))
    (define rule-label (if (or (equal? (fsa-adapter-type fsa) 'dfa) (equal? (fsa-adapter-type fsa) 'ndfa))
                           (cadr rule)
                           rule))
    (if (and (equal? start start-state) (equal? end end-state))
        (add-edge graph rule-label start end #:atb HIGHLIGHT-EDGE)
        (add-edge graph rule-label start end)))
  (foldl fsa-rule->edge graph (fsa-adapter-rules fsa)))



;; fsa-adapter->graph :: fsa-adapter -> graph
;; Converts the structure to a graph
(define (fsa-adapter->graph adapter color-blind-mode)
  (fsa-rules->edges adapter
                    (fsa-states->nodes adapter
                                       (create-graph 'G #:color color-blind-mode))))