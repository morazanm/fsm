#lang racket
(require "lib.rkt")

;; structure of a ndfa/dfa rule
(define (dfa/ndfa-rule? r)
  (listof symbol?))

;; structure of a pda/tm rule
(define (pda/tm-rule? r)
  (listof (listof symbol?) (listof symbol?)))

;; valid invariant state options
(define (inv-state? o)
  (member  o '(pass fail none)))

(define hex-string? string?)

(provide
 dfa/ndfa-rule?
 pda/tm-rule?
 inv-state?
 (contract-out
  [struct color-palette ([start hex-string?]
                         [inv-true hex-string?]
                         [inv-false hex-string?]
                         [edge-highlight hex-string?])]
  [struct fsa-adapter ([states (listof symbol?)]
                       [start (or/c null? symbol?)]
                       [finals (listof symbol?)]
                       [rules (or/c (listof dfa/ndfa-rule?)
                                    (listof pda/tm-rule?))]
                       [type symbol?]
                       [accept (or/c boolean? symbol?)]
                       [cur-state (or/c boolean? symbol?)]
                       [cur-rule (or/c boolean?
                                       dfa/ndfa-rule?
                                       pda/tm-rule?)]
                       [inv-state inv-state?]
                       [palette color-palette?])]
  [fsa-adapter->graph (-> fsa-adapter? graph?)]))


(struct fsa-adapter (states
                     start
                     finals
                     rules
                     type
                     accept
                     cur-state
                     cur-rule
                     inv-state ; 'none 'pass 'fail
                     palette) #:transparent)


;; holds the colors needed for the graph
(struct color-palette (start inv-true inv-false edge-highlight))

(define HIGHLIGHT-EDGE (hash
                        'color "#215dde"
                        'fontsize 15))
(define RULE-LIMIT 5)

(define (is-tm-lang-rec? type) (equal? type 'tm-language-recognizer))

;; rule-label->str: listof(rules) -> string
;; Purpose: Converts a list of rules to a graphviz label
(define (rule-label->str rules)
  (define (format-line l acc count)
    (match l
      ['() (cons acc '())]
      [`(,x ,xs ...)
       (if (and (not (empty? acc))
                (> (+ 2 count (string-length x)) RULE-LIMIT))
           (cons acc l)
           (format-line xs (append acc (list x)) (+ count (string-length x))))]))
  (define (format-lines lines)
    (match-define (cons line xs) (format-line lines '() 0))
    (if (empty? lines)
        '()
        (cons (string-join line ", ") (format-lines xs))))
  (string-join (format-lines (map fsa-rule->label rules)) ",\n"))


;; fsa-rule->label: transition -> string
;; Purpose: Converts the list to its string representation
(define (fsa-rule->label aList)
  (define (list->str los accum)
    (match los
      ['() (string-append (string-trim accum) ")")]
      [`(,x ,xs ...) (list->str
                      xs
                      (string-append accum (stringify-value x) " "))]))
  (match aList
    ;; dfa/ndfa
    [(list _ input _) (stringify-value input)]
    ;; pda
    [(list (list _ read pop) (list _ push))
     (format "[~a ~a ~a]" (stringify-value read)
             (if (list? pop) (list->str pop "(") (stringify-value pop))
             (if (list? push) (list->str push "(") (stringify-value push)))]
    ;; tm and lang rec
    [(list (list _ b) (list _ d))
     (format "[~a ~a]" (stringify-value b) (stringify-value d))]
    ;; dfa/ndfa legacy way
    [val (stringify-value val)]))


;; build-node-hash :: symbol -> fsa-adapter -> hash
;; creates the attribute hash for a node
(define (build-node-hash state fsa)
  (define inv-state (fsa-adapter-inv-state fsa))
  (define is-cur-state? (equal? state (fsa-adapter-cur-state fsa)))
  (define palette (fsa-adapter-palette fsa))
  (define state-type (cond
                       [(and (is-tm-lang-rec? (fsa-adapter-type fsa))
                             (equal? state (fsa-adapter-accept fsa))) 'accept]
                       [(and (equal? state (fsa-adapter-start fsa))
                             (member state (fsa-adapter-finals fsa))) 'startfinal]
                       [(equal? state (fsa-adapter-start fsa)) 'start]
                       [(member state (fsa-adapter-finals fsa)) 'final]
                       [else 'default]))
  (define color (match state-type
                  [(or 'start 'startfinal 'startaccept)
                   (color-palette-start palette)]
                  [_ "black"]))
  (define shape (match state-type
                  [(or 'startaccept 'accept) "doubleoctagon"]
                  [(or 'final 'startfinal) "doublecircle"]
                  [_ "circle"]))
  (define attributes `((color . ,color) (shape . ,shape)))
  ;; If there is an invariant and its the current state we need to add the fillcolor
  (make-immutable-hash
   (if (and is-cur-state? (not (equal? 'none inv-state)))
       (append attributes
               `((style . "filled")
                 (fillcolor . ,(match inv-state
                                 ['pass (color-palette-inv-true palette)]
                                 ['fail (color-palette-inv-false palette)]))))
       attributes)))

;; fsa-states->nodes: fsa-adapter -> graph -> graph
;; Adds all the machine states to the graph
(define (fsa-states->nodes fsa graph)
  (define (fsa-state->node state graph)
    (add-node graph
              state
              #:atb (build-node-hash state fsa)))
  (foldl fsa-state->node graph (fsa-adapter-states fsa)))


;; fsa-rules->edges :: fsa-adapter -> graph -> graph
;; Adds all the machines rules to the graph
(define (fsa-rules->edges fsa graph)
  (define palette (fsa-adapter-palette fsa))
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
        (add-edge graph
                  rule-label
                  start
                  end
                  #:atb (hash 'color  (color-palette-edge-highlight palette)
                                                         'fontsize 15))
        (add-edge graph rule-label start end)))
  (foldl fsa-rule->edge graph (fsa-adapter-rules fsa)))



;; fsa-adapter->graph :: fsa-adapter -> graph
;; Converts the structure to a graph
(define (fsa-adapter->graph adapter)
  (fsa-rules->edges adapter
                    (fsa-states->nodes adapter
                                       (create-graph
                                        'G
                                        #:fmtrs (formatters
                                                 (hash)
                                                 (hash)
                                                 (hash 'label rule-label->str))))))
