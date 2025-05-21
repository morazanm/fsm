#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         racket/treelist
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/vector-zipper.rkt"
         (except-in "../viz-lib/viz-constants.rkt"
                    INS-TOOLS-BUFFER)
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/mtape-tm.rkt" 
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    remake-mttm
                    FONT-SIZE)
         "default-informative-messages.rkt")

(provide mttm-viz)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
state -> the state at which the actions are applied | symbol
lota -> all the actions to applied to each tape     | (listof TM-actions)
|#
(struct half-rule (state lota) #:transparent)
#|
source -> the first of a mttm rule     | half-rule
destination -> the rest of a mttm rule | half-rule
|#
(struct rule (source destination) #:transparent)

;;tape is the input the tape
;;computations is a (listof computation) that attempt to consume the ci
;;accepting computations is (listof computation) for all accepting computations
;;accept-traces is a (listof configuration)
;;reject-traces is a (listof configuration)
;;M is the given machine
;;inv is a the (listof (state (listof symbol -> boolean)))
;;max-cmps is the max amount of transitions the machine can make
;;head-pos is the beginning head position of the tape=
(struct building-viz-state (tape
                            computations
                            tracked-accept-trace
                            tracked-reject-trace
                            all-accept-traces
                            all-reject-traces
                            M
                            inv
                            max-cmps
                            head-pos
                            machine-decision)
  #:transparent)

(define DUMMY-RULE (rule (half-rule LM LM) (half-rule LM LM)))

(define init-aux-tape (tape-config 0 (list BLANK)))
(define MIN-AUX-TAPE-INDEX 1) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;word natnum (listof rule) symbol symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word tape-amount lor start finals accepting-final max-cmps head-pos)

  ;; -> (listof tape-configs)
  ;;Purpose: Makes the initial tape configurations
  (define (make-init-tape-config)
    (cons (tape-config head-pos a-word)
          (make-list (sub1 tape-amount) init-aux-tape)))

  ;;(listof tm-action) (listof tape-config) -> boolean
  ;;Purpose: Determines if (listof tm-action) from a rule is applicable to the (listof tape-config)
  (define (applicable-rule? lota lotc)
    (andmap (λ (tc tm-action)
              (and (<= (tape-config-head-position tc) (length (tape-config-tape tc)))
                   (eq? tm-action (list-ref (tape-config-tape tc) (tape-config-head-position tc)))))
            lotc
            lota))
  
  ;;computation rule -> computation
  ;;Purpose: Applys the given rule to the given config and returns the updated computation
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-rule a-comp a-rule)

    ;;configuration -> configuration
    ;;Purpose: Applys the given rule to the given config and returns the updated configuraton 
    (define (apply-rule-helper a-config)
      ;;head-position tm-action -> head-position
      ;;Purpose: Applies the action portion of given rule to the given config to update the head position
      (define (update-head-position head-pos tm-action)
        (cond [(eq? tm-action RIGHT) (add1 (tape-config-head-position head-pos))]
              [(eq? tm-action LEFT)  (sub1 (tape-config-head-position head-pos))]
              [else (tape-config-head-position head-pos)]))
      ;;tape head-position tm-action -> tape
      ;;Purpose: Updates the given tape by using the given tm-action at the given head-position
      (define (update-tape tape head-position tm-action)
        ;;tape -> tape
        ;;Purpose: "Mutates" the tape if possible 
        (define (mutate-tape tape)
          (if (or (eq? tm-action RIGHT)
                  (eq? tm-action LEFT))
              tape
              (append (take tape head-position)
                      (list tm-action)
                      (rest (drop tape head-position)))))
        ;;tape -> tape
        ;;Purpose: Adds a blank to the end of the tape
        (define (add-blank tape)
          (if (not (= head-position (length tape)))
              tape
              (append tape (list BLANK))))
        (add-blank (mutate-tape (tape-config-tape tape))))
    
      (let* ([new-head-positions (map update-head-position (mttm-config-lotc a-config) (half-rule-lota (rule-destination a-rule)))]
             [new-tapes (map update-tape (mttm-config-lotc a-config) new-head-positions (half-rule-lota (rule-destination a-rule)))])
        (struct-copy mttm-config a-config
                     [state (half-rule-state (rule-destination a-rule))]
                     [lotc (map (λ (head-pos tape) (tape-config head-pos tape)) new-head-positions new-tapes)]
                     [index (add1 (mttm-config-index a-config))])))
    
    (struct-copy computation a-comp
                 [LoC (treelist-add (computation-LoC a-comp) (apply-rule-helper (treelist-last (computation-LoC a-comp))))]
                 [LoR (treelist-add (computation-LoR a-comp) a-rule)]
                 [visited (set-add (computation-visited a-comp) (treelist-last (computation-LoC a-comp)))]))

  ;;set
  ;;the set of final states
  (define finals-set (list->seteq finals))

  ;;(queueof computation) (treelistof computation) -> (listof computation)
  ;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
  ;;     that are within the bounds of the max computation limit
  (define (make-computations QoC path)
    (if (qempty? QoC)
        path
        (let* ([current-config (treelist-last (computation-LoC (qfirst QoC)))]
               [current-state (mttm-config-state current-config)]
               [current-lotc (mttm-config-lotc current-config)]
               [member-of-finals? (member? current-state finals eq?)]
               [reached-threshold? (> (treelist-length (computation-LoC (qfirst QoC))) max-cmps)])
          (if (or reached-threshold? member-of-finals?)
              (make-computations (dequeue QoC) (if (eq? current-state accepting-final)
                                                   (struct-copy paths path
                                                                [accepting (treelist-add (paths-accepting path) (qfirst QoC))]
                                                                [reached-final? (cond [(paths-reached-final? path) (paths-reached-final? path)]
                                                                                      [member-of-finals? #t]
                                                                                      [else (paths-reached-final? path)])]
                                                                [cut-off? (cond [(paths-cut-off? path) (paths-cut-off? path)]
                                                                                [reached-threshold? #t]
                                                                                [else (paths-cut-off? path)])])
                                                   (struct-copy paths path
                                                                [rejecting (treelist-add (paths-rejecting path) (qfirst QoC))]
                                                                [reached-final? (cond [(paths-reached-final? path) (paths-reached-final? path)]
                                                                                      [member-of-finals? #t]
                                                                                      [else (paths-reached-final? path)])]
                                                                [cut-off? (cond [(paths-cut-off? path) (paths-cut-off? path)]
                                                                                [reached-threshold? #t]
                                                                                [else (paths-cut-off? path)])])))
              (let* (;;(listof rules)
                     ;;Purpose: Filters all the rules that can be applied to the configuration by reading the element in the rule
                     [connected-read-rules (treelist-filter (λ (rule)
                                                              (and (eq? (half-rule-state (rule-source rule)) current-state)
                                                                   (applicable-rule? (half-rule-lota (rule-source rule)) current-lotc)))
                                                            lor)]
                     ;;(listof computation)
                     [new-configs (treelist-filter (λ (new-c) 
                                                     (not (set-member? (computation-visited (qfirst QoC)) (treelist-last (computation-LoC new-c)))))
                                                   (treelist-map connected-read-rules (λ (rule) (apply-rule (qfirst QoC) rule))))])
                ;new-configs
                (if (treelist-empty? new-configs)
                    (make-computations (dequeue QoC) (struct-copy paths path [rejecting (treelist-add (paths-rejecting path) (qfirst QoC))]))
                    (make-computations (enqueue new-configs (dequeue QoC)) path)))))))
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (treelist (mttm-config start (make-init-tape-config) 0))
                                           empty-treelist
                                           (set)
                                           1)])
    (make-computations (enqueue (treelist starting-computation) E-QUEUE) (paths empty-treelist empty-treelist #f #f))))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  ;(listof symbols) -> string
  ;;Purpose: Converts the given los into a string
  (define (make-edge-label rule)
    ;;(listof TM-actions) -> string
    ;;Purpose: Makes the actions portions of the rule
    (define (make-actions lota acc)
      (if (empty? lota)
          (string-append acc ")")
          (make-actions (rest lota) (string-append acc (symbol->string (first lota))))))
    (let ([source-actions (half-rule-lota (rule-source rule))]
          [destination-actions (half-rule-lota (rule-destination rule))])
      (string-append "\n["
                     (make-actions source-actions "(")
                     (make-actions destination-actions "(")
                     "]")))
  (map2 (λ (rule)
         (list (rule-source rule)
               (string->symbol (make-edge-label rule))
               (rule-destination rule)))
       rules))

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

;;(listof trace) (X -> Y) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-X LoT map-func)
  (filter-map-acc empty? map-func not first LoT))

;(listof symbol ((listof symbol) (listof symbol) -> boolean))) (X -> Y) -> (listof symbol ((listof symbol) (listof symbol) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) (listof symbols) -> boolean)))
(define (get-invariants inv func)
  (if (func (second inv))
      (mttm-config-state (first inv))
      '()))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not identity LoT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (let ([member-of-held-inv? (member? state held-inv eq?)]
                 [member-of-fail-inv? (member? state fail-inv eq?)])
             (add-node graph
                       state
                       #:atb (hash 'color (if (eq? (mttm-start M) state) 'green 'black)
                                   'style (cond [(and member-of-held-inv? member-of-fail-inv?) 'wedged]
                                                [(or member-of-held-inv? member-of-fail-inv?
                                                     (member? state cut-off equal?)) 'filled]
                                                [else 'solid])
                                   'shape (cond [(eq? state (mttm-accepting-final M)) 'doubleoctagon]
                                                [(member? state (mttm-finals M) equal?) 'doublecircle]
                                                [else 'circle])
                                   'fillcolor (cond [(member? state cut-off equal?) GRAPHVIZ-CUTOFF-GOLD]
                                                    [(and member-of-held-inv? member-of-fail-inv?) SPLIT-INV-COLOR]
                                                    [member-of-held-inv? HELD-INV-COLOR ]
                                                    [member-of-fail-inv? BRKN-INV-COLOR]
                                                    [else 'white])
                                   'label state
                                   'fontcolor 'black
                                   'fontname (if (and (member? state held-inv equal?) (member? state fail-inv equal?))
                                                 "times-bold"
                                                 "Times-Roman")))))
         dgraph
         (mttm-states M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-tracked-rules current-accept-rules current-reject-rules accepted?)
  ;;rule symbol (listof rules) -> boolean
  ;;Purpose: Determines if the given rule is a member of the given (listof rules)
  ;;         or similiar to one of the rules in the given (listof rules) 
  (define (find-rule? rule lor)
    (or (member? rule lor equal?)
        (ormap (λ (r)
                 (and (equal? (first rule) (first r))
                      (equal? (third rule) (third r))))
               lor)))
  (foldl (λ (rule graph)
           (let ([found-tracked-rule? (find-rule? rule current-tracked-rules)]
                 [found-accept-rule? (find-rule? rule current-accept-rules)]
                 [found-reject-rule? (find-rule? rule current-reject-rules)])
             (add-edge graph
                       (second rule)
                       (first rule)
                       (third rule)
                       #:atb (hash 'color (cond [(and found-tracked-rule? found-accept-rule?) SPLIT-ACCEPT-COLOR] ;;<--- watch out if coloring issue
                                                [(and found-tracked-rule? found-reject-rule?) SPLIT-REJECT-COLOR] ;;<--- watch out if coloring issue
                                                [(and accepted? found-tracked-rule?) TRACKED-ACCEPT-COLOR]
                                                [found-tracked-rule?  TRACKED-REJECT-COLOR]
                                                [found-accept-rule? ALL-ACCEPT-COLOR]
                                                [found-reject-rule? REJECT-COLOR]
                                                [else 'black])
                                   'style (if (and found-tracked-rule? accepted?) 'bold 'solid) ;;<--- watch out if bolding issue
                                   'fontsize 12))))
         dgraph
         rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  ;;(listof rules)
  ;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
  (define (make-rule-triples rules)
    ;(listof symbols) -> string
    ;;Purpose: Converts the given los into a string
    (define (make-edge-label rule)
      ;;(listof TM-actions) -> string
      ;;Purpose: Makes the actions portions of the rule
      (define (make-actions lota acc)
        (if (empty? lota)
            (string-append acc ")")
            (make-actions (rest lota) (string-append acc (symbol->string (first lota))))))
      (let ([source-actions (half-rule-lota (rule-source rule))]
            [destination-actions (half-rule-lota (rule-destination rule))])
        (string-append "\n["
                       (make-actions source-actions "(")
                       (make-actions destination-actions "(")
                       "]")))
    (map2 (λ (rule)
           (list (half-rule-state (rule-source rule))
                 (string->symbol (make-edge-label rule))
                 (half-rule-state (rule-destination rule))))
         rules))
  
  ;;(listof rules) -> (listof rules)
  ;;Purpose: Converts the given (listof configurations)s to rules
  (define (configs->rules a-config)
    (make-rule-triples
     (remove-duplicates (filter (λ (rule)
                                  (not (equal? rule DUMMY-RULE)))
                                a-config))))
  ;;configuration configuration -> boolean
  ;;Purpose: Determines if the given invariant is the same as the given current config
  (define (same-config? inv-config current-config)
    (and (equal? (mttm-config-state (first inv-config)) (mttm-config-state current-config))
         (equal? (mttm-config-lotc  (first inv-config)) (mttm-config-lotc  current-config))
         (equal? (mttm-config-index (first inv-config)) (mttm-config-index current-config))))
  
  (let* (;;(listof configuration)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs (remove-duplicates (map2 treelist-first (building-viz-state-computations a-vs)))]

         ;;(listof symbol)
         ;;Purpose: Gets the states where it's computation has cutoff
         [cut-off-states (if cut-off
                             (remove-duplicates (filter (λ (state)
                                                          (not (equal? state (mttm-accepting-final (building-viz-state-M a-vs)))))
                                                        (map2 (λ (comp) (mttm-config-state (treelist-first comp)))
                                                             (building-viz-state-computations a-vs))))
                             '())]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from of shown accepting computation
         [tracked-rules (get-trace-X (if (eq? (building-viz-state-machine-decision a-vs) 'accept)
                                         (building-viz-state-tracked-accept-trace a-vs)
                                         (building-viz-state-tracked-reject-trace a-vs))
                                     trace-rules)]
         
         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from all of the accepting computations
         [all-accepting-rules (get-trace-X (building-viz-state-all-accept-traces a-vs) trace-rules)]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from all of the rejecting computations
         [all-rejecting-rules (get-trace-X (building-viz-state-all-reject-traces a-vs) trace-rules)]
         
         ;;(listof rule)
         ;;Purpose: Converts the current rules from the rejecting computations and makes them usable for graphviz
         [current-reject-rules (configs->rules all-rejecting-rules)]
                  
         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [all-current-accept-rules (configs->rules all-accepting-rules)]

         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-tracked-rules (configs->rules tracked-rules)]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (filter (λ (rule)
                                                 (not (equal? (first (half-rule-lota (rule-source rule))) LM)))
                                               (treelist->list (mttm-rules (building-viz-state-M a-vs)))))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (same-config? invs curr))
                     invs)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (map2 (λ (inv) (get-invariants inv not)) get-invs)]
         
         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (map2 (λ (inv) (get-invariants inv id)) get-invs)])
    (make-edge-graph
     (make-node-graph
      (create-graph 'mttmgraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      held-invs
      brkn-invs
      cut-off-states)
     all-rules
     current-tracked-rules
     all-current-accept-rules
     current-reject-rules
     (eq? (building-viz-state-machine-decision a-vs) 'accept))))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(ormap (λ (comp-len) (>= (mttm-config-index comp-len) (building-viz-state-max-cmps a-vs)))
                (map2 treelist-first (building-viz-state-computations a-vs)))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [(and (zipper-at-end? (building-viz-state-tape a-vs))
              (zipper-at-end? (building-viz-state-head-pos a-vs)))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy
                                      building-viz-state
                                      a-vs
                                      [computations (filter (λ (comp) (not (treelist-empty? comp)))
                                                            (map2 treelist-rest (building-viz-state-computations a-vs)))]
                                      [tape (if (zipper-at-end? (building-viz-state-tape a-vs))
                                                (building-viz-state-tape a-vs)
                                                (zipper-next (building-viz-state-tape a-vs)))]
                                      [head-pos (if (zipper-at-end? (building-viz-state-head-pos a-vs))
                                                    (building-viz-state-head-pos a-vs)
                                                    (zipper-next (building-viz-state-head-pos a-vs)))]
                                      [tracked-accept-trace (get-next-traces (building-viz-state-tracked-accept-trace a-vs))]
                                      [tracked-reject-trace (get-next-traces (building-viz-state-tracked-reject-trace a-vs))]
                                      [all-accept-traces (get-next-traces (building-viz-state-all-accept-traces a-vs))]
                                      [all-reject-traces (get-next-traces (building-viz-state-all-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-mttm-rules-used (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-mttm-tapes (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-mttm-head-positions (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-mttm-computation-lengths (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-mttm-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-rejecting-trace (imsg-state-mttm-shown-rejecting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-mttm-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy
         imsg-state-mttm 
         (informative-messages-component-state (viz-state-informative-messages a-vs))
         ;;rules-used
         [rules-used (if (or (zipper-empty? imsg-state-rules-used) (zipper-at-end? imsg-state-rules-used))
                         imsg-state-rules-used 
                         (zipper-next imsg-state-rules-used))]
         ;;tapes
         [tapes (if (or (zipper-empty? imsg-state-tape) (zipper-at-end? imsg-state-tape))
                    imsg-state-tape 
                    (zipper-next imsg-state-tape))]
         ;;head-positions
         [head-positions (if (or (zipper-empty? imsg-state-head-position) (zipper-at-end? imsg-state-head-position))
                             imsg-state-head-position
                             (zipper-next imsg-state-head-position))]
         ;;computation-lengths
         [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths) (zipper-at-end? imsg-state-computation-lengths))
                                  imsg-state-computation-lengths 
                                  (zipper-next imsg-state-computation-lengths))]
         ;;shown-accepting-trace
         [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                        (zipper-at-end? imsg-state-shown-accepting-trace))
                                    imsg-state-shown-accepting-trace
                                    (zipper-next imsg-state-shown-accepting-trace))]
         ;;shown-rejecting-trace
         [shown-rejecting-trace (if (or (zipper-empty? imsg-state-shown-rejecting-trace)
                                        (zipper-at-end? imsg-state-shown-rejecting-trace))
                                    imsg-state-shown-rejecting-trace
                                    (zipper-next imsg-state-shown-rejecting-trace))]
         ;;invs-zipper
         [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                            [(and (not (zipper-at-end? imsg-state-invs-zipper))
                                  (>= (get-mttm-config-index-frm-trace imsg-state-shown-accepting-trace)
                                      (fourth (first (zipper-unprocessed imsg-state-invs-zipper)))))
                             (zipper-next imsg-state-invs-zipper)]
                            [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-mttm-rules-used (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-mttm-tapes (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-mttm-head-positions (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-mttm-computation-lengths (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-mttm-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-rejecting-trace (imsg-state-mttm-shown-rejecting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-mttm-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy
         imsg-state-mttm
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         ;;rules-used
         [rules-used (if (or (zipper-empty? imsg-state-rules-used) (zipper-at-end? imsg-state-rules-used))
                         imsg-state-rules-used 
                         (zipper-to-end imsg-state-rules-used))]
         ;;tapes
         [tapes (if (or (zipper-empty? imsg-state-tape) (zipper-at-end? imsg-state-tape))
                    imsg-state-tape
                    (zipper-to-end imsg-state-tape))]
         ;;head-positions
         [head-positions (if (or (zipper-empty? imsg-state-head-position) (zipper-at-end? imsg-state-head-position))
                             imsg-state-head-position
                             (zipper-to-end imsg-state-head-position))]
         ;;computation-lengths
         [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths) (zipper-at-end? imsg-state-computation-lengths))
                                  imsg-state-computation-lengths 
                                  (zipper-to-end imsg-state-computation-lengths))]
         ;;shown-accepting-trace
         [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                        (zipper-at-end? imsg-state-shown-accepting-trace))
                                    imsg-state-shown-accepting-trace
                                    (zipper-to-end imsg-state-shown-accepting-trace))]
         ;;shown-rejecting-trace
         [shown-rejecting-trace (if (or (zipper-empty? imsg-state-shown-rejecting-trace)
                                        (zipper-at-end? imsg-state-shown-rejecting-trace))
                                    imsg-state-shown-rejecting-trace
                                    (zipper-to-end imsg-state-shown-rejecting-trace))]
         ;;invs-zipper
         [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper) (zipper-at-end? imsg-state-invs-zipper))
                          imsg-state-invs-zipper
                          (zipper-to-end imsg-state-invs-zipper))])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-mttm-rules-used (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-mttm-tapes (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-mttm-head-positions (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-mttm-computation-lengths (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-mttm-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-rejecting-trace (imsg-state-mttm-shown-rejecting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-mttm-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy
         imsg-state-mttm
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         ;;rules-used
         [rules-used (if (or (zipper-empty? imsg-state-rules-used)
                             (zipper-at-begin? imsg-state-rules-used))
                         imsg-state-rules-used 
                         (zipper-prev imsg-state-rules-used))]
         ;;tapes
         [tapes (if (or (zipper-empty? imsg-state-tape)
                        (zipper-at-begin? imsg-state-tape))
                    imsg-state-tape
                    (zipper-prev imsg-state-tape))]
         ;;head-positions
         [head-positions (if (or (zipper-empty? imsg-state-head-position)
                                 (zipper-at-begin? imsg-state-head-position))
                             imsg-state-head-position
                             (zipper-prev imsg-state-head-position))]
         ;;computation-lengths
         [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths)
                                      (zipper-at-begin? imsg-state-computation-lengths))
                                  imsg-state-computation-lengths 
                                  (zipper-prev imsg-state-computation-lengths))]
         ;;shown-accepting-trace
         [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                        (zipper-at-begin? imsg-state-shown-accepting-trace))
                                    imsg-state-shown-accepting-trace
                                    (zipper-prev imsg-state-shown-accepting-trace))]
         ;;shown-rejecting-trace
         [shown-rejecting-trace (if (or (zipper-empty? imsg-state-shown-rejecting-trace)
                                        (zipper-at-begin? imsg-state-shown-rejecting-trace))
                                    imsg-state-shown-rejecting-trace
                                    (zipper-prev imsg-state-shown-rejecting-trace))]
         ;;invs-zipper
         [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                            [(and (not (zipper-at-begin? imsg-state-invs-zipper))
                                  (<= (get-mttm-config-index-frm-trace imsg-state-shown-accepting-trace)
                                      (fourth (first (zipper-processed imsg-state-invs-zipper)))))
                             (zipper-prev imsg-state-invs-zipper)]
                            [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
(define (up-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-mttm-rules-used (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-mttm-tapes (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-mttm-head-positions (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-mttm-computation-lengths (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-mttm-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-rejecting-trace (imsg-state-mttm-shown-rejecting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-mttm-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy
         imsg-state-mttm
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         ;;rules
         [rules-used (if (or (zipper-empty? imsg-state-rules-used)
                             (zipper-at-begin? imsg-state-rules-used))
                         imsg-state-rules-used 
                         (zipper-to-begin imsg-state-rules-used))]
         ;;tape
         [tapes (if (or (zipper-empty? imsg-state-tape)
                        (zipper-at-begin? imsg-state-tape))
                    imsg-state-tape
                    (zipper-to-begin imsg-state-tape))]
         ;;head-position
         [head-positions (if (or (zipper-empty? imsg-state-head-position)
                                 (zipper-at-begin? imsg-state-head-position))
                             imsg-state-head-position
                             (zipper-to-begin imsg-state-head-position))]
         ;;computation-lengths
         [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths)
                                      (zipper-at-begin? imsg-state-computation-lengths))
                                  imsg-state-computation-lengths 
                                  (zipper-to-begin imsg-state-computation-lengths))]
         ;;shown-accepting-trace
         [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                        (zipper-at-begin? imsg-state-shown-accepting-trace))
                                    imsg-state-shown-accepting-trace
                                    (zipper-to-begin imsg-state-shown-accepting-trace))]
         ;;show-rejecting-trace
         [shown-rejecting-trace (if (or (zipper-empty? imsg-state-shown-rejecting-trace)
                                        (zipper-at-begin? imsg-state-shown-rejecting-trace))
                                    imsg-state-shown-rejecting-trace
                                    (zipper-to-begin imsg-state-shown-rejecting-trace))]
         ;;invariant-zipper
         [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper)
                              (zipper-at-begin? imsg-state-invs-zipper))
                          imsg-state-invs-zipper
                          (zipper-to-begin imsg-state-invs-zipper))])])])))

;;viz-state -> viz-state
;;Purpose: Scrolls the auxillary tapes down
(define (f-key-pressed a-vs)
  (let ([imsg-state-aux-tape-index (imsg-state-mttm-aux-tape-index (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-M (imsg-state-mttm-M (informative-messages-component-state (viz-state-informative-messages a-vs)))])
    (if (and (> (mttm-tape-amount imsg-state-M) 4)
             (= imsg-state-aux-tape-index (- (mttm-tape-amount imsg-state-M) 3)))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy
             imsg-state-mttm
             (informative-messages-component-state
              (viz-state-informative-messages a-vs))
             [aux-tape-index (add1 imsg-state-aux-tape-index)])])]))))

;;viz-state -> viz-state
;;Purpose: Scrolls the auxillary tapes up
(define (e-key-pressed a-vs)
  (let ([imsg-state-aux-tape-index (imsg-state-mttm-aux-tape-index (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-M (imsg-state-mttm-M (informative-messages-component-state (viz-state-informative-messages a-vs)))])
    (if (and (> (mttm-tape-amount imsg-state-M) 4) (= imsg-state-aux-tape-index MIN-AUX-TAPE-INDEX))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy
             imsg-state-mttm
             (informative-messages-component-state
              (viz-state-informative-messages a-vs))
             [aux-tape-index (sub1 imsg-state-aux-tape-index)])])]))))


;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy informative-messages
                   (viz-state-informative-messages a-vs)
                   [component-state
                    (struct-copy imsg-state-mttm
                                 a-imsgs
                                 [word-img-offset 0]
                                 [scroll-accum 0])])])))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy viz-state
                 a-vs
                 [informative-messages
                  (struct-copy informative-messages
                               (viz-state-informative-messages a-vs)
                               [component-state
                                (struct-copy imsg-state-mttm
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-mttm-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (let* ([imsg-state-rules-used (imsg-state-mttm-rules-used (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))]
         [imsg-state-tape (imsg-state-mttm-tapes (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [imsg-state-head-position (imsg-state-mttm-head-positions (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))]
         [imsg-state-computation-lengths (imsg-state-mttm-computation-lengths (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs)))]
         [imsg-state-shown-accepting-trace (imsg-state-mttm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
         [imsg-state-shown-rejecting-trace (imsg-state-mttm-shown-rejecting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
         [imsg-state-invs-zipper (imsg-state-mttm-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))]
         [inv-config-frm-trace (get-mttm-config-index-frm-trace (if (eq? (mttm-type (imsg-state-mttm-M (informative-messages-component-state
                                                                                                        (viz-state-informative-messages a-vs))))
                                                                         'mttm-language-recognizer)
                                                                    imsg-state-shown-accepting-trace
                                                                    imsg-state-shown-rejecting-trace))])
    (if (or (zipper-empty? imsg-state-invs-zipper)
            (and (zipper-at-begin? imsg-state-invs-zipper)
                 (not (zipper-at-end? imsg-state-invs-zipper)))
            (< inv-config-frm-trace (get-mttm-config-index-frm-invs imsg-state-invs-zipper)))
        a-vs
        (let* ([zip (if (and (not (zipper-at-begin? imsg-state-invs-zipper))
                             (<= inv-config-frm-trace (get-mttm-config-index-frm-invs imsg-state-invs-zipper)))
                        (zipper-prev imsg-state-invs-zipper)
                        imsg-state-invs-zipper)]
               [next-inv-index (get-mttm-config-index-frm-invs zip)])
          (struct-copy
           viz-state
           a-vs
           [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) next-inv-index)]
           [informative-messages
            (struct-copy
             informative-messages
             (viz-state-informative-messages a-vs)
             [component-state
              (struct-copy
               imsg-state-mttm
               (informative-messages-component-state
                (viz-state-informative-messages a-vs))
               ;;rules
               [rules-used (zipper-to-idx imsg-state-rules-used next-inv-index)]
               ;;tape
               [tapes (zipper-to-idx imsg-state-tape next-inv-index)]
               ;;head-position
               [head-positions (zipper-to-idx imsg-state-head-position next-inv-index)]
               ;;computation-lengths
               [computation-lengths (zipper-to-idx imsg-state-computation-lengths next-inv-index)]
               ;;Shown accepting trace
               [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                          imsg-state-shown-accepting-trace
                                          (zipper-to-idx imsg-state-shown-accepting-trace next-inv-index))]
               ;;Shown rejecting trace
               [shown-rejecting-trace (if (zipper-empty? imsg-state-shown-rejecting-trace)
                                          imsg-state-shown-rejecting-trace
                                          (zipper-to-idx imsg-state-shown-rejecting-trace next-inv-index))]
               ;;invs-zipper
               [invs-zipper zip])])])))))


;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (let* ([imsg-state-rules-used (imsg-state-mttm-rules-used (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))]
         [imsg-state-tape (imsg-state-mttm-tapes (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [imsg-state-head-position (imsg-state-mttm-head-positions (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))]
         [imsg-state-computation-lengths (imsg-state-mttm-computation-lengths (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs)))]
         [imsg-state-shown-accepting-trace (imsg-state-mttm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
         [imsg-state-shown-rejecting-trace (imsg-state-mttm-shown-rejecting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
         [imsg-state-invs-zipper (imsg-state-mttm-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))]
         [inv-config-frm-trace (get-mttm-config-index-frm-trace (if (eq? (mttm-type (imsg-state-mttm-M (informative-messages-component-state
                                                                                                        (viz-state-informative-messages a-vs))))
                                                                         'mttm-language-recognizer)
                                                                    imsg-state-shown-accepting-trace
                                                                    imsg-state-shown-rejecting-trace))])
    (if (or (zipper-empty? imsg-state-invs-zipper)
            (and (zipper-at-end? imsg-state-invs-zipper)
                 (not (zipper-at-begin? imsg-state-invs-zipper)))
            (> inv-config-frm-trace (get-mttm-config-index-frm-invs imsg-state-invs-zipper)))
        a-vs
        (let* ([zip (if (and (not (zipper-at-end? imsg-state-invs-zipper))
                             (>= inv-config-frm-trace (get-mttm-config-index-frm-invs imsg-state-invs-zipper)))
                        (zipper-next imsg-state-invs-zipper)
                        imsg-state-invs-zipper)]
               [next-inv-index (get-mttm-config-index-frm-invs zip)])
          (struct-copy
           viz-state
           a-vs
           [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) next-inv-index)]
           [informative-messages
            (struct-copy
             informative-messages
             (viz-state-informative-messages a-vs)
             [component-state
              (struct-copy
               imsg-state-mttm
               (informative-messages-component-state (viz-state-informative-messages a-vs))
               ;;rules
               [rules-used (zipper-to-idx imsg-state-rules-used next-inv-index)]
               ;;tape
               [tapes (zipper-to-idx imsg-state-tape next-inv-index)]
               ;;head-position
               [head-positions (zipper-to-idx imsg-state-head-position next-inv-index)]
               ;;computation-lengths
               [computation-lengths (zipper-to-idx imsg-state-computation-lengths next-inv-index)]
               ;;Shown accepting trace
               [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                          imsg-state-shown-accepting-trace
                                          (zipper-to-idx imsg-state-shown-accepting-trace next-inv-index))]
               ;;Shown rejecting trace
               [shown-rejecting-trace (if (zipper-empty? imsg-state-shown-rejecting-trace)
                                          imsg-state-shown-rejecting-trace
                                          (zipper-to-idx imsg-state-shown-rejecting-trace next-inv-index))]
               ;;invariant-zipper
               [invs-zipper zip])])])))))

;;mttm tape [natnum] [natnum] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (mttm-viz M a-word head-pos #:cut-off [cut-off 100] invs)
  ;;Mttm -> mttm-struct
  ;;Purpose: Converts a mttm interface into the mttm structure
  (define (remake-mttm M)
    ;;(listof rule) -> (treelistof rule-struct)
    ;;Purose: Converts a rule into a rule-struct
    (define (remake-rules a-lor)
      (for/treelist ([mttm-rule a-lor])
        (rule (half-rule (first (first mttm-rule)) (second (first mttm-rule)))
              (half-rule (first (second mttm-rule)) (second (second mttm-rule))))))
    (mttm (mttm-get-states M)
          (mttm-get-sigma M)
          (mttm-get-start M)
          (mttm-get-finals M)
          (remake-rules (mttm-get-rules M))
          (M 'get-numtapes)
          (if (eq? (mttm-what-am-i M) 'mttm-language-recognizer) (mttm-get-accept M) 'none)
          (mttm-what-am-i M)))

  ;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
  ;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
  ;;         tracks each transition
  (define (make-trace configs rules acc)
    (cond [(treelist-empty? rules) (reverse acc)]
          [(and (empty? acc)
                (or (not (eq? (first (half-rule-lota (rule-source (treelist-first rules)))) BLANK))
                    (not (eq? (first (half-rule-lota (rule-source (treelist-first rules)))) LM))))
           (let ([res (trace (treelist-first configs) DUMMY-RULE)])
             (make-trace (treelist-rest configs) rules (cons res acc)))]
          [else (let ([res (trace (treelist-first configs) (treelist-first rules))])
                  (make-trace (treelist-rest configs) (treelist-rest rules) (cons res acc)))]))

  ;;(listof trace) (listof trace) -> (listof trace)
  ;;Purpose: Finds the longest computation for rejecting traces
  (define (find-longest-computation a-LoRT acc)
    (cond [(empty? a-LoRT) acc]
          [(> (length (first a-LoRT)) (length acc))
           (find-longest-computation (rest a-LoRT) (first a-LoRT))]
          [(find-longest-computation (rest a-LoRT) acc)]))
  
  ;;(listof configurations) (listof configurations) -> (listof configurations)
  ;;Purpose: Counts the number of unique configurations for each stage of the word
  (define (count-computations a-LoC acc)
    (if (empty? a-LoC)
        (reverse acc)
        (let [(new-LoC (filter-map (λ (comp) (and (not (treelist-empty? comp))
                                                  (treelist-rest comp)))
                                   a-LoC))
              (comp-len (length (remove-duplicates (filter-map (λ (comp) (and (not (treelist-empty? comp))
                                                                              (treelist-first comp)))
                                                               a-LoC))))]
          (count-computations new-LoC (cons comp-len acc)))))

  ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
  ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
  (define (get-inv-config-results computations invs)
    ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
    ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
    (define (get-inv-config-results-helper computations)
      ;;tape-config(structure) -> tape-config
      ;;Purpose: Converts the tape config structure into tape configs 
      (define (tc->tapes lotc)
        (for/list ([tc lotc])
          (list (tape-config-head-position tc) (tape-config-tape tc))))
      (if (treelist-empty? computations)
          '()
          (let* ([get-inv-for-inv-config (filter (λ (inv)
                                                   (equal? (first inv) (mttm-config-state (treelist-first computations))))
                                                 invs)]
                 [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                         '()
                                         (second (first get-inv-for-inv-config)))]
                 [inv-config-result (if (empty? inv-for-inv-config)
                                        '()
                                        (list (treelist-first computations)
                                              (inv-for-inv-config (tc->tapes (mttm-config-lotc (treelist-first computations))))))])
            (if (empty? inv-config-result)
                (get-inv-config-results-helper (treelist-rest computations))
                (cons inv-config-result
                      (get-inv-config-results-helper (treelist-rest computations)))))))
    (append-map (λ (comp)
                  (get-inv-config-results-helper comp))
                computations))

  ;;(listof configurations) (listof sybmols) -> (listof configurations)
  ;;Purpose: Extracts all the invariant configurations that failed
  (define (return-brk-inv-configs inv-config-results)
    (remove-duplicates (filter (λ (config) (not (second config))) inv-config-results)))
  
  (let* (;;tm-struct
         [M (remake-mttm M)]
         ;;paths ;Purpose: All computations that the machine can have seperated by accepting and rejecting and whether 
         [all-paths (get-computations a-word
                                      (mttm-tape-amount M)
                                      (mttm-rules M)
                                      (mttm-start M)
                                      (mttm-finals M)
                                      (mttm-accepting-final M)
                                      cut-off
                                      head-pos)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (treelist->list (paths-accepting all-paths))]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (treelist->list (paths-rejecting all-paths))]
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map2 computation-LoC (append accepting-computations rejecting-computations))]
         ;;boolean ;;Purpose: Determines if any computation 
         [reached-final? (paths-reached-final? all-paths)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (treelist->list (paths-accepting all-paths))]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (treelist->list (paths-rejecting all-paths))]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map2 (λ (acc-comp)
                                  (make-trace (computation-LoC acc-comp)
                                              (computation-LoR acc-comp)
                                              '()))
                                accepting-computations)]
         ;;boolean ;;Purpose: Determines if any computation reaches the cuts off treshold
         [computation-has-cut-off? (paths-cut-off? all-paths)]         
         ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
         [rejecting-traces (map2 (λ (comp)
                                  (make-trace (computation-LoC comp)
                                              (computation-LoR comp)
                                              '()))
                                rejecting-computations)]
         ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
         [accepting-trace (if (empty? accepting-traces) '() (first accepting-traces))]
         [rejecting-trace (if (empty? accepting-traces) (find-longest-computation rejecting-traces '()) '())]
         [all-tapes (map2 (λ (trace) (mttm-config-lotc (trace-config trace)))
                         (if (not (empty? accepting-trace)) #;(and (not computation-has-cut-off?) (not (empty? accepting-trace)))
                             accepting-trace
                             rejecting-trace))]
         [displayed-tape (for/list [(lotc all-tapes)]
                           (for/list [(tc lotc)] (tape-config-tape tc)))]
         [all-displayed-tape (list->zipper displayed-tape)]
         [tracked-head-pos (let ([head-positions (for/list [(lotc all-tapes)]
                                                   (for/list [(tc lotc)] (tape-config-head-position tc)))])
                             head-positions)]
                                 
                             
         [all-head-pos (list->zipper tracked-head-pos)]
         [machine-decision (if (not (empty? accepting-computations))
                               'accept
                               'reject)]
         
         [tracked-trace (list (if (not (empty? accepting-trace)) accepting-trace rejecting-trace))]
         ;;(listof (list config boolean)) ;;Purpose: Gets all the invariant configurations
         [all-inv-configs (if (empty? invs)
                              '()
                              (reverse (get-inv-config-results
                                        (if (and reached-final? (eq? (mttm-type M) 'mttm)) LoC (map2 computation-LoC accepting-computations))
                                        invs)))]
         ;;;;(listof (list config boolean)) ;;Purpose: Gets all the failed invariant configurations
         [failed-inv-configs (return-brk-inv-configs all-inv-configs)]
         ;;building-state struct
         [building-state (building-viz-state all-displayed-tape
                                             LoC
                                             (if (empty? accepting-traces) accepting-traces tracked-trace)
                                             (if (empty? accepting-traces) tracked-trace '())
                                             (if (empty? accepting-traces) accepting-traces (rest accepting-traces))
                                             (if (empty? accepting-traces) (filter (λ (config) (not (equal? config rejecting-trace)))
                                                                                   rejecting-traces)
                                                 rejecting-traces)
                                             M
                                             all-inv-configs
                                             cut-off
                                             all-head-pos
                                             machine-decision)]
                     
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [cut-off-computations-lengths (take (count-computations LoC '()) (length displayed-tape))]
         [MTTM-E-SCENE-HEIGHT (cond [(= (mttm-tape-amount M) 2) MTTM-2tape-E-SCENE-HEIGHT]
                                    [(= (mttm-tape-amount M) 3) MTTM-3tape-E-SCENE-HEIGHT]
                                    [else MTTM->=4tape-E-SCENE-HEIGHT])]
         [mttm-img-bounding-limit (cond [(= (mttm-tape-amount M) 2) mttm-2tape-img-bounding-limit]
                                        [(= (mttm-tape-amount M) 3) mttm-3tape-img-bounding-limit]
                                        [else mttm->=4tape-img-bounding-limit])]
         [mttm-info-img (cond [(= (mttm-tape-amount M) 2) mttm-2tape-info-img]
                              [(= (mttm-tape-amount M) 3) mttm-3tape-info-img]
                              [else mttm->=4tape-info-img])])
    (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ MTTM-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages mttm-create-draw-informative-message
                                   (imsg-state-mttm M
                                                    all-displayed-tape
                                                    all-head-pos
                                                    (list->zipper (map2 (λ (trace)
                                                                         (list (half-rule-lota (rule-source (trace-rules trace)))
                                                                               (half-rule-lota (rule-destination (trace-rules trace)))))
                                                                       (first tracked-trace)))
                                                    (list->zipper (if (empty? accepting-trace) accepting-trace (first tracked-trace)))
                                                    (list->zipper (if (empty? accepting-trace) (first tracked-trace) rejecting-trace))
                                                    (list->zipper failed-inv-configs) 
                                                    (list->zipper cut-off-computations-lengths)
                                                    cut-off
                                                    machine-decision
                                                    1
                                                    0
                                                    (let ([offset-cap (- (length a-word) TM-TAPE-SIZE)])
                                                      (if (> 0 offset-cap) 0 offset-cap))
                                                    0)
                                   mttm-img-bounding-limit)
             (instructions-graphic MTTM-E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       MTTM-E-SCENE-HEIGHT
                                                       (image-height mttm-info-img)
                                                       INS-TOOLS-BUFFER)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       MTTM-E-SCENE-HEIGHT
                                                       (image-height mttm-info-img)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))))
             (create-viz-draw-world E-SCENE-WIDTH MTTM-E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key [ "right" viz-go-next right-key-pressed]
                                     [ "left" viz-go-prev left-key-pressed]
                                     [ "up" viz-go-to-begin up-key-pressed]
                                     [ "down" viz-go-to-end down-key-pressed]
                                     [ "w" viz-zoom-in identity]
                                     [ "s" viz-zoom-out identity]
                                     [ "r" viz-max-zoom-out identity]
                                     [ "f" identity f-key-pressed]
                                     [ "e" identity e-key-pressed]
                                     [ "a" identity a-key-pressed]
                                     [ "d" identity d-key-pressed]
                                     [ "wheel-down" viz-zoom-in identity]
                                     [ "wheel-up" viz-zoom-out identity]
                                     [ "j" mttm-jump-prev j-key-pressed]
                                     [ "l" mttm-jump-next l-key-pressed]
                                     )
             (create-viz-process-tick (cond [(= (mttm-tape-amount M) 2) MTTM-2tape-E-SCENE-BOUNDING-LIMITS]
                                            [(= (mttm-tape-amount M) 3) MTTM-3tape-E-SCENE-BOUNDING-LIMITS]
                                            [else MTTM->=4tape-E-SCENE-BOUNDING-LIMITS])
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      MTTM-E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ( [mttm-img-bounding-limit
                                         (lambda (a-imsgs x-diff y-diff)
                                           (let ([new-scroll-accum (+ (imsg-state-mttm-scroll-accum a-imsgs) x-diff)])
                                             (cond
                                               [(and (>= (imsg-state-mttm-word-img-offset-cap a-imsgs)
                                                         (imsg-state-mttm-word-img-offset a-imsgs))
                                                     (<= (quotient (+ (imsg-state-mttm-scroll-accum a-imsgs) x-diff) 25) -1))
                                                (struct-copy imsg-state-mttm
                                                             a-imsgs
                                                             [word-img-offset (+ (imsg-state-mttm-word-img-offset a-imsgs) 1)]
                                                             [scroll-accum 0])]
                                               [(and (> (imsg-state-mttm-word-img-offset a-imsgs) 0)
                                                     (>= (quotient (+ (imsg-state-mttm-scroll-accum a-imsgs) x-diff) 25) 1))
                                                (struct-copy imsg-state-mttm
                                                             a-imsgs
                                                             [word-img-offset (- (imsg-state-mttm-word-img-offset a-imsgs) 1)]
                                                             [scroll-accum 0])]
                                               [else
                                                (struct-copy imsg-state-mttm
                                                             a-imsgs
                                                             [scroll-accum
                                                              (+ (imsg-state-mttm-scroll-accum a-imsgs) x-diff)])])))])
                                      ( [ ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [ W-KEY-DIMS viz-zoom-in identity]
                                        [ S-KEY-DIMS viz-zoom-out identity]
                                        [ R-KEY-DIMS viz-max-zoom-out identity]
                                        [ F-KEY-DIMS identity f-key-pressed]
                                        [ E-KEY-DIMS identity e-key-pressed]
                                        [ A-KEY-DIMS identity a-key-pressed]
                                        [ D-KEY-DIMS identity d-key-pressed]
                                        [ J-KEY-DIMS mttm-jump-prev j-key-pressed]
                                        [ L-KEY-DIMS mttm-jump-next l-key-pressed]))
             'mttm-viz)))