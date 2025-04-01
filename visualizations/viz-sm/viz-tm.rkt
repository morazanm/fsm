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
         "../../fsm-core/private/tm.rkt"
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE)
         "default-informative-messages.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                            all-accept-traces
                            all-reject-traces
                            M
                            inv
                            max-cmps
                            head-pos
                            machine-decision)
  #:transparent)

(define DUMMY-RULE (list (list BLANK BLANK) (list BLANK BLANK)))


;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start finals max-cmps head-pos)

;;computation rule -> computation
;;Purpose: Applys the given rule to the given config and returns the updated computation
;;ASSUMPTION: The given rule can be applied to the config
(define (apply-rule a-comp a-rule)

  ;;configuration -> configuration
  ;;Purpose: AppApplys the given rule to the given config and returns the updated configuraton 
  (define (apply-rule-helper a-config)
    ;;number -> number
    ;;Purpose: Applies the action portion of given rule to the given config to update the head position
    (define (apply-action head-pos)
      (cond [(eq? (rule-action a-rule) RIGHT) (add1 head-pos)]
            [(eq? (rule-action a-rule) LEFT)  (sub1 head-pos)]
            [else head-pos]))
    ;;tape -> tape
    ;;Purpose: "Mutates" the tape if possible 
    (define (update-tape tape head-position)
    

     (define (mutate-tape tape)
      (if (or (eq? (rule-action a-rule) RIGHT)
              (eq? (rule-action a-rule) LEFT))
          tape
          (append (take tape head-position)
                  (list (rule-action a-rule))
                  (rest (drop tape head-position)))))
   
    ;;tape -> tape
    ;;Purpose: Adds a blank to the end of the tape
    (define (add-blank tape)
      (if (not (= head-position (length tape)))
          tape
          (append tape (list BLANK))))
      
      (add-blank (mutate-tape tape)))
    
    (let ([new-head-position (apply-action (tm-config-head-position a-config))])
      (struct-copy tm-config a-config
                 [state (rule-destination a-rule)]
                 [head-position new-head-position]
                 [tape (update-tape (tm-config-tape a-config) new-head-position)]
                 [index (add1 (tm-config-index a-config))])))
    
  
  
  (struct-copy computation a-comp
               [LoC (treelist-add (computation-LoC a-comp) (apply-rule-helper (treelist-last (computation-LoC a-comp))))]
               [LoR (treelist-add (computation-LoR a-comp) a-rule)]))
  
  ;;mutable set
  ;;Purpose: holds all of the visited configurations
  (define visited-configuration-set (mutable-set))

  ;;configuration -> void
  ;;Purpose: updates the set of visited configurations
  (define (update-visited a-config)
    (set-add! visited-configuration-set a-config))

  ;;hash-set
  ;;Purpose: accumulates the number of computations in a hashset
  (define computation-number-hash (make-hash))
  
  ;;set
  ;;an empty set
  (define EMPTY-SET (set))

  ;;configuration word -> void
  ;;Purpose: updates the number of configurations using the given word as a key
  (define (update-hash a-config a-word)
    (hash-set! computation-number-hash
               a-word
               (set-add (hash-ref computation-number-hash
                                  a-word
                                  EMPTY-SET)
                        a-config)))

  ;;set
  ;;the set of final states
  (define finals-set (list->seteq finals))

  
  ;;(queueof computation) (treelistof computation) -> (list (listof computation) hashtable)
  ;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
  ;;     that are within the bounds of the max computation limit
  (define (make-computations QoC path)
    (if (qempty? QoC)
        (list path computation-number-hash)
        (let* ([current-config (treelist-last (computation-LoC (qfirst QoC)))]
               [current-state (tm-config-state current-config)]
               [current-tape (tm-config-tape current-config)]
               [current-head-position (tm-config-head-position current-config)])
          (if (or (> (treelist-length (computation-LoC (qfirst QoC))) max-cmps)
                  (member? current-state finals eq?))
              (begin
                (update-hash current-config current-tape)
                (make-computations (dequeue QoC) (treelist-add path (qfirst QoC))))
              (let* (;;(listof rules)
                     ;;Purpose: Filters all the rules that can be applied to the configuration by reading the element in the rule
                     [connected-read-rules (treelist-filter (λ (rule)
                                                     (and (< current-head-position (length current-tape))
                                                          (eq? (rule-source rule) current-state)
                                                          (eq? (rule-read rule) (list-ref current-tape current-head-position))))
                                                   lor)]
                     ;;(listof computation)
                     [new-configs (treelist-filter (λ (new-c) 
                                            (not (set-member? visited-configuration-set (treelist-last (computation-LoC new-c)))))
                                          (treelist-map connected-read-rules (λ (rule) (apply-rule (qfirst QoC) rule))))])
                (begin
                  (update-hash current-config current-tape)
                  (update-visited current-config)
                  (if (treelist-empty? new-configs)
                      (make-computations (dequeue QoC) (treelist-add path (qfirst QoC)))
                      (make-computations (enqueue new-configs (dequeue QoC)) path))))))))
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (treelist (tm-config start head-pos a-word 0))
                                           empty-treelist
                                           '())])
    (make-computations (enqueue (treelist starting-computation) E-QUEUE) empty-treelist)))


;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(treelist-empty? rules) (reverse acc)]
        [(and (empty? acc)
              (or (not (eq? (rule-read (treelist-first rules)) BLANK))
                  (not (eq? (rule-read (treelist-first rules)) LM))))
         (let ([res (trace (treelist-first configs) (rule BLANK BLANK BLANK BLANK))])
           (make-trace (treelist-rest configs) rules (cons res acc)))]
        [else (let ([res (trace (treelist-first configs) (treelist-first rules))])
                (make-trace (treelist-rest configs) (treelist-rest rules) (cons res acc)))]))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
(define (get-inv-config-results computations invs)
  (append-map (λ (comp)
                (get-inv-config-results-helper (computation-LoC comp) invs))
              computations))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
(define (get-inv-config-results-helper computations invs)
  (if (or (empty? invs) (treelist-empty? computations))
      '()
      (let* ([get-inv-for-inv-config (filter (λ (inv)
                                               (equal? (first inv) (tm-config-state (treelist-first computations))))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (treelist-first computations)
                                          (inv-for-inv-config (tm-config-tape (treelist-first computations))
                                                                            (tm-config-head-position (treelist-first computations)))))])
        (if (empty? inv-config-result)
            (get-inv-config-results-helper (treelist-rest computations) invs)
            (cons inv-config-result
                (get-inv-config-results-helper (treelist-rest computations) invs))))))

;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results)
  (remove-duplicates (filter (λ (config) (not (second config))) inv-config-results)))



;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (make-edge-label rule)
  (format "\n[~a ~a]" (rule-read rule) (rule-action rule)))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  (map (λ (rule)
         (list (rule-source rule)
               (string->symbol (make-edge-label rule))
               (rule-destination rule)))
       rules))


;;rule symbol (listof rules) -> boolean
;;Purpose: Determines if the given rule is a member of the given (listof rules)
;;         or similiar to one of the rules in the given (listof rules) 
(define (find-rule? rule lor)
  (or (member? rule lor equal?)
      (ormap (λ (r)
               (and (equal? (first rule) (first r))
                    (equal? (third rule) (third r))))
             lor)))

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

;(listof symbol ((listof symbol) (listof symbol) -> boolean))) (X -> Y) ->
;(listof symbol ((listof symbol) (listof symbol) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) (listof symbols) -> boolean)))
(define (get-invariants inv func)
  (if (func (second inv))
      (tm-config-state (first inv))
      '()))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not id LoT))

;; (listof configuration) number -> (listof configuration)
;; Purpose: Returns the first configuration in a (listof configuration) if it exceeds the cut-off amount
(define (get-cut-off LoC max-cmps)
  (filter-map (λ (config)
                (and (>= (length config) max-cmps)
                     (first config)))
              LoC))

;;(listof rules) -> (listof rules)
;;Purpose: Converts the given (listof configurations)s to rules
(define (configs->rules a-config)
  (make-rule-triples
   (remove-duplicates (filter (λ (rule)
                                (not (equal? rule DUMMY-RULE)))
                              a-config))))

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

;;(listof trace) (listof trace) -> (listof trace)
;;Purpose: Finds the longest computation for rejecting traces
(define (find-longest-computation a-LoRT acc)
  (cond [(empty? a-LoRT) acc]
        [(> (length (first a-LoRT)) (length acc))
         (find-longest-computation (rest a-LoRT) (first a-LoRT))]
        [(find-longest-computation (rest a-LoRT) acc)]))

;;configuration configuration -> boolean
;;Purpose: Determines if the given invariant is the same as the given current config
(define (same-config? inv-config current-config)
  (and (equal? (tm-config-state         (first inv-config)) (tm-config-state current-config))
       (equal? (tm-config-head-position (first inv-config)) (tm-config-head-position current-config))
       (equal? (tm-config-tape          (first inv-config)) (tm-config-tape current-config))
       (equal? (tm-config-index         (first inv-config)) (tm-config-index current-config))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (let ([member-of-held-inv? (member? state held-inv eq?)]
                 [member-of-fail-inv? (member? state fail-inv eq?)])
             (add-node graph
                       state
                       #:atb (hash 'color (if (eq? (tm-start M) state) 'green 'black)
                                   'style (cond [(and member-of-held-inv? member-of-fail-inv?) 'wedged]
                                                [(or member-of-held-inv? member-of-fail-inv?
                                                     (member? state cut-off equal?)) 'filled]
                                                [else 'solid])
                                   'shape (cond [(eq? state (tm-accepting-final M)) 'doubleoctagon]
                                                [(member? state (tm-finals M) equal?) 'doublecircle]
                                                [else 'circle])
                                   'fillcolor (cond [(member? state cut-off equal?) GRAPHVIZ-CUTOFF-GOLD]
                                                    [(and member-of-held-inv? member-of-fail-inv?)
                                                     "red:chartreuse4"]
                                                    [member-of-held-inv? HELD-INV-COLOR ]
                                                    [member-of-fail-inv? BRKN-INV-COLOR]
                                                    [else 'white])
                                   'label state
                                   'fontcolor 'black
                                   'fontname (if (and (member? state held-inv equal?) (member? state fail-inv equal?))
                                                 "times-bold"
                                                 "Times-Roman")))))
         dgraph
         (tm-states M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-shown-accept-rules current-accept-rules current-reject-rules)
  (foldl (λ (rule graph)
           (add-edge graph
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(and (member? rule current-shown-accept-rules equal?)
                                                    (member? rule current-accept-rules equal?))
                                               SPLIT-ACCEPT-COLOR]
                                              [(find-rule? rule current-shown-accept-rules) TRACKED-ACCEPT-COLOR]
                                              [(find-rule? rule current-accept-rules) ALL-ACCEPT-COLOR]
                                              [(find-rule? rule current-reject-rules) REJECT-COLOR]
                                              [else 'black])
                                 'style (if (member? rule current-accept-rules equal?)
                                            'bold
                                            'solid)
                                 ;'labelfloat 'true
                                 'fontsize 12)))
         dgraph
         rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  (let* (;;(listof configuration)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs (remove-duplicates (map treelist-first (building-viz-state-computations a-vs)))]

         ;;(listof symbol)
         ;;Purpose: Gets the states where it's computation has cutoff
         [cut-off-states (if cut-off
                             (remove-duplicates (filter (λ (state)
                                                          (not (equal? state (tm-accepting-final (building-viz-state-M a-vs)))))
                                                        (map (λ (comp) (tm-config-state (treelist-first comp)))
                                                             (building-viz-state-computations a-vs))))
                             '())]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from of shown accepting computation
         [tracked-accepting-rules (get-trace-X (building-viz-state-tracked-accept-trace a-vs) trace-rules)]
         
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
         [current-shown-accept-rules (configs->rules tracked-accepting-rules)]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (filter (λ (rule)
                                                 (not (equal? (rule-read rule) LM)))
                                               (treelist->list (tm-rules (building-viz-state-M a-vs)))))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (same-config? invs curr))
                     invs)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (map (λ (inv) (get-invariants inv not)) get-invs)]
         
         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (map (λ (inv) (get-invariants inv id)) get-invs)])
    (make-edge-graph
     (make-node-graph
      (create-graph 'tmgraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      held-invs
      brkn-invs
      cut-off-states)
     all-rules
     (if (equal? (building-viz-state-machine-decision a-vs) 'accept)
         current-shown-accept-rules
         '())
     all-current-accept-rules
     (if (equal? (building-viz-state-machine-decision a-vs) 'accept)
         current-reject-rules
         (append current-reject-rules current-shown-accept-rules)))))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(ormap (λ (comp-len) (>= (tm-config-index comp-len) (building-viz-state-max-cmps a-vs)))
                (map treelist-first (building-viz-state-computations a-vs)))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [(and (zipper-at-end? (building-viz-state-tape a-vs))
              (zipper-at-end? (building-viz-state-head-pos a-vs)))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy
                                      building-viz-state
                                      a-vs
                                      [computations (filter (λ (comp) (not (treelist-empty? comp)))
                                                            (map treelist-rest (building-viz-state-computations a-vs)))]
                                      [tape (zipper-next (building-viz-state-tape a-vs))]
                                      [head-pos (zipper-next (building-viz-state-head-pos a-vs))]
                                      [tracked-accept-trace (get-next-traces (building-viz-state-tracked-accept-trace a-vs))]
                                      [all-accept-traces (get-next-traces (building-viz-state-all-accept-traces a-vs))]
                                      [all-reject-traces (get-next-traces (building-viz-state-all-reject-traces a-vs))])
                                     (cons next-graph acc)))]))



;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
    (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy
       imsg-state-tm 
       (informative-messages-component-state (viz-state-informative-messages a-vs))
       [rules-used (if (or (zipper-empty? imsg-state-rules-used) (zipper-at-end? imsg-state-rules-used))
                       imsg-state-rules-used 
                       (zipper-next imsg-state-rules-used))]
                     
       [tape (if (or (zipper-empty? imsg-state-tape) (zipper-at-end? imsg-state-tape))
                 imsg-state-tape 
                 (zipper-next imsg-state-tape))]
                     
       [head-position (if (or (zipper-empty? imsg-state-head-position) (zipper-at-end? imsg-state-head-position))
                          imsg-state-head-position
                          (zipper-next imsg-state-head-position))]
                     
       [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths) (zipper-at-end? imsg-state-computation-lengths))
                                imsg-state-computation-lengths 
                                (zipper-next imsg-state-computation-lengths))]
                     
       [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace) (zipper-at-end? imsg-state-shown-accepting-trace))
                                  imsg-state-shown-accepting-trace
                                  (zipper-next imsg-state-shown-accepting-trace))]
                     
       [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                          [(and (not (zipper-at-end? imsg-state-invs-zipper))
                                (>= (accessor-func imsg-state-shown-accepting-trace) (fourth (first (zipper-unprocessed imsg-state-invs-zipper)))))
                           (zipper-next imsg-state-invs-zipper)]
                          [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy
       imsg-state-tm
       (informative-messages-component-state
        (viz-state-informative-messages a-vs))
       [rules-used (if (or (zipper-empty? imsg-state-rules-used) (zipper-at-end? imsg-state-rules-used))
                       imsg-state-rules-used 
                       (zipper-to-end imsg-state-rules-used))]
       [tape (if (or (zipper-empty? imsg-state-tape) (zipper-at-end? imsg-state-tape))
                 imsg-state-tape
                 (zipper-to-end imsg-state-tape))]
       [head-position (if (or (zipper-empty? imsg-state-head-position) (zipper-at-end? imsg-state-head-position))
                          imsg-state-head-position
                          (zipper-to-end imsg-state-head-position))]
         
       [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths) (zipper-at-end? imsg-state-computation-lengths))
                                imsg-state-computation-lengths 
                                (zipper-to-end imsg-state-computation-lengths))]
       [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace) (zipper-at-end? imsg-state-shown-accepting-trace))
                                  imsg-state-shown-accepting-trace
                                  (zipper-to-end imsg-state-shown-accepting-trace))]
         
       ;;(zipperof invariant)
       ;;Purpose: The index of the last failed invariant
       [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper) (zipper-at-end? imsg-state-invs-zipper))
                        imsg-state-invs-zipper
                        (zipper-to-end imsg-state-invs-zipper))])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy
       imsg-state-tm
       (informative-messages-component-state
        (viz-state-informative-messages a-vs))
       [rules-used (if (or (zipper-empty? imsg-state-rules-used)
                           (zipper-at-begin? imsg-state-rules-used))
                       imsg-state-rules-used 
                       (zipper-prev imsg-state-rules-used))]
       [tape (if (or (zipper-empty? imsg-state-tape)
                     (zipper-at-begin? imsg-state-tape))
                 imsg-state-tape
                 (zipper-prev imsg-state-tape))]
                     
       [head-position (if (or (zipper-empty? imsg-state-head-position)
                              (zipper-at-begin? imsg-state-head-position))
                          imsg-state-head-position
                          (zipper-prev imsg-state-head-position))]
                     
       [computation-lengths (if (or (zipper-empty? imsg-state-computation-lengths)
                                    (zipper-at-begin? imsg-state-computation-lengths))
                                imsg-state-computation-lengths 
                                (zipper-prev imsg-state-computation-lengths))]
       [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                      (zipper-at-begin? imsg-state-shown-accepting-trace))
                                  imsg-state-shown-accepting-trace
                                  (zipper-prev imsg-state-shown-accepting-trace))]
       [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                          [(and (not (zipper-at-begin? imsg-state-invs-zipper))
                                (<= (accessor-func imsg-state-shown-accepting-trace)
                                    (fourth (first (zipper-processed imsg-state-invs-zipper)))))
                           (zipper-prev imsg-state-invs-zipper)]
                          [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
(define (up-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (struct-copy
       imsg-state-tm
       (informative-messages-component-state
        (viz-state-informative-messages a-vs))
       ;;rules
       [rules-used (if (or (zipper-empty? imsg-state-rules-used)
                           (zipper-at-begin? imsg-state-rules-used))
                       imsg-state-rules-used 
                       (zipper-to-begin imsg-state-rules-used))]
       ;;tape
       [tape (if (or (zipper-empty? imsg-state-tape)
                     (zipper-at-begin? imsg-state-tape))
                 imsg-state-tape
                 (zipper-to-begin imsg-state-tape))]
       ;;head-position
       [head-position (if (or (zipper-empty? imsg-state-head-position)
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
       ;;invariant-zipper
       [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper)
                            (zipper-at-begin? imsg-state-invs-zipper))
                        imsg-state-invs-zipper
                        (zipper-to-begin imsg-state-invs-zipper))])])])))

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
                    (struct-copy imsg-state-tm
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
                                (struct-copy imsg-state-tm
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-tm-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (if (or (zipper-empty? imsg-state-invs-zipper)
          (and (zipper-at-begin? imsg-state-invs-zipper)
               (not (zipper-at-end? imsg-state-invs-zipper))))
      a-vs
      (let ([zip (if (and (not (zipper-at-begin? imsg-state-invs-zipper))
                          (<= (accessor-func imsg-state-shown-accepting-trace)
                              (get-index imsg-state-invs-zipper)))
                     (zipper-prev imsg-state-invs-zipper)
                     (imsg-state-tm-invs-zipper imsg-state-invs-zipper))])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-index zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy
             imsg-state-tm
             (informative-messages-component-state
              (viz-state-informative-messages a-vs))
             ;;rules
             [rules-used (zipper-to-idx imsg-state-rules-used (get-index zip))]
             ;;tape
             [tape (zipper-to-idx imsg-state-tape (get-index zip))]
             ;;head-position
             [head-position (zipper-to-idx imsg-state-head-position (get-index zip))]
             ;;computation-lengths
             [computation-lengths (zipper-to-idx imsg-state-computation-lengths (get-index zip))]
             ;;Shown accepting trace
             [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                        imsg-state-shown-accepting-trace
                                        (zipper-to-idx imsg-state-shown-accepting-trace (get-index zip)))]
             ;;invariant-zipper
             [invs-zipper zip])])])))))


;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (if (or (zipper-empty? imsg-state-invs-zipper)
          (and (zipper-at-end? imsg-state-invs-zipper)
               (not (zipper-at-begin? imsg-state-invs-zipper))))
      a-vs
      (let ([zip (if (and (not (zipper-at-end? imsg-state-invs-zipper))
                          (>= (accessor-func imsg-state-shown-accepting-trace)
                              (get-index imsg-state-invs-zipper)))
                     (zipper-next imsg-state-invs-zipper)
                     imsg-state-invs-zipper)])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-index zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy
             imsg-state-tm
             (informative-messages-component-state (viz-state-informative-messages a-vs))
             ;;rules
             [rules-used (zipper-to-idx imsg-state-rules-used (get-index zip))]
             ;;tape
             [tape (zipper-to-idx imsg-state-tape (get-index zip))]
             ;;head-position
             [head-position (zipper-to-idx imsg-state-head-position (get-index zip))]
             ;;computation-lengths
             [computation-lengths (zipper-to-idx imsg-state-computation-lengths (get-index zip))]
             ;;Shown accepting trace
             [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                        imsg-state-shown-accepting-trace
                                        (zipper-to-idx imsg-state-shown-accepting-trace (get-index zip)))]
             ;;invariant-zipper
             [invs-zipper zip])])])))))


;;tm tape [natnum] [natnum] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (tm-viz M a-word head-pos #:cut-off [cut-off 100] . invs) ;;GET RID OF . FOR TESTING
  (let* (;;tm-struct
         [M (remake-tm M)]
         ;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations+hash (get-computations a-word (tm-rules M) (tm-start M) (tm-finals M) cut-off head-pos)]

         [computations (treelist->list (first computations+hash))]
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (if (eq? (tm-type M) 'tm-language-recognizer)
                                     (filter (λ (comp)
                                           (eq? (tm-config-state (treelist-last (computation-LoC comp))) (tm-accepting-final M)))
                                         computations)
                                     '())]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map (λ (acc-comp)
                                  (make-trace (computation-LoC acc-comp)
                                              (computation-LoR acc-comp)
                                              '()))
                                accepting-computations)]
         [computation-has-cut-off? (ormap (λ (comp-length)
                                            (>= comp-length cut-off))
                                          (if (empty? accepting-traces)
                                              (map treelist-length LoC)
                                              '()))]
         ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
         [cut-accept-traces '()]
         ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
         [accept-cmps (if (empty? cut-accept-traces)
                          accepting-traces
                          (map (λ (configs last-reject)
                                 (append configs (list last-reject)))
                               accepting-traces
                               cut-accept-traces))]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (filter (λ (config)
                                           (not (member? config accepting-computations equal?)))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
         [rejecting-traces (map (λ (comp)
                                  (make-trace (computation-LoC comp)
                                              (computation-LoR comp)
                                              '()))
                                rejecting-computations)]
         
         ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
         [accepting-trace (if (empty? accept-cmps) '() (first accept-cmps))]
         [rejecting-trace (if (empty? accept-cmps) (find-longest-computation rejecting-traces '()) '())]
         [displayed-tape (map (λ (trace) (tm-config-tape (trace-config trace)))
                              (cond [(and (empty? accepting-trace)
                                          (not computation-has-cut-off?)
                                          (= (length rejecting-computations) 1))
                                     rejecting-trace]
                                    [(and (not computation-has-cut-off?) (not (empty? accepting-trace))) accepting-trace]
                                    [else '()]))]
         [all-displayed-tape (list->zipper displayed-tape)]
         [tracked-head-pos (map (λ (trace) (tm-config-head-position (trace-config trace)))
                                (if (empty? accepting-trace)
                                    rejecting-trace
                                    accepting-trace))]
         [all-head-pos (list->zipper tracked-head-pos)]
         [machine-decision (if (not (empty? accepting-computations))
                               'accept
                               'reject)]
         
         [tracked-trace (cond [(and (empty? accepting-trace)
                                    (not computation-has-cut-off?)
                                    (= (length rejecting-computations) 1))
                               (list rejecting-trace)]
                              [(and (not computation-has-cut-off?) (not (empty? accepting-trace))) (list accepting-trace)]
                              [computation-has-cut-off? (if (empty? accepting-trace)
                                                            (list rejecting-trace)
                                                            (list accepting-trace))]
                              [else '()])]
         ;;(listof number) ;;Purpose: Gets all the invariant configurations
         [all-inv-configs (reverse (get-inv-config-results accepting-computations invs))]
         [failed-inv-configs (return-brk-inv-configs all-inv-configs)]
         
         
         
         ;;building-state struct
         [building-state (building-viz-state all-displayed-tape
                                             LoC
                                             tracked-trace
                                             (if (empty? accept-cmps) '() (rest accept-cmps))
                                             rejecting-traces
                                             M
                                             all-inv-configs
                                             cut-off
                                             all-head-pos
                                             machine-decision)]
                     
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [cut-off-computations-lengths (take (count-computations LoC '()) (length tracked-head-pos))]
         )
    (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ TM-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages tm-create-draw-informative-message
                                   (imsg-state-tm M
                                                  all-displayed-tape
                                                  all-head-pos
                                                  (list->zipper (map (λ (trace)
                                                                       (list (rule-read (trace-rules trace))
                                                                             (rule-action (trace-rules trace))))
                                                                     (cond [(empty? tracked-trace) tracked-trace]
                                                                           [(or (and (not computation-has-cut-off?)
                                                                                     (not (empty? accepting-trace)))
                                                                                (and (empty? accepting-trace)
                                                                                     (not computation-has-cut-off?)
                                                                                     (= (length rejecting-computations) 1)))
                                                                            (first tracked-trace)]
                                                                           [else '()])
                                                                     ))
                                                  (list->zipper (if (empty? tracked-trace) tracked-trace (first tracked-trace)))
                                                  (list->zipper failed-inv-configs) 
                                                  (list->zipper cut-off-computations-lengths)
                                                  cut-off
                                                  machine-decision
                                                  0
                                                  (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                    (if (> 0 offset-cap) 0 offset-cap))
                                                  0)
                                   tm-img-bounding-limit)
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       TM-E-SCENE-HEIGHT
                                                       (image-height tm-info-img)
                                                       INS-TOOLS-BUFFER)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       TM-E-SCENE-HEIGHT
                                                       (image-height tm-info-img)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))))
             (create-viz-draw-world E-SCENE-WIDTH TM-E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key [ "right" viz-go-next right-key-pressed]
                                     [ "left" viz-go-prev left-key-pressed]
                                     [ "up" viz-go-to-begin up-key-pressed]
                                     [ "down" viz-go-to-end down-key-pressed]
                                     [ "w" viz-zoom-in identity]
                                     [ "s" viz-zoom-out identity]
                                     [ "r" viz-max-zoom-out identity]
                                     [ "f" viz-max-zoom-in identity]
                                     [ "e" viz-reset-zoom identity]
                                     [ "a" identity a-key-pressed]
                                     [ "d" identity d-key-pressed]
                                     [ "wheel-down" viz-zoom-in identity]
                                     [ "wheel-up" viz-zoom-out identity]
                                     [ "j" tm-jump-prev j-key-pressed]
                                     [ "l" tm-jump-next l-key-pressed]
                                     )
             (create-viz-process-tick TM-E-SCENE-BOUNDING-LIMITS
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      TM-E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ( [tm-img-bounding-limit
                                         (lambda (a-imsgs x-diff y-diff) a-imsgs)])
                                      ( [ ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [ W-KEY-DIMS viz-zoom-in identity]
                                        [ S-KEY-DIMS viz-zoom-out identity]
                                        [ R-KEY-DIMS viz-max-zoom-out identity]
                                        [ E-KEY-DIMS viz-reset-zoom identity]
                                        [ F-KEY-DIMS viz-max-zoom-in identity]
                                        [ A-KEY-DIMS identity a-key-pressed]
                                        [ D-KEY-DIMS identity d-key-pressed]
                                        [ J-KEY-DIMS tm-jump-prev j-key-pressed]
                                        [ L-KEY-DIMS tm-jump-next l-key-pressed]))
             'tm-viz)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;States (i = head's position)
;; K - tape[1..i-1] contains an even amount of a's and even bs
;; H - tape[1..i-1] contains an odd amount of a's and even bs
;; I - tape[1..i-1] contains an odd amount of b's and even as
;; B - tape[1..i-1] contains an odd amount of a's and odd bs
;; S - tape[i] = blank AND tape[1..i-1] contains an even amount of a's and even bs, final state

;;Pre-condition = tape = LMw_ AND i = 0 
(define EVEN-AS-&-BS (make-unchecked-tm '(K H I B S)
                                        '(a b)
                                        `(((K ,BLANK) (S ,BLANK))
                                          ((K a) (H ,RIGHT)) ((H a) (K ,RIGHT)) ((H b) (B ,RIGHT)) ((B b) (H ,RIGHT))
                                          ((K b) (I ,RIGHT)) ((I b) (K ,RIGHT)) ((I a) (B ,RIGHT)) ((B a) (I ,RIGHT)))
                                        'K
                                        '(S)
                                        'S))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an even # of a's and b's
(define (EVEN-K-INV tape headpos)
  (or (= headpos 0)
      (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
            (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
        (and (even? num-as)
             (even? num-bs)))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an odd # of a's and even # of b's
(define (EVEN-H-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (odd? num-as)
         (even? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an odd # of a's and even # of b's
(define (BRK-EVEN-H-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (even? num-as)
         (even? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an even # of a's and odd # of b's
(define (EVEN-I-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (even? num-as)
         (odd? num-bs))))

(define (BRK-EVEN-I-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (not (even? num-as))
         (odd? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an odd # of a's and odd # of b's
(define (EVEN-B-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (odd? num-as)
         (odd? num-bs))))


(define (BRK-EVEN-B-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (not (odd? num-as))
         (odd? num-bs))))

;;tape headpos -> boolean
;;Purpose: Determines if everything in the tape[1..i] is an even # of a's and b's and tape[i] = BLANK
(define (EVEN-S-INV tape headpos)
  (let [(num-as (length (filter (λ (w) (eq? w 'a)) (take (rest tape) (sub1 headpos)))))
        (num-bs (length (filter (λ (w) (eq? w 'b)) (take (rest tape) (sub1 headpos)))))]
    (and (even? num-as)
         (even? num-bs)
         (eq? (list-ref tape headpos) BLANK))))

(define a* (make-unchecked-tm '(S Y N)
                              '(a b)
                              `(((S a) (S ,RIGHT))
                                ((S b) (N b))
                                ((S ,BLANK) (Y ,BLANK)))
                              'S
                              '(Y N)
                              'Y))

;; States (i is the position of the head)
;; S: no tape elements read, starting sate
;; A: tape[1..i-1] has only a
;; B: tape[1..i-1] has only a
;; C: tape[1..i-2] has only a and tape[i-1] = b
;; Y: tape[i] = BLANK and tape[1..i-1] = a* or a*b,
;; final accepting state
;; N: tape[1..i-1] != a* or a*b, final state
;; L = a* U a*b
;; PRE: tape = LMw ANDi=1
(define a*Ua*b (make-unchecked-tm '(S A B C Y N)
                                  '(a b)
                                  `(((S ,BLANK) (Y ,BLANK))
                                    ((S a) (A ,RIGHT))
                                    ((S a) (B ,RIGHT))
                                    ((S b) (C ,RIGHT))
                                    ((A a) (A ,RIGHT))
                                    ((A ,BLANK) (Y ,BLANK))
                                    ((B a) (B ,RIGHT))
                                    ((B b) (C ,RIGHT))
                                    ((C a) (N ,RIGHT))
                                    ((C b) (N ,RIGHT))
                                    ((C ,BLANK) (Y ,BLANK)))
                                  'S
                                  '(Y N)
                                  'Y))


;; tape natnum → Boolean
;; Purpose: Determine that no tape elements read
(define (S-INV t i) (= i 1))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-1] only has a
(define (A-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-1] only has a
(define (B-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-2] has only a and
;; tape[i-1] = b
(define (C-INV t i)
  (and (>= i 2)
       (andmap (λ (s) (eq? s 'a)) (take (rest t) (- i 2)))
       (eq? (list-ref t (sub1 i)) 'b)))

;; tape natnum → Boolean
;; Purpose: Determine that tape[i] = BLANK and
;; tape[1..i-1] = a* or tape[1..i-1] = a*b
(define (Y-INV t i)
  (or (and (= i 2) (eq? (list-ref t (sub1 i)) BLANK))
      (andmap (λ (s) (eq? s 'a)) (take (rest t) (sub1 i)))
      (let* [(front (takef (rest t) (λ (s) (eq? s 'a))))
             (back (takef (drop t (add1 (length front)))
                          (λ (s) (not (eq? s BLANK)))))]
        (equal? back '(b)))))

;; tape natnum → Boolean
;; Purpose: Determine that tape[1..i-1] != a* or a*b
(define (N-INV t i)
  (and (not (andmap (λ (s) (eq? s 'a))
                    (take (rest t) (sub1 i))))
       (let* [(front (takef (rest t) (λ (s) (eq? s 'a))))
              (back (takef (drop t (add1 (length front)))
                           (λ (s) (not (eq? s BLANK)))))]
         (not (equal? back '(b))))))

(define anbncn (make-unchecked-tm
                '(S A B C D E F G H I J K L Y)
                '(a b c x)
                `(((S ,BLANK) (J ,RIGHT))
                  ((J ,BLANK) (Y ,BLANK))
                  ((J a) (A ,RIGHT))
                  ((A a) (A ,RIGHT))
                  ((A b) (B ,RIGHT))
                  ((B b) (B ,RIGHT))
                  ((B c) (C ,RIGHT))
                  ((C c) (C ,RIGHT))
                  ((C ,BLANK) (D ,LEFT))
                  ((D a) (D ,LEFT))
                  ((D b) (D ,LEFT))
                  ((D c) (D ,LEFT))
                  ((D x) (D ,LEFT))
                  ((D ,BLANK) (E ,RIGHT))
                  ((E x) (E ,RIGHT))
                  ((E a) (F x))
                  ((E a) (H x))
                  ((F a) (F ,RIGHT))
                  ((F b) (G x))
                  ((F x) (F ,RIGHT))
                  ((G b) (G ,RIGHT))
                  ((G x) (G ,RIGHT))
                  ((G c) (D x))
                  ((H x) (H ,RIGHT))
                  ((H b) (I x))
                  ((I x) (I ,RIGHT))
                  ((I c) (K x))
                  ((K x) (L ,RIGHT))
                  ((L ,BLANK) (Y ,BLANK)))
                'S
                '(Y)
                'Y))
;; word symbol → word
;; Purpose: Return the subword at the front of the
;; given word that only contains the given
;; symbol
(define (front-symbs w s)
  (takef w (λ (a) (eq? a s))))

;; tape natnum → Boolean
;; Purpose: Determine that head is in position 1 and
;; tape[i] = BLANK
(define (S-INV1 t i) (and (= i 1) (eq? (list-ref t i) BLANK)))

(define (A-INV1 t i)
  (and (> i 2)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))]
         (equal? as w))))


;; tape natnum → Boolean
;; Purpose: Determine head in position > 2 and
;; tape[2..i-1] = a+b+
(define (B-INV1 t i)
  (and (> i 3)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))
              (w-as (drop w (length as)))
              (bs (front-symbs w-as 'b))]
         (and (equal? w (append as bs))
              (not (empty? as))
              (not (empty? bs))))))

;; tape natnum → Boolean
;; Purpose: Determine head in position > 2 and
;; tape[2..i-1] = a+b+
(define (BRK-B-INV1 t i)
  (not (and (> i 3)
            (let* [(w (drop (take t i) 2))
                   (as (front-symbs w 'a))
                   (w-as (drop w (length as)))
                   (bs (front-symbs w-as 'b))]
              (and (equal? w (append as bs))
                   (not (empty? as))
                   (not (empty? bs)))))))

;; tape natnum → Boolean
;; Purpose: Determine head in position > 3 and
;; tape[2..i-1]=a+b+c+
(define (C-INV1 t i)
  (and (> i 4)
       (let* [(w (drop (take t i) 2))
              (as (front-symbs w 'a))
              (w-as (drop w (length as)))
              (bs (front-symbs w-as 'b))
              (w-asbs (drop w-as (length bs)))
              (cs (front-symbs w-asbs 'c))]
         (and (equal? w (append as bs cs))
              (not (empty? as))
              (not (empty? bs))
              (not (empty? cs))))))


;; tape natnum → Boolean
;; Purpose: Determine that head position is >= 1 and that
;; tape[i]=xna+xnb+xnc+
(define (D-INV t i)
  (and (>= i 1)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 0)
              (= (length xs1) (length xs2) (length xs3))))))


;; tape natnum → Boolean
;; Purpose: Determine head in position > 1 and
;; input word = xna+xnb+xnc+
(define (E-INV t i)
  (and (> i 1)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 0)
              (= (length xs1) (length xs2) (length xs3))))))


(define (BRK-E-INV t i)
  (not (and (> i 1)
            (let* [(w (takef (drop t 2)
                             (λ (s) (not (eq? s BLANK)))))
                   (xs1 (front-symbs w 'x))
                   (w-xs1 (drop w (length xs1)))
                   (as (front-symbs w-xs1 'a))
                   (w-xs1as (drop w-xs1 (length as)))
                   (xs2 (front-symbs w-xs1as 'x))
                   (w-xs1asxs2 (drop w-xs1as (length xs2)))
                   (bs (front-symbs w-xs1asxs2 'b))
                   (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
                   (xs3 (front-symbs w-xs1asxs2bs 'x))
                   (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
                   (cs (front-symbs w-xs1asbsxs3 'c))]
              (and (equal? w (append xs1 as xs2 bs xs3 cs))
                   (> (length as) 0)
                   (> (length bs) 0)
                   (> (length cs) 0)
                   (= (length xs1) (length xs2) (length xs3)))))))


;; tape natnum → Boolean
;; Purpose: Determine head in position > 1 and
;; input word = xn+1a+xnbb+xncc+
(define (F-INV t i)
  (and (> i 1)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 1)
              (> (length cs) 1)
              (= (sub1 (length xs1))
                 (length xs2)
                 (length xs3))))))

;; tape natnum → Boolean
;; Purpose: Determine head in position > 2 and
;; tape=xn+1a+xn+1b+xncc+
(define (G-INV t i)
  (and (> i 3)
       (let* [(w (takef (drop t 2)
                        (λ (s) (not (eq? s BLANK)))))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (as (front-symbs w-xs1 'a))
              (w-xs1as (drop w-xs1 (length as)))
              (xs2 (front-symbs w-xs1as 'x))
              (w-xs1asxs2 (drop w-xs1as (length xs2)))
              (bs (front-symbs w-xs1asxs2 'b))
              (w-xs1asxs2bs (drop w-xs1asxs2 (length bs)))
              (xs3 (front-symbs w-xs1asxs2bs 'x))
              (w-xs1asbsxs3 (drop w-xs1asxs2bs (length xs3)))
              (cs (front-symbs w-xs1asbsxs3 'c))]
         (and (equal? w (append xs1 as xs2 bs xs3 cs))
              (> (length as) 0)
              (> (length bs) 0)
              (> (length cs) 1)
              (= (sub1 (length xs1))
                 (sub1 (length xs2))
                 (length xs3))))))


;; tape natnum → Boolean
;; Purpose: Determine tape[i]=x^+bx^+c and |xs|%3 = 1 and
;; |x^+b| = 2*|x^+c|
(define (H-INV t i)
  (and (> i 1)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (b (front-symbs w-xs1 'b))
              (w-xs1b (drop w-xs1 (length b)))
              (xs2 (front-symbs w-xs1b 'x))
              (w-xs1bxs2 (drop w-xs1b (length xs2)))
              (c (front-symbs w-xs1bxs2 'c))]
         (and (equal? w (append xs1 b xs2 c))
              (= (add1 (length xs1)) (* 2 (add1 (length xs2))))
              (= (length b) 1)
              (= (length c) 1)
              (= (remainder (length (append xs1 xs2)) 3) 1)))))

;; tape natnum → Boolean
;; Purpose: Determine tape[i]=x*c and |xs|%3 = 2
(define (I-INV t i)
  (and (> i 2)
       (let* [(w (drop-right (drop t 2) 1))
              (xs1 (front-symbs w 'x))
              (w-xs1 (drop w (length xs1)))
              (c (front-symbs w-xs1 'c))]
         (and (equal? w (append xs1 c))
              (= (length c) 1)
              (= (remainder (length xs1) 3) 2)))))

;; tape natnum → Boolean
;; Purpose: Determine that head’s position is 2 and
;; tape[1] = BLANK
(define (J-INV tape i)
  (and (= i 2) (eq? (list-ref tape (sub1 i)) BLANK)))


;; tape natnum → Boolean
;; Purpose: Determine if w = xxxx* and |xs|%3 = 0
;; and tape[i] = x
(define (K-INV t i)
  (let [(w (drop-right (drop t 2) 1))]
    (and (eq? (list-ref t i) 'x) ;;;
         (andmap (λ (s) (eq? s 'x)) w)
         (>= (length w) 3)
         (= (remainder (length w) 3) 0)
         (= i (add1 (length w))))))

;; tape natnum → Boolean
;; Purpose: Determine that w = xxxx* and |xs|%3 = 0 and
;; i = |w| + 2
(define (L-INV t i)
  (let [(w (drop-right (drop t 2) 1))]
    (and (andmap (λ (s) (eq? s 'x)) w)
         (>= (length w) 3)
         (= (remainder (length w) 3) 0)
         (= i (+ (length w) 2)))))

;; tape natnum → Boolean
;; Purpose: Determine input word = x* and |xs|%3 = 0
(define (Y-INV1 t i)
  (let* [(w (drop-right (drop t 2) 1))]
    (and (andmap (λ (s) (eq? s 'x)) w)
         (= (remainder (length w) 3) 0))))



;(tm-viz EVEN-AS-&-BS '(@ a b a b) 0)
#;(tm-viz anbncn `(,LM ,BLANK a b c) 1 (list 'S S-INV1)
          (list 'A A-INV1)
          (list 'B B-INV1)
          (list 'C C-INV1)
          (list 'D D-INV)
          (list 'E E-INV)
          (list 'F F-INV)
          (list 'H H-INV)
          (list 'I I-INV)
          (list 'G G-INV)
          (list 'J J-INV)
          (list 'K K-INV)
          (list 'L L-INV)
          (list 'Y Y-INV1)
          #:cut-off 15)

#;(tm-viz EVEN-AS-&-BS `(,LM b a b a) 0 (list 'K EVEN-K-INV)
          (list 'H EVEN-H-INV)
          (list 'I BRK-EVEN-I-INV)
          (list 'B EVEN-B-INV)
          (list 'S EVEN-S-INV))

#;(tm-viz anbncn `(,LM ,BLANK a b c) 1 (list 'S S-INV)
          (list 'A A-INV)
          (list 'B B-INV)
          (list 'C C-INV)
          (list 'Y Y-INV)
          (list 'N N-INV))

(define FBR (make-unchecked-tm '(S A F)
                     '(a b)
                     `(((S a) (A ,RIGHT))
                       ((S b) (A ,RIGHT))
                       ((S ,BLANK) (A ,RIGHT))
                       ((A a) (A ,RIGHT))
                       ((A b) (A ,RIGHT))
                       ((A ,BLANK) (F ,BLANK)))
                     'S
                     '(F)))

(define ADD (make-unchecked-tm '(S A B C D E F G)
                     '(d)
                     `(((S ,BLANK) (A ,RIGHT))
                       ((A d) (A ,RIGHT))
                       ((A ,BLANK) (B d))
                       ((B d) (B ,RIGHT))
                       ((B ,BLANK) (C ,LEFT))
                       ((C d) (D ,BLANK))
                       ((D ,BLANK) (E ,LEFT))
                       ((E d) (F ,RIGHT))
                       ((E ,BLANK) (G ,RIGHT))
                       ((G ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))

"fix informative messages"