#lang racket/base

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         racket/treelist
         racket/list
         racket/set
         racket/function
         racket/contract
         "sm-viz-contracts/sm-viz-contracts.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/vector-zipper.rkt"
         (except-in "../viz-lib/viz-constants.rkt"
                    INS-TOOLS-BUFFER)
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../../fsm-core/private/constants.rkt"
         "sm-viz-helpers/david-imsg-state.rkt"
         "sm-viz-helpers/david-viz-constants.rkt"
         (except-in "sm-viz-helpers/david-imsg-dimensions.rkt"
                    FONT-SIZE)
         "sm-viz-helpers/default-informative-messages.rkt"
         racket/set)


(provide tm-viz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define FONT-SIZE 12)

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
                            tracked-trace
                            all-accept-traces
                            all-reject-traces
                            M
                            inv
                            max-cmps
                            head-pos
                            machine-decision
                            pallete)
  #:transparent)

(define DUMMY-RULE (list (list BLANK BLANK) (list BLANK BLANK)))

(define INIT-HEAD-POS 0)
(define INIT-COMPUTATION-LENGTH 1)
(define INIT-TAPE-CONFIG-INDEX 0)
(define INIT-REACHED-FINAL #f)
(define INIT-REACHED-CUTOFF #f)
;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start finals accepting-final max-cmps head-pos)

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
               [LoR (treelist-add (computation-LoR a-comp) a-rule)]
               [visited (set-add (computation-visited a-comp) (treelist-last (computation-LoC a-comp)))]
               [length (add1 (computation-length a-comp))]))

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

  ;;(setof tm-config) tm-config -> boolean
  ;;Purpose: Determines if the given tm-config is a member of the given set
  (define (set-member? st val)
    (for/or ([elem (in-set st)])
      (and (equal? (tm-config-state         elem) (tm-config-state val))
           (equal? (tm-config-head-position elem) (tm-config-head-position val))
           (equal? (tm-config-tape          elem) (tm-config-tape val)))))
  
  ;;(queueof computation) (treelistof computation) -> (list (listof computation) hashtable)
  ;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
  ;;     that are within the bounds of the max computation limit
  (define (make-computations QoC path)

    (define (update-computation a-comp)
      (if (> head-pos INIT-COMPUTATION-LENGTH)
          (struct-copy computation a-comp
               [LoC (treelist-drop (computation-LoC a-comp) head-pos)]
               [LoR (treelist-drop (computation-LoR a-comp) head-pos)])
          a-comp))
    
    (if (qempty? QoC)
        path
        (let* ([current-config (treelist-last (computation-LoC (qfirst QoC)))]
               [current-state (tm-config-state current-config)]
               [current-tape (tm-config-tape current-config)]
               [current-head-position (tm-config-head-position current-config)]
               [member-of-finals? (member? current-state finals eq?)]
               [reached-threshold? (> (computation-length (qfirst QoC)) max-cmps)])
          (if (or reached-threshold? (member? current-state finals eq?))
              (begin
                ;(update-hash current-config current-tape)
                (make-computations (dequeue QoC)
                                   (if (eq? current-state accepting-final)
                                       (struct-copy paths path
                                                    [accepting (treelist-add (paths-accepting path) (update-computation (qfirst QoC)))]
                                                    [reached-final? (cond [(paths-reached-final? path) (paths-reached-final? path)]
                                                                          [member-of-finals? #t]
                                                                          [else (paths-reached-final? path)])]
                                                    [cut-off? (cond [(paths-cut-off? path) (paths-cut-off? path)]
                                                                    [reached-threshold? #t]
                                                                    [else (paths-cut-off? path)])])
                                       (struct-copy paths path
                                                    [rejecting (treelist-add (paths-rejecting path) (update-computation (qfirst QoC)))]
                                                    [reached-final? (cond [(paths-reached-final? path) (paths-reached-final? path)]
                                                                          [member-of-finals? #t]
                                                                          [else (paths-reached-final? path)])]
                                                    [cut-off? (cond [(paths-cut-off? path) (paths-cut-off? path)]
                                                                    [reached-threshold? #t]
                                                                    [else (paths-cut-off? path)])]))))
              (let* (;;(listof rules)
                     ;;Purpose: Filters all the rules that can be applied to the configuration by reading the element in the rule
                     [connected-read-rules (treelist-filter (λ (rule)
                                                     (and (< current-head-position(length current-tape))
                                                          (eq? (rule-source rule) current-state)
                                                          (eq? (rule-read rule) (list-ref current-tape current-head-position))))
                                                   lor)]
                     ;;(listof computation)
                     [new-configs (treelist-filter (λ (new-c) 
                                            (not (set-member? (computation-visited (qfirst QoC)) (treelist-last (computation-LoC new-c)))))
                                          (treelist-map connected-read-rules (λ (rule) (apply-rule (qfirst QoC) rule))))])
                (begin
                  ;(update-hash current-config current-tape)
                  (if (treelist-empty? new-configs)
                      (make-computations (dequeue QoC)
                                         (struct-copy paths path
                                                      [rejecting (treelist-add (paths-rejecting path) (update-computation (qfirst QoC)))]))
                      (make-computations (enqueue new-configs (dequeue QoC)) path))))))))
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (treelist (tm-config start INIT-HEAD-POS a-word INIT-TAPE-CONFIG-INDEX))
                                           empty-treelist
                                           (set)
                                           INIT-COMPUTATION-LENGTH)])
    (make-computations (enqueue (treelist starting-computation) E-QUEUE)
                       (paths empty-treelist empty-treelist INIT-REACHED-FINAL INIT-REACHED-CUTOFF))))


;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(treelist-empty? rules) (reverse acc)]
        [(and (empty? acc)
              (or (not (eq? (rule-read (treelist-first rules)) BLANK))
                  (not (eq? (rule-read (treelist-first rules)) LM))))
         (let ([res (trace (treelist-first configs) (rule BLANK LM BLANK LM))])
           (make-trace (treelist-rest configs) rules (cons res acc)))]
        [else (let ([res (trace (treelist-first configs) (treelist-first rules))])
                (make-trace (treelist-rest configs) (treelist-rest rules) (cons res acc)))]))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
(define (get-inv-config-results computations invs)
  ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
  ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
  (define (get-inv-config-results-helper computations)
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
              (get-inv-config-results-helper (treelist-rest computations))
              (cons inv-config-result
                    (get-inv-config-results-helper (treelist-rest computations)))))))
  (append-map (λ (comp)
                (get-inv-config-results-helper comp))
              computations))


;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results)
  (remove-duplicates (filter-map (λ (config) (and (not (second config))
                                                  (first config))) inv-config-results)))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  ;(listof symbols) -> string
  ;;Purpose: Converts the given los into a string
  (define (make-edge-label rule)
    (format "\n[~a ~a]" (rule-read rule) (rule-action rule)))

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
  (filter-map-acc empty? rest not identity LoT))

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
(define (make-node-graph dgraph M held-inv fail-inv cut-off color-scheme)
  (foldl (λ (state graph)
           (let ([member-of-held-inv? (member? state held-inv eq?)]
                 [member-of-fail-inv? (member? state fail-inv eq?)]
                 [member-of-cut-off?  (member? state cut-off eq?)]
                 [graph-attributes (graph-attributes-node-attributes default-graph-attributes)])
             (add-node graph
                       state
                       #:atb (hash 'color (if (eq? (tm-start M) state)
                                              (color-palette-start-state-color color-scheme)
                                              (color-palette-font-color color-scheme))
                                   'style (cond [(and member-of-held-inv? member-of-fail-inv?)
                                                 (node-data-bi-inv-node graph-attributes)]
                                                [(or member-of-held-inv? member-of-fail-inv? member-of-cut-off?)
                                                 (node-data-inv-node graph-attributes)]
                                                [else (node-data-regular-node graph-attributes)])
                                   'shape (cond [(eq? state (tm-accepting-final M))
                                                 (node-data-accepting-final-state graph-attributes)]
                                                [(member? state (tm-finals M) eq?)
                                                 (node-data-final-state graph-attributes)]
                                                [else (node-data-regular-state graph-attributes)])
                                   'fillcolor (cond [member-of-cut-off? (color-palette-cut-off-color color-scheme)]
                                                    [(and member-of-held-inv? member-of-fail-inv?)
                                                     (color-palette-split-inv-color color-scheme)]
                                                    [member-of-held-inv? (color-palette-inv-hold-color color-scheme)]
                                                    [member-of-fail-inv? (color-palette-inv-fail-color color-scheme)]
                                                    [else (color-palette-blank-color color-scheme)])
                                   'label state
                                   'fontcolor (color-palette-font-color color-scheme)
                                   'fontname (if (and member-of-held-inv? member-of-fail-inv?)
                                                 (node-data-bi-inv-font graph-attributes)
                                                 (node-data-regular-font graph-attributes))))))
         dgraph
         (tm-states M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-tracked-rules current-accept-rules current-reject-rules accepted? color-scheme)
  (foldl (λ (rule graph)
           (let ([found-tracked-rule? (find-rule? rule current-tracked-rules)]
                 [found-accept-rule? (find-rule? rule current-accept-rules)]
                 [found-reject-rule? (find-rule? rule current-reject-rules)]
                 [graph-attributes (graph-attributes-edge-attributes default-graph-attributes)])
             (add-edge graph
                       (second rule)
                       (first rule)
                       (third rule)                     
                       #:atb (hash 'color
                                   (cond [(and found-tracked-rule? found-accept-rule?)
                                          (color-palette-split-accept-color color-scheme)] ;;<--- watch out if coloring issue
                                         [(and found-tracked-rule? found-reject-rule? (not accepted?))
                                          (color-palette-split-reject-color color-scheme)] ;;<-- may cause issue
                                         [(and found-tracked-rule? found-reject-rule? accepted?)
                                          (color-palette-split-accept-reject-color color-scheme)] ;;<-- may cause issue
                                         [(and found-tracked-rule? found-accept-rule? found-reject-rule? accepted?)
                                          (color-palette-bi-accept-reject-color color-scheme)]
                                         [(and accepted? found-tracked-rule?) (color-palette-shown-accept-color color-scheme)] 
                                         [found-tracked-rule?  (color-palette-shown-reject-color color-scheme)]
                                         [found-accept-rule?   (color-palette-other-accept-color color-scheme)]
                                         [found-reject-rule?   (color-palette-other-reject-color color-scheme)]
                                         [else (color-palette-font-color color-scheme)])
                                   'style (cond [found-tracked-rule? #;(and found-tracked-rule? accepted?)
                                                 (edge-data-accept-edge graph-attributes)]
                                                [(or found-accept-rule? #;found-tracked-rule? found-reject-rule?)
                                                 (edge-data-reject-edge graph-attributes)]
                                                [else (edge-data-regular-edge graph-attributes)])
                                   'fontsize FONT-SIZE))))
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
         [tracked-rules (get-trace-X (building-viz-state-tracked-trace a-vs) trace-rules)]
         
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
         [current-shown-tracked-rules (configs->rules tracked-rules)]
         
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
         [held-invs (map (λ (inv) (get-invariants inv identity)) get-invs)])
    (make-edge-graph
     (make-node-graph
      (create-graph 'tmgraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      held-invs
      brkn-invs
      cut-off-states
      (building-viz-state-pallete a-vs))
     all-rules
     current-shown-tracked-rules
     all-current-accept-rules
     current-reject-rules
     (equal? (building-viz-state-machine-decision a-vs) 'accept)
     (building-viz-state-pallete a-vs))))

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
                                      [tape (if (zipper-at-end? (building-viz-state-tape a-vs))
                                                (building-viz-state-tape a-vs)
                                                (zipper-next (building-viz-state-tape a-vs)))]
                                      [head-pos (if (zipper-at-end? (building-viz-state-head-pos a-vs))
                                                    (building-viz-state-head-pos a-vs)
                                                    (zipper-next (building-viz-state-head-pos a-vs)))]
                                      [tracked-trace (get-next-traces (building-viz-state-tracked-trace a-vs))]
                                      [all-accept-traces (get-next-traces (building-viz-state-all-accept-traces a-vs))]
                                      [all-reject-traces (get-next-traces (building-viz-state-all-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state
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
                     
       [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                      (zipper-at-end? imsg-state-shown-accepting-trace))
                                  imsg-state-shown-accepting-trace
                                  (zipper-next imsg-state-shown-accepting-trace))]
                     
       [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                          [(and (not (zipper-at-end? imsg-state-invs-zipper))
                                (>= (add1 (get-tm-config-index-frm-trace imsg-state-shown-accepting-trace))
                                    (tm-config-index (zipper-current imsg-state-invs-zipper))))
                           (zipper-next imsg-state-invs-zipper)]
                          [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state
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
       [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                      (zipper-at-end? imsg-state-shown-accepting-trace))
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
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state
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
                                (<= (sub1 (get-tm-config-index-frm-trace imsg-state-shown-accepting-trace))
                                    (tm-config-index (zipper-current imsg-state-invs-zipper))))
                           (zipper-prev imsg-state-invs-zipper)]
                          [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
(define (up-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state
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
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))])
  (if (or (zipper-empty? imsg-state-invs-zipper)
          (< (get-tm-config-index-frm-trace imsg-state-shown-accepting-trace)
             (get-tm-config-index-frm-invs imsg-state-invs-zipper)))
      a-vs
      (let ([zip (if (and (not (zipper-at-begin? imsg-state-invs-zipper))
                          (<= (get-tm-config-index-frm-trace imsg-state-shown-accepting-trace)
                              (get-tm-config-index-frm-invs imsg-state-invs-zipper)))
                     (zipper-prev imsg-state-invs-zipper)
                     imsg-state-invs-zipper)])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-tm-config-index-frm-invs zip))]
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
             [rules-used (zipper-to-idx imsg-state-rules-used (get-tm-config-index-frm-invs zip))]
             ;;tape
             [tape (zipper-to-idx imsg-state-tape (get-tm-config-index-frm-invs zip))]
             ;;head-position
             [head-position (zipper-to-idx imsg-state-head-position (get-tm-config-index-frm-invs zip))]
             ;;computation-lengths
             [computation-lengths (zipper-to-idx imsg-state-computation-lengths (get-tm-config-index-frm-invs zip))]
             ;;Shown accepting trace
             [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                        imsg-state-shown-accepting-trace
                                        (zipper-to-idx imsg-state-shown-accepting-trace (get-tm-config-index-frm-invs zip)))]
             ;;invariant-zipper
             [invs-zipper zip])])])))))


;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (let ([imsg-state-rules-used (imsg-state-tm-rules-used (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
        [imsg-state-tape (imsg-state-tm-tape (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-head-position (imsg-state-tm-head-position (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))]
        [imsg-state-computation-lengths (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))])
  (if (or (zipper-empty? imsg-state-invs-zipper)
          (> (get-tm-config-index-frm-trace imsg-state-shown-accepting-trace)
                              (get-tm-config-index-frm-invs imsg-state-invs-zipper)))
      a-vs
      (let ([zip (if (and (not (zipper-at-end? imsg-state-invs-zipper))
                          (>= (get-tm-config-index-frm-trace imsg-state-shown-accepting-trace)
                              (get-tm-config-index-frm-invs imsg-state-invs-zipper)))
                     (zipper-next imsg-state-invs-zipper)
                     imsg-state-invs-zipper)])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-tm-config-index-frm-invs zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy
             imsg-state-tm
             (informative-messages-component-state (viz-state-informative-messages a-vs))
             ;;rules
             [rules-used (zipper-to-idx imsg-state-rules-used (get-tm-config-index-frm-invs zip))]
             ;;tape
             [tape (zipper-to-idx imsg-state-tape (get-tm-config-index-frm-invs zip))]
             ;;head-position
             [head-position (zipper-to-idx imsg-state-head-position (get-tm-config-index-frm-invs zip))]
             ;;computation-lengths
             [computation-lengths (zipper-to-idx imsg-state-computation-lengths (get-tm-config-index-frm-invs zip))]
             ;;Shown accepting trace
             [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                        imsg-state-shown-accepting-trace
                                        (zipper-to-idx imsg-state-shown-accepting-trace (get-tm-config-index-frm-invs zip)))]
             ;;invariant-zipper
             [invs-zipper zip])])])))))


;;tm tape natnum [natnum] [symbol] . (listof (list state (t i -> boolean))) -> (void) 
;;Purpose: Visualizes the given tm processing the given word
;;Assumption: The given machine is tm
(define/contract (tm-viz M a-word head-pos #:cut-off [cut-off 100] #:palette [palette 'default] invs) ;;GET RID OF . FOR TESTING
  tm-viz/c
  (let* (;;tm-struct
         [M (remake-tm M)]
         ;;color-pallete ;;The corresponding color scheme to used in the viz
         [color-scheme (cond [(eq? palette 'prot) protanopia-color-scheme] ;;red color blind
                             [(eq? palette 'deut) deuteranopia-color-scheme] ;;green color blind 
                             [(eq? palette 'trit) tritanopia-color-scheme] ;;blue color blind
                             [else standard-color-scheme])]
         ;;(listof computations) ;;Purpose: All computations that the machine can have
         ;[computations (get-computations a-word (tm-rules M) (tm-start M) (tm-finals M) cut-off head-pos)]
         [all-paths (get-computations a-word (tm-rules M) (tm-start M) (tm-finals M) (tm-accepting-final M) cut-off head-pos)]
         ;;boolean ;;Purpose: Determines if any computation 
         [reached-final? (paths-reached-final? all-paths)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (treelist->list (paths-accepting all-paths))]
         ;;boolean ;;Purpose: Determines if the word has been rejected
         [rejected? (empty? accepting-computations)]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (treelist->list (paths-rejecting all-paths))]
         ;;(listof computation) ;;Purpose: Extracts the configurations from the computation
         [LoC (map2 computation-LoC (append accepting-computations rejecting-computations))]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map (λ (acc-comp)
                                  (make-trace (computation-LoC acc-comp)
                                              (computation-LoR acc-comp)
                                              '()))
                                accepting-computations)]
         ;;boolean ;;Purpose: Determines if any computation reaches the cuts off treshold
         [computation-has-cut-off? (paths-cut-off? all-paths)] 
         ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
         [rejecting-traces (map (λ (comp)
                                  (make-trace (computation-LoC comp)
                                              (computation-LoR comp)
                                              '()))
                                rejecting-computations)]
         
         ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
         [accepting-trace (if rejected? '() (first accepting-traces))]
         [rejecting-trace (if rejected? (find-longest-computation rejecting-traces '()) '())]
         [displayed-tape (map (λ (trace) (tm-config-tape (trace-config trace)))
                              (if (not (empty? accepting-trace))
                             accepting-trace
                             rejecting-trace))]
         [all-displayed-tape (list->zipper displayed-tape)]
         [tracked-head-pos (let ([head-pos (map (λ (trace) (tm-config-head-position (trace-config trace)))
                                                (if rejected?
                                                    rejecting-trace
                                                    accepting-trace))])
                             head-pos)]
         [all-head-pos (list->zipper tracked-head-pos)]
         [machine-decision (cond [(not rejected?) 'accept]
                                 [(and reached-final? (eq? (tm-type M) 'tm)) 'reached-final]
                                 [(and (not reached-final?) (eq? (tm-type M) 'tm)) 'halted]
                                 [else 'reject])]
         
         [tracked-trace (list (if (not rejected?) accepting-trace rejecting-trace))]
         ;;(listof number) ;;Purpose: Gets all the invariant configurations
         [all-inv-configs (if (empty? invs)
                              '()
                              (get-inv-config-results (if (and reached-final? (eq? (tm-type M) 'tm))
                                                                   LoC
                                                                   (map computation-LoC accepting-computations))
                                                               invs))]
         [failed-inv-configs (return-brk-inv-configs all-inv-configs)]
         ;;building-state struct
         [building-state (building-viz-state all-displayed-tape
                                             LoC
                                             tracked-trace
                                             (if rejected? '() (rest accepting-traces))
                                             (if rejected? (filter (λ (config) (not (equal? config rejecting-trace)))
                                                                                   rejecting-traces)
                                                 rejecting-traces)
                                             M
                                             all-inv-configs
                                             cut-off
                                             all-head-pos
                                             machine-decision
                                             color-scheme)]
                     
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [cut-off-computations-lengths (take (count-computations LoC '()) (length tracked-head-pos))]
         [color-legend (let* ([buffer-sqaure (square HEIGHT-BUFFER 'solid (color-palette-blank-color color-scheme))]
                              [spacer (beside buffer-sqaure buffer-sqaure buffer-sqaure buffer-sqaure)])
                         (if rejected?
                           (beside (text "Reject traced" 20 (color-palette-legend-shown-reject-color color-scheme))
                                   spacer
                                   (text "Reject not traced" 20 (color-palette-legend-other-reject-color color-scheme)))
                           (beside (text "Accept traced" 20 (color-palette-legend-shown-accept-color color-scheme))
                                   spacer
                                   (text "Accept not traced" 20 (color-palette-legend-other-accept-color color-scheme))
                                   spacer
                                   (text "Reject not traced" 20 (color-palette-legend-other-reject-color color-scheme)))))])
   
    #;
    (void)
    
   ;#;
    (run-viz graphs
            (list->vector (map (λ (x) (λ (grph) grph)) graphs))
             (posn (/ E-SCENE-WIDTH 2) (/ TM-E-SCENE-HEIGHT 2))
             E-SCENE-WIDTH TM-E-SCENE-HEIGHT PERCENT-BORDER-GAP
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages tm-create-draw-informative-message
                                   (imsg-state-tm M
                                                  (if (zipper-empty? all-displayed-tape) (list->zipper (list a-word)) all-displayed-tape)
                                                  all-head-pos
                                                  (list->zipper (map2 (λ (trace)
                                                                        (list (rule-read (trace-rules trace))
                                                                              (rule-action (trace-rules trace))))
                                                                      (first tracked-trace)) )
                                                  (list->zipper (if (empty? tracked-trace) tracked-trace (first tracked-trace)))
                                                  (list->zipper failed-inv-configs) 
                                                  (list->zipper cut-off-computations-lengths)
                                                  cut-off
                                                  machine-decision
                                                  0
                                                  (let ([offset-cap (- (length a-word) TM-TAPE-SIZE)])
                                                    (if (> 0 offset-cap) 0 offset-cap))
                                                  0
                                                  color-scheme)
                                   tm-img-bounding-limit)
             (instructions-graphic (above color-legend E-SCENE-TOOLS)
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
                                     [ "w" tm-viz-zoom-in identity]
                                     [ "s" tm-viz-zoom-out identity]
                                     [ "r" tm-viz-max-zoom-out identity]
                                     [ "f" tm-viz-max-zoom-in identity]
                                     [ "e" tm-viz-reset-zoom identity]
                                     [ "a" identity a-key-pressed]
                                     [ "d" identity d-key-pressed]
                                     [ "wheel-down" tm-viz-zoom-in identity]
                                     [ "wheel-up" tm-viz-zoom-out identity]
                                     [ "j" tm-jump-prev j-key-pressed]
                                     [ "l" tm-jump-next l-key-pressed]
                                     )
             (create-viz-process-tick TM-E-SCENE-BOUNDING-LIMITS
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      TM-E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ( [tm-img-bounding-limit
                                         (lambda (a-imsgs x-diff y-diff)
                                           (let ([new-scroll-accum (+ (imsg-state-tm-scroll-accum a-imsgs) x-diff)])
                                             (cond
                                               [(and (>= (imsg-state-tm-word-img-offset-cap a-imsgs)
                                                         (imsg-state-tm-word-img-offset a-imsgs))
                                                     (<= (quotient (+ (imsg-state-tm-scroll-accum a-imsgs) x-diff) 25) -1))
                                                (struct-copy imsg-state-tm
                                                             a-imsgs
                                                             [word-img-offset (+ (imsg-state-tm-word-img-offset a-imsgs) 1)]
                                                             [scroll-accum 0])]
                                               [(and (> (imsg-state-tm-word-img-offset a-imsgs) 0)
                                                     (>= (quotient (+ (imsg-state-tm-scroll-accum a-imsgs) x-diff) 25) 1))
                                                (struct-copy imsg-state-tm
                                                             a-imsgs
                                                             [word-img-offset (- (imsg-state-tm-word-img-offset a-imsgs) 1)]
                                                             [scroll-accum 0])]
                                               [else
                                                (struct-copy imsg-state-tm
                                                             a-imsgs
                                                             [scroll-accum
                                                              (+ (imsg-state-tm-scroll-accum a-imsgs) x-diff)])])))])
                                      ( [ ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [ W-KEY-DIMS tm-viz-zoom-in identity]
                                        [ S-KEY-DIMS tm-viz-zoom-out identity]
                                        [ R-KEY-DIMS tm-viz-max-zoom-out identity]
                                        [ E-KEY-DIMS tm-viz-reset-zoom identity]
                                        [ F-KEY-DIMS tm-viz-max-zoom-in identity]
                                        [ A-KEY-DIMS identity a-key-pressed]
                                        [ D-KEY-DIMS identity d-key-pressed]
                                        [ J-KEY-DIMS tm-jump-prev j-key-pressed]
                                        [ L-KEY-DIMS tm-jump-next l-key-pressed]))
             'tm-viz)))
