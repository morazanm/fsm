#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/viz.rkt"
         racket/treelist
         "../viz-lib/zipper.rkt"
         "../viz-lib/tl-zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/vector-zipper.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/misc.rkt"
         "default-informative-messages.rkt"
         "testing-parameter.rkt"
         ;profile-flame-graph
         (except-in "../viz-lib/viz-constants.rkt"
                    INS-TOOLS-BUFFER)
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE))

(provide pda-viz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define FONT-SIZE 18)


#|
A rule is a structure:
(make-rule triple pair)
triple is the first of the pda rule
pair is the second of the pda rule
|#
(struct rule (triple pair) #:transparent)

(struct triple (source read pop) #:transparent)

(struct pair (destination push) #:transparent)

(struct pda (states sigma gamma start finals rules) #:transparent)

(define DUMMY-RULE (rule (triple EMP EMP EMP) (pair EMP EMP)))
#|
ci                      | is a structure containing the unconsumed and consumed input => (zipperof ci)
computations            | is all of the computations that attempt to consume the ci => (listof computation)
acc-comp                | is all of the accepting computations => (listof computation)
stack                   | is the first accepting computation => (zipperof computation)
tracked-accept-trace    | is the first accepting trace (which is also the first accepting computation) to be followed 
accept-traces           | is all of the accepting traces => (listof trace)
reject-traces           | is all of the rejecting traces => (listof trace)
M                       | is the machine as a structure => pda
inv                     | is the invariant predicates for the machine => (listof (list symbol (ci stack -> boolean)))
dead                    | is the dead state symbol  => symbol 
max-cmps                | is the cut off threshold for the machine => postive integer
farthest-consumed-input | is the portion the ci that the machine consumed the most of => (listof symbol)
|#
(struct building-viz-state (CI
                            computations 
                            acc-comp 
                            stack 
                            tracked-accept-trace 
                            accept-traces
                            reject-traces
                            M    
                            inv
                            dead
                            max-cmps 
                            farthest-consumed-input))

(define get-index (compose1 pda-config-index zipper-current))


;(define-struct ci (upci pci stack))
;(struct config (state word stack index))

;;word -> (zipperof ci)
;;Purpose: Creates all valid combinations of the upci and pci
(define (remake-ci a-word)
  ;;natnum ;;Purpose: the length of the given word
  (define word-length (length a-word))

  ;;natnum (listof ci) -> (zipperof ci)
  ;;Purpose: Creates all valid combinations of the upci and pci
  ;;Acc = All valid combinations of unconsumed and consumed input after num-steps amount  of steps
  (define (make-ci-helper num-steps acc)
    (let [(upci (if (< num-steps 0)
                    '()
                    (take-right a-word num-steps)))
          (pci (if (< num-steps 0)
                    a-word
                    (drop-right a-word num-steps)))]
    (if (<= num-steps 0)
        (list->zipper (reverse (cons (ci upci pci) acc)))
        (make-ci-helper (sub1 num-steps) (cons (ci upci pci) acc)))))
  (make-ci-helper word-length '()))

;;rule -> boolean
;;Purpose: Determines if the given rule is an empty rule (e.i. reads, pops, and pushes empty)
(define (empty-rule? a-rule)
  (and (eq? (triple-read (rule-triple a-rule)) EMP)
       (eq? (triple-pop  (rule-triple a-rule)) EMP)
       (eq? (pair-push   (rule-pair a-rule))   EMP)))


;;word (listof rule) symbol number -> (list (treelistof computation) hashtable)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start finals max-cmps)
  ;;computation rule -> computation
  ;;Purpose: Applys the given rule to the given config and returns the updated config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-rule a-comp a-rule)
    ;;config -> config
    ;;Purpose: Applys the given rule to the given config and returns the updated config    
    (define (apply-rule-helper a-config)
      (let* (;;config
             ;;Purpose: Applies the read portion of given rule to the given config
             [apply-pop-result (if (eq? (triple-pop (rule-triple a-rule)) EMP)
                                   (pda-config-stack a-config)
                                   (drop (pda-config-stack a-config) (length (triple-pop (rule-triple a-rule)))))]
             ;;config
             ;;Purpose: Applies the push portion of given rule to the given config
             [apply-push-result (if (eq? (pair-push (rule-pair a-rule)) EMP)
                                    apply-pop-result
                                    (append (pair-push (rule-pair a-rule)) apply-pop-result))]
             ;;config
             ;;Purpose: Applies the read portion of given rule to the given config
             [apply-read-result (if (eq? (triple-read (rule-triple a-rule)) EMP)
                                    (pda-config-word a-config)
                                    (rest (pda-config-word a-config)))]
             ;;config
             ;;Purpose: Updates the config's number if something gets applied to the config (e.i. read/pop/push)
             [update-count-result (if (empty-rule? a-rule)
                                      (pda-config-index a-config)
                                      (add1 (pda-config-index a-config)))])
        (struct-copy pda-config a-config
                     [state (pair-destination (rule-pair a-rule))]
                     [stack apply-push-result]
                     [word apply-read-result]
                     [index update-count-result])))

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

  ;;(queueof computation) -> (listof computation hashtable)
  ;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
  ;;     that are within the bounds of the max computation limit
  (define (make-computations starting-computation)
    ;;(queueof computation) (treelistof computation) -> 
    (define (make-computations-helper QoC path)
      (if (qempty? QoC)
          (list path computation-number-hash)
          (let* (;;configuraton
                 ;;the current configuration
                 [curr-config (treelist-last (computation-LoC (qfirst QoC)))]
                 ;;word
                 ;;the unconsumed input of the current configuration
                 [curr-word (pda-config-word curr-config)]
                 ;;stack
                 ;;the current stack of the current configuration
                 [curr-stack (pda-config-stack curr-config)]
                 ;;state
                 ;;the current state of the current configuration
                 [curr-state (pda-config-state curr-config)])
            (if (or (and (empty? curr-word)
                         (empty? curr-stack)
                         (set-member? finals-set curr-state))
                    (> (treelist-length (computation-LoC (qfirst QoC))) max-cmps))
                (begin
                  (update-hash curr-config curr-word)
                  (make-computations-helper (dequeue QoC) (treelist-add path (qfirst QoC))))
                (let* (;;(listof rules)
                       ;;Purpose: Filters the rules that match the current state 
                       [curr-rules (treelist-filter (λ (rule) (eq? (triple-source (rule-triple rule))
                                                                   curr-state))
                                                    lor)]
                       ;;(listof rules)
                       ;;Purpose: Holds all rules that consume a first letter in the given configurations
                       [connected-read-rules (treelist-filter (λ (rule)
                                                                (and (not (empty? curr-word))
                                                                     (eq? (triple-read (rule-triple rule))
                                                                          (first curr-word))))
                                                              curr-rules)]
                       ;;(listof rules)
                       ;;Purpose: Holds all rules that consume no input for the given configurations
                       [connected-read-E-rules (treelist-filter (λ (rule)
                                                                  (eq? (triple-read (rule-triple rule))
                                                                       EMP))
                                                                curr-rules)]
                       ;;(listof rules)
                       ;;Purpose: Holds all rules that can pop what is in the stack
                       [connected-pop-rules (treelist-filter (λ (rule)
                                                               (or (eq? (triple-pop (rule-triple rule)) EMP)
                                                                   (and (>= (length curr-stack)
                                                                            (length (triple-pop (rule-triple rule))))
                                                                        (equal? (take curr-stack
                                                                                      (length
                                                                                       (triple-pop (rule-triple rule))))
                                                                                (triple-pop (rule-triple rule))))))
                                                             (treelist-append connected-read-E-rules connected-read-rules))]
                       ;;(listof coniguration)
                       ;;Purpose: Holds all the new configurations generated from the appliciable rules
                       [new-configs (treelist-filter (λ (new-c) 
                                                       (not (set-member? visited-configuration-set
                                                                         (treelist-last (computation-LoC new-c)))))
                                                     (treelist-map connected-pop-rules
                                                                   (λ (rule) (apply-rule (qfirst QoC) rule))
                                                                   ))])
                  (begin
                    (update-hash curr-config curr-word)
                    (update-visited curr-config)
                    (if (treelist-empty? new-configs)
                        (make-computations-helper (dequeue QoC) (treelist-add path (qfirst QoC)))
                        (make-computations-helper (enqueue new-configs (dequeue QoC)) path))))))))
    (make-computations-helper (enqueue (treelist starting-computation) E-QUEUE) empty-treelist))

  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (treelist (pda-config start a-word '() 0))
                                           empty-treelist
                                           (set))])
    (make-computations starting-computation)))


;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

(define (remake-rules lor)
  (for/treelist ([pda-rule lor])
    (rule (triple (first (first pda-rule))
                  (second (first pda-rule))
                  (third (first pda-rule)))
          (pair (first (second pda-rule))
                (second (second pda-rule))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M dead held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (let ([member-of-held-inv? (member? state held-inv eq?)]
                 [member-of-fail-inv? (member? state fail-inv eq?)]
                 [member-of-cut-off?  (member? state cut-off eq?)])
             (add-node graph
                       state
                       #:atb (hash 'color (if (eq? (pda-start M) state) 'green 'black)
                                   'style (cond [(and member-of-held-inv? member-of-fail-inv?) 'wedged]
                                                [(or member-of-held-inv?
                                                     member-of-fail-inv?
                                                     member-of-cut-off?) 'filled]
                                                [(eq? state dead) 'dashed]
                                                [else 'solid])
                                   'shape (if (member? state (pda-finals M) eq?) 'doublecircle 'circle)
                                   'fillcolor (cond [member-of-cut-off? GRAPHVIZ-CUTOFF-GOLD]
                                                    [(and member-of-held-inv? member-of-fail-inv?)
                                                     "red:chartreuse4"]
                                                    [member-of-held-inv? HELD-INV-COLOR]
                                                    [member-of-fail-inv? BRKN-INV-COLOR]
                                                    [else 'white])
                                   'label state
                                   'fontcolor 'black
                                   'fontname (if (and member-of-held-inv? member-of-fail-inv?)
                                                 "times-bold"
                                                 "Times-Roman")))))
         dgraph
         (pda-states M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-shown-accept-rules current-accept-rules current-reject-rules dead)

  ;;rule symbol (listof rules) -> boolean
  ;;Purpose: Determines if the given rule is a member of the given (listof rules)
  ;;         or similiar to one of the rules in the given (listof rules) 
  (define (find-rule? rule dead lor)
    (or (member? rule lor equal?)
        (for/or ([r lor])
          (and (eq? (triple-source rule) (triple-source r))
               (or (eq? (triple-pop rule) (triple-pop r))
                   (and (eq? (triple-pop rule) (triple-pop r))
                        (eq? (triple-pop rule) dead)))))))

  (foldl (λ (rule graph)
           (let ([member-of-current-accept-rules? (member? rule current-accept-rules equal?)])
             (add-edge graph
                       (triple-read rule)
                       (triple-source rule)
                       (triple-pop rule)
                       #:atb (hash 'color (cond [(and (member? rule current-shown-accept-rules equal?)
                                                      member-of-current-accept-rules?)
                                                 SPLIT-ACCEPT-COLOR]
                                                [(find-rule? rule dead current-shown-accept-rules) TRACKED-ACCEPT-COLOR]
                                                [(find-rule? rule dead current-accept-rules)       ALL-ACCEPT-COLOR]
                                                [(find-rule? rule dead current-reject-rules)       REJECT-COLOR]
                                                [else 'black])
                                   'style (cond [(eq? (triple-pop rule) dead) 'dashed]
                                                [member-of-current-accept-rules? 'bold]
                                                [else 'solid])
                                   'fontsize FONT-SIZE))))
         dgraph
         rules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])

  ;;(listof symbols) (listof configurations) -> (listof configurations)
  ;;Purpose: Returns the configurations have the given word as unconsumed input
  (define (get-portion-configs word full-configs)
    (append-map (λ (config)
                  (for/list ([config (in-treelist (computation-LoC config))]
                             #:when (equal? (pda-config-word config) word))
                    config))
                full-configs))

  ;;(listof symbol ((listof symbol) (listof symbol) -> boolean))) (X -> Y) ->
  ;;(listof symbol ((listof symbol) (listof symbol) -> boolean)))
  ;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) (listof symbols) -> boolean)))
  (define (get-invariants LoI func)
    (filter-map-acc (λ (x) ((second (first x)) (second x) (third x))) first func first LoI))

  ;;(listof rules)
  ;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
  (define (make-rule-triples rules)
    ;;(listof symbols) -> string
    ;;Purpose: Converts the given los into a string
    (define (make-edge-label rule)
      (format "\n[~a ~a ~a]"
              (triple-read (rule-triple rule))
              (triple-pop (rule-triple rule))
              (pair-push (rule-pair rule))))
    
    (for/list ([rule rules])
      (triple (triple-source (rule-triple rule)) 
              (string->symbol (make-edge-label rule))
              (pair-destination (rule-pair rule)))))
  
 
  ;;(listof rules) -> (listof rules)
  ;;Purpose: Converts the given (listof configurations)s to rules
  (define (configs->rules a-config)
    (make-rule-triples
     (remove-duplicates
      (for/list ([rule a-config]
                 #:unless (equal? rule DUMMY-RULE))
        rule))))


  ;;(listof trace) (X -> Y) -> (listof rule)
  ;;Purpose: Extracts the rule from the first trace in a (listof trace)
  (define (get-trace-X LoT map-func)
    (filter-map-acc empty? map-func not first LoT))

  (let* (;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of all configurations
         [rejecting-rules (get-trace-X (building-viz-state-reject-traces a-vs) trace-rules)]

         ;;(listof configuration)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs (get-portion-configs (ci-upci (zipper-current (building-viz-state-CI a-vs))) (building-viz-state-acc-comp a-vs))]

         ;;(listof symbol)
         ;;Purpose: Gets the states where it's computation has cutoff
         [cut-off-states (if cut-off
                             (remove-duplicates (for/list ([computation (building-viz-state-computations a-vs)])
                                                  (pda-config-state (first computation))))
                             '())]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [accepting-rules (get-trace-X (building-viz-state-accept-traces a-vs) trace-rules)]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from of shown accepting computation
         [tracked-accepting-rules (get-trace-X (building-viz-state-tracked-accept-trace a-vs) trace-rules)]

         ;;(listof rule)
         ;;Purpose: Converts the current rules from the rejecting computations and makes them usable for graphviz
         [current-reject-rules (configs->rules (flatten rejecting-rules))]

         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-accept-rules (configs->rules (flatten accepting-rules))]

         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-shown-accept-rules (configs->rules (flatten tracked-accepting-rules))]

         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (pda-rules (building-viz-state-M a-vs)))]

         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (eq? (first invs) (pda-config-state curr)))
                     (list invs (ci-pci (zipper-current (building-viz-state-CI a-vs))) (pda-config-stack curr)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (get-invariants get-invs not)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (get-invariants get-invs id)])
    (make-edge-graph
     (make-node-graph
      (create-graph 'pdagraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      (building-viz-state-dead a-vs)
      held-invs
      brkn-invs
      cut-off-states)
     all-rules
     current-shown-accept-rules
     current-accept-rules
     current-reject-rules
     (building-viz-state-dead a-vs))))

;;viz-state -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs)
  ;;(listof trace) -> (listof trace)
  ;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
  (define (get-next-traces LoT)
    (filter-map-acc empty? rest not identity LoT))
  ;;viz-state (listof graph-thunks) -> (listof graph-thunks)
  ;;Purpose: Creates all the graphs needed for the visualization
  (define (create-graph-thunks-helper a-vs acc)
    (cond [(or (for/or ([config (building-viz-state-computations a-vs)])
                 (>= (pda-config-index (first config)) (building-viz-state-max-cmps a-vs)))
               (and (zipper-at-end? (building-viz-state-CI a-vs))
                    (or (zipper-empty? (building-viz-state-stack a-vs))
                        (zipper-at-end? (building-viz-state-stack a-vs)))
                    (for/or ([config (building-viz-state-computations a-vs)])
                      (>= (pda-config-index (last config)) (building-viz-state-max-cmps a-vs)))))
           (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
          [(or (and (equal? (ci-upci (zipper-current (building-viz-state-CI a-vs)))
                             (building-viz-state-farthest-consumed-input a-vs))
                    (not (empty? (building-viz-state-farthest-consumed-input a-vs))))
               (and (zipper-at-end? (building-viz-state-CI a-vs))
                    (or (zipper-empty? (building-viz-state-stack a-vs))
                        (zipper-at-end? (building-viz-state-stack a-vs)))))
           (reverse (cons (create-graph-thunk a-vs) acc))]
          [else (let ([next-graph (create-graph-thunk a-vs)])
                  (create-graph-thunks-helper (struct-copy
                                               building-viz-state
                                               a-vs
                                               [CI (if (zipper-at-end? (building-viz-state-CI a-vs))
                                                       (building-viz-state-CI a-vs)
                                                       (zipper-next (building-viz-state-CI a-vs)))]
                                               [stack (if (or (zipper-empty? (building-viz-state-stack a-vs))
                                                              (zipper-at-end? (building-viz-state-stack a-vs)))
                                                          (building-viz-state-stack a-vs)
                                                          (zipper-next (building-viz-state-stack a-vs)))]
                                               [computations (for/list ([computation (building-viz-state-computations a-vs)]
                                                                        #:do [(define rest-of-computation (rest computation))]
                                                                        #:unless (empty? rest-of-computation))
                                                               rest-of-computation)]
                                               [tracked-accept-trace
                                                (get-next-traces (building-viz-state-tracked-accept-trace a-vs))]
                                               [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                               [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                              (cons next-graph acc)))]))
  (create-graph-thunks-helper a-vs '()))


;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* ([imsg-state-ci (imsg-state-pda-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [imsg-state-farthest-consumed-input (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))]
         [imsg-state-shown-accepting-trace (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
         [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                                    (zipper-at-end? imsg-state-shown-accepting-trace))
                                                imsg-state-shown-accepting-trace
                                                (zipper-next imsg-state-shown-accepting-trace))]
         [imsg-state-stack (imsg-state-pda-stack (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [imsg-state-invs-zipper (imsg-state-pda-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [next-rule (if (zipper-empty? shown-accepting-trace)
                        shown-accepting-trace
                        (first (trace-rules (zipper-current shown-accepting-trace))))]
         [rule (if (zipper-empty? imsg-state-shown-accepting-trace) DUMMY-RULE next-rule)])
  (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-pda
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [ci (if (or (zipper-at-end? imsg-state-ci)
                                 (and (eq? (triple-read (rule-triple rule)) EMP)
                                      (not (zipper-at-end? imsg-state-shown-accepting-trace))
                                      (or (equal? DUMMY-RULE rule)
                                          (not (empty-rule? rule)))))
                             imsg-state-ci
                             (zipper-next imsg-state-ci))]
                     [shown-accepting-trace shown-accepting-trace]
                     [stack (if (or (zipper-empty? imsg-state-stack)
                                    (zipper-at-end? imsg-state-stack))
                                imsg-state-stack
                                (zipper-next imsg-state-stack))]
                     [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                                        [(and (not (zipper-at-end? imsg-state-invs-zipper))
                                              (>= (pda-accessor-func imsg-state-shown-accepting-trace)
                                                   (pda-config-index (first (first (zipper-unprocessed imsg-state-invs-zipper))))))
                                         (zipper-next imsg-state-invs-zipper)]
                                        [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-pda-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-farthest-consumed-input (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))]
        [imsg-state-stack (imsg-state-pda-stack (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-pda-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])

    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy
         imsg-state-pda
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         [ci (cond [(zipper-at-end? imsg-state-ci) imsg-state-ci]
                   [(not (empty? (pda-config-word imsg-state-farthest-consumed-input)))
                    (zipper-to-idx imsg-state-ci (pda-config-index imsg-state-farthest-consumed-input))]
                   [else (zipper-to-end imsg-state-ci)])]
         [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace) (zipper-at-end? imsg-state-shown-accepting-trace))
                                    imsg-state-shown-accepting-trace
                                    (zipper-to-end imsg-state-shown-accepting-trace))]
         [stack (cond [(zipper-empty? imsg-state-stack) imsg-state-stack]
                      [(or (zipper-empty? imsg-state-stack) (zipper-at-end? imsg-state-stack)) imsg-state-stack]
                      [else (zipper-to-end imsg-state-stack)])]
         ;;(zipperof invariant)
         ;;Purpose: The index of the last failed invariant
         [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper) (= (zipper-length imsg-state-invs-zipper) 1))
                          imsg-state-invs-zipper
                          (zipper-to-end imsg-state-invs-zipper))])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([imsg-state-ci (imsg-state-pda-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [imsg-state-farthest-consumed-input (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))]
         [imsg-state-shown-accepting-trace (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
         [imsg-state-stack (imsg-state-pda-stack (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [imsg-state-invs-zipper (imsg-state-pda-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))]
         [next-rule (if (zipper-empty? imsg-state-shown-accepting-trace)
                        imsg-state-shown-accepting-trace
                        (first (trace-rules (zipper-current imsg-state-shown-accepting-trace))))]
         [rule (if (zipper-empty? imsg-state-shown-accepting-trace) DUMMY-RULE next-rule)])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-pda
                     (informative-messages-component-state
                      (viz-state-informative-messages a-vs))
                     [ci (if (or (zipper-at-begin? imsg-state-ci)
                                 (and (eq? (triple-read (rule-triple rule)) EMP)
                                      (not (empty-rule? rule))))
                             imsg-state-ci
                             (zipper-prev imsg-state-ci))]
                     [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                                    (zipper-at-begin? imsg-state-shown-accepting-trace))
                                    imsg-state-shown-accepting-trace
                                    (zipper-prev imsg-state-shown-accepting-trace))]
                     [stack (if (or (zipper-empty? imsg-state-stack) (zipper-at-begin? imsg-state-stack))
                                imsg-state-stack
                                (zipper-prev imsg-state-stack))]

                     [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                                        [(and (not (zipper-at-begin? imsg-state-invs-zipper))
                                              (<= (pda-accessor-func imsg-state-shown-accepting-trace)
                                                  (pda-config-index (first (first (zipper-processed imsg-state-invs-zipper))))))
                                         (zipper-prev imsg-state-invs-zipper)]
                                        [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
(define (up-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-pda-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-farthest-consumed-input (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))]
        [imsg-state-stack (imsg-state-pda-stack (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-pda-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-pda
                     (informative-messages-component-state
                      (viz-state-informative-messages a-vs))
                     [ci (if (zipper-at-begin? imsg-state-ci) imsg-state-ci (zipper-to-begin imsg-state-ci))]
                   
                     [shown-accepting-trace (if (or (zipper-empty? imsg-state-shown-accepting-trace)
                                                    (zipper-at-begin? imsg-state-shown-accepting-trace))
                                                imsg-state-shown-accepting-trace
                                                (zipper-to-begin imsg-state-shown-accepting-trace))]
                     [stack (if (or (zipper-empty? imsg-state-stack) (zipper-at-begin? imsg-state-stack))
                                imsg-state-stack
                                (zipper-to-begin imsg-state-stack))]
                     [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper) (zipper-at-begin? imsg-state-invs-zipper))
                                      imsg-state-invs-zipper
                                      (zipper-to-idx imsg-state-invs-zipper 0))])])])))

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
                    (struct-copy imsg-state-pda
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
                                (struct-copy imsg-state-pda
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-pda-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-pda-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))]
        [imsg-state-stack (imsg-state-pda-stack (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-pda-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (if (or (zipper-empty? imsg-state-invs-zipper)
          (and (zipper-at-begin? imsg-state-invs-zipper)
               (not (zipper-at-end? imsg-state-invs-zipper)))
          (< (pda-accessor-func imsg-state-shown-accepting-trace)
             (get-index-pda imsg-state-invs-zipper)))
      a-vs
      (let* ([zip (if (and (not (zipper-at-begin? imsg-state-invs-zipper))
                           (<= (pda-accessor-func imsg-state-shown-accepting-trace)
                               (get-index-pda imsg-state-invs-zipper)))
                      (zipper-prev imsg-state-invs-zipper)
                      imsg-state-invs-zipper)])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-index-pda zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state-pda
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [ci (if (< (zipper-length imsg-state-ci) (get-index-pda zip))
                                 (zipper-to-end imsg-state-ci)
                                 (zipper-to-idx imsg-state-ci (get-index-pda zip)))]
                         
                         [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                                    imsg-state-shown-accepting-trace
                                                    (zipper-to-idx imsg-state-shown-accepting-trace (get-index-pda zip)))]
                         
                         [stack (if (zipper-empty? imsg-state-stack) imsg-state-stack (zipper-to-idx imsg-state-stack (get-index-pda zip)))]
                         
                         
                         [invs-zipper zip])])])))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-pda-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))]
        [imsg-state-stack (imsg-state-pda-stack (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-pda-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
  (if (or (zipper-empty? imsg-state-invs-zipper)
          (and (zipper-at-end? imsg-state-invs-zipper)
               (not (zipper-at-begin? imsg-state-invs-zipper)))
          (> (pda-accessor-func imsg-state-shown-accepting-trace)
             (get-index-pda imsg-state-invs-zipper)))
      a-vs
      (let* ([zip (if (and (not (zipper-at-end? imsg-state-invs-zipper))
                           (>= (pda-accessor-func imsg-state-shown-accepting-trace)                            
                               (get-index-pda imsg-state-invs-zipper)))
                      (zipper-next imsg-state-invs-zipper)
                      imsg-state-invs-zipper)])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-index-pda zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state-pda
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [ci (zipper-to-idx imsg-state-ci (get-index-pda zip))]
                             
                         [shown-accepting-trace (if (zipper-empty? imsg-state-shown-accepting-trace)
                                                    imsg-state-shown-accepting-trace
                                                    (zipper-to-idx imsg-state-shown-accepting-trace (get-index-pda zip)))]
                         
                         [stack (if (zipper-empty? imsg-state-stack)
                                                    imsg-state-stack
                                                    (zipper-to-idx imsg-state-stack (get-index-pda zip)))] 
                         
                         [invs-zipper zip])])])))))

;;machine -> machine
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (local [;;symbol
          ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
          ;;         otherwise uses DEAD
          (define dead (if (member? DEAD (pda-getstates M) eq?) (gen-state (pda-getstates M)) DEAD))
          ;;(listof symbols)
          ;;Purpose: Makes partial rules for every combination of states in M and symbols in sigma of M
          (define new-read-rules
            (for*/list ([states (pda-getstates M)]
                        [sigma (pda-getalphabet M)])
              (list states sigma)))

          ;;(listof rules)
          ;;Purpose: Makes rules for that empty the stack and transition to the ds
          (define dead-pop-rules
            (for*/list ([ds (list dead)]
                        [gamma (pda-getgamma M)])
              (list (list ds EMP (list gamma)) (list ds EMP))))

          ;;(listof rules)
          ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
          (define dead-read-rules
            (for*/list ([ds (list dead)]
                        [sigma (pda-getalphabet M)])
              (list (list ds sigma EMP) (list ds EMP))))
          ;;(listof rules)
          ;;Purpose: Gets rules that are not currently in the original rules of M
          (define get-rules-not-in-M  (local [(define partial-rules (map (λ (rule)
                                                                           (list (first (first rule)) (second (first rule))))
                                                                         (pda-getrules M)))]
                                        (filter (λ (rule)
                                                  (not (member? rule partial-rules equal?)))
                                                new-read-rules)))
          ;;(listof rules)
          ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
          (define rules-to-dead
            (map (λ (rule) (cons (append rule (list EMP)) (list (list dead EMP))))
                 get-rules-not-in-M))]
    (pda (cons dead (pda-getstates M))
         (pda-getalphabet M)
         (pda-getgamma M)
         (pda-getstart M)
         (pda-getfinals M)
         (remake-rules (append (pda-getrules M) rules-to-dead dead-read-rules dead-pop-rules)))))


;;pda word [boolean] [natnum] . -> (void)
;;Purpose: Visualizes the given pda processing the given word
(define (pda-viz M a-word #:add-dead [add-dead #f] #:cut-off [cut-off 100] invs)

  ;;(listof configuration) (listof rules) (listof configurations) -> (listof configurations)
  ;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
  ;;         tracks each transition
  ;; acc = the trace of processed configurations and corresponding rules
  (define (make-trace configs rules acc)
    (cond [(and (= (length configs) 1) (empty? rules))
           (let* ([rle (rule (triple EMP EMP EMP) (pair EMP EMP))]
                  [res (trace (first configs) (list rle))])
             (reverse (cons res acc)))]
          [(empty? rules) (reverse acc)]
          [(and (empty? acc)
                (not (empty-rule? (first rules))))
           (let* ([rle (rule (triple EMP EMP EMP) (pair EMP EMP))]
                  [res (trace (first configs) (list rle))])
             (make-trace (rest configs) rules (cons res acc)))]
          [(and (not (empty? acc))
                (empty-rule? (first rules)))
           (let* ([rle (first rules)]
                  [res (struct-copy trace (first acc)
                                    [rules (cons rle (trace-rules (first acc)))])])
             (make-trace (rest configs) (rest rules) (cons res (rest acc))))]
          [else (let* ([rle (first rules)]
                       [res (trace (first configs) (list rle))])
                  (make-trace (rest configs) (rest rules) (cons res acc)))]))

  

  ;;word (listof computation) (listof (list symbol (word -> boolean)) -> (list (listof computation) boolean)
  ;;Purpoes: Extracts all the computations where its corresponding invariant doesn't hold
  (define (get-failed-invariants a-word LoC invariants)
    ;;(lisof computations) -> (listof computations)
    ;;Purpose: Makes configurations usable for invariant predicates
    (define (make-inv-configs LoC)

      ;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
      ;;Purpose: Makes configurations usable for invariant predicates
      (define (make-inv-configs-helper configs word-len)
        (letrec ([a-word-len (length a-word)]
                 [self (lambda (configs word-len)
                         (let* ([current-config (filter (λ (config) (= (length (pda-config-word config)) word-len)) configs)]
                                [inv-config (map (λ (configs)
                                                   (struct-copy pda-config configs
                                                                [word (drop-right a-word word-len)]))
                                                 current-config)])
                           (if (empty? configs)
                               '()
                               (append inv-config
                                       (self (rest configs) (sub1 word-len))))))])
          (self configs word-len)))

      (let ([word-len (length a-word)])
        (map (λ (comp)
               (make-inv-configs-helper (treelist->list (computation-LoC comp)) word-len))
             LoC)))

    ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
    ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
    (define (get-inv-config-results inv-configs)
    ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
    ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
    (define (get-inv-config-results-helper inv-configs)
      (if (or (empty? invs) (empty? inv-configs))
          '()
          (let* ([get-inv-for-inv-config (filter (λ (inv)
                                                   (equal? (first inv) (pda-config-state (first inv-configs))))
                                                 invs)]
                 [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                         '()
                                         (second (first get-inv-for-inv-config)))]
                 [inv-config-result (if (empty? inv-for-inv-config)
                                        '()
                                        (cons (first inv-configs)
                                              (list (inv-for-inv-config (pda-config-word (first inv-configs))
                                                                        (pda-config-stack (first inv-configs))))))])
            (if (empty? inv-config-result)
                (get-inv-config-results-helper (rest inv-configs))
                (cons inv-config-result
                      (get-inv-config-results-helper (rest inv-configs)))))))

    (append-map (λ (comp)
                  (get-inv-config-results-helper comp))
                inv-configs))

    ;;(listof configurations) (listof sybmols) -> (listof configurations)
    ;;Purpose: Extracts all the invariant configurations that failed
    (define (return-brk-inv-configs inv-config-results)
      (remove-duplicates (filter (λ (config) (not (second config))) inv-config-results)))
  
    (return-brk-inv-configs (get-inv-config-results (make-inv-configs LoC))))
  
  


  ;; (listof computation) (listof symbol) -> (listof symbol)
  ;; Purpose: Returns the most consumed input
  ;;acc = the word with smallest unconsumed input
  (define (get-farthest-consumed LoC acc)
    (cond [(empty? LoC) acc]
          [(< (length (pda-config-word (treelist-last (first LoC)))) (length (pda-config-word acc)))
           (get-farthest-consumed (rest LoC) (treelist-last (first LoC)))]
          [else (get-farthest-consumed (rest LoC) acc)]))

  ;;computation -> computation 
  ;;Purpose: removes any empty transitions from given computation
  (define (remove-empty a-computation)
    ;;computation -> computation
    ;;Purpose: removes any empty transitions from given computation
    ;;acc = the traversed configurations in the computation
    (define (remove-empty-helper computation acc)
      (cond [(< (length computation) 2) (reverse (append computation acc))]
            [(and (equal? (pda-config-word (first computation)) (pda-config-word (second computation)))
                  (equal? (pda-config-stack (first computation)) (pda-config-stack (second computation))))
             (remove-empty-helper (rest computation) acc)]
            [else (remove-empty-helper (rest computation) (cons (first computation) acc))]))
    (remove-empty-helper a-computation '()))
  
  (let* (;;pda ;;Purpose: A pda (structure) with the dead state if add-dead is true
         [new-M (if add-dead (make-new-M M)
                    (pda (pda-getstates M)
                         (pda-getalphabet M)
                         (pda-getgamma M)
                         (pda-getstart M)
                         (pda-getfinals M)
                         (remake-rules (pda-getrules M))))]
         ;;symbol ;;Purpose: The name of the dead state
         [dead-state (if add-dead (first (pda-states new-M)) 'no-dead)]
         ;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations+hash (get-computations a-word (pda-rules new-M) (pda-start new-M) (pda-finals new-M) cut-off)]

         [computations (treelist->list (first computations+hash))]

         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;number ;;Purpose: The length of the word
         ;[word-len (length a-word)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (filter (λ (comp)
                                           (and (member? (pda-config-state (treelist-last (computation-LoC comp)))
                                                         (pda-finals new-M) eq?)
                                                (empty? (pda-config-word (treelist-last (computation-LoC comp))))
                                                (empty? (pda-config-stack (treelist-last (computation-LoC comp))))))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accept-cmps (map (λ (acc-comp)
                             (make-trace (treelist->list (computation-LoC acc-comp))
                                         (treelist->list (computation-LoR acc-comp))
                                         '()))
                           accepting-computations)]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (filter (λ (config)
                                           (not (member? config accepting-computations equal?)))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
         [rejecting-traces (map (λ (computation)
                                  (make-trace (treelist->list (computation-LoC computation))
                                              (treelist->list (computation-LoR computation))
                                              '()))
                                rejecting-computations)]
         ;;(zipperof computation) ;;Purpose: Gets the stack of the first accepting computation
         [stack (remove-empty (if (empty? accepting-computations)
                                                '()
                                                (treelist->list (computation-LoC (first accepting-computations)))))]

         [computation-lens (begin
                             (for ([key (in-list (hash-keys (second computations+hash)))])
                               (hash-set! (second computations+hash)
                                          key
                                          (set-count (hash-ref (second computations+hash) key))))
                             (second computations+hash))]

         ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
         [accepting-trace (if (empty? accept-cmps) '() (first accept-cmps))]
         ;;(listof symbol) ;;Purpose: The portion of the ci that the machine can conusme the most 
         [most-consumed-word (let ([last-word (if (empty? accepting-trace)
                                                  (get-farthest-consumed LoC (pda-config (pda-start new-M) a-word '() 0))
                                                  (pda-config (pda-start new-M) '() '() 0))])
                               (if (empty? (pda-config-word last-word))
                                   last-word
                                   (struct-copy pda-config
                                                last-word
                                                [word (rest (pda-config-word last-word))]
                                                [index (add1 (pda-config-index last-word))])))]
         [CIs (remake-ci a-word)]
         [computation-has-cut-off? (and (empty? accepting-trace)
                                        (for/or ([computation LoC])
                                          (>= (pda-config-index (treelist-last computation)) cut-off)))]
         ;;(listof computation)
         ;;Purpose: Gets all the cut off computations if the length of the word is greater than max computations
         [get-cut-off-trace (if computation-has-cut-off? (map last rejecting-traces) '())]
         ;;(listof computation)
         ;;Purpose: Makes the cut off computations if the length of the word is greater than max computations
         [cut-off-traces (if (empty? get-cut-off-trace)
                             rejecting-traces
                             (map (λ (configs last-reject)
                                    (append configs (list last-reject)))
                                  rejecting-traces
                                  get-cut-off-trace))]
         ;;building-state struct
         [building-state (building-viz-state CIs
                                             (for/list ([computation LoC]) (treelist->list computation))
                                             accepting-computations
                                             (list->zipper stack)
                                             (list accepting-trace)
                                             (if (empty? accept-cmps) '() (rest accept-cmps))
                                             cut-off-traces
                                             new-M
                                             (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w s) #t)) invs) invs)
                                             dead-state
                                             cut-off
                                             (if computation-has-cut-off? '() most-consumed-word))]

         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state)]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         [inv-configs (get-failed-invariants a-word accepting-computations invs)])
    ;stack
    (zipper->list CIs)
    ;(length graphs)
    ;LoC
    accepting-trace
    ;accepting-computations
    ;most-consumed-word
   (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ PDA-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages pda-create-draw-informative-message
                                   (imsg-state-pda new-M
                                                   CIs
                                                   (list->zipper accepting-trace)
                                                   (list->zipper stack) 
                                                   most-consumed-word 
                                                   (list->zipper inv-configs)
                                                   computation-lens 
                                                   computation-has-cut-off? 
                                                   cut-off
                                                   0
                                                   (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                     (if (> 0 offset-cap) 0 offset-cap))
                                                   0)
                                   pda-img-bounding-limit)
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       PDA-E-SCENE-HEIGHT
                                                       (image-height pda-info-img)
                                                       INS-TOOLS-BUFFER)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       PDA-E-SCENE-HEIGHT
                                                       (image-height pda-info-img)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))))
             (create-viz-draw-world E-SCENE-WIDTH PDA-E-SCENE-HEIGHT INS-TOOLS-BUFFER)
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
                                     [ "j" pda-jump-prev j-key-pressed]
                                     [ "l" pda-jump-next l-key-pressed]
                                     )
             (create-viz-process-tick PDA-E-SCENE-BOUNDING-LIMITS
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      PDA-E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ( [pda-img-bounding-limit
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
                                        [ J-KEY-DIMS pda-jump-prev j-key-pressed]
                                        [ L-KEY-DIMS pda-jump-next l-key-pressed]))
             'pda-viz)
    #;(if testing?
        (void)
        (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ PDA-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages pda-create-draw-informative-message
                                   (imsg-state-pda new-M
                                                   CIs
                                                   a-word 
                                                   '() 
                                                   (list->zipper accepting-trace)
                                                   stack 
                                                   most-consumed-word 
                                                   (list->zipper inv-configs)
                                                   computation-lens 
                                                   LoC 
                                                   cut-off
                                                   0
                                                   (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                     (if (> 0 offset-cap) 0 offset-cap))
                                                   0)
                                   pda-img-bounding-limit)
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       PDA-E-SCENE-HEIGHT
                                                       (image-height pda-info-img)
                                                       INS-TOOLS-BUFFER)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       PDA-E-SCENE-HEIGHT
                                                       (image-height pda-info-img)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))))
             (create-viz-draw-world E-SCENE-WIDTH PDA-E-SCENE-HEIGHT INS-TOOLS-BUFFER)
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
                                     [ "j" pda-jump-prev j-key-pressed]
                                     [ "l" pda-jump-next l-key-pressed]
                                     )
             (create-viz-process-tick PDA-E-SCENE-BOUNDING-LIMITS
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      PDA-E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ( [pda-img-bounding-limit
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
                                        [ J-KEY-DIMS pda-jump-prev j-key-pressed]
                                        [ L-KEY-DIMS pda-jump-next l-key-pressed]))
             'pda-viz))))

(define numb>numa (make-unchecked-cfg '(S A)
                                      '(a b)
                                      `((S ,ARROW b)
                                        (S ,ARROW AbA)
                                        (A ,ARROW AaAbA)
                                        (A ,ARROW AbAaA)
                                        (A ,ARROW ,EMP)
                                        (A ,ARROW bA))
                                      'S))

(define pd-numb>numa (cfg->pda numb>numa))

#;(time (pda-viz pd-numb>numa '(a b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b) '()
                 #:cut-off 15))
#;(time (pda-viz pd-numb>numa
                 '(a b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b) '()))
#;(profile-thunk (lambda ()
                   (pda-viz pd-numb>numa
                            '(a b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b) '()
                            #:cut-off 15))
                 #:repeat 10
                 #:svg-path (string->path "/home/sora/Pictures/test-flame.svg"))

(define P3 (make-unchecked-ndpda '(S H)
                                 '(a b)
                                 '(b)
                                 'S
                                 '(H)
                                 `(((S ε ε)(H ε))
                                   ((S a ε)(S (b b)))
                                   ((H b (b b))(H ε)) ((H b (b))(H ε)))))

#;(time (pda-viz P3 '(a a b a b b b) '()))


(define P2 (make-unchecked-ndpda '(S H)
                                 '(a b)
                                 '(b)
                                 'S
                                 '(H)
                                 `(((S ε ε)(H ε))     ((S a ε)(S (b b)))
                                                      ((H b (b b))(H ε)) ((H ε (b))(H ε)))))

;;purpose: to determine if the number of a's in the word is less than or equal the number of 'b's
(define (P-S-INV a-word stck)
  (and (andmap (λ (w) (eq? w 'b)) stck)
       (andmap (λ (w) (eq? w 'a)) a-word)
       (= (* 2 (length a-word)) (length stck))))
;;Purpose: to determine if the number of b's in the stack is greater than or equal to the number of b's in the ci
(define (P-H-INV ci stck)
  (let ([ci-as (filter (λ (w) (eq? w 'a)) ci)]
        [ci-bs (filter (λ (w) (eq? w 'b)) ci)])
    (and (equal? ci (append ci-as ci-bs))
         (andmap (λ (w) (eq? w 'b)) stck)
         (<= (length ci-as) (length (append ci-bs stck)) (* 2 (length ci-as))))))

#;(time (pda-viz P2 '(a a a b b) (list (list 'S P-S-INV) (list 'H P-H-INV))))


;[(<= max-cmps 0) (error (format "The maximum amount of computations, ~a, must be integer greater than 0" max-cmps))] DONT FORGET