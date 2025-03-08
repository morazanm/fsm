#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/viz.rkt"
         racket/treelist
         "../viz-lib/zipper.rkt"
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

;;upci is the unprocessed consumed input (listof symbol)
;;pci is the proccessed consumed input (listof symbol)
;;computations is a (listof computation) that attempt to consume the ci
;;accepting computations is (listof computation) for all accepting computations
;;stack is a (zipperof computation)
;;accept-traces is a (listof configuration)
;;reject-traces is a (listof configuration)
;;M is the given machine
;;inv is a the (listof (state (listof symbol -> boolean)))
;;dead is the sybmol of dead state
;;the max amount of transitions the machine can make
;;farthest-consumed-input (listof symbol) => the portion the ci that the machine consumed the most of
(struct building-viz-state (upci pci computations acc-comp stack tracked-accept-trace
                                 accept-traces reject-traces M inv dead max-cmps farthest-consumed-input))

(define get-index (compose1 config-index zipper-current))

;(struct config (state word stack index))

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func)
  (ormap (λ (L) (eq-func x L)) lst))

;;rule -> boolean
;;Purpose: Determines if the given rule is an empty rule (e.i. reads, pops, and pushes empty)
(define (empty-rule? a-rule)
  (and (eq? (triple-read (rule-triple a-rule)) EMP)
       (eq? (triple-pop  (rule-triple a-rule)) EMP)
       (eq? (pair-push   (rule-pair a-rule))   EMP)))


;;word (listof rule) symbol number -> (listof computation) hash table
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start finals max-cmps)

  ;;config rule -> config
  ;;Purpose: Applys the given rule to the given config and returns the updated config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-rule a-comp a-rule)
  
    ;;config -> config
    ;;Purpose: Applies the read portion of given rule to the given config
    ;;ASSUMPTION: The given rule can be applied to the config
    (define (apply-read a-config)
      (if (eq? (triple-read (rule-triple a-rule)) EMP)
          (config (pair-destination (rule-pair a-rule)) (config-word a-config) (config-stack a-config) (config-index a-config))
          (config (pair-destination (rule-pair a-rule)) (rest (config-word a-config)) (config-stack a-config) (config-index a-config))))
  
    ;;config -> config
    ;;Purpose: Applies the pop portion of given rule to the given config
    ;;ASSUMPTION: The given rule can be applied to the config
    (define (apply-pop a-config)
      (if (eq? (triple-pop (rule-triple a-rule)) EMP)
          a-config
          (struct-copy config a-config
                       [stack (drop (config-stack a-config) (length (triple-pop (rule-triple a-rule))))])))
  
    ;;config -> config
    ;;Purpose: Applies the push portion of given rule to the given config
    ;;ASSUMPTION: The given rule can be applied to the config
    (define (apply-push a-config)
      (if (eq? (pair-push (rule-pair a-rule)) EMP)
          a-config
          (struct-copy config a-config
                       [stack (append (pair-push (rule-pair a-rule)) (config-stack a-config))])))
  
    ;;config -> config
    ;;Purpose: Updates the config's number if something gets applied to the config (e.i. read/pop/push)
    ;;ASSUMPTION: The given rule can be applied to the config
    (define (update-count a-config)
      (if (empty-rule? a-rule)
          a-config
          (struct-copy config a-config [index (add1 (config-index a-config))])))
    
    
    (struct-copy computation a-comp
                 [LoC (cons (update-count (apply-push (apply-pop (apply-read (first (computation-LoC a-comp))))))
                            (computation-LoC a-comp))]
                 [LoR (cons a-rule (computation-LoR a-comp))]
                 #;[visited (set-add (computation-visited a-comp) (first (computation-LoC a-comp)))
                          #;(cons (first (computation-LoC a-comp)) (computation-visited a-comp))]))


  (define visited-configuration-set (set))

  (define (update-visited a-computation)
    (set-add visited-configuration-set (first (computation-LoC a-computation))))
  
  
  (define computation-number-hash (make-hash))

  (define (update-hash a-computation)
    (hash-set! computation-number-hash
               (config-word (first (computation-LoC a-computation)))
               (cons (first (computation-LoC a-computation))
                     (hash-ref computation-number-hash
                               (config-word (first (computation-LoC a-computation)))
                               '()))))
  
  
  
  ;;(listof rules) (queueof computation) (listof computation) number -> (listof computation)
  ;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
  ;;     that are within the bounds of the max computation limit
  (define (make-computations lor finals QoC path max-cmps)
    (letrec ([self (lambda (QoC path)
                     (cond [(qempty? QoC) (list path computation-number-hash)]
                           [(or (>= (length (computation-LoC (qfirst QoC))) max-cmps)
                                (and (member? (config-state (first (computation-LoC (qfirst QoC)))) finals eq?)
                                     (empty? (config-word (first (computation-LoC (qfirst QoC)))))
                                     (empty? (config-stack (first (computation-LoC (qfirst QoC)))))))
                            (let ([update-count (update-hash (qfirst QoC))])
                              (self (dequeue QoC) (cons (qfirst QoC) path)))]
                           [else
                            (let* ([stack (config-stack (first (computation-LoC (qfirst QoC))))]
                                   ;;(listof rules)
                                   ;;Purpose: Holds all rules that consume a first letter in the given configurations
                                   [connected-read-rules (filter (λ (rule)
                                                                   (and (not (empty? (config-word
                                                                                      (first (computation-LoC (qfirst QoC))))))
                                                                        (eq? (triple-source (rule-triple (first rule)))
                                                                             (config-state
                                                                              (first (computation-LoC (qfirst QoC)))))
                                                                        (eq? (triple-read (rule-triple (first rule)))
                                                                             (first (config-word
                                                                                     (first (computation-LoC (qfirst QoC))))))))
                                                                 lor)]
                                   ;;(listof rules)
                                   ;;Purpose: Holds all rules that consume no input for the given configurations
                                   [connected-read-E-rules (filter (λ (rule)
                                                                     (and (eq? (triple-source (rule-triple (first rule)))
                                                                               (config-state
                                                                                (first (computation-LoC (qfirst QoC)))))
                                                                          (eq? (triple-read (rule-triple (first rule))) EMP)))
                                                                   lor)]
                                   ;;(listof rules)
                                   ;;Purpose: Holds all rules that can pop what is in the stack
                                   [connected-pop-rules (filter (λ (rule)
                                                                  (or (eq? (triple-pop (rule-triple (first rule))) EMP)
                                                                      (and (>= (length stack)
                                                                               (length (triple-pop (rule-triple (first rule)))))
                                                                           (equal? (take stack
                                                                                         (length
                                                                                          (triple-pop (rule-triple (first rule)))))
                                                                                   (triple-pop (rule-triple (first rule)))))))
                                                                (append connected-read-E-rules connected-read-rules))]
                                   [new-configs (filter (λ (new-c) 
                                                          (not (set-member? visited-configuration-set
                                                                            (first (computation-LoC new-c))))
                                                          #;(not (member? (first (computation-LoC new-c))
                                                                          (computation-visited new-c) equal?)))
                                                        (map (λ (rule) (apply-rule (qfirst QoC) (first rule))) connected-pop-rules))]
                                   [update-count (update-hash (qfirst QoC))]
                                   [update-visited (update-visited (qfirst QoC))])
                              (if (empty? new-configs)
                                  (self (dequeue QoC) (cons (qfirst QoC) path))
                                  (self (enqueue new-configs (dequeue QoC)) path)))]))])
      (self QoC path)))
  
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (list (config start a-word '() 0))
                                           '()
                                           (set))])
    (make-computations lor
                       finals
                       (enqueue (list starting-computation) E-QUEUE)
                       '()
                       max-cmps)))

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M dead held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (pda-start M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv eq?) (member? state fail-inv eq?)) 'wedged]
                                              [(or (member? state held-inv eq?)
                                                   (member? state fail-inv eq?)
                                                   (member? state cut-off eq?)) 'filled]
                                              [(eq? state dead) 'dashed]
                                              [else 'solid])
                                 'shape (if (member? state (pda-finals M) eq?) 'doublecircle 'circle)
                                 'fillcolor (cond [(member? state cut-off eq?) GRAPHVIZ-CUTOFF-GOLD]
                                                  [(and (member? state held-inv eq?) (member? state fail-inv eq?))
                                                   "red:chartreuse4"]
                                                  [(member? state held-inv eq?) HELD-INV-COLOR ]
                                                  [(member? state fail-inv eq?) BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 'fontcolor 'black
                                 'fontname (if (and (member? state held-inv eq?) (member? state fail-inv equal?))
                                               "times-bold"
                                               "Times-Roman"))))
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
        (ormap (λ (r)
                 (and (eq? (first rule) (first r))
                      (or (eq? (third rule) (third r))
                          (and (eq? (third rule) (third r))
                               (eq? (third rule) dead)))))
               lor)))
  
  (foldl (λ (rule graph)
           (add-edge graph
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(and (member? rule current-shown-accept-rules eq?)
                                                    (member? rule current-accept-rules equal?))
                                               SPLIT-ACCEPT-COLOR]
                                              [(find-rule? rule dead current-shown-accept-rules) TRACKED-ACCEPT-COLOR]
                                              [(find-rule? rule dead current-accept-rules)       ALL-ACCEPT-COLOR]
                                              [(find-rule? rule dead current-reject-rules)       REJECT-COLOR]
                                              [else 'black])
                                 'style (cond [(eq? (third rule) dead) 'dashed]
                                              [(member? rule current-accept-rules equal?) 'bold]
                                              [else 'solid])
                                 'fontsize FONT-SIZE)))
         dgraph
         rules))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  
  ;;(listof symbols) (listof configurations) -> (listof configurations)
  ;;Purpose: Returns the configurations have the given word as unconsumed input
  (define (get-portion-configs word full-configs)
    (append-map (λ (config)
                  (filter (λ (configs)
                            (equal? (config-word configs) word))
                          (computation-LoC config)))
                full-configs))

  ;; (listof configuration) number -> (listof configuration)
  ;; Purpose: Returns the first configuration in a (listof configuration) if it exceeds the cut-off amount
  (define (get-cut-off LoC max-cmps)
    (filter-map (λ (config)
                  (and (>= (length config) max-cmps)
                       (first config)))
                LoC))

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
      (format "\n[~a ~a ~a]" (triple-read (rule-triple rule))
              (triple-pop (rule-triple rule))
              (pair-push (rule-pair rule))))
  
    (map (λ (rule)
           (append (list (triple-source (rule-triple (first rule))))
                   (list (string->symbol (make-edge-label (first rule))))
                   (list (pair-destination (rule-pair (first rule))) #;(first (second rule)))))
         rules))


  
  ;;(listof rules) -> (listof rules)
  ;;Purpose: Converts the given (listof configurations)s to rules
  (define (configs->rules a-config)
  
    ;;(listof rule-struct) -> (listof rule)
    ;;Purpose: Remakes the rules extracted from the rule-struct
    (define (remake-rules lor)
      (map (λ (rule)
             (rule (triple (first (first rule))
                           (second (first rule))
                           (third (first rule)))
                   (pair (first (second rule))
                         (second (second rule))))
             #;(list (triple rule)
                     (rule-pair rule)))
           lor)
      #;(append-map (λ (lor)
                      (map (λ (rule)
                             (list (rule-triple rule)
                                   (rule-pair rule)))
                           lor))
                    trace-rules))
    ;(displayln (first a-config))
    (make-rule-triples
     (remove-duplicates
      (filter (λ (rule)
                (not (equal? rule DUMMY-RULE)))
              a-config))))

  
  ;;(listof trace) (X -> Y) -> (listof rule)
  ;;Purpose: Extracts the rule from the first trace in a (listof trace)
  (define (get-trace-X LoT map-func)
    (filter-map-acc empty? map-func not first LoT))
  
  (let* (;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of all configurations
         [rejecting-rules (get-trace-X (building-viz-state-reject-traces a-vs) trace-rules)]

         ;;(listof configuration)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs (get-portion-configs (building-viz-state-upci a-vs)
                                               (building-viz-state-acc-comp a-vs))]

         ;;(listof symbol)
         ;;Purpose: Gets the states where it's computation has cutoff
         [cut-off-states (if cut-off
                             (remove-duplicates (map config-state (get-cut-off (building-viz-state-computations a-vs)
                                                                               (building-viz-state-max-cmps a-vs))))
                             '())]
         
         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [accepting-rules (get-trace-X (building-viz-state-accept-traces a-vs) trace-rules)]         

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from of shown accepting computation
         [tracked-accepting-rules (get-trace-X (building-viz-state-tracked-accept-trace a-vs) trace-rules)]
         
         ;;(listof rule)
         ;;Purpose: Converts the current rules from the rejecting computations and makes them usable for graphviz
         [current-reject-rules (configs->rules rejecting-rules)]
                  
         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-accept-rules (configs->rules accepting-rules)]

         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-shown-accept-rules (configs->rules tracked-accepting-rules)]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (pda-rules (building-viz-state-M a-vs)))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (eq? (first invs) (config-state curr)))
                     (list invs (building-viz-state-pci a-vs) (config-stack curr)))]

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
    
;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs)
  ;;(listof trace) -> (listof trace)
  ;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
  (define (get-next-traces LoT)
    (filter-map-acc empty? rest not id LoT))
  
  (define (create-graph-thunks-helper a-vs acc)
    (cond [(and (empty? (building-viz-state-upci a-vs))
                (or (list? (building-viz-state-stack a-vs))
                    (zipper-at-end? (building-viz-state-stack a-vs))))
           (reverse (cons (create-graph-thunk a-vs) acc))]
          [(and (eq? (building-viz-state-upci a-vs) (building-viz-state-farthest-consumed-input a-vs))
                (ormap (λ (comp-len) (>= comp-len (building-viz-state-max-cmps a-vs)))
                       (map length (building-viz-state-computations a-vs))))
           (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
          [else (let ([next-graph (create-graph-thunk a-vs)])
                  (create-graph-thunks-helper (struct-copy
                                               building-viz-state
                                               a-vs
                                               [upci (if (empty? (building-viz-state-upci a-vs))
                                                         (building-viz-state-upci a-vs)
                                                         (rest (building-viz-state-upci a-vs)))]
                                               [pci (if (empty? (building-viz-state-upci a-vs))
                                                        (building-viz-state-pci a-vs)
                                                        (append (building-viz-state-pci a-vs)
                                                                (list (first (building-viz-state-upci a-vs)))))]
                                               [stack (if (or (zipper-empty? (building-viz-state-stack a-vs))
                                                              (zipper-at-end? (building-viz-state-stack a-vs)))
                                                          (building-viz-state-stack a-vs)
                                                          (zipper-next (building-viz-state-stack a-vs)))]
                                               [computations (filter (λ (comp)    
                                                                       (not (eq? (config-word (first comp))
                                                                                 (building-viz-state-upci a-vs))))
                                                                     (building-viz-state-computations a-vs))]
                                               [tracked-accept-trace
                                                (get-next-traces (building-viz-state-tracked-accept-trace a-vs))]
                                               [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                               [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                              (cons next-graph acc)))]))
  (create-graph-thunks-helper a-vs '()))


;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (#;[completed-config? (ormap (λ (config) (empty? (config-word (first (computation-LoC config)))))
                                   (first (get-computations (imsg-state-pda-pci (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))
                                                            (pda-getrules (imsg-state-pda-M (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs))))
                                                            (pda-getstart (imsg-state-pda-M (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs))))
                                                            (pda-getfinals (imsg-state-pda-M (informative-messages-component-state
                                                                                              (viz-state-informative-messages a-vs))))
                                                            (imsg-state-pda-max-cmps (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs))))))]
         ;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [pci (if (or #;(not completed-config?)
                      (empty? (imsg-state-pda-upci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))
                      (eq? (imsg-state-pda-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                           (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))
                  (imsg-state-pda-pci (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))
                  (append (imsg-state-pda-pci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                          (list (first (imsg-state-pda-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))))]
         [pci-len (length pci)])
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
                     [upci (if (or #;(not completed-config?)
                                   (empty? (imsg-state-pda-upci (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                   (eq? (imsg-state-pda-upci (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))
                                        (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))))
                               
                               (imsg-state-pda-upci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-pda-upci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [shown-accepting-trace (if (or (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (zipper-at-end? (imsg-state-pda-shown-accepting-trace
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))
                                                (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs)))
                                                (zipper-next (imsg-state-pda-shown-accepting-trace
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
                     [stack (if (or (zipper-empty? (imsg-state-pda-stack (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                    (zipper-at-end? (imsg-state-pda-stack (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))))
                                (imsg-state-pda-stack (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))
                                (zipper-next (imsg-state-pda-stack (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))]
                     [invs-zipper (cond [(zipper-empty? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                         (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                                              (>= pci-len (second (first (zipper-unprocessed
                                                                          (imsg-state-pda-invs-zipper
                                                                           (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))))))
                                         (zipper-next (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  ;;(listof symbols) machine -> (listof symbols)
  ;;Purpose: Returns the last fully consumed word for the given machine
  #;(define (last-fully-consumed a-word M max-cmps)
    (letrec ([self (lambda (a-word)
                     (cond [(empty? a-word) '()]
                           [(not (ormap (λ (config) (empty? (config-word (first config))))
                                        (map computation-LoC (first (get-computations a-word
                                                                                      (pda-getrules M)
                                                                                      (pda-getstart M)
                                                                                      (pda-getfinals M)
                                                                                      max-cmps)))))
                            (self (take a-word (sub1 (length a-word))))]
                           [a-word]))])
      (self a-word)))
  
  ;;(listof X) (listof X) (listof X) -> (listof X)
  ;;Purpose: Removes all similiarities between lst1 and lst2
  ;;Acc = The differences between the previous path and the current path
  (define (remove-similarities prev-path curr-path acc)
    (cond [(empty? prev-path) (append acc curr-path)]
          [(empty? curr-path) prev-path]
          [(equal? (first prev-path) (first curr-path))
           (remove-similarities (rest prev-path) (rest curr-path) acc)]
          [(remove-similarities (rest prev-path) (rest curr-path) (append acc (list (first curr-path))))]))
  
  (let* (;;(listof symbol)
         ;;Purpose: The entire given word
         [full-word (append (imsg-state-pda-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                            (imsg-state-pda-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
         ;;(listof symbol)
         ;;Purpose: The last word that could be fully consumed by the pda
         #;[last-consumed-word (last-fully-consumed
                              full-word
                              (imsg-state-pda-M (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                              (imsg-state-pda-max-cmps (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))]
         ;;(listof symbol)
         ;;Purpose: The portion of the word that cannont be consumed
         #;[unconsumed-word (remove-similarities last-consumed-word full-word '())]
         ;;(zipperof invariant)
         ;;Purpose: The index of the last failed invariant
         [zip (if (zipper-empty? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                  (imsg-state-pda-invs-zipper (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                  (zipper-to-idx (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))
                                 (imsg-state-pda-inv-amount (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))])
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
         [upci (cond [(empty? (imsg-state-pda-upci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))
                      (imsg-state-pda-upci (informative-messages-component-state
                                            (viz-state-informative-messages a-vs)))]
                     [(not (empty? (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))
                      (drop full-word (- (length full-word) (length (imsg-state-pda-farthest-consumed-input
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))))))]
                     [else '()])]
         [pci 
          (cond [(empty? (imsg-state-pda-upci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))
                 (imsg-state-pda-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs)))]
                [(not (empty? (imsg-state-pda-farthest-consumed-input (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                 (take full-word (- (length full-word) (length (imsg-state-pda-farthest-consumed-input
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))))]
                [else full-word])]
         [shown-accepting-trace (if (or (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                                        (zipper-at-end? (imsg-state-pda-shown-accepting-trace
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))))
                                    (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))
                                    (zipper-to-end (imsg-state-pda-shown-accepting-trace
                                                    (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))))]
         [stack (cond [(zipper-empty? (imsg-state-pda-stack
                                       (informative-messages-component-state
                                        (viz-state-informative-messages a-vs))))
                       (imsg-state-pda-stack (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))]
                      [(or (zipper-empty? (imsg-state-pda-stack (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                           (zipper-at-end? (imsg-state-pda-stack (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))
                       (imsg-state-pda-stack (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))]
                      [else (zipper-to-end (imsg-state-pda-stack (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs))))])]
         
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([shown-accepting-trace (if (or (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                                        (zipper-at-begin? (imsg-state-pda-shown-accepting-trace
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))
                                    (imsg-state-pda-shown-accepting-trace
                                     (informative-messages-component-state
                                      (viz-state-informative-messages a-vs)))
                                    (zipper-prev (imsg-state-pda-shown-accepting-trace
                                                  (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))))]
         [next-rule (if (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                        (informative-messages-component-state
                                         (viz-state-informative-messages a-vs))))
                        (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))
                        (first (trace-rules (zipper-current (imsg-state-pda-shown-accepting-trace
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))))]
         [rule (if (zipper-empty? (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                   DUMMY-RULE
                   next-rule #;(list (rule-triple next-rule) (rule-pair next-rule)))]
         [pci (if (or (empty? (imsg-state-pda-pci (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                      (and (eq? (triple-read (rule-triple rule)) EMP)
                           (not (empty-rule? #;(first rule) rule))))
                  (imsg-state-pda-pci (informative-messages-component-state
                                       (viz-state-informative-messages a-vs)))
                  (take (imsg-state-pda-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                        (sub1 (length (imsg-state-pda-pci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))))))]
         [pci-len (length pci)])
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
                     [upci (if (or (empty? (imsg-state-pda-pci (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                   (and (eq? (triple-read (rule-triple rule)) EMP)
                                        (not (empty-rule? #;(first rule) rule)))
                                   #;(and (eq? (second (first rule)) EMP)
                                        (not (empty-rule? rule))))
                               (imsg-state-pda-upci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pda-pci (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                     (imsg-state-pda-upci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [shown-accepting-trace shown-accepting-trace]
                     [stack (if (or (zipper-empty? (imsg-state-pda-stack (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))
                                    (zipper-at-begin? (imsg-state-pda-stack (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs)))))
                                (imsg-state-pda-stack (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))
                                (zipper-prev (imsg-state-pda-stack (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))]
                     
                     [invs-zipper (cond [(zipper-empty? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                         (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                      (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                                              (<= pci-len (second (first (zipper-processed
                                                                          (imsg-state-pda-invs-zipper
                                                                           (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))))))
                                         (zipper-prev (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
(define (up-key-pressed a-vs)
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
                   [upci (if (empty? (imsg-state-pda-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                             (imsg-state-pda-upci (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))
                             (append (imsg-state-pda-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                     (imsg-state-pda-upci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))))]
                   [pci (if (empty? (imsg-state-pda-pci (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                            (imsg-state-pda-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                            '())]
                   [shown-accepting-trace (if (or (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
                                                  (zipper-at-begin? (imsg-state-pda-shown-accepting-trace
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))
                                              (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))
                                              (zipper-to-begin (imsg-state-pda-shown-accepting-trace
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))))]
                   [stack (if (or (zipper-empty? (imsg-state-pda-stack (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                  (zipper-at-begin? (imsg-state-pda-stack (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))))
                              (imsg-state-pda-stack (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                              (zipper-to-begin (imsg-state-pda-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))]
                   [invs-zipper (if (or (zipper-empty? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs))))
                                        (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs)))))
                                    (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))) 0))])])]))

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
  (if (or (zipper-empty? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))
          (and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
               (not (zipper-at-end? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))))
          (< (pda-accessor-func (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
             (get-index-pda (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs)))))
                           (<= (get-index (imsg-state-pda-stack (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                               (get-index-pda (imsg-state-pda-invs-zipper
                                               (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))))
                      (zipper-prev (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                      (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pda-pci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                                (imsg-state-pda-upci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))]
             [partial-word (if (equal? (config-word (first (zipper-current zip))) full-word)
                               '()
                               (drop full-word (get-index-pda zip)))])
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
                         [upci (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs))))
                                           (zipper-at-end? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs))))
                                           (< (get-index (imsg-state-pda-stack
                                                          (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
                                              (get-index-pda (imsg-state-pda-invs-zipper
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))))
                                      (imsg-state-pda-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
                                     [(and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs))))
                                           (not (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                 (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs))))))
                                      (imsg-state-pda-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
                                     [else partial-word])]
                         [shown-accepting-trace (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                               (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                                            (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                             (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                            (< (get-index (imsg-state-pda-stack
                                                                           (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))
                                                               (get-index-pda (imsg-state-pda-invs-zipper
                                                                               (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs)))))
                                                            (not (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                                                                 (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))))
                                                       (imsg-state-pda-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))]
                                                      [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                               (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                                            (not (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                                  (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))))
                                                       (imsg-state-pda-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))]
                                                      [else (zipper-to-idx (imsg-state-pda-shown-accepting-trace
                                                                            (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs)))
                                                                           (get-index-pda zip))])]
                         [stack (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                            (zipper-at-end? (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                                            (< (get-index (imsg-state-pda-stack
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                               (get-index-pda (imsg-state-pda-invs-zipper
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))))
                                       (imsg-state-pda-stack (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))]
                                      [(and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs))))
                                            (not (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))))
                                       
                                       (imsg-state-pda-stack (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))]
                                      [else (zipper-to-idx (imsg-state-pda-stack
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                                           (get-index-pda zip))])]
                         [pci (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-pda-invs-zipper
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                          (< (get-index (imsg-state-pda-stack
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                                             (get-index-pda (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))))
                                     (imsg-state-pda-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
                                    [(and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                         (viz-state-informative-messages a-vs))))
                                          (not (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))))
                                     (imsg-state-pda-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
                                    [else (config-word (first (zipper-current zip)))])]
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))
          (and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
               (not (zipper-at-end? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))))
          (> (pda-accessor-func (imsg-state-pda-shown-accepting-trace (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
             (get-index-pda  (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-end? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs)))))
                           (>= (get-index (imsg-state-pda-stack (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                               (get-index-pda (imsg-state-pda-invs-zipper
                                               (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))))
                      (zipper-next (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                      (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pda-pci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))
                                (imsg-state-pda-upci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))]
             [partial-word (if (equal? (config-word (first (zipper-current zip))) full-word)
                               '()
                               (drop full-word (get-index-pda zip)))])
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
                         [upci (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs))))
                                           (zipper-at-end? (imsg-state-pda-invs-zipper (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs))))
                                           (> (get-index (imsg-state-pda-stack
                                                          (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
                                              (get-index-pda (imsg-state-pda-invs-zipper
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))))
                                      (imsg-state-pda-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
                                     [(and (zipper-at-end? (imsg-state-pda-invs-zipper
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                           (not (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))))
                                      (imsg-state-pda-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))]
                                     [else partial-word])]
                         [shown-accepting-trace (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                               (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                                            (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                             (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                            (> (get-index (imsg-state-pda-stack
                                                                           (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))
                                                               (get-index-pda (imsg-state-pda-invs-zipper
                                                                               (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs)))))
                                                            (not (zipper-empty? (imsg-state-pda-shown-accepting-trace
                                                                                 (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))))
                                                       (imsg-state-pda-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))]
                                                      [(and (zipper-at-end? (imsg-state-pda-invs-zipper
                                                                             (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                                            (not (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                                    (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))))
                                                       (imsg-state-pda-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))]
                                                      [else (zipper-to-idx (imsg-state-pda-shown-accepting-trace
                                                                            (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs)))
                                                                           (get-index-pda zip))])]
                         [stack (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                            (zipper-at-end? (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                                            (> (get-index (imsg-state-pda-stack
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                               (get-index-pda (imsg-state-pda-invs-zipper
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))))
                                       (imsg-state-pda-stack (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))]
                                      [(and (zipper-at-end? (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                                            (not (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))))
                                       (imsg-state-pda-stack (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))]
                                      [else (zipper-to-idx (imsg-state-pda-stack
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                                           (get-index-pda zip))])] 
                         [pci (cond [(and (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-pda-invs-zipper
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                          (> (get-index (imsg-state-pda-stack
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                                             (get-index-pda (imsg-state-pda-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))))
                                     (imsg-state-pda-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
                                    [(and (zipper-at-end? (imsg-state-pda-invs-zipper
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                          (not (zipper-at-begin? (imsg-state-pda-invs-zipper
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))))
                                     (imsg-state-pda-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
                                    [else (config-word (first (zipper-current zip)))])]
                         [invs-zipper zip])])]))))

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
                 get-rules-not-in-M))

          
          (define (remake-rules lor)
            (map (λ (pda-rule)
                   (list (rule (triple (first (first pda-rule))
                                       (second (first pda-rule))
                                       (third (first pda-rule)))
                               (pair (first (second pda-rule))
                                     (second (second pda-rule))))))
                 lor))]
    (pda (cons dead (pda-getstates M))
         (pda-getalphabet M)
         (pda-getgamma M)
         (pda-getstart M)
         (pda-getfinals M)
         (remake-rules (append (pda-getrules M) rules-to-dead dead-read-rules dead-pop-rules)))))


;;pda word [boolean] [natnum] . -> (void)
;;Purpose: Visualizes the given pda processing the given word
(define (pda-viz M a-word #:add-dead [add-dead #f] #:cut-off [cut-off 100] invs)
  
  ;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
  ;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
  ;;         tracks each transition
  (define (make-trace configs rules acc)
    (cond [(empty? rules) (reverse acc)]
          [(and (empty? acc)
                (not (eq? (triple-read (rule-triple (first rules))) EMP)))
           (let* ([rle (rule (triple EMP EMP EMP) (pair EMP EMP))]
                  [res (trace (first configs) (list rle))])
             (make-trace (rest configs) rules (cons res acc)))]
          [(and (not (empty? acc))
                (empty-rule? (first rules)))
           (let* ([rle (first rules)
                       #;(rule (triple (first (first (first rules)))
                                     (second (first (first rules)))
                                     (third (first (first rules))))
                             (pair (first (second (first rules)))
                                   (second (second (first rules)))))]
                  [res (struct-copy trace (first acc)
                                    [rules (cons rle (trace-rules (first acc)))])])
             (make-trace (rest configs) (rest rules) (cons res (rest acc))))]
          [else (let* ([rle (first rules)
                            #;(rule (triple (first (first (first rules)))
                                          (second (first (first rules)))
                                          (third (first (first rules))))
                                  (pair (first (second (first rules)))
                                        (second (second (first rules)))))]
                       [res (trace (first configs) (list rle))])
                  (make-trace (rest configs) (rest rules) (cons res acc)))]))

  ;;(listof configurations) (listof sybmols) -> (listof configurations)
  ;;Purpose: Extracts all the invariant configurations that failed
  (define (return-brk-inv-configs inv-config-results)
    (remove-duplicates (filter (λ (config) (not (second config))) inv-config-results)))

  ;;word (listof configurations) (listof configurations) -> (listof configurations)
  ;;Purpose: Counts the number of unique configurations for each stage of the word
  (define (count-computations a-word a-LoC)
    ;;word -> number
    ;;Purpose: Counts the number of unique configurations based on the given word
    (define (get-config a-word) 
      (length (remove-duplicates
               (append-map (λ (configs)
                             (filter (λ (config)
                                       (equal? a-word (config-word config)))
                                     configs))
                           a-LoC))))
    (define (count-computations-helper a-word a-LoC acc)
      (if (empty? a-word)
          (reverse (cons (get-config a-word) acc))
          (count-computations-helper (rest a-word) a-LoC (cons (get-config a-word) acc))))
    (count-computations-helper a-word a-LoC '()))

  ;;(listof symbols) (lisof configurations) -> (listof configurations)
  ;;Purpose: Makes configurations usable for invariant predicates
  (define (make-inv-configs a-word configs)
    
    ;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
    ;;Purpose: Makes configurations usable for invariant predicates
    (define (make-inv-configs-helper a-word configs word-len)
      (letrec ([a-word-len (length a-word)]
               [self (lambda (configs word-len)
                       (let* ([current-config (filter (λ (config) (= (length (config-word config)) word-len)) configs)]
                              [inv-config (map (λ (configs)
                                                 (struct-copy config configs
                                                              [word (take a-word (- a-word-len word-len))]))
                                               current-config)])
                         (if (empty? configs)
                             '()
                             (append inv-config
                                     (self (rest configs) (sub1 word-len))))))])
        (self configs word-len)))
    
    (let ([word-len (length a-word)])
      (map (λ (comp)
             (make-inv-configs-helper a-word (reverse (computation-LoC comp)) word-len))
           configs)))

  ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
  ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
  (define (get-inv-config-results inv-configs invs)
    ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
    ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
    (define (get-inv-config-results-helper inv-configs)
      (if (or (empty? invs) (empty? inv-configs))
          '()
          (let* ([get-inv-for-inv-config (filter (λ (inv)
                                                   (equal? (first inv) (config-state (first inv-configs))))
                                                 invs)]
                 [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                         '()
                                         (second (first get-inv-for-inv-config)))]
                 [inv-config-result (if (empty? inv-for-inv-config)
                                        '()
                                        (cons (first inv-configs)
                                              (list (inv-for-inv-config (config-word (first inv-configs))
                                                                        (config-stack (first inv-configs))))))])
            (cons inv-config-result
                  (get-inv-config-results-helper (rest inv-configs))))))
  
    (append-map (λ (comp)
                  (get-inv-config-results-helper comp))
                inv-configs))

  
  ;; (listof configuration) (listof symbol) -> (listof symbol)
  ;; Purpose: Returns the most consumed input
  (define (get-farthest-consumed LoC acc)
    (cond [(empty? LoC) acc]
          [(< (length (config-word (first (first LoC)))) (length acc))
           (get-farthest-consumed (rest LoC) (config-word (first (first LoC))))]
          [else (get-farthest-consumed (rest LoC) acc)]))

  ;;(listof configurations) -> (listof configurations)
  ;;Purpose: filters the given list of any empty transitions
  (define (remove-empty a-LoC)
    (define (remove-empty-helper a-LoC acc)
      (cond [(< (length a-LoC) 2) (reverse (append a-LoC acc))]
            [(and (equal? (config-word (first a-LoC)) (config-word (second a-LoC)))
                  (equal? (config-stack (first a-LoC)) (config-stack (second a-LoC))))
             (remove-empty-helper (rest a-LoC) acc)]
            [else (remove-empty-helper (rest a-LoC) (cons (first a-LoC) acc))]))
    (remove-empty-helper a-LoC '()))

  (define (remake-rules lor)
            (map (λ (pda-rule)
                   (list (rule (triple (first (first pda-rule))
                                       (second (first pda-rule))
                                       (third (first pda-rule)))
                               (pair (first (second pda-rule))
                                     (second (second pda-rule))))))
                 lor))
  
  (let* (;;M ;;Purpose: A new machine with the dead state if add-dead is true
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

         [computations (first computations+hash)]
         
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;number ;;Purpose: The length of the word
         [word-len (length a-word)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (filter (λ (comp)
                                           (and (member? (config-state (first (computation-LoC comp))) (pda-finals new-M) eq?)
                                                (empty? (config-word (first (computation-LoC comp))))
                                                (empty? (config-stack (first (computation-LoC comp))))))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map (λ (acc-comp)
                                  (make-trace (reverse (computation-LoC acc-comp))
                                              (reverse (computation-LoR acc-comp))
                                              '()))
                                accepting-computations)]
         ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
         [cut-accept-traces (if (> word-len cut-off)
                                (map last accepting-traces)
                                '())]
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
         [rejecting-traces (map (λ (c)
                                  (make-trace (reverse (computation-LoC c))
                                              (reverse (computation-LoR c))
                                              '()))
                                rejecting-computations)]
         ;;(zipperof computation) ;;Purpose: Gets the stack of the first accepting computation
         [stack (list->zipper (remove-empty (if (empty? accepting-computations)
                                                '()
                                                (reverse (computation-LoC (first accepting-computations))))))]

         [computation-lens (map (lambda (key) (hash-set! (second computations+hash)
                                                         key
                                                         (length (remove-duplicates
                                                                  (hash-ref (second computations+hash) key)))))
                                (hash-keys (second computations+hash)))]
         
         ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
         [accepting-trace (if (empty? accept-cmps) '() (first accept-cmps))]
         ;;(listof symbol) ;;Purpose: The portion of the ci that the machine can conusme the most 
         [most-consumed-word (get-farthest-consumed LoC a-word)]
         ;;building-state struct
         [building-state (building-viz-state a-word
                                             '()
                                             LoC
                                             accepting-computations
                                             stack
                                             (list accepting-trace)
                                             (if (empty? accept-cmps) '() (rest accept-cmps))
                                             rejecting-traces
                                             new-M
                                             (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w s) #t)) invs) invs)
                                             dead-state
                                             cut-off
                                             most-consumed-word)]
                     
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state)]
         ;;(listof computation)
         ;;Purpose: Gets all the cut off computations if the length of the word is greater than max computations
         [get-cut-off-comp (if (> word-len cut-off)
                               (map first LoC)
                               '())]
         ;;(listof computation)
         ;;Purpose: Makes the cut off computations if the length of the word is greater than max computations
         [cut-off-comp (if (empty? get-cut-off-comp)
                           LoC
                           (map (λ (cut-off-comp comp)
                                  (append comp (list cut-off-comp)))
                                get-cut-off-comp
                                LoC))]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         ;[computation-lengths (hash-map (second computations+hash) (λ (k v) v))]

         ;[computation-lens2 (hash-map (second computations+hash) (λ (k v ) k #;(length k #;(remove-duplicates k))))]
         ;[old-computation-lens (count-computations a-word LoC)]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         [inv-configs (remove-duplicates (return-brk-inv-configs
                                          (get-inv-config-results
                                           (make-inv-configs a-word accepting-computations)
                                           invs)))])
    ;(map displayln LoC)
    ;(displayln (length accepting-trace))
    ;(displayln computation-lengths)
    ;(displayln old-computation-lens)
    ;(displayln computation-lens2)
    ;;(displayln (length LoC))
    ;(displayln accepting-trace)
    ;(void)
    (run-viz graphs
               (lambda () (graph->bitmap (first graphs)))
               (posn (/ E-SCENE-WIDTH 2) (/ PDA-E-SCENE-HEIGHT 2))
               DEFAULT-ZOOM
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM-FLOOR
               (informative-messages pda-create-draw-informative-message
                                     (imsg-state-pda new-M  
                                                     a-word 
                                                     '() 
                                                     (list->zipper accepting-trace)
                                                     stack 
                                                     most-consumed-word 
                                                     (list->zipper inv-configs) 
                                                     (sub1 (length inv-configs)) 
                                                     (second computations+hash) #;computation-lens 
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
               'pda-viz)))

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
#;(time (pda-viz pd-numb>numa '(a b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b b) '()))
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
                                   `(((S ε ε)(H ε))     ((S a ε)(S (b b)))
                                                        ((H b (b b))(H ε)) ((H b (b))(H ε)))))

(time (pda-viz P3 '(a a b a b b b) '()))

;[(<= max-cmps 0) (error (format "The maximum amount of computations, ~a, must be integer greater than 0" max-cmps))] DONT FORGET
