#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/vector-zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         (except-in "../viz-lib/viz-constants.rkt" INS-TOOLS-BUFFER)
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "david-imsg-state.rkt"
         racket/treelist
         (except-in "david-viz-constants.rkt" FONT-SIZE)
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/misc.rkt"
         "default-informative-messages.rkt")

(provide ndfa-viz)

(define FONT-SIZE 18)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define NODE-SIZE 50)

(define DEFAULT-ZOOM-FLOOR .6)


;(define INS-TOOLS-BUFFER 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
A rule is a structure:
(make-rule triple)
triple is the entire of the ndfa rule
|#
(struct rule (triple) #:transparent)


(struct triple (source read destination) #:transparent)

(struct ndfa (states alphabet start finals rules) #:transparent)


;(define-struct ci (upci pci) #:transparent)

;;word -> (zipperof ci)
;;Purpose: Creates all valid combinations of the upci and pci
(define (remake-ci a-word)
  ;;natnum ;;Purpose: the length of the given word
  (define word-length (length a-word))

  ;;natnum (listof ci) -> (zipperof ci)
  ;;Purpose: Creates all valid combinations of the upci and pci 
  (define (make-ci-helper pci-amt acc)
    (if (= pci-amt word-length)
        (list->zipper (cons (ci (take-right a-word pci-amt) (drop-right a-word pci-amt)) acc))
        (make-ci-helper (add1 pci-amt) (cons (ci (take-right a-word pci-amt) (drop-right a-word pci-amt)) acc))))
  
  (make-ci-helper 0 '()))



;;(listof symbol) (listof rule) symbol (setof symbol) -> (listof (treelistof computation) hashtable)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (trace-computations word lor start finals)

  ;;config rule -> config
  ;;Purpose: Applies the given rule to the given config
  ;;ASSUMPTION: the given rule is applicable to the given config
  (define (apply-rule a-computation rule)

    (define (make-new-config a-config)
      (struct-copy ndfa-config a-config
                   [state (triple-destination rule)]
                   [word (if (equal? (triple-read rule) EMP)
                             (ndfa-config-word a-config)
                             (rest (ndfa-config-word a-config)))]
                   [index (if (equal? (triple-read rule) EMP)
                              (ndfa-config-index a-config)
                              (add1 (ndfa-config-index a-config)))]))
    
    (struct-copy computation a-computation
                 [LoC (treelist-add (computation-LoC a-computation)
                                    (make-new-config (treelist-last (computation-LoC a-computation))))]
                 [LoR (treelist-add (computation-LoR a-computation) rule)]
                 [visited (treelist-add (computation-visited a-computation) (treelist-last (computation-LoC a-computation)))]))
  
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

  
  ;;(queueof computation) (treelistof computation) -> (listof (treelistof computation) hashtable)
  ;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
  (define (make-computations QoC path)
    (if (qempty? QoC)
        (list path computation-number-hash)
        (let* ([current-config (treelist-last (computation-LoC (qfirst QoC)))]
               [current-state (ndfa-config-state current-config)]
               [current-word (ndfa-config-word current-config)])
          (if (and (set-member? finals current-state) (empty? current-word))
              (begin
                (update-hash current-config current-word)
                (make-computations (dequeue QoC) (treelist-add path (qfirst QoC))))
              (let* (;;(treelistof rules)
                     ;;Purpose: Filters the rules that match the current state 
                     [curr-rules (treelist-filter (λ (rule) (eq? (triple-source rule) current-state)) lor)]
                     ;;(treelistof rules)
                     ;;Purpose: Returns all rules that consume a letter using the given configurations
                     [connected-read-rules (treelist-filter (λ (rule)
                                                              (and (not (empty? current-word))
                                                                   (eq? (triple-read rule) (first current-word))))
                                                            curr-rules)]
                     
                     ;;(treelistof rules)
                     ;;Purpose: Returns all rules that have an empty transition using the given configurations
                     [connected-emp-rules (treelist-filter (λ (rule)
                                                             (eq? (triple-read rule) EMP))
                                                           curr-rules)]
                     ;;(trreelistof configurations)
                     ;;Purpose: Makes new configurations using given word and connected-rules
                     [new-configs (treelist-filter (λ (new-c)
                                                     (not (set-member? visited-configuration-set
                                                                       (treelist-last (computation-LoC new-c)))))
                                                   
                                                   (treelist-map (treelist-append connected-emp-rules connected-read-rules)
                                                                 (λ (rule) (apply-rule (qfirst QoC) rule))))])
                (begin
                  (update-hash current-config current-word)
                  (update-visited current-config)
                  (if (treelist-empty? new-configs)
                      (make-computations (dequeue QoC) (treelist-add path (qfirst QoC)))
                      (make-computations (enqueue new-configs (dequeue QoC)) path))))))))

  (let (;;configuration
        ;;Purpose: The starting configuration
        [starting-config (computation (treelist (ndfa-config start word 0))
                                      empty-treelist
                                      empty-treelist)])
    (make-computations (enqueue (treelist starting-config) E-QUEUE) empty-treelist)))

  
;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (triple-read (first rules)) EMP)))
         (let* ([rle (rule (triple EMP EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-trace (rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (equal? (triple-read (first rules)) EMP))
         (let* ([rle (rule (first rules))]
                [res (struct-copy trace (first acc)
                                  [rules (cons rle (trace-rules (first acc)))])])
           (make-trace configs (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first rules))]
                     [res (trace (first configs)
                                 (list rle))])
                (make-trace (rest configs) (rest rules) (cons res acc)))]))

;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                     (trace-computations a-word (fsa-getrules M) (fsa-getstart M) (fsa-getfinals M))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
        [a-word]))

;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word LoC)
  (map (λ (computation) (make-inv-configs-helper a-word computation (length a-word))) LoC))

;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs-helper a-word computation word-len)
  (let* ([config (filter (λ (config) (= (length (ndfa-config-word config)) word-len)) computation)]
         [inv-config (map (λ (config)
                          (ndfa-config (ndfa-config-state config)
                                    (take a-word (- (length a-word) word-len))
                                    (ndfa-config-index config)))
                          config)])
    (if (empty? computation)
        '()
        (append inv-config
                (make-inv-configs-helper a-word (rest computation) (sub1 word-len))))))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
(define (get-inv-config-results inv-configs invs)
  (append-map (λ (comp)
                (get-inv-config-results-helper comp invs))
              inv-configs))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
(define (get-inv-config-results-helper inv-configs invs)
  (if (empty? inv-configs)
      '()
      (let* ([get-inv-for-inv-config (filter (λ (inv)
                                               (eq? (first inv) (ndfa-config-state (first inv-configs))))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (first inv-configs) (inv-for-inv-config (ndfa-config-word (first inv-configs)))))])
        (if (empty? inv-config-result)
            (get-inv-config-results-helper (rest inv-configs) invs)
            (cons inv-config-result
                  (get-inv-config-results-helper (rest inv-configs) invs))))))

;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results)
  (filter (λ (config) (not (second config))) inv-config-results))

;;rule symbol (listof rules) -> boolean
;;Purpose: Determines if the given rule is a member of the given (listof rules)
;;         or similiar to one of the rules in the given (listof rules) 
(define (find-rule? rule dead lor)
  (or (member? rule lor equal?)
      (for/or ([r lor])
        (and (eq? (triple-source rule) (triple-source r))
             (or (eq? (triple-destination rule) (triple-destination r))
                 (and (eq? (triple-destination rule) (triple-destination r))
                      (eq? (triple-destination rule) dead)))))))

;;(listof symbols) (listof configurations) -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                          (equal? (ndfa-config-word configs) word))
                        config))
              full-configs))


;;(listof trace-rule) -> (listof rules)
;;Purpose: Remakes the rule extracted from the rule-struct
(define (extract-rules trace-rules)
  (map (λ (rule)
         (rule-triple rule))
       trace-rules))

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

;;(listof trace) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-rule LoT)
  (filter-map-acc empty? trace-rules not first LoT))

;;(listof symbol ((listof symbols) -> boolean))) (X -> Y) -> (listof symbol ((listof symbols) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) -> boolean)))
(define (get-invariants LoI func)
  (filter-map-acc (λ (x) ((second (first x)) (second x))) first func first LoI))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not id LoT))

;; (listof configuration) (listof symbol) -> (listof symbol)
;; Purpose: Returns the most consumed input
(define (get-farthest-consumed LoC acc)
  (cond [(empty? LoC) acc]
        [(< (length (ndfa-config-word (treelist-last (first LoC)))) (length (ndfa-config-word acc)))
         (get-farthest-consumed (rest LoC) (treelist-last (first LoC)))]
        [else (get-farthest-consumed (rest LoC) acc)]))


(define (remake-rules lor)
  (for/treelist ([ndfa-rule lor])
    (triple (first ndfa-rule)
            (second ndfa-rule)
            (third ndfa-rule))))

(define (remake-machine M)
  (ndfa (fsa-getstates M)
        (fsa-getalphabet M)
        (fsa-getstart M)
        (list->seteq (fsa-getfinals M))
        (remake-rules (fsa-getrules M))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graph machine (listof symbols) symbol (listof symbols) (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M dead held-inv fail-inv)
  (foldl (λ (state result)
           (let ([member-of-held-inv? (member? state held-inv eq?)]
                 [member-of-fail-inv? (member? state fail-inv eq?)])
             (add-node result
                     state
                     #:atb (hash 'color (if (eq? state (ndfa-start M)) 'green 'black)
                                 'style (cond [(or member-of-held-inv? member-of-fail-inv?) 'filled]
                                              [(eq? state dead) 'dashed]
                                              [else 'solid])
                                 'shape (if (set-member? (ndfa-finals M) state) 'doublecircle 'circle)
                                 'fillcolor (cond [member-of-held-inv? HELD-INV-COLOR]
                                                  [member-of-fail-inv? BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 'fontcolor 'black))))
         cgraph
         (ndfa-states M)))

;; graph machine word (listof rules) (listof rules) symbol -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M current-shown-accept-rules other-current-accept-rules current-reject-rules dead)
  (foldl (λ (rule result)
           (let ([other-current-accept-rule-find-rule? (find-rule? rule dead other-current-accept-rules)])
           (add-edge result
                     (triple-read rule)
                     (triple-source rule)
                     (triple-destination rule)
                     #:atb (hash 'color (cond [(and (member? rule current-shown-accept-rules eq?)
                                                    (member? rule other-current-accept-rules eq?)) SPLIT-ACCEPT-COLOR]
                                              [(find-rule? rule dead current-shown-accept-rules) TRACKED-ACCEPT-COLOR]
                                              [other-current-accept-rule-find-rule? ALL-ACCEPT-COLOR]
                                              [(find-rule? rule dead current-reject-rules) REJECT-COLOR]
                                              [else 'black])
                                 'fontsize FONT-SIZE
                                 'style (cond [(equal? (triple-destination rule) dead) 'dashed]
                                              [other-current-accept-rule-find-rule? 'bold]
                                              [else 'solid])))))
         cgraph
         (treelist->list (ndfa-rules M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;upci is the unprocessed consumed input (listof symbols)
;;pci is the proccessed consumed input (listof symbols)
;;M is a machine
;;inv is a the (listof (state (listof symbols -> boolean)))
;;dead is the sybmol of dead state
(struct building-viz-state (CI upci pci M inv dead tracked-accept-trace
                                 accepting-computations accept-traces reject-traces farthest-consumed)
  #:transparent)


;;viz-state -> (list graph-thunk computation-length)
;;Purpose: Creates a graph thunk and finds the associated computation's length for a given viz-state
(define (create-graph-thunk a-vs)
  (let* (;;(listof configurations)
         ;;Purpose: Returns all configurations using the given word
         [all-configs (get-portion-configs (building-viz-state-upci a-vs)
                                           (map (λ (comp) (treelist->list (computation-LoC comp)))
                                                (building-viz-state-accepting-computations a-vs)))]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from of shown accepting computation
         [tracked-accepting-rules (get-trace-rule (building-viz-state-tracked-accept-trace a-vs))]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of all configurations
         [r-config (get-trace-rule (building-viz-state-reject-traces a-vs))]

         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [current-rules (append-map extract-rules r-config)]

         ;;(listof rule-structs)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [a-configs (get-trace-rule (building-viz-state-accept-traces a-vs))]

         ;;A dummy dfa/ndfa rule
         [dummy-rule (triple EMP EMP EMP)]

         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-shown-accept-rules (filter (λ (rule) (not (equal? rule dummy-rule)))
                                             (append-map extract-rules tracked-accepting-rules))]

         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [current-a-rules (filter (λ (rule) (not (equal? rule dummy-rule)))
                                  (append-map extract-rules a-configs))]
     
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr all-configs]
                               #:when (equal? (first invs) (ndfa-config-state curr)))
                     (list invs (building-viz-state-pci a-vs)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (get-invariants get-invs id)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (get-invariants get-invs not)])
    (edge-graph
     (node-graph
      (create-graph 'ndfagraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      (building-viz-state-dead a-vs)
      held-invs
      brkn-invs)
     (building-viz-state-M a-vs)
     current-shown-accept-rules
     current-a-rules
     current-rules
     (building-viz-state-dead a-vs))))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [#;(empty? (building-viz-state-upci a-vs)) (zipper-empty? (building-viz-state-CI a-vs)) (reverse (cons (create-graph-thunk a-vs) acc))]
        [(equal? (ci-upci (zipper-current (building-viz-state-CI a-vs))) #;(building-viz-state-upci a-vs)
                 (ndfa-config-word (building-viz-state-farthest-consumed a-vs)))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  [CI (zipper-next (building-viz-state-CI a-vs))]
                                                  [upci (rest (building-viz-state-upci a-vs))]
                                                  [pci (append (building-viz-state-pci a-vs)
                                                               (list (first (building-viz-state-upci a-vs))))]
                                                  [tracked-accept-trace
                                                   (get-next-traces (building-viz-state-tracked-accept-trace a-vs))]
                                                  [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                                  [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* ([shown-accepting-trace (if (or (zipper-at-end? (imsg-state-ndfa-shown-accepting-trace
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                                        (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))))
                                    (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))
                                    (zipper-next (imsg-state-ndfa-shown-accepting-trace
                                                  (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))))]
         [index (if (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                    (informative-messages-component-state
                                     (viz-state-informative-messages a-vs))))
                    shown-accepting-trace
                    (ndfa-accessor-func shown-accepting-trace))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-ndfa
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [ci (if (or (zipper-at-end? (imsg-state-ndfa-ci
                                                  (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                                 (equal? (ci-upci (zipper-current (imsg-state-ndfa-ci (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs)))))
                                           (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))))
                             (imsg-state-ndfa-ci (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs)))
                             (zipper-next (imsg-state-ndfa-ci (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs)))))]
                     #;[upci (if (or (empty? (imsg-state-ndfa-upci (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs))))
                                   (equal? (imsg-state-ndfa-upci (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))
                                           (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))))
                               (imsg-state-ndfa-upci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-ndfa-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))]
                     [shown-accepting-trace shown-accepting-trace]
                     #;[pci (if (or (empty? (imsg-state-ndfa-upci (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                  (equal? (imsg-state-ndfa-upci (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))
                                           (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))))
                              (imsg-state-ndfa-pci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs)))
                              (append (imsg-state-ndfa-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))
                                      (list (first (imsg-state-ndfa-upci (informative-messages-component-state
                                                                          (viz-state-informative-messages
                                                                           a-vs)))))))]
                     [invs-zipper (cond [(zipper-empty? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs))))
                                         (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                                              (>= (ndfa-accessor-func
                                                   (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))
                                                  (ndfa-config-index
                                                         (first (first
                                                                 (zipper-unprocessed
                                                                       (imsg-state-ndfa-invs-zipper
                                                                        (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))))))
                                         (zipper-next (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [full-word (append (imsg-state-ndfa-pci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))
                            (imsg-state-ndfa-upci (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))]
         [zip (if (zipper-empty? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
                  (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                (viz-state-informative-messages a-vs)))
                  (zipper-to-idx (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))
                                 (imsg-state-ndfa-inv-amount (informative-messages-component-state
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
         imsg-state-ndfa
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         [ci (cond [(zipper-at-end? (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs))))
                   (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
                   [(not (empty? (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))))
                    (zipper-to-idx (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))
                                   (ndfa-config-index (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))))]
                   [else (zipper-to-end (imsg-state-ndfa-ci (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs))))])]
         #;[upci (cond [(empty? (imsg-state-ndfa-upci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                      (imsg-state-ndfa-upci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))]
                     [(not (empty? (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs)))))
                      (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))]
                     [else '()])]
         #;[pci (cond [(empty? (imsg-state-ndfa-upci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))
                     (imsg-state-ndfa-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))]
                    [(not (empty? (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))
                     (take full-word (- (length full-word)
                                        (ndfa-config-index (imsg-state-ndfa-farthest-consumed-input
                                                 (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs))))))]
                    [else full-word])]
         [shown-accepting-trace (if (or (zipper-at-end? (imsg-state-ndfa-shown-accepting-trace
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                                        (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))))
                                    (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))
                                    (zipper-to-end (imsg-state-ndfa-shown-accepting-trace
                                                    (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs)))))]
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-ndfa
                     (informative-messages-component-state
                      (viz-state-informative-messages a-vs))
                     [ci (if (zipper-at-begin? (imsg-state-ndfa-ci
                                                  (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
                             (imsg-state-ndfa-ci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))
                             (zipper-prev (imsg-state-ndfa-ci (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
                     
                     [upci (if (empty? (imsg-state-ndfa-pci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                               (imsg-state-ndfa-upci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-ndfa-pci (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                     (imsg-state-ndfa-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))]
                     [shown-accepting-trace (if (or (zipper-at-begin? (imsg-state-ndfa-shown-accepting-trace
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                    (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                                                (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs)))
                                                (zipper-prev (imsg-state-ndfa-shown-accepting-trace
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
                     
                     [pci (if (empty? (imsg-state-ndfa-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                              (imsg-state-ndfa-pci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs)))
                              (take (imsg-state-ndfa-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                    (sub1 (length (imsg-state-ndfa-pci (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))))]
                     [invs-zipper (cond [(zipper-empty? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs))))
                                         (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                                      (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                                              (<= (ndfa-accessor-func
                                                   (imsg-state-ndfa-shown-accepting-trace
                                                    (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                                               #;index (ndfa-config-index
                                                         (first (first (zipper-processed
                                                                       (imsg-state-ndfa-invs-zipper
                                                                        (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))))))
                                         (zipper-prev (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))])])])]))

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
      (struct-copy imsg-state-ndfa
                   (informative-messages-component-state
                    (viz-state-informative-messages a-vs))
                   [ci (if (zipper-at-begin? (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs))))
                            (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))
                            (zipper-to-begin (imsg-state-ndfa-ci (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))]
                   #;[upci (if (empty? (imsg-state-ndfa-pci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
                             (imsg-state-ndfa-upci (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs)))
                             (append (imsg-state-ndfa-pci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))
                                     (imsg-state-ndfa-upci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))]
                   #;[pci (if (empty? (imsg-state-ndfa-pci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                            (imsg-state-ndfa-pci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))
                            '())]
                   [invs-zipper (if (zipper-empty? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))))
                                    (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))) 0))]
                   [shown-accepting-trace (if (or (zipper-at-begin? (imsg-state-ndfa-shown-accepting-trace
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))))
                                                  (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))))
                                              (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))
                                              (zipper-to-begin (imsg-state-ndfa-shown-accepting-trace
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))))])])]))

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
                    (struct-copy imsg-state-ndfa a-imsgs [word-img-offset 0] [scroll-accum 0])])])))

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
                                (struct-copy imsg-state-ndfa
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset
                                              (imsg-state-ndfa-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs))))
          (and (zipper-at-begin? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
               (not (zipper-at-end? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs))))))
          #;(< (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
             (get-index-ndfa (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                   (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs)))))
                           (<= (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                    (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                               (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))))
                      (zipper-prev (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                      (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))]
             #;[full-word (append (ci-pci (imsg-state-ndfa-ci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                (ci-upci (imsg-state-ndfa-ci (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-index-ndfa zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state-ndfa
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [ci (zipper-to-idx (imsg-state-ndfa-ci (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))
                                            (get-index-ndfa zip))
                             #;(cond [(zipper-at-end? (imsg-state-ndfa-ci (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                    (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
                                   [(not (empty? (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))))
                                    (zipper-to-idx (imsg-state-ndfa-ci (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))
                                                   (ndfa-config-index (imsg-state-ndfa-farthest-consumed-input
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))]
                                   [else (zipper-to-end (imsg-state-ndfa-ci (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs))))])]
                         #;[upci (cond [(and (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
                                           (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                           (< (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                               (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))))
                                      (drop full-word (get-index-ndfa zip))]
                                     [(zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                                      (imsg-state-ndfa-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))]
                                     [else (drop full-word (get-index-ndfa zip))])]
                         #;[pci (cond [(and (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                           (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                          (< (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                                              (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))))
                                     (take full-word (get-index-ndfa zip))]
                                    [(zipper-at-begin? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                     (imsg-state-ndfa-pci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))]
                                    [else (take full-word (get-index-ndfa zip))])]
                         [shown-accepting-trace (if (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs)))
                                                    (zipper-to-idx (imsg-state-ndfa-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))
                                                                   (get-index-ndfa zip)))]
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)  
  (if (or (zipper-empty? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs))))
          (and (not (zipper-at-begin? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))
               (zipper-at-end? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
               (> (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                  (get-index-ndfa (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-end? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))))
                           (>= (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                    (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                               (get-index-ndfa (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs))))))
                      (zipper-next (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                      (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))]
             #;[full-word (append (imsg-state-ndfa-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                                (imsg-state-ndfa-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs))))])
        (struct-copy
         viz-state
         a-vs
         [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-index-ndfa zip))]
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state-ndfa
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [ci (zipper-to-idx (imsg-state-ndfa-ci (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))
                                            (get-index-ndfa zip))
                             #;(cond [(zipper-at-end? (imsg-state-ndfa-ci (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                    (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
                                   [(not (empty? (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))))
                                    (zipper-to-idx (imsg-state-ndfa-ci (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))
                                                   (ndfa-config-index (imsg-state-ndfa-farthest-consumed-input
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))]
                                   [else (zipper-to-end (imsg-state-ndfa-ci (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs))))])]
                         #;[upci (cond [(or (and (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
                                               (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                               (> (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                  (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))))
                                          (and (not (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))
                                               (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                               (< (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                  (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))))
                                      (drop full-word (get-index-ndfa zip))]
                                     [(zipper-at-end? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs))))
                                      (imsg-state-ndfa-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))]
                                     [else (drop full-word (get-index-ndfa zip))])]
                         #;[pci (cond [(or (and (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
                                               (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                               (>= (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                  (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))))
                                          (and (not (zipper-at-begin? (imsg-state-ndfa-invs-zipper
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))
                                               (zipper-at-end? (imsg-state-ndfa-invs-zipper
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                               (< (ndfa-accessor-func (imsg-state-ndfa-shown-accepting-trace
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))
                                                  (get-index-ndfa (imsg-state-ndfa-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))))
                                     (take full-word (get-index-ndfa zip))]
                                    [(zipper-at-end? (imsg-state-ndfa-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))
                                     (imsg-state-ndfa-pci (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))]
                                    [else (take full-word (get-index-ndfa zip))])]
                         [shown-accepting-trace (if (zipper-empty? (imsg-state-ndfa-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (imsg-state-ndfa-shown-accepting-trace
                                                     (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                                                    (zipper-to-idx (imsg-state-ndfa-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))
                                                                   (get-index-ndfa zip)))]
                         [invs-zipper zip])])]))))

;;machine -> machine
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (cond [(eq? (M 'whatami) 'ndfa)
         (local [;;symbol
                 ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
                 ;;         otherwise uses DEAD
                 (define dead (if (member? DEAD (fsa-getstates M) eq?) (gen-state (fsa-getstates M)) DEAD))
                 ;;(listof symbols)
                 ;;Purpose: Makes partial rules for every combination of states in M and symbols in sigma of M
                 (define new-rules
                   (for*/list ([states (fsa-getstates M)]
                               [sigma (fsa-getalphabet M)])
                     (list states sigma)))
                 ;;(listof rules)
                 ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
                 (define dead-rules
                   (for*/list ([ds (list dead)]
                               [sigma (fsa-getalphabet M)])
                     (list ds sigma ds)))
                 ;;(listof rules)
                 ;;Purpose: Gets rules that are not currently in the original rules of M
                 (define get-rules-not-in-M  (local [(define partial-rules (map (λ (rule)
                                                                                  (append (list (first rule)) (list (second rule))))
                                                                                (fsa-getrules M)))]
                                               (filter (λ (rule)
                                                         (not (member? rule partial-rules equal?)))
                                                       new-rules)))
                 ;;(listof rules)
                 ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
                 (define rules-to-dead
                   (map (λ (rule) (append rule (list dead)))
                        get-rules-not-in-M))]
           (make-unchecked-ndfa (cons dead (fsa-getstates M))
                                (fsa-getalphabet M)
                                (fsa-getstart M)
                                (fsa-getfinals M)
                                (append (fsa-getrules M) rules-to-dead dead-rules)))]
        [(and (eq? (M 'whatami) 'dfa) (not (member? DEAD (fsa-getstates M) equal?)))
         (make-unchecked-dfa (fsa-getstates M) (fsa-getalphabet M) (fsa-getstart M) (fsa-getfinals M) (fsa-getrules M))]
        [else M]))



;;ndfa word [boolean] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (ndfa-viz M a-word #:add-dead [add-dead #f] invs)
  (let* (;;M ;;Purpose: A new machine with the dead state if add-dead is true
         [new-M (remake-machine (if add-dead (make-new-M M) M))]
         ;;symbol ;;Purpose: The name of the dead state
         [dead-state (cond [(and add-dead (eq? (M 'whatami) 'ndfa)) (first (ndfa-states new-M))]
                           [(and add-dead (eq? (M 'whatami) 'dfa)) DEAD]
                           [else 'no-dead])]
         ;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations+hash (trace-computations a-word (ndfa-rules new-M) (ndfa-start new-M) (ndfa-finals new-M))]

         [computations (treelist->list (first computations+hash))]
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (filter (λ (comp)
                                           (and (set-member? (ndfa-finals new-M)
                                                             (ndfa-config-state (treelist-last (computation-LoC comp))))
                                                (empty? (ndfa-config-word (treelist-last (computation-LoC comp))))))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map (λ (accept-comp)
                                  (make-trace (treelist->list (computation-LoC accept-comp))
                                              (treelist->list (computation-LoR accept-comp))
                                              '()))
                                accepting-computations)]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (filter (λ (config)
                                           (not (member? config accepting-computations equal?)))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
         [rejecting-traces (map (λ (reject-comp)
                                  (make-trace (treelist->list (computation-LoC reject-comp))
                                              (treelist->list (computation-LoR reject-comp))
                                              '()))
                                rejecting-computations)]
         ;;(listof symbol) ;;Purpose: The portion of the ci that the machine can conusme the most 
         [most-consumed-word (let ([last-word (if (empty? accepting-traces)
                                                  (get-farthest-consumed LoC (ndfa-config (ndfa-start new-M) a-word 0))
                                                  (ndfa-config (ndfa-start new-M) '() 0))])
                               (if (empty? (ndfa-config-word last-word))
                                   last-word
                                   (struct-copy ndfa-config
                                                last-word
                                                [word (rest (ndfa-config-word last-word))]
                                                [index (add1 (ndfa-config-index last-word))])))]
         [CIs (remake-ci a-word)]
         ;;building-state struct
         [building-state (building-viz-state CIs
                                             a-word
                                             '()
                                             new-M
                                             (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w) #t)) invs) invs) 
                                             dead-state
                                             (if (empty? accepting-traces) accepting-traces (list (first accepting-traces)))
                                             accepting-computations
                                             (if (empty? accepting-traces) accepting-traces (rest accepting-traces))
                                             rejecting-traces
                                             most-consumed-word)]
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [computation-lens (begin
                             (for ([key (in-list (hash-keys (second computations+hash)))])
                               (hash-set! (second computations+hash)
                                          key
                                          (set-count (hash-ref (second computations+hash) key))))
                             (second computations+hash))]
         ;[computation-lens (count-computations a-word (map computation-LoC computations) '())]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         [inv-configs (return-brk-inv-configs
                                          (get-inv-config-results
                                           (make-inv-configs a-word
                                                             (map (λ (comp)
                                                                    (treelist->list (computation-LoC comp)))
                                                                  accepting-computations))
                                           invs))
                      #;(if (empty? invs)
                          invs
                          (return-brk-inv-configs
                                          (get-inv-config-results
                                           (make-inv-configs a-word
                                                             (map (λ (comp)
                                                                    (treelist->list (computation-LoC comp)))
                                                                  accepting-computations))
                                           invs)))])
    
    (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ NDFA-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages ndfa-create-draw-informative-message
                                   (imsg-state-ndfa new-M
                                                    CIs
                                                    a-word
                                                    '()
                                                    (list->zipper (if (empty? accepting-traces) '() (first accepting-traces)))
                                                    most-consumed-word
                                                    (list->zipper inv-configs)
                                                    (sub1 (length inv-configs))
                                                    computation-lens
                                                    (for/list ([computation LoC]) (treelist->list computation)) ;LoC
                                                    0
                                                    (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                      (if (> 0 offset-cap) 0 offset-cap))
                                                    0)
                                   ndfa-img-bounding-limit)
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width E-SCENE-TOOLS)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       NDFA-E-SCENE-HEIGHT
                                                       (image-height ndfa-info-img)
                                                       INS-TOOLS-BUFFER)
                                                    (+ EXTRA-HEIGHT-FROM-CURSOR
                                                       NDFA-E-SCENE-HEIGHT
                                                       (image-height ndfa-info-img)
                                                       INS-TOOLS-BUFFER
                                                       (image-height ARROW-UP-KEY))))
             (create-viz-draw-world E-SCENE-WIDTH NDFA-E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key ["right" viz-go-next right-key-pressed]
                                     ["left" viz-go-prev left-key-pressed]
                                     ["up" viz-go-to-begin up-key-pressed]
                                     ["down" viz-go-to-end down-key-pressed]
                                     [ "w" viz-zoom-in identity]
                                     [ "s" viz-zoom-out identity]
                                     [ "r" viz-max-zoom-out identity]
                                     [ "f" viz-max-zoom-in identity]
                                     [ "e" viz-reset-zoom identity]
                                     [ "a" identity a-key-pressed]
                                     [ "d" identity d-key-pressed]
                                     [ "wheel-down" viz-zoom-in identity]
                                     [ "wheel-up" viz-zoom-out identity]
                                     [ "j" ndfa-jump-prev j-key-pressed]
                                     [ "l" ndfa-jump-next l-key-pressed]
                                     )
             (create-viz-process-tick NDFA-E-SCENE-BOUNDING-LIMITS
                                      NODE-SIZE
                                      E-SCENE-WIDTH
                                      NDFA-E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      ([ndfa-img-bounding-limit
                                        (lambda (a-imsgs x-diff y-diff) a-imsgs)])
                                      ( [ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [W-KEY-DIMS viz-zoom-in identity]
                                        [S-KEY-DIMS viz-zoom-out identity]
                                        [R-KEY-DIMS viz-max-zoom-out identity]
                                        [E-KEY-DIMS viz-reset-zoom identity]
                                        [ F-KEY-DIMS viz-max-zoom-in identity]
                                        [ A-KEY-DIMS identity a-key-pressed]
                                        [ D-KEY-DIMS identity d-key-pressed]
                                        [ J-KEY-DIMS ndfa-jump-prev j-key-pressed]
                                        [ L-KEY-DIMS ndfa-jump-next l-key-pressed]))
             (if (eq? (M 'whatami) 'ndfa)
                 'ndfa-viz
                 'dfa-viz))))


;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"