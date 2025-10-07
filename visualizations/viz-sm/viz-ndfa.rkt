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
         (except-in "david-imsg-dimensions.rkt" FONT-SIZE)
         "david-viz-constants.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/misc.rkt"
         "default-informative-messages.rkt")

(provide ndfa-viz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
A rule is a structure:
triple is a structure that contains the entire of the ndfa rule
|#
(struct rule (triple) #:transparent)

#|
A triple is a structure:
source -> the source state of the ndfa rule           | symbol
read -> the element to be read by the ndfa rule       | symbol
destination -> the destination state of the ndfa rule | symbol
|#
(struct triple (source read destination) #:transparent)

#|
An ndfa is structure:
states -> the states for the ndfa       | (listof symbol)
alphabet -> the alphabet for the ndfa   | (listof symbol)
start -> the start state for the ndfa   | symbol
finals -> the final states for the ndfa | (setof symbol)
rules -> the rules used by the ndfa     | (listof rule)
type -> the type of the ndfa (ndfa/dfa) | symbol

|#
(struct ndfa (states alphabet start finals rules type) #:transparent)

;;word -> (zipperof ci)
;;Purpose: Creates all valid combinations of the upci and pci
(define (remake-ci a-word)
  ;;natnum ;;Purpose: the length of the given word
  (define word-length (length a-word))

  ;;natnum (listof ci) -> (zipperof ci)
  ;;Purpose: Creates all valid combinations of the upci and pci
  ;;Acc = All valid combinations of unconsumed and consumed input after num-steps amount  of steps
  (define (make-ci-helper num-steps acc)
    (if (= num-steps word-length)
        (list->zipper (cons (ci (take-right a-word num-steps) (drop-right a-word num-steps)) acc))
        (make-ci-helper (add1 num-steps) (cons (ci (take-right a-word num-steps) (drop-right a-word num-steps)) acc))))
  (make-ci-helper 0 '()))


;;(listof symbol) (listof rule) symbol (setof symbol) -> (listof (treelistof computation) hashtable)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (trace-computations word lor start finals)

  ;;config rule -> config
  ;;Purpose: Applies the given rule to the given config
  ;;ASSUMPTION: the given rule is applicable to the given config
  (define (apply-rule a-computation rule)

    ;;config -> config
    ;;Purpose: Makes a new config from the given config using its applicable rule 
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
                 [visited (set-add (computation-visited a-computation) (treelist-last (computation-LoC a-computation)))]))
  
  ;;hash-set
  ;;Purpose: accumulates the number of computations in a hashset
  (define computation-number-hash (make-hash))
  
  ;;set
  ;;an empty set
  (define EMPTY-SET (set))

  ;;(setof ndfa-config) ndfa-config -> boolean
  ;;Purpose: Determines if the given ndfa-config is a member of the given set
  (define (set-member st val)
    (for/or ([elem (in-set st)])
      (and (equal? (ndfa-config-state elem) (ndfa-config-state val))
           (equal? (ndfa-config-word  elem) (ndfa-config-word  val)))))
  
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
               [current-word (ndfa-config-word current-config)]
               [member-of-finals? (set-member? finals current-state)])
          (if (and member-of-finals? (empty? current-word))
              (begin
                (update-hash current-config current-word)
                (make-computations (dequeue QoC) (struct-copy paths path
                                                              [accepting (treelist-add (paths-accepting path) (qfirst QoC))]
                                                              [reached-final? (cond [(paths-reached-final? path) (paths-reached-final? path)]
                                                                                    [member-of-finals? #t]
                                                                                    [else (paths-reached-final? path)])])))
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
                                                     (not (set-member (computation-visited (qfirst QoC)) (treelist-last (computation-LoC new-c)))))
                                                   (treelist-map (treelist-append connected-emp-rules connected-read-rules)
                                                                 (λ (rule) (apply-rule (qfirst QoC) rule))))])
                (begin
                  (update-hash current-config current-word)
                  (if (treelist-empty? new-configs)
                      (make-computations (dequeue QoC) (struct-copy paths path [rejecting (treelist-add (paths-rejecting path) (qfirst QoC))]))
                      (make-computations (enqueue new-configs (dequeue QoC)) path))))))))

  (let (;;configuration
        ;;Purpose: The starting configuration
        [starting-config (computation (treelist (ndfa-config start word 0))
                                      empty-treelist
                                      (set)
                                      1)])
    (make-computations (enqueue (treelist starting-config) E-QUEUE) (paths empty-treelist empty-treelist #f #f))))

  
;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(and (= (length configs) 1) (empty? rules))
         (let* ([rle (rule (triple EMP EMP EMP))]
                [res (trace (first configs) (list rle))])
           (reverse (cons res acc)))]
         [(or (empty? rules)
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

;;word (listof computation) (listof (list symbol (word -> boolean)) -> (list (listof computation) boolean)
;;Purpoes: Extracts all the computations where its corresponding invariant doesn't hold
(define (get-failed-invariants a-word LoC invariants)
  ;;(lisof computations) -> (listof computations)
  ;;Purpose: Makes configurations usable for invariant predicates
  (define (make-inv-configs LoC)

    ;;configuration natnum ->  computation
    ;;Purpose: Makes configurations usable for invariant predicates
    (define (make-inv-configs-helper computation word-len)
      (let* ([config (filter (λ (config) (= (length (ndfa-config-word config)) word-len)) computation)]
             [inv-config (map (λ (config)
                                (struct-copy ndfa-config config [word (drop-right a-word word-len)]))
                              config)])
        (if (empty? computation)
            '()
            (append inv-config
                    (make-inv-configs-helper (rest computation) (sub1 word-len))))))
  
    (map (λ (computation) (make-inv-configs-helper computation (length a-word))) LoC))

  ;;configuration -> configuration
  ;;Purpose: Adds the results of each invariant predicate to its corresponding invariant configuration 
  (define (get-inv-config-results inv-configs)

    ;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
    ;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
    (define (get-inv-config-results-helper inv-configs)
      (if (or (empty? invariants) (empty? inv-configs))
          '()
          (let* ([get-inv-for-inv-config (filter (λ (inv)
                                                   (eq? (first inv) (ndfa-config-state (first inv-configs))))
                                                 invariants)]
                 [extract-inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                                 '()
                                                 (second (first get-inv-for-inv-config)))]
                 [inv-config-result (if (empty? extract-inv-for-inv-config)
                                        '()
                                        (list (first inv-configs) (extract-inv-for-inv-config (ndfa-config-word (first inv-configs)))))])
            (if (empty? inv-config-result)
                (get-inv-config-results-helper (rest inv-configs))
                (cons inv-config-result
                      (get-inv-config-results-helper (rest inv-configs)))))))

    (append-map (λ (comp)
                  (get-inv-config-results-helper comp))
                inv-configs))

  ;;(listof configurations) -> (listof configurations)
  ;;Purpose: Extracts all the invariant configurations that failed
  (define (return-brk-inv-configs inv-config-results)
    (filter (λ (config) (not (second config))) inv-config-results))
  
  (return-brk-inv-configs (get-inv-config-results (make-inv-configs LoC))))

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


;;(listof trace-rule) -> (listof triple)
;;Purpose: Remakes the rule extracted from the rule-struct
(define (extract-rules trace-rules)
  (map (λ (rule)
         (rule-triple rule))
       trace-rules))

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
  (filter-map-acc empty? rest not identity LoT))

;; (listof configuration) (listof symbol) -> (listof symbol)
;; Purpose: Returns the most consumed input
(define (get-farthest-consumed LoC acc)
  (cond [(empty? LoC) acc]
        [(< (length (ndfa-config-word (treelist-last (first LoC)))) (length (ndfa-config-word acc)))
         (get-farthest-consumed (rest LoC) (treelist-last (first LoC)))]
        [else (get-farthest-consumed (rest LoC) acc)]))

;;fsa -> ndfa(structure)
;;Purpose: Converts the fsa into an ndfa
(define (remake-machine M)
  ;;(listof rule) -> (treelistof rule)
  ;;Purpose: Converts the ndfa rules into an ndfa rule (structure)
  (define (remake-rules lor)
    (for/treelist ([ndfa-rule lor])
      (triple (first ndfa-rule)
              (second ndfa-rule)
              (third ndfa-rule))))
  (ndfa (fsa-getstates M)
        (fsa-getalphabet M)
        (fsa-getstart M)
        (list->seteq (fsa-getfinals M))
        (remake-rules (fsa-getrules M))
        (M 'whatami)))

;;(listof trace) (listof trace) -> (listof trace)
  ;;Purpose: Finds the longest computation for rejecting traces
  (define (find-longest-computation a-LoRT acc)
    (cond [(empty? a-LoRT) acc]
          [(> (length (first a-LoRT)) (length acc))
           (find-longest-computation (rest a-LoRT) (first a-LoRT))]
          [(find-longest-computation (rest a-LoRT) acc)]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graph machine (listof symbols) symbol (listof symbols) (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M dead held-inv fail-inv color-scheme)
  (foldl (λ (state result)
           (let ([member-of-held-inv? (member? state held-inv eq?)]
                 [member-of-fail-inv? (member? state fail-inv eq?)]
                 [graph-attributes (graph-attributes-node-attributes default-graph-attributes)])
             (add-node result
                       state
                       #:atb (hash 'color (if (eq? state (ndfa-start M))
                                              (color-palette-start-state-color color-scheme)
                                              (color-palette-font-color color-scheme))
                                   'style (cond [(or member-of-held-inv? member-of-fail-inv?) (node-data-inv-node graph-attributes)]
                                                [(eq? state dead) (node-data-dead-node graph-attributes)]
                                                [else (node-data-regular-node graph-attributes)])
                                   'shape (if (set-member? (ndfa-finals M) state)
                                              (node-data-final-state graph-attributes)
                                              (node-data-regular-state graph-attributes))
                                   'fillcolor (cond [member-of-held-inv? (color-palette-inv-hold-color color-scheme)]
                                                    [member-of-fail-inv? (color-palette-inv-fail-color color-scheme)]
                                                    [else (color-palette-blank-color color-scheme)])
                                   'label state
                                   'fontcolor (color-palette-font-color color-scheme)))))
         cgraph
         (ndfa-states M)))

;; graph machine word (listof rules) (listof rules) symbol -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M current-shown-rules other-current-accept-rules current-reject-rules dead color-scheme accepted?)
  (foldl (λ (rule result)
           (let ([other-current-accept-rule-find-rule? (find-rule? rule dead other-current-accept-rules)]
                 [current-shown-rule-find-rule? (find-rule? rule dead current-shown-rules)]
                 [found-current-reject-rule? (find-rule? rule dead current-reject-rules)]
                 [graph-attributes (graph-attributes-edge-attributes default-graph-attributes)])
             (add-edge result
                       (triple-read rule)
                       (triple-source rule)
                       (triple-destination rule)
                       #:atb (hash 'color (cond [(and current-shown-rule-find-rule? other-current-accept-rule-find-rule? accepted?)
                                                 (color-palette-split-accept-color color-scheme)]
                                                [(and current-shown-rule-find-rule? found-current-reject-rule? (not accepted?))
                                                 (color-palette-split-reject-color color-scheme)]
                                                [(and current-shown-rule-find-rule? found-current-reject-rule? accepted?)
                                                 (color-palette-split-accept-reject-color color-scheme)]
                                                [(and current-shown-rule-find-rule? other-current-accept-rule-find-rule?
                                                      found-current-reject-rule?)
                                                 (color-palette-bi-accept-reject-color color-scheme)]
                                                [(and accepted? current-shown-rule-find-rule?) (color-palette-shown-accept-color color-scheme)]
                                                [current-shown-rule-find-rule? (color-palette-shown-reject-color color-scheme)]
                                                [other-current-accept-rule-find-rule? (color-palette-other-accept-color color-scheme)]
                                                [found-current-reject-rule? (color-palette-other-reject-color color-scheme)]
                                                [else (color-palette-font-color color-scheme)])
                                   'fontsize FONT-SIZE
                                   'style (cond [current-shown-rule-find-rule? (edge-data-accept-edge graph-attributes)]
                                                [(or other-current-accept-rule-find-rule? found-current-reject-rule?)
                                                 (edge-data-reject-edge graph-attributes)]
                                                [(equal? (triple-destination rule) dead) (edge-data-dead-edge graph-attributes)]
                                                [else (edge-data-regular-edge graph-attributes)])))))
         cgraph
         (treelist->list (ndfa-rules M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;CI -> is zipper of ci-structure containing unconsumed and consumed input                                 | (zipperof ci)
;;M  -> is an ndfa                                                                                         | (ndfa-struct)
;;inv -> a list of the invariants provided for verfiying their machine                                     | (listof state (word -> boolean))
;;dead is the sybmol of dead state                                                                         | symbol
;;tracked-accept-trace -> the accepting computation to be showned in the informative messages/main graphic | (listof trace)
;;accepting-computations -> a list of all the computations that lead to accept                             | (listof computation)
;;accept-traces -> The OTHER accepting computations that are showned but not explictly followed            | (listof trace)
;;reject-traces -> All of the computations that machine rejects                                            | (listof trace) 
;;farthest-consumed -> the portion of the word that machine can consume the most of                        | word
;;palette -> the color scheme used for the transition diagrams in the visualization                        | (color-palette-struct)
(struct building-viz-state (CI M inv dead tracked-trace accepting-computations accept-traces reject-traces farthest-consumed palette rejected?)
  #:transparent)

;;viz-state -> (list graph-thunk computation-length)
;;Purpose: Creates a graph thunk and finds the associated computation's length for a given viz-state
(define (create-graph-thunk a-vs)
  (let* (;;(listof configurations)
         ;;Purpose: Returns all configurations using the given word
         [all-configs (get-portion-configs (ci-upci (zipper-current (building-viz-state-CI a-vs)))
                                           (map (λ (comp) (treelist->list (computation-LoC comp)))
                                                (building-viz-state-accepting-computations a-vs)))]

         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from of shown accepting computation
         [tracked-rules (get-trace-rule (building-viz-state-tracked-trace a-vs))]

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
         [current-shown-rules (filter (λ (rule) (not (equal? rule dummy-rule)))
                                             (append-map extract-rules tracked-rules))]

         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [current-a-rules (filter (λ (rule) (not (equal? rule dummy-rule)))
                                  (append-map extract-rules a-configs))]
     
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr all-configs]
                               #:when (equal? (first invs) (ndfa-config-state curr)))
                     (list invs (ci-pci (zipper-current (building-viz-state-CI a-vs)))))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (get-invariants get-invs identity)]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (get-invariants get-invs not)])
    (edge-graph
     (node-graph
      (create-graph 'ndfagraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      (building-viz-state-dead a-vs)
      held-invs
      brkn-invs
      (building-viz-state-palette a-vs))
     (building-viz-state-M a-vs)
     current-shown-rules
     current-a-rules
     current-rules
     (building-viz-state-dead a-vs)
     (building-viz-state-palette a-vs)
     (not (building-viz-state-rejected? a-vs)))))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(zipper-at-end? (building-viz-state-CI a-vs))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [(equal? (ci-upci (zipper-current (building-viz-state-CI a-vs))) 
                 (ndfa-config-word (building-viz-state-farthest-consumed a-vs)))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  [CI (zipper-next (building-viz-state-CI a-vs))]
                                                  [tracked-trace (get-next-traces (building-viz-state-tracked-trace a-vs))]
                                                  [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                                  [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-farthest-consumed-input (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-ndfa-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
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
                     ;;ci
                     [ci (if (or (zipper-at-end? imsg-state-ci)
                                 (equal? (ci-upci (zipper-current imsg-state-ci)) (ndfa-config-word imsg-state-farthest-consumed-input)))
                             imsg-state-ci
                             (zipper-next imsg-state-ci))]
                     ;;shown-accepting-trace
                     [shown-accepting-trace (if (zipper-at-end? imsg-state-shown-accepting-trace) 
                                                imsg-state-shown-accepting-trace
                                                (zipper-next imsg-state-shown-accepting-trace))]
                     ;;invs-zipper
                     [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                                        [(and (not (zipper-at-end? imsg-state-invs-zipper))
                                              (>= (add1 (get-ndfa-config-index-frm-trace imsg-state-shown-accepting-trace))
                                                  (ndfa-config-index (first (zipper-current imsg-state-invs-zipper)))))
                                         (zipper-next imsg-state-invs-zipper)]
                                        [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-farthest-consumed-input (imsg-state-ndfa-farthest-consumed-input (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-ndfa-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
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
         ;;ci
         [ci (cond [(zipper-at-end? imsg-state-ci) imsg-state-ci]
                   [(list? (ndfa-config-word imsg-state-farthest-consumed-input))
                    (zipper-to-idx imsg-state-ci (ndfa-config-index imsg-state-farthest-consumed-input))]
                   [else (zipper-to-end imsg-state-ci)])]
         ;;shown-accepting-trace
         [shown-accepting-trace (if (zipper-at-end? imsg-state-shown-accepting-trace)
                                    imsg-state-shown-accepting-trace
                                    (zipper-to-end imsg-state-shown-accepting-trace))]
         ;;invs-zipper
         [invs-zipper (if (or (zipper-empty? imsg-state-invs-zipper)
                              (zipper-at-end? imsg-state-invs-zipper)
                              (= (zipper-length imsg-state-invs-zipper) 1))
                          imsg-state-invs-zipper
                          (zipper-to-end imsg-state-invs-zipper))])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-ndfa-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
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
                     ;;ci
                     [ci (if (zipper-at-begin? imsg-state-ci) imsg-state-ci (zipper-prev imsg-state-ci))]                     
                     ;;shown-accepting-trace
                     [shown-accepting-trace (if (zipper-at-begin? imsg-state-shown-accepting-trace)
                                                imsg-state-shown-accepting-trace
                                                (zipper-prev imsg-state-shown-accepting-trace))]                     
                     ;;invariant-zipper
                     [invs-zipper (cond [(zipper-empty? imsg-state-invs-zipper) imsg-state-invs-zipper]
                                        [(and (not (zipper-at-begin? imsg-state-invs-zipper))
                                              (<= (sub1 (get-ndfa-config-index-frm-trace imsg-state-shown-accepting-trace))
                                                  (ndfa-config-index (first (zipper-current imsg-state-invs-zipper)))))
                                         (zipper-prev imsg-state-invs-zipper)]
                                        [else imsg-state-invs-zipper])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the beginning
(define (up-key-pressed a-vs)
  (let ([imsg-state-ci (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-shown-accepting-trace (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))]
        [imsg-state-invs-zipper (imsg-state-ndfa-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))])
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
                     ;;ci
                     [ci (if (zipper-at-begin? imsg-state-ci) imsg-state-ci (zipper-to-begin imsg-state-ci))]                   
                     ;;invariant-zipper
                     [invs-zipper (if (zipper-empty? imsg-state-invs-zipper)
                                      imsg-state-invs-zipper
                                      (zipper-to-idx imsg-state-invs-zipper 0))]
                     ;;shown-accepting-trace
                     [shown-accepting-trace (if (zipper-at-begin? imsg-state-shown-accepting-trace)
                                                imsg-state-shown-accepting-trace
                                                (zipper-to-begin imsg-state-shown-accepting-trace))])])])))

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
                    (struct-copy imsg-state-ndfa
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
                                (struct-copy imsg-state-ndfa
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-ndfa-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (let ([imsg-state-invs-zipper (imsg-state-ndfa-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-ci (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]        
        [imsg-state-shown-accepting-trace (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))])
    (if (or (zipper-empty? imsg-state-invs-zipper)
            (< (get-ndfa-config-index-frm-trace imsg-state-shown-accepting-trace)
               (get-ndfa-config-index-frm-invs imsg-state-invs-zipper)))
        a-vs
        (let ([zip (if (and (not (zipper-at-begin? imsg-state-invs-zipper))
                            (<= (get-ndfa-config-index-frm-trace imsg-state-shown-accepting-trace)
                                (get-ndfa-config-index-frm-invs imsg-state-invs-zipper)))
                       (zipper-prev imsg-state-invs-zipper)
                       imsg-state-invs-zipper)])
          (struct-copy
           viz-state
           a-vs
           [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-ndfa-config-index-frm-invs zip))]
           [informative-messages
            (struct-copy
             informative-messages
             (viz-state-informative-messages a-vs)
             [component-state
              (struct-copy imsg-state-ndfa
                           (informative-messages-component-state
                            (viz-state-informative-messages a-vs))
                           ;;ci
                           [ci (zipper-to-idx imsg-state-ci (get-ndfa-config-index-frm-invs zip))]
                           ;;shown-accepting-trace
                           [shown-accepting-trace (zipper-to-idx imsg-state-shown-accepting-trace (get-ndfa-config-index-frm-invs zip))]
                           ;;invariant-zipper
                           [invs-zipper zip])])])))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)  
  (let ([imsg-state-invs-zipper (imsg-state-ndfa-invs-zipper (informative-messages-component-state (viz-state-informative-messages a-vs)))]
        [imsg-state-ci (imsg-state-ndfa-ci (informative-messages-component-state (viz-state-informative-messages a-vs)))]        
        [imsg-state-shown-accepting-trace (imsg-state-ndfa-shown-accepting-trace (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs)))])
    (if (or (zipper-empty? imsg-state-invs-zipper)
            (> (get-ndfa-config-index-frm-trace imsg-state-shown-accepting-trace)
               (get-ndfa-config-index-frm-invs imsg-state-invs-zipper)))
        a-vs
        (let* ([zip (if (and (not (zipper-at-end? imsg-state-invs-zipper))
                             (>= (get-ndfa-config-index-frm-trace imsg-state-shown-accepting-trace)
                                 (get-ndfa-config-index-frm-invs imsg-state-invs-zipper)))
                        (zipper-next imsg-state-invs-zipper)
                        imsg-state-invs-zipper)])
          (struct-copy
           viz-state
           a-vs
           [imgs (vector-zipper-to-idx (viz-state-imgs a-vs) (get-ndfa-config-index-frm-invs zip))]
           [informative-messages
            (struct-copy
             informative-messages
             (viz-state-informative-messages a-vs)
             [component-state
              (struct-copy imsg-state-ndfa
                           (informative-messages-component-state
                            (viz-state-informative-messages a-vs))
                           ;;ci
                           [ci (zipper-to-idx imsg-state-ci (get-ndfa-config-index-frm-invs zip))]                         
                           ;;shown-accepting-trace
                           [shown-accepting-trace (zipper-to-idx imsg-state-shown-accepting-trace (get-ndfa-config-index-frm-invs zip))]
                           ;;invariant zip
                           [invs-zipper zip])])])))))

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


;;ndfa word [boolean] [symbol] . (listof (list state (w -> boolean))) -> (void)
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (ndfa-viz M a-word #:add-dead [add-dead #f] #:palette [palette 'default] invs)
  (let* (;;M ;;Purpose: A new machine with the dead state if add-dead is true
         [new-M (remake-machine (if add-dead (make-new-M M) M))]
         ;;color-pallete ;;The corresponding color scheme to used in the viz
         [color-scheme (cond [(eq? palette 'prot) protanopia-color-scheme] ;;red color blind
                             [(eq? palette 'deut) deuteranopia-color-scheme] ;;green color blind 
                             [(eq? palette 'trit) tritanopia-color-scheme] ;;blue color blind
                             [else standard-color-scheme])]
         ;;symbol ;;Purpose: The name of the dead state
         [dead-state (cond [(and add-dead (eq? (ndfa-type new-M) 'ndfa)) (first (ndfa-states new-M))]
                           [(and add-dead (eq? (ndfa-type new-M) 'dfa)) DEAD]
                           [else 'no-dead])]
         ;;(list (listof computations) hash) ;;Purpose: All computations that the machine can have and the length of computations
         [computations+hash (trace-computations a-word (ndfa-rules new-M) (ndfa-start new-M) (ndfa-finals new-M))]
         ;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations (first computations+hash)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (treelist->list (paths-accepting computations))]
         ;;boolean ;;Purpose: Determines if the word has been rejected
         [rejected? (empty? accepting-computations)]
         ;;(listof computation) ;;Purpose: Extracts all rejecting computations
         [rejecting-computations (treelist->list (paths-rejecting computations))]
         ;;(listof computations) ;;Combination of rejecting and accepting computations
         [pre-loc (append accepting-computations rejecting-computations)]
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map2 computation-LoC pre-loc)]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map2 (λ (accept-comp)
                                  (make-trace (treelist->list (computation-LoC accept-comp))
                                              (treelist->list (computation-LoR accept-comp))
                                              '()))
                                accepting-computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
         [rejecting-traces (map2 (λ (reject-comp)
                                  (make-trace (treelist->list (computation-LoC reject-comp))
                                              (treelist->list (computation-LoR reject-comp))
                                              '()))
                                rejecting-computations)]
         [rejecting-trace (if rejected? (find-longest-computation rejecting-traces '()) '())]
         ;;(listof symbol) ;;Purpose: The portion of the ci that the machine can conusme the most 
         [most-consumed-word (let* ([farthest-consumed (get-farthest-consumed LoC (ndfa-config (ndfa-start new-M) a-word 0))]
                                    [last-word (if (and rejected? (not (empty? (ndfa-config-word farthest-consumed))))
                                                  farthest-consumed
                                                  (ndfa-config (ndfa-start new-M) 'none 0))])
                               (if (eq? 'none (ndfa-config-word last-word))
                                   last-word
                                   (struct-copy ndfa-config
                                                last-word
                                                [state 'last-consumed]
                                                [word (rest (ndfa-config-word last-word))]
                                                [index (add1 (ndfa-config-index last-word))])))]
         ;;(zipperof ci) ;;Purpose: All valid combinations of unconsumed and consumed input
         [CIs (remake-ci a-word)]
         ;;building-state struct
         [building-state (building-viz-state CIs
                                             new-M
                                             invs #;(if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w) #t)) invs) invs) 
                                             dead-state
                                             (if rejected? (list rejecting-trace) (list (first accepting-traces)))
                                             (if (eq? (ndfa-type new-M) 'ndfa) accepting-computations pre-loc)
                                             (if rejected? accepting-traces (rest accepting-traces))
                                             (if rejected? (filter (λ (config) (not (equal? config rejecting-trace)))
                                                                                   rejecting-traces)
                                                 rejecting-traces)
                                             most-consumed-word
                                             color-scheme
                                             rejected?)]
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [computation-lens (begin
                             (for ([key (in-list (hash-keys (second computations+hash)))])
                               (hash-set! (second computations+hash)
                                          key
                                          (set-count (hash-ref (second computations+hash) key))))
                             (second computations+hash))]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         [inv-configs (if (empty? invs)
                          '()
                          (let ([computations (if (eq? (ndfa-type new-M) 'ndfa)
                                                  (map2 (λ (comp) (treelist->list (computation-LoC comp))) accepting-computations)
                                                  (map2 treelist->list LoC))])
                            (get-failed-invariants a-word computations invs)))]
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
    #;(map (λ (x) (λ (grph) (identity grph))) graphs)
    ;(list->vector (map (lambda (x) (lambda (graph0 graph1) (above graph0 graph1))) graphs))
    #;(values rejected? rejecting-trace pre-loc)
    (run-viz graphs
             (list->vector (map (λ (x) (λ (grph) grph)) graphs)
                           #;(map (λ (x) (λ (grph) (identity grph))) graphs))
             #;(list->vector (map (lambda (x) (lambda (graph0 graph1) (above graph0 graph1))) graphs))
             #;(lambda () (list (graph->bitmap (first graphs))))
             (posn (/ E-SCENE-WIDTH 2) (/ NDFA-E-SCENE-HEIGHT 2))
              E-SCENE-WIDTH NDFA-E-SCENE-HEIGHT PERCENT-BORDER-GAP
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages ndfa-create-draw-informative-message
                                   (imsg-state-ndfa new-M
                                                    CIs
                                                    (list->zipper (if (empty? accepting-traces) rejecting-trace (first accepting-traces)))
                                                    most-consumed-word
                                                    (list->zipper inv-configs)
                                                    computation-lens
                                                    (not (empty? accepting-traces))
                                                    0
                                                    (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                      (if (> 0 offset-cap) 0 offset-cap))
                                                    0
                                                    color-scheme)
                                   ndfa-img-bounding-limit)
             (instructions-graphic (above color-legend E-SCENE-TOOLS)
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
                                        (lambda (a-imsgs x-diff y-diff)
                                           (let ([new-scroll-accum (+ (imsg-state-ndfa-scroll-accum a-imsgs) x-diff)])
                                             (cond
                                               [(and (>= (imsg-state-ndfa-word-img-offset-cap a-imsgs)
                                                         (imsg-state-ndfa-word-img-offset a-imsgs))
                                                     (<= (quotient (+ (imsg-state-ndfa-scroll-accum a-imsgs) x-diff) 25) -1))
                                                (struct-copy imsg-state-ndfa
                                                             a-imsgs
                                                             [word-img-offset (+ (imsg-state-ndfa-word-img-offset a-imsgs) 1)]
                                                             [scroll-accum 0])]
                                               [(and (> (imsg-state-ndfa-word-img-offset a-imsgs) 0)
                                                     (>= (quotient (+ (imsg-state-ndfa-scroll-accum a-imsgs) x-diff) 25) 1))
                                                (struct-copy imsg-state-ndfa
                                                             a-imsgs
                                                             [word-img-offset (- (imsg-state-ndfa-word-img-offset a-imsgs) 1)]
                                                             [scroll-accum 0])]
                                               [else
                                                (struct-copy imsg-state-ndfa
                                                             a-imsgs
                                                             [scroll-accum
                                                              (+ (imsg-state-ndfa-scroll-accum a-imsgs) x-diff)])])))])
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
             (if (eq? (ndfa-type new-M) 'ndfa) 'ndfa-viz 'dfa-viz))))