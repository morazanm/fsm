#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         math/matrix
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/misc.rkt"
         "default-informative-messages.rkt"
         (except-in "../viz-lib/viz-constants.rkt"
                    INS-TOOLS-BUFFER)
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE))

(provide pda-viz)
(define FONT-SIZE 18)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
A rule is a structure:
(make-rule triple pair)
triple is the first of the pda rule
pair is the second of the pda rule
|#
(struct rule (triple pair) #:transparent)

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rule is a rule-struct
|#
(struct trace (config rule) #:transparent)

#|
A computation is a structure: (make-computation LoC LoR LoT visited)
LoC is a (listof configuration)
LoR is a (listof rule)
visited is a (listof configuration)
|#
(struct computation (LoC LoR visited) #:transparent)

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func)
  (ormap (λ (L) (eq-func x L)) lst))

;;(listof configurations) -> (listof configurations)
;;Purpose: filters the given list of any empty transitions
(define (remove-empty a-LoC acc)
  (cond [(< (length a-LoC) 2) (reverse (append a-LoC acc))]
        [(and (equal? (second (first a-LoC)) (second (second a-LoC)))
              (equal? (third (first a-LoC)) (third (second a-LoC))))
         (remove-empty (rest a-LoC) acc)]
        [else (remove-empty (rest a-LoC) (cons (first a-LoC) acc))]))

;;rule -> boolean
;;Purpose: Determines if the given rule is an empty rule (e.i. reads, pops, and pushes empty)
(define (empty-rule? a-rule)
  (and (equal? (second (first a-rule)) EMP)
       (equal? (third (first a-rule)) EMP)
       (equal? (second (second a-rule)) EMP)))

;;config rule -> config
;;Purpose: Applys the given rule to the given config and returns the updated config
;;ASSUMPTION: The given rule can be applied to the config
(define (apply-rule a-comp a-rule)
  ;;config -> config
  ;;Purpose: Applies the read portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-read a-config)
    (if (equal? (second (first a-rule)) EMP)
        (list (first (second a-rule)) (second a-config) (third a-config) (fourth a-config))
        (list (first (second a-rule)) (rest (second a-config)) (third a-config) (fourth a-config))))
  ;;config -> config
  ;;Purpose: Applies the pop portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-pop a-config)
    (if (equal? (third (first a-rule)) EMP)
        a-config
        (list (first a-config) (second a-config)
              (drop (third a-config) (length (third (first a-rule)))) (fourth a-config))))
  ;;config -> config
  ;;Purpose: Applies the push portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-push a-config)
    (if (equal? (second (second a-rule)) EMP)
        a-config
        (list (first a-config) (second a-config)
              (append (second (second a-rule)) (third a-config))
              (fourth a-config))))
  ;;config -> config
  ;;Purpose: Updates the config's number if something gets applied to the config (e.i. read/pop/push)
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (update-count a-config)
    (if (empty-rule? a-rule)
        a-config
        (list (first a-config) (second a-config) (third a-config) (add1 (fourth a-config)))))
  (struct-copy computation a-comp
               [LoC (cons (update-count (apply-push (apply-pop (apply-read (first (computation-LoC a-comp))))))
                          (computation-LoC a-comp))]
               [LoR (cons a-rule (computation-LoR a-comp))]
               [visited (cons (first (computation-LoC a-comp)) (computation-visited a-comp))]))
       
(define qempty? empty?)

(define E-QUEUE '())

;; (qof X) → X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; (listof X) (qof X) → (qof X)
;; Purpose: Add the given list of X to the given
;;          queue of X
(define (enqueue a-lox a-qox)
  (append a-qox a-lox))

;; (qof X) → (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))

;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start finals max-cmps)
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (list (append (list start) (list a-word) (list '()) (list 0)))
                                           '()
                                           '())])
    (make-computations lor
                       finals
                       (enqueue (list starting-computation) E-QUEUE)
                       '()
                       max-cmps)))


;;(listof rules) (queueof computation) (listof computation) number -> (listof computation)
;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
;;     that are within the bounds of the max computation limit
(define (make-computations lor finals QoC path max-cmps)
  (cond [(qempty? QoC) path]
        [(and (> (length (computation-LoC (qfirst QoC))) max-cmps)
              (and (member? (first (first (computation-LoC (qfirst QoC)))) finals equal?)
                   (empty? (second (first (computation-LoC (qfirst QoC)))) finals)
                   (empty? (third (first (computation-LoC (qfirst QoC)))) finals)))
         (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)]
        [else (let* ([stack (third (first (computation-LoC (qfirst QoC))))]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume a first letter in the given configurations
                     [connected-read-rules (filter (λ (rule)
                                                     (and (not (empty? (second (first (computation-LoC (qfirst QoC))))))
                                                          (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                          (equal? (second (first rule)) (first (second (first (computation-LoC (qfirst QoC))))))))
                                                   lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume no input for the given configurations
                     [connected-read-E-rules (filter (λ (rule)
                                                       (and (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                            (equal? (second (first rule)) EMP)))
                                                     lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that can pop what is in the stack
                     [connected-pop-rules (filter (λ (rule)
                                                    (or (equal? (third (first rule)) EMP)
                                                        (and (>= (length stack) (length (third (first rule))))
                                                             (equal? (take stack (length (third (first rule)))) (third (first rule))))))
                                                  (append connected-read-E-rules connected-read-rules))]
                     [new-configs (filter (λ (new-c) 
                                            (not (member? (first (computation-LoC new-c)) (computation-visited new-c) equal?)))
                                          (map (λ (rule) (apply-rule (qfirst QoC) rule)) connected-pop-rules))])
                (if (empty? new-configs)
                    (make-computations lor finals (dequeue QoC) (cons (qfirst QoC) path) max-cmps)
                    (make-computations lor finals (enqueue new-configs (dequeue QoC)) path max-cmps)))]))

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(empty? rules) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first (first rules))) EMP)))
         (let* ([rle (rule (list EMP EMP EMP) (list EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-trace(rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (empty-rule? (first rules)))
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (struct-copy trace (first acc)
                                  [rule (cons rle (trace-rule (first acc)))])])
           (make-trace (rest configs) (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) (list rle))])
                (make-trace (rest configs) (rest rules) (cons res acc)))]))

;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word configs)
  (append-map (λ (comp)
                (make-inv-configs-helper a-word (reverse (computation-LoC comp)) (length a-word)))
              configs))

;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs-helper a-word configs word-len)
  (let* ([config (filter (λ (config) (= (length (second config)) word-len)) configs)]
         [inv-config (map (λ (config)
                            (append (list (first config))
                                    (list (take a-word (- (length a-word) word-len)))
                                    (list (third config))
                                    (list (fourth config))))
                          config)])
    (if (empty? configs)
        '()
        (append inv-config
                (make-inv-configs-helper a-word (rest configs) (sub1 word-len))))))

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
                                               (equal? (first inv) (first inv-configs)))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (append inv-configs
                                                  (list (inv-for-inv-config (second inv-configs)
                                                                            (third inv-configs))))))])
        (append inv-config-result
                (get-inv-config-results-helper (rest inv-configs) invs)))))

;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results a-word)
  (remove-duplicates (filter (λ (config) (not (fifth config))) inv-config-results)))

;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M max-cmps)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (second (first config))))
                     (map computation-LoC (get-computations a-word
                                                            (pda-getrules M)
                                                            (pda-getstart M)
                                                            max-cmps))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M max-cmps)]
        [a-word]))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(empty? curr-path) prev-path]
        [(equal? (first prev-path) (first curr-path))
         (remove-similarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similarities (rest prev-path) (rest curr-path) (append acc (list (first curr-path))))]))

;;(listof rules) -> (listof rules)
;;Purpose: Converts the given (listof configurations)s to rules
(define (configs->rules curr-config)
  (make-rule-triples (remove-duplicates curr-config)))

;;word (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Counts the number of unique configurations for each stage of the word
(define (count-computations a-word a-LoC acc)
  ;;word -> number
  ;;Purpose: Counts the number of unique configurations based on the given word
  (define (get-config a-word)
    (length (remove-duplicates
             (append-map (λ (configs)
                           (filter (λ (config)
                                     (equal? a-word (second config)))
                                   configs))
                         a-LoC))))
  (if (empty? a-word)
      (reverse (cons (get-config a-word) acc))
      (count-computations (rest a-word) a-LoC (cons (get-config a-word) acc))))

;;(listof rule-struct) -> (listof rule)
;;Purpose: Remakes the rules extracted from the rule-struct
(define (remake-rules trace-rules)
  (append-map (λ (lor)
                (map (λ (rule)
                       (list (rule-triple rule)
                             (rule-pair rule)))
                     lor))
              trace-rules))

;;rule symbol (listof rules) -> boolean
;;Purpose: Determines if the given rule is a member of the given (listof rules)
;;         or similiar to one of the rules in the given (listof rules) 
(define (find-rule? rule dead lor)
  (or (member? rule lor equal?)
      (ormap (λ (r)
               (and (equal? (first rule) (first r))
                    (or (equal? (third rule) (third r))
                        (and (equal? (third rule) (third r))
                             (equal? (third rule) dead)))))
             lor)))

;;X -> X
;;Purpose: Returns X
(define (id x) x)

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (make-edge-label rule)
  (format "\n[~a ~a ~a]" (second (first rule)) (third (first rule)) (second (second rule))))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  (map (λ (rule)
         (append (list (first (first rule)))
                 (list (string->symbol (make-edge-label rule)))
                 (list (first (second rule)))))
       rules))

;;(listof trace) (X -> Y) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-X LoT map-func)
  (filter-map-acc empty? map-func not first LoT))

;(listof symbol ((listof symbol) (listof symbol) -> boolean))) (X -> Y) -> (listof symbol ((listof symbol) (listof symbol) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) (listof symbols) -> boolean)))
(define (get-invariants LoI func)
  (filter-map-acc (λ (x) ((second (first x)) (second x) (third x))) first func first LoI))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not id LoT))

;;(listof symbols) (listof configurations) -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                          (equal? (second configs) word))
                        (computation-LoC config)))
              full-configs))

;; (listof configuration) number -> (listof configuration)
;; Purpose: Returns the first configuration in a (listof configuration) if it exceeds the cut-off amount
(define (get-cut-off LoC max-cmps)
  (filter-map (λ (config)
                (and (>= (length config) max-cmps)
                     (first config)))
              LoC))

;; (listof configuration) (listof symbol) -> (listof symbol)
;; Purpose: Returns the most consumed input
(define (get-farthest-consumed LoC acc)
  (cond [(empty? LoC) acc]
        [(< (length (second (first (first LoC)))) (length acc))
         (get-farthest-consumed (rest LoC) (second (first (first LoC))))]
        [else (get-farthest-consumed (rest LoC) acc)]))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M dead held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (pda-getstart M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv equal?) (member? state fail-inv equal?)) 'wedged]
                                              [(or (member? state held-inv equal?)
                                                   (member? state fail-inv equal?)
                                                   (member? state cut-off equal?)) 'filled]
                                              [(eq? state dead) 'dashed]
                                              [else 'solid])
                                 'shape (if (member? state (pda-getfinals M) equal?) 'doublecircle 'circle)
                                 'fillcolor (cond [(member? state cut-off equal?) GRAPHVIZ-CUTOFF-GOLD]
                                                  [(and (member? state held-inv equal?) (member? state fail-inv equal?))
                                                   "red:chartreuse4"]
                                                  [(member? state held-inv equal?) HELD-INV-COLOR ]
                                                  [(member? state fail-inv equal?) BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 'fontcolor 'black
                                 'fontname (if (and (member? state held-inv equal?) (member? state fail-inv equal?))
                                               "times-bold"
                                               "Times-Roman"))))
         dgraph
         (pda-getstates M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-a-rules current-rules dead)
  (foldl (λ (rule graph)
           (add-edge graph
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(find-rule? rule dead current-a-rules)
                                               'green]
                                              [(find-rule? rule dead current-rules)
                                               'violetred]
                                              [else 'black])
                                 'style (cond [(equal? (third rule) dead) 'dashed]
                                              [(member? rule current-a-rules equal?) 'bold]
                                              [else 'solid])
                                 'fontsize FONT-SIZE)))
         dgraph
         rules))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  (let* (;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of all configurations
         [rejecting-rules (get-trace-X (building-viz-state-reject-traces a-vs) trace-rule)]

         ;;(listof configuration)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs (get-portion-configs (building-viz-state-upci a-vs)
                                               (building-viz-state-acc-comp a-vs))]

         ;;(listof symbol)
         ;;Purpose: Gets the states where it's computation has cutoff
         [cut-off-states (if cut-off
                             (remove-duplicates (map first (get-cut-off (building-viz-state-computations a-vs)
                                                                        (building-viz-state-max-cmps a-vs))))
                             '())]
         
         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [accepting-rules (get-trace-X (building-viz-state-accept-traces a-vs) trace-rule)]         
         
         ;;(listof rule)
         ;;Purpose: Converts the current rules from the rejecting computations and makes them usable for graphviz
         [current-r-rules (configs->rules (filter (λ (rule) (not (equal? rule DUMMY-RULE))) (remake-rules rejecting-rules)))]
                  
         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-a-rules (configs->rules (filter (λ (rule) (not (equal? rule DUMMY-RULE))) (remake-rules accepting-rules)))]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (pda-getrules (building-viz-state-M a-vs)))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (equal? (first invs) (first curr)))
                     (list invs (building-viz-state-pci a-vs) (third curr)))]

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
     current-a-rules
     current-r-rules
     (building-viz-state-dead a-vs))))
    
;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(and (empty? (building-viz-state-upci a-vs))
              (or (list? (building-viz-state-stack a-vs))
                  (zipper-at-end? (building-viz-state-stack a-vs))))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [(and (eq? (building-viz-state-upci a-vs) (building-viz-state-farthest-consumed a-vs))
              (ormap (λ (comp-len) (>= comp-len (building-viz-state-max-cmps a-vs)))
                     (map length (building-viz-state-computations a-vs))))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
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
                                                                          (not (eq? (second (first comp)) (building-viz-state-upci a-vs))))
                                                                        (building-viz-state-computations a-vs))]
                                                  [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                                  [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

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
;;farthest-consumed (listof symbol) => the portion the ci that the machine consumed the most of
(struct building-viz-state (upci pci computations acc-comp stack accept-traces reject-traces M inv dead max-cmps farthest-consumed))

(define E-SCENE (empty-scene 1250 600))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [pci (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (eq? (imsg-state-upci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                           (imsg-state-farthest-consumed (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))))
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (append (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                          (list (first (imsg-state-upci (informative-messages-component-state
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
        (struct-copy imsg-state
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [upci (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                   (eq? (imsg-state-upci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                        (imsg-state-farthest-consumed (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                               
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (zipper-at-end? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))))
                                     (imsg-state-acpt-trace (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                     (zipper-next (imsg-state-acpt-trace (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))]
                     [stack (if (or (zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))))
                                    (zipper-at-end? (imsg-state-stack (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                                (imsg-state-stack (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))
                                (zipper-next (imsg-state-stack (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))))]
                     [invs-zipper (cond [(zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))))
                                         (imsg-state-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs)))))
                                              (>= pci-len (first (zipper-unprocessed
                                                                  (imsg-state-invs-zipper (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs)))))))
                                         (zipper-next (imsg-state-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-invs-zipper (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let* (;;(listof symbol)
         ;;Purpose: The entire given word
         [full-word (append (imsg-state-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                            (imsg-state-upci (informative-messages-component-state
                                              (viz-state-informative-messages a-vs))))]
         ;;(zipperof invariant)
         ;;Purpose: The index of the last failed invariant
         [zip (if (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                  (imsg-state-invs-zipper (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                  (zipper-to-idx (imsg-state-invs-zipper (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                 (imsg-state-inv-amt (informative-messages-component-state
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
         imsg-state
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         [upci (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (imsg-state-upci (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))]
                     [(not (empty? (imsg-state-farthest-consumed (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))
                      (drop full-word (- (length full-word)
                                         (length (imsg-state-farthest-consumed (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))))]
                     [else '()])]
         [pci 
          (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
                 (imsg-state-pci (informative-messages-component-state
                                  (viz-state-informative-messages a-vs)))]
                [(not (empty? (imsg-state-farthest-consumed (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))
                 (take full-word (- (length full-word)
                                    (length (imsg-state-farthest-consumed (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs))))))]
                [else full-word])]
         [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                             (zipper-at-end? (imsg-state-acpt-trace (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                         (imsg-state-acpt-trace (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                         (zipper-to-end (imsg-state-acpt-trace (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))))]
         [stack (cond [(zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                       (imsg-state-stack (informative-messages-component-state
                                          (viz-state-informative-messages a-vs)))]
                      [(or (zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                           (zipper-at-end? (imsg-state-stack (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))
                       (imsg-state-stack (informative-messages-component-state
                                          (viz-state-informative-messages a-vs)))]
                      [else (zipper-to-end (imsg-state-stack (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))])]
         
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                             (zipper-at-begin? (imsg-state-acpt-trace (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                         (imsg-state-acpt-trace (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                         (zipper-prev (imsg-state-acpt-trace (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))]
         [next-rule (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
                        (imsg-state-acpt-trace (informative-messages-component-state
                                                (viz-state-informative-messages a-vs)))
                        (first (trace-rule (zipper-current (imsg-state-acpt-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))))))]
         [rule (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                   DUMMY-RULE
                   (list (rule-triple next-rule) (rule-pair next-rule)))]
         [pci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))
                      (and (equal? (second (first rule)) EMP)
                           (not (empty-rule? rule))))
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (take (imsg-state-pci (informative-messages-component-state
                                         (viz-state-informative-messages a-vs)))
                        (sub1 (length (imsg-state-pci (informative-messages-component-state
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
        (struct-copy imsg-state
                     (informative-messages-component-state
                      (viz-state-informative-messages a-vs))
                     [upci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                   (and (equal? (second (first rule)) EMP)
                                        (not (empty-rule? rule))))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
                     [acpt-trace acpt-trace]
                     [stack (if (or (zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))))
                                    (zipper-at-begin? (imsg-state-stack (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))
                                (imsg-state-stack (informative-messages-component-state
                                                   (viz-state-informative-messages a-vs)))
                                (zipper-prev (imsg-state-stack (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))))]
                     
                     [invs-zipper (cond [(zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))))
                                         (imsg-state-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                              (viz-state-informative-messages a-vs)))))
                                              (<= pci-len (first (zipper-processed
                                                                  (imsg-state-invs-zipper (informative-messages-component-state
                                                                                           (viz-state-informative-messages a-vs)))))))
                                         (zipper-prev (imsg-state-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-invs-zipper (informative-messages-component-state
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
      (struct-copy imsg-state
                   (informative-messages-component-state
                    (viz-state-informative-messages a-vs))
                   [upci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))
                             (imsg-state-upci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                             (append (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                   [pci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
                            (imsg-state-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                            '())]
                   [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                       (zipper-at-begin? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))))
                                   (imsg-state-acpt-trace (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs)))
                                   (zipper-to-begin (imsg-state-acpt-trace (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))]
                   [stack (if (or (zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
                                  (zipper-at-begin? (imsg-state-stack (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                              (imsg-state-stack (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                              (zipper-to-begin (imsg-state-stack (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))]
                   [invs-zipper (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                        (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))))
                                    (imsg-state-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-invs-zipper (informative-messages-component-state
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
                    (struct-copy imsg-state
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
                                (struct-copy imsg-state
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset (imsg-state-word-img-offset-cap a-imsgs)])])])))

;;viz-state -> viz-state
;;Purpose: Jumps to the previous broken invariant
(define (j-key-pressed a-vs)
  (if (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                              (viz-state-informative-messages a-vs))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))))
                           (<= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                      (zipper-prev (imsg-state-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                      (imsg-state-invs-zipper (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))])
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [upci (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))
                                          (>= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                                      (drop full-word (zipper-current zip))]
                                     [(zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs))))
                                      (imsg-state-upci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))]
                                     [else (drop full-word (zipper-current zip))])]
                         [acpt-trace (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs))))
                                                 (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs))))
                                                 (not (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs))))))
                                            (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))
                                                           (zipper-current zip))]
                                           [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs))))
                                                 (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs)))))
                                            (imsg-state-acpt-trace (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))]
                                           [else (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs)))
                                                                (zipper-current zip))])]
                         [stack (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs))))
                                            (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))))
                                       (zipper-to-idx (imsg-state-stack (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))
                                                      (zipper-current zip))]
                                      [(zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))
                                       (imsg-state-stack (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
                                      [else (zipper-to-idx (imsg-state-stack (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))
                                                           (zipper-current zip))])]
                         [pci (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))
                                          (>= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                                     (take full-word (zipper-current zip))]
                                    [(zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                     (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))]
                                    [else (take full-word (zipper-current zip))])]
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (if (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                              (viz-state-informative-messages a-vs))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))
                           (or (>= (length (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                   (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))))
                      (zipper-next (imsg-state-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                      (imsg-state-invs-zipper (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             [partial-word (if (> (zipper-current zip) (length full-word))
                               full-word
                               (take full-word (zipper-current zip)))])
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         [upci (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))
                                          (<= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                                      (drop full-word (zipper-current zip))]
                                     [(zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs))))
                                      (imsg-state-upci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))]
                                     [else (drop full-word (zipper-current zip))])]
                         [acpt-trace (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs))))
                                                 (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs))))
                                                 (not (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                             (viz-state-informative-messages a-vs))))))
                                            (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))
                                                           (zipper-current zip))]
                                           [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                            (viz-state-informative-messages a-vs))))
                                                 (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs)))))
                                            (imsg-state-acpt-trace (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))]
                                           [else (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                        (viz-state-informative-messages a-vs)))
                                                                (zipper-current zip))])]
                         [stack (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs))))
                                            (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))))
                                       (zipper-to-idx (imsg-state-stack (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))
                                                      (zipper-current zip))]
                                      [(zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))
                                       (imsg-state-stack (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))]
                                      [else (zipper-to-idx (imsg-state-stack (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))
                                                           (zipper-current zip))])] 
                         [pci (cond [(and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs))))
                                          (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))
                                          (<= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                                     (take full-word (zipper-current zip))]
                                    [(zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs))))
                                     (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))]
                                    [else (take full-word (zipper-current zip))])]
                         [invs-zipper zip])])]))))

;;machine -> machine
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (local [;;symbol
          ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
          ;;         otherwise uses DEAD
          (define dead (if (member? DEAD (pda-getstates M) equal?) (gen-state (pda-getstates M)) DEAD))
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
    (make-unchecked-ndpda (append (pda-getstates M) (list dead))
                          (pda-getalphabet M)
                          (pda-getgamma M)
                          (pda-getstart M)
                          (pda-getfinals M)
                          (append (pda-getrules M) rules-to-dead dead-read-rules dead-pop-rules))))


;;pda word [boolean] [natnum] . -> (void)
;;Purpose: Visualizes the given pda processing the given word
(define (pda-viz M a-word #:add-dead [add-dead #f] #:cut-off [cut-off 100] invs)
  (let* (;;M ;;Purpose: A new machine with the dead state if add-dead is true
         [new-M (if add-dead (make-new-M M) M)]
         ;;symbol ;;Purpose: The name of the dead state
         [dead-state (if add-dead (last (pda-getstates new-M)) 'no-dead)]
         ;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations (get-computations a-word (pda-getrules new-M) (pda-getstart new-M) (pda-getfinals new-M) cut-off)]
         
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;number ;;Purpose: The length of the word
         [word-len (length a-word)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (filter (λ (comp)
                                           (and (member? (first (first (computation-LoC comp))) (pda-getfinals new-M) equal?)
                                                (empty? (second (first (computation-LoC comp))))
                                                (empty? (third (first (computation-LoC comp))))))
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
                                                (reverse (computation-LoC (first accepting-computations))))
                                            '()))]
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
                                             accept-cmps
                                             rejecting-traces
                                             new-M
                                             (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w s) #t)) invs) invs) 
                                             dead-state
                                             cut-off
                                             most-consumed-word)]
                     
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof computation) ;;Purpose: Gets all the cut off computations if the length of the word is greater than max computations
         [get-cut-off-comp (if (> word-len cut-off)
                               (map first LoC)
                               '())]
         ;;(listof computation) ;;Purpose: Makes the cut off computations if the length of the word is greater than max computations
         [cut-off-comp (if (empty? get-cut-off-comp)
                           LoC
                           (map (λ (cut-off-comp comp)
                                  (append comp (list cut-off-comp)))
                                get-cut-off-comp
                                LoC))]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [computation-lens (count-computations a-word cut-off-comp '())]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         [inv-configs (remove-duplicates (sort (map (λ (con)
                                                      (fourth con))
                                                    (return-brk-inv-configs
                                                     (get-inv-config-results
                                                      (make-inv-configs a-word accepting-computations)
                                                      invs)
                                                     a-word)) <))])
    (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ PDA-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages pda-create-draw-informative-message
                                   (imsg-state new-M
                                               a-word
                                               '()
                                               (list->zipper accepting-trace)
                                               stack
                                               most-consumed-word
                                               (list->zipper inv-configs) 
                                               (sub1 (length inv-configs))
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
                                     [ "j" jump-prev j-key-pressed]
                                     [ "l" jump-next l-key-pressed]
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
                                        [ J-KEY-DIMS jump-prev j-key-pressed]
                                        [ L-KEY-DIMS jump-next l-key-pressed]))
             'pda-viz)))

;[(<= max-cmps 0) (error (format "The maximum amount of computations, ~a, must be integer greater than 0" max-cmps))] DONT FORGET
