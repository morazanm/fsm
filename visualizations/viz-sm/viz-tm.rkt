#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         math/matrix
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         (except-in "../viz-lib/viz-constants.rkt"
                    INS-TOOLS-BUFFER)
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/misc.rkt"
          "../../fsm-core/interface.rkt"
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE)
         "default-informative-messages.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule)
|#
(struct trace (config rules) #:transparent)

#|
A rule is a structure:
(make-rule read action)
read is the first pair in a tm rule
action is the second pair in a tm rule
|#
(struct rule (read action) #:transparent)


;;tape is the input the tape
;;computations is a (listof computation) that attempt to consume the ci
;;accepting computations is (listof computation) for all accepting computations
;;accept-traces is a (listof configuration)
;;reject-traces is a (listof configuration)
;;M is the given machine
;;inv is a the (listof (state (listof symbol -> boolean)))
;;max-cmps is the max amount of transitions the machine can make
;;head-pos is the beginning head position of the tape=
(struct building-viz-state (tape computations acc-comp accept-traces reject-traces M inv max-cmps head-pos) #:transparent)



#|
A computation is a structure: (make-computation LoC LoR LoT visited)
LoC is a (listof configuration)
LoR is a (listof rule)
visited is a (listof configuration)
|#
(struct computation (LoC LoR visited) #:transparent)

(define DUMMY-RULE (list (list BLANK BLANK) (list BLANK BLANK)))

(define qempty? empty?)

(define E-QUEUE '())

(define (tm-getalphabet m) (m '() 0 'get-alphabet)) 
  
(define (tm-getstates m) (m '() 0 'get-states))
  
(define (tm-getfinals m) (m '() 0 'get-finals))

(define (tm-getdelta m) (m '() 0 'get-delta)) ;;; parsed rules

(define (tm-getrules m) (m '() 0 'get-rules))  ;;; unparsed rules

(define (tm-getstart m) (m '() 0 'get-start))
  
(define (tm-getaccept m) (m '() 0 'get-accept))

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

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func)
  (ormap (λ (L) (eq-func x L)) lst))


;;config rule -> config
;;Purpose: Applys the given rule to the given config and returns the updated config
;;ASSUMPTION: The given rule can be applied to the config
(define (apply-rule a-comp a-rule)
  ;;config -> config
  ;;Purpose: Applies the action portion of given rule to the given config
  (define (apply-action a-config)
    (list (first (second a-rule))
          (cond [(eq? (second (second a-rule)) RIGHT) (add1 (second a-config))]
                [(eq? (second (second a-rule)) LEFT)  (sub1 (second a-config))]
                [else (second a-config)])
          (mutate-tape a-config)))
  ;;config -> tape
  ;;Purpose: "Mutates" the tape if possible 
  (define (mutate-tape a-config)
    (if (or (eq? (second (second a-rule)) BLANK)
            (eq? (second (second a-rule)) RIGHT)
            (eq? (second (second a-rule)) LEFT))
        (third a-config)
        (append (take (third a-config) (second a-config))
                (list (second (second a-rule)))
                (rest (drop (third a-config) (second a-config))))))
   
  ;;config -> config
  ;;Purpose: Adds a blank to the end of the tape
  (define (add-blank a-config)
    (if (not (= (second a-config) (length (third a-config))))
        a-config
        (list (first a-config)
              (second a-config)
              (append (third a-config) (list BLANK)))))
  
  (struct-copy computation a-comp
               [LoC (cons (add-blank (apply-action (first (computation-LoC a-comp)))) (computation-LoC a-comp))]
               [LoR (cons a-rule (computation-LoR a-comp))]
               [visited (cons (first (computation-LoC a-comp)) (computation-visited a-comp))]))

;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start finals max-cmps head-pos)
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (list (append (list start) (list head-pos) (list a-word)))
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
        [(or (> (length (computation-LoC (qfirst QoC))) max-cmps)
             (member? (first (first (computation-LoC (qfirst QoC)))) finals equal?))
         (make-computations lor finals (dequeue QoC) (cons (qfirst QoC) path) max-cmps)]
        [else (let* (;;(listof rules)
                     ;;Purpose: Filters all the rules that can be applied to the configuration by reading the element in the rule
                     [connected-read-rules (filter (λ (rule)
                                                     (and (< (second (first (computation-LoC (qfirst QoC))))
                                                             (length (third (first (computation-LoC (qfirst QoC))))))
                                                          (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                          (equal? (second (first rule))
                                                                  (list-ref (third (first (computation-LoC (qfirst QoC))))
                                                                            (second (first (computation-LoC (qfirst QoC))))))))
                                                   lor)]
                     ;;(listof computation)
                     [new-configs (filter (λ (new-c) 
                                            (not (member? (first (computation-LoC new-c)) (computation-visited new-c) equal?)))
                                          (map (λ (rule) (apply-rule (qfirst QoC) rule))
                                               connected-read-rules))])
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
         (let* ([rle (rule (first DUMMY-RULE) (second DUMMY-RULE))]
                [res (trace (first configs) (list rle))])
           (make-trace(rest configs) rules (cons res acc)))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) (list rle))])
                (make-trace (rest configs) (rest rules) (cons res acc)))]))


;(listof symbols) (lisof configurations) -> (listof configurations)
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


;;(listof rule-struct) -> (listof rule)
;;Purpose: Remakes the rules extracted from the rule-struct
(define (remake-rules trace-rules)
  (append-map (λ (lor)
                (map (λ (rule)
                       (list (rule-read rule)
                             (rule-action rule)))
                     lor))
              trace-rules))


;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (make-edge-label rule)
  (format "\n[~a ~a]" (second (first rule)) (second (second rule))))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  (map (λ (rule)
         (append (list (first (first rule)))
                 (list (string->symbol (make-edge-label rule)))
                 (list (first (second rule)))))
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

;; (listof configuration) number -> (listof configuration)
;; Purpose: Returns the first configuration in a (listof configuration) if it exceeds the cut-off amount
(define (get-cut-off LoC max-cmps)
  (filter-map (λ (config)
                (and (>= (length config) max-cmps)
                     (first config)))
              LoC))

;;(listof rules) -> (listof rules)
;;Purpose: Converts the given (listof configurations)s to rules
(define (configs->rules curr-config)
  (make-rule-triples (remove-duplicates curr-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (sm-start M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv equal?) (member? state fail-inv equal?)) 'wedged]
                                              [(or (member? state held-inv equal?)
                                                   (member? state fail-inv equal?)
                                                   (member? state cut-off equal?)) 'filled]
                                              [else 'solid])
                                 'shape (cond [(equal? state (sm-accept M)) 'doubleoctagon]
                                              [(member? state (sm-finals M) equal?) 'doublecircle]
                                              [else 'circle])
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
         (sm-states M)))

;;graph machine -> graph
;;Purpose: Creates the edges for the given graph
(define (make-edge-graph dgraph rules current-a-rules current-rules)
  (foldl (λ (rule graph)
           (add-edge graph
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(find-rule? rule current-a-rules) 'green]
                                              [(find-rule? rule current-rules) 'violetred]
                                              [else 'black])
                                 'style (if (member? rule current-a-rules equal?)
                                            'bold
                                            'solid)
                                 ;'labelfloat 'true
                                 'fontsize 12)))
         dgraph
         rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  (let* (;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of all configurations
         [rejecting-rules (get-trace-X (building-viz-state-reject-traces a-vs) trace-rules)]

         ;;(listof configuration)
         ;;Purpose: Extracts all the configs from both the accepting and rejecting configs
         [current-configs '()#;(get-portion-configs (building-viz-state-upci a-vs)
                                               (building-viz-state-acc-comp a-vs))]

         ;;(listof symbol)
         ;;Purpose: Gets the states where it's computation has cutoff
         [cut-off-states (if cut-off
                             (remove-duplicates (map first (get-cut-off (building-viz-state-computations a-vs)
                                                                        (building-viz-state-max-cmps a-vs))))
                             '())]
         
         ;;(listof rule-struct)
         ;;Purpose: Extracts the rules from the first of the accepting computations
         [accepting-rules (get-trace-X (building-viz-state-accept-traces a-vs) trace-rules)]         
         
         ;;(listof rule)
         ;;Purpose: Converts the current rules from the rejecting computations and makes them usable for graphviz
         [current-r-rules (configs->rules (filter (λ (rule) (not (equal? rule DUMMY-RULE))) (remake-rules rejecting-rules)))]
                  
         ;;(listof rules)
         ;;Purpose: Converts the current rules from the accepting computations and makes them usable for graphviz
         [current-a-rules (configs->rules (filter (λ (rule) (not (equal? rule DUMMY-RULE))) (remake-rules accepting-rules)))]
         
         ;;(listof rules)
         ;;Purpose: All of the pda rules converted to triples
         [all-rules (make-rule-triples (filter (λ (rule) (not (equal? (second (first rule)) LM))) (sm-rules (building-viz-state-M a-vs))))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (equal? (first invs) (first curr)))
                     (list invs '() #;(building-viz-state-pci a-vs) (third curr)))]

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         [brkn-invs (get-invariants get-invs not)]
         
         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         [held-invs (get-invariants get-invs id)])
    (make-edge-graph
     (make-node-graph
      (create-graph 'tmgraph #:atb (hash 'rankdir "LR"))
      (building-viz-state-M a-vs)
      held-invs
      brkn-invs
      cut-off-states)
     all-rules
     current-a-rules
     current-r-rules)))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(and (empty? (building-viz-state-accept-traces a-vs))
              (empty? (building-viz-state-reject-traces a-vs)))
         (reverse acc)]
        [(ormap (λ (comp-len) (>= comp-len (building-viz-state-max-cmps a-vs)))
                     (map length (building-viz-state-computations a-vs)))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  #;[computations (filter (λ (comp)
                                                                          (not (eq? (second (first comp)) (building-viz-state-upci a-vs))))
                                                                        (building-viz-state-computations a-vs))]
                                                  [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                                  [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                     (cons next-graph acc)))]))



;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (#;[completed-config? (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                                   (get-computations (imsg-state-pci (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))
                                                     (pda-getrules (imsg-state-M (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))
                                                     (pda-getstart (imsg-state-M (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))
                                                     (imsg-state-max-cmps (informative-messages-component-state
                                                                           (viz-state-informative-messages a-vs)))))]
         ;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         #;[pci (if (or (not completed-config?)
                      (empty? (imsg-state-upci (informative-messages-component-state
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
         [pci-len 0])
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
                     #;[upci (if (or (not completed-config?)
                                   (empty? (imsg-state-upci (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                   (eq? (imsg-state-upci (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                                        (imsg-state-farthest-consumed (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                               
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     #;[pci pci]
                     [acpt-trace (if (or (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (zipper-at-end? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                 (viz-state-informative-messages a-vs)))))
                                     (imsg-state-acpt-trace (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                     (zipper-next (imsg-state-acpt-trace (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))]
                     #;[stack (if (or (zipper-empty? (imsg-state-stack (informative-messages-component-state
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
         #;[full-word (append (imsg-state-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                            (imsg-state-upci (informative-messages-component-state
                                              (viz-state-informative-messages a-vs))))]
         ;;(listof symbol)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         #;[last-consumed-word (last-fully-consumed
                              full-word
                              (imsg-state-M (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                              (imsg-state-max-cmps (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))))]
         ;;(listof symbol)
         ;;Purpose: The portion of the word that cannont be consumed
         #;[unconsumed-word (remove-similarities last-consumed-word full-word '())]
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
         #;[upci (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (imsg-state-upci (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))]
                     [(not (empty? (imsg-state-farthest-consumed (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs)))))
                      (drop full-word (- (length full-word) (length (imsg-state-farthest-consumed (informative-messages-component-state
                                                                                                   (viz-state-informative-messages a-vs))))))]
                     [else '()])]
         #;[pci 
          (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
                 (imsg-state-pci (informative-messages-component-state
                                  (viz-state-informative-messages a-vs)))]
                [(not (empty? (imsg-state-farthest-consumed (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))
                 (take full-word (- (length full-word) (length (imsg-state-farthest-consumed (informative-messages-component-state
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
         #;[stack (cond [(zipper-empty? (imsg-state-stack (informative-messages-component-state
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
         #;[next-rule (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
                        (imsg-state-acpt-trace (informative-messages-component-state
                                                (viz-state-informative-messages a-vs)))
                        (first (trace-rules (zipper-current (imsg-state-acpt-trace (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))))))]
         #;[rule (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
                   DUMMY-RULE
                   (list (rule-triple next-rule) (rule-pair next-rule)))]
         #;[pci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))
                      (and (equal? (second (first rule)) EMP)
                           (not (empty-rule? rule))))
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (take (imsg-state-pci (informative-messages-component-state
                                         (viz-state-informative-messages a-vs)))
                        (sub1 (length (imsg-state-pci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))))]
         [pci-len 0 #;(length pci)])
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
                     #;[upci (if (or (empty? (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                   (and (equal? (second (first rule)) EMP)
                                        (not (empty-rule? rule))))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     #;[pci pci]
                     [acpt-trace acpt-trace]
                     #;[stack (if (or (zipper-empty? (imsg-state-stack (informative-messages-component-state
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
                   #;[upci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))
                             (imsg-state-upci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs)))
                             (append (imsg-state-pci (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs)))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                   #;[pci (if (empty? (imsg-state-pci (informative-messages-component-state
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
                   #;[stack (if (or (zipper-empty? (imsg-state-stack (informative-messages-component-state
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
  (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))
          (and (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs))))
               (not (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))))
          (< (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))))
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
             #;[full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             #;[partial-word (if (> (zipper-current zip) (length full-word))
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
                         #;[upci (remove-similarities full-word partial-word '())]
                         [acpt-trace (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (imsg-state-acpt-trace (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))
                                         (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs)))
                                                        (zipper-current zip)))]
                         #;[stack (if (zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))))
                                    (imsg-state-stack (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))
                                                   (zipper-current zip)))]
                         #;[pci partial-word]
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))
          (and (zipper-at-end? (imsg-state-invs-zipper (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
               (not (zipper-at-begin? (imsg-state-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))))
          (> (length (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))))))
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
             #;[full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             #;[partial-word (if (> (zipper-current zip) (length full-word))
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
                         #;[upci (remove-similarities full-word partial-word '())]
                         [acpt-trace (if (zipper-empty? (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))))
                                         (imsg-state-acpt-trace (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))
                                         (zipper-to-idx (imsg-state-acpt-trace (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs)))
                                                        (zipper-current zip)))]
                         #;[stack (if (zipper-empty? (imsg-state-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))))
                                    (imsg-state-stack (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-stack (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))
                                                   (zipper-current zip)))]
                         #;[pci partial-word]
                         [invs-zipper zip])])]))))


;;tm tape [natnum] [natnum] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (tm-viz M a-word head-pos #:cut-off [cut-off 100] . invs) ;;GET RID OF . FOR TESTING
  (let* (;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations (get-computations a-word (sm-rules M) (sm-start M) (sm-finals M) cut-off head-pos)]
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;number ;;Purpose: The length of the word
         [word-len (length a-word)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (filter (λ (comp)
                                           (equal? (first (first (computation-LoC comp))) (sm-accept M)))
                                         computations)]
         ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
         [accepting-traces (map (λ (acc-comp)
                                  (make-trace (reverse (computation-LoC acc-comp))
                                              (reverse (computation-LoR acc-comp))
                                              '()))
                                accepting-computations)]
         ;;(listof trace) ;;Purpose: Gets the cut off trace if the the word length is greater than the cut
         [cut-accept-traces '()
                            #;(if (> word-len max-cmps)
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
         ;;(listof rules) ;;Purpose: Returns the first accepting computations (listof rules)
         [accepting-trace (if (empty? accept-cmps) '() (first accept-cmps))]
         ;;building-state struct
         [building-state (building-viz-state a-word
                                             LoC
                                             accepting-computations
                                             accept-cmps
                                             rejecting-traces
                                             M
                                             invs
                                             cut-off
                                             head-pos)]
                     
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
         [computation-lens 0 #;(count-computations a-word cut-off-comp '())]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         #;[inv-configs (map (λ (con)
                             (fourth con))
                           (return-brk-inv-configs
                            (get-inv-config-results
                             (make-inv-configs a-word accepting-computations)
                             invs)
                            a-word))])
    ;(displayln computations)
    (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ TM-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages tm-create-draw-informative-message
                                   (imsg-state-tm M
                                               a-word
                                               (list->zipper accepting-trace)
                                               (list->zipper '() #;inv-configs) 
                                               0 ;(sub1 (length inv-configs))
                                               computation-lens
                                               LoC
                                               cut-off
                                               head-pos
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
                                     [ "j" jump-prev j-key-pressed]
                                     [ "l" jump-next l-key-pressed]
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
                                        [ J-KEY-DIMS jump-prev j-key-pressed]
                                        [ L-KEY-DIMS jump-next l-key-pressed]))
             'tm-viz)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;States (i = head's position)
;; K - tape[1..i-1] contains an even amount of a's and even bs
;; H - tape[1..i-1] contains an odd amount of a's and even bs
;; I - tape[1..i-1] contains an odd amount of b's and even as
;; B - tape[1..i-1] contains an odd amount of a's and odd bs
;; S - tape[i] = blank AND tape[1..i-1] contains an even amount of a's and even bs, final state

;;Pre-condition = tape = LMw_ AND i = 0
(define EVEN-AS-&-BS (make-tm '(K H I B S)
                          '(a b)
                          `(((K ,BLANK) (S ,BLANK))
                            ((K a) (H ,RIGHT)) ((H a) (K ,RIGHT)) ((H b) (B ,RIGHT)) ((B b) (H ,RIGHT))
                            ((K b) (I ,RIGHT)) ((I b) (K ,RIGHT)) ((I a) (B ,RIGHT)) ((B a) (I ,RIGHT)))
                          'K
                          '(S)
                          'S))

(define a* (make-tm '(S Y N)
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
(define a*Ua*b (make-tm '(S A B C Y N)
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

(define anbncn (make-tm '(S A B C D E F G H I J K L Y)
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



#|
(reverse (computation-LoC (first (get-computations '(@ a a b)
                                                   (sm-rules a*Ua*b)
                                                   (sm-start a*Ua*b)
                                                   (sm-finals a*Ua*b)
                                                   100
                                                   #:head-pos 1))))
(sm-showtransitions a*Ua*b '(@ a a b) 1)
""
(reverse (computation-LoC (first (get-computations '(@ a b a b)
                                                   (sm-rules EVEN-AS-&-BS)
                                                   (sm-start EVEN-AS-&-BS)
                                                   (sm-finals EVEN-AS-&-BS)
                                                   100))))
(sm-showtransitions EVEN-AS-&-BS '(@ a b a b))
""
(reverse (computation-LoC (first (get-computations '(@ _ a b c)
                                                   (sm-rules anbncn)
                                                   (sm-start anbncn)
                                                   (sm-finals anbncn)
                                                   100
                                                   #:head-pos 1))))
(sm-showtransitions anbncn `(,LM ,BLANK a b c) 1)
|#
;(tm-viz EVEN-AS-&-BS '(@ a b a b) 0)
"fix informative messages"