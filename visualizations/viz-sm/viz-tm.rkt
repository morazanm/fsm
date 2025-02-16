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
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE)
         "default-informative-messages.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(struct imsg-state-tm (M ;;TM
                         tape ;;(zipperof symbol)
                         head-position ;;natnum
                         rules ;;(zipperof tm-rule)
                         shown-accepting-trace ;;(zipperof trace)
                         invs-zipper ;;(zipperof inv-configs)
                         inv-amount ;;natnum
                         computation-lengths ;(zipperof natnum)
                         computations ;;(listof computation)
                         max-cmps ;;natnum
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum)
    #:transparent)


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
                                                          (equal? (first (first rule))
                                                                  (first (first (computation-LoC (qfirst QoC)))))
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
              (not (equal? (second (first (first rules))) BLANK)))
         (let* ([rle (rule (first DUMMY-RULE) (second DUMMY-RULE))]
                [res (trace (first configs) rle)])
           (make-trace (rest configs) rules (cons res acc)))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) rle)])
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
  (map (λ (rule)
         (list (rule-read rule)
               (rule-action rule)))
       trace-rules)
  #;(append-map (λ (lor)
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
(define (configs->rules a-config)
  (make-rule-triples
   (remove-duplicates (filter (λ (rule)
                                (not (equal? rule DUMMY-RULE)))
                              (remake-rules a-config)))))

;;(listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Counts the number of unique configurations for each stage of the word
(define (count-computations a-LoC acc)
  (if (empty? a-LoC)
      (reverse acc)
      (let [(new-LoC (filter-map (λ (comp) (and (not (empty? comp))
                                                (rest comp)))
                                 a-LoC))
            (comp-len (length (remove-duplicates (filter-map (λ (comp) (and (not (empty? comp))
                                                                            (first comp)))
                                                             a-LoC))))]
        (count-computations new-LoC (cons comp-len acc)))))

;;(listof trace) (listof trace) -> (listof trace)
;;Purpose: Finds the longest computation for rejecting traces
(define (find-longest-computation a-LoRT acc)
  (cond [(empty? a-LoRT) acc]
        [(> (length (first a-LoRT)) (length acc))
         (find-longest-computation (rest a-LoRT) (first a-LoRT))]
        [(find-longest-computation (rest a-LoRT) acc)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;graph machine -> graph
;;Purpose: Creates the nodes for the given graph
(define (make-node-graph dgraph M held-inv fail-inv cut-off)
  (foldl (λ (state graph)
           (add-node graph
                     state
                     #:atb (hash 'color (if (eq? (tm-getstart M) state) 'green 'black)
                                 'style (cond [(and (member? state held-inv equal?) (member? state fail-inv equal?)) 'wedged]
                                              [(or (member? state held-inv equal?)
                                                   (member? state fail-inv equal?)
                                                   (member? state cut-off equal?)) 'filled]
                                              [else 'solid])
                                 'shape (cond [(equal? state (tm-getaccept M)) 'doubleoctagon]
                                              [(member? state (tm-getfinals M) equal?) 'doublecircle]
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
         (tm-getstates M)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;viz-state -> graph-thunk
;;Purpose: Creates a graph thunk for a given viz-state
(define (create-graph-thunk a-vs #:cut-off [cut-off #f])
  (let* (;;(listof configuration)
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
                                                 (not (equal? (second (first rule)) LM)))
                                               (tm-getrules (building-viz-state-M a-vs))))]
         
         ;;(listof (listof symbol ((listof symbols) (listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                               [curr current-configs]
                               #:when (equal? (first invs) (first curr)))
                     (list invs '()  (third curr)))]

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
  (cond [(and (zipper-at-end? (building-viz-state-tape a-vs))
              (zipper-at-end? (building-viz-state-head-pos a-vs)))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [(ormap (λ (comp-len) (>= comp-len (building-viz-state-max-cmps a-vs)))
                (map length (building-viz-state-computations a-vs)))
         (reverse (cons (create-graph-thunk a-vs #:cut-off #t) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  [tape (zipper-next (building-viz-state-tape a-vs))]
                                                  [head-pos (zipper-next (building-viz-state-head-pos a-vs))]
                                                  [tracked-accept-trace
                                                   (get-next-traces (building-viz-state-tracked-accept-trace a-vs))]
                                                  [all-accept-traces (get-next-traces (building-viz-state-all-accept-traces a-vs))]
                                                  [all-reject-traces (get-next-traces (building-viz-state-all-reject-traces a-vs))])
                                     (cons next-graph acc)))]))



;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* ([pci-len 0])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-tm 
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [rules (if (or (zipper-empty? (imsg-state-tm-rules (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                    (zipper-at-end? (imsg-state-tm-rules (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))
                                (imsg-state-tm-rules (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))) 
                                (zipper-next (imsg-state-tm-rules (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))))]
                     
                     [tape (if (or (zipper-empty? (imsg-state-tm-tape (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
                                   (zipper-at-end? (imsg-state-tm-tape (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))
                               (imsg-state-tm-tape (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))) 
                               (zipper-next (imsg-state-tm-tape (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))))]
                     
                     [head-position (if (or (zipper-empty? (imsg-state-tm-head-position
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                            (zipper-at-end? (imsg-state-tm-head-position
                                                             (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))
                                        (imsg-state-tm-head-position (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))
                                        (zipper-next (imsg-state-tm-head-position
                                                      (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     
                     [computation-lengths (if (or (zipper-empty? (imsg-state-tm-computation-lengths
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
                                                  (zipper-at-end? (imsg-state-tm-computation-lengths
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))
                                              (imsg-state-tm-computation-lengths
                                               (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))) 
                                              (zipper-next (imsg-state-tm-computation-lengths
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))]
                     
                     [shown-accepting-trace (if (or (zipper-empty? (imsg-state-tm-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (zipper-at-end? (imsg-state-tm-shown-accepting-trace
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))
                                                (imsg-state-tm-shown-accepting-trace
                                                 (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))
                                                (zipper-next (imsg-state-tm-shown-accepting-trace
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
                     
                     [invs-zipper (cond [(zipper-empty? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs))))
                                         (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-end? (imsg-state-tm-invs-zipper
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))
                                              (>= pci-len (first (zipper-unprocessed
                                                                  (imsg-state-tm-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))))
                                         (zipper-next (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))])])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization to the end
(define (down-key-pressed a-vs)
  (let* (;;(zipperof invariant)
         ;;Purpose: The index of the last failed invariant
         [zip (if (zipper-empty? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                  (imsg-state-tm-invs-zipper (informative-messages-component-state
                                              (viz-state-informative-messages a-vs)))
                  (zipper-to-idx (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))
                                 (imsg-state-tm-inv-amount (informative-messages-component-state
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
         imsg-state-tm
         (informative-messages-component-state
          (viz-state-informative-messages a-vs))
         [rules (if (or (zipper-empty? (imsg-state-tm-rules (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                        (zipper-at-end? (imsg-state-tm-rules (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs)))))
                    (imsg-state-tm-rules (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))) 
                    (zipper-to-end (imsg-state-tm-rules (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs)))))]
         [tape (if (or (zipper-empty? (imsg-state-tm-tape (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
                       (zipper-at-end? (imsg-state-tm-tape (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs)))))
                   (imsg-state-tm-tape (informative-messages-component-state
                                        (viz-state-informative-messages a-vs)))
                   (zipper-to-end (imsg-state-tm-tape (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
         [head-position (if (or (zipper-empty? (imsg-state-tm-head-position (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs))))
                                (zipper-at-end? (imsg-state-tm-head-position (informative-messages-component-state
                                                                              (viz-state-informative-messages a-vs)))))
                            (imsg-state-tm-head-position (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))
                            (zipper-to-end (imsg-state-tm-head-position (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs)))))]
         
         [computation-lengths (if (or (zipper-empty? (imsg-state-tm-computation-lengths
                                                      (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs))))
                                      (zipper-at-end? (imsg-state-tm-computation-lengths
                                                       (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs)))))
                                  (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs))) 
                                  (zipper-to-end (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))))]
         [shown-accepting-trace (if (or (zipper-empty? (imsg-state-tm-shown-accepting-trace
                                                        (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))
                                        (zipper-at-end? (imsg-state-tm-shown-accepting-trace
                                                         (informative-messages-component-state
                                                          (viz-state-informative-messages a-vs)))))
                                    (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))
                                    (zipper-to-end (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                         (viz-state-informative-messages a-vs)))))]
         
         
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([pci-len 0])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (struct-copy imsg-state-tm
                     (informative-messages-component-state
                      (viz-state-informative-messages a-vs))
                     [rules (if (or (zipper-empty? (imsg-state-tm-rules (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                    (zipper-at-begin? (imsg-state-tm-rules (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))
                                (imsg-state-tm-rules (informative-messages-component-state
                                                      (viz-state-informative-messages a-vs))) 
                                (zipper-prev (imsg-state-tm-rules (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))))]
                     [tape (if (or (zipper-empty? (imsg-state-tm-tape (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
                                   (zipper-at-begin? (imsg-state-tm-tape (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))
                               (imsg-state-tm-tape (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs)))
                               (zipper-prev (imsg-state-tm-tape (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))))]
                     
                     [head-position (if (or (zipper-empty? (imsg-state-tm-head-position
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
                                            (zipper-at-begin? (imsg-state-tm-head-position
                                                               (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))))
                                        (imsg-state-tm-head-position (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))
                                        (zipper-prev (imsg-state-tm-head-position (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs)))))]
                     
                     [computation-lengths (if (or (zipper-empty? (imsg-state-tm-computation-lengths
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
                                                  (zipper-at-begin? (imsg-state-tm-computation-lengths
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))
                                              (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))) 
                                              (zipper-prev (imsg-state-tm-computation-lengths
                                                            (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs)))))]
                     [shown-accepting-trace (if (or (zipper-empty? (imsg-state-tm-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (zipper-at-begin? (imsg-state-tm-shown-accepting-trace
                                                                       (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))
                                                (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))
                                                (zipper-prev (imsg-state-tm-shown-accepting-trace
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
                     [invs-zipper (cond [(zipper-empty? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs))))
                                         (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))]
                                        [(and (not (zipper-at-begin? (imsg-state-tm-invs-zipper
                                                                      (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs)))))
                                              (<= pci-len (first (zipper-processed
                                                                  (imsg-state-tm-invs-zipper
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))))
                                         (zipper-prev (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                                  (viz-state-informative-messages a-vs))))]
                                        [else (imsg-state-tm-invs-zipper (informative-messages-component-state
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
      (struct-copy imsg-state-tm
                   (informative-messages-component-state
                    (viz-state-informative-messages a-vs))
                   [rules (if (or (zipper-empty? (imsg-state-tm-rules (informative-messages-component-state
                                                                       (viz-state-informative-messages a-vs))))
                                  (zipper-at-begin? (imsg-state-tm-rules (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs)))))
                              (imsg-state-tm-rules (informative-messages-component-state
                                                    (viz-state-informative-messages a-vs))) 
                              (zipper-to-begin (imsg-state-tm-rules (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))))]
                   [tape (if (or (zipper-empty? (imsg-state-tm-tape (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                 (zipper-at-begin? (imsg-state-tm-tape (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs)))))
                             (imsg-state-tm-tape (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs)))
                             (zipper-to-begin (imsg-state-tm-tape (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs)))))]
                   [head-position (if (or (zipper-empty? (imsg-state-tm-head-position (informative-messages-component-state
                                                                                       (viz-state-informative-messages a-vs))))
                                          (zipper-at-begin? (imsg-state-tm-head-position (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs)))))
                                      (imsg-state-tm-head-position (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))
                                      (zipper-to-begin (imsg-state-tm-head-position (informative-messages-component-state
                                                                                     (viz-state-informative-messages a-vs)))))]
                   [computation-lengths (if (or (zipper-empty? (imsg-state-tm-computation-lengths
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))))
                                                (zipper-at-begin? (imsg-state-tm-computation-lengths
                                                                   (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs)))))
                                            (imsg-state-tm-computation-lengths (informative-messages-component-state
                                                                                (viz-state-informative-messages a-vs))) 
                                            (zipper-to-begin (imsg-state-tm-computation-lengths
                                                              (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs)))))]
                   [shown-accepting-trace (if (or (zipper-empty? (imsg-state-tm-shown-accepting-trace
                                                                  (informative-messages-component-state
                                                                   (viz-state-informative-messages a-vs))))
                                                  (zipper-at-begin? (imsg-state-tm-shown-accepting-trace
                                                                     (informative-messages-component-state
                                                                      (viz-state-informative-messages a-vs)))))
                                              (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                    (viz-state-informative-messages a-vs)))
                                              (zipper-to-begin (imsg-state-tm-shown-accepting-trace
                                                                (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs)))))]
                   
                   [invs-zipper (if (or (zipper-empty? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                                   (viz-state-informative-messages a-vs))))
                                        (zipper-at-begin? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                                      (viz-state-informative-messages a-vs)))))
                                    (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs)))
                                    (zipper-to-idx (imsg-state-tm-invs-zipper (informative-messages-component-state
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
  (if (or (zipper-empty? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
          (and (zipper-at-begin? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                             (viz-state-informative-messages a-vs))))
               (not (zipper-at-end? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))))
          (< (length (imsg-state-tm-tape (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))))
      a-vs
      (let ([zip (if (and (not (zipper-at-begin? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                             (viz-state-informative-messages a-vs)))))
                          (<= (length (imsg-state-tm-tape (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
                              (zipper-current (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                          (viz-state-informative-messages a-vs))))))
                     (zipper-prev (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                              (viz-state-informative-messages a-vs))))
                     (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs))))])
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state-tm
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         
                         [shown-accepting-trace (if (zipper-empty? (imsg-state-tm-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs)))
                                                    (zipper-to-idx (imsg-state-tm-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))
                                                                   (zipper-current zip)))]
                         
                         
                         [invs-zipper zip])])]))))

;;viz-state -> viz-state
;;Purpose: Jumps to the next failed invariant
(define (l-key-pressed a-vs)
  (if (or (zipper-empty? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                     (viz-state-informative-messages a-vs))))
          (and (zipper-at-end? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                           (viz-state-informative-messages a-vs))))
               (not (zipper-at-begin? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                  (viz-state-informative-messages a-vs))))))
          (> (length (imsg-state-tm-tape (informative-messages-component-state
                                          (viz-state-informative-messages a-vs))))
             (zipper-current (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                         (viz-state-informative-messages a-vs))))))
      a-vs
      (let* ([zip (if (and (not (zipper-at-end? (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                            (viz-state-informative-messages a-vs)))))
                           (or (>= (length (imsg-state-tm-tape (informative-messages-component-state
                                                                (viz-state-informative-messages a-vs))))
                                   (zipper-current (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                                               (viz-state-informative-messages a-vs)))))))
                      (zipper-next (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                               (viz-state-informative-messages a-vs))))
                      (imsg-state-tm-invs-zipper (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))])
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy
           informative-messages
           (viz-state-informative-messages a-vs)
           [component-state
            (struct-copy imsg-state-tm
                         (informative-messages-component-state
                          (viz-state-informative-messages a-vs))
                         
                         [shown-accepting-trace (if (zipper-empty? (imsg-state-tm-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs))))
                                                    (imsg-state-tm-shown-accepting-trace (informative-messages-component-state
                                                                                          (viz-state-informative-messages a-vs)))
                                                    (zipper-to-idx (imsg-state-tm-shown-accepting-trace
                                                                    (informative-messages-component-state
                                                                     (viz-state-informative-messages a-vs)))
                                                                   (zipper-current zip)))]
                         
                         
                         [invs-zipper zip])])]))))


;;tm tape [natnum] [natnum] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (tm-viz M a-word head-pos #:cut-off [cut-off 100] . invs) ;;GET RID OF . FOR TESTING
  (let* (;;(listof computations) ;;Purpose: All computations that the machine can have
         [computations (get-computations a-word (tm-getrules M) (tm-getstart M) (tm-getfinals M) cut-off head-pos)]
         ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
         [LoC (map computation-LoC computations)]
         ;;number ;;Purpose: The length of the word
         [word-len (length a-word)]
         ;;(listof computation) ;;Purpose: Extracts all accepting computations
         [accepting-computations (filter (λ (comp)
                                           (equal? (first (first (computation-LoC comp))) (tm-getaccept M)))
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
         [rejecting-trace (if (empty? accept-cmps) (find-longest-computation rejecting-traces '()) '())]
         [all-tapes (list->zipper (map (λ (trace) (third (trace-config trace)))
                                       (cond [(and (empty? accepting-trace)
                                                   (= (length rejecting-traces) 1))
                                              rejecting-trace]
                                             [(not (empty? accepting-trace)) accepting-trace]
                                             [else '()])
                                       #;(if (empty? accepting-trace)
                                             rejecting-trace
                                             accepting-trace)))]
         [all-head-pos (list->zipper (map (λ (trace) (second (trace-config trace)))
                                          (if (empty? accepting-trace)
                                              rejecting-trace
                                              accepting-trace)))]
         [machine-decision (if (not (empty? accepting-computations))
                               'accept
                               'reject)]

         [tracked-trace (cond [(and (empty? accepting-trace)
                                    (= (length rejecting-computations) 1))
                               (list rejecting-trace)]
                              [(not (empty? accepting-trace)) (list accepting-trace)]
                              [else '()])]
         ;;building-state struct
         [building-state (building-viz-state all-tapes
                                             LoC
                                             tracked-trace
                                             (if (empty? accept-cmps) '() (rest accept-cmps))
                                             rejecting-traces
                                             M
                                             invs
                                             cut-off
                                             all-head-pos
                                             machine-decision)]
                     
         ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
         [graphs (create-graph-thunks building-state '())]
         ;;(listof computation) ;;Purpose: Gets all the cut off computations if
         ;; the length of the word is greater than max computations
         [get-cut-off-comp (if (> word-len cut-off)
                               (map first LoC)
                               '())]
         ;;(listof computation) ;;Purpose: Makes the cut off computations if
         ;;  the length of the word is greater than max computations
         [cut-off-comp (if (empty? get-cut-off-comp)
                           LoC
                           (map (λ (cut-off-comp comp)
                                  (append comp (list cut-off-comp)))
                                get-cut-off-comp
                                LoC))]
         ;;(listof number) ;;Purpose: Gets the number of computations for each step
         [computation-lengths (take (drop (count-computations (map reverse LoC) '()) head-pos) (length (zipper->list all-head-pos)))]
         ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
         #;[inv-configs (map (λ (con)
                               (fourth con))
                             (return-brk-inv-configs
                              (get-inv-config-results
                               (make-inv-configs a-word accepting-computations)
                               invs)
                              a-word))])
    ;(first (map reverse LoC))
    ;rejecting-trace
    ;tracked-trace
    ;(writeln (zipper->list all-head-pos))
    ;(writeln (count-computations (map reverse LoC) '()))
    #;(displayln rejecting-computations)
     (run-viz graphs
             (lambda () (graph->bitmap (first graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ TM-E-SCENE-HEIGHT 2))
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages tm-create-draw-informative-message
                                   (imsg-state-tm M
                                                  all-tapes
                                                  all-head-pos
                                                  (list->zipper (map (λ (trace)
                                                                       (list (rule-read (trace-rules trace))
                                                                             (rule-action (trace-rules trace))))
                                                                     (if (empty? tracked-trace)
                                                                         tracked-trace
                                                                         (first tracked-trace))))
                                                  (list->zipper (if (empty? tracked-trace) tracked-trace (first tracked-trace)))
                                                  (list->zipper '() #;inv-configs) 
                                                  0 ;(sub1 (length inv-configs))
                                                  (list->zipper computation-lengths)
                                                  LoC
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
'(((S 1 (@ _ a b c c))  (J 2 (@ _ a b c c))  (A 3 (@ _ a b c c))  (B 4 (@ _ a b c c))  (C 5 (@ _ a b c c)) (C 6 (@ _ a b c c _))  (D 5 (@ _ a b c c _))  (D 4 (@ _ a b c c _))  (D 3 (@ _ a b c c _))  (D 2 (@ _ a b c c _))  (D 1 (@ _ a b c c _))  (E 2 (@ _ a b c c _))  (F 2 (@ _ x b c c _))  (F 3 (@ _ x b c c _))  (G 3 (@ _ x x c c _))  (G 4 (@ _ x x c c _))  (D 4 (@ _ x x x c _))  (D 3 (@ _ x x x c _))  (D 2 (@ _ x x x c _))  (D 1 (@ _ x x x c _))  (E 2 (@ _ x x x c _))  (E 3 (@ _ x x x c _))  (E 4 (@ _ x x x c _))  (E 5 (@ _ x x x c _)))
  ((S 1 (@ _ a b c c))  (J 2 (@ _ a b c c))  (A 3 (@ _ a b c c))  (B 4 (@ _ a b c c))  (C 5 (@ _ a b c c)) (C 6 (@ _ a b c c _))  (D 5 (@ _ a b c c _))  (D 4 (@ _ a b c c _))  (D 3 (@ _ a b c c _))  (D 2 (@ _ a b c c _))  (D 1 (@ _ a b c c _))  (E 2 (@ _ a b c c _))  (H 2 (@ _ x b c c _))  (H 3 (@ _ x b c c _))  (I 3 (@ _ x x c c _))  (I 4 (@ _ x x c c _))  (K 4 (@ _ x x x c _))  (L 5 (@ _ x x x c _))))
    '(                 1                  1                     1                      1                  1                  1                       1                          1                 1                      1                      1                      1                        2                     2                     2                       2                        2                  2                        1                       1                       1                         1                1                      1                       )
|#


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

(define anbncn (make-unchecked-tm '(S A B C D E F G H I J K L Y)
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
                                                   (tm-getrules a*Ua*b)
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