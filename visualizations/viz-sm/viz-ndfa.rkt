#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         2htdp/image
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "david-imsg-state.rkt"
         "david-viz-constants.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/misc.rkt")

(provide ndfa-viz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rules) #:transparent)

#|
A rule is a structure:
(make-rule triple)
triple is the entire of the ndfa rule
|#
(struct rule (triple) #:transparent)

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond [(empty? prev-path) (cons curr-path acc)]
        [(empty? curr-path) prev-path]
        [(equal? (first prev-path) (first curr-path))
         (remove-similarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similarities (rest prev-path) (rest curr-path) (cons (first curr-path) acc))]))

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


(struct computation (LoC LoR visited) #:transparent)


;;config rule -> config
;;Purpose: Applies the given rule to the given config
;;ASSUMPTION: the given rule is applicable to the given config
(define (apply-rule config rule)
  (let* ([new-config (list (third rule)
                           (if (eq? (second rule) EMP)
                               (second (first (computation-LoC config)))
                               (rest (second (first (computation-LoC config))))))])
    (struct-copy computation config
                 [LoC (cons new-config (computation-LoC config))]
                 [LoR (cons rule (computation-LoR config))]
                 [visited (cons (first (computation-LoC config)) (computation-visited config))])))



;;(listof symbol) (listof rule) symbol -> (listof computation)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (trace-computations word lor start)
  (let (;;configuration
        ;;Purpose: The starting configuration
        [starting-config (computation (list (append (list start) (list word)))
                                      '()
                                      '())])
    (make-computations lor
                       (enqueue (list starting-config) E-QUEUE)
                       '())))


;;(listof rule) -> (listof computation)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (make-computations lor QoC path)
  (if (qempty? QoC)
      path
      (let* ([first-computation (first (computation-LoC (qfirst QoC)))]

             ;;(listof rules)
             ;;Purpose: Returns all rules that consume a letter using the given configurations
             [connected-read-rules (if (empty? (second first-computation))
                                       '()
                                       (filter (λ (rule)
                                                 (and (equal? (first rule) (first first-computation))
                                                      (equal? (second rule) (first (second first-computation)))))
                                               lor))]
             ;;(listof rules)
             ;;Purpose: Returns all rules that have an empty transition using the given configurations
             [connected-emp-rules
              (filter (λ (rule)
                        (and (equal? (first rule) (first first-computation))
                             (equal? (second rule) EMP)))
                      lor)]
             ;;(listof configurations)
             ;;Purpose: Makes new configurations using given word and connected-rules
             [new-configs (filter (λ (new-c)
                                    (not (member? (first (computation-LoC new-c)) (computation-visited new-c))))
                                  (map (λ (rule) (apply-rule (qfirst QoC) rule)) (append connected-read-rules connected-emp-rules)))])
        (if  (empty? new-configs)
             (make-computations lor (dequeue QoC) (cons (qfirst QoC) path))
             (make-computations lor (enqueue new-configs (dequeue QoC)) path)))))
  

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first rules)) EMP)))
         (let* ([rle (rule (list EMP EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-trace (rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (equal? (second (first rules)) EMP))
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
                     (trace-computations a-word (fsa-getrules M) (fsa-getstart M))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
        [a-word]))

;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word configs)
 (map (λ (config) (make-inv-configs-helper a-word config (length a-word))) configs))

;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs-helper a-word configs word-len)
  (let* ([config (filter (λ (config) (= (length (second config)) word-len)) configs)]
         [inv-config (map (λ (config)
                            (append (list (first config))
                                    (list (take a-word (- (length a-word) word-len)))))
                          config)])
    (if (empty? configs)
        '()
        (append inv-config
                (make-inv-configs-helper a-word (rest configs) (sub1 word-len))))))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
(define (get-inv-config-results inv-configs invs)
  (if (or (empty? inv-configs)
          (empty? invs))
      '()
      (append (list (get-inv-config-results-helper (first inv-configs) invs))
              (get-inv-config-results (rest inv-configs) invs))))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
(define (get-inv-config-results-helper inv-configs invs)
  (if (empty? inv-configs)
      '()
      (let* ([get-inv-for-inv-config (filter (λ (inv)
                                               (equal? (first inv) (first (first inv-configs))))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (append (first inv-configs)
                                                  (list (inv-for-inv-config (second (first inv-configs)))))))])
        (append inv-config-result
                (get-inv-config-results-helper (rest inv-configs) invs)))))

;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results a-word)
  (if (empty? inv-config-results)
      '()
      (return-brk-inv-configs-helper inv-config-results a-word (length a-word) '())))

;;(listof configurations) (listof sybmols) natnum (listof configurations) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
;;Acc = all invariants that fail when a given portion of the word has been consumed
(define (return-brk-inv-configs-helper inv-config-results a-word word-len acc)
  (if (< word-len 0)
      (filter (λ (res) (not (empty? res))) (reverse acc)) ;;might remove if not can index using the length of the wht was processed
      (let* ([new-acc (append-map (λ (inv-configs)
                                    (filter (λ (config)
                                              (and (equal? (second config) (take a-word (- (length a-word) word-len)))
                                                   (not (third config))))
                                            inv-configs))
                                  inv-config-results)])
        (return-brk-inv-configs-helper inv-config-results a-word (sub1 word-len) (cons new-acc acc)))))

;;rule symbol (listof rules) -> boolean
;;Purpose: Determines if the given rule is a member of the given (listof rules)
;;         or similiar to one of the rules in the given (listof rules) 
(define (find-rule? rule dead lor)
  (or (member? rule lor)
      (ormap (λ (p)
               (and (equal? (first rule) (first p))
                    (or (equal? (third rule) (third p))
                        (and (equal? (third rule) (third p))
                             (equal? (third rule) dead)))))
             lor)))

;;(listof symbols) (listof configurations) -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                          (equal? (second configs) word))
                        config))
              full-configs))


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

;;(listof trace-rule) -> (listof rules)
;;Purpose: Remakes the rule extracted from the rule-struct
(define (extract-rules trace-rules)
  (map (λ (rule)
         (rule-triple rule))
       trace-rules))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graph machine (listof symbols) symbol (listof symbols) (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M dead held-inv brkn-inv)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color (if (eq? state (fsa-getstart M)) 'green 'black)
                                 'style (cond [(or (member? state held-inv) (member? state brkn-inv)) 'filled]
                                              [(eq? state dead) 'dashed]
                                              [else 'solid])
                                 'shape (if (member? state (fsa-getfinals M)) 'doublecircle 'circle)
                                 'fillcolor (cond [(member? state held-inv) HELD-INV-COLOR]
                                                  [(member? state brkn-inv) BRKN-INV-COLOR]
                                                  [else 'white])
                                 'label state
                                 'fontcolor 'black)))
         cgraph
         (fsa-getstates M)))

;; graph machine word (listof rules) (listof rules) symbol -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M current-a-rules current-rules dead)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (cond [(find-rule? rule dead current-a-rules) 'green]
                                              [(find-rule? rule dead current-rules) 'violetred]
                                              [else 'black])
                                 'fontsize 20
                                 'style (cond [(equal? (third rule) dead) 'dashed]
                                              [(find-rule? rule dead current-a-rules) 'bold]
                                              [else 'solid]))))
         cgraph
         (fsa-getrules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;upci is the unprocessed consumed input (listof symbols)
;;pci is the proccessed consumed input (listof symbols)
;;M is a machine
;;inv is a the (listof (state (listof symbols -> boolean)))
;;dead is the sybmol of dead state
(struct building-viz-state (upci pci M inv dead computations acc-comps accept-traces reject-traces))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                                   (trace-computations (imsg-state-pci imsg-st)
                                                (fsa-getrules (imsg-state-M imsg-st))
                                                (fsa-getstart (imsg-state-M imsg-st))))]
         
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed (imsg-state-pci imsg-st) (imsg-state-M imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (drop entire-word (length last-consumed-word))]
         [machine-decision (if (not (zipper-empty? (imsg-state-acpt-trace imsg-st)))
                               'accept
                               'reject)]) 

   
    (overlay/align
     'left 'middle
     (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaa" 20 'white)
                      (text "Word: " 20 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) 20 'gray)
                          (text (format "~a" EMP) 20 'red)))
              (beside (text "Consumed: " 20 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) 20 'black)
                          (text (format "~a" EMP) 20 'white))))]
            [(and (not (empty? (imsg-state-pci imsg-st))) (not completed-config?))
             (above/align
              'left
              (beside (text "aaaa" 20 'white)
                      (text "Word: " 20 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length last-consumed-word) 'gray)
                                               (list (length last-consumed-word) 'red)))))
              (beside (text "Consumed: " 20 'black)
                      (if (empty? last-consumed-word)
                          (text "" 20 'black)
                          (make-tape-img last-consumed-word
                                         (if (> (length last-consumed-word) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaa" 20 'white)
                                       (text "Word: " 20 'black)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " 20 'black)
                                       (make-tape-img (imsg-state-pci imsg-st)
                                                      (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (list-ref (imsg-state-comps-len imsg-st)
                                               (length (imsg-state-pci imsg-st)))))
             20
             'brown)
      (cond [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." 20 'red)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." 20 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'reject))
              (text "All computations end in a non-final state and the machine rejects." 20 'red)]
             [else (text "Word Status: accept " 20 'white)])
      )
     (rectangle 1250 50 'solid 'white))))


;;viz-state -> (list graph-thunk computation-length)
;;Purpose: Creates a graph thunk and finds the associated computation's length for a given viz-state
(define (create-graph-thunk a-vs)
  (let* (;;(listof configurations)
         ;;Purpose: Returns all configurations using the given word
         [all-configs (get-portion-configs (building-viz-state-upci a-vs)
                                           (map computation-LoC (building-viz-state-acc-comps a-vs)))]

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
         [dummy-rule (list EMP EMP EMP)]
         
         ;;(listof rules)
         ;;Purpose: Reconstructs the rules from rule-structs
         [current-a-rules (filter (λ (rule) (and (not (equal? rule dummy-rule)) rule))
                                  (append-map extract-rules a-configs))]
     
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         [get-invs (for*/list ([invs (building-viz-state-inv a-vs)]
                                 [curr all-configs]
                                 #:when (equal? (first invs) (first curr)))
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
           current-a-rules
           current-rules
           (building-viz-state-dead a-vs))))

;;viz-state (listof graph-thunks) -> (listof graph-thunks)
;;Purpose: Creates all the graphs needed for the visualization
(define (create-graph-thunks a-vs acc)
  (cond [(empty? (building-viz-state-upci a-vs)) (reverse (cons (create-graph-thunk a-vs) acc))]
        [(not (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                     (trace-computations (building-viz-state-pci a-vs)
                                  (fsa-getrules (building-viz-state-M a-vs))
                                  (fsa-getstart (building-viz-state-M a-vs)))))
         (reverse (cons (create-graph-thunk a-vs) acc))]
        [else (let ([next-graph (create-graph-thunk a-vs)])
                (create-graph-thunks (struct-copy building-viz-state
                                                  a-vs
                                                  [upci (rest (building-viz-state-upci a-vs))]
                                                  [pci (append (building-viz-state-pci a-vs)
                                                               (list (first (building-viz-state-upci a-vs))))]
                                                  [accept-traces (get-next-traces (building-viz-state-accept-traces a-vs))]
                                                  [reject-traces (get-next-traces (building-viz-state-reject-traces a-vs))])
                                     (cons next-graph acc)))]))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization forward by one step
(define (right-key-pressed a-vs)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config)
                                     (empty? (second (first (computation-LoC config)))))
                                   (trace-computations (imsg-state-pci (informative-messages-component-state
                                                                 (viz-state-informative-messages a-vs))) 
                                                (fsa-getrules (imsg-state-M (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))
                                                (fsa-getstart (imsg-state-M (informative-messages-component-state
                                                                         (viz-state-informative-messages a-vs))))))]
         [pci (if (or (empty? (imsg-state-upci (informative-messages-component-state
                                                (viz-state-informative-messages a-vs))))
                      (not completed-config?))
                  (imsg-state-pci (informative-messages-component-state
                                   (viz-state-informative-messages a-vs)))
                  (append (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs)))
                          (list (first (imsg-state-upci (informative-messages-component-state
                                                         (viz-state-informative-messages
                                                          a-vs)))))))]
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
                                   (not completed-config?))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (rest (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
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
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [full-word (append (imsg-state-pci (informative-messages-component-state
                                             (viz-state-informative-messages a-vs)))
                            (imsg-state-upci (informative-messages-component-state
                                              (viz-state-informative-messages a-vs))))]
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed
                              full-word
                              (imsg-state-M (informative-messages-component-state
                                             (viz-state-informative-messages a-vs))))]
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (drop full-word (length last-consumed-word))]
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
                     [(not (equal? last-consumed-word full-word))
                      (rest unconsumed-word)]
                     [else '()])]
         [pci (cond [(empty? (imsg-state-upci (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))
                     (imsg-state-pci (informative-messages-component-state
                                      (viz-state-informative-messages a-vs)))]
                    [(not (equal? last-consumed-word full-word))
                     (append last-consumed-word (take unconsumed-word 1))]
                    [else full-word])]
         [invs-zipper zip])])])))

;;viz-state -> viz-state
;;Purpose: Progresses the visualization backward by one step
(define (left-key-pressed a-vs)
  (let* ([pci (if (empty? (imsg-state-pci (informative-messages-component-state
                                           (viz-state-informative-messages a-vs))))
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
                     [upci (if (empty? (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (imsg-state-upci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                               (cons (last (imsg-state-pci (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                                     (imsg-state-upci (informative-messages-component-state
                                                       (viz-state-informative-messages a-vs)))))]
                     [pci pci]
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
                   [invs-zipper (if (zipper-empty? (imsg-state-invs-zipper (informative-messages-component-state
                                                                    (viz-state-informative-messages a-vs))))
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
                    (struct-copy imsg-state a-imsgs [word-img-offset 0] [scroll-accum 0])])])))

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
                                             [word-img-offset
                                              (imsg-state-word-img-offset-cap a-imsgs)])])])))

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
             [full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             [partial-word (take full-word (zipper-current zip))])
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
                         [upci (drop full-word (zipper-current zip))]
                         [pci partial-word]
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
                           (>= (length (imsg-state-pci (informative-messages-component-state
                                                        (viz-state-informative-messages a-vs))))
                               (zipper-current (imsg-state-invs-zipper (informative-messages-component-state
                                                                        (viz-state-informative-messages a-vs))))))
                      (zipper-next (imsg-state-invs-zipper (informative-messages-component-state
                                                            (viz-state-informative-messages a-vs))))
                      (imsg-state-invs-zipper (informative-messages-component-state
                                               (viz-state-informative-messages a-vs))))]
             [full-word (append (imsg-state-pci (informative-messages-component-state
                                                 (viz-state-informative-messages a-vs)))
                                (imsg-state-upci (informative-messages-component-state
                                                  (viz-state-informative-messages a-vs))))]
             [partial-word (take full-word (zipper-current zip))])
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
                         [upci (drop full-word (zipper-current zip))]
                         [pci partial-word]
                         [invs-zipper zip])])]))))

;;machine -> machine
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (cond
    [(eq? '(M whatami) 'ndfa)
     (local
       [;;symbol
        ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
        ;;         otherwise uses DEAD
        (define dead (if (member? DEAD (fsa-getstates M)) (gen-state (fsa-getstates M)) DEAD))
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
                                                (not (member? rule partial-rules)))
                                              new-rules)))
        ;;(listof rules)
        ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
        (define rules-to-dead
          (map (λ (rule) (append rule (list dead)))
               get-rules-not-in-M))]
       (make-unchecked-ndfa (append (fsa-getstates M) (list dead))
                  (fsa-getalphabet M)
                  (fsa-getstart M)
                  (fsa-getfinals M)
                  (append (fsa-getrules M) rules-to-dead dead-rules)))]
    [(and (eq? '(M whatami) 'dfa) (not (member? DEAD (fsa-getstates M))))
     (make-unchecked-dfa (fsa-getstates M) (fsa-getalphabet M) (fsa-getstart M) (fsa-getfinals M) (fsa-getrules M))]
    [else M]))



;;ndfa word [boolean] . -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (ndfa-viz M a-word #:add-dead [add-dead #f] invs)
  (let* (;;M ;;Purpose: A new machine with the dead state if add-dead is true
             [new-M (if add-dead (make-new-M M) M)]
             ;;symbol ;;Purpose: The name of the dead state
             [dead-state (cond [(and add-dead (eq? '(M whatami) 'ndfa)) (last (fsa-getstates new-M))]
                               [(and add-dead (eq? '(M whatami) 'dfa)) DEAD]
                               [else 'no-dead])]
             ;;(listof computations) ;;Purpose: All computations that the machine can have
             [computations (trace-computations a-word (fsa-getrules new-M) (fsa-getstart new-M))]
             ;;(listof configurations) ;;Purpose: Extracts the configurations from the computation
             [LoC (map computation-LoC computations)]
             ;;(listof computation) ;;Purpose: Extracts all accepting computations
             [accepting-computations (filter (λ (comp)
                                       (and (member? (first (first (computation-LoC comp))) (fsa-getfinals new-M))
                                            (empty? (second (first (computation-LoC comp))))))
                                     computations)]
             ;;(listof trace) ;;Purpose: Makes traces from the accepting computations
             [accepting-traces (map (λ (acc-comp)
                                      (make-trace (reverse (computation-LoC acc-comp))
                                                  (reverse (computation-LoR acc-comp))
                                                  '()))
                                    accepting-computations)]
             ;;(listof computation) ;;Purpose: Extracts all rejecting computations
             [rejecting-computations (filter (λ (config)
                                               (not (member? config accepting-computations)))
                                             computations)]
             ;;(listof trace) ;;Purpose: Makes traces from the rejecting computations
             [rejecting-traces (map (λ (rej-comp)
                                      (make-trace (reverse (computation-LoC rej-comp))
                                                  (reverse (computation-LoR rej-comp))
                                                  '()))
                                    rejecting-computations)]
             ;;building-state struct
             [building-state (building-viz-state a-word
                                                 '()
                                                 new-M
                                                 (if (and add-dead (not (empty? invs))) (cons (list dead-state (λ (w) #t)) invs) invs) 
                                                 dead-state
                                                 (map reverse LoC)
                                                 accepting-computations
                                                 accepting-traces
                                                 rejecting-traces)]
             ;;(listof graph-thunk) ;;Purpose: Gets all the graphs needed to run the viz
             [graphs (create-graph-thunks building-state '())]
             ;;(listof number) ;;Purpose: Gets the number of computations for each step
             [computation-lens (count-computations a-word (map computation-LoC computations) '())]
             ;;(listof number) ;;Purpose: Gets the index of image where an invariant failed
             [inv-configs (map (λ (con)
                                 (length (second (first con))))
                               (return-brk-inv-configs
                                (get-inv-config-results
                                 (make-inv-configs a-word
                                                   (map (λ (comp)
                                                          (reverse (computation-LoC comp)))
                                                        accepting-computations))
                                 invs)
                                a-word))])
        (run-viz graphs
                 (lambda () (graph->bitmap (first graphs)))
                 (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
                 DEFAULT-ZOOM
                 DEFAULT-ZOOM-CAP
                 DEFAULT-ZOOM-FLOOR
                 (informative-messages create-draw-informative-message
                                       (imsg-state new-M
                                                   a-word
                                                   '()
                                                   (list->zipper accepting-traces)
                                                   'no-stck
                                                   'no-farthest-consumed
                                                   (list->zipper inv-configs)
                                                   (sub1 (length inv-configs))
                                                   computation-lens
                                                   LoC
                                                   'no-max-cmps
                                                   0
                                                   (let ([offset-cap (- (length a-word) TAPE-SIZE)])
                                                     (if (> 0 offset-cap) 0 offset-cap))
                                                   0)
                                       RULE-YIELD-DIMS)
                 (instructions-graphic E-SCENE-TOOLS
                                       (bounding-limits 0
                                                        (image-width E-SCENE-TOOLS)
                                                        (+ EXTRA-HEIGHT-FROM-CURSOR
                                                           E-SCENE-HEIGHT
                                                           (bounding-limits-height RULE-YIELD-DIMS)
                                                           INS-TOOLS-BUFFER)
                                                        (+ EXTRA-HEIGHT-FROM-CURSOR
                                                           E-SCENE-HEIGHT
                                                           (bounding-limits-height RULE-YIELD-DIMS)
                                                           INS-TOOLS-BUFFER
                                                           (image-height ARROW-UP-KEY))))
                 (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
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
                                         [ "j" jump-prev j-key-pressed]
                                         [ "l" jump-next l-key-pressed]
                                         )
                 (create-viz-process-tick E-SCENE-BOUNDING-LIMITS
                                          NODE-SIZE
                                          E-SCENE-WIDTH
                                          E-SCENE-HEIGHT
                                          CLICK-BUFFER-SECONDS
                                          ([RULE-YIELD-DIMS
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
                                            [ J-KEY-DIMS jump-prev j-key-pressed]
                                            [ L-KEY-DIMS jump-next l-key-pressed])
                                          )
                 (if (eq? (M 'whatami) 'ndfa)
                          'ndfa-viz
                          'dfa-viz))))

;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
