#lang racket

(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "definitions-viz.rkt"
         "run-viz.rkt"
         "../fsm-core/interface.rkt")

(define FNAME "fsm")

(define HELD-INV-COLOR 'chartreuse4)
(define BRKN-INV-COLOR 'red2)

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

;;symbol (listof rules) -> (listof rules)
;;Purpose: Returns all empty transitions from the start state
(define (get-empties-from-start start lor)
  (append-map (λ (rule)
                (if (and (equal? (first rule)  start)
                         (equal? (second rule) EMP))
                    (append (list rule) (get-empties-from-start (third rule) lor))
                    '()))
              lor))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(empty? curr-path) prev-path]
        [(equal? (first prev-path) (first curr-path))
         (remove-similarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similarities (rest prev-path) (rest curr-path)
                               (append acc (list (first curr-path))))]))

;;(listof rules) (listof rules) -> (listof rules)
;;Purpose: Appends empty transitions to the end of the path if applicable
(define (attach-empties path lor)
  (if (empty? path)
      path
      (let* [(empty-from-path
              (filter (λ (rule)
                        (and (equal? (last path) (first rule))
                             (equal? (second rule) EMP)))
                      lor))]
        (if (empty? empty-from-path)
            (append (list path) empty-from-path)
            (append (list path) (attach-empties (first empty-from-path) lor))))))

;;symbol (listof rules) -> (listof states)
;;Purpose: Returns all states that have an empty transitions from the given start state
(define (get-empty-states-from-start start lor)
  (flatten
   (map (λ (rule)
          (if (and (equal? (first rule)  start)
                   (equal? (second rule) EMP))
              (cons (third rule) (list (get-empty-states-from-start (third rule) lor)))
              '()))
        lor)))

;;(listof symbols) (listof rules) symbol -> (listof configurations)
;;Purpose: Returns all possible configurations from start that consume the given word
(define (get-configs a-word lor start)
  (let* [;;(listof states)
         ;;Purpose: A list of states reachable from the start state on an empty transition
         (empty-states-frm-start (cons start (get-empty-states-from-start start lor)))
         ;;(listof configurations)
         ;;Purpose: All configurations from the start state
         (configs (map (λ (state) (append (list state) (list a-word)))
                       empty-states-frm-start))]
    (get-configs-helper configs a-word lor configs)))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) -> (listof configurations)
;;Purpose: Returns all possible configurations using the given word and  (listof rules)
;;Visited = All configurations of the consumed portion of the given word
(define (get-configs-helper configs a-word lor visited)
  (if (empty? configs)
      '()      
      (append (make-configs (first configs) a-word lor visited (list (first configs)))
              (get-configs-helper (rest configs) a-word lor visited))))

;;configuration (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Explores all possible configurations using the given word, (listof rules), and visited
;;Visited = All configurations of the consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs config a-word lor visited path)
  (let* [;;(listof rules)
         ;;Purpose: Returns all rules that consume a letter using the given configurations
         (connected-rules (if (empty? a-word)
                              '()
                              (filter (λ (rule)
                                        (and (equal? (first rule) (first config))
                                             (equal? (second rule) (first (second config)))))
                                      lor)))
         ;;(listof configurations)
         ;;Purpose: Makes new configurations using given word and connected-rules 
         (new-config (filter (λ (new-config)
                               (not (member? new-config visited)))
                             (map (λ (rule)
                                    (if (empty? a-word)
                                        (append (list (third rule)) (list a-word))
                                        (append (list (third rule)) (list (rest a-word)))))
                                  connected-rules)))
         ;;(listof rules)
         ;;Purpose: Returns all rules that have an empty transition using the given configurations
         (connected-rules-via-emp (filter (λ (rule)
                                            (and (equal? (first rule) (first config))
                                                 (equal? (second rule) EMP)))
                                          lor))
         ;;(listof configurations)
         ;;Purpose: Makes new configurations using the given word and connected-rules-via-emp 
         (new-config-via-emp (filter (λ (new-config)
                                       (not (member? new-config visited)))
                                     (map (λ (rule)
                                            (append (list (third rule)) (list a-word)))
                                          connected-rules-via-emp)))]
    (cond [(and (empty? new-config)
                (empty? new-config-via-emp))
           (list path)] ;;no rules found that connect the config
          [(and (empty? a-word)
                (empty? new-config-via-emp)
                (= (length new-config) 1)) ;;word and new-config-via-emp is empty and there is exactly 1 new-config
           (make-configs (first new-config)
                         a-word
                         lor
                         (append new-config visited)
                         (append path new-config))]
          [(and (empty? a-word)
                (empty? new-config)
                (= (length new-config-via-emp) 1)) ;;word and new-config is empty and there is exactly 1 new-config-via-emp
           (make-configs (first new-config-via-emp)
                         a-word
                         lor
                         (append new-config-via-emp visited)
                         (append path new-config-via-emp))]
          [(and (empty? a-word)
                (empty? new-config-via-emp)) ;;word and new-config-via-emp is empty and there are multiple new-configs
           (make-configs-helper new-config
                                a-word
                                lor
                                (append new-config visited)
                                (append path new-config))]
          [(and (empty? a-word)
                (empty? new-config))  ;;word and new-config is empty and there are multiple new-config-via-emp
           (make-configs-helper new-config-via-emp
                                a-word
                                lor
                                (append new-config visited)
                                (append path new-config))]
          [(and (empty? new-config-via-emp)
                (= (length new-config) 1)) ;;new-config-via-emp is empty and there is exactly 1 new-config
           (make-configs (first new-config)
                         (rest a-word)
                         lor
                         (append new-config visited)
                         (append path new-config))]
          [(and (empty? new-config-via-emp)
                (> (length new-config) 1)) ;;new-config-via-emp is empty and there are multiple new-config
           (make-configs-helper new-config
                                (rest a-word)
                                lor
                                (append new-config visited)
                                (append path new-config))]
          [(and (empty? new-config)
                (= (length new-config-via-emp) 1)) ;;new-config is empty and there is exactly 1 new-config-via-emp
           (make-configs (first new-config-via-emp)
                         a-word
                         lor
                         (append new-config-via-emp visited)
                         (append path new-config-via-emp))]
          [(and (empty? new-config)
                (> (length new-config-via-emp) 1)) ;;new-config is empty and there are multiple new-config-via-emp
           (make-configs-helper new-config-via-emp
                                a-word
                                lor
                                (append path new-config-via-emp)
                                (append path new-config-via-emp))]
          [else (append (make-configs-helper new-config     ;;there is a combination of new-config and new-config-via-emp
                                             (rest a-word)
                                             lor
                                             (append new-config visited)
                                             (append path new-config))
                        (make-configs-helper new-config-via-emp
                                             a-word
                                             lor
                                             (append new-config-via-emp visited)
                                             (append path new-config-via-emp)))])))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Ensures that all possible configurations are explored
;;Visited = All configurations that consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs-helper configs a-word lor visited path)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited path)
              (make-configs-helper (rest configs) a-word lor visited path))))


;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (last (last config))))
                     (get-configs a-word
                                  (sm-rules M)
                                  (sm-start M))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
        [a-word]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graph machine (listof symbols) symbol (listof symbols) (listof symbols) -> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M dead held-inv brkn-inv)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (if (eq? state (sm-start M))
                                   'darkgreen
                                   'black)
                        'style (cond [(or (member? state held-inv)
                                          (member? state brkn-inv))
                                      'filled]
                                     [(eq? state dead) 'dashed]
                                     [else 'solid])
                        'shape (if (member? state (sm-finals M))
                                   'doublecircle
                                   'circle)
                        'fillcolor (cond [(member? state held-inv) HELD-INV-COLOR]
                                         [(member? state brkn-inv) BRKN-INV-COLOR]
                                         [else 'white])
                        'label state
                        'fontcolor 'black)))
         cgraph
         (sm-states M)))

;; graph machine word (listof rules) (listof rules) symbol -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M current-rules dead)
  (foldl (λ (rule result) 
           (add-edge result
                     (second rule)
                     (first rule)
                     (third rule)
                     #:atb (hash 'color (if (or (member? rule current-rules)
                                                (ormap (λ (p)
                                                         (and (equal? (first rule) (first p))
                                                              (or (equal? (third rule) (third p))
                                                                  (and (equal? (third rule) (third p))
                                                                       (equal? (third rule) dead)))))
                                                       current-rules))
                                            'violetred
                                            'black)        
                                 'fontsize 20
                                 'style (cond [(equal? (third rule) dead) 'dashed]
                                              [(or (member? rule current-rules)
                                                   (ormap (λ (p)
                                                            (and (equal? (first rule) (first p))
                                                                 (equal? (third rule) (third p))))
                                                          current-rules))
                                               'bold]
                                              [else 'solid]))))
         cgraph
         (sm-rules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;upci is the unprocessed consumed input (listof symbols)
;;pci is the proccessed consumed input (listof symbols)
;;M is a machine
;;inv is a the (listof (state (listof symbols -> boolean)))
;;dead is the sybmol of dead state
(struct viz-state-ndfa (upci pci M inv dead))

(define E-SCENE (empty-scene 1250 600))
(define E-SCENE-TOOLS (overlay (beside (above (above (triangle  30    'solid 'black)
                                                     (rectangle 10 30 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Restart the visualization" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rectangle 30 10         'solid 'black)
                                                      (rotate 270 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Move one step forward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rotate 90 (triangle  30    'solid 'black))
                                                      (rectangle 30 10 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Move one step backward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (above (rectangle 10 30             'solid 'black)
                                                     (rotate 180 (triangle  30    'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Complete the visualization" 18 'black)))
                               (empty-scene 1250 100)))
;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (los2str los)
  (define (helper los)
    (if (empty? (rest los))
        (string-append (symbol->string (first los)) " ")
        (string-append (symbol->string (first los))
                       " "
                       (helper (rest los)))))
  (helper los))

;; image int int -> image
;; PurpScales a image to the given dimentions 
(define (resize-image img max-width max-height)
  (let* [(src-width (image-width img))
         (src-height (image-height img))
         (aspect (/ src-width src-height))
         (scale (min
                 (/ max-width src-width)
                 (/ max-height src-height)))

         (scaled-width (* src-width scale))
         (scaled-height (* src-height scale))]
    (cond [(and (> scaled-width max-width)
                (<= scaled-height max-height))
           (list (scale/xy
                  (/ max-width src-width)
                  (/ (/ scaled-width aspect) src-height)
                  img)
                 (/ max-width src-width)
                 (/ (/ scaled-width aspect) src-height))]
          [(and (<= scaled-width max-width)
                (> scaled-height max-height))
           (let [(scaled-aspect (/ scaled-width scaled-height))]
             (list (scale/xy
                    (/ (* scaled-height scaled-aspect) src-width)
                    (/ max-height src-height)
                    img)
                   (/ (* scaled-height scaled-aspect) src-width)
                   (/ max-height src-height)))]
          [(and (> scaled-width max-width)
                (> scaled-height max-height))
           (let* [(new-scaled-height (/ max-width aspect))
                  (scaled-aspect (/ max-width new-scaled-height))] 
             (list (scale/xy
                    (/ (* max-height scaled-aspect) src-width)
                    (/ max-height src-height)
                    img)
                   (/ (* max-height scaled-aspect) src-width)
                   (/ max-height src-height)))]
          [(and (<= scaled-width max-width)
                (<= scaled-height max-height))
           (list (scale/xy
                  (/ scaled-width src-width)
                  (/ scaled-height src-height)
                  img)
                 (/ scaled-width src-width)
                 (/ scaled-height src-height))])))

;;viz-state -> scene
;;Purpose: Draws the given viz-state onto the scene
(define (draw-graph a-vs)
  (let* [ ;;for reference, a config(computation) is a pair(list) with a state as the first element and a word as the second
         ;;EX. (S (a b a))
         ;;(listof configurations)
         ;;Purpose: Returns all starting configurations of for the entire word
         (starting-configs (map (λ (state) (append (list state) (list (append (viz-state-ndfa-pci a-vs)
                                                                              (viz-state-ndfa-upci a-vs)))))
                                (cons (sm-start (viz-state-ndfa-M a-vs))
                                      (get-empty-states-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                                   (sm-rules (viz-state-ndfa-M a-vs))))))
       
         ;;(listof configurations)
         ;;Purpose: Returns all configurations using the CI
         (curr-config (append-map (λ (config)
                                    (filter (λ (configs)
                                              (equal? (second configs) (viz-state-ndfa-upci a-vs)))
                                            config))
                                  (get-configs (append (viz-state-ndfa-pci a-vs)
                                                       (viz-state-ndfa-upci a-vs))
                                               (sm-rules (viz-state-ndfa-M a-vs))
                                               (sm-start (viz-state-ndfa-M a-vs)))))
         
         
         ;;(listof configurations)
         ;;Purpose: Returns the all configurations using the CI that is one step behind
         (prev-config (cond [(empty? (viz-state-ndfa-pci a-vs)) '()]
                            [(= (length (viz-state-ndfa-pci a-vs)) 1)
                             starting-configs]
                            [else (append-map (λ (config)
                                                (filter (λ (configs)
                                                          (equal? (second configs)
                                                                  (cons (last (viz-state-ndfa-pci a-vs))
                                                                        (viz-state-ndfa-upci a-vs))))
                                                        config))
                                              (get-configs (append (viz-state-ndfa-pci a-vs)
                                                                   (viz-state-ndfa-upci a-vs))
                                                           (sm-rules (viz-state-ndfa-M a-vs))
                                                           (sm-start (viz-state-ndfa-M a-vs))))]))
                             
         ;;(listof rules)
         ;;Purpose: The current rules that the ndfa is using to consume the last of the CI
         ;;How: By checking every possible combintion, compares each state in the previous
         ;;     config with the first of the rule,  each state in the current config with
         ;;     the third of the rule, and the first of letter in the word of the previous
         ;;     config with the second of the rule to obtain the current rule(s) being applied 
         (current-rules (if (empty? (viz-state-ndfa-pci a-vs))
                            (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                 (sm-rules (viz-state-ndfa-M a-vs)))
                            (append-map (λ (path)
                                          (attach-empties path (sm-rules (viz-state-ndfa-M a-vs))))
                                        (remove-duplicates (for*/list ([prev prev-config]
                                                                       [curr curr-config]
                                                                       [rule (sm-rules (viz-state-ndfa-M a-vs))]
                                                                       #:when (and (equal? (first prev) (first rule))
                                                                                   (equal? (first curr) (third rule))
                                                                                   (equal? (first (second prev))
                                                                                           (second rule))))
                                                             rule)))))
         ;;(listof (listof symbol ((listof symbols) -> boolean))) (listof symbols))
         ;;Purpose: Extracts all invariants for the states that the machine can be in
         (get-invs (for*/list ([invs (viz-state-ndfa-inv a-vs)]
                               [curr curr-config]
                               #:when (equal? (first invs) (first curr)))
                     (list invs (viz-state-ndfa-pci a-vs))))

         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants holds
         (held-invs (append-map (λ (inv)
                                  (if ((second (first inv)) (second inv))
                                      (list (first (first inv)))
                                      '()))
                                get-invs))
         
         ;;(listof symbols)
         ;;Purpose: Returns all states whose invariants fail
         (brkn-invs (append-map (λ (inv)
                              (if (not (member? (first (first inv)) held-invs))
                                  (list (first (first inv)))
                                  '()))
                            get-invs))
         ;;(listof symbols)
         ;;Purpose: The states that the ndfa could be in 
         (destin-states (if (not (empty? (viz-state-ndfa-pci a-vs)))
                            (map last current-rules)
                            (cons (sm-start (viz-state-ndfa-M a-vs))
                                  (map last current-rules))))
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         (last-consumed-word (last-fully-consumed (viz-state-ndfa-pci a-vs)
                                                  (viz-state-ndfa-M a-vs)))

         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         (unconsumed-word (remove-similarities last-consumed-word
                                                (append (viz-state-ndfa-pci a-vs)
                                                        (viz-state-ndfa-upci a-vs))
                                                '()))
         ;;image
         ;;Purpose: Determines which informative message is displayed to the user
         (informative-messages (let [;;boolean
                                     ;;Purpose: Determines if the pci can be can be fully consumed
                                     (completed-config? (ormap (λ (config) (empty? (last (last config))))
                                                               (get-configs (viz-state-ndfa-pci a-vs)
                                                                            (sm-rules (viz-state-ndfa-M a-vs))
                                                                            (sm-start (viz-state-ndfa-M a-vs)))))]
                                 (above/align 'left
                                              (cond [(and (empty? (viz-state-ndfa-pci a-vs))
                                                          (empty? (viz-state-ndfa-upci a-vs)))
                                                     (above/align 'left
                                                                  (beside (text "aaaa" 20 'white)
                                                                          (text "Word: " 20 'black)
                                                                          (if (equal? (sm-apply (viz-state-ndfa-M a-vs)
                                                                                                (viz-state-ndfa-pci a-vs)) 'accept)
                                                                              (text (format "~a" EMP)
                                                                                    20
                                                                                    'gray)
                                                                              (text (format "~a" EMP)
                                                                                    20
                                                                                    'red)))
                                                                  (beside (text "Consumed: " 20 'black)
                                                                          (if (equal? (sm-apply (viz-state-ndfa-M a-vs)
                                                                                                (viz-state-ndfa-pci a-vs)) 'accept)
                                                                              (text (format "~a" EMP)
                                                                                    20
                                                                                    'black)
                                                                              (text (format "~a" EMP)
                                                                                    20
                                                                                    'white))))]
                                                    [(and (not (empty? (viz-state-ndfa-pci a-vs)))
                                                          (not completed-config?))
                                                     (above/align 'left
                                                                  (beside (text "aaaa" 20 'white)
                                                                          (text "Word: " 20 'black)
                                                                          (beside (text (if (empty? last-consumed-word)
                                                                                            ""
                                                                                            (los2str last-consumed-word))
                                                                                        20
                                                                                        'gray)
                                                                                  (text (los2str (list (first unconsumed-word)))
                                                                                        20
                                                                                        'red)
                                                                                  (text (if (empty? (rest unconsumed-word))
                                                                                            ""
                                                                                            (los2str (rest unconsumed-word)))
                                                                                        20
                                                                                        'black)))
                                                                  (beside (text "Consumed: " 20 'black)
                                                                          (text (if (empty? last-consumed-word)
                                                                                    ""
                                                                                    (los2str last-consumed-word))
                                                                                20
                                                                                'black)))]
                                                    [(empty? (viz-state-ndfa-upci a-vs))
                                                     (above/align 'left
                                                                  (beside (text "aaaa" 20 'white)
                                                                          (text "Word: " 20 'black)
                                                                          (text (los2str (viz-state-ndfa-pci a-vs))
                                                                                20
                                                                                'gray))
                                                                  (beside (text "Consumed: " 20 'black)
                                                                          (text (los2str (viz-state-ndfa-pci a-vs))
                                                                                20
                                                                                'black)))]
                                                    [(empty? (viz-state-ndfa-pci a-vs))
                                                     (above/align 'left (beside (text "aaaa" 20 'white)
                                                                                (text "Word: " 20 'black)
                                                                                (text (los2str (viz-state-ndfa-upci a-vs))
                                                                                      20
                                                                                      'black))
                                                                  (text "Consumed: " 20 'black))]
                                                    [else (above/align 'left
                                                                       (beside (text "aaaa" 20 'white)
                                                                               (text "Word: " 20 'black)
                                                                               (beside (text (los2str (viz-state-ndfa-pci a-vs))
                                                                                             20
                                                                                             'gray)
                                                                                       (text (los2str (viz-state-ndfa-upci a-vs))
                                                                                             20
                                                                                             'black)))
                                                                       (beside (text "Consumed: " 20 'black)
                                                                               (text (los2str (viz-state-ndfa-pci a-vs))
                                                                                     20
                                                                                     'black)))])
                                              (beside (text (format "The number of possible computations is ~a. "
                                                                    (number->string (length destin-states)))
                                                            20
                                                            'brown)
                                                      (text "aaaaa" 20 'white)
                                                      (cond [(and (empty? (viz-state-ndfa-upci a-vs))
                                                                  (equal? (sm-apply (viz-state-ndfa-M a-vs)
                                                                                    (viz-state-ndfa-pci a-vs)) 'accept))
                                                             (text "There is a computation that accepts."
                                                                   20
                                                                   'forestgreen)]
                                                            [(and (empty? (viz-state-ndfa-upci a-vs))
                                                                  (equal? (sm-apply (viz-state-ndfa-M a-vs)
                                                                                    (viz-state-ndfa-pci a-vs)) 'reject))
                                                             (text "All computations end in a non-final state and the machine rejects."
                                                                   20
                                                                   'red)]
                                                            [(not completed-config?)
                                                             (text "All computations do not consume the entire word and the machine rejects."
                                                                   20
                                                                   'red)]
                                                            [(text "Word Status: accept " 20 'white)]))
                                              (text "Word Status: accept " 20 'white))))]
    (above (first (resize-image
                   (graph->bitmap
                    (edge-graph
                     (node-graph (create-graph 'ndfagraph #:atb
                                               (hash 'rankdir "LR"))
                                 (viz-state-ndfa-M a-vs)
                                 (viz-state-ndfa-dead a-vs)
                                 held-invs
                                 brkn-invs)
                     (viz-state-ndfa-M a-vs)
                     current-rules
                     (viz-state-ndfa-dead a-vs)))
                   500
                   515))
           (above/align 'left
                        informative-messages      
                        E-SCENE-TOOLS))))

;; process-key
;; viz-state key -> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (let* [;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         (completed-config? (ormap (λ (config) (empty? (last (last config))))
                                   (get-configs (viz-state-ndfa-pci a-vs)
                                                (sm-rules (viz-state-ndfa-M a-vs))
                                                (sm-start (viz-state-ndfa-M a-vs)))))
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         (last-consumed-word (last-fully-consumed (append (viz-state-ndfa-pci a-vs)
                                                          (viz-state-ndfa-upci a-vs))
                                                  (viz-state-ndfa-M a-vs)))
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         (unconsumed-word (remove-similarities last-consumed-word
                                               (append (viz-state-ndfa-pci a-vs)
                                                       (viz-state-ndfa-upci a-vs))
                                               '()))]
    (viz-state-ndfa (cond [(key=? "right" a-key)
                           (if (or (empty? (viz-state-ndfa-upci a-vs))
                                   (not completed-config?))
                              (viz-state-ndfa-upci a-vs)
                              (rest (viz-state-ndfa-upci a-vs)))]
                          [(key=? "left" a-key)
                           (if (empty? (viz-state-ndfa-pci a-vs))
                               (viz-state-ndfa-upci a-vs)
                               (cons (last (viz-state-ndfa-pci a-vs))
                                     (viz-state-ndfa-upci a-vs)))]
                          [(key=? "down" a-key)
                           (cond [(empty? (viz-state-ndfa-upci a-vs))
                                  (viz-state-ndfa-upci a-vs)]
                                 [(not (equal? last-consumed-word (append (viz-state-ndfa-pci a-vs)
                                                                          (viz-state-ndfa-upci a-vs))))
                                  (rest unconsumed-word)]
                                 [else '()])]
                          [(key=? "up" a-key)
                           (if (empty? (viz-state-ndfa-pci a-vs))
                             (viz-state-ndfa-upci a-vs)
                             (append (viz-state-ndfa-pci a-vs)
                                     (viz-state-ndfa-upci a-vs)))]
                          [(viz-state-ndfa-upci a-vs)])
                  (cond [(key=? "right" a-key)
                         (if (or (empty? (viz-state-ndfa-upci a-vs))
                                   (not completed-config?))
                              (viz-state-ndfa-pci a-vs)
                              (append (viz-state-ndfa-pci a-vs)
                                     (list (first (viz-state-ndfa-upci a-vs)))))]
                        [(key=? "left" a-key)
                         (if (empty? (viz-state-ndfa-pci a-vs))
                             (viz-state-ndfa-pci a-vs)
                             (take (viz-state-ndfa-pci a-vs)
                                   (sub1 (length (viz-state-ndfa-pci a-vs)))))]
                        [(key=? "down" a-key)
                         (cond [(empty? (viz-state-ndfa-upci a-vs))
                                (viz-state-ndfa-pci a-vs)]
                               [(not (equal? last-consumed-word (append (viz-state-ndfa-pci a-vs)
                                                          (viz-state-ndfa-upci a-vs))))
                                (append last-consumed-word (take unconsumed-word 1))]
                               [else (append (viz-state-ndfa-pci a-vs)
                                             (viz-state-ndfa-upci a-vs))])]
                        [(key=? "up" a-key)
                         (if (empty? (viz-state-ndfa-pci a-vs))
                             (viz-state-ndfa-pci a-vs)
                             '())]
                        [(viz-state-ndfa-pci a-vs)])   
                  (viz-state-ndfa-M a-vs)
                  (viz-state-ndfa-inv a-vs)
                  (viz-state-ndfa-dead a-vs))))

;;machine -> machine 
;;Purpose: Produces an equivalent machine with the addition of the dead state and rules to the dead state
(define (make-new-M M)
  (cond [(eq? (sm-type M) 'ndfa)
         (local [;;symbol
                ;;Purpose: If ds is already used as a state in M, then generates a random seed symbol,
                ;;         otherwise uses DEAD
                (define dead (if (member? DEAD (sm-states M))
                                 (gen-state (sm-states M))
                                 DEAD))
                ;;(listof rules)
                ;;Purpose: Makes rules for every combination of states in M and symbols in sigma of M
                (define new-rules (for*/list ([states (sm-states M)]
                                              [sigma (sm-sigma M)])
                                    (list states sigma states)))
                ;;(listof rules)
                ;;Purpose: Makes rules for every dead state transition to itself using the symbols in sigma of M
                (define dead-rules (for*/list ([ds (list dead)]
                                               [sigma (sm-sigma M)])
                                     (list ds sigma ds)))
                ;;(listof rules)
                ;;Purpose: Gets rules that are not currently in the original rules of M
                (define get-rules-not-in-M (filter (λ (rule)
                                                     (not (member? rule (sm-rules M))))
                                                   new-rules))
                ;;(listof rules)
                ;;Purpose: Maps the dead state as a destination for all rules that are not currently in the original rules of M
                (define rules-to-dead (map (λ (rule)
                                             (append (list (first rule))
                                                     (list (second rule))
                                                     (list dead)))
                                           get-rules-not-in-M))]
           (make-ndfa (append (sm-states M) (list dead))
                      (sm-sigma M)
                      (sm-start M)
                      (sm-finals M)
                      (append (sm-rules M)
                              rules-to-dead
                              dead-rules)))]
        [(and (eq? (sm-type M) 'dfa)
              (not (member? DEAD (sm-states M))))
         (make-dfa (sm-states M)
                   (sm-sigma M)
                   (sm-start M)
                   (sm-finals M)
                   (sm-rules M))]
        [else M]))
  
;;ndfa word -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa or dfa
(define (ndfa-viz M a-word #:add-dead [add-dead #f] . invs)
  (if (not (or (eq? (sm-type M) 'ndfa)
               (eq? (sm-type M) 'dfa)))
      (error "The given machine must be a ndfa.")
      (let [(new-M (make-new-M M))]
           (run-viz-ndfa
            (viz-state-ndfa a-word
                            '()
                            (if add-dead
                                new-M
                                M)
                            invs
                            (cond [(and add-dead
                                        (eq? (sm-type M) 'ndfa))
                                   (last (sm-states new-M))]
                                  [(and add-dead
                                        (eq? (sm-type M) 'dfa))
                                   DEAD]
                                  [else 'no-dead]))
            'ndfa-viz))))

;;viz-state string -> (void)
;;Purpose: Visualizes the given viz-state
(define (run-viz-ndfa a-vs a-name)
  (begin
    (big-bang
        a-vs                
      [on-draw draw-graph]
      [on-key process-key]
      [name a-name]))
  (void)) 


(define aa*Uab* (make-ndfa '(K B D)
                           '(a b)
                           'K
                           '(B D)
                           `((K a D)
                             (K a B)
                             (B a B)
                             (D b D))))

(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C) '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                                                     (A b A) (A c A)
                                                     (B a B) (B c B)
                                                     (C a C) (C b C))))

(define p2-ndfa (make-ndfa '(S A B C D E)
                           '(a b)
                           'S
                           '(C E)
                           `((S ,EMP A)
                             (S ,EMP D)
                             (A a B)
                             (B b A)
                             (A ,EMP C)
                             (C b C)                             
                             (D a E)
                             (E b E))))

(define AB*B*UAB* (make-ndfa '(S K B C H)
                             '(a b)
                             'S
                             '(H)
                             `((S ,EMP K)
                               (S a C)
                               (K a B)
                               (K ,EMP H)
                               (B b K)
                               (C ,EMP H)
                               (H b H))))

(define AB*B*UAB*2 (make-ndfa '(S K B C H)
                              '(a b)
                              'S
                              '(H)
                              `((S ,EMP K)
                                (S a C)
                                (K a B)
                                (K ,EMP H)
                                (B b K)
                                (C ,EMP H)
                                (H b H)
                                (H a S))))

(define aa-ab (make-ndfa `(S A B F)
                         '(a b)
                         'S
                         '(A B F)
                         `((S a A)
                           (S a B)
                           (S ,EMP F)
                           (A a A)
                           (B b B))))

(define ends-with-two-bs (make-ndfa `(S A B)
                                    '(a b)
                                    'S
                                    '(B)
                                    `((S a S)
                                      (S b S)
                                      (S b A)
                                      (A b B)
                                      (B b B))))
(define ENDS-WITH-TWO-Bs (make-ndfa `(S A B)
                                    '(a b)
                                    'S
                                    '(B)
                                    `((S a S)
                                      (S b A)
                                      (A b B)
                                      (A a S)
                                      (B b B)
                                      (B a S))))

(define missing-exactly-one (make-ndfa '(S A B C D E F G H I J K L M N O P)
                                       '(a b c)
                                       'S
                                       '(E G I K M O)
                                       `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                                                    (A b D) (A c F)
                                                    (B a H) (B b J)
                                                    (C a L) (C c N)
                                                    (D b D) (D c E)
                                                    (F c F) (F b G)
                                                    (H a H) (H b I)
                                                    (J b J) (J a K)
                                                    (L a L) (L c M)
                                                    (N c N) (N a O)
                                                    (E c E) (E b E) (E a P)
                                                    (G b G) (G c G) (G a P)
                                                    (I b I) (I a I) (I c P)
                                                    (K a K) (K b K) (K c P)
                                                    (M a M) (M c M) (M b P)
                                                    (O a O) (O c O) (O b P)
                                                    (P a P) (P b P) (P c P))))

;; L = (aba)* U (ab)*
(define ND (make-ndfa '(S A B C D E)
                      '(a b)
                      'S
                      '(S)
                      `((S a A)
                        (S a B)
                        (A b C)
                        (B b D)
                        (C a E)
                        (D ,EMP S)
                        (E ,EMP S))))

(define ND2 (make-ndfa '(S A B C D E F)
                       '(a b)
                       'S
                       '(D E)
                       `((S ,EMP A)
                         (S ,EMP B)
                         (A ,EMP D)
                         (D b D)
                         (D ,EMP F)
                         (B a E)
                         (B b B)
                         (E a E)
                         (E b E)
                         (E ,EMP C))))

(define ND3 (make-ndfa '(S A B C D)
                       '(a b)
                       'S
                       '(B)
                       `((S ,EMP A)
                         (S ,EMP B)
                         (A a A)
                         (A ,EMP C)
                         (C ,EMP D)
                         (D ,EMP B)
                         (B b B))))

(define ND4 (make-ndfa '(S ds)
                       '(a b)
                       'S
                       '(ds)
                       `((S a ds)
                         (ds a ds))))

(define EVEN-NUM-Bs (make-dfa '(S F)
                              (list 'a 'b)
                              'S
                              (list 'S)
                              `((S a S)
                                (S b F)
                                (F a F)
                                (F b S))
                              'no-dead))

#;(let [(res (graph->bitmap cgraph (current-directory) FNAME))]
    (begin
      (delete-file (string-append FNAME ".dot"))
      (delete-file (string-append FNAME ".png"))
      res))
#|
;;accept examples
(ndfa-viz AB*B*UAB* '(a b b))
(ndfa-viz p2-ndfa '(a b b))
(ndfa-viz missing-exactly-one '(a a a a b b b b a a b b a a))
(ndfa-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b))
(ndfa-viz aa-ab '(a a a a))
;;reject examples
(ndfa-viz AB*B*UAB* '(a b b a))
(ndfa-viz p2-ndfa '(a b b a))
(ndfa-viz missing-exactly-one '(a a a a b b b b a a b b a a c))
(ndfa-viz AT-LEAST-ONE-MISSING '(c c c c b b b b c b a))
(ndfa-viz aa-ab '(a b b b a)
;;buggy examples
(ndfa-viz ends-with-two-bs '(a b a b a b b b))
(ndfa-viz ends-with-two-bs '(a b a b a b b a a a b b))
(ndfa-viz p2-ndfa '(a b a b))
(ndfa-viz AB*B*UAB* '(a b a b))
|#

;;word -> boolean
;;Purpose: Determines if the given word is empty
(define (S-INV a-word)
  (empty? a-word))

;;word -> boolean
;;Purpose: Determines if the last letter in the given word is an b 
(define (K-INV a-word)
  (or (empty? a-word)
      (equal? (last a-word) 'b)))

;;word -> boolean
;;Purpose: Determines if the last letter in the given word is an a
(define (B-INV a-word)
  (and (not (empty? a-word))
       (equal? (last a-word) 'a)))

;;word -> boolean
;;Purpose: Determines if the given word has one a
(define (C-INV a-word)
  (= (length (filter (λ (w) (equal? w 'a)) a-word)) 1))

;;word -> boolean
;;Purpose: Determines if the given word is empty or if the last letter is an a or b
(define (H-INV a-word)
  (or (empty? a-word)
      (equal? (last a-word) 'a)
      (equal? (last a-word) 'b)))

;;word -> boolean
;;Purpose: Determine if the given word has an even number of Bs
(define (EVEN-NUM-Bs-S-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))

;;word -> boolean
;;Purpose: Determine if the given word has an odd number of Bs
(define (EVEN-NUM-Bs-F-INV a-word)
  (odd? (length (filter (λ (w) (equal? w 'b)) a-word))))