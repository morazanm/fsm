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

(define cgraph (create-graph 'cgraph
                             #:atb (hash 'rankdir "LR")))

(define destin-color 'violetred)
(define curr-color 'blue)
;; graph machine (listof states)-> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M current-states destin-states pci)
  (foldl (λ (state result)
           (cond #;[(and (empty? pci)
                       (equal? state (sm-start M))
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color curr-color
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 #;[(and (empty? pci)
                       (equal? state (sm-start M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color curr-color
                               'shape 'circle
                               'label state
                               'fontcolor 'black))]
                 [(and (member state destin-states) 
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color destin-color
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 [(member state destin-states)
                  (add-node
                   result
                   state
                   #:atb (hash 'color destin-color
                               'shape 'circle
                               'label state
                               'fontcolor 'black))]
                 #;[(and (member state current-states) 
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color curr-color
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 #;[(member state current-states)
                  (add-node
                   result
                   state
                   #:atb (hash 'color curr-color
                               'shape 'circle
                               'label state
                               'fontcolor 'black))]
                 [(member state (sm-finals M))
                  (add-node
                   result
                   state
                   #:atb (hash 'color 'black
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 [(eq? state (sm-start M))
                  (add-node
                   result
                   state
                   #:atb (hash 'color 'darkgreen
                               'shape 'circle
                               'label state
                               'fontcolor 'black))]
                 [else (add-node result 
                                 state
                                 #:atb (hash 'color 'black 'shape 'circle 'label
                                             state
                                             'fontcolor 'black))]))
         cgraph
         (sm-states M)))

;; graph NDFA word (listof rules) (listof rules) -> graph
;; Purpose: To create a graph of edges from the given list of rules
(define (edge-graph cgraph M pci current-place prev-path)
  (foldl (λ (rule result)
           (cond [(and (empty? pci)
                       (member rule (get-empties-from-start (sm-start M) (sm-rules M))))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color destin-color 'fontsize 20 'style 'solid))]
                 [(and (empty? pci)
                       (ormap (λ (p) (and (equal? (first rule) (first p))
                                          (not (equal? (second rule) EMP))))
                              current-place))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'fontsize 20 'style 'solid))]
                 #;[(and (not (empty? pci)) (equal? (second rule) EMP)
                         (equal? (first rule) (sm-start M)) (member rule prev-path))
                    (add-edge result
                              (second rule)
                              (first rule)
                              (third rule)
                              #:atb (hash 'color 'gray 'fontsize 20 'style 'solid))]
                 [(or (member rule current-place)
                      (ormap (λ (p) (and (equal? (first rule) (first p))
                                         (equal? (third rule) (third p))))
                             current-place)
                      #;(ormap (λ (w) (equal? (second rule) w))                  
                               pci)
                      #;(not (ormap (λ (s) (equal? (first rule)
                                                   (first s)))
                                    (get-empties (sm-rules M)))))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color destin-color 'fontsize 20 'style 'solid))]
                 #;[(member rule prev-path)
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color 'gray 'fontsize 20 'style 'solid))]
                 [else
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'fontsize 20 'style 'solid))]))
         cgraph
         (sm-rules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

;;(listof rules) -> (listof rules)
;;Purpose: Returns all empty transitions
(define (get-empties lor)
  (filter (λ (rule)
            (equal? (second rule) EMP))
          lor))

;;symbol (listof rules) -> (listof rules)
;;Purpose: Returns all empty transitions from the start state
(define (get-empties-from-start start lor)
  (filter (λ (rule)
            (and (equal? (first rule)  start)
                 (equal? (second rule) EMP)))
          lor))

;;symbol (listof rules) word -> (listof rules)
;;Purpose: Returns all readable rules from the
;;         starting state using the first of the
;;         given word
(define (get-readables-from-start start lor word)
  (if (empty? word)
      '()
      (filter
       (λ (rule)
         (and (equal? (first rule)   start)
              (equal? (second rule) (first word))))
       lor)))

;;(listof rules) word -> (listof rules)
;;Purpose: Returns all rules contain letter in the given word
(define (get-rules-from-word lor word)
  (append-map (λ (w)
                (filter (λ (rule) (or (equal? (second rule) w)
                                      (equal? (second rule) EMP)))
                        lor))
              word))

;;symbol (listof rules) (listof rules) word -> (listof rules)
;;Purpose: Returns all paths whose length is greater than or
;;         equal to the length of the word
(define (path-find start lost rfw word lor)
  (append-map (λ (path)
                (if (>= (length path) (length word))
                    (attach-empties path lor)
                    '()))
              (make-path start lost rfw word)))

;;symbol (listof rules) (listof rules) word -> (listof (listof rules))
;;Purpose: Returns all possible paths for the given word
;;         starting from the given start symbol and (listof starting rule)
;;ASSUMPTION: The given word is non-empty
(define (make-path start lost rfw word)
  (if (empty? lost)
      '()
      (cons (construct-path start word (first lost) rfw)
            (make-path start (rest lost) rfw word))))

;;symbol word rule (listof rules) -> (listof rules)
;;Purpose: Constructs a path from the given symbol
;;         using the given rule and given word
(define (construct-path start word rule rfw)
  (begin
    ;(display (format "rule ~s \n" rule))
    ;(display (format "start ~s \n" start))
    ;(display (format "word ~s \n" word))
    (cond [(empty? word) '()]
          [(and (equal? start        (first rule))  ;; Start symbol equal to (firstof rule)
                (equal? (first word) (second rule)));; and (firstof word) equal to (secondof rule)
           (cons rule
                 (construct-path-from-destin (third rule) (rest word) rfw))]
          [(and (equal? start (first rule))   ;;Start symbol equal to (firstof rule) 
                (equal? EMP   (second rule))) ;; and is empty transition
           (cons rule
                 (construct-path-from-destin (third rule) word rfw))]
          [else (construct-path-from-destin start word rfw)]))) 

;;symbol word (listof rules) -> (listof rules)
;;Purpose: Constructs a path from the given symbol
;;         using the given and given (lisof rules)
(define (construct-path-from-destin destin word rfw)
  #;(let [(cp-frm-first (if (empty? rfw)
                            '()
                            (construct-path destin word (first rfw) (rest rfw))))]
      (cond [(or (empty? word) (empty? rfw)) '()]
            [(>= (length cp-frm-first) (length word))
             cp-frm-first]
            [else (construct-path-from-destin destin word (rest rfw))]))
  (if (or (empty? word) (empty? rfw))
      '()
      (construct-path destin word (first rfw) (rest rfw))))

(define (return-differences prev-path curr-path)
  (cond [(and (empty? prev-path)
              (empty? curr-path))
         (remove-similiarities prev-path curr-path '())]
        [(and (empty? prev-path)
              (>= (length curr-path) 1))
         (remove-similiarities prev-path (first curr-path) '())]
        [(and (empty? curr-path)
              (>= (length prev-path) 1))
         (remove-similiarities (first prev-path) curr-path '())]
        [(and (= (length prev-path) 1)
              (= (length curr-path) 1))
         (middle-man (first prev-path) (first curr-path) '())]
        [(< (length prev-path) (length curr-path))
         (append (remove-similiarities (first prev-path) (first curr-path) '())
                 (return-differences prev-path (rest curr-path)))]
        [(> (length prev-path) (length curr-path))
         (append (middle-man (first prev-path) (first curr-path) '())
                 (return-differences (rest prev-path) curr-path))]
        [else (append (middle-man (first prev-path) (first curr-path) '())
                      (return-differences (rest prev-path) (rest curr-path)))])
  #;(remove-similiarities prev-path curr-path '()))

(define (middle-man prev-path curr-path acc)
  (if (or (equal? (first (first prev-path))
              (first (first curr-path)))
          (equal? (second (first prev-path)) EMP))
      (remove-similiarities prev-path curr-path acc)
      '()))

;;(listof rules) (listof rules) (listof rules) -> (listof rules)
;;Purpose: Returns the complement of the intersection
;;         between the lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similiarities prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(empty? curr-path) acc]
        [(equal? (first prev-path) (first curr-path))
         (remove-similiarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similiarities (rest prev-path) (rest curr-path)
                               (append acc (list (first curr-path))))]))

;;(listof rules) (listof rules) -> (listof rules)
;;Purpose: Appends empty transitions to the end of the path if applicable
;;ASSUMPTION: The given path is non-empty
(define (attach-empties path lor)
  (let [(empty-from-path
         (filter (λ (rule)
                   (and (equal? (third (last path))
                                (first rule))
                        (equal? (second rule) EMP)))
                 lor))]
    (append path empty-from-path)))

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
    (filter (λ (config)
              (empty? (last (last config))))
            (remove-duplicates
             (get-configs-helper configs a-word lor configs)))))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) -> (listof configurations)
;;Purpose: Returns all possible configurations using the given word and  (listof rules)
;;Visited = All configurations of the consumed portion of the given word
(define (get-configs-helper configs a-word lor visited)
  (cond [(empty? configs) '()]
        ;[(empty? a-word) configs]
        [else (append (make-configs (first configs) a-word lor visited (list (first configs)))
                      (get-configs-helper (rest configs) a-word lor visited))]))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Explores all possible configurations using the given word, (listof rules), and visited
;;Visited = All configurations of the consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs config a-word lor visited path)
  (let* [;;(listof rules)
         ;;Purpose: Returns all rules
         (connected-rules (if (empty? a-word)
                              #;(filter (λ (rule)
                                          (and (equal? (first rule) (first config))
                                               (equal? (second rule) EMP)))
                                        lor)
                              '()
                              (filter (λ (rule)
                                        (and (equal? (first rule) (first config))
                                             (equal? (second rule) (first (second config)))))
                                      lor)))
         (new-config (filter (λ (new-config)
                               (not (member? new-config visited)))
                             ;'()
                             ;new-config))
                             (map (λ (rule)
                                    (cond [(empty? a-word) (append (list (third rule)) (list a-word))]
                                          ;[(equal? (second rule) EMP) (append (list (third rule)) (list a-word))]
                                          [else (append (list (third rule)) (list (rest a-word)))]))
                                  connected-rules)))
         (connected-rules-via-emp (filter (λ (rule)
                                            (and (equal? (first rule) (first config))
                                                 (equal? (second rule) EMP)))
                                          lor))       
         (new-config-via-emp (filter (λ (new-config)
                                       (not (member? new-config visited)))
                                     ; '()
                                     ;new-config))
                                     (map (λ (rule)
                                            (append (list (third rule)) (list a-word)))
                                          connected-rules-via-emp)))]
    (begin
      
      ;(display (format "visited ~s \n" visited))
      ;(display (format "config ~s \n" config))
      ;(display (format "word ~s \n" a-word))
      ;(display (format "new-config post ~s \n" new-config))
      ;(display (format "connected-rules ~s \n" connected-rules))
      ;(display (format "new-config-via-emp post ~s \n" new-config-via-emp))
      ;(display (format "connected-rules-via-emp ~s \n" connected-rules-via-emp))
      ;(display (format "path ~s \n" (append path new-config)))
      ;(display (format "path-via-emp ~s \n" (append path new-config-via-emp)))
      (cond [(and (empty? new-config)
                  (empty? new-config-via-emp))
             (list path)]
            [(and (empty? a-word) (empty? new-config-via-emp)
                  (= (length new-config) 1))
             (make-configs (first new-config)
                           a-word
                           lor
                           (append new-config visited)
                           (append path new-config))]
            
            [(and (empty? a-word) (empty? new-config)
                  (= (length new-config-via-emp) 1))
             
             (make-configs (first new-config-via-emp)
                           a-word
                           lor
                           (append new-config-via-emp visited)
                           (append path new-config-via-emp))]
            
            [(and (empty? a-word) (empty? new-config-via-emp))
             (make-configs-helper new-config
                                  a-word
                                  lor
                                  (append new-config visited)
                                  (append path new-config))]
            [(and (empty? a-word) (empty? new-config))
             (make-configs-helper new-config-via-emp
                                  a-word
                                  lor
                                  (append new-config visited)
                                  (append path new-config))]
            [(and (empty? new-config-via-emp)
                  (= (length new-config) 1))
             (make-configs (first new-config)
                           (rest a-word)
                           lor
                           (append new-config visited)
                           (append path new-config))]
            [(and (empty? new-config-via-emp)
                  (> (length new-config) 1))
             (make-configs-helper new-config
                                  (rest a-word)
                                  lor
                                  (append new-config visited)
                                  (append path new-config))]
            [(and (empty? new-config)
                  (= (length new-config-via-emp) 1))
             (make-configs (first new-config-via-emp)
                           a-word
                           lor
                           (append new-config-via-emp visited)
                           (append path new-config-via-emp))]
            [(and (empty? new-config)
                  (> (length new-config-via-emp) 1))
             (make-configs-helper new-config-via-emp
                                  a-word
                                  lor
                                  (append path new-config-via-emp)
                                  (append path new-config-via-emp))]
            [else (append (make-configs-helper new-config
                                               (rest a-word)
                                               lor
                                               (append new-config visited)
                                               (append path new-config))
                          (make-configs-helper new-config-via-emp
                                               a-word
                                               lor
                                               (append new-config-via-emp visited)
                                               (append path new-config-via-emp)))]))))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Ensures that all possible configurations are explored
;;Visited = All configurations that consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs-helper configs a-word lor visited path)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited path)
              (make-configs-helper (rest configs) a-word lor visited path))))

(define (path-maker a-word lor configs)
  (if (empty? configs)
      '()
      (cons (path-constructor a-word lor (first configs) '() (first configs))
              (path-maker a-word lor (rest configs)))))

(define (path-constructor a-word lor config path prev-config)
  (cond [(= (length config) 1) (reverse path)]
        [(and (= (length (second (first config)))
                 (length (second (second config))))
              (empty? (second (first config)))
              (empty? (second (second config))))
         (let* [(similiar-configs (append (list (first config)) (list (second config))))
                (get-rules (remove-duplicates
                            (filter (λ (rules)
                                     (not (member? rules path)))
                                     (append-map
                                      (λ (sim-config)
                                         (filter (λ (rule)
                                                   (or (and (equal? (first prev-config) (first rule))
                                                            (equal? (first sim-config) (third rule)))
                                                       (and (equal? (first (first config)) (first rule))
                                                            (equal? (first (second config)) (third rule))
                                                            (equal? EMP (second rule)))
                                                        #;(or (equal? (last (second prev-config)) (second rule))
                                                            (equal? EMP (second rule)))))
                                                 lor))
                                       similiar-configs))))]
          
             (begin
               ;(display (format "get-rules emp ~s \n" get-rules))
               ;(display (format "(first (first config)) emp ~s \n" (first (first config))))
               ;(display (format "(first (second config)) emp ~s \n" (first (second config))))
               (path-constructor a-word lor (rest config) (append get-rules path) (first config))))]
        [(= (length (second (first config)))
            (length (second (second config))))
         (let* [(similiar-configs (append (list (first config)) (list (second config))))
                (get-rules (remove-duplicates
                            (filter (λ (rules)
                                     (not (member? rules path)))
                                     (append-map
                                      (λ (sim-config)
                                         (filter (λ (rule)
                                                   (or (and (equal? (first prev-config) (first rule))
                                                            (equal? (first sim-config) (third rule)))
                                                       (and (equal? (first (first config)) (first rule))
                                                            (equal? (first (second config)) (third rule))
                                                            (equal? EMP (second rule)))
                                                        #;(or (equal? (last (second prev-config)) (second rule))
                                                            (equal? EMP (second rule)))))
                                                 lor))
                                       similiar-configs))))]
           (begin
             ;(display (format "get-rules ~s \n" get-rules))
             ;(display (format "(first (first config)) ~s \n" (first (first config))))
             ;(display (format "(first (second config)) ~s \n" (first (second config))))
             ;(display (format "prev-config ~s \n" prev-config ))
             #;(display (format "alt style ~s \n" (append
                                                   (path-constructor a-word lor (cons (first config)
                                                                                      (rest (rest config))) (append get-rules '()) prev-config)
                                                   (path-constructor a-word lor (rest config) (append get-rules '()) prev-config)))) 
             #;(remove-duplicates
                (append
                 (path-constructor a-word lor (cons (first config) (rest (rest config))) (append get-rules path) prev-config)
                 (path-constructor a-word lor (rest config) (append get-rules path) prev-config)))
             (append (reverse (append get-rules path))
                     (path-constructor a-word lor (cons (first config) (rest (rest config))) '() prev-config)
                     (path-constructor a-word lor (rest config)                              '() (first config)))))]
        [(empty? (second (first config)))
         (let [(get-rules (filter (λ (rule)
                                    (and (equal? (first (first config)) (first rule))
                                         (equal? (first (second config)) (third rule))
                                         (equal? EMP (second rule))))
                                  lor))]
           (path-constructor a-word lor (rest config) (append get-rules path) (first config)))]
       
        [else (let [(get-rules (filter (λ (rules)
                                         (not (member? rules path)))
                                         (filter (λ (rule)
                                         (and (equal? (first (first config)) (first rule))
                                              (equal? (first (second config)) (third rule))
                                              (or (equal? (first (second (first config))) (second rule))
                                                  (equal? EMP (second rule)))))
                                       lor)))]
               (if (empty? get-rules)
                   '()
               (begin
                  ;(display (format "get-rules else ~s \n" get-rules))
                  ;(display (format "(first (first config)) else ~s \n" (first (first config))))
                  ;(display (format "(first (second config)) else ~s \n" (first (second config))))
                  (path-constructor a-word lor (rest config) (append get-rules path) (first config)))))]))


#;'(((S a S) (S b S) (S b S) (S b S) (S b A) (S a S) (S b S) (S b S) (S b A) (S a S) (S b S) (S b S) (S b A) (S a S) (S b S) (S b A))
    ((S a S) (S b S) (S b S) (S a S) (S b S) (S b S) (A b B) (S a S) (S b S) (S a S) (S b S) (A b B))
    ((S a S) (S b S) (B b B) (S a S) (S b S) (A b B) (B b B)))

#;(append-map (λ (sim-config)
                (filter (λ (rule)
                          (and (equal? (first (first '((S (a a a a))))) (first rule))
                               (equal? (first sim-config) (third rule))
                               (or (equal? (first (second (first '((S (a a a a)))))) (second rule))
                                   (equal? EMP (second rule)))))
                        (sm-rules aa-ab)))
              '((A (a a a)) (B (a a a))))

;;using new method without remove duplicates
#;'(((S b A) (S b S) (S b A) (S b S) (S b A) (S b S) (S b A) (S b S))
    ((S b A) (S b S) (S b A) (S b S) (A b B) (A b B))
    ((S b A) (S b S) (B b B) (S b A) (S b S) (A b B) (B b B)))
;;using remove duplicates
#;'(((S a S) (S b S) (S b A))
    ((S a S) (S b S) (S b A) (A b B))
    ((S a S) (S b S) (S b A) (B b B) (A b B)))
;;informative messaging -> number of computations

(define (add-empties-to-start path lor)
  (if (empty? path)
      '()
      (append-map (λ (rule)
                (if (equal? (first (first path))
                            (third rule))
                    (append rule path)
                    '()))
              lor)))

#|
want to say why every component used for the tool is important
want to implement a bug on bigger machines to emphasis importance of yield and colors
want an example that highlights the importance of zoom
want show a combination of all the features if possible
|#

(define (get-empties-from-start2 start lor)
   (append-map (λ (rule)
          (if (and (equal? (first rule)  start)
                   (equal? (second rule) EMP))
              (append (list rule) (get-empties-from-start2 (third rule) lor))
              '()))
        lor))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;upci is the unprocessed consumed input
;;pci is the proccessed consumed input
;;M is an ndfa
(struct viz-state-ndfa (upci pci M))

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

;; resize-image
;; image -> int -> int -> image
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
                 (/ scaled-height src-height))]
          )))

;;viz-state -> scene
;;Purpose: Draws the given viz-state onto the scene
(define (draw-graph a-vs)
  (let* [;;(listof rules)
         ;;Purpose: All paths reachable on an empty transition from start
         (starting-empties (let [(empties (get-empties-from-start2 (sm-start (viz-state-ndfa-M a-vs))
                                                            (sm-rules (viz-state-ndfa-M a-vs))))]
                             (if (empty? empties)
                                 '()
                                 (list empties)))
   #;(append-map (λ (path) (attach-empties (list path) (sm-rules (viz-state-ndfa-M a-vs))))
                                       (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                               (sm-rules (viz-state-ndfa-M a-vs)))))
         ;;(listof (listof rules))
         ;;Purpose: Returns all viable paths using the proccessed CI
         (curr-path (if (empty? (viz-state-ndfa-pci a-vs))
                        starting-empties
                        #;(path-find (sm-start (viz-state-ndfa-M a-vs))
                                     (append (get-readables-from-start
                                              (sm-start (viz-state-ndfa-M   a-vs))
                                              (sm-rules (viz-state-ndfa-M   a-vs))
                                              (viz-state-ndfa-pci a-vs))
                                             (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                                     (sm-rules (viz-state-ndfa-M a-vs))))
                                     (get-rules-from-word (sm-rules (viz-state-ndfa-M   a-vs))
                                                          (viz-state-ndfa-pci a-vs))
                                     (viz-state-ndfa-pci a-vs)
                                     (sm-rules (viz-state-ndfa-M a-vs)))
                        ;(append-map (λ (rule) (attach-empties rule (sm-rules (viz-state-ndfa-M a-vs))))
                                    (path-maker (viz-state-ndfa-pci a-vs)
                                                (sm-rules (viz-state-ndfa-M a-vs))
                                                (get-configs (viz-state-ndfa-pci a-vs)
                                                             (sm-rules (viz-state-ndfa-M a-vs))
                                                             (sm-start (viz-state-ndfa-M a-vs))))));)
         
         
         ;;(listof rules)
         ;;Purpose: Returns the all viable paths using the
         ;;         proccessed CI that is one step behind
         (full-prev-path (cond [(empty?    (viz-state-ndfa-pci a-vs)) '()]
                               [(= (length (viz-state-ndfa-pci a-vs)) 1)
                                starting-empties]
                               [else #;(path-find (sm-start (viz-state-ndfa-M a-vs))
                                                  (append (get-readables-from-start (sm-start (viz-state-ndfa-M   a-vs))
                                                                                    (sm-rules (viz-state-ndfa-M   a-vs))
                                                                                    (take     (viz-state-ndfa-pci a-vs)
                                                                                              (sub1 (length (viz-state-ndfa-pci a-vs)))))
                                                          (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                                                  (sm-rules (viz-state-ndfa-M a-vs))))
                                                  (get-rules-from-word (sm-rules (viz-state-ndfa-M   a-vs))
                                                                       (take     (viz-state-ndfa-pci a-vs)
                                                                                 (sub1 (length (viz-state-ndfa-pci a-vs)))))
                                                  (take (viz-state-ndfa-pci a-vs)
                                                        (sub1 (length (viz-state-ndfa-pci a-vs))))
                                                  (sm-rules (viz-state-ndfa-M a-vs)))
                                     ;(append-map (λ (rule) (attach-empties rule (sm-rules (viz-state-ndfa-M a-vs))))
                                                 (begin
                                                   ;(display "\n \n prev-path \n")
                                                   (path-maker (take (viz-state-ndfa-pci a-vs)
                                                                   (sub1 (length (viz-state-ndfa-pci a-vs))))
                                                             (sm-rules (viz-state-ndfa-M a-vs))
                                                             (get-configs (take (viz-state-ndfa-pci a-vs)
                                                                                (sub1 (length (viz-state-ndfa-pci a-vs))))
                                                                          (sm-rules (viz-state-ndfa-M a-vs))
                                                                          (sm-start (viz-state-ndfa-M a-vs)))))]))
         ;;(listof rules)
         ;;Purpose: All paths that have similiarities with
         ;;         the current path
         ;(prev-path (map last full-prev-path))
                    
         ;;(listof rules)
         ;;Purpose: The current rules that the ndfa is using to consume the proccess CI
         (current-place ;(return-differences full-prev-path curr-path)
          #;(if (empty? (viz-state-ndfa-pci a-vs))
                curr-path
                (map last curr-path)
                #;(remove-duplicates
                   (append-map (λ (path)
                                 (filter (λ (p) (equal? (last (viz-state-ndfa-pci a-vs))
                                                        (second p)))
                                         path))
                               curr-path)))
          (begin
            ;(display (format "curr-path ~s \n" curr-path))
            ;(display (format "prev-path ~s \n" full-prev-path))
            #;(foldl (λ (prev curr)
                   (return-differences prev curr))
                   curr-path
                   full-prev-path)
            (return-differences full-prev-path curr-path)))
         #;(map (λ (path)
                  (filter (λ (p) (equal? (last (viz-state-ndfa-pci a-vs))
                                         (second p)))
                          path))
                curr-path)
         ;;(listof symbols)
         ;;Purpose: The current state that the ndfa is in
         (current-states (begin
                           ;(display (format "current-place ~s \n \n" current-place))
                           (remove-duplicates
                          (map first
                               current-place))))
         ;;(listof symbols)
         ;;Purpose: The current state that the ndfa is in
         (destin-states (remove-duplicates
                         (map last
                              current-place)))
         ;;image
         ;;Purpose: Determines which informative message is displayed to the user
         (informative-messages (cond [(empty? (viz-state-ndfa-upci a-vs))
                                      (if (eq? (sm-apply (viz-state-ndfa-M a-vs)
                                                         (viz-state-ndfa-pci a-vs))
                                               'accept)
                                          (above (beside
                                              (overlay/offset
                                              (overlay/offset (text "Word: " 20 'black)        
                                                              -23 23
                                                              (text "Consumed: " 20' black))
                                                             -15 35
                                                             (text "Computations: " 20 'black))
                                              (above/align 'left
                                                           (text (los2str (viz-state-ndfa-pci a-vs))
                                                                            20
                                                                            'gray)
                                                           (text (los2str (viz-state-ndfa-pci a-vs))
                                                                      20
                                                                      'black)
                                                           (text "There is a computation that leads to accept" 20 'black)))
                                             (text "Word Status: accept " 20 'white))
                                          (above (beside
                                              (overlay/offset
                                              (overlay/offset (text "Word: " 20 'black)        
                                                              -23 23
                                                              (text "Consumed: " 20' black))
                                                             -15 35
                                                             (text "Computations: " 20 'black))
                                              (above/align 'left
                                                           (text (los2str (viz-state-ndfa-pci a-vs))
                                                                            20
                                                                            'gray)
                                                           (text (los2str (viz-state-ndfa-pci a-vs))
                                                                      20
                                                                      'black)
                                                           (text "All computations lead to reject" 20 'black)))
                                             (text "Word Status: accept " 20 'white)))]
                                     [(and (empty? destin-states)
                                           (not (empty? (viz-state-ndfa-pci a-vs))))
                                      (above (beside
                                              (overlay/offset
                                              (overlay/offset (text "Word: " 20 'black)        
                                                              -23 23
                                                              (text "Consumed: " 20' black))
                                                             -15 35
                                                             (text "Computations: " 20 'black))
                                              (above/align 'left
                                                           (beside (text (los2str (viz-state-ndfa-pci a-vs))
                                                                            20
                                                                            'gray)
                                                                      (text (los2str (viz-state-ndfa-upci a-vs))
                                                                            20
                                                                            'black))
                                                           (text (los2str (viz-state-ndfa-pci a-vs))
                                                                      20
                                                                      'black)
                                                           (text "All computations lead to reject" 20 'black)))
                                             (text "Word Status: accept " 20 'white))]
                                     [(empty? (viz-state-ndfa-pci a-vs)) 
                                      (above (beside
                                              (overlay/offset
                                              (overlay/offset (text "Word: " 20 'black)        
                                                              -23 23
                                                              (text "Consumed: " 20 'black))
                                                             -15 35
                                                             (text "Computations: " 20 'black))
                                              (above/align 'left
                                                           (text (los2str (viz-state-ndfa-upci a-vs))
                                                                            20
                                                                            'black)
                                                           (text "Consumed: " 20 'white)
                                                           (text (number->string (length destin-states)) 20 'black)))
                                             (text "Word Status: accept " 20 'white))]
                                     [else (above (beside
                                              (overlay/offset
                                              (overlay/offset (text "Word: " 20 'black)        
                                                              -23 23
                                                              (text "Consumed: " 20 'black))
                                                             -15 35
                                                             (text "Computations: " 20 'black))
                                              (above/align 'left
                                                           (beside (text (los2str (viz-state-ndfa-pci a-vs))
                                                                            20
                                                                            'gray)
                                                                      (text (los2str (viz-state-ndfa-upci a-vs))
                                                                            20
                                                                            'black))
                                                           (text (los2str (viz-state-ndfa-pci a-vs))
                                                                      20
                                                                      'black)
                                                           (text (number->string (length destin-states)) 20 'black)))
                                             (text "Word Status: accept " 20 'white))
                                           #;(above/align 'left (beside (text "Word: " 20 'black)
                                                                      (text (los2str (viz-state-ndfa-pci a-vs))
                                                                            20
                                                                            'gray)
                                                                      (text (los2str (viz-state-ndfa-upci a-vs))
                                                                            20
                                                                            'black))
                                                        (beside (text "Consumed: " 20' black)
                                                                (text (los2str (viz-state-ndfa-pci a-vs))
                                                                      20
                                                                      'black))
                                                        (beside (text "Computations: " 20 'black)
                                                           (text (number->string (length destin-states)) 20 'black))
                                                        (text "Word Status " 20 'white))]))]
    #;(cond [(empty? (viz-state-ndfa-upci a-vs))
             (place-image 
              informative-messages
              250 450
              (above (first (resize-image
                             (graph->bitmap
                              (edge-graph
                               (node-graph (create-graph 'ndfagraph #:atb
                                                         (hash 'rankdir "LR"))
                                           (viz-state-ndfa-M a-vs)
                                           current-states
                                           destin-states
                                           (viz-state-ndfa-pci a-vs))
                               (viz-state-ndfa-M a-vs)
                               (viz-state-ndfa-pci a-vs)
                               current-place
                               prev-path))
                             500
                             585))
                     E-SCENE-TOOLS))]
            [(empty? (viz-state-ndfa-pci a-vs))
             (place-image 
              informative-messages
              250 450
              (above (first (resize-image
                             (graph->bitmap
                              (edge-graph
                               (node-graph (create-graph 'ndfagraph #:atb
                                                         (hash 'rankdir "LR"))
                                           (viz-state-ndfa-M a-vs)
                                           current-states
                                           destin-states
                                           (viz-state-ndfa-pci a-vs))
                               (viz-state-ndfa-M a-vs)
                               (viz-state-ndfa-pci a-vs)
                               current-place
                               prev-path))
                             500
                             585))
                     E-SCENE-TOOLS)
              E-SCENE-TOOLS)]
            [else (place-image 
                   informative-messages
                   250 450
                   (above (first (resize-image
                                  (graph->bitmap
                                   (edge-graph
                                    (node-graph (create-graph 'ndfagraph #:atb
                                                              (hash 'rankdir "LR"))
                                                (viz-state-ndfa-M a-vs)
                                                current-states
                                                destin-states
                                                (viz-state-ndfa-pci a-vs))
                                    (viz-state-ndfa-M a-vs)
                                    (viz-state-ndfa-pci a-vs)
                                    current-place
                                    prev-path))
                                  500
                                  585))
                          E-SCENE-TOOLS))])
    ;(above/align 'left ;place-image 
    ;  informative-messages
    ;150 450
    (above (first (resize-image
                   (graph->bitmap
                    (edge-graph
                     (node-graph (create-graph 'ndfagraph #:atb
                                               (hash 'rankdir "LR"))
                                 (viz-state-ndfa-M a-vs)
                                 current-states
                                 destin-states
                                 (viz-state-ndfa-pci a-vs))
                     (viz-state-ndfa-M a-vs)
                     (viz-state-ndfa-pci a-vs)
                     current-place
                     full-prev-path))
                   500
                   515))
           (above/align 'left
                        informative-messages      
                        E-SCENE-TOOLS))))
#;(underlay/xy E-SCENE
               6 500
               (scale .99 E-SCENE-TOOLS))
;; process-key
;; viz-state key -> viz-state
;; Purpose: Move the visualization one step forward, one step
;;          backwards, or to the end.
(define (process-key a-vs a-key)
  (cond [(key=? "right" a-key)
         (if (empty? (viz-state-ndfa-upci a-vs))
             a-vs
             (viz-state-ndfa (rest (viz-state-ndfa-upci a-vs))
                             (append (viz-state-ndfa-pci a-vs)
                                     (list (first (viz-state-ndfa-upci a-vs))))
                             (viz-state-ndfa-M a-vs)))]
        [(key=? "left" a-key) 
         (if (empty? (viz-state-ndfa-pci a-vs))
             a-vs
             (viz-state-ndfa (cons (last (viz-state-ndfa-pci a-vs))
                                   (viz-state-ndfa-upci a-vs))
                             (take (viz-state-ndfa-pci a-vs)
                                   (sub1 (length (viz-state-ndfa-pci a-vs))))
                             (viz-state-ndfa-M a-vs)))]
        [(key=? "down" a-key) 
         (if (empty? (viz-state-ndfa-upci a-vs))
             a-vs
             (viz-state-ndfa '()
                             (append (viz-state-ndfa-pci a-vs)
                                     (viz-state-ndfa-upci a-vs))
                             (viz-state-ndfa-M a-vs)))]
        [(key=? "up" a-key)
         (if (= (length (viz-state-ndfa-pci a-vs)) 1)
             a-vs
             (viz-state-ndfa (append (viz-state-ndfa-pci a-vs)
                                     (viz-state-ndfa-upci a-vs))
                             '()
                             (viz-state-ndfa-M a-vs)))]
        [else a-vs]))

;;ndfa word -> (void) Throws error
;;Purpose: Visualizes the given ndfa processing the given word
;;Assumption: The given machine is a ndfa
(define (ndfa-viz M a-word)
  (if (not (eq? (sm-type M) 'ndfa))
      (error "The given machine must be a ndfa")
      (run-viz-ndfa
       (viz-state-ndfa a-word
                       '()
                       M)
       'ndfa-viz)))

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


;;sample configurations uses empties from start
;;ex using p2-ndfa -> word is (a b a)
;;all empties from S -> consuming a -> consuming b -> consuming a
;;(S (a b a)) -> 
;;(D (a b a)) -> (E (b a))
;;(A (a b a)) -> (B (b a)) -> (A (a)) -> (B ())
;;(C (a b a)) ->
;;have an acc to track visited configs 

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
    (filter (λ (config)
              (empty? (last (last config))))
            (remove-duplicates
             (get-configs-helper configs a-word lor configs)))))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) -> (listof configurations)
;;Purpose: Returns all possible configurations using the given word and  (listof rules)
;;Visited = All configurations of the consumed portion of the given word
(define (get-configs-helper configs a-word lor visited)
  (cond [(empty? configs) '()]
        ;[(empty? a-word) configs]
        [else (append (make-configs (first configs) a-word lor visited (list (first configs)))
                      (get-configs-helper (rest configs) a-word lor visited))]))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Explores all possible configurations using the given word, (listof rules), and visited
;;Visited = All configurations of the consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs config a-word lor visited path)
  (let* [;;(listof rules)
         ;;Purpose: Returns all rules
         (connected-rules (if (empty? a-word)
                              #;(filter (λ (rule)
                                          (and (equal? (first rule) (first config))
                                               (equal? (second rule) EMP)))
                                        lor)
                              '()
                              (filter (λ (rule)
                                        (and (equal? (first rule) (first config))
                                             (equal? (second rule) (first (second config)))))
                                      lor)))
         (new-config (filter (λ (new-config)
                               (not (member? new-config visited)))
                             ;'()
                             ;new-config))
                             (map (λ (rule)
                                    (cond [(empty? a-word) (append (list (third rule)) (list a-word))]
                                          ;[(equal? (second rule) EMP) (append (list (third rule)) (list a-word))]
                                          [else (append (list (third rule)) (list (rest a-word)))]))
                                  connected-rules)))
         (connected-rules-via-emp (filter (λ (rule)
                                            (and (equal? (first rule) (first config))
                                                 (equal? (second rule) EMP)))
                                          lor))       
         (new-config-via-emp (filter (λ (new-config)
                                       (not (member? new-config visited)))
                                     ; '()
                                     ;new-config))
                                     (map (λ (rule)
                                            (append (list (third rule)) (list a-word)))
                                          connected-rules-via-emp)))]
    (begin
      
      ;(display (format "visited ~s \n" visited))
      ;(display (format "config ~s \n" config))
      ;(display (format "word ~s \n" a-word))
      ;(display (format "new-config post ~s \n" new-config))
      ;(display (format "connected-rules ~s \n" connected-rules))
      ;(display (format "new-config-via-emp post ~s \n" new-config-via-emp))
      ;(display (format "connected-rules-via-emp ~s \n" connected-rules-via-emp))
      ;(display (format "path ~s \n" (append path new-config)))
      ;(display (format "path-via-emp ~s \n" (append path new-config-via-emp)))
      (cond [(and (empty? new-config)
                  (empty? new-config-via-emp))
             (list path)]
            [(and (empty? a-word) (empty? new-config-via-emp)
                  (= (length new-config) 1))
             (make-configs (first new-config)
                           a-word
                           lor
                           (append new-config visited)
                           (append path new-config))]
            
            [(and (empty? a-word) (empty? new-config)
                  (= (length new-config-via-emp) 1))
             
             (make-configs (first new-config-via-emp)
                           a-word
                           lor
                           (append new-config-via-emp visited)
                           (append path new-config-via-emp))]
            
            [(and (empty? a-word) (empty? new-config-via-emp))
             (make-configs-helper new-config
                                  a-word
                                  lor
                                  (append new-config visited)
                                  (append path new-config))]
            [(and (empty? a-word) (empty? new-config))
             (make-configs-helper new-config-via-emp
                                  a-word
                                  lor
                                  (append new-config visited)
                                  (append path new-config))]
            [(and (empty? new-config-via-emp)
                  (= (length new-config) 1))
             (make-configs (first new-config)
                           (rest a-word)
                           lor
                           (append new-config visited)
                           (append path new-config))]
            [(and (empty? new-config-via-emp)
                  (> (length new-config) 1))
             (make-configs-helper new-config
                                  (rest a-word)
                                  lor
                                  (append new-config visited)
                                  (append path new-config))]
            [(and (empty? new-config)
                  (= (length new-config-via-emp) 1))
             (make-configs (first new-config-via-emp)
                           a-word
                           lor
                           (append new-config-via-emp visited)
                           (append path new-config-via-emp))]
            [(and (empty? new-config)
                  (> (length new-config-via-emp) 1))
             (make-configs-helper new-config-via-emp
                                  a-word
                                  lor
                                  (append path new-config-via-emp)
                                  (append path new-config-via-emp))]
            [else (append (make-configs-helper new-config
                                               (rest a-word)
                                               lor
                                               (append new-config visited)
                                               (append path new-config))
                          (make-configs-helper new-config-via-emp
                                               a-word
                                               lor
                                               (append new-config-via-emp visited)
                                               (append path new-config-via-emp)))]))))

;;(listof configurations) (listof symbols) (listof rule) (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Ensures that all possible configurations are explored
;;Visited = All configurations that consumed the processed word
;;Path = The configurations that consumed the processed word
(define (make-configs-helper configs a-word lor visited path)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited path)
              (make-configs-helper (rest configs) a-word lor visited path))))

(define (path-maker a-word lor configs)
  (if (empty? configs)
      '()
      (cons (path-constructor a-word lor (first configs) '() (first configs))
              (path-maker a-word lor (rest configs)))))

(define (path-constructor a-word lor config path prev-config)
  (cond [(= (length config) 1) (reverse path)]
        [(and (= (length (second (first config)))
                 (length (second (second config))))
              (empty? (second (first config)))
              (empty? (second (second config))))
         (let* [(similiar-configs (append (list (first config)) (list (second config))))
                (get-rules (remove-duplicates
                            (filter (λ (rules)
                                     (not (member? rules path)))
                                     (append-map
                                      (λ (sim-config)
                                         (filter (λ (rule)
                                                   (or (and (equal? (first prev-config) (first rule))
                                                            (equal? (first sim-config) (third rule)))
                                                       (and (equal? (first (first config)) (first rule))
                                                            (equal? (first (second config)) (third rule))
                                                            (equal? EMP (second rule)))
                                                        #;(or (equal? (last (second prev-config)) (second rule))
                                                            (equal? EMP (second rule)))))
                                                 lor))
                                       similiar-configs))))]
          
             (begin
               ;(display (format "get-rules emp ~s \n" get-rules))
               ;(display (format "(first (first config)) emp ~s \n" (first (first config))))
               ;(display (format "(first (second config)) emp ~s \n" (first (second config))))
               (path-constructor a-word lor (rest config) (append get-rules path) (first config))))]
        [(= (length (second (first config)))
            (length (second (second config))))
         (let* [(similiar-configs (append (list (first config)) (list (second config))))
                (get-rules (remove-duplicates
                            (filter (λ (rules)
                                     (not (member? rules path)))
                                     (append-map
                                      (λ (sim-config)
                                         (filter (λ (rule)
                                                   (or (and (equal? (first prev-config) (first rule))
                                                            (equal? (first sim-config) (third rule)))
                                                       (and (equal? (first (first config)) (first rule))
                                                            (equal? (first (second config)) (third rule))
                                                            (equal? EMP (second rule)))
                                                        #;(or (equal? (last (second prev-config)) (second rule))
                                                            (equal? EMP (second rule)))))
                                                 lor))
                                       similiar-configs))))]
           (begin
             ;(display (format "get-rules ~s \n" get-rules))
             ;(display (format "(first (first config)) ~s \n" (first (first config))))
             ;(display (format "(first (second config)) ~s \n" (first (second config))))
             ;(display (format "prev-config ~s \n" prev-config ))
             #;(display (format "alt style ~s \n" (append
                                                   (path-constructor a-word lor (cons (first config)
                                                                                      (rest (rest config))) (append get-rules '()) prev-config)
                                                   (path-constructor a-word lor (rest config) (append get-rules '()) prev-config)))) 
             #;(remove-duplicates
                (append
                 (path-constructor a-word lor (cons (first config) (rest (rest config))) (append get-rules path) prev-config)
                 (path-constructor a-word lor (rest config) (append get-rules path) prev-config)))
             (append (reverse (append get-rules path))
                     (path-constructor a-word lor (cons (first config) (rest (rest config))) '() prev-config)
                     (path-constructor a-word lor (rest config)                              '() (first config)))))]
        [(empty? (second (first config)))
         (let [(get-rules (filter (λ (rule)
                                    (and (equal? (first (first config)) (first rule))
                                         (equal? (first (second config)) (third rule))
                                         (equal? EMP (second rule))))
                                  lor))]
           (path-constructor a-word lor (rest config) (append get-rules path) (first config)))]
       
        [else (let [(get-rules (filter (λ (rules)
                                         (not (member? rules path)))
                                         (filter (λ (rule)
                                         (and (equal? (first (first config)) (first rule))
                                              (equal? (first (second config)) (third rule))
                                              (or (equal? (first (second (first config))) (second rule))
                                                  (equal? EMP (second rule)))))
                                       lor)))]
               (if (empty? get-rules)
                   '()
               (begin
                  ;(display (format "get-rules else ~s \n" get-rules))
                  ;(display (format "(first (first config)) else ~s \n" (first (first config))))
                  ;(display (format "(first (second config)) else ~s \n" (first (second config))))
                  (path-constructor a-word lor (rest config) (append get-rules path) (first config)))))]))


#;'(((S a S) (S b S) (S b S) (S b S) (S b A) (S a S) (S b S) (S b S) (S b A) (S a S) (S b S) (S b S) (S b A) (S a S) (S b S) (S b A))
    ((S a S) (S b S) (S b S) (S a S) (S b S) (S b S) (A b B) (S a S) (S b S) (S a S) (S b S) (A b B))
    ((S a S) (S b S) (B b B) (S a S) (S b S) (A b B) (B b B)))

#;(append-map (λ (sim-config)
                (filter (λ (rule)
                          (and (equal? (first (first '((S (a a a a))))) (first rule))
                               (equal? (first sim-config) (third rule))
                               (or (equal? (first (second (first '((S (a a a a)))))) (second rule))
                                   (equal? EMP (second rule)))))
                        (sm-rules aa-ab)))
              '((A (a a a)) (B (a a a))))

;;using new method without remove duplicates
#;'(((S b A) (S b S) (S b A) (S b S) (S b A) (S b S) (S b A) (S b S))
    ((S b A) (S b S) (S b A) (S b S) (A b B) (A b B))
    ((S b A) (S b S) (B b B) (S b A) (S b S) (A b B) (B b B)))
;;using remove duplicates
#;'(((S a S) (S b S) (S b A))
    ((S a S) (S b S) (S b A) (A b B))
    ((S a S) (S b S) (S b A) (B b B) (A b B)))
;;informative messaging -> number of computations

(define (add-empties-to-start path lor)
  (if (empty? path)
      '()
      (append-map (λ (rule)
                (if (equal? (first (first path))
                            (third rule))
                    (append rule path)
                    '()))
              lor)))

#|
want to say why every component used for the tool is important
want to implement a bug on bigger machines to emphasis importance of yield and colors
want an example that highlights the importance of zoom
want show a combination of all the features if possible
|#

(define (get-empties-from-start2 start lor)
   (append-map (λ (rule)
          (if (and (equal? (first rule)  start)
                   (equal? (second rule) EMP))
              (append (list rule) (get-empties-from-start2 (third rule) lor))
              '()))
        lor))

#|
curr-path    (((S a A) (S a B) (A b C) (C a E) (E ε S) (C a E) (E ε S)) ((S a A) (S a B) (D ε S) (S a A) (S a B) (B b D) (D ε S) (S a A) (S a B))) 
prev-path    (((S a A) (S a B) (A b C))                                 ((S a A) (S a B) (D ε S) (B b D) (D ε S))) 
current-place                         ((C a E) (E ε S) (C a E) (E ε S)                           (S a A) (S a B) (B b D) (D ε S) (S a A) (S a B)) 
|#
#;(map (λ (curr)
       (map (λ (prev)
              (remove-similiarities prev curr '()))
              '(prev goes here)))
       '(curr goes here))
curr-path (((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B) (A b C) (C a E) (E ε S)) ((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B) (B b D) (D ε S) (S a A) (S a B))) 
prev-path (((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B) (A b C))                 ((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B) (B b D) (D ε S))) 
current-place                                                       ((C a E) (E ε S)                                                                  (S a A) (S a B))


curr-path (((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B) (A b C)) ((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B) (B b D) (D ε S))) 
prev-path (((S a A) (S a B) (A b C) (C a E) (E ε S))                 ((S a A) (S a B) (B b D) (D ε S) (S a A) (S a B))) 
current-place              ((B b D) (D ε S) (S a A) (S a B) (A b C)                                                    (B b D) (D ε S))
|#