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

;; graph machine (listof states)-> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M current-states destin-states)
  (foldl (λ (state result)
           (cond [(and (member state destin-states) 
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color 'purple
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 [(member state destin-states)
                  (add-node
                   result
                   state
                   #:atb (hash 'color 'purple
                               'shape 'circle
                               'label state
                               'fontcolor 'black))]
                 [(and (member state current-states) 
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color 'blue
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 [(member state current-states)
                  (add-node
                   result
                   state
                   #:atb (hash 'color 'blue
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
                       (member rule (get-empties (sm-rules M))))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color 'blue 'fontsize 20 'style 'solid))]
                 [(and (empty? pci)
                       (ormap (λ (p) (and (equal? (first rule) (first p))
                                          (not (equal? (second rule) EMP))))
                              current-place))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'fontsize 20 'style 'solid))]
                 [(and (not (empty? pci)) (equal? (second rule) EMP)
                       (equal? (first rule) (sm-start M)) (member rule prev-path))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color 'lightblue 'fontsize 20 'style 'solid))]
                 [(or (member rule current-place)
                      (and (ormap (λ (p) (equal? (first rule) (first p)))
                             current-place)
                           (ormap (λ (w) (equal? (second rule) w))
                             pci)))
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color 'blue 'fontsize 20 'style 'solid))]
                 [(member rule prev-path)
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'color 'lightblue 'fontsize 20 'style 'solid))]
                 [else
                  (add-edge result
                            (second rule)
                            (first rule)
                            (third rule)
                            #:atb (hash 'fontsize 20 'style 'solid))]))
         cgraph
         (sm-rules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define (get-rules-from-word2 lor word)
  (append-map (λ (w)
                (filter (λ (rule) (equal? (second rule) w))
                        lor))
              word))

;;symbol (listof rules) (listof rules) word -> (listof rules)
;;Purpose: Returns all paths whose length is greater than or
;;         equal to the length of the word
(define (path-find start lost rfw word)
  (append-map (λ (path)
                (if (>= (length path) (length word))
                            path
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
          [(= (length cp-frm-first) (length word))
           cp-frm-first]
          [else (construct-path-from-destin destin word (rest rfw))]))
  (if (or (empty? word) (empty? rfw))
      '()
    (construct-path destin word (first rfw) (rest rfw))))
;;(listof rules) (listof rules) (listof rules) -> (listof rules)
;;Purpose: Returns the complement of the intersection
;;         between the lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (return-differences prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(equal? (first prev-path) (first curr-path))
         (return-differences (rest prev-path) (rest curr-path) acc)]
        [(return-differences prev-path (rest curr-path)
                             (append acc (list (first curr-path))))]))



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

;;viz-state -> scene
;;Purpose: Draws the given viz-state onto the scene
(define (draw-graph a-vs)
  (let* [;;(listof rules)
         ;;Purpose: Returns all viable paths using the proccessed CI
         (curr-path (if (empty? (viz-state-ndfa-pci a-vs))
                        (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                (sm-rules (viz-state-ndfa-M a-vs)))
                        (path-find (sm-start (viz-state-ndfa-M a-vs))
                                    (append (get-readables-from-start
                                             (sm-start (viz-state-ndfa-M   a-vs))
                                             (sm-rules (viz-state-ndfa-M   a-vs))
                                                       (viz-state-ndfa-pci a-vs))
                                            (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                                    (sm-rules (viz-state-ndfa-M a-vs))))
                                    (get-rules-from-word2 (sm-rules (viz-state-ndfa-M   a-vs))
                                                                    (viz-state-ndfa-pci a-vs))
                                    (viz-state-ndfa-pci a-vs))))
         ;;(listof rules)
         ;;Purpose: Returns the all viable paths using the
         ;;         proccessed CI that is one step behind
         (full-prev-path (cond [(empty?    (viz-state-ndfa-pci a-vs)) '()]
                               [(= (length (viz-state-ndfa-pci a-vs)) 1)
                                (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                        (sm-rules (viz-state-ndfa-M a-vs)))]
                               [else (path-find (sm-start (viz-state-ndfa-M a-vs))
                                                 (append (get-readables-from-start (sm-start (viz-state-ndfa-M   a-vs))
                                                                                   (sm-rules (viz-state-ndfa-M   a-vs))
                                                                                   (take     (viz-state-ndfa-pci a-vs)
                                                                                             (sub1 (length (viz-state-ndfa-pci a-vs)))))
                                                         (get-empties-from-start (sm-start (viz-state-ndfa-M a-vs))
                                                                                 (sm-rules (viz-state-ndfa-M a-vs))))
                                                 (get-rules-from-word2 (sm-rules (viz-state-ndfa-M   a-vs))
                                                                       (take     (viz-state-ndfa-pci a-vs)
                                                                             (sub1 (length (viz-state-ndfa-pci a-vs)))))
                                                 (take (viz-state-ndfa-pci a-vs)
                                                       (sub1 (length (viz-state-ndfa-pci a-vs)))))]))
         ;;(listof rules)
         ;;Purpose: All paths that have similiarities with
         ;;         the current path
         (prev-path (filter (λ (p) (member p curr-path))
                            full-prev-path))
         ;;(listof rules)
         ;;Purpose: The current rules that the ndfa is using to consume the proccess CI
         (current-place (return-differences prev-path curr-path '()))
         ;;(listof symbols)
         ;;Purpose: The current state that the ndfa is in
         (current-states (remove-duplicates
                          (map (λ (s) (first s))
                               current-place)))
         ;;(listof symbols)
         ;;Purpose: The current state that the ndfa is in
         (destin-states (remove-duplicates
                          (map (λ (s) (last s))
                               current-place)))]
    (cond [(and (empty? (viz-state-ndfa-upci a-vs))
                (empty? (viz-state-ndfa-pci a-vs)))
           (above (graph->bitmap
                   (edge-graph
                    (node-graph (create-graph 'ndfagraph #:atb
                                              (hash 'rankdir "LR"))
                                (viz-state-ndfa-M a-vs)
                                current-states
                                destin-states)
                    (viz-state-ndfa-M a-vs)
                    (viz-state-ndfa-pci a-vs)
                    current-place
                    prev-path))
                  E-SCENE-TOOLS)]
          [(empty? (viz-state-ndfa-upci a-vs))
           (above (text (los2str (viz-state-ndfa-pci a-vs))
                        20
                        'gray)
                  (above (graph->bitmap
                          (edge-graph
                           (node-graph (create-graph 'ndfagraph #:atb
                                              (hash 'rankdir "LR"))
                                (viz-state-ndfa-M a-vs)
                                current-states
                                destin-states)
                           (viz-state-ndfa-M a-vs)
                           (viz-state-ndfa-pci a-vs)
                           current-place
                           prev-path))
                         E-SCENE-TOOLS))]
          [(empty? (viz-state-ndfa-pci a-vs))
           (above (text (los2str (viz-state-ndfa-upci a-vs))
                        20
                        'black)
                  (above (graph->bitmap
                          (edge-graph
                           (node-graph (create-graph 'ndfagraph #:atb
                                              (hash 'rankdir "LR"))
                                (viz-state-ndfa-M a-vs)
                                current-states
                                destin-states)
                           (viz-state-ndfa-M a-vs)
                           (viz-state-ndfa-pci a-vs)
                           current-place
                           prev-path))
                         E-SCENE-TOOLS))]
          [else (above (beside (text (los2str (viz-state-ndfa-pci a-vs))
                                     20
                                     'gray)
                               (text (los2str (viz-state-ndfa-upci a-vs))
                                     20
                                     'black))
                       (above (graph->bitmap
                               (edge-graph
                                (node-graph (create-graph 'ndfagraph #:atb
                                              (hash 'rankdir "LR"))
                                (viz-state-ndfa-M a-vs)
                                current-states
                                destin-states)
                                (viz-state-ndfa-M a-vs)
                                (viz-state-ndfa-pci a-vs)
                                current-place
                                prev-path))
                              E-SCENE-TOOLS))])))

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

;;ndfa word -> (void)
;;Purpose: Visualizes the given ndfa processing the given word
(define (ndfa-viz M a-word)
  (run-viz-ndfa
   (viz-state-ndfa a-word
                   '()
                   M)
   'ndfa-viz))

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
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))
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

(define missing-exactly-one (make-ndfa '(S A B C D E F G H I J K L M N O P)
                                       '(a b c)
                                       'S
                                       '(E G I K M O)
                                       `((S ,EMP A)
                                         (S ,EMP B)
                                         (S ,EMP C)
                                         (A b D)
                                         (A c F)
                                         (B a H)
                                         (B b J)
                                         (C a L)
                                         (C c N)
                                         (D b D)
                                         (D c E)
                                         (F c F)
                                         (F b G)
                                         (H a H)
                                         (H b I)
                                         (J b J)
                                         (J a K)
                                         (L a L)
                                         (L c M)
                                         (N c N)
                                         (N a O)
                                         (E c E)
                                         (E b E)
                                         (E a P)
                                         (G b G)
                                         (G c G)
                                         (G a P)
                                         (I b I)
                                         (I a I)
                                         (I c P)
                                         (K a K)
                                         (K b K)
                                         (K c P)
                                         (M a M)
                                         (M c M)
                                         (M b P)
                                         (O a O)
                                         (O c O)
                                         (O b P)
                                         (P a P)
                                         (P b P)
                                         (P c P))))

#;(let [(res (graph->bitmap cgraph (current-directory) FNAME))]
    (begin
      (delete-file (string-append FNAME ".dot"))
      (delete-file (string-append FNAME ".png"))
      res))