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

(define destin-color 'blue)
(define curr-color 'purple)
;; graph machine (listof states)-> graph
;; Purpose: To create a graph of nodes from the given list of rules
(define (node-graph cgraph M current-states destin-states pci)
  (foldl (λ (state result)
           (cond [(and (empty? pci)
                       (equal? state (sm-start M))
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color curr-color
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 [(and (empty? pci)
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
                 [(and (member state current-states) 
                       (member state (sm-finals M)))
                  (add-node
                   result
                   state
                   #:atb (hash 'color curr-color
                               'shape 'doublecircle
                               'label state
                               'fontcolor 'black))]
                 [(member state current-states)
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
  (if (not (> (length curr-path) (length prev-path)))
      curr-path
      (remove-similiarities prev-path curr-path '())))

;;(listof rules) (listof rules) (listof rules) -> (listof rules)
;;Purpose: Returns the complement of the intersection
;;         between the lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similiarities prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(empty? curr-path) acc]
        [(equal? (first prev-path) (first curr-path))
         (remove-similiarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similiarities prev-path (rest curr-path)
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
         ;;Purpose: All paths reachable on an empty transition from start
         (starting-empties (append-map (λ (path) (attach-empties (list path) (sm-rules (viz-state-ndfa-M a-vs))))
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
                        (append-map (λ (rule) (attach-empties rule (sm-rules (viz-state-ndfa-M a-vs))))
                             (path-maker (viz-state-ndfa-pci a-vs)
                                    (sm-rules (viz-state-ndfa-M a-vs))
                                    (get-configs (viz-state-ndfa-pci a-vs)
                                                 (sm-rules (viz-state-ndfa-M a-vs))
                                                 (sm-start (viz-state-ndfa-M a-vs)))))))
         
         
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
                                     (append-map (λ (rule) (attach-empties rule (sm-rules (viz-state-ndfa-M a-vs))))
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
         (prev-path (map last full-prev-path))
                    
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
                          (display (format "curr-path ~s \n" curr-path))
                          (display (format "full-prev-path ~s \n" full-prev-path))
                          (return-differences full-prev-path curr-path)))
         #;(map (λ (path)
                  (filter (λ (p) (equal? (last (viz-state-ndfa-pci a-vs))
                                         (second p)))
                          path))
                curr-path)
         ;;(listof symbols)
         ;;Purpose: The current state that the ndfa is in
         (current-states (remove-duplicates
                          (map first
                               current-place)))
         ;;(listof symbols)
         ;;Purpose: The current state that the ndfa is in
         (destin-states (remove-duplicates
                         (map last
                              current-place)))]
    (cond [(and (empty? (viz-state-ndfa-upci a-vs))
                (empty? (viz-state-ndfa-pci a-vs)))
           (above (graph->bitmap
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
                                       destin-states
                                       (viz-state-ndfa-pci a-vs))
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
                                       destin-states
                                       (viz-state-ndfa-pci a-vs))
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
                                            destin-states
                                            (viz-state-ndfa-pci a-vs))
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

;;sample configurations uses empties from start
;;ex using p2-ndfa -> word is (a b a)
;;all empties from S -> consuming a -> consuming b -> consuming a
;;(S (a b a)) -> 
;;(D (a b a)) -> (E (b a))
;;(A (a b a)) -> (B (b a)) -> (A (a)) -> (B ())
;;(C (a b a)) ->
;;have an acc to track visited configs 

;;symbol (listof rules) -> (listof states)
;;Purpose: Returns all states that have an empty transitions from the start state
(define (get-empty-states-from-start start lor)
  (flatten (map (λ (rule)
                  (if (and (equal? (first rule)  start)
                           (equal? (second rule) EMP))
                      (cons (third rule) (list (get-empty-states-from-start (third rule) lor)))
                      '()))
                lor)))

(define (get-configs a-word lor start)
  (let* [(empty-states-frm-start (cons start (get-empty-states-from-start start lor)))
         (configs (map (λ (state) (append (list state) (list a-word)))
                       empty-states-frm-start))]
    (filter (λ (config)
              (empty? (last (last config))))
            (remove-duplicates (get-configs-helper configs a-word lor configs)))))

(define (get-configs-helper configs a-word lor visited)
  (cond [(empty? configs) '()]
        ;[(empty? a-word) configs]
        [else (append (make-configs (first configs) a-word lor visited (list (first configs)))
                      (get-configs-helper (rest configs) a-word lor visited))]))

(define (make-configs config a-word lor visited path)
  (let* [(connected-rules (if (empty? a-word)
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

(define (make-configs-helper configs a-word lor visited path)
  (if (empty? configs)
      '()
      (append (make-configs (first configs) a-word lor visited path)
              (make-configs-helper (rest configs) a-word lor visited path))))

(define (path-maker a-word lor configs)
  (if (empty? configs)
      '()
      (cons (path-constructor a-word lor (first configs) '() '())
            (path-maker a-word lor (rest configs)))))

(define (path-constructor a-word lor config path prev-config)
  (cond [(= (length config) 1) (reverse path)]
        [(= (length (second (first config)))
            (length (second (second config))))
         (let* [(similiar-configs (append (list (first config)) (list (second config))))
                (get-rules (append-map (λ (sim-config)
                                         (filter (λ (rule)
                                                   (and (equal? (first prev-config) (first rule))
                                                        (equal? (first sim-config) (third rule))
                                                        (or (equal? (first (second prev-config)) (second rule))
                                                            (equal? EMP (second rule)))))
                                                 lor))
                                       similiar-configs))]
           (begin
             ;(display (format "get-rules ~s \n" get-rules))
             ;(display (format "prev-config ~s \n" prev-config ))
             (remove-duplicates
              (append
               (path-constructor a-word lor (cons (first config) (rest (rest config))) (append get-rules path) prev-config)
               (path-constructor a-word lor (rest config) (append get-rules path) prev-config)))))]
        [(empty? (second (first config)))
         (let [(get-rules (filter (λ (rule)
                                    (and (equal? (first (first config)) (first rule))
                                         (equal? (first (second config)) (third rule))
                                         (or (equal? (last a-word) (second rule))
                                             (equal? EMP (second rule)))))
                                  lor))]
           (path-constructor a-word lor (rest config) (append get-rules path) (first config)))]
        #;[(= (length (second (first config)))
              (length (second (second config))))
           (let* [(similiar-configs (append (list (first config)) (list (second config))))
                  (get-rules (append-map (λ (sim-config)
                                           (filter (λ (rule)
                                                     (and (equal? (first prev-config) (first rule))
                                                          (equal? (first sim-config) (third rule))
                                                          (or (equal? (first (second prev-config)) (second rule))
                                                              (equal? EMP (second rule)))))
                                                   lor))
                                         similiar-configs))]
             (append (path-constructor a-word lor (cons (first config) (rest (rest config))) (append get-rules path) prev-config)
                     (path-constructor a-word lor (rest config) (append get-rules path) prev-config)))]
        [else (let [(get-rules (filter (λ (rule)
                                         (and (equal? (first (first config)) (first rule))
                                              (equal? (first (second config)) (third rule))
                                              (or (equal? (first (second (first config))) (second rule))
                                                  (equal? EMP (second rule)))))
                                       lor))]
                (path-constructor a-word lor (rest config) (append get-rules path) (first config)))]))

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
;;informative messaging -> number of computations 