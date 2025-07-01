#lang racket
(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../../sm-graph.rkt")

(struct imsg-state (table state-pairs) #:transparent)

(struct state-pair (s1 s2 marked? destination-pairs) #:transparent)

(struct state-pairings (all-pairs) #:transparent)

(struct merged-state (new-symbol old-symbols) #:transparent)

(struct dfa (states alphabet start finals rules no-dead) #:transparent)

(struct phase (number attributes) #:transparent)
#|
viz phases

0 -> only show input machine

1 -> if applicable, remove unreachable states

2 -> show empty transition table and new machine

3 -> mark all final, non-final pairings

4 -> fill the table

5 -> build the new machine from scratch 


|#

(struct minimization-results (new-machine unreachables-removed-M loSP init-states-table) #:transparent)

(define (unchecked->dfa M)
  (dfa (fsa-getstates M)
       (fsa-getalphabet M)
       (fsa-getstart M)
       (fsa-getfinals M)
       (fsa-getrules M)
       'no-dead))



;;dfa dfa -> boolean
;;Purpose: Determines if the two dfa have any changes
(define (machine-changed? old-M new-M)
  (not (= (length (fsa-getstates old-M)) (length (fsa-getstates new-M)))))

;;dfa -> dfa
;;Purpose: If possible minimizes the given dfa, by merging equivalent states and removing unreachable states
(define (minimize-dfa M)
  ;;dfa -> dfa
  ;;Purpose: Removes any unreachable states
  (define (remove-unreachables M)
  ;;state (listof rule) (listof path) -> boolean
  ;;Purpose: Determines if the given state is reachable from the start state
  (define (reachable-from-start? destination rules paths)

    (define e-queue '())
    ;;(queueof X) X -> (queueof X)
    ;;Purpose: Adds the X to the back of the given (queueof X) 
    (define (enqueue queue x)
      (append queue x))

    (define qfirst first)
    ;;(queueof X) -> (queueof X)
    ;;Purpose: Removes the first element of the given (queueof X) 
    (define (dequeue queue)
      (rest queue))

    (define qempty? empty?)
  
    ;; path -> boolean
    ;; Purpose: Determines if the given path has reached the destination state
    (define (reached-destination? path)
      (eq? (third (first path)) destination))
    ;;start destination path -> path
    ;; Purpose: Updates the path to the destination state
    (define (reachable-from-start-helper path)
      (let* ([last-rules-used (first path)]
             [usable-rules (filter (λ (rule) (and (eq? (third last-rules-used) (first rule))
                                                  (not (member rule path))))
                                   rules)])
        (for/list ([last-rule last-rules-used]
                   [connected-rules usable-rules])
          (cons connected-rules path))))
    (cond [(ormap reached-destination? paths) #t]
          [(qempty? paths) #f]
          [else (reachable-from-start? destination
                                       rules
                                       (enqueue (dequeue paths) (reachable-from-start-helper (qfirst paths))))]))
  (let* ([states (fsa-getstates M)]
         [start (fsa-getstart M)]
         [rules (fsa-getrules M)]
         [starter-rules (filter-map (λ (rule) (and (eq? (first rule) start)
                                                   (list rule))) rules)]
         [reachable-states (filter (λ (state) (or (eq? state start)
                                                  (reachable-from-start? state rules starter-rules))) states)]
         [usable-rules (filter (λ (r) (ormap (λ (s) (eq? (first r) s)) reachable-states)) rules)])
    (make-unchecked-dfa reachable-states
                        (fsa-getalphabet M)
                        (fsa-getstart M)
                        (filter (λ (f) (member f reachable-states)) (fsa-getfinals M))
                        usable-rules
                        'no-dead)))
  ;;dfa -> (hash state rules)
  ;;Purpose: Makes a transition table with the states and its applicable rules
  (define (make-transition-table dfa)
    (let ([states (fsa-getstates dfa)]
          [rules (fsa-getrules dfa)])
      (foldl (λ (state hash)
               (let ([applicable-rules (filter-map (λ (rule) (and (eq? state (first rule))
                                                                  (list (second rule) (third rule))))
                                                   rules)])
                 (hash-set hash state applicable-rules)))
             (hash)
             states)))
  ;;dfa -> (listof state-pair)
  ;;Purpose: Makes the state table needed to minimize the dfa
  (define (make-states-table dfa transition-table)
    ;;(listof state-pair) (listof state-pair) -> (listof state-pair)
    ;;Purpose: Makes half of the state-pairing table
    (define (make-half-table loSP new-table)
      (cond [(empty? loSP) new-table]
            [(boolean? (member (first loSP) new-table (λ (sp1 sp2) (and (eq? (state-pair-s1 sp1) (state-pair-s2 sp2))
                                                                        (eq? (state-pair-s2 sp1) (state-pair-s1 sp2))))))
             (make-half-table (rest loSP) (cons (first loSP) new-table))]
            [else (make-half-table (rest loSP) new-table)]))
    (define (make-destination-pairs sp)
      (let* ([ump-s1-transitions (hash-ref transition-table (state-pair-s1 sp))]
             [ump-s2-transitions (hash-ref transition-table (state-pair-s2 sp))]
             [state-pairs-from-transitions (map (λ (x) (let ([s1-tran (filter (λ (tran) (eq? x (first tran))) ump-s1-transitions)]
                                                             [s2-tran (filter (λ (tran) (eq? x (first tran))) ump-s2-transitions)])
                                                         (state-pair (second (first s1-tran)) (second (first s2-tran)) #f 'none)))
                                                (fsa-getalphabet dfa))])
        (struct-copy state-pair sp [destination-pairs state-pairs-from-transitions])))
    (let* ([states (fsa-getstates dfa)]
           [init-states-pairing (for*/list ([s1 states]
                                            [s2 states]
                                            #:unless (eq? s1 s2))
                                  (make-destination-pairs (state-pair s1 s2 #f 'none)))]
           
           [other-half-of-table (make-half-table init-states-pairing '())])
      (filter (λ (sp) (not (member sp other-half-of-table))) init-states-pairing)))
  ;; merged-state state-pair -> boolean
  ;; Purpose: Determines if given state-pair shares at least one state with the given merged-state
  (define (at-least-one-state-matches? merged-state unmarked-pair)
    (or (set-member? (merged-state-old-symbols merged-state) (state-pair-s1 unmarked-pair))
        (set-member? (merged-state-old-symbols merged-state) (state-pair-s2 unmarked-pair))))
  ;;state-pair final-states -> state-pair
  ;;Purpose: Marks if the given state-pair if only one of the states is a final state
  (define (mark-states-table pairing finals)
    (if (or (and (list? (member (state-pair-s1 pairing) finals))
                 (boolean? (member (state-pair-s2 pairing) finals)))
          
            (and (list? (member (state-pair-s2 pairing) finals))
                 (boolean? (member (state-pair-s1 pairing) finals))))
        (struct-copy state-pair pairing [marked? #t])
        pairing))
  ;;(listof state-pairings) transition-table alphabet -> state-pairings
  ;; Purpose: Updates the marks of the given (listof state-pairings) if applicable, terminates when two pairings are identical.
  (define (make-matches loSP transition-table alphabet)
    ;;(listof state-pair) (listof state-pair) (listof state-pair) -> state-pairings
    ;;Purpose: Updates the unmarked-pairs to become marked-pairs if applicable.
    (define (update-pairs marked-pairs unmarked-pairs remaining-unmarked-pairs)
      ;;state-pair (listof state-pair) transition-table alphabet -> boolean
      ;;Purpose: Determines if the given state-pair needs to be marked.
      (define (update-mark? unmarked-pair)
        (ormap (λ (sp) (list? (member sp marked-pairs (λ (sp1 sp2) (or (and (eq? (state-pair-s1 sp1) (state-pair-s1 sp2))
                                                                            (eq? (state-pair-s2 sp1) (state-pair-s2 sp2)))
                                                                       (and (eq? (state-pair-s1 sp1) (state-pair-s2 sp2))
                                                                            (eq? (state-pair-s2 sp1) (state-pair-s1 sp2))))))))
               (state-pair-destination-pairs unmarked-pair)))
      (cond [(empty? unmarked-pairs) (state-pairings (append marked-pairs remaining-unmarked-pairs))]
            [(update-mark? (first unmarked-pairs))
             (update-pairs (cons (struct-copy state-pair (first unmarked-pairs) [marked? #t]) marked-pairs)
                           (rest unmarked-pairs)
                           remaining-unmarked-pairs)]
            [else (update-pairs marked-pairs
                                (rest unmarked-pairs)
                                (cons (first unmarked-pairs) remaining-unmarked-pairs))]))
    ;;(listof state-pairings) (listof state-pairings) -> boolean
    ;; Purpose: Determines if the two given state-pairings are the same
    (define (same-markings? loSP1 loSP2)
      (let ([unmarked-SP1 (filter (λ (sp) (not (state-pair-marked? sp))) loSP1)]
            [unmarked-SP2 (filter (λ (sp) (not (state-pair-marked? sp))) loSP2)]
            [marked-SP1 (filter (λ (sp) (state-pair-marked? sp)) loSP1)]
            [marked-SP2 (filter (λ (sp) (state-pair-marked? sp)) loSP2)])
        (and (andmap (λ (sp) (list? (member sp unmarked-SP2))) unmarked-SP1)
             (andmap (λ (sp) (list? (member sp marked-SP2))) marked-SP1))))
    (if (and (>= (length loSP) 2)
             (same-markings? (state-pairings-all-pairs (first loSP)) (state-pairings-all-pairs (second loSP))))
         loSP
        (let ([marked-pairs (filter (λ (sp) (state-pair-marked? sp)) (state-pairings-all-pairs (first loSP)))]
              [unmarked-pairs (filter (λ (sp) (not (state-pair-marked? sp))) (state-pairings-all-pairs (first loSP)))])
          (make-matches (cons (update-pairs marked-pairs unmarked-pairs '()) loSP)
                        transition-table
                        alphabet))))

  
  ;; (listof state-pairings) dfa transtition-table -> dfa
  ;;Purpose: Converts the (listof state-pairings), dfa, and transition table into an equivalent minimized (if possible) dfa.
  (define (table->dfa loSP old-dfa transition-table)
    ;;state (listof merged-state) -> state
    ;; Purpose: Searches for the merged-state that contains the given state
    (define (search-for-merged-state old-state merged-states)
      (first (filter-map (λ (ms) (and (set-member? (merged-state-old-symbols ms) old-state)
                                      (merged-state-new-symbol ms)))                
                         merged-states)))
    ;;dfa (listof state-pair) (listof merged-state) -> (listof merged-state)
    ;;Purpose: Accumulates the conversion of state-pairs into merged-states
    (define (accumulate-unmarked-pairs unmarked-pairs acc)
      ;; state-pair (listof merged-state) -> boolean
      ;; Purpose: Determines if given state-pair has any overlap (at least one same state) with any state-pair in the given (listof state-pairings)
      (define (overlap? unmarked-pair loSP)
        (ormap (λ (sp) (at-least-one-state-matches? sp unmarked-pair)) loSP))
        ;;dfa state-pair (listof merged-state) -> (listof merged-state)
        ;;Purpose: Merges the state-pair into its overlapping pair and updates the given (listof merged-state) to contain the new merged-state
      (define (merge-pairs unmarked-pair loSP)
        ;;dfa state-pair merged-state -> merged-state
        ;;Purpose: Updates the given merged state to contain the states from the given state-pair
        (define (update-merged-state unmarked-pair overlapped-pair)
          (let* ([start (fsa-getstart old-dfa)]
                 [finals (fsa-getfinals old-dfa)]
                 [ump-s1 (state-pair-s1 unmarked-pair)]
                 [ump-s2 (state-pair-s2 unmarked-pair)]
                 [new-old-symbols-set (set-add (set-add (merged-state-old-symbols overlapped-pair) ump-s1) ump-s2)])
            (cond [(and (or (eq? ump-s1 start) (eq? ump-s2 start))
                        (not (eq? (merged-state-new-symbol overlapped-pair) start)))
                   (struct-copy merged-state overlapped-pair
                                [new-symbol start]
                                [old-symbols new-old-symbols-set])]
                  [(and (or (member ump-s1 finals) (member ump-s2 finals))
                        (not (member (merged-state-new-symbol overlapped-pair) finals)))
                   (struct-copy merged-state overlapped-pair
                                [new-symbol (if (member ump-s1 finals) ump-s1 ump-s2)]
                                [old-symbols new-old-symbols-set])]
                  [else (struct-copy merged-state overlapped-pair [old-symbols new-old-symbols-set])])))
        (let* ([overlapped-pair (first (filter (λ (sp) (at-least-one-state-matches? sp unmarked-pair)) loSP))]
               [new-merged-state (update-merged-state unmarked-pair overlapped-pair)])
          (map (λ (sp) (if (equal? overlapped-pair sp) new-merged-state sp)) loSP)))
        ;;dfa state-pair -> merged-state
        ;;Purpose: Converts a state pair into a merged-state
        (define (make-merged-state unmarked-pair)
          (let ([start (fsa-getstart old-dfa)]
                [finals (fsa-getfinals old-dfa)]
                [ump-s1 (state-pair-s1 unmarked-pair)]
                [ump-s2 (state-pair-s2 unmarked-pair)])
            (cond [(or (eq? ump-s1 start)
                       (eq? ump-s2 start))
                   (merged-state start (set ump-s1 ump-s2))]
                  [(or (member ump-s1 finals)
                       (member ump-s2 finals))
                   (merged-state (if (member ump-s1 finals)
                                     ump-s1
                                     ump-s2)
                                 (set ump-s1 ump-s2))]
                  [else (merged-state ump-s1 (set (state-pair-s1 unmarked-pair) ump-s2))])))
        (cond [(empty? unmarked-pairs) acc]
              [(overlap? (first unmarked-pairs) acc)
               (accumulate-unmarked-pairs (rest unmarked-pairs) (merge-pairs (first unmarked-pairs) acc))]
              [(accumulate-unmarked-pairs (rest unmarked-pairs) (cons (make-merged-state (first unmarked-pairs)) acc))]))
    (let* ([states (fsa-getstates old-dfa)]
           [finals (fsa-getfinals old-dfa)]
           [marked-pairs (filter (λ (sp) (state-pair-marked? sp)) loSP)]
           [unmarked-pairs (filter (λ (sp) (not (state-pair-marked? sp))) loSP)]
           [merged-unmarked-pairs (accumulate-unmarked-pairs unmarked-pairs '())]
           [states-that-were-merged (set->list (foldl (λ (mp acc) (set-remove (set-union acc (merged-state-old-symbols mp))
                                                                              (merged-state-new-symbol mp)))
                                                      (set) merged-unmarked-pairs))]
           [remaining-states (filter (λ (s) (not (member s states-that-were-merged))) states)]
           [new-finals (filter (λ (s) (member s finals)) remaining-states)]
           [table->rules (append-map identity
                                     (hash-map transition-table
                                               (λ (key val)
                                                 (if (list? (member key remaining-states))
                                                     (map (λ (r)
                                                            (if (list? (member (second r) remaining-states))
                                                                (cons key r)
                                                                (list key (first r) (search-for-merged-state (second r) merged-unmarked-pairs))))
                                                          val)
                                                     '()))))])
      (make-unchecked-dfa remaining-states
                          (fsa-getalphabet old-dfa)
                          (fsa-getstart old-dfa)
                          new-finals
                          table->rules
                          'no-dead)))
  (let* ([dfa (remove-unreachables (ndfa->dfa M))]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [init-states-table (make-states-table dfa transition-table)]
         [states-table (map (λ (sp) (mark-states-table sp finals)) init-states-table)]
         [filled-table (make-matches (list (state-pairings states-table)) transition-table (fsa-getalphabet dfa))]
         [new-M (table->dfa (state-pairings-all-pairs (first filled-table)) dfa transition-table)])
    (minimization-results new-M dfa filled-table (state-pairings init-states-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER FONT-SIZE
                                               (list (list ARROW-UP-KEY "Restart")
                                                     (list ARROW-RIGHT-KEY "Forward")
                                                     (list ARROW-LEFT-KEY "Backward")
                                                     (list ARROW-DOWN-KEY "Finish")
                                                     (list CURSOR "Hold to drag")
                                                     (list W-KEY "Zoom in")
                                                     (list S-KEY "Zoom out")
                                                     (list R-KEY "Min zoom")
                                                     (list E-KEY "Mid zoom")
                                                     (list F-KEY "Max zoom"))))

(define imsg-img
  (above (text "Kleenestar of the ndfa" FONT-SIZE 'black)
         (text (format "Generated starting state:") FONT-SIZE 'black)
         (text (format "Added edges:") FONT-SIZE 'black)))

(define E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height imsg-img)
                          (image-height E-SCENE-TOOLS)))
(define MIDDLE-E-SCENE (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))

(define RULE-YIELD-DIMS
  (bounding-limits 0
                   (image-width imsg-img)
                   E-SCENE-HEIGHT
                   (+ E-SCENE-HEIGHT (image-height imsg-img))))

(create-bounding-limits E-SCENE-WIDTH E-SCENE-HEIGHT (image-width E-SCENE-TOOLS) RULE-YIELD-DIMS FONT-SIZE LETTER-KEY-WIDTH-BUFFER INS-TOOLS-BUFFER
                        ((ARROW-UP-KEY "Restart")
                         (ARROW-RIGHT-KEY "Forward")
                         (ARROW-LEFT-KEY "Backward")
                         (ARROW-DOWN-KEY "Finish")
                         (CURSOR "Hold to drag")
                         (W-KEY "Zoom in")
                         (S-KEY "Zoom out")
                         (R-KEY "Min zoom")
                         (E-KEY "Mid zoom")
                         (F-KEY "Max zoom")))


(define (make-table M)
  (make-table-helper (dfa-states M) (add1 (length (dfa-states M))))
  #;(let ([M (unchecked->dfa M)])
    (make-table-helper (dfa-states M) (add1 (length (dfa-states M))))))

(define (make-table-helper states num-rows)
  (build-vector num-rows (λ (row-num)
                           (build-vector num-rows
                                         (λ (col-num) (let ([blank-tile-count (- (length states) (- num-rows row-num))])
                                                        (cond [(= row-num 0) (if (= col-num 0) 'blank (list-ref states (sub1 col-num)))]
                                                              [(= col-num 0) (list-ref states (sub1 row-num))]
                                                              [(<= col-num blank-tile-count) 'blank]
                                                              [else 'black])))))))

(define (look-up-in-table table row column)
  (vector-ref (vector-ref table column) row))


(define (draw-table table finals)
  (define (draw-table-helper row-amount idx)
    (if (= row-amount idx)
        (make-row (vector-ref table idx) 0)
        (above (make-row (vector-ref table idx) 0)
               (draw-table-helper row-amount (add1 idx)))))
  (define (make-row row idx)
    (define (draw-square sym)
      (let ([base-square-img (overlay (square 40 'solid 'white) (square 45 'solid 'gray))]
            [final-state-square-img (overlay (square 40 'solid 'lightorange) (square 45 'solid 'gray))])
        (match sym
          ['blank base-square-img]
          ['black (overlay (square 40 'solid 'black) (square 45 'solid 'gray))]
          ['new-mark (overlay (text "X" 38 'red) base-square-img)]
          ['mark (overlay (text "X" 38 'black) base-square-img)]
          [_ (overlay (text (symbol->string sym) 38 'black) (if (member sym finals) final-state-square-img base-square-img))])))
    (if (= (sub1 (vector-length row)) idx)
        (draw-square (vector-ref row idx))
        (beside (draw-square (vector-ref row idx)) (make-row row (add1 idx)))))
  (draw-table-helper (sub1 (vector-length table)) 0))

(define (make-main-graphic M)
  (beside (sm-graph M)
          (square 10 'solid 'white)
          (draw-table (make-table M) (fsa-getfinals M))))


;; imsg-state -> image
(define (draw-imsg a-imsg-state)
  (above (text "TBD" 20 'black)
         (text "TBD" 20 'black)))

(define (make-info-messages M)
  (above (text "TBD" 20 'black)
         (text "TBD" 20 'black)))


(define (make-viz-demo M)
  (overlay (above (make-main-graphic M)
                  (square 60 'solid 'white)
                  (make-info-messages M)
                  (square 40 'solid 'white)
                  E-SCENE-TOOLS)
                  (empty-scene E-SCENE-WIDTH E-SCENE-HEIGHT)))


(define viz-go-next
  (go-next E-SCENE-WIDTH
           E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))
(define viz-go-prev
  (go-prev E-SCENE-WIDTH
           E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))
(define viz-go-to-begin
  (go-to-begin E-SCENE-WIDTH
               E-SCENE-HEIGHT
               NODE-SIZE
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM-FLOOR
               PERCENT-BORDER-GAP))
(define viz-go-to-end
  (go-to-end E-SCENE-WIDTH
             E-SCENE-HEIGHT
             NODE-SIZE
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             PERCENT-BORDER-GAP))

(define viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))
(define viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))
(define viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))
(define viz-max-zoom-in
  (max-zoom-in E-SCENE-WIDTH
               E-SCENE-HEIGHT
               ZOOM-INCREASE
               ZOOM-DECREASE
               NODE-SIZE
               PERCENT-BORDER-GAP
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM))
(define viz-reset-zoom
  (reset-zoom E-SCENE-WIDTH
              E-SCENE-HEIGHT
              ZOOM-INCREASE
              ZOOM-DECREASE
              NODE-SIZE
              PERCENT-BORDER-GAP
              DEFAULT-ZOOM-CAP
              DEFAULT-ZOOM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph los s f)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color
                                 (cond
                                   [(eq? state s) 'darkgreen]
                                   [else 'black])
                                 'shape
                                 (if (member state f) 'doublecircle 'circle)
                                 'label
                                 (if (equal? state '()) 'ds state)
                                 'fontcolor
                                 'black
                                 'font
                                 "Sans")))
         graph
         los))
;; make-init-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-init-edge-graph graph M new-start)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (if (equal? (first rule) '()) 'ds (first rule))
                     (if (equal? (third rule) '()) 'ds (third rule))
                     #:atb (hash 'fontsize
                                 20
                                 'style
                                 'solid
                                 'color
                                 (cond
                                   [(member rule (dfa-rules M)) 'violet]
                                   [else 'black]))))
         graph
         (dfa-rules M)))

;; make-init-grph-structure
;; ndfa -> dgraph
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-structure M)
  (let* ([graph (make-init-edge-graph
                 (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                  (dfa-states M)
                                  (dfa-start M)
                                  (dfa-finals M))
                 M
                 (dfa-start M))])
    graph))

;; ndfa -> graph
;; Creates the graph structure used to create the initially displayed graphic
(define (create-init-graph-struct M)
  (make-init-edge-graph (make-node-graph (create-graph 'dgraph
                                                       #:atb (hash 'rankdir "LR" 'font "Sans"))
                                         (dfa-states M)
                                         (dfa-start M)
                                         (dfa-finals M))
                        M
                        (dfa-start M)))

(define (make-img M)
  (beside (graph->bitmap (make-init-grph-structure M))
          (square 10 'solid 'white)
          (draw-table (make-table M) (dfa-finals M))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;minimization-viz
;; fsa -> void
(define (minimization-viz M)
  (let* ([unchecked-M M]
         [results-from-minimization (minimize-dfa unchecked-M)]
         [no-unreachables-M (minimization-results-unreachables-removed-M results-from-minimization)]
         [minimized-M (minimization-results-new-machine results-from-minimization)]
         [has-unreachables? (machine-changed? unchecked-M no-unreachables-M)]
         [can-be-minimized? (machine-changed? unchecked-M minimized-M)]
         [M (unchecked->dfa M)]
         [state-table-mappings (for/hash ([state (dfa-states M)]
                                          [num (in-naturals)])
                                 (values state (add1 num)))]
         [state-pairing-table (make-table M)])
    
    results-from-minimization
    
  #;(run-viz (list (create-init-graph-struct M))
           (lambda () (make-img M))
           MIDDLE-E-SCENE
           DEFAULT-ZOOM
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           (informative-messages draw-imsg
                                 (imsg-state 0 0)

                                  #;(let ([new-start (generate-symbol 'K (sm-states M))])
                                   (imsg-state 0
                                               new-start
                                               (list (list new-start EMP (sm-start M))
                                                     (map (λ (f) (list f EMP new-start))
                                                          (sm-finals M)))))
                                 RULE-YIELD-DIMS)
           (instructions-graphic E-SCENE-TOOLS
                                 (bounding-limits 0
                                                  (image-width imsg-img)
                                                  E-SCENE-HEIGHT
                                                  (+ E-SCENE-HEIGHT (image-height imsg-img))))
           (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
           (create-viz-process-key ["right" viz-go-next identity #;right-key-pressed]
                                   ["left" viz-go-prev identity #;left-key-pressed]
                                   ["up" viz-go-to-begin identity #;up-key-pressed]
                                   ["down" viz-go-to-end identity #;down-key-pressed]
                                   ["w" viz-zoom-in identity]
                                   ["s" viz-zoom-out identity]
                                   ["r" viz-max-zoom-out identity]
                                   ["f" viz-max-zoom-in identity]
                                   ["e" viz-reset-zoom identity]
                                   ["wheel-down" viz-zoom-in identity]
                                   ["wheel-up" viz-zoom-out identity])
           (create-viz-process-tick E-SCENE-BOUNDING-LIMITS
                                    NODE-SIZE
                                    E-SCENE-WIDTH
                                    E-SCENE-HEIGHT
                                    CLICK-BUFFER-SECONDS
                                    ()
                                    ( [ARROW-UP-KEY-DIMS viz-go-to-begin identity #;up-key-pressed]
                                      [ARROW-DOWN-KEY-DIMS viz-go-to-end identity #;down-key-pressed]
                                      [ARROW-LEFT-KEY-DIMS viz-go-prev identity #;left-key-pressed]
                                      [ARROW-RIGHT-KEY-DIMS viz-go-next identity #;right-key-pressed]
                                      [W-KEY-DIMS viz-zoom-in identity]
                                      [S-KEY-DIMS viz-zoom-out identity]
                                      [R-KEY-DIMS viz-max-zoom-out identity]
                                      [E-KEY-DIMS viz-reset-zoom identity]
                                      [F-KEY-DIMS viz-max-zoom-in identity]))
           'minimization-viz)))


(define EX4 (make-unchecked-dfa '(A B C D E F G)
                      '(0 1)
                      'A
                      '(B C G)
                      '((A 0 B) (A 1 C)
                        (B 0 D) (B 1 E)
                        (C 0 E) (C 1 D)
                        (D 0 G) (D 1 G)
                        (E 0 G) (E 1 G)
                        (F 0 D) (F 1 E)
                        (G 0 G) (G 1 G))
                      'no-dead))

(define EX5 (make-unchecked-dfa '(A B C D E F G H I)
                      '(0 1)
                      'A
                      '(B C G)
                      '((A 0 B) (A 1 C)
                        (B 0 D) (B 1 E)
                        (C 0 E) (C 1 D)
                        (D 0 G) (D 1 G)
                        (E 0 G) (E 1 G)
                        (F 0 D) (F 1 E)
                        (H 0 F) (H 1 I)
                        (I 0 H) (I 1 F)
                        (G 0 G) (G 1 G))
                      'no-dead))


(define EX6 (make-unchecked-dfa '(A B C D E)
                    '(0 1)
                    'A
                    '(E)
                    '((A 0 B) (A 1 C)
                              (B 0 B) (B 1 D)
                              (C 0 B) (C 1 C)
                              (D 0 B) (D 1 E)
                              (E 0 B) (E 1 C))
                    'no-dead))