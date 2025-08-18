#lang racket
(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/colors.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/zipper.rkt")

(provide minimization-viz test-colors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONSTANTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BLANK-SPACE 'blank)

(define BLACK 'black)

(define NEW-MARK 'new-mark)

(define MARK 'mark)

(define e-queue '())

(define qfirst first)

(define qempty? empty?)

(define FONT-SIZE 20)

(define sigma-list (append '(a b c d e f g h i j k l m n o p q r s t u v w x y z) (range 10)))

(define base-color-list (list 'darkslategray 'olive 'aqua 'midnightblue 'lime 'palegreen1 'peru 'darkorchid4 'red2 'rosybrown1 'royalblue
                              'salmon 'seagreen1 'steelblue2 'rebeccapurple 'crimson 'deeppink 'fuchsia 'deepskyblue3 'cornflowerblue
                              'indigo 'hotpink 'blueviolet 'darkslateblue 'chocolate 'firebrick2 'lightslateblue 'sienna1 'dodgerblue3 'springgreen
                              'teal 'darksalmon 'tomato 'darkviolet 'orchid 'lightcoral))

(define sigma-color-pairings (foldl (λ (sig color ht)
                                      (hash-set ht sig color))
                                    (hash)
                                    sigma-list
                                    base-color-list))
                                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;STRUCTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;phase | all of the phases needed for the visualization | (zipperof phase)  
(struct imsg-state (phase) #:transparent)

;;s1      | the first of the state in the state pair  | state
;;s2      | the second of the state in the state pair | state
;;marked? | Determines if the state pair should be marked in the table | boolean
;;destination-pairs | The state pairs that s1 and s2 transition to | (listof state-pair)
(struct state-pair (s1 s2 marked? destination-pairs) #:transparent)

;;all-pairs | all of the state-pairs after each iteration of filling the table | (listof state-pair) 
(struct state-pairings (all-pairs) #:transparent)

;;new-symbol | the new state symbol that represents all of the states that got merged | symbol
;;old-symbols | all of the symbols that merged into one state | (listof state)
(struct merged-state (new-symbol old-symbols) #:transparent)

;;new-machine | the *possibly* minimized machine that resulted from the minimize-dfa |fsa
;;unreachables-removed-M | the machine with unreachable states removed | fsa
;;loSP | all of the state-pairings that was created and used for the minimization algorithm |(listof state-pairings)
;;merged-states | all of merged-state created from the minimization algorithm | (listof merged-state)
(struct minimization-results (original-M new-machine unreachables-removed-M loSP merged-states) #:transparent)

;;states | the states of the dfa | (listof state)
;;alphabet | the alphabet that the dfa reads | (list of symbol)
;;start | the starting state of the dfa | state
;;finals | the final states of the dfa| (listof finals)
;;rules | the rules that the dfa must follow | (listof rule)
;;no-dead | the symbol of the dead state | symbol
(struct dfa (states alphabet start finals rules deterministic? no-dead) #:transparent)

;;number | the number denoting the phase | natnum
;;M | the dfa associated with the given phase and step | dfa
;;state-pairing-table | the pairing-table associated with the given phase and step | (vectorof (vectorf marking))
;;attributes | extra data needed for the phase | phase-#-attribute
(struct phase (number M state-pairing-table attributes) #:transparent)

(struct phase-0-attributes () #:transparent)

;;unreachable-state | the states that were deemed unreachable from the start state | (listof state)
(struct phase-1-attributes (unreachable-state) #:transparent)

(struct phase-2-attributes () #:transparent)

;;initial-pairings | the final X non-final state pairings needed for the minimization algorithm | (listof of state-pair)
(struct phase-3-attributes (initial-pairings) #:transparent)

;;unmarked-pair | all of the pairings used in the algorithm | (listof state-pair)
(struct phase-4-attributes (unmarked-pair) #:transparent)

;;merged-stated | all of the states that got merged into another state | (listof merged-state)
(struct phase-5-attributes (rebuild-M merged-states remaining-states) #:transparent)

;;minimized? | determines if the machine has been minimized | boolean
(struct phase-6-attributes (minimized?) #:transparent)

;;loPhase | the accumulated phases created from reenacting the minimization algorithm | (listof phase)
;;new-table | the finalized table after completing a phase | (vectorof (vectorof marking))
(struct phase-results (loPhase new-table) #:transparent)

#|
viz phases

0 -> only show input machine
imsg "Original machine"

.5 -> coonvert to dfa
ismg Converting Machine into a dfa

1 -> if applicable, remove unreachable states
imsg "States ... are unreachable and have been removed"

2 -> show empty state-pairing table and new machine
imsg "Creating state pairings table"

3 -> mark all final, non-final pairings
imsg "Marking state pairing that contain one final and one non-final state"

4 -> fill the table
ismg "marking the unmarked state pairings"

5 -> build the new machine from scratch 
imsg "building the new machine"

6 -> finalized machine
ismg "finished machine"
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MINIMIZATION ALGORITHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;unchecked-dfa -> dfa-struct
;;Purpose: Converts the given unchecked-dfa to a dfa struct
(define (unchecked->dfa M)
  (dfa (fsa-getstates M)
       (fsa-getalphabet M)
       (fsa-getstart M)
       (fsa-getfinals M)
       (fsa-getrules M)
       (M null 'is-deterministic?)
       'no-dead))


;;(queueof X) (queue X) -> (queueof X)
;;Purpose: Adds the X to the back of the given (queueof X) 
(define (enqueue queue x)
  (append queue x))

;;(queueof X) -> (queueof X)
;;Purpose: Removes the first element of the given (queueof X) 
(define (dequeue queue)
  (rest queue))

;;dfa -> dfa
;;Purpose: If possible minimizes the given dfa, by merging equivalent states and removing unreachable states
(define (minimize-dfa M)
  ;;dfa -> dfa
  ;;Purpose: Removes any unreachable states
  (define (remove-unreachables M)
    ;;state (listof rule) (listof path) -> boolean
    ;;Purpose: Determines if the given state is reachable from the start state
    (define (reachable-from-start? destination rules paths)  
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
                          (filter (λ (final) (member final reachable-states)) (fsa-getfinals M))
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
    ;;state-pair -> state-pair
    ;;Purpose: Creates the destination state-pairs using the given state-pair and destination state-pairs to the orignal-state-pair
    (define (make-destination-pairs sp)
      (let* ([ump-s1-transitions (hash-ref transition-table (state-pair-s1 sp))]
             [ump-s2-transitions (hash-ref transition-table (state-pair-s2 sp))]
             [state-pairs-from-transitions (map (λ (x) (let ([s1-tran (filter (λ (tran) (eq? x (first tran))) ump-s1-transitions)]
                                                             [s2-tran (filter (λ (tran) (eq? x (first tran))) ump-s2-transitions)])
                                                         (state-pair (second (first s1-tran)) (second (first s2-tran)) #f x)))
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
           [table->rules (apply append
                                     (hash-map transition-table
                                               (λ (key val)
                                                 (if (list? (member key remaining-states))
                                                     (map (λ (r)
                                                            (if (list? (member (second r) remaining-states))
                                                                (cons key r)
                                                                (list key (first r)
                                                                      (search-for-merged-state (second r) merged-unmarked-pairs))))
                                                          val)
                                                     '()))))])
      (list (make-unchecked-dfa remaining-states
                                (fsa-getalphabet old-dfa)
                                (fsa-getstart old-dfa)
                                new-finals
                                table->rules
                                'no-dead)
            merged-unmarked-pairs)))
  (let* ([dfa (remove-unreachables (ndfa->dfa M))]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [init-states-table (make-states-table dfa transition-table)]
         [states-table (map (λ (sp) (mark-states-table sp finals)) init-states-table)]
         [filled-table (make-matches (list (state-pairings states-table)) transition-table (fsa-getalphabet dfa))]
         [new-M (table->dfa (state-pairings-all-pairs (first filled-table)) dfa transition-table)])
    (minimization-results M (first new-M) dfa filled-table (second new-M))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VIZ-SCENE-STUFF;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VIZ-FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;viz-state -> viz-state
;;Purpose: Steps the visualization forward once
(define (right-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (let ([all-phases (imsg-state-phase (informative-messages-component-state (viz-state-informative-messages a-vs)))])
        (struct-copy imsg-state
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [phase
                      (if (zipper-at-end? all-phases)
                          all-phases
                          (zipper-next all-phases))]))])]))

;;viz-state -> viz-state
;;Purpose: Steps the visualization backward once
(define (left-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (let ([all-phases (imsg-state-phase (informative-messages-component-state (viz-state-informative-messages a-vs)))])
        (struct-copy imsg-state
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [phase
                      (if (zipper-at-begin? all-phases)
                          all-phases
                          (zipper-prev all-phases))]))])]))

;;viz-state -> viz-state
;;Purpose: Jumps to the end of the visualization
(define (down-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (let ([all-phases (imsg-state-phase (informative-messages-component-state (viz-state-informative-messages a-vs)))])
        (struct-copy imsg-state
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [phase
                      (if (zipper-at-end? all-phases)
                          all-phases
                          (zipper-to-end all-phases))]))])]))

;;viz-state -> viz-state
;;Purpose: Jumps to the beginning of the visualization
(define (up-key-pressed a-vs)
  (struct-copy
   viz-state
   a-vs
   [informative-messages
    (struct-copy
     informative-messages
     (viz-state-informative-messages a-vs)
     [component-state
      (let ([all-phases (imsg-state-phase (informative-messages-component-state (viz-state-informative-messages a-vs)))])
        (struct-copy imsg-state
                     (informative-messages-component-state (viz-state-informative-messages a-vs))
                     [phase
                      (if (zipper-at-begin? all-phases)
                          all-phases
                          (zipper-to-begin all-phases))]))])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DRAWING FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(listof phase) -> (listof graph-thunk)
;;Purpose: Creates all of the graphics need for the visualization using the given (listof phase)
(define (make-main-graphic loPhase state-table-mappings)
  ;;phase -> graph-thunk
  ;;Purpose: Creates the transition diagram thunk and state pairing table using the given phase
  (define (draw-graphic phase)
    ;; phase -> graph-thunk
    ;;Purpose: Draws the transition diagram thunk of using the given phase
    (define (draw-graph)
      (list (create-graph-struct (phase-M phase))
            (square 1 'solid 'white)))
    ;; phase -> graph-thunk
    ;; Purpose: Draws the state-pairing table next to the transition diagram thunk using the given phase
    (define (draw-table-and-graph)
      ;; (vectorof (vectorof marking)) (listof state) -> image
      ;; Purpose: Draws the given table and distinguishes the final states from the remaining state
      (define (draw-table table finals state-pair)
        ;;natnum natum -> image
        ;;Purpose: Draws the given table row by row
        (define (draw-table-helper row-amount row-idx)
          ;;natnum natnum -> image
          ;;Purpose: Draws the given row
          (define (make-row row column-idx)
            ;; symbol -> image
            ;;Purpose: Draws a sqaure in the table using the given symbol
            (define (draw-square sym)
              (let* ([base-square-img (overlay (square 40 'solid 'white) (square 45 'solid 'gray))]
                     [select-square-img (overlay (square 40 'solid 'white) (square 45 'solid 'red))]
                     [final-state-square-img (overlay (square 40 'solid 'orange) (square 45 'solid 'gray))]
                     [current-pair? (and (not (list? state-pair))
                                         (= (hash-ref state-table-mappings (state-pair-s1 state-pair)) row-idx)
                                         (= (hash-ref state-table-mappings (state-pair-s2 state-pair)) column-idx))])
                (cond [(eq? sym BLANK-SPACE) (if current-pair? select-square-img base-square-img)]
                      [(eq? sym BLACK) (overlay (square 40 'solid BLACK) (square 45 'solid 'gray))]
                      [(eq? sym MARK) (overlay (text "X" 38 BLACK) base-square-img)]
                      [(eq? sym NEW-MARK) (overlay (text "X" 38 'red) (if current-pair? select-square-img base-square-img))]
                      [else (overlay (text (symbol->string sym) 38 BLACK)
                                     (if (member sym finals)
                                         final-state-square-img
                                         base-square-img))])))
            (if (= (sub1 (vector-length row)) column-idx)
                (draw-square (vector-ref row column-idx))
                (beside (draw-square (vector-ref row column-idx))
                        (make-row row (add1 column-idx)))))
          (if (= row-amount row-idx)
              (make-row (vector-ref table row-idx) 0)
              (above (make-row (vector-ref table row-idx) 0)
                     (draw-table-helper row-amount (add1 row-idx)))))
        (draw-table-helper (sub1 (vector-length table)) 0))
      (let ([state-pair (if (= (phase-number phase) 4)
                            (phase-4-attributes-unmarked-pair (phase-attributes phase))
                            '())]
            [merge-state (if (= (phase-number phase) 5)
                             (phase-5-attributes-merged-states (phase-attributes phase))
                             '())])
        (if (= (phase-number phase) 5)
            (list (list (create-graph-struct (phase-M phase) #:merge-state merge-state)
                        (create-graph-struct (phase-5-attributes-rebuild-M (phase-attributes phase)) #:merge-state merge-state))
                  (draw-table (phase-state-pairing-table phase) (dfa-finals (phase-M phase)) state-pair))
            (list (create-graph-struct (phase-M phase)
                                       #:state-pair (if (and (= (phase-number phase) 4)
                                                             (not (symbol? state-pair)))
                                                        state-pair
                                                        '()))
                  (draw-table (phase-state-pairing-table phase)
                              (dfa-finals (phase-M phase))
                              (if (symbol? state-pair) '() state-pair))))))
    (if (<= 2 (phase-number phase) 5)
        (draw-table-and-graph)
        (draw-graph)))
  (map (λ (phase) (draw-graphic phase)) loPhase))


;; imsg-state -> image
;;Purpose: Draws the informative messages using the given imsg-state
(define (draw-imsg imsg-state)

  (define PHASE--1-IMSG (text "Input Machine" FONT-SIZE BLACK))

  (define PHASE0-IMSG (text "Converted Input Machine to DFA" FONT-SIZE BLACK))

  ;;(listof state) -> string
  ;;Purpose: Converts the given (listof state) into a string
  (define (convert-to-string los)
    (if (= (length los) 1)
        (symbol->string (first los))
        (string-append (symbol->string (first los)) ", "
                       (convert-to-string (rest los)))))

  ;;phase-attributes -> image
  ;;Purpose: Makes the imsg for phase 1
  (define (make-phase1-imsg phase-attribute)
    (text (format "~a ~a" 
                  (if (= (length (phase-1-attributes-unreachable-state phase-attribute)) 1)
                      "Removed the unreachable state:"
                      "Removed unreachable states:")
                  (convert-to-string (phase-1-attributes-unreachable-state phase-attribute)))
          FONT-SIZE BLACK))
  
  (define PHASE2-IMSG (text "State Pairing Table" FONT-SIZE BLACK))

  ;;state-pair -> string
  ;;Purpose: Makes the state pair readable
  (define (pretty-print-state-pair state-pair)
    (string-append "(" (symbol->string (state-pair-s1 state-pair)) "," (symbol->string (state-pair-s2 state-pair)) ")"))

  ;;state-pair -> string
  ;;Purpose: Makes the state pair readable
  (define (pretty-print-destination-state-pair state-pair)
    (text (string-append "(" (symbol->string (state-pair-s1 state-pair)) "," (symbol->string (state-pair-s2 state-pair)) ") ")
          FONT-SIZE
          (hash-ref X11-AS-RACKET-HASH (hash-ref sigma-color-pairings (state-pair-destination-pairs state-pair)))))

  ;;phase-attributes -> image
  ;;Purpose: Makes the imsg for phase 3
  (define (make-phase3-imsg phase-attribute)
    (text "Marking all state pairings that contain one final and one non-final state" FONT-SIZE BLACK))

  ;;phase-attributes -> image
  ;;Purpose: Makes the imsg for phase 4
  (define (make-phase4-imsg phase-attribute)    
    (let ([phase-4-state-pair (phase-4-attributes-unmarked-pair phase-attribute)])
      (above (text "Attempting to mark all state pairings that are currently unmarked" FONT-SIZE BLACK)
             (if (symbol? phase-4-state-pair)
                 (text "Completed this pass of unmarked state pairings, moving to the next."
                           FONT-SIZE BLACK)
                 
                 (beside (text "State pair " FONT-SIZE BLACK)
                         (text (pretty-print-state-pair phase-4-state-pair) FONT-SIZE 'red)
                         (text (if (state-pair-marked? phase-4-state-pair)
                                   " is marked because one of it's destination state pairing:"
                                   " remains unmarked because both of it's destination state pairings:")
                           FONT-SIZE BLACK)
                     (apply beside (map pretty-print-destination-state-pair (state-pair-destination-pairs phase-4-state-pair)))
                     (text (if (state-pair-marked? phase-4-state-pair)
                               " is marked"
                               " are unmarked")
                           FONT-SIZE BLACK))))))

  ;;phase-attributes -> image
  ;;Purpose: Makes the imsg for phase 5
  (define (make-phase5-imsg phase-attribute)
    (let ([merged-state (phase-5-attributes-merged-states phase-attribute)])
      (above (text "Rebuilding the machine" FONT-SIZE BLACK)
             (if (empty? merged-state)
                 (text "yler" FONT-SIZE 'white)
                 (text (format "States ~a have been merged to create state ~s"
                               (convert-to-string (set->list (merged-state-old-symbols merged-state)))
                               (merged-state-new-symbol merged-state))
                       FONT-SIZE
                       BLACK))
             (if (< (set-count (merged-state-old-symbols merged-state)) 2)
                 (text "yler" FONT-SIZE 'white)
                 (text "These pairs are unmarked and share states" FONT-SIZE BLACK))
             (text (format "States remaining to be used for building: ~s" (convert-to-string (phase-5-attributes-remaining-states phase-attribute))) FONT-SIZE BLACK))))

  ;;phase-attributes -> image
  ;;Purpose: Makes the imsg for phase 6
  (define (make-phase6-imsg phase-attribute)
    (text (if (phase-6-attributes-minimized? phase-attribute)
              "Minimized machine"
              "This machine cannot be minimized")
          FONT-SIZE BLACK))
  (let ([current-phase (zipper-current (imsg-state-phase imsg-state))])
    (cond [(= (phase-number current-phase) -1) PHASE--1-IMSG]
          [(= (phase-number current-phase) 0) PHASE0-IMSG]
          [(= (phase-number current-phase) 1) (make-phase1-imsg (phase-attributes current-phase))]
          [(= (phase-number current-phase) 2) PHASE2-IMSG]
          [(= (phase-number current-phase) 3) (make-phase3-imsg (phase-attributes current-phase))]
          [(= (phase-number current-phase) 4) (make-phase4-imsg (phase-attributes current-phase))]
          [(= (phase-number current-phase) 5) (make-phase5-imsg (phase-attributes current-phase))]
          [else (make-phase6-imsg (phase-attributes current-phase))])))

;; ndfa -> graph-thunk
;;Purpose: Creates a graph-thunk using the given dfa and optional state-pair
(define (create-graph-struct M #:state-pair [state-pair '()] #:merge-state [merge-states '()])
  ;; graph (listof state) start (listof state) -> graph
  ;; Purpose: To make a node graph
  (define (make-node-graph graph los start finals)
    (foldl (λ (state result)
             (let ([member-of-finals? (member state finals)])
               (add-node result
                         state
                         #:atb (hash 'color (if (eq? state start) 'darkgreen BLACK)
                                     'shape (if member-of-finals? 'doublecircle 'circle)
                                     'style (if member-of-finals? 'filled 'solid)
                                     'fillcolor 'orange
                                     'label state
                                     'fontcolor BLACK
                                     'font "Sans"))))
           graph
           los))

  ;; graph dfa (listof state-pair) -> graph
  ;; Purpose: To make an edge graph
  (define (make-edge-graph graph state-pair merge-states)
    (foldl (λ (rule result)
             (let* ([source-state (first rule)]
                    [found-state-from-state-pair?
                     (and (not (list? state-pair))
                          (or (eq? source-state (state-pair-s1 state-pair))
                              (eq? source-state (state-pair-s2 state-pair))))]
                    [found-state-from-merge-state?
                     (and (set? merge-states)
                          (set-member? merge-states source-state))])
               (add-edge result
                         (second rule)
                         (first rule)
                         (third rule)
                         #:atb (hash 'fontsize FONT-SIZE
                                     'style (if (or found-state-from-state-pair? found-state-from-merge-state?)
                                                'bold
                                                'solid)
                                     'color (if (or found-state-from-state-pair? found-state-from-merge-state?)
                                                (hash-ref sigma-color-pairings (second rule))
                                                BLACK)))))
           graph
           (dfa-rules M)))
  (make-edge-graph (make-node-graph (create-graph 'dgraph
                                                  #:atb (hash 'rankdir "LR" 'font "Sans"))
                                    (dfa-states M)
                                    (dfa-start M)
                                    (dfa-finals M))                  
                   state-pair
                   (if (empty? merge-states)
                       merge-states
                       (merged-state-old-symbols merge-states))))


(define (test-colors M)

  (define (make-node-graph graph los start finals)
    (foldl (λ (state result)
             (let ([member-of-finals? (member state finals)])
               (add-node result
                         state
                         #:atb (hash 'color (if (eq? state start) 'darkgreen BLACK)
                                     'shape (if member-of-finals? 'doublecircle 'circle)
                                     'style (if member-of-finals? 'filled 'solid)
                                     'fillcolor 'orange
                                     'label state
                                     'fontcolor BLACK
                                     'font "Sans"))))
           graph
           los))

  ;; graph dfa (listof state-pair) -> graph
  ;; Purpose: To make an edge graph
  (define (make-edge-graph graph rules)
    (foldl (λ (rule result)
             (let* ([source-state (first rule)])
               (add-edge result
                         (second rule)
                         (first rule)
                         (third rule)
                         #:atb (hash 'fontsize FONT-SIZE
                                     'style 'solid
                                     'color (hash-ref sigma-color-pairings (second rule))))))
           graph
           rules))
  
  (let ([M (unchecked->dfa M)])
    (graph->bitmap (make-edge-graph (make-node-graph (create-graph 'dgraph
                                                                   #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                     (dfa-states M)
                                                     (dfa-start M)
                                                     (dfa-finals M))
                                    (dfa-rules M)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;VIZ-PRIMITIVE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;; fsa -> void
;; Purpose: Displays the process of minimizing a dfa
(define (minimization-viz M)
  ;;dfa dfa -> boolean
  ;;Purpose: Determines if the two dfa have any changes
  (define (machine-changed? old-M new-M)
    (and (test-equiv-fsa old-M new-M)
         (not (= (length (fsa-getstates old-M)) (length (fsa-getstates new-M))))))
  
  ;; dfa -> (vectorof (vectorof marking))
  ;; Purpose: Creates the representation of the state pairing table
  (define (make-table M)
    ;; (listof state) natnum -> (vectorof (vectorof marking))
    ;; Purpose: Makes the state pairing table using given states 
    (define (make-table-helper states num-rows)
      (build-vector num-rows
                    (λ (row-num)
                      (build-vector num-rows
                                    (λ (col-num)
                                      (let ([blank-tile-count (- (length states) (- num-rows row-num))])
                                        (cond [(= row-num 0) (if (= col-num 0) BLANK-SPACE (list-ref states (sub1 col-num)))]
                                              [(= col-num 0) (list-ref states (sub1 row-num))]
                                              [(<= col-num blank-tile-count) BLANK-SPACE]
                                              [else BLACK])))))))
    (make-table-helper (dfa-states M) (add1 (length (dfa-states M)))))

  ;; dfa -> (listof dfa)
  ;; Purpose: Incrementally rebuilds the given machine
  (define (reconstruct-machine minimized-M merged-states)
    ;;dfa (queueof state) dfa (listof dfa) -> (listof dfa)
    ;;Purpose: Builds the next step to the given dfa using the first state in the (queueof state)
    (define (machine-rebuilder qoS rebuild-M acc)
      (if (qempty? qoS)
          (reverse acc)
          (let* ([next-state-to-add (qfirst qoS)]
                 [connecting-state-rules (filter (λ (rule)
                                                   (let ([source-state (first rule)]
                                                         [destination-state (third rule)])
                                                     (or (and (member source-state (dfa-states rebuild-M))
                                                                    (eq? destination-state next-state-to-add))
                                                               (and (member destination-state (dfa-states rebuild-M))
                                                                    (eq? source-state next-state-to-add))
                                                               (and (eq? source-state next-state-to-add)
                                                                    (eq? destination-state next-state-to-add)))))
                                                 (dfa-rules minimized-M))]
                 [reachables-from-state (filter-map (λ (rule)
                                                      (let ([source-state (first rule)]
                                                            [destination-state (third rule)])
                                                        (and (and (eq? source-state next-state-to-add)
                                                                  (not (eq? destination-state next-state-to-add))
                                                                  (not (member destination-state (dfa-states rebuild-M))))
                                                             destination-state)))
                                                     (dfa-rules minimized-M))]
                 [new-rebuild-M (struct-copy dfa rebuild-M
                                             [states (cons next-state-to-add (dfa-states rebuild-M))]
                                             [rules (append connecting-state-rules (dfa-rules rebuild-M))]
                                             [finals (if (or (member next-state-to-add (dfa-finals minimized-M))
                                                             (ormap (λ (ms)
                                                                      (set-member? (merged-state-old-symbols ms) next-state-to-add))
                                                                    merged-states))
                                                         (cons next-state-to-add (dfa-finals rebuild-M))
                                                         (dfa-finals rebuild-M))])])
            (machine-rebuilder (remove-duplicates (enqueue (dequeue qoS) reachables-from-state))
                               new-rebuild-M
                               (cons new-rebuild-M acc)))))  
    (let* ([start (dfa-start minimized-M)]
           [states-connected-to-start (remove-duplicates (filter-map (λ (rule)
                                                                       (and (and (eq? (first rule) start)
                                                                                 (not (eq? (third rule) start)))
                                                                            (third rule)))
                                                                     (dfa-rules minimized-M)))]
           [looping-rules (filter (λ (rule)
                                    (and (eq? (first rule) start)
                                         (eq? (third rule) start)))
                                  (dfa-rules minimized-M))]
           [rebuild-M (dfa (list start)
                           (dfa-alphabet minimized-M)
                           start
                           (if (member start (dfa-finals minimized-M)) (list start) '())
                           looping-rules
                           #t
                           'no-dead)])
      (machine-rebuilder states-connected-to-start rebuild-M (list rebuild-M))))

  ;;natnum phase-attribute-struct dfa (vectorof (vectorof marking)) (listof state-pair) (hash state . natnum) (set state-pair) (set state-pair) -> (listof phase)
  ;;Purpose: Makes all of phase for the given natnum using the given dfa, state-table, (listof state-pair), and state-table-mappings 
  (define (make-phase phase-id attribute-struct-id no-unreachables-M state-pairing-table loSP state-table-mappings seen-markings seen-unmarkings)
    ;; (vectorof (vectorof marking)) (listof state-pair) (listof phase) -> (listof phase)
    ;; Purpose: Makes the phase with the updated table and corresponding state pair
    (define (make-phase-helper state-pairing-table loSP acc seen-markings seen-unmarkings)
      ;;(vectorof (vectorof marking)) state-pair (hash state . natnum) -> (vectorof (vectorof marking))
      ;; Purpose: Updates the given table using the given state pair and hash
      (define (make-mark-on-table state-pairing)
        ;; (vectorof marking) natnum -> (vectorof marking)
        ;; Purpose: Updates a spot on state pairing table using the given row and column number
        (define (update-row row column-id)
          ;;symbol -> symbol
          ;;Purpose: Updates the given mark for the state pairing table
          (define (choose-mark-for-row current-table-mark)
            (match
                current-table-mark
              [BLANK-SPACE NEW-MARK]
              [NEW-MARK MARK]
              [_ MARK]))
          (let* ([current-column-ref (vector-ref row column-id)])
            (vector-set/copy row column-id (choose-mark-for-row current-column-ref))))
        (let* ([row-id (hash-ref state-table-mappings (state-pair-s1 state-pairing))]
               [column-id (hash-ref state-table-mappings (state-pair-s2 state-pairing))]
               [current-row (vector-ref state-pairing-table row-id)])
          (vector-set/copy state-pairing-table
                           row-id
                           (update-row current-row column-id))))
      (if (empty? loSP)
          (phase-results (reverse acc) state-pairing-table)
          (let ([state-pairing (first loSP)])
            (if (or (set-member? seen-markings state-pairing)
                    (set-member? seen-unmarkings state-pairing))
                (if (set-member? seen-markings state-pairing)
                    (make-phase-helper state-pairing-table (rest loSP) acc seen-markings seen-unmarkings)
                    (make-phase-helper state-pairing-table loSP
                                       (cons (phase phase-id no-unreachables-M state-pairing-table (attribute-struct-id 'next-pass)) acc)
                                       seen-markings
                                       (set)))
                (let* ([new-table (if (state-pair-marked? state-pairing)
                                      (make-mark-on-table state-pairing)
                                      state-pairing-table)]
                       [new-phase (phase phase-id no-unreachables-M new-table (attribute-struct-id state-pairing))]
                       [updated-seen (if (state-pair-marked? state-pairing) (set-add seen-markings state-pairing) seen-markings)]
                       [updated-unseen (if (not (state-pair-marked? state-pairing)) (set-add seen-unmarkings state-pairing) seen-unmarkings)])
                  (make-phase-helper new-table (rest loSP) (cons new-phase acc) updated-seen updated-unseen))))))
    (make-phase-helper state-pairing-table loSP '() seen-markings seen-unmarkings))

  ;; (listof dfa) (listof merged-state) (vectorof (vectorof marking)) -> (listof phase)
  ;; Purpose: Pairs a merged-state with a rebuild dfa that contains either the new merged state symbol or one of the states that got merged
  (define (make-phase-5 unminimized-M loRM loMS state-pairing-table states)
    ;;(listof dfa) (listof merged-state) (listof phase)
    ;;Purpose: Associates the given rebuilt dfa with a merged-state if any of the components a merged-state has been found
    (define (make-phase-5-helper loRM loMS states acc)
      (if (empty? loRM)
          (reverse acc)
          (let* ([rebuild-M (first loRM)]
                 [merged-state (filter (λ (ms)
                                         (or (member (merged-state-new-symbol ms) (dfa-states rebuild-M))
                                             (ormap (λ (state) (set-member? (merged-state-old-symbols ms) state)) (dfa-states rebuild-M))))
                                       loMS)]
                 [found-merged-state? (not (empty? merged-state))]
                 [remaining-states (if found-merged-state?
                                       (filter-not (λ (state)
                                                 (set-member? (merged-state-old-symbols (first merged-state)) state))
                                                 states)
                                       states)]
                 [new-phase (phase 5 unminimized-M state-pairing-table (phase-5-attributes
                                                                        (first loRM)
                                                                        (if found-merged-state? (first merged-state) merged-state)
                                                                        remaining-states))])
            (make-phase-5-helper (rest loRM) (if found-merged-state? (remove (first merged-state) loMS) loMS) remaining-states (cons new-phase acc)))))
    (make-phase-5-helper loRM loMS states '()))
  (let* ([unchecked-M M]
         [results-from-minimization (minimize-dfa unchecked-M)]
         [original-M (unchecked->dfa (minimization-results-original-M results-from-minimization))]
         [no-unreachables-M (minimization-results-unreachables-removed-M results-from-minimization)]
         [all-loSP (reverse (minimization-results-loSP results-from-minimization))]
         [final-non-final-pairings (filter state-pair-marked? (state-pairings-all-pairs (first all-loSP)))]
         [rest-loSP (append-map state-pairings-all-pairs (rest all-loSP))]
         [unreachable-states (filter (λ (s) (not (member s (fsa-getstates no-unreachables-M)))) (fsa-getstates unchecked-M))]
         [M (ndfa->dfa M)]
         [minimized-M (minimization-results-new-machine results-from-minimization)]
         [has-unreachables? (machine-changed? M no-unreachables-M)]
         [can-be-minimized? (machine-changed? M minimized-M)]
         [M (unchecked->dfa M)]
         [no-unreachables-M (unchecked->dfa no-unreachables-M)]
         [state-table-mappings (for/hash ([state (dfa-states no-unreachables-M)]
                                          [num (in-naturals)])
                                 (values state (add1 num)))]
         [no-unreachables-M-state-pairing-table (make-table no-unreachables-M)]
         [minimized-M (unchecked->dfa minimized-M)]
         [phase--1 (list (phase -1 original-M no-unreachables-M-state-pairing-table (phase-0-attributes)))]
         [phase-0 (if (not (dfa-deterministic? original-M))
                       (list (phase 0 M  no-unreachables-M-state-pairing-table (phase-0-attributes)))
                       '())]
         [phase-1 (if has-unreachables?
                      (list (phase 1 no-unreachables-M no-unreachables-M-state-pairing-table (phase-1-attributes unreachable-states)))
                      '())]
         [phase-2 (list (phase 2 no-unreachables-M no-unreachables-M-state-pairing-table (phase-2-attributes)))]
         [phase3+new-table (make-phase 3 phase-3-attributes no-unreachables-M no-unreachables-M-state-pairing-table
                                      final-non-final-pairings state-table-mappings (set) (set))]
         [phase-3-lo-phase (phase-results-loPhase phase3+new-table)]
         [phase-3 (if (empty? phase-3-lo-phase)
                      phase-3-lo-phase
                      (list (last phase-3-lo-phase)))]
         [table-with-initial-markings (phase-results-new-table phase3+new-table)]
         [phase4+new-table (make-phase 4
                                       phase-4-attributes
                                       no-unreachables-M
                                       table-with-initial-markings
                                       rest-loSP
                                       state-table-mappings
                                       (list->set (map (compose1 phase-3-attributes-initial-pairings phase-attributes)
                                            (phase-results-loPhase phase3+new-table)))
                                       (set))]
         [phase-4 (phase-results-loPhase phase4+new-table)]
         [filled-table (phase-results-new-table phase4+new-table)]
         [merged-states (minimization-results-merged-states results-from-minimization)]
         [rebuilding-machines (reconstruct-machine minimized-M merged-states)]
         [phase-5 (if can-be-minimized?
                      (make-phase-5 no-unreachables-M rebuilding-machines merged-states filled-table (dfa-states M))
                      '())]
         [phase-6 (list (phase 6 (last rebuilding-machines) filled-table (phase-6-attributes can-be-minimized?)))]
         [all-phases (append phase--1 phase-0 phase-1 phase-2 phase-3 phase-4 phase-5 phase-6)]
         [graphs (make-main-graphic all-phases state-table-mappings)])
    ;(void)
    phase-5
    #;
    (run-viz (map first graphs)
             (list->vector (map (λ (x table)
                                  (if (list? (first x))
                                      (λ (graph1 graph2)
                                        (beside (above graph1 graph2)
                                                (square 10 'solid "white")
                                                table))
                                      (λ (graph)
                                        (beside graph
                                                (square 10 'solid "white")
                                                table))))
                                graphs
                                (map (lambda (x) (second x)) graphs)))
             (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
             E-SCENE-WIDTH
             E-SCENE-HEIGHT
             PERCENT-BORDER-GAP
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (imsg-state (list->zipper all-phases))
                                   RULE-YIELD-DIMS)
             (instructions-graphic E-SCENE-TOOLS
                                   (bounding-limits 0
                                                    (image-width imsg-img)
                                                    E-SCENE-HEIGHT
                                                    (+ E-SCENE-HEIGHT (image-height imsg-img))))
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key ["right" viz-go-next right-key-pressed]
                                     ["left" viz-go-prev left-key-pressed]
                                     ["up" viz-go-to-begin up-key-pressed]
                                     ["down" viz-go-to-end down-key-pressed]
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
                                      ( [ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                                        [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                                        [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                                        [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                                        [W-KEY-DIMS viz-zoom-in identity]
                                        [S-KEY-DIMS viz-zoom-out identity]
                                        [R-KEY-DIMS viz-max-zoom-out identity]
                                        [E-KEY-DIMS viz-reset-zoom identity]
                                        [F-KEY-DIMS viz-max-zoom-in identity]))
             'minimization-viz)))
