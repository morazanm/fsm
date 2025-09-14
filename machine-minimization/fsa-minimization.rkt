#lang racket

(require "../fsm-core/private/fsa.rkt")

(provide unchecked->dfa
         minimize-dfa
         dfa)

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

;;states | the states of the dfa | (listof state)
;;alphabet | the alphabet that the dfa reads | (list of symbol)
;;start | the starting state of the dfa | state
;;finals | the final states of the dfa| (listof finals)
;;rules | the rules that the dfa must follow | (listof rule)
;;no-dead | the symbol of the dead state | symbol
(struct dfa (states alphabet start finals rules no-dead) #:transparent)

;;unchecked-dfa -> dfa-struct
;;Purpose: Converts the given unchecked-dfa to a dfa struct
(define (unchecked->dfa M)
  (dfa (fsa-getstates M)
       (fsa-getalphabet M)
       (fsa-getstart M)
       (fsa-getfinals M)
       (fsa-getrules M)
       'no-dead))

(define e-queue '())

(define qfirst first)

(define qempty? empty?)

;;(queueof X) (queue X) -> (queueof X)
;;Purpose: Adds the X to the back of the given (queueof X) 
(define (enqueue queue x)
  (append queue x))

;;(queueof X) -> (queueof X)
;;Purpose: Removes the first element of the given (queueof X) 
(define (dequeue queue)
  (rest queue))

;;ndfa -> dfa
;;Purpose: If possible, minimizes the given ndfa
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
    ;;state-pair -> state-pair
    ;;Purpose: Creates the destination state-pairs using the given state-pair and destination state-pairs to the orignal-state-pair
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
        (first loSP)
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
         [new-M (table->dfa (state-pairings-all-pairs filled-table) dfa transition-table)])
    new-M))