#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/fsa.rkt"
         "../sm-graph.rkt")

(provide unchecked->dfa minimize-dfa dfa)

(struct dfa (states alphabet start finals rules no-dead) #:transparent)
(struct equivalence-class (non-final final) #:transparent)

(struct merged-state (new-symbol old-symbols) #:transparent)


;;probably raise error if final is unreachable
(define (remove-unreachables M)
  (let* ([states (fsa-getstates M)]
         [start (fsa-getstart M)]
         [rules (fsa-getrules M)]
         [reachable-states (filter (λ (s) (ormap (λ (r) (or (eq? start s)
                                                            (and (not (eq? (first r) (third r)))
                                                                 (eq? (third r) s)))) rules)) states)]
         [usable-rules (filter (λ (r) (ormap (λ (s) (eq? (first r) s)) reachable-states)) rules)])
    (make-unchecked-dfa reachable-states
                        (fsa-getalphabet M)
                        (fsa-getstart M)
                        (filter (λ (f) (member f reachable-states)) (fsa-getfinals M))
                        usable-rules
                        'no-dead)))




;;implementation 4 -> removing unreachables
(define (minimize-dfa4 M)
  (let* ([dfa (remove-unreachables (ndfa->dfa M))]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [equivalence-class (make-equivalence-classes (filter (λ (s) (not (member s finals))) (fsa-getstates dfa)) finals transition-table)])
    (equivalence-class->dfa (fsa-getalphabet dfa) (fsa-getstart dfa) finals equivalence-class transition-table)
    #;(if (list? equivalence-class)
        equivalence-class
        (equivalence-class->dfa (fsa-getalphabet dfa) (fsa-getstart dfa) finals equivalence-class transition-table))))


(define (equivalence-class->dfa alphabet start finals EC transition-table)
  (define (search-for-merged-state old-state merged-states)
    (first (filter-map (λ (ms) (and (set-member? (merged-state-old-symbols ms) old-state)
                                    (merged-state-new-symbol ms)))                
                       merged-states)))
  (let* ([non-final-merged-states (map (λ (s) (if (set-member? s start)
                                                  (merged-state start s)
                                                  (merged-state (set-first s) s)))
                                       (equivalence-class-non-final EC))]
         [final-merged-states (filter-map (λ (s) (and (> (set-count s) 1)
                                                      (merged-state (set-first s) s))) (equivalence-class-final EC))]
         [merged-states (append final-merged-states non-final-merged-states)]
         [other-states (append-map set->list (filter (λ (s) (= (set-count s) 1)) (equivalence-class-final EC)))]
         [new-states (remove-duplicates (append (map merged-state-new-symbol merged-states) other-states))]
         [new-finals (filter (λ (s) (member s finals)) new-states)]
         [table->rules (foldl (λ (row acc)
                                (if (list? (member (first row) new-states))
                                    (append (map (λ (r) (if (list? (member (second r) new-states))
                                                            (cons (first row) r)
                                                            (list (first row) (first r) (search-for-merged-state (second r) merged-states))))
                                                 (second row)) acc)
                                    acc))
                              '()
                              transition-table)])
    
     (make-unchecked-dfa new-states
                        alphabet
                        start
                        new-finals
                        table->rules
                        'no-dead)))

(define (make-transition-table dfa)
  (let ([states (fsa-getstates dfa)]
        [rules (fsa-getrules dfa)])
    (for/list ([state states]
               #:do [(define applicable-rules (filter-map (λ (rule) (and (eq? state (first rule))
                                                                         (list (second rule) (third rule))))
                                                          rules))])
      (list state applicable-rules))))
    









(define (equivalence? matchee matcher last-ec-non-finals last-ec-finals transition-table matching-finals)
  (define (same-transitions? matchee-transitions matcher-transitions)
    (made-match? andmap matchee-transitions matcher-transitions))

  (define (partial-match? matchee-transitions matcher-transitions)
    (made-match? ormap matchee-transitions matcher-transitions))

  (define (made-match? f matchee-transitions matcher-transitions)
    (list? (f (λ (t) (member t matchee-transitions)) matcher-transitions)))
    
  (define (transition-to-ec-final-apart-of-same-set transitions finals-set)
    (for*/and ([tran transitions]
               [finals last-ec-finals])
      (not (set-member? finals (second tran)))))
    
  (define (at-least-one-transitions-to-ec-non-final transitions non-finals-set)
    (for*/or ([tran transitions]
              [non-finals non-finals-set])
      (set-member? non-finals (second tran))))
  
  (define (matchee-transition-to-matcher? matchee-transitions)
    (let ([matchee-destinations (map second matchee-transitions)])
      (list? (member matcher matchee-destinations))))

  (define (transitions-apart-of-same-set? matchee-transitions matcher-transitions)
    (let* ([matchee-destinations (map second matchee-transitions)]
           [matcher-destinations (map second matcher-transitions)]
           [matcher-destination-set-from-non-finals (append-map
                                                     (λ (d) (filter (λ (s) (proper-subset? (set d) s)) last-ec-non-finals))
                                                     matcher-destinations)]
           [matcher-destination-set-from-finals (append-map
                                                 (λ (d) (filter (λ (s) (proper-subset? (set d) s)) last-ec-finals))
                                                 matcher-destinations)])
      (ormap (λ (d) (or (ormap (λ (s) (proper-subset? (set d) s)) matcher-destination-set-from-non-finals)
                        (ormap (λ (s) (proper-subset? (set d) s)) matcher-destination-set-from-finals))) matchee-destinations)))
      
  (define (connection-between-matchee-and-matcher matchee-transitions matcher-transitions)
    (or (and (partial-match? matchee-transitions matcher-transitions)
             (at-least-one-transitions-to-ec-non-final matchee-transitions last-ec-non-finals)
             (at-least-one-transitions-to-ec-non-final matcher-transitions last-ec-non-finals))
        (and (matchee-transition-to-matcher? matchee-transitions)
             (transitions-apart-of-same-set? matchee-transitions matcher-transitions))))
  
  (let ([matchee-transitions (first (filter-map (λ (row) (and (eq? matchee (first row))
                                                              (second row)))
                                                transition-table))]
        [matcher-transitions (first (filter-map (λ (row) (and (eq? matcher (first row))
                                                              (second row)))
                                                transition-table))])
    #|(displayln matchee-transitions)
    (displayln matcher-transitions)
    (displayln last-ec-non-finals)
    (displayln last-ec-finals)
    (displayln transition-table)|#
    (or (same-transitions? matchee-transitions matcher-transitions)
        (and (connection-between-matchee-and-matcher matchee-transitions matcher-transitions)
             (transition-to-ec-final-apart-of-same-set matchee-transitions last-ec-finals)
             (transition-to-ec-final-apart-of-same-set matcher-transitions last-ec-finals))
        (and (ormap (λ (s) (> (set-count s) 1)) last-ec-finals)
             (or (and matching-finals (transitions-apart-of-same-set? matchee-transitions matcher-transitions))
                 (connection-between-matchee-and-matcher matchee-transitions matcher-transitions))
             #;(connection-between-matchee-and-matcher matchee-transitions matcher-transitions)))))
         




;;establish equivalence classes where

(define (make-equivalence-classes states finals transition-table)
  (define (make-next-equivalence-class last-ec matches-to-make new-ec transition-table matching-finals)
    (define (make-next-equivalence-class-helper last-ec matches-to-make new-ec transition-table matching-finals)
      (define (make-equivalence-helper matchee lo-matches-to-make last-ec new-ec transition-table matching-finals)
        (define (make-equivalence matchee potential-matches new-ec last-ec-non-finals last-ec-finals transition-table matching-finals)
          (define (make-equivalence-set matchee matched non-finals)
            (let ([set-index (index-where non-finals (λ (s) (set-member? s matched)))])
              (if (number? set-index)
                  (cons (set-add (list-ref non-finals set-index) matchee) (remove (list-ref non-finals set-index) non-finals))
                  (cons (set matchee matched) non-finals))))
  
          #;(define (equivalence? matchee matcher last-ec-non-finals last-ec-finals transition-table)
            (define (same-transitions? matchee-transitions matcher-transitions)
              (made-match? andmap matchee-transitions matcher-transitions))

            (define (partial-match? matchee-transitions matcher-transitions)
              (made-match? ormap matchee-transitions matcher-transitions))

            (define (made-match? f matchee-transitions matcher-transitions)
              (list? (f (λ (t) (member t matchee-transitions)) matcher-transitions)))
    
            (define (transition-to-ec-final-apart-of-same-set transitions finals-set)
              (for*/and ([tran transitions]
                         [finals last-ec-finals])
                (not (set-member? finals (second tran)))))
    
            (define (at-least-one-transitions-to-ec-non-final transitions non-finals-set)
              (for*/or ([tran transitions]
                        [non-finals non-finals-set])
                (set-member? non-finals (second tran))))
  
            (define (matchee-transition-to-matcher? matchee-transitions)
              (let ([matchee-destinations (map second matchee-transitions)])
                (list? (member matcher matchee-destinations))))

            (define (transitions-apart-of-same-set? matchee-transitions matcher-transitions)
              (let* ([matchee-destinations (map second matchee-transitions)]
                     [matcher-destinations (map second matcher-transitions)]
                     [matcher-destination-set-from-non-finals (append-map
                                                               (λ (d) (filter (λ (s) (proper-subset? (set d) s)) last-ec-non-finals))
                                                               matcher-destinations)]
                     [matcher-destination-set-from-finals (append-map
                                                           (λ (d) (filter (λ (s) (proper-subset? (set d) s)) last-ec-finals))
                                                           matcher-destinations)])
                (ormap (λ (d) (or (ormap (λ (s) (proper-subset? (set d) s)) matcher-destination-set-from-non-finals)
                                  (ormap (λ (s) (proper-subset? (set d) s)) matcher-destination-set-from-finals))) matchee-destinations)))
      
            (define (connection-between-matchee-and-matcher matchee-transitions matcher-transitions)
              (or (and (partial-match? matchee-transitions matcher-transitions)
                       (at-least-one-transitions-to-ec-non-final matchee-transitions last-ec-non-finals)
                       (at-least-one-transitions-to-ec-non-final matcher-transitions last-ec-non-finals))
                  (and (matchee-transition-to-matcher? matchee-transitions)
                       (transitions-apart-of-same-set? matchee-transitions matcher-transitions))))
  
            (let ([matchee-transitions (first (filter-map (λ (row) (and (eq? matchee (first row))
                                                                        (second row)))
                                                          transition-table))]
                  [matcher-transitions (first (filter-map (λ (row) (and (eq? matcher (first row))
                                                                        (second row)))
                                                          transition-table))])
              (or (same-transitions? matchee-transitions matcher-transitions)
                  (and (connection-between-matchee-and-matcher matchee-transitions matcher-transitions)
                       (transition-to-ec-final-apart-of-same-set matchee-transitions last-ec-finals)
                       (transition-to-ec-final-apart-of-same-set matcher-transitions last-ec-finals))
                  (and (ormap (λ (s) (> (set-count s) 1)) last-ec-finals)
                       (connection-between-matchee-and-matcher matchee-transitions matcher-transitions)))))
          
          (cond [(set-empty? potential-matches)
                 (if #;(member (set matchee) (equivalence-class-final new-ec))
                     (or (ormap (λ (s) (proper-subset? (set matchee) s)) (equivalence-class-non-final new-ec))
                         #;(ormap (λ (s) (subset? (set matchee) s)) (equivalence-class-final new-ec)))
                     new-ec
                     (struct-copy equivalence-class
                                  new-ec
                                  [final (cons (set matchee) (equivalence-class-final new-ec))]))]
                [(equivalence? matchee (set-first potential-matches) last-ec-non-finals last-ec-finals transition-table matching-finals)
                 (struct-copy equivalence-class
                              new-ec
                              [non-final (make-equivalence-set matchee (set-first potential-matches) (equivalence-class-non-final new-ec))])]
                [else (make-equivalence matchee
                                        (set-rest potential-matches)
                                        new-ec
                                        last-ec-non-finals
                                        last-ec-finals
                                        transition-table
                                        matching-finals)]))
        (if (empty? lo-matches-to-make)
            new-ec
            (make-equivalence-helper matchee
                                     (rest lo-matches-to-make)
                                     last-ec
                                     (make-equivalence matchee
                                                       (first lo-matches-to-make)
                                                       new-ec
                                                       (equivalence-class-non-final last-ec)
                                                       (equivalence-class-final last-ec)
                                                       transition-table
                                                       matching-finals)
                                     transition-table
                                     matching-finals)))
      (if (set-empty? matches-to-make)
          new-ec 
          (make-next-equivalence-class-helper last-ec
                                              (set-rest matches-to-make)
                                              (make-equivalence-helper (set-first matches-to-make)
                                                                       (if (empty? (equivalence-class-non-final new-ec))
                                                                           (list (set-rest matches-to-make))
                                                                           (map (λ (nfs) (set-union (set-rest matches-to-make) nfs))
                                                                                (equivalence-class-non-final new-ec)))
                                                                       last-ec
                                                                       new-ec
                                                                       transition-table
                                                                       matching-finals)
                                              transition-table
                                              matching-finals)))
    (if (empty? matches-to-make)
        new-ec 
        (make-next-equivalence-class last-ec
                                     (rest matches-to-make)
                                     (make-next-equivalence-class-helper last-ec (first matches-to-make) new-ec transition-table matching-finals)
                                     transition-table
                                     matching-finals)))
  
  (let ([first-equivalence-class (equivalence-class (list (list->set states)) (list (list->set finals)))])
    (define (make-equivalence-classes-helper LoEC)
      (define (same-equivalence-class? EC1 EC2)
        (define (same-equivalence-final)
          (and (= (length (equivalence-class-final EC1)) (length (equivalence-class-final EC2)))
               (list? (andmap (λ (t) (member t (equivalence-class-final EC1))) (equivalence-class-final EC2)))))
        (define (same-equivalence-non-final)
          (and (= (length (equivalence-class-non-final EC1)) (length (equivalence-class-non-final EC2)))
               (list? (andmap (λ (s1) (member s1 (equivalence-class-non-final EC2))) (equivalence-class-non-final EC1)))))
        (and (same-equivalence-non-final) (same-equivalence-final)))
      (cond [(or (and (>= (length LoEC) 2)
                  (same-equivalence-class? (first LoEC) (second LoEC)))
                 (= (length LoEC) 4))
             (first LoEC)]
            [(ormap (λ (s) (> (set-count s) 1)) (equivalence-class-final (first LoEC)))
             (let ([new-ec-for-non-finals (make-next-equivalence-class
                                           (first LoEC)
                                           (equivalence-class-non-final (first LoEC))
                                           (equivalence-class (list) (equivalence-class-final (first LoEC)))
                                           transition-table #f)]
                   [new-ec-for-finals (make-next-equivalence-class
                                       (first LoEC)
                                       (equivalence-class-final (first LoEC))
                                       (equivalence-class (list) (list) #;(equivalence-class-final (first LoEC)))
                                       transition-table
                                       #t)])
               
               (make-equivalence-classes-helper
                      (cons (struct-copy equivalence-class
                                   (first LoEC)
                                   [non-final (equivalence-class-non-final new-ec-for-non-finals)]
                                   [final (remove-duplicates
                                           (append (filter (λ (s) (not (member s (equivalence-class-final (first LoEC)))))
                                                           (equivalence-class-final new-ec-for-non-finals))
                                                   (equivalence-class-non-final new-ec-for-finals)
                                                   (equivalence-class-final new-ec-for-finals)))])
                      LoEC)))]
            [else (make-equivalence-classes-helper (cons (make-next-equivalence-class (first LoEC)
                                                                                      (equivalence-class-non-final (first LoEC))
                                                                                      (equivalence-class (list)
                                                                                                         (equivalence-class-final (first LoEC)))
                                                                                      transition-table
                                                                                      #f)
                                                         LoEC))])) 
    (make-equivalence-classes-helper (list first-equivalence-class))))

(struct state-pair (s1 s2 marked?) #:transparent)

(struct state-pairings (all-pairs) #:transparent)

(define (unchecked->dfa old-dfa)
  (dfa (fsa-getstates old-dfa)
       (fsa-getalphabet old-dfa)
       (fsa-getstart old-dfa)
       (fsa-getfinals old-dfa)
       (fsa-getrules old-dfa)
       'no-dead))

;;table filling method
(define (minimize-dfa M)
  (let* ([dfa (remove-unreachables (ndfa->dfa M))]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [states-table (map (λ (sp) (mark-states-table sp finals)) (make-states-table dfa))]
         [filled-table (make-matches (list (state-pairings states-table)) transition-table (fsa-getalphabet dfa))])
   (table->dfa (state-pairings-all-pairs filled-table) dfa transition-table)))


(define (table->dfa loSP old-dfa transition-table)
  (define (search-for-merged-state old-state merged-states)
    (first (filter-map (λ (ms) (and (set-member? (merged-state-old-symbols ms) old-state)
                                    (merged-state-new-symbol ms)))                
                       merged-states)))
  
  (let* ([states (fsa-getstates old-dfa)]
         [finals (fsa-getfinals old-dfa)]
         [marked-pairs (filter (λ (sp) (state-pair-marked? sp)) loSP)]
         [unmarked-pairs (filter (λ (sp) (not (state-pair-marked? sp))) loSP)]
         [merged-unmarked-pairs (accumulate-unmarked-pairs old-dfa unmarked-pairs '())]
         [states-that-were-merged (set->list (foldl (λ (mp acc) (set-remove (set-union acc (merged-state-old-symbols mp))
                                                                            (merged-state-new-symbol mp)))
                                                    (set) merged-unmarked-pairs))]
         [remaining-states (filter (λ (s) (not (member s states-that-were-merged))) states)]
         [new-finals (filter (λ (s) (member s finals)) remaining-states)]
         [table->rules (foldl (λ (row acc)
                                  (if (list? (member (first row) remaining-states))
                                      (append (map (λ (r) (if (list? (member (second r) remaining-states))
                                                              (cons (first row) r)
                                                              (list (first row) (first r) (search-for-merged-state (second r)
                                                                                                                   merged-unmarked-pairs))))
                                                   (second row)) acc)
                                      acc))
                                '()
                                transition-table)])
   (make-unchecked-dfa remaining-states
                        (fsa-getalphabet old-dfa)
                        (fsa-getstart old-dfa)
                        new-finals
                        table->rules
                        'no-dead)))




(define (overlap? unmarked-pair loSP)
  (ormap (λ (sp) (or (set-member? (merged-state-old-symbols sp) (state-pair-s1 unmarked-pair))
                     (set-member? (merged-state-old-symbols sp) (state-pair-s2 unmarked-pair))))
         loSP))


(define (merge-pairs dfa unmarked-pair loSP)
  (let* ([overlapped-pair (first (filter (λ (sp) (or (set-member? (merged-state-old-symbols sp) (state-pair-s1 unmarked-pair))
                                              (set-member? (merged-state-old-symbols sp) (state-pair-s2 unmarked-pair))))
                                 loSP))]
        [new-merged-state (update-merged-state dfa unmarked-pair overlapped-pair)])
    (map (λ (sp) (if (equal? overlapped-pair sp) new-merged-state sp)) loSP)))

(define (update-merged-state dfa unmarked-pair overlapped-pair)
  (let* ([start (fsa-getstart dfa)]
         [finals (fsa-getfinals dfa)]
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


           
          

(define (make-merged-state dfa unmarked-pair)
  (let ([start (fsa-getstart dfa)]
        [finals (fsa-getfinals dfa)]
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
                     
    
(define (accumulate-unmarked-pairs dfa unmarked-pairs acc)
  (cond [(empty? unmarked-pairs) acc]
        [(overlap? (first unmarked-pairs) acc)
         (accumulate-unmarked-pairs dfa (rest unmarked-pairs) (merge-pairs dfa (first unmarked-pairs) acc))]
        [(accumulate-unmarked-pairs dfa (rest unmarked-pairs) (cons (make-merged-state dfa (first unmarked-pairs)) acc))]))

(define (mark-states-table pairing finals)
  (if (or (and (list? (member (state-pair-s1 pairing) finals))
               (boolean? (member (state-pair-s2 pairing) finals)))
          
          (and (list? (member (state-pair-s2 pairing) finals))
               (boolean? (member (state-pair-s1 pairing) finals))))
      (struct-copy state-pair pairing [marked? #t])
      pairing))


(define (make-matches loSP transition-table alphabet)
  (define (update-pairs marked-pairs unmarked-pairs remaining-unmarked-pairs)
    (cond [(empty? unmarked-pairs) (state-pairings (append marked-pairs remaining-unmarked-pairs))]
          [(update-mark? (first unmarked-pairs) marked-pairs transition-table alphabet)
           (update-pairs (cons (struct-copy state-pair (first unmarked-pairs) [marked? #t]) marked-pairs)
                         (rest unmarked-pairs)
                         remaining-unmarked-pairs)]
          [else (update-pairs marked-pairs
                         (rest unmarked-pairs)
                         (cons (first unmarked-pairs) remaining-unmarked-pairs))]))
  (if (and (>= (length loSP) 2)
           (same-markings? (state-pairings-all-pairs (first loSP)) (state-pairings-all-pairs (second loSP))))
     (first loSP)
      (let ([marked-pairs (filter (λ (sp) (state-pair-marked? sp)) (state-pairings-all-pairs (first loSP)))]
            [unmarked-pairs (filter (λ (sp) (not (state-pair-marked? sp))) (state-pairings-all-pairs (first loSP)))])
        (make-matches (cons (update-pairs marked-pairs unmarked-pairs '()) loSP)
                      transition-table
                      alphabet))))

(define (same-markings? loSP1 loSP2)
  (let ([unmarked-SP1 (filter (λ (sp) (not (state-pair-marked? sp))) loSP1)]
        [unmarked-SP2 (filter (λ (sp) (not (state-pair-marked? sp))) loSP2)]
        [marked-SP1 (filter (λ (sp) (state-pair-marked? sp)) loSP1)]
        [marked-SP2 (filter (λ (sp) (state-pair-marked? sp)) loSP2)])
    (and (andmap (λ (sp) (list? (member sp unmarked-SP2))) unmarked-SP1)
         (andmap (λ (sp) (list? (member sp marked-SP2))) marked-SP1))))

(define (update-mark? unmarked-pair marked-pairs transition-table alphabet)
  (let* ([ump-s1-transitions (first (filter (λ (transition) (eq? (first transition) (state-pair-s1 unmarked-pair))) transition-table))]
         [ump-s2-transitions (first (filter (λ (transition) (eq? (first transition) (state-pair-s2 unmarked-pair))) transition-table))]
         [state-pairs-from-transitions (map (λ (x) (let ([s1-tran (filter (λ (tran) (eq? x (first tran))) (second ump-s1-transitions))]
                                                         [s2-tran (filter (λ (tran) (eq? x (first tran))) (second ump-s2-transitions))])
                                                     (state-pair (second (first s1-tran)) (second (first s2-tran)) #f)))
                                            alphabet)])
    (ormap (λ (sp) (list? (member sp marked-pairs (λ (sp1 sp2) (or (and (eq? (state-pair-s1 sp1) (state-pair-s1 sp2))
                                                                        (eq? (state-pair-s2 sp1) (state-pair-s2 sp2)))
                                                                   (and (eq? (state-pair-s1 sp1) (state-pair-s2 sp2))
                                                                        (eq? (state-pair-s2 sp1) (state-pair-s1 sp2))))))))
           state-pairs-from-transitions)))
  


(define (make-states-table dfa)
  (define (make-half-table loSP new-table)
    (cond [(empty? loSP) new-table]
          [(boolean? (member (first loSP) new-table (λ (sp1 sp2) (and (eq? (state-pair-s1 sp1) (state-pair-s2 sp2))
                                                            (eq? (state-pair-s2 sp1) (state-pair-s1 sp2))))))
           (make-half-table (rest loSP) (cons (first loSP) new-table))]
          [else (make-half-table (rest loSP) new-table)]))
  (let* ([states (fsa-getstates dfa)]
         [init-states-pairing (for*/list ([s1 states]
                                          [s2 states]
                                          #:unless (eq? s1 s2))
                                (state-pair s1 s2 #f))]
         [other-half-of-table (make-half-table init-states-pairing '())]
         [half-of-table (make-half-table (reverse init-states-pairing) '())])
    
    (filter (λ (sp) (not (member sp other-half-of-table))) init-states-pairing)))  
      



#|
"minimize 4 - moore"
(define minimize4-test (map (λ (M) (if (boolean? (test-equiv-fsa (ndfa->dfa M) (minimize-dfa4 M)))
                                       (status (M 'whatami) #t)
                                       (status (M 'whatami) #f))) listofmachines))
"all passed?"
(andmap (λ (s) (status-result s)) minimize4-test)

"total"
(length listofmachines)
"pass"
(length (filter (λ (s) (status-result s)) minimize4-test))
"fail"
(- (length listofmachines) (length (filter (λ (s) (status-result s)) minimize4-test)))
"success rate"
(* 100 (/ (length (filter (λ (s) (status-result s)) minimize4-test)) (length listofmachines)))

|#