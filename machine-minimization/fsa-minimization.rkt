#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/misc.rkt"
         #;(only-in "../fsm-core/interface.rkt"
                  ndfa->dfa
                  EMP
                  DEAD
                  make-uncheckeddfa
                  make-uncheckedndfa)
         "../sm-graph.rkt")


(struct equivalence-class (non-final final) #:transparent)

(struct merged-state (new-symbol old-symbols) #:transparent)

(define EX1 (make-unchecked-dfa '(A B C D E)
                      '(0 1)
                      'A
                      '(E)
                      '((A 0 B) (A 1 C)
                        (B 0 B) (B 1 D)
                        (C 0 B) (C 1 C)
                        (D 1 E) (D 0 B)
                        (E 0 B) (E 1 C))
                      'no-dead))
                              ; Q 0 1 2 3 4 5 6 7
(define EX2-trans (make-unchecked-dfa '(A B C D E F G H)
                      '(0 1)
                      'A
                      '(C)
                      '((A 0 B) (A 1 F)
                        (B 0 G) (B 1 C)
                        (C 0 A) (C 1 C)
                        (D 0 C) (D 1 G)
                        (E 0 H) (E 1 F)
                        (F 0 C) (F 1 G)
                        (G 0 G) (G 1 E)
                        (H 0 G) (H 1 C))
                      'no-dead))

                                     ;A  B  C  D  E  F  G  H
(define EX2-vid (make-unchecked-dfa '(Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                      '(0 1)
                      'Q0
                      '(Q2)
                      '((Q0 0 Q1) (Q0 1 Q5)
                        (Q1 0 Q6) (Q1 1 Q2)
                        (Q2 0 Q0) (Q2 1 Q2)
                        (Q3 0 Q2) (Q3 1 Q6)
                        (Q4 0 Q7) (Q4 1 Q5)
                        (Q5 0 Q2) (Q5 1 Q6)
                        (Q6 0 Q6) (Q6 1 Q4)
                        (Q7 0 Q6) (Q7 1 Q2))
                      'no-dead))

(define EX3-vid (make-unchecked-dfa '(A B C D E F)
                      '(0 1)
                      'A
                      '(C D E)
                      '((A 0 B) (A 1 C)
                        (B 0 A) (B 1 D)
                        (C 0 E) (C 1 F)
                        (D 0 E) (D 1 F)
                        (E 0 E) (E 1 F)
                        (F 0 F) (F 1 F))
                      'no-dead))

(define EX4-vid-unreachable-final (make-unchecked-dfa '(A B C D E F G)
                      '(0 1)
                      'A
                      '(F) ;;'(B C G)
                      '((A 0 B) (A 1 C)
                        (B 0 D) (B 1 E)
                        (C 0 E) (C 1 D)
                        (D 0 G) (D 1 G)
                        (E 0 G) (E 1 G)
                        (F 0 D) (F 1 E)
                        (G 0 G) (G 1 G))
                      'no-dead))

(define EX4-vid (make-unchecked-dfa '(A B C D E F G)
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
    

;;starter code
(define (minimize-dfa1 M)
  (define (equivalence-class->dfa alphabet start finals EC transition-table)
    (let* ([merged-states (cond  [(set-member? (equivalence-class-non-final EC) start) start]
                                 [(not (set-empty? (equivalence-class-non-final EC))) (first (set->list (equivalence-class-non-final EC)))]
                                 ['()])]
           [other-states (append-map set->list (equivalence-class-final EC))]
           [new-states (if (empty? merged-states)
                           other-states
                           (cons merged-states other-states))]
           [table->rules (foldl (λ (row acc)
                                          (if (list? (member (first row) new-states))
                                              (append (map (λ (r) (if (list? (member (second r) new-states))
                                                                      (cons (first row) r)
                                                                      (list (first row) (first r) merged-states)))
                                                           (second row)) acc)
                                              acc))
                                        '()
                                        transition-table)])
      (make-unchecked-dfa new-states
                          alphabet
                          start
                          finals
                          table->rules
                          'no-dead)))

  (define (make-equivalence-classes states finals transition-table)
    (let ([first-equivalence-class (equivalence-class (list->set states) (list (list->set finals)))])
    
      (define (equivalence? matchee matcher last-ec-non-finals)
        (define (same-transitions? matchee-transitions matcher-transitions)
          (list? (andmap (λ (transition) (member transition matchee-transitions)) matcher-transitions)))

        (define (partial-match? matchee-transitions matcher-transitions)
          (list? (ormap (λ (t) (member t matchee-transitions)) matcher-transitions)))
        (let ([matchee-transitions (first (filter-map (λ (row) (and (eq? matchee (first row))
                                                                    (second row)))
                                                      transition-table))]
              [matcher-transitions (first (filter-map (λ (row) (and (eq? matcher (first row))
                                                                    (second row)))
                                                      transition-table))])
          (or (same-transitions? matchee-transitions matcher-transitions)
              (and (partial-match? matchee-transitions matcher-transitions)
                   (andmap (λ (t) (set-member? last-ec-non-finals (second t))) matchee-transitions)
                   (andmap (λ (t) (set-member? last-ec-non-finals (second t))) matcher-transitions)))))
                 
           
      (define (make-equivalence matchee potential-matches new-ec last-ec-non-finals)
        (cond [(set-empty? potential-matches)
               (struct-copy equivalence-class
                            new-ec
                            [final (cons (set matchee) (equivalence-class-final new-ec))])]
              [(equivalence? matchee (set-first potential-matches) last-ec-non-finals)
               (struct-copy equivalence-class
                            new-ec
                            [non-final (if (set-empty? (equivalence-class-non-final new-ec))
                                           (set-add (set-add (equivalence-class-non-final new-ec) matchee) (set-first potential-matches))
                                           (set-add (equivalence-class-non-final new-ec) matchee))])]
              [else (make-equivalence matchee (set-rest potential-matches) new-ec last-ec-non-finals)]))

    
      (define (make-next-equivalence-class last-ec matches-to-make new-ec)
        (let ([set-second (compose1 set-first set-rest)])
          (if (set-empty? matches-to-make)
              new-ec
              (make-next-equivalence-class last-ec
                                           (set-rest matches-to-make)
                                           (make-equivalence (set-first matches-to-make)
                                                             (set-union (set-rest matches-to-make) (equivalence-class-non-final new-ec))
                                                             new-ec
                                                             (equivalence-class-non-final last-ec))))))
    
      (define (make-equivalence-classes-helper LoEC)
        (define (same-equivalence-class? EC1 EC2)
          (and (list? (andmap (λ (t) (member t (equivalence-class-final EC1))) (equivalence-class-final EC2)))
               (set=? (equivalence-class-non-final EC1) (equivalence-class-non-final EC2))))
        (if (and (>= (length LoEC) 2)
                 (same-equivalence-class? (first LoEC) (second LoEC)))
            (first LoEC)
            (make-equivalence-classes-helper (cons (make-next-equivalence-class (first LoEC)
                                                                                (equivalence-class-non-final (first LoEC))
                                                                                (equivalence-class (set) (equivalence-class-final (first LoEC))))
                                                   LoEC)))) 
      (make-equivalence-classes-helper (list first-equivalence-class))))

  (define (make-transition-table dfa)
    (let ([states (fsa-getstates dfa)]
          [rules (fsa-getrules dfa)])
      (for/list ([state states]
                 #:do [(define applicable-rules (filter-map (λ (rule) (and (eq? state (first rule))
                                                                           (list (second rule) (third rule))))
                                                            rules))])
        (list state applicable-rules))))
  (let* ([dfa (ndfa->dfa M)]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [equivalence-class (make-equivalence-classes (filter (λ (s) (not (member s finals))) (fsa-getstates dfa)) finals transition-table)])
    (equivalence-class->dfa (fsa-getalphabet dfa) (fsa-getstart dfa) finals equivalence-class transition-table)))

;;implementation 2 - merging multiple states
(define (minimize-dfa2 M)
  (define (make-transition-table dfa)
    (let ([states (fsa-getstates dfa)]
          [rules (fsa-getrules dfa)])
      (for/list ([state states]
                 #:do [(define applicable-rules (filter-map (λ (rule) (and (eq? state (first rule))
                                                                           (list (second rule) (third rule))))
                                                            rules))])
        (list state applicable-rules))))
  
  (define (make-equivalence-classes states finals transition-table)

    (define (make-next-equivalence-class last-ec matches-to-make new-ec transition-table)

      (define (make-next-equivalence-class-helper last-ec matches-to-make new-ec transition-table)

        (define (make-equivalence-helper matchee lo-matches-to-make last-ec new-ec transition-table)

          (define (make-equivalence matchee potential-matches new-ec last-ec-non-finals last-ec-finals transition-table)
            (define (equivalence? matchee matcher last-ec-non-finals last-ec-finals transition-table)
                (define (same-transitions? matchee-transitions matcher-transitions)
                  (made-match? andmap matchee-transitions matcher-transitions))

                (define (partial-match? matchee-transitions matcher-transitions)
                  (made-match? ormap matchee-transitions matcher-transitions))

                (define (made-match? f matchee-transitions matcher-transitions)
                  (list? (f (λ (t) (member t matchee-transitions)) matcher-transitions)))
    
                (define (none-transition-to-ec-final transitions finals-set)
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
                                                                   matcher-destinations)])
                    (ormap (λ (d) (ormap (λ (s) (proper-subset? (set d) s)) matcher-destination-set-from-non-finals)) matchee-destinations)))
      
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
                           (none-transition-to-ec-final matchee-transitions last-ec-finals)
                           (none-transition-to-ec-final matcher-transitions last-ec-finals)))))
            (define (make-equivalence-set matchee matched non-finals)
              

          
              (let ([set-index (index-where non-finals (λ (s) (set-member? s matched)))])
                (if (number? set-index)
                    (cons (set-add (list-ref non-finals set-index) matchee) (remove (list-ref non-finals set-index) non-finals))
                    (cons (set matchee matched) non-finals))))
            (cond [(set-empty? potential-matches)
                   (if (or (ormap (λ (s) (proper-subset? (set matchee) s)) (equivalence-class-non-final new-ec))
                           (ormap (λ (s) (subset? (set matchee) s)) (equivalence-class-final new-ec)))
                       new-ec
                       (struct-copy equivalence-class
                                    new-ec
                                    [final (cons (set matchee) (equivalence-class-final new-ec))]))]
                  [(equivalence? matchee (set-first potential-matches) last-ec-non-finals last-ec-finals transition-table)
                   (struct-copy equivalence-class
                                new-ec
                                [non-final (make-equivalence-set matchee (set-first potential-matches) (equivalence-class-non-final new-ec))])]
                  [else (make-equivalence matchee (set-rest potential-matches) new-ec last-ec-non-finals last-ec-finals transition-table)]))
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
                                                         transition-table)
                                       transition-table)))
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
                                                                         transition-table)
                                                transition-table)))
      (if (empty? matches-to-make)
          new-ec 
          (make-next-equivalence-class last-ec
                                       (rest matches-to-make)
                                       (make-next-equivalence-class-helper last-ec (first matches-to-make) new-ec transition-table)
                                       transition-table)))
    
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
      
        (if (and (>= (length LoEC) 2)
                 (same-equivalence-class? (first LoEC) (second LoEC)))
            (first LoEC)
            (make-equivalence-classes-helper (cons (make-next-equivalence-class (first LoEC)
                                                                                (equivalence-class-non-final (first LoEC))
                                                                                (equivalence-class (list) (equivalence-class-final (first LoEC)))
                                                                                transition-table)
                                                   LoEC)))) 
      (make-equivalence-classes-helper (list first-equivalence-class))))

  (define (equivalence-class->dfa alphabet start finals EC transition-table)
    (define (search-for-merged-state old-state merged-states)
      (first (filter-map (λ (ms) (and (set-member? (merged-state-old-symbols ms) old-state)
                                      (merged-state-new-symbol ms)))                
                         merged-states)))
    (let* ([merged-states (map (λ (s) (if (set-member? s start)
                                          (merged-state start s)
                                          (merged-state (set-first s) s)))
                               (equivalence-class-non-final EC))]
           [other-states (append-map set->list (equivalence-class-final EC))]
           [new-states (append (map merged-state-new-symbol merged-states) other-states)]
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
                          finals
                          table->rules
                          'no-dead)))
  
  (let* ([dfa (ndfa->dfa M)]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [equivalence-class (make-equivalence-classes (filter (λ (s) (not (member s finals))) (fsa-getstates dfa)) finals transition-table)])
    (equivalence-class->dfa (fsa-getalphabet dfa) (fsa-getstart dfa) finals equivalence-class transition-table)))


;;implementation 3 -> merging of finals 
(define (minimize-dfa3 M)
  (define (make-equivalence-classes states finals transition-table)
    (define (make-next-equivalence-class last-ec matches-to-make new-ec transition-table)
      (define (make-next-equivalence-class-helper last-ec matches-to-make new-ec transition-table)
        (define (make-equivalence-helper matchee lo-matches-to-make last-ec new-ec transition-table)
          (define (make-equivalence matchee potential-matches new-ec last-ec-non-finals last-ec-finals transition-table)
            (define (make-equivalence-set matchee matched non-finals)
              (let ([set-index (index-where non-finals (λ (s) (set-member? s matched)))])
                (if (number? set-index)
                    (cons (set-add (list-ref non-finals set-index) matchee) (remove (list-ref non-finals set-index) non-finals))
                    (cons (set matchee matched) non-finals))))
  
            (define (equivalence? matchee matcher last-ec-non-finals last-ec-finals transition-table)
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
                   (if (or (ormap (λ (s) (proper-subset? (set matchee) s)) (equivalence-class-non-final new-ec))
                           (ormap (λ (s) (subset? (set matchee) s)) (equivalence-class-final new-ec)))
                       new-ec
                       (struct-copy equivalence-class
                                    new-ec
                                    [final (cons (set matchee) (equivalence-class-final new-ec))]))]
                  [(equivalence? matchee (set-first potential-matches) last-ec-non-finals last-ec-finals transition-table)
                   (struct-copy equivalence-class
                                new-ec
                                [non-final (make-equivalence-set matchee (set-first potential-matches) (equivalence-class-non-final new-ec))])]
                  [else (make-equivalence matchee (set-rest potential-matches) new-ec last-ec-non-finals last-ec-finals transition-table)]))
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
                                                         transition-table)
                                       transition-table)))
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
                                                                         transition-table)
                                                transition-table)))
      (if (empty? matches-to-make)
          new-ec 
          (make-next-equivalence-class last-ec
                                       (rest matches-to-make)
                                       (make-next-equivalence-class-helper last-ec (first matches-to-make) new-ec transition-table)
                                       transition-table)))
  
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
      
        (cond [(and (>= (length LoEC) 2)
                    (same-equivalence-class? (first LoEC) (second LoEC)))
               (first LoEC)]
              [(ormap (λ (s) (> (set-count s) 1)) (equivalence-class-final (first LoEC)))
               (let ([new-ec-for-non-finals (make-next-equivalence-class
                                             (first LoEC)
                                             (equivalence-class-non-final (first LoEC))
                                             (equivalence-class (list) (equivalence-class-final (first LoEC)))
                                             transition-table)])
                 (make-equivalence-classes-helper
                  (cons (struct-copy equivalence-class
                                     (first LoEC)
                                     [non-final (equivalence-class-non-final new-ec-for-non-finals)]
                                     [final (remove-duplicates
                                             (append (equivalence-class-final new-ec-for-non-finals)
                                                     (equivalence-class-final
                                                      (make-next-equivalence-class
                                                       (first LoEC)
                                                       (equivalence-class-final (first LoEC))
                                                       (equivalence-class (list) (equivalence-class-final (first LoEC)))
                                                       transition-table))))])
                        LoEC)))]
              [else (make-equivalence-classes-helper (cons (make-next-equivalence-class (first LoEC)
                                                                                        (equivalence-class-non-final (first LoEC))
                                                                                        (equivalence-class (list) (equivalence-class-final (first LoEC)))
                                                                                        transition-table)
                                                           LoEC))])) 
      (make-equivalence-classes-helper (list first-equivalence-class)))) 
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
           [new-states (append (map merged-state-new-symbol merged-states) other-states)]
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
                          finals
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
  
  (let* ([dfa (ndfa->dfa M)]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [equivalence-class (make-equivalence-classes (filter (λ (s) (not (member s finals))) (fsa-getstates dfa)) finals transition-table)])
    (equivalence-class->dfa (fsa-getalphabet dfa) (fsa-getstart dfa) finals equivalence-class transition-table)))



;;implementation 4 -> removing unreachables
(define (minimize-dfa4 M)
  (let* ([dfa (remove-unreachables (ndfa->dfa M))]
         [transition-table (make-transition-table dfa)]
         [finals (fsa-getfinals dfa)]
         [equivalence-class (make-equivalence-classes (filter (λ (s) (not (member s finals))) (fsa-getstates dfa)) finals transition-table)])
    (if (list? equivalence-class)
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
         [new-states (append (map merged-state-new-symbol merged-states) other-states)]
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
                        finals
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
                 (if (or (ormap (λ (s) (proper-subset? (set matchee) s)) (equivalence-class-non-final new-ec))
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
             (if (= (length LoEC) 4) LoEC (first LoEC))]
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
                                                                                      (equivalence-class (list) (equivalence-class-final (first LoEC)))
                                                                                      transition-table
                                                                                      #f)
                                                         LoEC))])) 
    (make-equivalence-classes-helper (list first-equivalence-class))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; L = ab* U (ab)*
(define M (make-unchecked-ndfa '(S A B C D E F G H I)
                     '(a b)
                     'S
                     '(A B C D)
                     `((S ,EMP A)
                       (S ,EMP D)
                       (A a B)
                       (B ,EMP C)
                       (C b I)
                       (I ,EMP B)
                       (D ,EMP E)
                       (E a F)
                       (F ,EMP G)
                       (G b H)
                       (H ,EMP D))))

(define L (make-unchecked-ndfa '(S A B C D E)
                               '(a b)
                               'S
                               '(S B C E)
                               `((S ,EMP A) (S ,EMP C) (A a B) (B b B)
                                 (C a D) (C b E) (D b C) (E b E))))


(define aa*Uab* (make-unchecked-ndfa '(K B D)
                           '(a b)
                           'K
                           '(B D)
                           `((K a D) (K a B)
                                     (B a B)
                                     (D b D))))

(define AT-LEAST-ONE-MISSING
  (make-unchecked-ndfa '(S A B C)
             '(a b c)
             'S
             '(A B C)
             `((S ,EMP A) (S ,EMP B) (S ,EMP C)
                          (A b A) (A c A)
                          (B a B) (B c B)
                          (C a C) (C b C))))

(define d (ndfa->dfa AT-LEAST-ONE-MISSING))

(define p2-ndfa
  (make-unchecked-ndfa '(S A B C D E)
             '(a b)
             'S
             '(C E)
             `((S ,EMP A) (S ,EMP D)
                          (A a B) (A ,EMP C)
                          (B b A)
                          (C b C)
                          (D a E)
                          (E b E))))

(define AB*B*UAB*
  (make-unchecked-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))

(define AB*B*UAB*2
  (make-unchecked-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H) (H a S))))

(define aa-ab
  (make-unchecked-ndfa `(S A B F)
             '(a b)
             'S
             '(A B F)
             `((S a A) (S a B) (S ,EMP F)
                       (A a A)
                       (B b B))))

(define ends-with-two-bs
  (make-unchecked-ndfa `(S A B)
             '(a b)
             'S
             '(B)
             `((S a S) (S b S) (S b A)
                       (A b B)
                       (B b B))))
(define ENDS-WITH-TWO-Bs
  (make-unchecked-ndfa `(S A B)
             '(a b)
             'S
             '(B)
             `((S a S) (S b A)
                       (A b B) (A a S)
                       (B b B) (B a S))))

(define nd-a* (make-unchecked-ndfa '(K H)
                         '(a b)
                         'K
                         '(H)
                         `((K ,EMP H)
                           (H a H))))

(define missing-exactly-one
  (make-unchecked-ndfa '(S A B C D E F G H I J K L M N O P)
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

(define nd (make-unchecked-ndfa '(S Z Y A B)
                      '(a b)
                      'S
                      '(B)
                      `((S b Z)
                        (S b Y)
                        (Y a A)
                        (Z a A)
                        (A a B))))

(define n (make-unchecked-ndfa '(K H F M I)
                     '(a b)
                     'K
                     '(I)
                     `((K b H)
                       (H ,EMP F)
                       (H ,EMP M)
                       (F a I)
                       (M a I))))

(define nk (make-unchecked-ndfa '(K H F M I)
                      '(a b)
                      'K
                      '(I)
                      `((K b H)
                        (H ,EMP F)
                        (F ,EMP M)
                        (M ,EMP I)
                        (I ,EMP H))))

(define ab*-U-ab*b*-ndfa 
       (make-unchecked-ndfa '(S A B C D E)
                  '(a b)
                  'S
                  '(C E)
                  `((S ,EMP A) (S ,EMP D) (A a B) (A ,EMP C)
                               (B b A) (C b C) (D a E) (E b E))))


(define PROP-BI (make-unchecked-dfa '(S M N)
                          '(0 1)
                          'S
                          '(N M)
                          '((S 1 M)
                            (S 0 N)
                            (M 0 M)
                            (M 1 M))))

(define DNA-SEQUENCE (make-unchecked-dfa '(K H F M I D B S R) ;C)
                               '(a t c g)
                               'K
                               '(K F I B R)
                               `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                         (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                         (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                         (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

;; L = (aba)* U (ab)*
(define ND
  (make-unchecked-ndfa '(S A B C D E)
             '(a b)
             'S
             '(S)
             `((S a A) (S a B)
                       (A b C)
                       (B b D)
                       (C a E)
                       (D ,EMP S)
                       (E ,EMP S))))

(define ND2
  (make-unchecked-ndfa
   '(S A B C D E F)
   '(a b)
   'S
   '(D E)
   `((S ,EMP A) (S ,EMP B)
                (A ,EMP D)
                (D b D) (D ,EMP F)
                (B a E) (B b B)
                (E a E) (E b E) (E ,EMP C))))

(define ND3
  (make-unchecked-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) (S ,EMP B)
                          (A a A) (A ,EMP C)
                          (C ,EMP D)
                          (D ,EMP B)
                          (B b B))))

(define ND4 (make-unchecked-ndfa '(S ds)
                       '(a b)
                       'S
                       '(ds)
                       `((S a ds)
                         (ds a ds))))

(define ND5
  (make-unchecked-ndfa '(S A B C D)
             '(a b)
             'S
             '(B)
             `((S ,EMP A) 
               (A ,EMP B)
               (B ,EMP C)
               (C ,EMP D)
               (D ,EMP S))))

(define EVEN-NUM-Bs
  (make-unchecked-dfa '(S F)
            '(a b)
            'S
            '(S)
            `((S a S) (S b F)
                      (F a F) (F b S))
            'no-dead))

(define M2 (make-unchecked-dfa `(S A F ,DEAD)
                     '(a b)
                     'S
                     '(F)
                     `((S a A) (S b ,DEAD) (,DEAD a ,DEAD) (,DEAD b ,DEAD)
                               (A a ,DEAD) (A b F)
                               (F a ,DEAD) (F b F))
                     'no-dead))
