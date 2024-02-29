; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

;;; DFSA & NDFSA

(module fsa racket
  (require "configuration.rkt" "regexp.rkt" "word.rkt" "rules.rkt" "state.rkt" 
           "string.rkt" "path.rkt" "constants.rkt" "misc.rkt" "regular-grammar.rkt"
           "regexp.rkt"
           )
  
  (provide make-unchecked-dfa make-unchecked-ndfa union-fsa concat-fsa kleenestar-fsa complement-fsa intersection-fsa
           fsa->regexp regexp->fsa ndfa->dfa test-fsa test-equiv-fsa
           printable-rrules fsa->rg rg->fsa rename-states-fsa show-transitions-fsa fsa-getrules fsa-getstates
           fsa-getstart fsa-getfinals fsa-getalphabet apply-fsa)
  
  
  ; CONSTRUCTORS
  
  ; make-unchecked-dfa: (listof state) alphabet state (listof state) (listof rule)) [symbol] --> dfsa
  (define (make-unchecked-dfa states sigma start finals deltas . adddead)
    (define (concrete-dfsa states sigma start finals deltas)
      (lambda (w . l)    
        ; wi state --> config
        (define (make-transition a-wi s)
          (cond [(word-consumed? a-wi w) empty]
                [else (mk-config (+ a-wi 1)
                                 (get-next-state (get-symb-word w a-wi) s deltas))]))
        ; symbol state rules --> state
        (define (get-next-state symb state rules)
          (let ((rule (get-applicable-fsarules symb state rules)))
            (cond [(null? rule) DEAD]
                  [else (to-state-fsarule (from-state-fsarule rule))])))
        
        ; wi state --> (append (listof config) (list ('accept or 'reject)))
        (define (consume a-wi s)
          (cond [(word-consumed? a-wi w) (get-result s finals)] ; w consumed return accept or reject list
                [else (let ((new-config (make-transition a-wi s)))
                        (cons new-config (consume (wi-config new-config)
                                                  (state-config new-config))))]))
        (cond [(eq? w 'whatami) 'dfa]
              [else (let ((res (mk-path (mk-config 0 start) (consume 0 start))))
                      (cond [(null? l) (last res)]
                            [(eq? (car l) 'transitions) (convert-path res w)]
                            [(eq? (car l) 'get-deltas) deltas]
                            [(eq? (car l) 'get-states) states]
                            [(eq? (car l) 'get-sigma) sigma]
                            [(eq? (car l) 'get-start) start]
                            [(eq? (car l) 'get-finals) finals]
                            [(eq? (car l) 'is-deterministic?) #t]
                            [else (raise-arguments-error 'Unkown-request
                                                         "The machine does not understand"
                                                         "Request" (car l))]))])))
    (let [(dead (if (member DEAD states)
                    (generate-symbol 'D states)
                    DEAD))]
      (concrete-dfsa (if (null? adddead) ; add dead state if optional argument is missing
                         (cons dead states)
                         states)
                     sigma
                     start
                     finals
                     (append deltas 
                             (if (null? adddead)
                                 (new-dead-rules (cons dead states) sigma deltas dead)
                                 null))))) ; add dead state  transitions if optional argument is missing
  
  
  ; make-unchecked-ndfa: (listof states) alphabet state (listof state) (listof rule)
  ;            -->
  ;            (list-of symbol) [symbol] --> symbol OR 
  ;                                           (list path symbol) OR
  ;                                           (list-of rule)
  (define (make-unchecked-ndfa states sigma start finals deltas . adddead)
    (define (concrete-ndfsa states sigma start finals deltas)
      ; word [symbol] --> symbol or (listof (list word state)) or boolean or (listof rule) or state or (listof state)
      (lambda (w . l)
        ; symb state --> (listof rule)
        (define (get-rules symb state) (get-applicable-fsarules symb state deltas))
        
        ; natnum state rule --> config
        (define (mk-transition pos r)
          (cond [(not (eq? EMP (symb-fsarule r))) (list (+ pos 1) (to-state-fsarule r))]
                [else (list pos (to-state-fsarule r))]))
        
        ; config (listof config) --> (listof config)
        ; Purpose: return the list of configs that have not been visited
        (define (mk-transitions config visited)
          (let* ((pos (wi-config config))
                 (state (state-config config))
                 (rules (get-applicable-fsarules (if (= pos (length w)) EMP (list-ref w pos)) ; if input consumed only e-transitions are possible
                                                 state 
                                                 deltas))
                 (new-configs (filter (lambda (c) (not (member-config? c visited)))
                                      (map (lambda (r) (mk-transition pos r)) rules))))
            new-configs))
        
        ;(listof config) --> config or null
        (define (first-accept-config configs)
          (cond [(null? configs) null]
                [(and (>= (wi-config (car configs)) (length w)) ; can only accept if all input is consumed
                      (member (state-config (car configs)) finals)) 
                 (car configs)]
                [else (first-accept-config (cdr configs))]))
        
        ;(listof config) (listof path) --> path
        ; BFS search
        (define (consume visited paths)
          (cond [(null? paths) null] ; null paths means there is no path that accepts w
                [else (let* ((path (car paths)) ; the first path                             
                             (config (first-config-path path)) ; the first config of the path
                             (newconfigs (mk-transitions config visited)) ; a list of configs obtained using one transition from config
                             (accept (first-accept-config newconfigs))) ; null or the first accept configuration in newconfigs
                        (cond [(not (null? accept)) (mk-path accept path)] 
                              [else (consume (cons config visited) 
                                             (append (cdr paths) (map (lambda (c) (mk-path c path)) newconfigs)))]))]))
        (cond [(eq? w 'whatami) 'ndfa]
              [else (let ((res (if (and (empty-word? w) (member start finals))
                                   (mk-path (mk-config 0 start) null) ; if the input is empty and the initial state is a final state --> initial configuration is accepting
                                   (consume null (list (mk-path (mk-config 0 start) null)))))) ; else make transitions using BFS searching for an accepting path if it exists
                      (cond [(null? l) (if (empty-path? res) 'reject 'accept)]
                            [(eq? (car l) 'transitions) 
                             (if (empty-path? res) 'reject (append (printable-path w res) '(accept)))]
                            [(eq? (car l) 'get-deltas) deltas]
                            [(eq? (car l) 'get-states) states]
                            [(eq? (car l) 'get-sigma) sigma]
                            [(eq? (car l) 'get-start) start]
                            [(eq? (car l) 'get-finals) finals]
                            [(eq? (car l) 'is-deterministic?) #f]
                            [else (raise-arguments-error 'Unkown-request
                                                         "The machine does not understand"
                                                         "Request" (car l))]))])))
    (concrete-ndfsa states
                    sigma
                    start
                    finals
                    deltas))
  ;(append deltas (if (null? adddead) (new-dead-rules (cons DEAD states) sigma deltas) null)))) dead state not added to an ndfa, because missing transitions in the transition relation may violate state INVs.


  ;;; ndfsa->dfsa 
  
  
  ; fsm --> fsm
  #;(define (ndfa->dfa m)
      (define (ndfsm->dfsm m)
        (let* ((esr (compute-superstate-rules null 
                                              (list (sort-symbols (empties (fsa-getstart m) (fsa-getrules m)))) 
                                              (fsa-getalphabet m) 
                                              (fsa-getrules m) 
                                              null))
               (new-states (extract-sstates esr))
               (new-start (superstate->state (sort-symbols (empties (fsa-getstart m) (fsa-getrules m)))))
               (new-finals (map superstate->state (extract-final-ss new-states (fsa-getfinals m))))
               (new-rules (convert2rules esr)))
          (make-unchecked-dfa (map superstate->state new-states) (fsa-getalphabet m) new-start new-finals new-rules 'no-dead)))
      (if (eq? (m 'whatami) 'dfa)
          m
          (ndfsm->dfsm m)))

  ;; ndfa --> dfa
  ;; Convert the given ndfa to an equivalent dfa
  (define (ndfa->dfa M)

    ;; state emps-tbl --> ss
    ;; Purpose: Extract the empties of the given state
    ;; Assume: Given state is in the given table
    (define (extract-empties st empties)
      (second (first (filter (λ (e) (eq? (first e) st)) empties))))

    ;; (listof ss) alphabet emps-tbl (listof ndfa-rule) (listof ss) --> (listof ss-dfa-rule)
    ;; Purpose: Compute the dfa rules
    ;; Accumulator Invariants
    ;;   to-search-ssts = the super states that must still be explored
    ;;             ssts = the super states explored
    (define (compute-ss-dfa-rules to-search-ssts sigma empties rules ssts)
      ;; state symbol (listof ndfa-rule) emps-tbl --> ss
      ;; Purpose: Find the reachable super state from the given state and the given alphabet element
      (define (find-reachables-from-st-on-a st a rules empties)
        (let* [(rls (filter (λ (r) (and (eq? (first r) st) (eq? (second r) a)))
                            rules))
               (to-states (map third rls))]
          (remove-duplicates (append-map (λ (st) (extract-empties st empties)) to-states))))

      ;; state alphabet (listof ndfa-rule) emps-tbl --> (listof ss)
      ;; Purpose: Find the reachable super state from the given state for each element of the given alphabet
      (define (find-reachables-from-st st sigma rules empties)
        (map (λ (a) (find-reachables-from-st-on-a st a rules empties))
             sigma))

      ;; ss alphabet (listof ndfa-rule) emps-tbl --> (listof (listof ss))
      ;; Purpose: Compute reachable super states from given super state
      (define (find-reachables ss sigma rules empties)
        (map (λ (st) (find-reachables-from-st st sigma rules empties)) ss))

      ;; natnum (listof (listof ss)) --> (listof ss)
      ;; Purpose: Return ss of ith (listof state) in each given list element
      (define (get-reachable i reachables)
        (remove-duplicates (append-map (λ (reached) (list-ref reached i))
                                       reachables)))
      (if (empty? to-search-ssts)
          '()
          (let* [(curr-ss (first to-search-ssts))
                 (reachables (find-reachables curr-ss sigma rules empties))
                 (to-super-states (build-list (length sigma)
                                              (λ (i) (get-reachable i reachables))))
                 (new-rules (map (λ (sst a) (list curr-ss a sst))
                                 to-super-states
                                 sigma))]
            (append new-rules (compute-ss-dfa-rules
                               (append (rest to-search-ssts)
                                       (filter (λ (ss) (not (member ss (append to-search-ssts ssts))))
                                               to-super-states))
                               sigma
                               empties
                               rules
                               (cons curr-ss ssts))))))

    ;; (listof ss) --> ss-name-tbl
    ;; Purpose: Create a table for ss names
    (define (compute-ss-name-tbl super-states)
      (map (λ (ss) (list ss (generate-symbol 'X '(X))))                                      
           super-states))

    ;; (listof state) rules --> emps-tbl
    ;; Purpose: Compute empties table for all given states
    (define (compute-empties-tbl states rules)
      ;; state (listof state) (listof ndfa-rule) --> (listof ndfa-rule)
      ;; Purpose: Extract empty transitions to non-generated states for the given state
      (define (get-e-trans state gen-states rules)
        (filter (λ (r) (and (eq? (first r) state)
                            (eq? (second r) EMP)
                            (not (member (third r) gen-states))))
                rules))
    
      ;; (listof state) (listof ndfa-rules) (listof state) --> (listof state)
      ;; Purpose: Compute the empties for the states left to explore in the first given (listof state)
      ;; Accumlator Invariants:
      ;;     to-search = states reachable by consuming no input that have not been visited
      ;;       visited = states reachable by consuming no input
      (define (compute-empties to-search rules visited)
        (if (empty? to-search)
            visited
            (let* [(curr (first to-search))
                   (curr-e-rules (get-e-trans curr (append to-search visited) rules))]
              (compute-empties (append (rest to-search) (map third curr-e-rules))
                               rules
                               (cons curr visited)))))
      (map (λ (st) (list st (compute-empties (list st) rules '()))) states))

    ;; (listof state) alphabet state (listof state) (list-of ndfa-rule) --> dfa
    ;; Purpose: Create a dfa from the given ndfa components
    (define (convert states sigma start finals rules)
      (let* [(empties (compute-empties-tbl states rules))
             (ss-dfa-rules 
              (compute-ss-dfa-rules (list (extract-empties start empties))
                                    sigma
                                    empties
                                    rules
                                    '()))
             (super-states (remove-duplicates
                            (append-map
                             (λ (r) (list (first r) (third r)))
                             ss-dfa-rules)))
             (ss-name-tbl (compute-ss-name-tbl super-states))]
        (make-unchecked-dfa (map (λ (ss) (second (assoc ss ss-name-tbl)))
                                 super-states)
                            sigma
                            (second (assoc (first super-states) ss-name-tbl))
                            (map (λ (ss) (second (assoc ss ss-name-tbl)))
                                 (filter (λ (ss) (ormap (λ (s) (member s finals)) ss))
                                         super-states))
                            (map (λ (r) (list (second (assoc (first r) ss-name-tbl))
                                              (second r)
                                              (second (assoc (third r) ss-name-tbl))))
                                 ss-dfa-rules)
                            'no-dead)))
  
    (if (eq? (M 'whatami) 'dfa)
        M
        (convert (fsa-getstates M)
                 (fsa-getalphabet M)
                 (fsa-getstart M)
                 (fsa-getfinals M)
                 (fsa-getrules M))))
  
  ; (listof superstate) (listof superstate) alphabet (listof rule) (listof ssr) --> (listof ssr) 
  (define (compute-superstate-rules visited tovisit sigma rules res)
    ; state symbol --> (listof state)
    (define (get-to-states s a) 
      (let* ((rls (filter (lambda (r) (and (eq? (from-state-fsarule r) s) (eq? (symb-fsarule r) a))) 
                          rules))
             (neighbors (map to-state-fsarule rls)))
        (append-map (lambda (s) (empties s rules)) neighbors)))
    ; (listof (listof state)) symbol --> (list (listof state) symbol (listof state))
    (define (gen-rule SS a)
      (let ((to-states (sort-symbols (remove-duplicates (append-map (lambda (s) (get-to-states s a)) SS)))))
        (if (null? to-states)
            (list SS a (list DEAD))
            (list SS a to-states))))
    ; superstate alphabet --> (listof ssr)
    (define (gen-rules SS sigma)
      (map (lambda (a) (gen-rule SS a)) sigma))
    (cond [(null? tovisit) res]
          [else (let* ((SS (car tovisit)) ;; next super state to visit
                       (new-visited (cons SS visited)) ; new list of visited states
                       (new-SS-rules (gen-rules SS sigma)) ; new rules for SS
                       (genSS (append visited tovisit)) ; list of all super states generated
                       (new-generated-superS (filter (lambda (A) (not (member A genSS))) 
                                                     (extract-sstates new-SS-rules)))
                       (new-tovisit (append (cdr tovisit) new-generated-superS))
                       (new-res (append res new-SS-rules)))
                  (compute-superstate-rules new-visited new-tovisit sigma rules new-res))]))
  
  ;;; ndfsa->dfsa END
  
  ; (listof state) fsa --> fsa
  (define (rename-states-fsa los m)
    (let* ((mstates (fsa-getstates m))
           (sts mstates #;(if (member DEAD mstates) mstates (cons DEAD mstates)))
           (rename-table (map (lambda (s) (list s (generate-symbol s los)))
                              sts))
           (new-states (map (lambda (s) (cadr (assoc s rename-table))) sts))
           (new-start (cadr (assoc (fsa-getstart m) rename-table)))
           (new-finals (map (lambda (s) (cadr (assoc s rename-table))) (fsa-getfinals m)))
           (new-rules (map (lambda (r) (list (cadr (assoc (from-state-fsarule r) rename-table))
                                             (symb-fsarule r)
                                             (cadr (assoc (to-state-fsarule r) rename-table))))
                           (m null 'get-deltas))))
      (cond [(m '() 'is-deterministic?) (make-unchecked-dfa new-states (m null 'get-sigma) new-start new-finals new-rules 'nodead)]
            [else (make-unchecked-ndfa new-states (m null 'get-sigma) new-start new-finals new-rules 'nodead)])))
  
  ; fsa fsa --> ndfa
  (define (union-fsa m1 m)
    (let* ((nm1 (rename-states-fsa (fsa-getstates m) m1))
           (nm2 (rename-states-fsa (fsa-getstates nm1) m)))
      (let* ((new-start (generate-symbol START (append (fsa-getstates nm1) 
                                                       (fsa-getstates nm2))))
             (new-states (cons new-start
                               (append (fsa-getstates nm1) 
                                       (fsa-getstates nm2)))) ; nm1 & nm2 & s have no common state names
             (alphabet (remove-duplicates (append (fsa-getalphabet nm1) (fsa-getalphabet nm2))))
             (new-finals (union-states (fsa-getfinals nm1) (fsa-getfinals nm2)))
             (new-rules (cons (mk-fsarule new-start EMP (fsa-getstart nm1)) 
                              (cons (mk-fsarule new-start EMP (fsa-getstart nm2)) 
                                    (append (nm1 null 'get-deltas) 
                                            (nm2 null 'get-deltas))))))
        (make-unchecked-ndfa new-states
                             alphabet
                             new-start
                             new-finals
                             new-rules))))
  
  ; fsa fsa --> fsa
  (define (concat-fsa m1 m2)
    (let* ((nm1 (rename-states-fsa (fsa-getstates m2) m1))
           (nm2 m2))
      (make-unchecked-ndfa (union-states (fsa-getstates nm1) (fsa-getstates nm2)) ; nm1 & nm2 have no common state names
                           (remove-duplicates (append (fsa-getalphabet nm1) (fsa-getalphabet nm2)))
                           (fsa-getstart nm1)
                           (fsa-getfinals nm2)
                           (append (fsa-getrules nm1)
                                   (fsa-getrules nm2)
                                   (map (lambda (s) (mk-fsarule s EMP (fsa-getstart nm2))) 
                                        (fsa-getfinals nm1))))))
  
  ; fsa --> fsa
  (define (kleenestar-fsa m1)
    (let ((START (generate-symbol 'S-0 (fsa-getstates m1))))
      (make-unchecked-ndfa (cons START (fsa-getstates m1))
                           (fsa-getalphabet m1)
                           START
                           (cons START (fsa-getfinals m1))
                           (cons (mk-fsarule START EMP (fsa-getstart m1))
                                 (append (fsa-getrules m1)
                                         (map (lambda (s) (mk-fsarule s EMP (fsa-getstart m1))) 
                                              (fsa-getfinals m1)))))))
  
  ; dfsa --> dfsa
  (define (complement-fsa m)
    ; rename-states-fsa is not needed, because no new states are introduced
    (let ((m1 (ndfa->dfa m)))
      (make-unchecked-dfa (fsa-getstates m1)
                          (fsa-getalphabet m1)
                          (fsa-getstart m1)
                          (minus-set (fsa-getstates m1) (fsa-getfinals m1))
                          (fsa-getrules m1)
                          'nodead)))
  
  ; fsa fsa --> fsa
  (define (intersection-fsa m1 m2)
    (let ((km1 (complement-fsa m1))
          (km2 (complement-fsa m2)))
      (let* ((K (union-fsa km1 km2))
             (notK (complement-fsa K)))
        notK)))
  
  ;;; OBSERVERS
  
  ;  ; fsa --> string
  ;  ; Purpose: To return the the string for the regexp of the language of the given fsa
  ;  (define (fsa->regexp m)
  ;    ; state (listof state) --> (listof state)
  ;    (define (start-first s sts)
  ;      (cons s (filter (lambda (st) (not (eq? st s))) sts)))
  ;    ; (listof (list natnum state) (listof state) (listof rule) natnum state --> regexp
  ;    (define (build-reg-exp table finals rules n start)
  ;      ; state (listof (list natnum state)) --> natnum
  ;      (define (get-assoc s t)
  ;        (cadr (assoc s (map (lambda (a) (list (cadr a) (car a))) t))))
  ;      
  ;      ; (listof symbol) --> string
  ;      (define (losymb->string l) (lostr->string (los->lostr l)))
  ;      
  ;      ; (listof symbol) --> union-regexp or singleton-regexp
  ;      (define (make-lunion-regexp l)
  ;        (let ((car-regexp (if (eq? (car l) EMP) (empty-regexp) (make-unchecked-singleton (symbol->string (car l))))))
  ;          (cond [(null? (cdr l)) car-regexp]
  ;                [else (make-unchecked-union car-regexp
  ;                                            (make-lunion-regexp (cdr l)))])))
  ;      
  ;      
  ;      ; natnum natnum natnum --> regexp
  ;      (define (R i j k)
  ;        (cond [(= k 0)
  ;               (let* ((rls (filter (lambda (r) (and (eq? (from-state-fsarule r) (cadr (assoc i table)))
  ;                                                    (eq? (to-state-fsarule r) (cadr (assoc j table)))))
  ;                                   rules)))
  ;                 (let* ((x (cond [(= (length rls) 0) (null-regexp)]
  ;                                 [else (let ((rs (map string->symbol
  ;                                                      (sort-strings (map symbol->string 
  ;                                                                         (remove-duplicates (map symb-fsarule rls)))))))
  ;                                         (make-lunion-regexp rs))])))
  ;                   (let* ((ans (if (= i j) (make-unchecked-union (empty-regexp) x) x)))
  ;                     ans)))]
  ;              [else (let* ((R1 (R i j (- k 1)))
  ;                           (R2 (R i k (- k 1)))
  ;                           (R3 (R k k (- k 1)))
  ;                           (R4 (R k j (- k 1)))
  ;                           )
  ;                      (make-unchecked-union R1
  ;                                            (make-unchecked-concat R2
  ;                                                                   (make-unchecked-concat (make-unchecked-kleenestar R3) R4))))]))
  ;      (cond [(null? finals) (null-regexp)]
  ;            [(null? (cdr finals)) (let* ((current-final (car finals)))
  ;                                    (R 1 (get-assoc current-final table) n))] 
  ;            [else (let* ((current-final (car finals)))
  ;                    (make-unchecked-union (R 1 (get-assoc current-final table) n)
  ;                                          (build-reg-exp table (cdr finals) rules n start)))]))
  ;    (let* ((sts (start-first (fsa-getstart m) (fsa-getstates m)))
  ;           (state-table (build-list (length sts)
  ;                                    (lambda (i) (list (+ i 1) (list-ref sts i)))))
  ;           )
  ;      (printable-regexp
  ;       (simplify-regexp (build-reg-exp state-table
  ;                                       (fsa-getfinals m) 
  ;                                       (m null 'get-deltas) 
  ;                                       (length (fsa-getstates m)) 
  ;                                       (fsa-getstart m))))))

  ;; fsa --> regexp
  (define (fsa->regexp m)

    (define (degree-in s rules)
      (length (filter (λ (r) (eq? s (third r))) rules)))

    (define (degree-out s rules) 
      (length (filter (λ (r) (eq? s (first r))) rules)))

    ;; nonempty-(listof edges) --> edge
    ;; Purpose: Merge given edges into a single edge
    (define (merge-edges edges)

      (define (mk-union-label r1 r2)
        (cond [(and (empty-regexp? r1) (kleenestar-regexp? r2)) r2]
              [(and (empty-regexp? r2) (kleenestar-regexp? r1)) r1]
              [else (make-unchecked-union r1 r2)]))
    
      (cond [(empty? (rest edges)) (first edges)] ;; only 1 edge
            [(empty? (rest (rest edges))) ;; only two edges
             (list (first (first edges))
                   (mk-union-label (second (first edges)) (second (second edges)))
                   (third (first edges)))]
            [else (let ((u1 (second (first edges)))
                        (u2 (merge-edges (rest edges))))
                    (list (first (first edges))
                          (mk-union-label u1 u2)
                          (third (first edges))))]))

    (define (merge-medges G ne)
      (if (empty? G)
          ne
          (let* ((frule (first G))
                 (ffrom (first frule))
                 (fto (third frule))
                 (medges (filter (λ (e) (and (eq? (first e) ffrom) (eq? (third e) fto))) G)))
            (merge-medges (filter (λ (e) (not (and (eq? (first e) ffrom) (eq? (third e) fto))))
                                  G)
                          (cons (merge-edges medges) ne)))))
    

    ;; (listof edges) (listof state) --> (listof edges)
    ;; Assume: The list of states does not include start/final states
    (define (remove-internal-states G istates strt fnl)

      (define (remove-state s)
      
        ;; state (listof edges) (listof edges) --> (listof edges)
        (define (make-new-rem-edges s G into-s-edges out-s-edges)

          ;; regexp regexp --> regexp
          (define (mk-concat-regexp re1 re2)          
            (cond [(empty-regexp? re1) re2]
                  [(empty-regexp? re2) re1]
                  [else (make-unchecked-concat re1 re2)]))

          (define (merge-out-edge e ins)
            (map (λ (ie)
                   (list (first ie)
                         (let* ((self-loop-lst (filter (λ (e) (and (eq? s (first e)) (eq? s (third e))))
                                                       G))
                                (self-loop-edge (if (null? self-loop-lst) '() (first self-loop-lst))))
                           (if (null? self-loop-edge)
                               (mk-concat-regexp (second ie) (second e)) ;; s does not have a self-loop
                               (mk-concat-regexp (second ie)
                                                 (mk-concat-regexp
                                                  (make-unchecked-kleenestar (second self-loop-edge))
                                                  (second e)))))
                         (third e)))
                 ins))
          
          (define (merge-into-edge e outs)

            (define res (map (λ (eo)
                               (list (first e)
                                     (let* ((self-loop-lst (filter (λ (e) (and (eq? s (first e)) (eq? s (third e))))
                                                                   G))
                                            (self-loop-edge (if (null? self-loop-lst) '() (first self-loop-lst))))
                                       (if (null? self-loop-edge)
                                           (mk-concat-regexp (second e) (second eo)) ;; s does not have a self-loop
                                           (mk-concat-regexp (second e)
                                                             (mk-concat-regexp
                                                              (make-unchecked-kleenestar (second self-loop-edge))
                                                              (second eo)))))
                                     (third eo)))
                             outs))
            res)
          (let* ((mins (append-map (λ (e)
                                     (let* ((me (merge-into-edge e out-s-edges)))
                                       me))
                                   into-s-edges))
                 (mouts (append-map (λ (e)
                                      (let* ((me (merge-out-edge e into-s-edges)))
                                        me))
                                    out-s-edges)))
            (append mins mouts)))

        (define (remove-self-loops g) (filter (λ (e) (not (eq? (first e) (third e)))) g))
      
        (let* ((out-s-edges (remove-self-loops (filter (λ (e) (eq? (first e) s)) G)))
               (into-s-edges (remove-self-loops (filter (λ (e) (eq? (third e) s)) G))))
          (make-new-rem-edges s G into-s-edges out-s-edges)))
      (cond [(null? istates) G]
            [(or (eq? (first istates) strt) (eq? (first istates) fnl))
             (remove-internal-states G (rest istates) strt fnl)]
            [else (let* ((rems (first istates))
                         (removed-state-edges (remove-state rems))
                         (new-G (remove-duplicates
                                 (append removed-state-edges
                                         (filter (λ (e)
                                                   (and (not (eq? (first e) rems))
                                                        (not (eq? (third e) rems))))
                                                 G))))
                         (mme-G (merge-medges  new-G '())))
                    (remove-internal-states mme-G (rest istates) strt fnl))]))
  
    (let* ((rules (fsa-getrules m)) ;; edge representation of a graph
           (start (fsa-getstart m))
           (states (fsa-getstates m))
           (finals (fsa-getfinals m))
           (trules (map (λ (r)
                          (list (first r)
                                (if (eq? (second r) EMP)
                                    (empty-regexp)
                                    (make-unchecked-singleton (symbol->string (second r))))
                                (third r)))
                        rules)) ;; edges with singleton regexp
           (new-start (if (= (degree-in (fsa-getstart m) rules) 0)
                          start
                          (generate-symbol start states)))
           (new-final (if (and (= (length finals) 1)
                               (= (degree-out (first finals) rules) 0))
                          (first finals)
                          (generate-symbol 'F states)))
           (new-states (cond [(and (not (eq? start new-start))
                                   (not (eq? new-final (first finals))))
                              (cons new-start (cons new-final states))]
                             [(and (eq? start new-start) (eq? new-final (first finals)))
                              states]
                           
                             [(eq? start new-start) (cons new-final states)]
                             [else (cons new-start states)]))
           (new-finals (list new-final))
           (new-graph (cond [(and (not (eq? start new-start))
                                  (not (eq? (first finals) new-final))) ;; need new start and final states
                             (cons (list new-start (empty-regexp) start)
                                   (append (map (λ (fs) (list fs (empty-regexp) new-final))
                                                finals)
                                           trules))]
                            [(not (eq? start new-start)) ;; need new start state
                             (cons (list new-start (empty-regexp) start) trules)]
                            [(not (eq? (first finals) new-final)) ;; need new final state
                             (append (map (λ (fs) (list fs (empty-regexp) new-final))
                                          finals)
                                     trules)]
                            ;; new start and final states not needed
                            [else trules]))
           (merged-edges-graph (merge-medges new-graph '())) ;; graph with merged mutiple edges between any two nodes and new start and final state if necessary
           (final-graph
            (remove-internal-states merged-edges-graph
                                    new-states
                                    new-start
                                    new-final)))
    
      (printable-regexp (simplify-regexp (second (first final-graph))))))
  
  ; regexp --> fsa
  (define (regexp->fsa r)
    ; regexp --> alphabet
    (define (build-alphabet r)
      (cond [(empty-regexp? r) null]
            [(singleton-regexp? r) (list (string->symbol (singleton-regexp-a r)))]
            [(concat-regexp? r) 
             (let ((a1 (build-alphabet (concat-regexp-r1 r)))
                   (a2 (build-alphabet (concat-regexp-r2 r))))
               (append a1 a2))]
            [(union-regexp? r) 
             (let ((a1 (build-alphabet (union-regexp-r1 r)))
                   (a2 (build-alphabet (union-regexp-r2 r))))
               (append a1 a2))]
            [(kleenestar-regexp? r) 
             (let ((a1 (build-alphabet (kleenestar-regexp-r1 r))))
               a1)]))
    
    (define SIGMA (remove-duplicates (build-alphabet r)))
    
    ; regexp --> fsa
    ; ASSUMPTION: The given regexp is not a null-regexp
    (define (build-fsa r)
      
      ; --> fsa
      (define (make-empty-fsa)
        (make-unchecked-dfa '(q0) SIGMA 'q0 '(q0) '()))
      ; symbol --> fsa
      (define (make-singleton-fsa symb)
        (make-unchecked-dfa '(q0 q1) SIGMA 'q0 '(q1) (list (mk-fsarule 'q0 symb 'q1))))
      
      (cond [(empty-regexp? r) (make-empty-fsa)]
            [(singleton-regexp? r) (make-singleton-fsa
                                    (string->symbol (singleton-regexp-a r)))]
            [(concat-regexp? r) 
             (let ((m1 (build-fsa (concat-regexp-r1 r)))
                   (m2 (build-fsa (concat-regexp-r2 r))))
               (concat-fsa m1 m2))]
            [(union-regexp? r) 
             (let ((m1 (build-fsa (union-regexp-r1 r)))
                   (m2 (build-fsa (union-regexp-r2 r))))
               (union-fsa m1 m2))]
            [(kleenestar-regexp? r) 
             (let ((m1 (build-fsa (kleenestar-regexp-r1 r))))
               (kleenestar-fsa m1))]))
    (build-fsa (simplify-regexp r)))
  
  ;;; dfa --> rg
  (define (fsa->rg m)
    ; rule --> crule
    (define (rule->crule r) 
      (crule (from-state-fsarule r) (symb-fsarule r) (to-state-fsarule r)))
    
    ; rule --> srule
    (define (rule->srule r) (srule (from-state-fsarule r) (symb-fsarule r)))
    
    (let* ((dm (if (is-deterministic-fsa? m) m (ndfa->dfa m)))
           (V (map symbol-upcase (fsa-getstates dm)))
           (tfinals (map symbol-upcase (fsa-getfinals dm)))
           (trules (map   (lambda (r) (mk-fsarule (symbol-upcase (from-state-fsarule r))
                                                  (symb-fsarule r)
                                                  (symbol-upcase (to-state-fsarule r))))
                          (fsa-getrules dm)))
           (rls1 (map rule->crule trules))
           (rls2 (map rule->srule (filter (lambda (r) (member (symbol-upcase (to-state-fsarule r)) tfinals))
                                          trules)))
           (rls3 (if (member (fsa-getstart dm) (fsa-getfinals dm)) (list (erule (symbol-upcase (fsa-getstart dm)))) null)))
      (make-unchecked-rg V (fsa-getalphabet dm) (append rls1 rls2 rls3) (symbol-upcase (fsa-getstart dm)))))
  
  ; rg --> fsa
  (define (rg->fsa g)
    (define FS (generate-symbol 'Z (rg-nts g)))
    ; rrule --> fsarule
    (define (make-delta rr)
      (cond [(erule? rr) (mk-fsarule (symbol-upcase (rg-s g)) EMP FS)]
            [(srule? rr) (mk-fsarule (symbol-upcase (srule-lhs rr))
                                     (srule-rhs rr)
                                     FS)]
            [(crule? rr) (mk-fsarule (symbol-upcase (crule-lhs rr))
                                     (crule-rhs1 rr)
                                     (symbol-upcase (crule-rhs2 rr)))]))
    
    (let ((F (list FS))
          (K (cons FS (map symbol-upcase (rg-nts g))))
          (S (symbol-upcase (rg-s g)))
          (SIGMA (rg-sigma g))
          (DELTAS (map make-delta (rg-rules g))))
      (make-unchecked-ndfa K SIGMA S F DELTAS 'nodead))) ; ok not to add the dead state?
  
  (define (apply-fsa M w) (M w))
  
  (define (show-transitions-fsa M w) (M w 'transitions))
  
  (define (fsa-getrules M) (M null 'get-deltas))
  
  (define (fsa-getstates M) (M null 'get-states))
  
  (define (fsa-getstart M) (M null 'get-start))
  
  (define (fsa-getfinals M) (M null 'get-finals))
  
  (define (fsa-getalphabet M) (M null 'get-sigma))
  
  (define (is-deterministic-fsa? M) (M null 'is-deterministic?))
  
  
  ;;; TESTING
  
  ; fsa fsa word --> boolean
  (define (same-result-fsa? m1 m2 w) (eq? (apply-fsa m1 w) (apply-fsa m2 w)))
  
  ; fsa fsa [natnum] --> boolean or (listof word)
  (define (test-equiv-fsa m1 m2 . l)
    (define N (if (null? l) NUM-TESTS (car l)))
    
    ; natnum --> (listof (list word boolean))
    (define (tester n) 
      (let ((low (generate-words n (fsa-getalphabet m1) null)))
        (map (lambda (w) (list w (same-result-fsa? m1 m2 w))) low)))
    (if (not (equal? (fsa-getalphabet m1) (fsa-getalphabet m2)))
        (begin
          #f)
        (let ((res (tester N)))
          (if (andmap (lambda (p) (cadr p)) res)
              (begin
                #t)
              (begin
                (let ((failures (filter (lambda (p) (not (cadr p))) res))) 
                  (begin 
                    (map (lambda (p) (list (car p) 'failed)) failures))))))))
  
  
  ; fsa [natnum] --> (listof (list word symbol))
  (define (test-fsa m . l)
    (define number-tests (if (null? l) NUM-TESTS (car l)))
    (let ((test-words (generate-words number-tests (fsa-getalphabet m) null))) ;(build-list number-tests (lambda (i) (generate-word (fsa-getalphabet m))))))
      (map (lambda (w) (list w (apply-fsa m w))) test-words)))

  (define aUb (regexp->fsa (make-unchecked-union
                            (make-unchecked-singleton "a")
                            (make-unchecked-singleton "b"))))
                         
  )  ; closes module
