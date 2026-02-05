#lang racket/base
(provide sm-test-invs-pda)
;(provide (all-defined-out))
(require racket/list
         rackunit
         racket/set
         data/queue
         racket/sequence
         "../fsm-core/private/sm-getters.rkt"
         "../fsm-core/private/pda.rkt")


;; CONSTANT FOR LIMIT ON LENGTH OF A PATH
(define MAX-PATH-LENGTH 20) ;not in use rn


;; USEFUL FUNCTIONS

;; X loX -> Boolean
;; Purpose: To determine if the given item is a member of the given list
(define (member? x lox)
  (ormap (λ (y) (equal? x y)) lox))



;                                                                                                     
;                                                                                                     
;                                                                                                     
;                             ;                                                                       
;                             ;                                                                       
;  ;;;    ;;;                                              ;;;;;                                      
;   ;;    ;;                                                  ;;                                      
;   ; ;  ; ;     ;;;;       ;;;      ;; ;;;                  ;  ;      ;;  ;;      ;;;;       ;;;;    
;   ; ;  ; ;    ;    ;        ;       ;;   ;                 ;  ;       ;;;  ;    ;    ;     ;    ;   
;   ; ;  ; ;         ;        ;       ;    ;                 ;  ;       ;        ;      ;         ;   
;   ;  ;;  ;    ;;;;;;        ;       ;    ;                ;    ;      ;        ;;;;;;;;    ;;;;;;   
;   ;  ;;  ;   ;     ;        ;       ;    ;                ;;;;;;      ;        ;          ;     ;   
;   ;      ;   ;     ;        ;       ;    ;                ;    ;      ;        ;          ;     ;   
;   ;      ;   ;    ;;        ;       ;    ;               ;      ;     ;         ;     ;   ;    ;;   
;  ;;;    ;;;   ;;;; ;;    ;;;;;;;   ;;;  ;;;             ;;;    ;;;   ;;;;;;      ;;;;;     ;;;; ;;  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     

;; a PATH is a structure: (make-PATH (listof rule) (listof symbol)
(define-struct PATH (lor stack) #:transparent)

;; AUXILARY FUNCTIONS FOR PDA RULES
;;     pda rule: ((Source read pop) (Destination push))

;; pda-rule -> state
;; Purpose: To get the source state of a pda rule
(define (get-source-state rule)
  (first (first rule)))

;; pda-rule -> symbol
;; Purpose: To get the element read of a pda rule
(define (get-elem-read rule)
  (second (first rule)))

;; pda-rule -> (listof symbol) or EMP
;; Purpose: To get the elements to pop from the top of the stack
(define (get-pop rule)
  (third (first rule)))

;; pda-rule -> state
;; Purpose: To get the destination state of the pda rule
(define (get-destination-state rule)
  (first (second rule)))

;; pda-rule -> (listof symbol) or EMP
;; Purpose: To get the elements to push to the stack
(define (get-push rule)
  (second (second rule)))






;; (listof (list state (word -> boolean))) (listof symbol) (listof symbol) -> Boolean
;; Purpose: Determine if the given invariant holds 
(define (invariant-holds? a-loi a-word a-stack)
  (or (empty? a-loi)   
      ((second (first a-loi)) a-word a-stack)))

;; pda-rule (listof pda-rule) -> (listof pda-rule)
;; Purpose: To return the next pda-rules that can be used from the given pda-rules
(define (get-next-rules a-rule rules)
  (filter (λ (rule) (eq? (get-destination-state a-rule) (get-source-state rule))) rules))



;; PATH (listof pda-rule) -> (listof (listof PATH) (listof (listof state word stack)))
;; Purpose: To return a list of paths that comes from the given path and rules
(define (new-paths&visited a-path rules visited max-length)
  ;; (listof pda-rule) (listof PATH) -> (listof (listof PATH) (listof (listof state word stack)))
  ;; Purpose: To return a list of paths that come from the given paths and rules
  ;; Accumulator invariant: accum = list of paths
  (define (new-paths-helper next-rules accum new-visited)
    (cond [(empty? next-rules) (list accum new-visited)]
          [(or (set-member? new-visited
                            (list (get-destination-state (first next-rules))
                                  (word-of-path (make-PATH (append (PATH-lor a-path) (list (first next-rules)))
                                                           '())) ;; doesn't matter what the stack is for this
                                  (append (if (eq? 'ε (get-push (first next-rules)))
                                              '()
                                              (get-push (first next-rules)))
                                          (drop (PATH-stack a-path)
                                                (length (if (eq? 'ε (get-pop (first next-rules)))     ;; makes sure that we're not at the 
                                                            '()                                       ;; same state with the same word and
                                                            (get-pop (first next-rules))))))))        ;; stack again (no double doing work),
               (< max-length                                                                          ;; making sure the new word and stack at 
                  (length (append (PATH-lor a-path) (list (first next-rules))))))  ;<- caps # of paths     ;; the state has not been visited already
           (new-paths-helper (rest next-rules) accum new-visited)]
          [else (let* [(new-path-rules (append (PATH-lor a-path)
                                               (list (first next-rules))))
                       (new-path-stack (append (if (eq? 'ε (get-push (first next-rules)))
                                                   '()
                                                   (get-push (first next-rules)))
                                               (drop (PATH-stack a-path)
                                                     (length (if (eq? 'ε (get-pop (first next-rules)))
                                                                 '()
                                                                 (get-pop (first next-rules)))))))
                       (new-path (make-PATH new-path-rules
                                            new-path-stack))
                       (destination-state-last-rule (get-destination-state (last new-path-rules)))
                       (word-of-new-path (word-of-path new-path))
                       (stack-of-new-path (PATH-stack new-path))]
                  (new-paths-helper (rest next-rules)
                                    (cons new-path
                                          accum)
                                    (set-add new-visited
                                             (list destination-state-last-rule
                                                   word-of-new-path
                                                   stack-of-new-path))))]
          ))
  (new-paths-helper rules '() visited))




;; CONSTANT FOR MAX NUMBER OF REPETITIONS OF A RULE IN A PATH
(define MAX-NUM-REPETITIONS 3)



;; pda -> (listof PATH)
;; Purpose: To return all the paths in the given pda 
(define (find-paths a-machine #:max-length [max-length 12])
  (define queue (make-queue))
  (define rules (sm-rules a-machine))
  ;; (queueof (listof PATH)) (listof PATH) -> (listof PATH)
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invariant: paths = list of current paths
  ;;                        visited = set of a state, word, and stack that has
  ;;                                  been visited
  (define (find-paths-helper paths visited)
    (if (queue-empty? queue) paths
        (let* [;(hello (displayln "queue:    "))
               ;(hi (displayln (sequence->list (in-queue queue))))
               (qfirst (dequeue! queue))
              (next-rules-first-path (get-next-rules (last (PATH-lor qfirst))
                                                     (filter
                                                      (λ (rule)
                                                        (and (or (equal? (get-pop rule) 'ε)
                                                                 (and (<= (length (get-pop rule)) (length (PATH-stack qfirst)))
                                                                      (equal? (take (PATH-stack qfirst)
                                                                                    (length (get-pop rule)))   ;; this part makes sure that we the rules are able to be applied bc 
                                                                              (get-pop rule))))                    ;; can't pop elems off stack if aren't there
                                                             #;(< (count (λ (rl) (equal? rule rl))
                                                                         (PATH-lor qfirst))
                                                                  MAX-NUM-REPETITIONS)))
                                                      rules)))
              (paths-with-qfirst (cons qfirst paths))] ;; <-- the paths with the first of the queue included
          (if (empty? next-rules-first-path)
              (find-paths-helper paths-with-qfirst
                                 visited)
              (begin ;(displayln qfirst)
                      ;(displayln "new paths:   ")
                      ;(displayln (first (new-paths&visited qfirst next-rules-first-path visited max-length)))

                      
                      ;(enqueue! queue (first (new-paths&visited qfirst next-rules-first-path visited max-length))) ;; this has to be fixed, needs to map enqueue i think, enqueue the dequeue?
                      (map (λ (x) (enqueue! queue x)) (first (new-paths&visited qfirst next-rules-first-path visited max-length)))

                      ;(displayln "updated paths: ")
                      ;(displayln paths-with-qfirst)
                      
                      
                      (find-paths-helper paths-with-qfirst
                                         (second (new-paths&visited qfirst next-rules-first-path visited max-length))))
          ))))
  (begin #;(enqueue! queue (map (λ (y) (make-PATH y (if (eq? 'ε (get-push (first y)))
                                                           '()
                                                           (get-push (first y)))))     ;; have to fix this part, when enqueing, maybe map it onto each element?
                                   (map (λ (x) (list x))
                                        (filter
                                         (λ (rule) (eq? (get-source-state rule) (sm-start a-machine)))
                                         rules))))
          (map (λ (x) (enqueue! queue x)) (map (λ (y) (make-PATH y (if (eq? 'ε (get-push (first y)))
                                                                       '()
                                                                       (get-push (first y)))))     ;; have to fix this part, when enqueing, maybe map it onto each element?
                                               (map (λ (x) (list x))
                                                    (filter
                                                     (λ (rule) (eq? (get-source-state rule) (sm-start a-machine)))
                                                     rules))))
          (find-paths-helper '() (set)) ))




;; pda -> (listof PATH)
;; Purpose: Returns all the paths that lead to an accepting word of the given machine
(define (get-accepting-paths a-pda #:max-length [max-length 12])
  (define paths-that-end-in-finals (filter (λ (x) (member? (get-destination-state (last (PATH-lor x))) (sm-finals a-pda)))
                                           (find-paths a-pda #:max-length max-length)))
  ;; PATH -> Boolean
  ;; Purpose: To determine if the given path leads to an accept 
  (define (leads-to-accepting? a-path)
    (empty? (PATH-stack a-path)))

  ;; PATH -> (listof PATH)
  ;; Purpose: To return all the sub paths of the given
  ;;          path, including the given path
  (define (get-sub-paths a-path)
    ;; (listof pda-rules) -> (listof symbol)
    ;; Purpose: To get the stack of the given path
    (define (get-stack path-lor)
      ;; (listof pda-rules) (listof symbol) -> (listof symbol)
      ;; Purpose: To get the stack of the given path
      (define (get-stack-helper rules-left cur-stack)
        (if (empty? rules-left)
            cur-stack
            (get-stack-helper (rest rules-left)
                              (append (if (eq? 'ε (get-push (first rules-left)))
                                          '()
                                          (get-push (first rules-left)))
                                      (drop cur-stack
                                            (length (if (eq? 'ε (get-pop (first rules-left)))
                                                        '()
                                                        (get-pop (first rules-left)))))))))
      (get-stack-helper path-lor '()))
                    
    ;; number (listof PATH) -> (setof PATH)
    ;; Purpose: To return all the sub paths of the given
    ;;          path, including the given path
    (define (get-sub-paths-helper length-cur-path cur-set)
      (if (equal? (take (PATH-lor a-path) length-cur-path) (PATH-lor a-path))
          (set-add cur-set (make-PATH (take (PATH-lor a-path) length-cur-path)
                                      (get-stack (take (PATH-lor a-path) length-cur-path))))
          (get-sub-paths-helper  (+ 1 length-cur-path)
                                 (set-add cur-set (make-PATH (take (PATH-lor a-path) length-cur-path)
                                                             (get-stack (take (PATH-lor a-path) length-cur-path)))))))
    (get-sub-paths-helper 1 (set)))
  
  (set->list (apply set-union (map get-sub-paths
                                   (filter leads-to-accepting? paths-that-end-in-finals)))))


;; DONT FORGET TO FIX THE DOCUMENTATION



;; pda -> pda
;; Purpose: Takes in pda and remove states and rules that can't reach a final state 
(define (remove-states-that-cannot-reach-finals a-pda #:max-length [max-length 12])
  (define paths-that-end-in-finals (filter (λ (x) (member? (get-destination-state (last (PATH-lor x))) (sm-finals a-pda)))
                                           (find-paths a-pda #:max-length max-length)))
  (define new-rules (remove-duplicates (apply append (map (λ (x) (PATH-lor x)) paths-that-end-in-finals))))
  (define new-states (remove-duplicates (append-map (λ (x) (list (get-source-state x) (get-destination-state x))) new-rules)))
  (make-unchecked-ndpda new-states (sm-sigma a-pda) (sm-gamma a-pda) (sm-start a-pda) (sm-finals a-pda) new-rules))


;; (listof pda-rule) -> word
;; Purpose: To return a word that is made from the given path
(define (word-of-path a-path)
  (filter (λ (x) (not (eq? 'ε x)))
          (append-map (λ (x) (list (get-elem-read x))) (PATH-lor a-path))))


;; PATH (listof (list state (word -> boolean))) -> Boolean
;; Purpose: To determine if a the invariant for a given path holds
(define (path-inv-not-hold? a-path a-loi)
  (not (invariant-holds? a-loi
                         (word-of-path a-path)
                         (PATH-stack a-path))))



;; machine (listof (list state (word -> boolean))) -> (listof (listof state (listof word)))
;; Purpose: To return a list of all posible words that can be at each state in a machine 
(define (sm-all-possible-words a-machine #:max-path-length [max-path-length 12])
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (remove-states-that-cannot-reach-finals a-machine #:max-length max-path-length))
 
  ;; list of the paths that lead to accept of the refactored pda 
  (define all-paths-new-machine (get-accepting-paths new-machine #:max-length max-path-length))
  

  ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose:  To return a list of all posible words that can be at each state in a machine 
  ;; Accumulator Invarient: accum = list of lists of words with the states that the can possibly be at
  #;(define (sm-all-possible-words-helper all-paths accum)
    (if (empty? all-paths)
        accum
        (sm-all-possible-words-helper (rest all-paths)
                                      (cons (list (word-of-path (first all-paths))              ;; ⚙️this returns a list of 
                                                  (third (last (first all-paths)))) accum))))   ;;      a list of words and the
                                                                                                ;;      state that the word ends at




  (define (sm-all-possible-words-helper all-paths accum)
      (if (empty? all-paths)
          accum
          (sm-all-possible-words-helper (rest all-paths)
                                        (cons (list (word-of-path (first all-paths))              
                                                    (PATH-stack (first all-paths))
                                                    (get-destination-state (last (PATH-lor (first all-paths))))) accum))))      ;<-- pda version, now there's also the stack
                                                                                                         ;; returns list of word path state
  
  
  ;; (listof symbol) -> (listof (symbol (listof word)))
  ;; Purpose: To generate a list of lists of a state and an empty list for each given states
  (define (build-list-of-states-&-empty-low states)
    ;; (listof symbol) (listof word)-> (listof (symbol '()))
    ;; Purpose: To generate a list of lists of a state and an empty list for each given states
    ;; Accumulator Invariant: accum = list containing a list with the state and an empty list of words
    (define (build-list-of-states-&-empty-low-helper states accum)
      (if (empty? states) accum
          (build-list-of-states-&-empty-low-helper (rest states) (cons (list (first states) '()) accum))))  ;; ⚙️this is what builds the main list
    (build-list-of-states-&-empty-low-helper states '()))                                                   ;;      with the states and empty list of words
          
  (define states-&-empty-low (build-list-of-states-&-empty-low (sm-states new-machine)))

  ;; (listof (listof word state)) -> (listof (listof state (listof word)))
  ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
  (define (sort-words listof-all-words-&-states)
    ;; (listof (word state)) -> (listof word)
    ;; Purpose: To make the given (listof (word state)) into a list of words
    (define (make-low list-of-words-&-states)
      ;; (listof (word state)) -> (listof word)
      ;; Purpose: To make the given (listof (word state)) into a list of words
      ;; Accumulator Invariant: accum = current list of words for the state
      (define (make-low-helper list-of-words-&-states accum)
        (if (empty? list-of-words-&-states) accum
            (make-low-helper (rest list-of-words-&-states) (cons (list (first (first list-of-words-&-states))
                                                                       (second (first list-of-words-&-states))) accum))))  ;; ⚙️turning the list of words, stack and state into a list of the words
      (make-low-helper list-of-words-&-states '()))                                                                       ;; the given list is a list of configs that go to a state
    ;; (listof (listof word state)) -> (listof (listof state (listof word)))
    ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
    ;; Accumulator Invairant: accum = list of states with all the possible words in them 
    (define (sort-words-helper states-&-empty-low accum)
      (if (empty? states-&-empty-low) accum
          (sort-words-helper (rest states-&-empty-low)                                 ;; ⚙️accumulates the list of words and states sorted
                             (cons (list (first (first states-&-empty-low))
                                         (make-low (filter (λ (x) (eq? (first (first states-&-empty-low)) (third x)))
                                                           listof-all-words-&-states))) accum))
          ))
            
    (sort-words-helper states-&-empty-low '()))
  (sort-words (sm-all-possible-words-helper all-paths-new-machine
                                            (list (list '() '() (sm-start a-machine))))))






;; pda (listof (list state (word -> boolean))) -> (listof (word stack state))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs-pda a-machine max-path-length a-loi)
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (remove-states-that-cannot-reach-finals a-machine #:max-length max-path-length))
  ;; list of invariants that are reachable from the starting configuration
  (define reachable-inv (filter (λ (x) (member? (first x) (sm-states new-machine))) a-loi))
  ;; all accepting paths of new-machine
  (define all-paths-new-machine (get-accepting-paths new-machine #:max-length max-path-length))

  ;; (listof PATH) (listof (word stack state)) -> (listof (word stack state))
  ;; Purpose: To return a list of the invariants and the word that causes them not to hold
  ;; Accumulator Invariant: accum = list of lists of words and stacks that cause the invarient not to hold
  ;;                                & the state that it doesn't hold for
  (define (sm-test-invs-helper all-paths accum)
    (if (empty? all-paths)
        accum
        (sm-test-invs-helper (rest all-paths)
                             (if (path-inv-not-hold? (first all-paths)
                                                     (filter
                                                      (λ (x) (eq? (get-destination-state (last (PATH-lor (first all-paths))))
                                                                  (first x)))
                                                      reachable-inv))
                                 (cons (list (word-of-path (first all-paths))
                                             (PATH-stack (first all-paths))
                                             (get-destination-state (last (PATH-lor (first all-paths))))) accum)
                                 accum))))
          
  (sm-test-invs-helper all-paths-new-machine
                       (let [(start-pair (assoc (sm-start a-machine) a-loi))]
                         (if (or (not (pair? start-pair))
                                 ((second start-pair) '() '()))
                             '()
                             (list (list '() '() (sm-start a-machine)))))))


