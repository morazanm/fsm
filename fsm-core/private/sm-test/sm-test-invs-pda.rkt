#lang racket/base

;(provide sm-all-possible-words sm-test-invs-pda)
(provide (all-defined-out))

(require racket/list
         rackunit
         racket/set
         "../sm-getters.rkt"
         "../pda.rkt")

;; USEFUL FUNCTIONS

;; word word -> Boolean
;; Purpose: Determine if the second given word appears in
;;          the first given word
(define (contains? w pattern)
  (cond [(< (length w) (length pattern)) #f]
        [(equal? (take w (length pattern)) pattern) #t]
        [else (contains? (rest w) pattern)]))


;; X loX -> Boolean
;; Purpose: To determine if the given item is a member of the given list
(define (member? x lox)
  (ormap (λ (y) (equal? x y)) lox))

;; A queue of X, (qof X), is eithe(r:
;;  1. empty
;;  2. (cons X (qof X))


(define E-QUEUE '())

(define qempty? empty?)

;; Tests for qempty?
(check-equal? (qempty? '())      #true)
(check-equal? (qempty? '(a b c)) #false)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (append a-qox a-lox))

;; Tests for enqueue
(check-equal? (enqueue '(8 d) '()) '(8 d))
(check-equal? (enqueue '(d) '(a b c)) '(a b c d))
(check-equal? (enqueue '(6 5 4) '(7)) '(7 6 5 4))

;; (qof X) --> X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; Tests for qfirst
;(check-error  (qfirst '()) "qfirst applied to an empty queue")
(check-equal? (qfirst '(a b c)) 'a)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))

;; Tests for qfirst
;(check-error  (dequeue '()) "dequeue applied to an empty queue")
(check-equal? (dequeue '(a b c)) '(b c))



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



;; CONSTANT FOR LIMIT ON LENGTH OF A PATH
(define MAX-PATH-LENGTH 12)


;; PATH (listof pda-rule) -> (listof (listof PATH) (listof (listof state word stack)))
;; Purpose: To return a list of paths that comes from the given path and rules
(define (new-paths&visited a-path rules visited)
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
                                                (length (if (eq? 'ε (get-pop (first next-rules)))
                                                            '()
                                                            (get-pop (first next-rules))))))))
               (< MAX-PATH-LENGTH
                  (length (append (PATH-lor a-path) (list (first next-rules))))))
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
(define (find-paths a-machine)
  (define rules (sm-rules a-machine))
  ;; (queueof (listof PATH)) (listof PATH) -> (listof PATH)
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invariant: paths = list of current paths
  ;;                        visited = set of a state, word, and stack that has
  ;;                                  been visited
  (define (find-paths-helper a-qop paths visited)
    (if (qempty? a-qop) paths
        (let [(next-rules-first-path (get-next-rules (last (PATH-lor (qfirst a-qop)))
                                                     (filter
                                                      (λ (rule)
                                                        (and (or (equal? (get-pop rule) 'ε)
                                                                 (and (<= (length (get-pop rule)) (length (PATH-stack (qfirst a-qop))))
                                                                      (equal? (take (PATH-stack (qfirst a-qop))
                                                                                    (length (get-pop rule)))
                                                                              (get-pop rule))))
                                                             #;(< (count (λ (rl) (equal? rule rl))
                                                                         (PATH-lor (qfirst a-qop)))
                                                                  MAX-NUM-REPETITIONS)))
                                                      rules)))
              (paths-with-qfirst (cons (qfirst a-qop) paths))]
          (if (empty? next-rules-first-path)
              (find-paths-helper (dequeue a-qop)
                                 paths-with-qfirst
                                 visited)
              (find-paths-helper (enqueue (first (new-paths&visited (qfirst a-qop) next-rules-first-path visited))
                                          (dequeue a-qop)) 
                                 paths-with-qfirst
                                 (second (new-paths&visited (qfirst a-qop) next-rules-first-path visited))))
          )))
  (find-paths-helper (enqueue (map (λ (y) (make-PATH y (if (eq? 'ε (get-push (first y)))
                                                           '()
                                                           (list (get-push (first y))))))
                                   (map (λ (x) (list x))
                                        (filter
                                         (λ (rule) (eq? (get-source-state rule) (sm-start a-machine)))
                                         rules)))
                              '())
                     '()
                     (set)))




;; pda -> (listof PATH)
;; Purpose: Returns all the paths that lead to an accepting word of the given machine
(define (get-accepting-paths a-pda)
  (define paths-that-end-in-finals (filter (λ (x) (member? (get-destination-state (last (PATH-lor x))) (sm-finals a-pda)))
                                           (find-paths a-pda)))
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
(define (remove-states-that-cannot-reach-finals a-pda)
  (define paths-that-end-in-finals (filter (λ (x) (member? (get-destination-state (last (PATH-lor x))) (sm-finals a-pda)))
                                           (find-paths a-pda)))
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
(define (sm-all-possible-words a-machine a-loi)
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                          a-machine
                          (remove-states-that-cannot-reach-finals a-machine)))
  ;; list of invariants that are reachable from the starting configuration
  (define reachable-inv (filter (λ (x) (member? (first x) (sm-states new-machine))) a-loi))
  ;; all paths of new-machine
  (define all-paths-new-machine (find-paths new-machine))

  ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose:  To return a list of all posible words that can be at each state in a machine 
  ;; Accumulator Invarient: accum = list of lists of words with the states that the can possibly be at
  (define (sm-all-possible-words-helper all-paths accum)
    (if (empty? all-paths)
        accum
        (sm-all-possible-words-helper (rest all-paths)
                                      (cons (list (word-of-path (first all-paths))
                                                  (third (last (first all-paths)))) accum))))

  ;; (listof symbol) -> (listof (symbol (listof word)))
  ;; Purpose: To generate a list of lists of a state and an empty list for each given states
  (define (build-list-of-states-&-empty-low states)
    ;; (listof symbol) (listof word)-> (listof (symbol '()))
    ;; Purpose: To generate a list of lists of a state and an empty list for each given states
    ;; Accumulator Invariant: accum = list containing a list with the state and an empty list of words
    (define (build-list-of-states-&-empty-low-helper states accum)
      (if (empty? states) accum
          (build-list-of-states-&-empty-low-helper (rest states) (cons (list (first states) '()) accum))))
    (build-list-of-states-&-empty-low-helper states '()))
          
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
            (make-low-helper (rest list-of-words-&-states) (cons (first (first list-of-words-&-states)) accum))))
      (make-low-helper list-of-words-&-states '()))
    ;; (listof (listof word state)) -> (listof (listof state (listof word)))
    ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
    ;; Accumulator Invairant: accum = list of states with all the possible words in them 
    (define (sort-words-helper states-&-empty-low accum)
      (if (empty? states-&-empty-low) accum
          (sort-words-helper (rest states-&-empty-low)
                             (cons (list (first (first states-&-empty-low))
                                         (make-low (filter (λ (x) (eq? (first (first states-&-empty-low)) (second x)))
                                                           listof-all-words-&-states))) accum))
          ))
            
    (sort-words-helper states-&-empty-low '()))
  (sort-words (sm-all-possible-words-helper all-paths-new-machine
                                            (list (list '() (sm-start a-machine))))))

;; pda (listof (list state (word -> boolean))) -> (listof (word stack state))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs-pda a-machine . a-loi)
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (remove-states-that-cannot-reach-finals a-machine))
  ;; list of invariants that are reachable from the starting configuration
  (define reachable-inv (filter (λ (x) (member? (first x) (sm-states new-machine))) a-loi))
  ;; all accepting paths of new-machine
  (define all-paths-new-machine (get-accepting-paths new-machine))

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


