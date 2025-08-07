#lang fsm

(provide sm-all-possible-words sm-test-invs)

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





(struct rule (source element-read destination) #:transparent)

(define (rule->struct a-rule)
  (rule (first a-rule) (second a-rule) (third a-rule)))


;; (listof (list state (word -> boolean))) (listof symbol) -> Boolean
;; Purpose: Determine if the given invariant holds 
(define (invariant-holds? a-loi a-config)
  (or (empty? a-loi)
      ((second (first a-loi)) (first a-config))))



;; rule (listof rule) -> (listof rule)
;; Purpose: To return the next rules that can be used from the given rule
(define (get-next-rules a-rule rules)
  (filter (λ (rule) (eq? (third a-rule) (first rule))) rules))

;; (listof rule) (listof rule) -> (listof (listof rule))
;; Purpose: To return a list of paths that comes from the given path and rules
(define (new-paths a-path rules)
  (local [;; (listof rule) (listof (listof rule)) -> (listof (listofrule))
          ;; Purpose: To return a list of paths that come from the given paths and rules
          ;; Accumulator invariant: accum = list of paths
          (define (new-paths-helper next-rules accum)
            (if (empty? next-rules) accum
                (new-paths-helper (rest next-rules)
                                  (append (list (append a-path (list (first next-rules)))) accum))))]
    (new-paths-helper rules '())))


;; machine -> (listof rules)
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine)
  (local[(define rules (sm-rules a-machine))
         ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
         ;; Purpose: To return all the paths of the given machine
         ;; Accumulator invarient: paths = list of current paths
         (define (find-paths-helper a-qop paths)
           (if (qempty? a-qop) paths
               (local [(define next-rules-first-path (get-next-rules (last (qfirst a-qop))
                                                                     (filter
                                                                      (λ (rule)
                                                                        (< (count (λ (rl) (equal? rule rl))
                                                                                  (qfirst a-qop))
                                                                           2)
                                                                        #;(not (member? x (qfirst a-qop)))
                                                                        )
                                                                      rules)))
                       (define paths-with-qfirst (cons (qfirst a-qop) paths))]
                 (if (empty? next-rules-first-path)
                     (find-paths-helper (dequeue a-qop)
                                        paths-with-qfirst)
                     (find-paths-helper (enqueue (new-paths (qfirst a-qop) next-rules-first-path)
                                                 (dequeue a-qop))
                                        paths-with-qfirst))
                 )))]
    (find-paths-helper (enqueue (map (λ (x) (list x))
                                     (filter
                                      (λ (rule) (eq? (first rule) (sm-start a-machine)))
                                      (sm-rules a-machine)))
                                '())
                       '())))



;; ndfa -> ndfa
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
(define (remove-states-that-cannot-reach-finals a-ndfa)
  (local [(define paths-that-end-in-finals (filter (λ (x) (member? (third (last x)) (sm-finals a-ndfa)))
                                                   (find-paths a-ndfa)))
          (define new-rules (remove-duplicates (apply append paths-that-end-in-finals)))
          (define new-states (remove-duplicates (append-map (λ (x) (list (first x) (third x))) new-rules)))]
    (make-ndfa new-states (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules)))


;; (listof rules) -> word
;; Purpose: To return a word that is made from the given list of rules
(define (word-of-path a-lor)
  (filter (λ (x) (not (eq? 'ε x))) (append-map (λ (x) (list (second x))) a-lor)))


;; (listof (listof symbol)) (listof (list state (word -> boolean))) -> Boolean
;; Purpose: To determine if a the invariant for a given path holds
(define (path-inv-not-hold? a-path a-loi)
  (not (invariant-holds? a-loi (list (word-of-path a-path) (third (last a-path))))))

;; machine (listof (list state (word -> boolean))) -> (listof (listof state (listof word)))
;; Purpose: To return a list of all posible words that can be at each state in a machine 
(define (sm-all-possible-words a-machine a-loi)
  (local [;; the given machine without the states and rules of states that cannot reach a final state
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
            (local [;; (listof symbol) (listof word)-> (listof (symbol '()))
                    ;; Purpose: To generate a list of lists of a state and an empty list for each given states
                    ;; Accumulator Invariant: accum = list containing a list with the state and an empty list of words
                    (define (build-list-of-states-&-empty-low-helper states accum)
                      (if (empty? states) accum
                          (build-list-of-states-&-empty-low-helper (rest states) (cons (list (first states) '()) accum))))]
              (build-list-of-states-&-empty-low-helper states '())))
          
          (define states-&-empty-low (build-list-of-states-&-empty-low (sm-states new-machine)))

          ;; (listof (listof word state)) -> (listof (listof state (listof word)))
          ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
          (define (sort-words listof-all-words-&-states)
            (local [;; (listof (word state)) -> (listof word)
                    ;; Purpose: To make the given (listof (word state)) into a list of words
                    (define (make-low list-of-words-&-states)
                      (local [;; (listof (word state)) -> (listof word)
                              ;; Purpose: To make the given (listof (word state)) into a list of words
                              ;; Accumulator Invariant: accum = current list of words for the state
                              (define (make-low-helper list-of-words-&-states accum)
                                (if (empty? list-of-words-&-states) accum
                                    (make-low-helper (rest list-of-words-&-states) (cons (first (first list-of-words-&-states)) accum))))]
                        (make-low-helper list-of-words-&-states '())))
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
                    ]
              (sort-words-helper states-&-empty-low '())))]
    (sort-words (sm-all-possible-words-helper all-paths-new-machine
                                              (list (list '() (sm-start a-machine)))))))

;; machine (listof (list state (word -> boolean))) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs a-machine . a-loi)
  (local [;; the given machine without the states and rules of states that cannot reach a final state
          (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                                  a-machine
                                  (remove-states-that-cannot-reach-finals a-machine)))
          ;; list of invariants that are reachable from the starting configuration
          (define reachable-inv (filter (λ (x) (member? (first x) (sm-states new-machine))) a-loi))
          ;; all paths of new-machine
          (define all-paths-new-machine (find-paths new-machine))

          ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
          ;; Purpose: To return a list of the invarients and the word that causes them not to hold
          ;; Accumulator Invarient: accum = list of lists of words that cause the invarient not to hold
          ;;                                & the state that it doesn't hold for
          (define (sm-test-invs-helper all-paths accum)
            (if (empty? all-paths)
                accum
                (sm-test-invs-helper (rest all-paths)
                                     (if (path-inv-not-hold? (first all-paths)
                                                             (filter
                                                              (λ (x) (eq? (third (last (first all-paths)))
                                                                          (first x)))
                                                              reachable-inv))
                                         (cons (list (word-of-path (first all-paths))
                                                     (third (last (first all-paths)))) accum)
                                         accum))))
          ]
    (sm-test-invs-helper all-paths-new-machine
                         (let [(start-pair (assoc (sm-start a-machine) a-loi))]
                         (if (or
                              (not (pair? start-pair))
                              ((second start-pair) '()))
                             '()
                             (list (list '() (sm-start a-machine))))))))



