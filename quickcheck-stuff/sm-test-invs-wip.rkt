#lang racket/base

(provide sm-all-possible-words remove-states-that-cannot-reach-finals sm-test-invs)
(require "../fsm-core/private/sm-getters.rkt"
         "../fsm-core/private/fsa.rkt"
         racket/list)

;; USEFUL FUNCTIONS

;; X loX -> Boolean
;; Purpose: To determine if the given item is a member of the given list
(define (member? x lox)
  (ormap (λ (y) (equal? x y)) lox))

;; A queue of X, (qof X), is eithe(r:
;;  1. empty
;;  2. (cons X (qof X))


(define E-QUEUE '())

(define qempty? null?)



;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (append a-qox a-lox))


;; (qof X) --> X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (car a-qox)))


;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (cdr a-qox)))



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





;; (listof (list state (word -> boolean))) (listof symbol) -> Boolean
;; Purpose: Determine if the given invariant holds 
(define (invariant-holds? a-loi a-config)
  (or (null? a-loi)
      ((second (car a-loi)) (car a-config))))



;; rule (listof rule) -> (listof rule)
;; Purpose: To return the next rules that can be used from the given rule
(define (get-next-rules a-rule rules)
  (filter (λ (rule) (eq? (third a-rule) (car rule))) rules))

;; (listof rule) (listof rule) -> (listof (listof rule))
;; Purpose: To return a list of paths that comes from the given path and rules
(define (new-paths a-path rules)
  ;; (listof rule) (listof (listof rule)) -> (listof (listofrule))
  ;; Purpose: To return a list of paths that come from the given paths and rules
  ;; Accumulator invariant: accum = list of paths
  (define (new-paths-helper next-rules accum)
    (if (null? next-rules) accum
        (new-paths-helper (cdr next-rules)
                          (append (list (append a-path (list (car next-rules)))) accum))))
  
  (new-paths-helper rules '()))


;; machine -> (listof rules)
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine)
  (define rules (sm-rules a-machine))
  
  ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invarient: paths = list of current paths
  (define (find-paths-helper a-qop paths)
    (if (qempty? a-qop) paths
        (let [(next-rules-first-path (get-next-rules (last (qfirst a-qop))
                                                     (filter
                                                      (λ (rule)
                                                        (< (count (λ (rl) (equal? rule rl))
                                                                  (qfirst a-qop))
                                                           2)
                                                        #;(not (member? x (qfirst a-qop)))
                                                        )
                                                      rules)))
              (paths-with-qfirst (cons (qfirst a-qop) paths))]
          (if (null? next-rules-first-path)
              (find-paths-helper (dequeue a-qop)
                                 paths-with-qfirst)
              (find-paths-helper (enqueue (new-paths (qfirst a-qop) next-rules-first-path)
                                          (dequeue a-qop))
                                 paths-with-qfirst))
          )))
  (find-paths-helper (enqueue (map (λ (x) (list x))
                                   (filter
                                    (λ (rule) (eq? (car rule) (sm-start a-machine)))
                                    (sm-rules a-machine)))
                              '())
                     '()))

(struct ndfa (st sig s f r) #:transparent)

;; ndfa -> ndfa
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
(define (remove-states-that-cannot-reach-finals a-ndfa)
  ;; machine -> (listof rules)
  ;; Purpose: To return all the paths in the given machine 
  (define (explore-all-paths a-machine)
    (define rules (sm-rules a-machine)) 
    ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
    ;; Purpose: To return all the paths of the given machine
    ;; Accumulator invariant: paths = list of current paths
    (define (explore-all-paths-helper a-qop paths)
      (if (qempty? a-qop)
          paths
          (let* [(firstofq (qfirst a-qop))
                 (next-rules-first-path (filter-not (λ (rule) (member? rule firstofq))
                                                    (get-next-rules (last firstofq) rules)))
                 (paths-with-qfirst (cons firstofq paths))]
            (if (null? next-rules-first-path)
                (explore-all-paths-helper (dequeue a-qop) paths-with-qfirst)
                (explore-all-paths-helper (enqueue (new-paths firstofq next-rules-first-path)
                                                   (dequeue a-qop))
                                   paths-with-qfirst)))))
    (explore-all-paths-helper (enqueue (map (λ (x) (list x))
                                     (filter
                                      (λ (rule) (eq? (car rule) (sm-start a-machine)))
                                      rules))
                                '())
                       '()))
  (define paths-that-end-in-finals (filter (λ (x) (member? (third (last x)) (sm-finals a-ndfa)))
                                           (explore-all-paths a-ndfa)))
  
  (define new-states (remove-duplicates (append-map (λ (x) (list (car x) (third x)))
                                                    (remove-duplicates (apply append paths-that-end-in-finals)))))
  (define new-rules (filter (λ (rule) (and (member? (first rule) new-states)
                                           (member? (third rule) new-states)))
                            (sm-rules a-ndfa)))
  (make-unchecked-ndfa new-states (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules)
  #;(values
   ;(length paths-that-end-in-finals)
   ;(length paths-that-end-in-finals2)
   (ndfa #;make-unchecked-ndfa new-states (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules)
   (ndfa #;make-unchecked-ndfa new-states2 (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules2)
   (test-equiv-fsa (make-unchecked-ndfa new-states (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules)
                   (make-unchecked-ndfa new-states2 (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) new-rules2))))

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
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                          a-machine
                          (remove-states-that-cannot-reach-finals a-machine)))
  ;; list of invariants that are reachable from the starting configuration
  (define reachable-inv (filter (λ (x) (member? (car x) (sm-states new-machine))) a-loi))
  ;; all paths of new-machine
  (define all-paths-new-machine (find-paths new-machine))

  ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose:  To return a list of all posible words that can be at each state in a machine 
  ;; Accumulator Invarient: accum = list of lists of words with the states that the can possibly be at
  (define (sm-all-possible-words-helper all-paths accum)
    (if (null? all-paths)
        accum
        (sm-all-possible-words-helper (cdr all-paths)
                                      (cons (list (word-of-path (car all-paths))
                                                  (third (last (car all-paths)))) accum))))

  ;; (listof symbol) -> (listof (symbol (listof word)))
  ;; Purpose: To generate a list of lists of a state and an empty list for each given states
  (define (build-list-of-states-&-empty-low states)
    ;; (listof symbol) (listof word)-> (listof (symbol '()))
    ;; Purpose: To generate a list of lists of a state and an empty list for each given states
    ;; Accumulator Invariant: accum = list containing a list with the state and an empty list of words
    (define (build-list-of-states-&-empty-low-helper states accum)
      (if (null? states) accum
          (build-list-of-states-&-empty-low-helper (cdr states) (cons (list (car states) '()) accum))))
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
        (if (null? list-of-words-&-states) accum
            (make-low-helper (cdr list-of-words-&-states) (cons (car (car list-of-words-&-states)) accum))))
      (make-low-helper list-of-words-&-states '()))
    ;; (listof (listof word state)) -> (listof (listof state (listof word)))
    ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
    ;; Accumulator Invairant: accum = list of states with all the possible words in them 
    (define (sort-words-helper states-&-empty-low accum)
      (if (null? states-&-empty-low) accum
          (sort-words-helper (cdr states-&-empty-low)
                             (cons (list (car (car states-&-empty-low))
                                         (make-low (filter (λ (x) (eq? (car (car states-&-empty-low)) (second x)))
                                                           listof-all-words-&-states))) accum))
          ))
            
    (sort-words-helper states-&-empty-low '()))
  (sort-words (sm-all-possible-words-helper all-paths-new-machine
                                            (list (list '() (sm-start a-machine))))))

;; machine . (list state (word -> boolean)) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs a-machine . a-loi)
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                          a-machine
                          (remove-states-that-cannot-reach-finals a-machine)))
  ;; list of invariants that are reachable from the starting configuration
  (define reachable-inv (filter (λ (x) (member? (car x) (sm-states new-machine))) a-loi))
  ;; all paths of new-machine
  (define all-paths-new-machine (find-paths new-machine))

  ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose: To return a list of the invarients and the word that causes them not to hold
  ;; Accumulator Invarient: accum = list of lists of words that cause the invarient not to hold
  ;;                                & the state that it doesn't hold for
  (define (sm-test-invs-helper all-paths accum)
    (if (null? all-paths)
        accum
        (sm-test-invs-helper (cdr all-paths)
                             (if (path-inv-not-hold? (car all-paths)
                                                     (filter
                                                      (λ (x) (eq? (third (last (car all-paths)))
                                                                  (car x)))
                                                      reachable-inv))
                                 (cons (list (word-of-path (car all-paths))
                                             (third (last (car all-paths)))) accum)
                                 accum))))
          
  (sm-test-invs-helper all-paths-new-machine
                       (let [(start-pair (assoc (sm-start a-machine) a-loi))]
                         (if (or
                              (not (pair? start-pair))
                              ((second start-pair) '()))
                             '()
                             (list (list '() (sm-start a-machine)))))))



