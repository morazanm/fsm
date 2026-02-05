#lang racket/base
(provide get-all-regexp-new-new
         find-paths)

(require rackunit
         racket/list
         racket/treelist
         racket/set
         "../fsm-core/private/regexp.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-getters.rkt")

;; QUEUE FUNCTIONS

;; (qof X) --> (qof X)
;; Purpose: To determine if the given queue is empty
(define qempty? treelist-empty?)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (foldr (lambda (val accum) (treelist-add accum val))
                                     a-qox
                                     a-lox))

;; (qof X) --> X throws error
;; Purpose: Return first X of the given queue
(define qfirst treelist-first)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define dequeue treelist-rest)


;; A path is a treelist of rules

;; A path-with-hash is (PATH hash)

(struct path-with-hash (path hash) #:transparent)


;; machine -> (listof (listof rules))
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine)
  
  ;; (queueof path-with-hash)  -> (listof (listof rule))
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invarient: paths = list of current paths
  (define (find-paths-helper a-qop)
    (if (qempty? a-qop)
        '()
        (let* ([cache (third (first (path-with-hash-path (qfirst a-qop))))]  ;<- destination state of
                                                                             ;   the last rule in path
               [next-rules-first-path
                (for/list ([rule (in-list (sm-rules a-machine))]  ;<- creating the rules that can be used 
                           #:when (and (eq? cache (car rule))     ;   next(cannot have already been used)
                                       (< (hash-ref (path-with-hash-hash (qfirst a-qop))
                                                    rule
                                                    0)
                                          1)))  
                  (path-with-hash (cons rule (path-with-hash-path (qfirst a-qop)))
                                  (hash-set (path-with-hash-hash (qfirst a-qop))
                                            rule
                                            (add1 (hash-ref (path-with-hash-hash (qfirst a-qop))
                                                            rule
                                                            0)))))])
          (cons (qfirst a-qop) (find-paths-helper (enqueue next-rules-first-path (dequeue a-qop)))))))
  
  (map (lambda (x) (reverse (path-with-hash-path x)))
       (find-paths-helper (enqueue (for/list ([rule (in-list (sm-rules a-machine))]        ;<-enqueuing paths
                                              #:when (eq? (car rule) (sm-start a-machine))); with the starting rules
                                     (path-with-hash (list rule) (hash rule 1)))
                                   (treelist)))))




;; (listof path) (listof states) -> (listof path)
;; purpose: to return the paths that reach finals
(define (get-paths-to-finals paths finals)
  (filter (λ (path) (member (third (last path)) finals))
          paths))

;; (listof path) -> (listof rule)
;; purpose: to extract the rule set from the given paths
(define (extract-rule-set paths)
  (remove-duplicates (apply append paths)))

;; (listof rules) -> (listof states)
;; purpose: To extract the set of states from the given the rule set
(define (extract-state-set rules)
  (remove-duplicates (append-map (λ (r) (list (first r) (third r))) rules)))

;; (listof paths) (listof states) -> (listof paths)
;; purpose: To return the paths that contain the given states
(define (filter-paths paths states)
  (define (has-only? p states) (andmap (λ (r) (and (member (first r) states)
                                                   (member (third r) states)))
                                       p))
  (filter (λ (p) (has-only? p states)) paths))

;; state (listof paths) -> (listof paths)
;; purpose: to return all the paths to the given state
(define (all-paths-to A paths)
  (filter (λ (p) (eq? (third (last p)) A)) paths))
  

;; machine -> hashtable
;; Purpose: Construct regular expression hash table for the given machine
(define (get-all-regexp-new-new M)
  (let* [(regexp-ht (make-hash))  ;; empty ht for regexps
         (machine-paths (find-paths M)) ;; all machine paths starting at the starting state including paths to dead states
         (paths-to-finals (get-paths-to-finals machine-paths (sm-finals M))) ;; paths to final states starting at the starting state
         
         (new-rules (extract-rule-set paths-to-finals))  ;; all rules used on all paths to a final state from the starting state
         (new-states (extract-state-set new-rules)) ;; all states traversed on all paths to a final state from the starting state
         (refactored-paths (filter-paths machine-paths new-states))] ;; all machine paths starting at the starting state that only traverse states on a path to final state reachable from the starting state
    (for ([A new-states]) ;; traverse all stated on a path to a final state from the starting state
      (if (and (eq? A (sm-start M))
               (empty? (all-paths-to A refactored-paths)))
          ;; starting state special case: if there are no paths to the starting state in refactored-paths, the starting state is still reachable by consuming empty
          (hash-set! regexp-ht
                     A
                     (empty-regexp))
          (let* [(A-paths (all-paths-to A refactored-paths)) ;; all paths to A on a path from the final state from the starting state
                 (rules4A (extract-rule-set A-paths)) ;; rules used to reach A
                 (states4A (extract-state-set rules4A)) ;; states traversed to reach A
                 (A-ndfa (make-unchecked-ndfa states4A  ;; ndfa for A (A is the final state)
                                              (sm-sigma M) 
                                              (sm-start M) 
                                              (list A) 
                                              rules4A))]
            (hash-set! regexp-ht ;; add regexp for A to ht (maybe also need to simplify regexp, not sure it will do anything useful)
                       A 
                       (fsa->regexp A-ndfa)))))
    regexp-ht))
