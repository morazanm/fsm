#lang racket/base
(provide get-all-regexp)
(require rackunit
         racket/list
         "../fsm-core/private/regexp.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-getters.rkt")

;; X loX -> Boolean
;; Purpose: To determine if the given item is a member of the given list
(define (member? x lox)
  (ormap (λ (y) (equal? x y)) lox))

;; A queue of X, (qof X), is eithe(r:
;;  1. empty
;;  2. (cons X (qof X))


(define E-QUEUE '())

(define qempty? null?)

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
;; Purpose: Return car X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (car a-qox)))

;; Tests for qfirst
;(check-error  (qfirst '()) "qfirst applied to an empty queue")
(check-equal? (qfirst '(a b c)) 'a)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (cdr a-qox)))

;; Tests for qfirst
;(check-error  (dequeue '()) "dequeue applied to an empty queue")
(check-equal? (dequeue '(a b c)) '(b c))






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
    (if (empty? next-rules) accum
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
    (if (qempty? a-qop)
        paths
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
          (if (empty? next-rules-first-path)
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


;; ndfa -> ndfa or symbol
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
(define (remove-states-that-cannot-reach-finals a-ndfa)
  (define paths-that-end-in-finals (filter (λ (x) (member? (third (last x)) (sm-finals a-ndfa)))
                                           (find-paths a-ndfa)))
  
  (define new-rules (remove-duplicates (apply append paths-that-end-in-finals)))
  
  (define new-states (remove-duplicates (append-map (λ (x) (list (car x) (third x))) new-rules)))

  (define new-finals (filter (λ (final-state) (member? final-state new-states)) (sm-finals a-ndfa)))

  (if (member? (sm-start a-ndfa) new-states)
      (make-unchecked-ndfa new-states (sm-sigma a-ndfa) (sm-start a-ndfa) new-finals new-rules)
      (null-regexp)))



;; machine -> hashtable
;; Purpose: To return a hashtable of all regular expressions that
;;          can be made from the given machine
(define (get-all-regexp a-machine)
  (define machine-with-states-that-reach-finals (remove-states-that-cannot-reach-finals a-machine))

  (define state&its-machine (make-hash))
  
  ;; (listof symbols) (listof regexp) -> (listof (listof symbol regexp))
  ;; Purpose: To return a hashtable of all regular expressions that
  ;;          can be made from the given machine
  (define (get-all-regexp-helper los all-paths states)
    (if (empty? los)
        state&its-machine
        (let* [;; all paths that reach the first of the los
               (paths-to-first-los (filter (λ (path) (eq? (third (last path)) (car los))) all-paths))
                      
                        
               ;; all rules used for all the paths of the first of the los (used to filter out rules that aren't used)
               (rules-for-first-los (remove-duplicates (apply append paths-to-first-los)))
                        
                        
               ;; all states to needed for all possible paths to get to the first of los
               (states-to-first-los (filter (λ (state) (or (member? state (map (λ (rule) (car rule)) rules-for-first-los))
                                                           (member? state (map (λ (rule) (third rule)) rules-for-first-los))))
                                            states))
               ;; final states for the new machine 
               (finals-in-first-los (filter (λ (a-final-state) (member? a-final-state states-to-first-los)) (sm-finals machine-with-states-that-reach-finals)))
                        
               ;; updated machine with new rules and new states                                          
               (machine-only-paths-to-first-los
                (make-unchecked-ndfa states-to-first-los
                           (sm-sigma machine-with-states-that-reach-finals)
                           (sm-start machine-with-states-that-reach-finals)
                           (list (car los))
                           rules-for-first-los))
               (regexp-first-los (simplify-regexp (fsa->regexp machine-only-paths-to-first-los)))]
          (begin
            ;;adding the state and the machine for that state to the hashtable
            (hash-set! state&its-machine (car los) regexp-first-los)
            (get-all-regexp-helper (cdr los)
                                   all-paths
                                   states)))))
  
  (if (eq? machine-with-states-that-reach-finals (null-regexp))
      (null-regexp)
      (let* [(all-paths (find-paths machine-with-states-that-reach-finals))
             
             (states (sm-states machine-with-states-that-reach-finals))
             
             ;; have to filter out starting state since there is no path to it
             (states-no-start-state (filter (λ (state) (not (eq? state (sm-start machine-with-states-that-reach-finals)))) states))

             ;;paths to start state
             (paths-to-start-state (filter (λ (path) (eq? (third (last path)) (sm-start machine-with-states-that-reach-finals))) all-paths))
          
             (rules (sm-rules machine-with-states-that-reach-finals))

             ]

        (if (empty? paths-to-start-state)
            (begin (hash-set! state&its-machine
                              (sm-start machine-with-states-that-reach-finals)
                              (empty-regexp))
                   (get-all-regexp-helper states-no-start-state
                                          all-paths
                                          states))
            (get-all-regexp-helper states all-paths states)))))
 

