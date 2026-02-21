#lang racket/base
(provide get-all-regexp
         find-path-rule&states
         find-rules&states-to-state
         find-path-rule&states-dead-state-sweep
         )
(require racket/list
         racket/treelist
         racket/set
         "../regexp.rkt"
         "../fsa.rkt"
         "../sm-getters.rkt"
         "../../../sm-graph.rkt" )

(define E-QUEUE (treelist))

(define qempty? treelist-empty?)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (foldr (lambda (val accum) (treelist-add accum val))
                                     a-qox
                                     a-lox))

;; (qof X) --> X throws error
;; Purpose: Return car X of the given queue
(define qfirst treelist-first)


;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define dequeue treelist-rest)




;; machine state -> machine
;; purpose: removes dead states that aren't reachable from starting state
(define (find-path-rule&states a-machine)
  (define finals (sm-finals a-machine))
  (define start-state (sm-start a-machine))

  (define states-set (mutable-seteq))
  
  ;; (queueof (listof rule)) (listof (listof rule)) -> machine
  ;; Purpose: removes dead states that aren't reachable from starting state
  (define (find-rules&states-to-state-helper qos lor)  
    (if (qempty? qos)
        (make-unchecked-ndfa (set->list states-set)
                             (sm-sigma a-machine)
                             start-state
                             finals
                             (set->list lor))
        (let* ([new-rules
                (for/list ([rule (in-list (sm-rules a-machine))]
                           #:when (and (not (set-member? lor rule))
                                       (eq? (qfirst qos) (first rule))))
                  
                  rule)]
               [new-los (for/fold ([accum-lst '()])
                                  ([new-rule (in-list new-rules)])
                          (if (set-member? states-set (first new-rule))
                              (cond [(set-member? states-set (third new-rule))
                                     accum-lst]
                                    [else
                                     (set-add! states-set (third new-rule))
                                     (cons (third new-rule) accum-lst)])
                              (cond [(set-member? states-set (third new-rule))
                                     (set-add! states-set (first new-rule))
                                     (cons (first new-rule) accum-lst)]
                                    [else
                                     (set-add! states-set (first new-rule))
                                     (set-add! states-set (third new-rule))
                                     (cons (first new-rule) (cons (third new-rule) accum-lst))]))
                              
                          )])
          (find-rules&states-to-state-helper (enqueue new-los (dequeue qos))
                                             (for/fold ([rules-set lor])
                                                       ([new-rule new-rules])
                                               (set-add rules-set new-rule))
                                             #;(append new-rules lor)))))

  (set-add! states-set start-state)
  (find-rules&states-to-state-helper (enqueue (for/list ([rule (in-list (sm-rules a-machine))]
                                                         #:when (eq? start-state (first rule)))
                                                (first rule)) (treelist))
                     
                                     (for/set ([rule (in-list (sm-rules a-machine))]
                                                #:when (eq? start-state (third rule)))
                                       rule)))








;; machine  -> (list (listof rules) (listof states))
;; purpose: removes inner dead states of the given machine (assumes that everything is reachable from the starting state)
(define (find-path-rule&states-dead-state-sweep a-machine first-sweep-rules&states)
  (define finals (sm-finals a-machine))

  (define list-of-rules&states (for/list ([final-state (in-list finals)])
                                 (find-rules&states-to-state a-machine final-state)))

  (define rules (remove-duplicates (apply append (map (λ (x) (first x)) list-of-rules&states) )))
  (define states (remove-duplicates (apply append (map (λ (x) (second x)) list-of-rules&states))))

  (list rules states))








;; machine state -> (list (listof rules) (listof states))
;; purpose: to return the rules and states needed to get to a given state in the given machine 
(define (find-rules&states-to-state a-machine state)
  (define states-set (mutable-seteq))
  
  ;; (queueof (listof rule)) (listof (listof rule)) -> (list (listof rule) (listof state)
  ;; Purpose: to return the rules and states needed to get to a given state in the given machine 
  (define (find-rules&states-to-state-helper qos lor)  
    (if (qempty? qos)
        (list lor (set->list states-set))
        (let* ([new-rules
                (for/list ([rule (in-list (sm-rules a-machine))]
                           #:when (and (not (member rule lor))
                                       (eq? (qfirst qos) (third rule)))) ;<- checking to see if the state in first of the queue matches the destination/third of a given rule
                  
                  rule)]
               [new-los (for/fold ([accum-lst '()])
                                  ([new-rule (in-list new-rules)])
                          (if (set-member? states-set (first new-rule))
                              (cond [(set-member? states-set (third new-rule))
                                     accum-lst]
                                    [else
                                     (set-add! states-set (third new-rule))
                                     (cons (third new-rule) accum-lst)])
                              (cond [(set-member? states-set (third new-rule))
                                     (set-add! states-set (first new-rule))
                                     (cons (first new-rule) accum-lst)]
                                    [else
                                     (set-add! states-set (first new-rule))
                                     (set-add! states-set (third new-rule))
                                     (cons (first new-rule) (cons (third new-rule) accum-lst))]))
                              
                          )])
          (find-rules&states-to-state-helper (enqueue new-los (dequeue qos)) (append new-rules lor)))))

  (set-add! states-set state)
  (find-rules&states-to-state-helper (enqueue (for/list ([rule (in-list (sm-rules a-machine))]
                                                         #:when (eq? state (third rule)))
                                                (first rule)) (treelist))
                     
                                     (for/list ([rule (in-list (sm-rules a-machine))]
                                                #:when (eq? state (third rule)))
                                       rule)))







;; machine -> hashtable
;; Purpose: To return a hashtable of all regular expressions that
;;          can be made from the given machine
(define (get-all-regexp a-machine)

  (define rules&states (find-path-rule&states-dead-state-sweep a-machine (find-path-rule&states a-machine)))
  (define new-states (second rules&states))
  (define new-rules (first rules&states))
  (define starting-state (sm-start a-machine))
  
  (define finals-set (list->seteq (sm-finals a-machine)))
  (define new-finals (filter (λ (x) (set-member? finals-set x)) new-states))
     
  (cond [(not (set-member? new-states starting-state))
         (null-regexp)]
        [else 
         (define machine-with-states-that-reach-finals
           (make-unchecked-ndfa (set->list new-states)
                                (sm-sigma a-machine)
                                starting-state
                                new-finals
                                new-rules))

         ;(displayln (sm-graph machine-with-states-that-reach-finals)) ;<- this is the refactored machine
         
         (define state&its-machine (make-hash))
         
         
         (define states (sm-states machine-with-states-that-reach-finals))
  
         ;; (listof symbols) (listof regexp) -> (listof (listof symbol regexp))
         ;; Purpose: To return a hashtable of all regular expressions that
         ;;          can be made from the given machine, takes in a list of states that we want to find the regexps of 
         (define (get-all-regexp-helper los)
           (for ([symb (in-list los)])
             (define rules&states (find-rules&states-to-state a-machine symb))
             
             (define machine-only-paths-to-first-los    
               (make-unchecked-ndfa (second rules&states)
                                    (sm-sigma machine-with-states-that-reach-finals)
                                    starting-state
                                    (list symb)
                                    (first rules&states)))

             ;(displayln (sm-graph machine-only-paths-to-first-los)) ;<- this will display all the regular expressions of the states
                          
             (hash-set! state&its-machine symb (simplify-regexp (fsa->regexp machine-only-paths-to-first-los))))
           state&its-machine)

         
         (define states-no-start-state (filter (λ (state) (not (eq? state starting-state))) states))
         (define paths-to-start-state (filter (λ (rule) (eq? (third rule) starting-state)) new-rules))
         (if (empty? paths-to-start-state)
             (begin (hash-set! state&its-machine
                               starting-state
                               (empty-regexp))
                    (get-all-regexp-helper states-no-start-state))
             (get-all-regexp-helper states))]))
 

