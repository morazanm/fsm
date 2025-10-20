#lang racket/base
(provide get-all-regexp
         find-paths)
(require rackunit
         racket/list
         racket/treelist
         racket/set
         "../fsm-core/private/regexp.rkt"
         "../fsm-core/private/fsa.rkt"
         "../fsm-core/private/sm-getters.rkt")

;; A queue of X, (qof X), is eithe(r:
;;  1. empty
;;  2. (cons X (qof X))


(define E-QUEUE (treelist))

(define qempty? treelist-empty?)

;; Tests for qempty?
;(check-equal? (qempty? '())      #true)
;(check-equal? (qempty? '(a b c)) #false)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (foldr (lambda (val accum) (treelist-add accum val))
                                     a-qox
                                     a-lox))

;; Tests for enqueue
;(check-equal? (enqueue '(8 d) '()) '(8 d))
;(check-equal? (enqueue '(d) '(a b c)) '(a b c d))
;(check-equal? (enqueue '(6 5 4) '(7)) '(7 6 5 4))

;; (qof X) --> X throws error
;; Purpose: Return car X of the given queue
(define qfirst treelist-first)

;; Tests for qfirst
;(check-error  (qfirst '()) "qfirst applied to an empty queue")
;(check-equal? (qfirst '(a b c)) 'a)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define dequeue treelist-rest)

;; Tests for qfirst
;(check-error  (dequeue '()) "dequeue applied to an empty queue")
;(check-equal? (dequeue '(a b c)) '(b c))
(struct path-with-hash (path hash) #:transparent)
;; machine -> (listof rules)
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine)
  ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invarient: paths = list of current paths
  (define (find-paths-helper a-qop)
    (if (qempty? a-qop)
        '()
        (let* ([cache (third (first (path-with-hash-path (qfirst a-qop))))]
               [next-rules-first-path
                (for/list ([rule (in-list (sm-rules a-machine))]
                           #:when (and (eq? cache (car rule))
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
       (find-paths-helper (enqueue (for/list ([rule (in-list (sm-rules a-machine))]
                                              #:when (eq? (car rule) (sm-start a-machine)))
                                     (path-with-hash (list rule) (hash rule 1)))
                                   (treelist)))))

;; machine -> hashtable
;; Purpose: To return a hashtable of all regular expressions that
;;          can be made from the given machine
(define (get-all-regexp a-machine)
  (define finals-set (list->seteq (sm-finals a-machine)))
  (define new-states (mutable-seteq))
  (define machine-paths (find-paths a-machine))
  (define new-rules 
    (remove-duplicates (for*/list ([path (in-list machine-paths)]
                                   ;; eliminate all paths that do not end in a final state
                                   #:when (set-member? finals-set (third (last path)))
                                   [rule (in-list path)])
                         (set-add! new-states (first rule))
                         (set-add! new-states (third rule))
                         rule)))
  (cond [(not (set-member? new-states (sm-start a-machine)))
         (null-regexp)]
        [else 
         #;(define machine-with-states-that-reach-finals
             (make-unchecked-ndfa (set->list new-states)
                                  (sm-sigma a-machine)
                                  (sm-start a-machine)
                                  (filter (λ (final-state)
                                            (set-member? new-states final-state))
                                          (sm-finals a-machine))
                                  new-rules))

         ;; path sethash -> Boolean
         ;; Purpose: Determine if all states in given path are in the given sethash
         (define (no-dead-states-in-path? path new-states)
           (let [(path-states (remove-duplicates
                               (append-map (λ (rule) (list (first rule) (third rule)))
                                           path)))]
             (andmap (λ (st) (set-member? new-states st))
                     path-states)))
                 
         ;; (listof paths) sethash -> (listof paths)
         ;; Purpose: Remove paths containing dead states
         (define (remove-dead-paths machine-paths new-states)
           (filter (λ (path) (no-dead-states-in-path? path new-states))
                   machine-paths))
         
         (define state&its-machine (make-hash))
         (define refactored-states (set->list new-states))
         (define refactored-sigma (sm-sigma a-machine))
         (define refactored-start (sm-start a-machine))
         (define refactored-finals (filter (λ (final-state)
                                             (set-member? new-states final-state))
                                           (sm-finals a-machine)))
         (define refactored-rules new-rules)
         (define refactored-paths (remove-dead-paths machine-paths new-states)
           #;(find-paths machine-with-states-that-reach-finals))
         ;#(define states (sm-states machine-with-states-that-reach-finals))
  
         ;; (listof symbols) (listof regexp) -> (listof (listof symbol regexp))
         ;; Purpose: To return a hashtable of all regular expressions that
         ;;          can be made from the given machine
         (define (get-all-regexp-helper los)
           (for ([symb (in-list los)])
             
             (define paths-to-first-los
               (filter (λ (path) (eq? (third (last path)) symb))
                       refactored-paths))
              
             (define (make-rules-states rules states paths)
               (define (make-rules-states-helper rules states path)
                 (if (empty? path)
                     (values rules states)
                     (make-rules-states-helper
                      (cons (first path) rules)
                      (cons (first (first path))
                            (cons (third (first path))
                                  states))
                      (rest path))))
               (if (empty? paths)
                   (values (remove-duplicates rules) (remove-duplicates states))
                   (call-with-values (lambda () (make-rules-states-helper rules states (first paths)))
                                     (lambda (rules states) (make-rules-states rules states (rest paths))))))
        
             (define-values (rules-for-first-los states-to-first-los) (make-rules-states '() '() paths-to-first-los))
             
             (define machine-only-paths-to-first-los
               (make-unchecked-ndfa states-to-first-los
                                    refactored-sigma
                                    refactored-start
                                    (list symb)
                                    rules-for-first-los))
             
             (hash-set! state&its-machine
                        symb
                        (simplify-regexp
                         (fsa->regexp machine-only-paths-to-first-los))))
           state&its-machine)
         (define states-no-start-state
           (filter (λ (state) (not (eq? state refactored-start)))
                   refactored-states))
         (define paths-to-start-state
           (filter (λ (path) (eq? (third (last path)) refactored-start))
                   refactored-paths))
         (if (empty? paths-to-start-state)
             (begin (hash-set! state&its-machine
                               refactored-start
                               (empty-regexp))
                    (get-all-regexp-helper states-no-start-state))
             (get-all-regexp-helper refactored-states))]))
 

