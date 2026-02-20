#lang racket/base
(provide get-all-regexp
         find-paths
         find-rules&states-to-state
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
(define (find-paths a-machine #:num-rep [rep-limit 1])
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
                                          rep-limit)))
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



;;; need to queue the states that we have to explore, if qos is empty, then stop

;; add the starting state (the state that we want to get the paths to)
;; then get the transitions that keep track of the state
;; then check if the states that the transitions 

;; THINGS WE'RE KEEPING TRACK OF
;; queue of states to explore
;; list of rules that weve used
;; list of visited states

(define (find-rules&states-to-state a-machine state)
  (define machine-states (sm-states a-machine))
  (define finals (sm-finals a-machine))

  (define states-set (mutable-seteq))
  
  ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invarient: paths = list of current paths
  (define (find-paths-helper qos lor)  
    (if (qempty? qos)
        (list lor (set->list states-set))
        (let* ([new-rules
                (for/list ([rule (in-list (sm-rules a-machine))]
                           #:when (and (not (member rule lor))
                                       (eq? (qfirst qos) (third rule)))) ;<- checking to see if the state in first of the queue matches the destination/third of a given rule
                  
                  rule)] ;<- for each new rule, adding it to the LOR, uhh this isn't gonna work properly rn, can't just cons it 
               [new-los (for/fold ([accum-lst '()])
                                  ([new-rule (in-list new-rules)])
                          (if (set-member? states-set (first new-rule))
                              (cond [(set-member? states-set (third new-rule))
                                     '()]
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
                              
                          )
                        #;(flatten (map (λ (x) (list (first x) (third x))) new-rules))])
          (begin
            (displayln (qfirst qos))
            
           
            (find-paths-helper (enqueue new-los (dequeue qos)) (append new-rules lor))))))

  (set-add! states-set state)
  (find-paths-helper (enqueue (for/list ([rule (in-list (sm-rules a-machine))]
                                         #:when (eq? state (third rule)))
                                (first rule)) (treelist))
                     
                     (for/list ([rule (in-list (sm-rules a-machine))]
                                #:when (eq? state (third rule)))
                       rule)))









;; machine (setof finals) -> (listof (listof paths) (listof rules) (mutable-seteq states))
;; Purpose: To return all the paths of the machine that take it from the starting state to any final state,
;;          it ups the rep-limit each time to ensure that all paths are being covered, also returns the
;;          list of rules and states of the paths
#;(define (get-all-paths a-machine finals-set)
    (define rep1-paths (find-paths a-machine))

    ;; (listof path) -> (listof (listof rules) (mutable-seteq states))
    ;; purpose: gets the rules of the given list of paths
    (define (get-path-rules&states machine-paths)
      (define new-states (mutable-seteq))
      (list (remove-duplicates (for*/list ([path (in-list machine-paths)]
                                           #:when (set-member? finals-set (third (last path)))
                                           [rule (in-list path)])
                                 (set-add! new-states (first rule))  
                                 (set-add! new-states (third rule))
                                 rule))
            new-states))

    ;; (listof paths) (listof rules) number -> (listof (listof path) (listof rules) (mutable-seteq state))
    ;; purpose: To return all the paths of the machine that take it from the starting state to any final state,
    ;;          it ups the rep-limit each time to ensure that all paths are being covered also returns the
    ;;          list of rules and states of the paths
    (define (get-all-paths-helper cur-paths cur-path-rules rep-num)
      (define next-rep-paths (find-paths a-machine #:num-rep rep-num))  ;; <- getting the paths of the next repetition

      (define rules&states-next-rep (get-path-rules&states next-rep-paths))
      (define rules-of-next-rep (first rules&states-next-rep)) ;; <- getting the rules of the next repetition
      (define states-of-next-rep (second rules&states-next-rep)) ;; <- getting the states of the next repetition
    
      (if (> (length rules-of-next-rep) (length cur-path-rules))                 ;;<- check if length next rep rules is greater than last loop of rules
          (get-all-paths-helper next-rep-paths rules-of-next-rep (add1 rep-num)) ;;<- if its greater then that means we have to loop again
          (list next-rep-paths rules-of-next-rep states-of-next-rep)))           ;; <- if its less than or equal to, we return the cur rep's paths and its rules
          
    (get-all-paths-helper rep1-paths (get-path-rules&states rep1-paths) 2))
 
    

;; machine -> hashtable
;; Purpose: To return a hashtable of all regular expressions that
;;          can be made from the given machine
(define (get-all-regexp a-machine)
  (define finals-set (list->seteq (sm-finals a-machine)))
   
  (define new-states (mutable-seteq)) 
  
  (define new-rules
    (remove-duplicates (for*/list ([path (in-list (find-paths a-machine))]
                                   #:when (set-member? finals-set (third (last path))) 
                                   [rule (in-list path)])
                         (set-add! new-states (first rule))
                         (set-add! new-states (third rule))
                         rule)))

  
  (cond [(not (set-member? new-states (sm-start a-machine)))
         (null-regexp)]
        [else 
         (define machine-with-states-that-reach-finals
           (make-unchecked-ndfa (set->list new-states)
                                (sm-sigma a-machine)
                                (sm-start a-machine)
                                (filter (λ (final-state) (set-member? new-states final-state)) (sm-finals a-machine))
                                new-rules))

         (displayln (sm-graph machine-with-states-that-reach-finals))
         
         (define state&its-machine (make-hash))
         
         #;(define all-paths (find-paths machine-with-states-that-reach-finals)) ;<- REFACTORED (old)
         (define all-paths (find-paths a-machine)) ;<- UNREFACTORED PATHS (old)
         

         
         (define states (sm-states machine-with-states-that-reach-finals))
  
         ;; (listof symbols) (listof regexp) -> (listof (listof symbol regexp))
         ;; Purpose: To return a hashtable of all regular expressions that
         ;;          can be made from the given machine, takes in a list of states that we want to find the regexps of 
         (define (get-all-regexp-helper los)
           (for ([symb (in-list los)])
             
             (define paths-to-first-los (filter (λ (path) (eq? (third (last path)) symb)) all-paths))
              
             #;(define (make-rules-states rules states paths)
               (define (make-rules-states-helper rules states path)
                 (if (empty? path)
                     (values rules states)
                     (make-rules-states-helper (cons (first path) rules)
                                               (cons (first (first path))
                                                     (cons (third (first path))
                                                           states))
                                               (rest path))))
               (if (empty? paths)
                   (values (remove-duplicates rules) (remove-duplicates states))
                   (call-with-values (lambda () (make-rules-states-helper rules states (first paths)))
                                     (lambda (rules states) (make-rules-states rules states (rest paths))))))
        
             ;(define-values (rules-for-first-los states-to-first-los) (make-rules-states '() '() paths-to-first-los))

             (define rules&states (find-rules&states-to-state a-machine symb))
             
             (define machine-only-paths-to-first-los    ;;; ✨ can get the rules, states using the function
               (make-unchecked-ndfa (second rules&states)
                                    (sm-sigma machine-with-states-that-reach-finals)
                                    (sm-start machine-with-states-that-reach-finals)
                                    (list symb)
                                    (first rules&states)))
             
             (hash-set! state&its-machine symb (simplify-regexp (fsa->regexp machine-only-paths-to-first-los))))
           state&its-machine)
         (define states-no-start-state (filter (λ (state) (not (eq? state (sm-start machine-with-states-that-reach-finals)))) states))
         (define paths-to-start-state (filter (λ (path) (eq? (third (last path)) (sm-start machine-with-states-that-reach-finals))) all-paths))
         (if (empty? paths-to-start-state)
             (begin (hash-set! state&its-machine
                               (sm-start machine-with-states-that-reach-finals)
                               (empty-regexp))
                    (get-all-regexp-helper states-no-start-state))
             (get-all-regexp-helper states))]))
 

