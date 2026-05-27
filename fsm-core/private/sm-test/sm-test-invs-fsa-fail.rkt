#lang racket
(provide sm-test-invs-fsa-fail)
(require "state-machine-function.rkt"
         "../sm-getters.rkt"
         "../fsa.rkt"
         "../../../sm-graph.rkt"
         "../sm-operations.rkt"
         racket/list
         racket/set
         data/queue)

;; complement testing for fsas

;; testing words that should reject

;; get the state machines for each state DONE

;; then get the complement of those states, bf traversal of those state machine for words that the machine should accept
;; hash map to make everything complement, find all paths func on each machince, get accepting paths

;; then from these state machines, update them to get the complement for each machine

;; breath first traversal to get accepting paths of the given machine, i think i can pull that from fsa testing



(struct path-with-hash (path hash) #:transparent)

;; machine -> (listof rules)
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine rep-limit)
  (define queue (make-queue))
  (define rules (sm-rules a-machine))
  
  ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invarient: paths = list of current paths
  (define (find-paths-helper)
    (if (queue-empty? queue)
        '()
        (let [(qfirst (dequeue! queue))]
          (for ([rule (in-list rules)]
                #:when (and (eq? (caddr (car (path-with-hash-path qfirst))) (car rule))
                            (< (hash-ref (path-with-hash-hash qfirst)
                                         rule
                                         0)
                               rep-limit)))
            (enqueue! queue (path-with-hash (cons rule (path-with-hash-path qfirst))
                                            (hash-set (path-with-hash-hash qfirst)
                                                      rule
                                                      (add1 (hash-ref (path-with-hash-hash qfirst)
                                                                      rule
                                                                      0))))))
          (cons qfirst (find-paths-helper)))))
  (for ([rule (in-list (sm-rules a-machine))]
        #:when (eq? (car rule) (sm-start a-machine)))
    (enqueue! queue (path-with-hash (list rule) (hash rule 1))))
  (map (lambda (x) (path-with-hash-path x)) (find-paths-helper)))

;; ndfa -> ndfa
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
(define (remove-states-that-cannot-reach-finals a-ndfa)
  (define queue (make-queue))
  (define rules (sm-rules a-ndfa))
  
  ;; machine -> (listof rules)
  ;; Purpose: To return all the paths in the given machine 
  (define (explore-all-paths a-machine)
    ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
    ;; Purpose: To return all the paths of the given machine
    ;; Accumulator invariant: paths = list of current paths
    (define (explore-all-paths-helper)
      (if (queue-empty? queue)
          '()
          (let [(firstofq (dequeue! queue))]
            (for ([rule (in-list rules)]
                  #:when (and (eq? (caddr (car firstofq)) (car rule))
                              (not (member rule firstofq))))
              (enqueue! queue (cons rule firstofq)))
            (cons firstofq (explore-all-paths-helper)))))
    (for ([rule (in-list rules)]
          #:when (eq? (car rule) (sm-start a-machine)))
      (enqueue! queue (list rule)))
    (explore-all-paths-helper))
  
  (define new-states-set (mutable-seteq))
  (define new-rules-set (mutable-set))
  (for* ([path (in-list (for/list ([path (in-list (explore-all-paths a-ndfa))]
                                   #:when (member (caddr (car path)) (sm-finals a-ndfa)))
                          (reverse path)))]
         [rule (in-list path)])
    (set-add! new-states-set (car rule))
    (set-add! new-states-set (caddr rule)))
  (for ([rule (in-list rules)]
        #:when (and (set-member? new-states-set (car rule))
                    (set-member? new-states-set (caddr rule))))
    (set-add! new-rules-set rule))
  (make-unchecked-ndfa (set->list new-states-set) (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) (set->list new-rules-set)))




;; (listof rules) -> word
;; Purpose: To return a word that is made from the given list of rules
(define (word-of-path a-lor)
  (define (word-of-path-helper accum a-lor)
    (if (null? a-lor)
        accum
        (if (eq? 'ε (cadr (car a-lor)))
            (word-of-path-helper accum (cdr a-lor))
            (word-of-path-helper (cons (cadr (car a-lor)) accum) (cdr a-lor)))))
  (word-of-path-helper '() a-lor))


(define (get-paths-to-finals paths finals)
  (filter (λ (path) (member (third (first path)) finals))
          paths))

(define (extract-rule-set paths)
  (remove-duplicates (apply append paths)))

(define (extract-state-set rules)
  (remove-duplicates (append-map (λ (r) (list (first r) (third r))) rules)))

(define (filter-paths paths states)
  (define (has-only? p states) (andmap (λ (r) (and (member (first r) states)
                                                   (member (third r) states)))
                                       p))
  (filter (λ (p) (has-only? p states)) paths))




;; have to optimize it
;; - i mean i could like combine all the loops together but it would lowkey be unreadable
;; - i could also give have get-all-state-machines return the complements instead
;; - the paths i can't touch other than find-path only accumulate paths that lead to an accept, no intermediate paths
;; - for getting the words, i don't think i can change anything



;; machine . (list state (word -> boolean)) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs-fsa-fail a-machine rep-limit ds-remove a-loi)  
  ;;🦜
  (define state-machine-hash (get-all-state-machines a-machine ds-remove)) ;<- okay so this is a hash of the state an machine for that state

  
  (define complement-state-hash (for/hash ([(key value)
                                            (in-hash state-machine-hash)])
                                  (sm-graph value)
                                  (values key (sm-complement value))))

  ;; okay now have to get the paths for each state machine
  (define paths-for-state-machines-hash (for/hash ([(key value)
                                                   (in-hash complement-state-hash)])
                                          (values key (list (sm-finals value) (find-paths value rep-limit)))))

  
  ;; getting the accepting paths
  (define accepting-paths-state-machine-hash (for/hash ([(key value)
                                                        (in-hash paths-for-state-machines-hash)])
                                               (values key (filter (λ (x) (member (third (first x)) (first value))) (second value)))))


  ;; making all the accepting paths be words
  (define state-words-hash (for/hash ([(key value)
                                         (in-hash accepting-paths-state-machine-hash)])
                               (values key (map (λ (x) (word-of-path x)) value))))


  ;; now have to test the corresponding state with all the words, and accum the words that don't hold
  
  ;; inv-pair -> hash
  (define get-failing-words-hash (for/hash ([(key value)
                                             (in-hash state-words-hash)])
                                   (define cur-state-inv (assoc key a-loi))
                                   (values key (filter (λ (word) ((second cur-state-inv) word)) value))))
  

  ;; filter for words that don't hold for the inv pred^^^
  ;; use state-word-hash for this^^

  ;; okay so at this point get-failing-words should be a hash with the key being the state and the value being a list of the words that fail to fail
  
  

  
  ;; for each state, get the words from the state-words-hash of that state, then test all the words, and accum those words in a list
  ;; wait i can just do the for hash again

  
 get-failing-words-hash)







