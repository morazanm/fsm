#lang racket/base

(provide #;sm-all-possible-words sm-test-invs)
(require "../fsm-core/private/sm-getters.rkt"
         "../fsm-core/private/fsa.rkt"
         "../sm-graph.rkt"
         racket/list
         racket/set
         data/queue)

(define REPETITION-LIMIT 1)

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

(struct path-with-hash (path hash) #:transparent)

;; machine -> (listof rules)
;; Purpose: To return all the paths in the given machine 
(define (find-paths a-machine)
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
                               REPETITION-LIMIT)))
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

(struct ndfa (st sig s f r) #:transparent)

;; ndfa -> ndfa
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
#;(define (remove-states-that-cannot-reach-finals a-ndfa)
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

;; machine (listof (list state (word -> boolean))) -> (listof (listof state (listof word)))
;; Purpose: To return a list of all posible words that can be at each state in a machine 
#;(define (sm-all-possible-words a-machine a-loi)
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                          a-machine
                          (remove-states-that-cannot-reach-finals a-machine)))
  ;; list of invariants that are reachable from the starting configuration
  #;(define reachable-inv (filter (λ (x) (member? (car x) (sm-states new-machine))) a-loi))
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
                                                  (caddr (last (car all-paths)))) accum))))

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
                                         (make-low (filter (λ (x) (eq? (car (car states-&-empty-low)) (cadr x)))
                                                           listof-all-words-&-states))) accum))))
            
    (sort-words-helper states-&-empty-low '()))
  (sort-words (sm-all-possible-words-helper all-paths-new-machine
                                            (list (list '() (sm-start a-machine))))))

;; machine . (list state (word -> boolean)) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs a-machine . a-loi)
  (define a-loi-hash (for/hash ([inv (in-list a-loi)])
                       (values (car inv) (cadr inv))))
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine a-machine #;(remove-states-that-cannot-reach-finals a-machine))
  
  ;; all paths of new-machine
  (define all-paths-new-machine (find-paths new-machine))

  ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose: To return a list of the invarients and the word that causes them not to hold
  ;; Accumulator Invarient: accum = list of lists of words that cause the invarient not to hold
  ;;                                & the state that it doesn't hold for

  (define (always-true x)
    #t)
  (define (always-true-thunk)
    always-true)
  
  (define (sm-test-invs-helper all-paths)
    #;(map displayln (filter (lambda (x) (eq? 'S (third (car x)))) all-paths))
    ;(filter (λ (x) (not (eq? 'ε x))) (append-map (λ (x) (list (cadr x))) a-lor))
    (for/list ([path (in-list all-paths)]
               #:do [(define cache (caddr (car path)))
                     (define a-config (list (word-of-path path) cache))]
               #:when (begin
                        (when (eq? 'S (caddr (car path)))
                          (displayln (word-of-path path))
                          (displayln ((hash-ref a-loi-hash cache always-true-thunk) (car a-config))))
                        (not ((hash-ref a-loi-hash cache always-true-thunk) (car a-config)))))
      a-config))
  
  (if (null? a-loi)
      '()
      (cons (let [(start-pair (hash-ref a-loi-hash (sm-start a-machine) #f))]
              (if (or
                   (not (pair? start-pair))
                   ((cadr start-pair) '()))
                  '()
                  (list (list '() (sm-start a-machine)))))
            (sm-test-invs-helper all-paths-new-machine)
            )))