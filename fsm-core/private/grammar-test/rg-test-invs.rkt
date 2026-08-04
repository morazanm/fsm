#lang racket/base

(require (for-syntax racket/base
                     racket/set)
         racket/require
         #;racket/fixnum
         (filtered-in
          (λ (name)
            (define unsafe-methods-used
              (set "unsafe-vector*-ref"
                   "unsafe-vector*-set!"
                   "unsafe-vector*-length"
                   "unsafe-fxvector-ref"
                   "unsafe-fxvector-set!"
                   "unsafe-fxvector-length"
                   "unsafe-car"
                   "unsafe-cdr"))
            (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
                  [(regexp-match #rx"^unsafe-cons-list" name) "cons"]
                  [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          racket/unsafe/ops)
         (only-in racket/fixnum for/fxvector in-fxvector)
         #;"../testing-utils/structs.rkt"
         (filtered-in
          (λ (name)
            (cond [(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          "../testing-utils/unsafe-struct-accessors.rkt")
         ;"../cfg-struct.rkt"
         ;"../cfg-constructors.rkt"
         "../rg-constructors.rkt"
         "../regular-grammar.rkt"
         "../constants.rkt"
         ;"../pda.rkt"
         "../fsa.rkt"
         "../testing-utils/unsafe-packed-vector.rkt"
         "../testing-utils/work-stealing-deque.rkt"
         "../misc.rkt"
         "../../../sm-graph.rkt"
         racket/set
         racket/list
         data/queue
         racket/treelist
         "../sm-getters.rkt"
         "../grammar-getters.rkt")
(provide rg-test-invs)
;; andres stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (determine-num-bits-needed num-rules)   
    (define (helper num)
      (if (>= (expt 2 num) num-rules)
          num
          (helper (add1 num))))
    (define res (helper 1))
    (if (byte? res)
        res
        (error "idk")))

(define (sm-test-invs-fsa-spooky a-machine rep-limit nt-tested inv)
    (define rules-len (length (fsa-getrules a-machine)))
    (define finals-set (list->seteq (fsa-getfinals a-machine)))
    (define-values (make-gcfxvector
                    gcfxvector-ref
                    gcfxvector-add!
                    gcfxvector-add/copy)
      (create-gcfxv-functions (determine-num-bits-needed rules-len)))
  
    (define rules
      (for/vector #:length rules-len
        ([rule (in-list (fsa-getrules a-machine))])
        (rule-struct (car rule) (cadr rule) (caddr rule))))
           
    (define rule-idx-from-final
      (for/hasheq ([rule (in-vector rules)])
        (values (unsafe-struct*-ref rule 2)
                (for/fxvector ([a-rule (in-vector rules)]
                               [idx (in-naturals)]
                               #:when (eq? (unsafe-struct*-ref rule 2)
                                           (unsafe-struct*-ref a-rule 0)))
                  idx))))
           
    (define (word-of-path a-gv)
      (define (word-of-path-helper accum idx)
        (if (fx= idx 0)
            (if (eq? 'ε (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv 0)) 1))
                accum
                (cons (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv 0)) 1) accum))
            (if (eq? 'ε (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv idx)) 1))
                (word-of-path-helper accum (fx- idx 1))
                (word-of-path-helper (cons (unsafe-struct*-ref (vector*-ref rules (gcfxvector-ref a-gv idx)) 1) accum)
                                     (fx- idx 1)))))
      (word-of-path-helper '() (fx- (unsafe-struct*-ref a-gv 1) 1)))

           
    (define (create-new-words stack qfirst)
      (define final-state
        (unsafe-struct*-ref
         (vector*-ref rules
                      (gcfxvector-ref (unsafe-struct*-ref qfirst 0)
                                      (fx- (unsafe-struct*-ref (unsafe-struct*-ref qfirst 0) 1) 1)))
         2))
                    
      (for ([idx (in-fxvector (hash-ref rule-idx-from-final final-state))]
            #:do [(define curr-rep-count (bytes-ref (unsafe-struct*-ref qfirst 1)
                                                    idx))]
            #:when (fx< curr-rep-count
                        rep-limit))
        (define len (bytes-length (unsafe-struct*-ref qfirst 1)))
        (define new-vec (make-bytes len 0))
        (bytes-copy! new-vec 0 (unsafe-struct*-ref qfirst 1) 0 len)
        (bytes-set! new-vec idx (fx+ curr-rep-count 1))
        (push! stack
               (path-with-rep-count (gcfxvector-add/copy (unsafe-struct*-ref qfirst 0) idx)
                                    new-vec)))
      (define word-path (word-of-path (unsafe-struct*-ref qfirst 0)))
      (if (set-member? finals-set final-state) 
          (if (inv word-path)
              #f
              (list nt-tested word-path))
          #f))

    (define (accumulate-results-into-lst vec)
      (define (loop idx accum)
        (if (fx= idx (vector*-length vec))
            accum
            (loop (fx+ idx 1) (append (vector*-ref vec idx) accum))))
      (loop 0 '()))
           
    (accumulate-results-into-lst
     (run-parallel (for/list ([rule (in-vector rules)]
                              [idx (in-naturals)]
                              #:when (eq? (unsafe-struct*-ref rule 0) (fsa-getstart a-machine)))
                     (define vec (make-bytes rules-len 0))
                     (bytes-set! vec idx 1)
                     (path-with-rep-count (make-gcfxvector 16 idx) vec))
                   create-new-words
                   2048)))



(define (rg->state-ndfa a-rg new-start)   
  (define old-nts (list->seteq (cons EMP (rg-getnts a-rg))))
  (define new-rules (mutable-set))
  (define new-nts (mutable-seteq))

  (define (found-new-rule rules)
    (cond [(null? rules)
           (find-new-rule (rg-getunparsedrules a-rg))]
          [else
           (define first-rule (car rules))
           (define lhs-nt (car first-rule))
           (cond [(and (set-member? new-nts lhs-nt)
                       (not (set-member? new-rules first-rule)))
                  (set-add! new-rules first-rule)
                  (set-add! new-nts lhs-nt)
                  (for ([symb (in-list (symbol->fsmlos (caddr first-rule)))]
                        #:when (set-member? old-nts symb))
                    (when (not (eq? symb EMP))
                      (set-add! new-nts symb)))
                  (found-new-rule (cdr rules))]
                 [else
                  (found-new-rule (cdr rules))])]))
  
  (define (find-new-rule rules)
    (cond [(null? rules)
           (void)]
          [(and (set-member? new-nts (caar rules))
                (not (set-member? new-rules (car rules))))
           (set-add! new-rules (car rules))
           (set-add! new-nts (caar rules))
           (for ([symb (in-list (symbol->fsmlos (caddar rules)))]
                 #:when (set-member? old-nts symb))
             (when (not (eq? symb EMP))
               (set-add! new-nts symb)))
           (found-new-rule (cdr rules))]
          [else
           (find-new-rule (cdr rules))]))
  (set-add! new-nts new-start)
  (find-new-rule (rg-getunparsedrules a-rg))
        
  (define new-rg
    (make-unchecked-rg
     (set->list new-nts)
     (rg-getalphabet a-rg)
     (set->list new-rules)
     new-start))
  (define new-state-ndfa
    (rg->fsa new-rg))
  new-state-ndfa)

(struct rule-struct (start-state read-elem dest-state) #:transparent)
(struct path-with-rep-count (path rep-counts) #:transparent)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; OLD VERSION OF SM-TEST-INVS-FSA
(define REPETITION-LIMIT 1)

;(struct path-with-hash (path hash) #:transparent) ;<- old path structure
(struct path-with-hash (config-path word-of-path path-starting-state hash) #:transparent)


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
        empty-treelist
        (let [(qfirst (dequeue! queue))]
          (for ([rule (in-list rules)]
                #:when (and #;(eq? (caddr (car (path-with-hash-path qfirst))) (car rule)) ;; when the dest of last rule is equal to the first of another
                            (eq? (car rule) (car (car (path-with-hash-config-path qfirst))))
                            (< (hash-ref (path-with-hash-hash qfirst)
                                         rule
                                         0)
                               rep-limit)))
            (let* [(first-config-path (first (path-with-hash-config-path qfirst)))
                   (path-word (if (eq? 'ε (second rule))
                                  (second first-config-path)
                                  (cons (second rule) (second first-config-path))))]
              (enqueue! queue (path-with-hash (cons (list (third rule)
                                                          path-word) ;<- the word is backwards in the configs here
                                                    (path-with-hash-config-path qfirst))
                                              path-word
                                              (path-with-hash-path-starting-state qfirst)
                                              (hash-set (path-with-hash-hash qfirst)
                                                        rule
                                                        (add1 (hash-ref (path-with-hash-hash qfirst)
                                                                        rule
                                                                        0))))))
            #;(enqueue! queue (path-with-hash (cons rule (path-with-hash-path qfirst))
                                              (hash-set (path-with-hash-hash qfirst)
                                                        rule
                                                        (add1 (hash-ref (path-with-hash-hash qfirst)
                                                                        rule
                                                                        0))))))
          (treelist-cons (find-paths-helper) qfirst))))
  (for ([rule (in-list (sm-rules a-machine))]
        #:when (eq? (car rule) (sm-start a-machine)))
    (enqueue! queue #;(path-with-hash (list rule) (hash rule 1))
              (path-with-hash (list (list (third rule) (list (second rule))))
                              (list (second rule))
                              (sm-start a-machine)
                              (hash rule 1))))
  #;(map (lambda (x) (path-with-hash-path x)) (find-paths-helper))
  #;(treelist-map (find-paths-helper)
                (λ (x) (path-with-hash (reverse (path-with-hash-config-path x))  ;<- NEED THIS IF USING SPLIT-PATHS
                                       (path-with-hash-word-of-path x)
                                       (path-with-hash-path-starting-state x)
                                       (path-with-hash-hash x))))
  (treelist-map (find-paths-helper)
                (λ (x) (path-with-hash (path-with-hash-config-path x) 
                                       (reverse (path-with-hash-word-of-path x))
                                       (path-with-hash-path-starting-state x)
                                       (path-with-hash-hash x))))) ;<- NEED THIS IF USING original version of sm-test-invs 



;; (listof path) nts -> ht
;; Purpose: returns a hash table of test words for each nt
(define (split-paths paths nts)
  ;; making hash table for the nt and all the words that are gonna be used to test them
  (define nts-hash (for/hash ([nt (in-list nts)])
                     (values nt (mutable-set))))

  ;; for each path, split the path if state dif from cur path state
  ;; purpose: returns a ht of the key being the nt and the value being a set of words to test the inv of the nt
  (define (add&split paths)
    (for ([path (in-treelist paths)])  ;<- adding all the words to test the nt into its set in the ht
      (let ([cur-set (hash-ref nts-hash (path-with-hash-path-starting-state path))])
        (for ([config (in-list (path-with-hash-config-path path))])   
          (set-add! cur-set (reverse (second config))))) ;<- the word is in reverse in the configs so have to unreverse it

      ;; now gonna go down the list of configs and split them up to add them to the other nt hash sets

      ;; path  ->
      ;; purpose: to break up a given path and add the words into the nt sets
      (define (break-up-path path)
        ;; path (listof symbol) natnum -> 
        ;; purpose: helper function for break-up-path 
        (define (break-up-path-helper cur-path nts-visited-set length-to-drop-word) 
          (if (< (length (path-with-hash-config-path cur-path)) 2)
              null
              (if (and (not (eq? (path-with-hash-path-starting-state cur-path) (first (first (path-with-hash-config-path cur-path)))))
                       (not (set-member? nts-visited-set (first (first (path-with-hash-config-path cur-path))))))                   
                  (begin
                    ;; adding all the configs that are in the path to the nt's set
                    ;(displayln cur-path)
                    ;(displayln length-to-drop-word)
                    (let ([cur-set (hash-ref nts-hash (first (first (path-with-hash-config-path cur-path))))])
                      (for ([config (in-list (rest (path-with-hash-config-path cur-path)))])   
                        (set-add! cur-set (drop (reverse (second config)) length-to-drop-word))))
               
                    (break-up-path-helper (path-with-hash (rest (path-with-hash-config-path cur-path))
                                                          (path-with-hash-word-of-path cur-path)
                                                          (first (first (path-with-hash-config-path cur-path)))
                                                          (path-with-hash-hash cur-path))
                                          (set-add nts-visited-set (first (path-with-hash-config-path cur-path)))
                                          (+ length-to-drop-word (length (second (first (path-with-hash-config-path cur-path)))))))
                    
                  (break-up-path-helper (path-with-hash (rest (path-with-hash-config-path cur-path))
                                                        (path-with-hash-word-of-path cur-path)
                                                        (path-with-hash-path-starting-state cur-path)
                                                        (path-with-hash-hash cur-path))
                                        nts-visited-set
                                        length-to-drop-word))))
        (break-up-path-helper path (set (path-with-hash-path-starting-state path)) (length (second (first (path-with-hash-config-path path))))))
      
      (treelist-map paths break-up-path)
      ))        
  (add&split paths)
  nts-hash)


;; MIGHT HAVE TO GET RID OF THIS FUNCTION (below), BUT WE'LL SEE 

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


;; the difference with this verision of sm-test-invs and the original one is that this one tests the inv with words that reach final states


;; machine . (list state (word -> boolean)) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs-fsa a-machine rep-limit nt-tested #;a-loi inv)
  #;(define a-loi-hash (for/hash ([inv (in-list a-loi)])
                       (values (car inv) (cadr inv))))
  (define finals-set (list->seteq (fsa-getfinals a-machine)))
  (define machine-paths (treelist-filter (λ (x) (set-member? finals-set (first (first (path-with-hash-config-path x)))))
                                         (find-paths a-machine rep-limit)))
  #;(define machine-paths (find-paths a-machine rep-limit))
  #;(define machine-paths (find-paths a-machine REPETITION-LIMIT)) 

  
  ;(define paths-to-finals (get-paths-to-finals machine-paths (sm-finals a-machine)))

  
  ;(define new-rules (extract-rule-set paths-to-finals))

  
  ;(define new-states (extract-state-set new-rules))

  (define all-paths-new-machine machine-paths) ;<- not refact
  
  
  ;; all paths of new-machine
  #;(define new-machine (remove-states-that-do-not-reach-finals a-machine))
  #;(define all-paths-new-machine (find-paths new-machine REPETITION-LIMIT))

  ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose: To return a list of the invarients and the word that causes them not to hold
  ;; Accumulator Invarient: accum = list of lists of words that cause the invarient not to hold
  ;;                                & the state that it doesn't hold for


  
  (define (sm-test-invs-helper all-paths)
    (for/list ([path (in-treelist all-paths)]
               #:do [;(define cache (caddr (car path)))
                     #;(define a-config (list (word-of-path path) cache))
                     (define a-config (list nt-tested (path-with-hash-word-of-path path)))]
               #:when (not (inv (second a-config))))
      #;a-config
      (second a-config)))
  
  #;(if (null? a-loi)
      '()
      (let [(start-pair (hash-ref a-loi-hash (sm-start a-machine) #f))]
        (if (or
             (not (procedure? start-pair)) ;; <- starting inv not given
             (start-pair '())) ;; <- testing if empty holds for starting state's inv 
            (sm-test-invs-helper all-paths-new-machine)
            (cons (list '() (sm-start a-machine))
                  (sm-test-invs-helper all-paths-new-machine)))))
  (list nt-tested (sm-test-invs-helper all-paths-new-machine)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is the old one
(define (rg-test-invs-andres a-rg a-loi)
  (define inv-nts (map (λ (inv-pair) (first inv-pair)) a-loi)) ;; <- all nts that need to be tested
  (define state-fsas
    (map (lambda (start) (list start (rg->state-ndfa a-rg start))) (filter (λ (nt) (member nt inv-nts))  (rg-getnts a-rg))))
     ;(displayln (map (λ (x) (sm-graph (second x))) state-fsas))
    (define (test-rgs rg-lst)
      (cond [(null? rg-lst)
             '()]
            [else
             (define rg-pair (car rg-lst))
             (cons (sm-test-invs-fsa (cadr rg-pair) 1 (car rg-pair) (cadr (assoc (car rg-pair) a-loi)))
                   (test-rgs (cdr rg-lst)))]))
    (filter (λ (x) (not (empty? (second x)))) (test-rgs state-fsas)))


;; this is the new one 
(define (rg-test-invs a-rg a-loi)
  (define a-loi-hash (for/hash ([inv (in-list a-loi)])
                       (values (car inv) (cadr inv))))
  ;; turning the rg into an fsa
  (define rg-machine (rg->state-ndfa a-rg (grammar-start a-rg)))

  ;; gettng ht of nts and words to test them with
  (define test-word-ht (split-paths (find-paths rg-machine REPETITION-LIMIT) (sm-states rg-machine)))
  
  ;;testing each nt in ht with the test words

  ;; nt (setof word) -> (listof (list nt (listof word))
  ;; purpose: to return a list of the nt and the words that fail (returns empty if list of fail words empty
  (define (test-nt-inv nt sow)
    (define nt-inv (hash-ref a-loi-hash nt #t))
    (define fail-word-set (mutable-set)) ;<- set of all the words that cause the inv to not hold

    ;; every word that the inv doesn't hold for gets put into the set
    (for ([word (in-set sow)]
          #:when (not (nt-inv word)))
      (set-add! fail-word-set word))

    (define fail-word-list (set->list fail-word-set))

    (if (empty? fail-word-list)
        null
        (list nt fail-word-list))
    )
  (for/list ([(nt sow) (in-hash test-word-ht)]
             #:do [(define test-result (test-nt-inv nt sow))]
             #:when (and (not (empty? test-result))
                         (hash-has-key? a-loi-hash nt)))
    test-result)
  )




;; syntactic categories
;;  S = words that start with aa, starting nonterminal
;;  A = words that start with a
;;  B = words that start with either a or b
(define rg-STARTS-WITH-aa (make-rg '(S A B) 
                                   '(a b) 
                                   '((S -> aA)
                                     (A -> a)
                                     (A -> aB)
                                     (B -> a)
                                     (B -> aB)
                                     (B -> b)
                                     (B -> bB))
                                   'S))

(define rg-STARTS-WITH-aa-buggy (make-rg '(S B) 
                                   '(a b) 
                                   '((S -> aB)
                                     (B -> a)
                                     (B -> aB)
                                     (B -> b)
                                     (B -> bB))
                                   'S))

;(check-derive? rg-STARTS-WITH-aa-buggy '(a a) '(a a b a) '(a a a) '(a a b b b))
;(check-not-derive? rg-STARTS-WITH-aa-buggy '(b) '(b a) '(a))

;; invariants

;; word -> boolean
;;purpose: to determine if a would ought to be generated by S
(define (S-INV w)
  (<= 2 (length (takef w (λ (x) (eq? 'a x))))))

;; word -> boolean
;;purpose: to determine if a would ought to be generated by A
(define (A-INV w)
  (<= 1 (length (takef w (λ (x) (eq? 'a x))))))

;; word -> boolean
;;purpose: to determine if a would ought to be generated by B
(define (B-INV w)
  (equal? w (takef w (λ (x) (or (eq? 'b x)
                                (eq? 'a x))))))

  
(rg-test-invs-andres rg-STARTS-WITH-aa #;2 (list (list 'S S-INV #;(lambda (x) #t))
                                          (list 'A A-INV #;(lambda (x) #t))
                                          (list 'B B-INV #;(lambda (x) #t))))


(rg-test-invs-andres rg-STARTS-WITH-aa #;2 (list (list 'S (lambda (x) #f))
                                          (list 'A (lambda (x) #f))
                                          (list 'B (lambda (x) #f))))




(define ba* (make-rg '(S A B) 
                                   '(a b) 
                                   '((S -> bA)
                                     (A -> a)
                                     (A -> aA))
                                   'S))



;; this is to order the output to be the list of the list of the state and the words it doesn't hold for:
;   '(((a) A) ((b a) A) ((a b a) A) ((b b a) A) ((a b b a) A))

;; the results is a list of lists, and these lists are the word and the state
#;(define (order-output results)
    ;; have to get the states of the results
    (define state-set (mutable-set))
  
    (for ([pair (in-list results)])
      (set-add! state-set (second pair)))  ;<-- adds all the states in the set

    ;; making the lists for the states
  
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
  
    (define states-&-empty-low (build-list-of-states-&-empty-low (set->list state-set)))

    ;; now adding in all the words to the lists

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
                                           (make-low (remove-duplicates
                                                      (filter (λ (x) (eq? (car (car states-&-empty-low)) (cadr x)))
                                                              listof-all-words-&-states)))) accum))))
            
      (sort-words-helper states-&-empty-low '()))

    (sort-words results))