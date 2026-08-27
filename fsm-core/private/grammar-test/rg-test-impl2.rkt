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
(provide rg-test-invs-impl2)


(define (rg->state-ndfa a-rg new-start)   
  (define old-nts (list->seteq (cons EMP (rg-getnts a-rg))))
  (define new-rules (mutable-set))
  (define new-nts (mutable-seteq))
  
  (define all-rules (rg-getunparsedrules a-rg))

  ;; adds all the given nts rules and new nts to the set
  (define (add-nt-rules nt)
    (cond [(set-member? new-nts nt)
           (void)]
          [else (set-add! new-nts nt)
                (for ([rule (in-list all-rules)]
                      #:when (eq? nt (car rule))) ;<- means nt was just discovered, so add all nts rules
                  (set-add! new-rules rule)
                  (for ([symb (in-list (symbol->fsmlos (caddr rule)))]
                        #:when (set-member? old-nts symb))
                    (when (not (eq? symb EMP))
                      (add-nt-rules symb))))]))
    
  (add-nt-rules new-start)
  
  (define new-rg
    (make-unchecked-rg
     (set->list new-nts)
     (rg-getalphabet a-rg)
     (set->list new-rules)
     new-start))
  (define new-state-ndfa
    (rg->fsa new-rg))
  new-state-ndfa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; OLD VERSION OF SM-TEST-INVS-FSA
(define REPETITION-LIMIT 2)

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
              (path-with-hash (list (list (third rule) (list (second rule)))
                                    (list (sm-start a-machine) '()))  ;<- adding in starting config (start '())
                              (list (second rule))
                              (sm-start a-machine)
                              (hash rule 1))))
  #;(map (lambda (x) (path-with-hash-path x)) (find-paths-helper))
  (treelist-map (find-paths-helper)
                (λ (x) (path-with-hash (reverse (path-with-hash-config-path x))  ;<- NEED THIS IF USING SPLIT-PATHS
                                       (path-with-hash-word-of-path x)
                                       (path-with-hash-path-starting-state x)
                                       (path-with-hash-hash x))))
  #;(treelist-map (find-paths-helper)
                (λ (x) (path-with-hash (path-with-hash-config-path x) 
                                       (reverse (path-with-hash-word-of-path x))
                                       (path-with-hash-path-starting-state x)
                                       (path-with-hash-hash x))))) ;<- NEED THIS IF USING 'original' version of sm-test-invs 



;; (listof path) nts -> ht
;; Purpose: returns a hash table of test words for each nt
(define (new-split-paths paths nts finals start-nt)
  ;; making hash table for the nt and all the words that are gonna be used to test them
  (define nts-hash (for/hash ([nt (in-list nts)])
                     (values nt (mutable-set))))

  ;; path -> (void)
  ;; processes a single path
  (define (process-path path)
    ;;last word in the config
    (define last-config-word (reverse (second (last (path-with-hash-config-path path)))))

    ;; adding the words to the test suite of each nt
    (define (add-words path word prev-config-nt)
      (cond [(empty? path)
             null]
            [else ;(displayln path)
                  ;(displayln word)
                  (if (set-member? finals (first (first path)) #;prev-config-nt)
                      null #;(set-add! (hash-ref nts-hash start-nt) word)
                      (set-add! (hash-ref nts-hash prev-config-nt #;(first (first path))) word))
                  (add-words (rest path) (if (empty? word)
                                             word
                                             (rest word)) (first (first path)))]))
    
    ;(displayln (path-with-hash-config-path path))
    ;(displayln last-config-word)
    (set-add! (hash-ref nts-hash start-nt) last-config-word)
    (path-with-hash-config-path path) #;(add-words (rest (path-with-hash-config-path path)) (rest last-config-word) start-nt))


  (treelist-map paths process-path)
  #;nts-hash
  )

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
          (set-add! cur-set (filter (λ (x) (not (eq? 'ε x)))  (reverse (second config)))))) ;<- the word is in reverse in the configs so have to unreverse it

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
                        (set-add! cur-set (filter (λ (x) (not (eq? 'ε x))) (drop (reverse (second config)) length-to-drop-word)))))
               
                    (break-up-path-helper (path-with-hash (rest (path-with-hash-config-path cur-path))
                                                          (path-with-hash-word-of-path cur-path)
                                                          (first (first (path-with-hash-config-path cur-path)))
                                                          (path-with-hash-hash cur-path))
                                          (set-add nts-visited-set (first (path-with-hash-config-path cur-path)))
                                          (+ length-to-drop-word (length (drop (second (first (path-with-hash-config-path cur-path)))
                                                                               length-to-drop-word)))))
                    
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
               #:when (not (inv (filter (λ (x) (not (eq? 'ε x))) (second a-config)))))
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


;; this is the new one 
(define (rg-test-invs-impl2 a-rg a-loi)
  (define a-loi-hash (for/hash ([inv (in-list a-loi)])
                       (values (car inv) (cadr inv))))

  (define inv-nts (map (λ (inv-pair) (first inv-pair)) a-loi));; <- all nts that need to be tested
  ;; turning the rg into an fsa
  (define rg-machine (rg->state-ndfa a-rg (grammar-start a-rg)))

  ;; gettng ht of nts and words to test them with
  ;(define test-word-ht (split-paths (find-paths rg-machine REPETITION-LIMIT) (sm-states rg-machine)))
  (define test-word-ht (new-split-paths (find-paths rg-machine REPETITION-LIMIT)
                                        (rest (sm-states rg-machine))
                                        (sm-finals rg-machine)
                                        (sm-start rg-machine))) ;<- with new-split-paths
  
  #;(displayln test-word-ht)
  ;;testing each nt in ht with the test words

  ;; nt (setof word) -> (listof (list nt (listof word))
  ;; purpose: to return a list of the nt and the words that fail (returns empty if list of fail words empty
  (define (test-nt-inv nt sow)
    (define nt-inv (hash-ref a-loi-hash nt nt))
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
  test-word-ht
  #;(for/list ([(nt sow) (in-hash test-word-ht)]
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

  
(rg-test-invs-impl2 rg-STARTS-WITH-aa #;2 (list (list 'S S-INV #;(lambda (x) #t))
                                          (list 'A A-INV #;(lambda (x) #t))
                                          (list 'B B-INV #;(lambda (x) #t))))


(rg-test-invs-impl2 rg-STARTS-WITH-aa #;2 (list (list 'S (lambda (x) #f))
                                          (list 'A (lambda (x) #f))
                                          (list 'B (lambda (x) #f))))







