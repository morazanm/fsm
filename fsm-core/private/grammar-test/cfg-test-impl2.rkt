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
         #;"../testing-utils/structs.rkt"
         (filtered-in
          (λ (name)
            (cond [(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          "../testing-utils/unsafe-struct-accessors.rkt")
         "../cfg-struct.rkt"
         "../cfg-constructors.rkt"
         "../constants.rkt"
         "../pda.rkt"
         "../misc.rkt"
         "../../../sm-graph.rkt"
         "../sm-getters.rkt"
         "../grammar-getters.rkt"
         racket/treelist
         racket/set
         racket/list
         data/queue
         racket/function)
(provide cfg-test-invs-impl2)


(define (cfg->state-pda a-cfg new-start)
  (define old-nts (list->seteq (cons EMP (cfg-get-v a-cfg))))
  (define new-rules (mutable-set))
  (define new-nts (mutable-seteq))

  (define all-rules (cfg-get-rules a-cfg))
 
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

  (define (found-new-rule rules)
    (cond [(null? rules)
           (find-new-rule (cfg-get-rules a-cfg))]
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
           (found-new-rule (cdr rules))]))
  
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
  #;(set-add! new-nts new-start)
  #;(find-new-rule (cfg-get-rules a-cfg))


  (add-nt-rules new-start) ;;;;;;;

  
  (define new-cfg
    (make-unchecked-cfg
     (set->list new-nts)
     (cfg-get-alphabet a-cfg)
     (set->list new-rules)
     new-start))
  (define new-state-pda
    (cfg->pda new-cfg))
  new-state-pda)


;; a PATH is a structure: (make-PATH (listof rule) (listof symbol)
(define-struct PATH (lor stack word path-length destination-state)#:transparent)

;; AUXILARY FUNCTIONS FOR PDA RULES
;;     pda rule: ((Source read pop) (Destination push))


;; a PDA-rule is a structure: (make-PDA-rule state symbol (list symbol) state (list symbol))
(define-struct PDA-rule (source read pop destination push pop-length) #:transparent)

;; pda-rule -> state
;; Purpose: To get the source state of a pda rule
(define (get-source-state rule)
  (PDA-rule-source rule))

;; pda-rule -> symbol
;; Purpose: To get the element read of a pda rule
(define (get-elem-read rule)
  (PDA-rule-read rule))

;; pda-rule -> (listof symbol) or EMP
;; Purpose: To get the elements to pop from the top of the stack
(define (get-pop rule)
  (PDA-rule-pop rule))

;; pda-rule -> state
;; Purpose: To get the destination state of the pda rule
(define (get-destination-state rule)
  (PDA-rule-destination rule))

;; pda-rule -> (listof symbol) or EMP
;; Purpose: To get the elements to push to the stack
(define (get-push rule)
  (PDA-rule-push rule))




;; pda rule: ((Source read pop) (Destination push))

;; a PDA-rule is a structure: (make-PDA-rule state symbol (list symbol) state (list symbol) natnum)

;; pda-rule -> PDA-rule
;; Purpose: To turn the given pda rule into PDA-rule structure
(define (rule->PDA-rule rule)
  (make-PDA-rule (first (first rule))
                 (second (first rule))
                 (third (first rule))
                 (first (second rule))
                 (second (second rule))
                 (if (eq? 'ε (third (first rule)))
                     0
                     (length (third (first rule))))))

;; pda-rule (listof pda-rule) -> (listof pda-rule)
;; Purpose: To return the next pda-rules that can be used from the given pda-rules
(define (get-next-rules #;a-rule dest-state rules-ht)
  #;(hash-ref rules-ht (get-destination-state a-rule))
  (hash-ref rules-ht dest-state))


(define-struct config (state rev-word stack))

;; PATH (listof pda-rule) -> (listof (listof PATH) (listof (listof state word stack)))
;; Purpose: To return a list of paths that comes from the given path and rules
(define (new-paths&visited a-path rules visited nts nt-prime-hash prime-nt-hash max-length)
  ;; (listof pda-rule) (listof PATH) -> (listof (listof PATH) (listof (listof state word stack)))
  ;; Purpose: To return a list of paths that come from the given paths and rules
  ;; Accumulator invariant: accum = list of paths
  (define (new-paths-helper next-rules accum #;new-visited)
    (cond [(empty? next-rules) (list accum visited #;new-visited)]
          [(or (set-member? visited #;new-visited ;<- if we are at same state with same word & stack in visited or the path is at the max length
                            (make-config (get-destination-state (first next-rules))
                                         (if (eq? 'ε (get-elem-read (first next-rules)))
                                             (PATH-word a-path)
                                             (cons (get-elem-read (first next-rules)) (PATH-word a-path)))
                                         (append (if (eq? 'ε (get-push (first next-rules)))     ;;<- makes sure that we're not at the 
                                                     '()                                        ;; same state with the same word and
                                                     (get-push (first next-rules)))             ;; stack again (no double doing work),
                                                 (drop (PATH-stack a-path)                      ;; making sure the new word and stack at 
                                                       (PDA-rule-pop-length (first next-rules)) ;; the state has not been visited already
                                                       ))))
               (< max-length                                                                         
                  (+ 1 (PATH-path-length a-path))))  ;<- caps # of paths  
           (new-paths-helper (rest next-rules) accum)]
          [else (let* [(new-path-word (if (eq? 'ε (get-elem-read (first next-rules)))
                                          (PATH-word a-path)
                                          (cons (get-elem-read (first next-rules)) (PATH-word a-path)))) ;<- word is reversed
                       

                       (new-path-stack-config (append (if (eq? 'ε (get-push (first next-rules)))
                                                          '()
                                                          (get-push (first next-rules)))
                                                      (if (set-member? nts (first (PATH-stack a-path))) 
                                                          (list (hash-ref prime-nt-hash (first (PATH-stack a-path)))) ;<- adding the nt prime in stack if popped nt
                                                          '())
                                                      (drop #;(PATH-stack a-path) (third (first (PATH-lor a-path)))               
                                                            (PDA-rule-pop-length (first next-rules)))
                                                      )) 

                       (new-path-word-config (cond [(set-member? nts (third (first (PATH-lor a-path)))) (let [(stack-nt (third (first (PATH-lor a-path))))]
                                                                                                          (append (get-pop (first next-rules)) (second (first (PATH-lor a-path)))))]; <- nt is popped its added to ci
                                                   [(hash-ref nt-prime-hash (first (PATH-stack a-path)) #f) (append (second (first (PATH-lor a-path))) (first (PATH-stack a-path)))]
                                                   [else new-path-word]))

 
                       (new-path-stack (append (if (eq? 'ε (get-push (first next-rules)))
                                                   '()
                                                   (get-push (first next-rules)))
                                               (drop (PATH-stack a-path)                      
                                                     (PDA-rule-pop-length (first next-rules))
                                                     ))) ;<- word is reversed
                       
                       
                       (new-path-rules (cons (list (if (set-member? nts (first (get-pop (first next-rules)))) ;; <- now its going to be a config of the cur state & word (no stack)
                                                       (first (get-pop (first next-rules))) ;<- nt is getting popped off the stack, so that the one thats gonna be in the config 
                                                       (first (first (PATH-lor a-path)))) ;<- still producing words for the prev nt
                                                   new-path-word-config
                                                   new-path-stack-config
                                                   )
                                             (PATH-lor a-path)))] ;<- the paths are in reverse
                  
                  (begin (displayln (make-PATH new-path-rules
                                               new-path-stack
                                               new-path-word
                                               (+ 1 (PATH-path-length a-path))
                                               (get-destination-state (first next-rules))))
                         (display new-path-word-config) (displayln new-path-stack-config)
                         (displayln "")
                         (set-add! visited
                                   (make-config (get-destination-state (first next-rules))
                                                new-path-word ;<- the word is in reverse
                                                new-path-stack))
                         (new-paths-helper (rest next-rules)
                                           (cons (make-PATH new-path-rules
                                                            new-path-stack
                                                            new-path-word
                                                            (+ 1 (PATH-path-length a-path))
                                                            (get-destination-state (first next-rules)))
                                                 accum
                                                 )))
                  )]
          ))
  (new-paths-helper rules '()))




;; pda -> (listof PATH)
;; Purpose: To return all the paths in the given pda 
(define (find-paths-pda a-machine start-nt set-nts #:max-length [max-length 6])
  (define queue (make-queue))
  (define rules (map rule->PDA-rule (sm-rules a-machine)))

  (define nt->primes-ht (for/hasheq ([nt set-nts])  ;<- making a ht of the key being the nt's prime and the value is the original nt
                          (let* [(nt-string (symbol->string nt))
                                 (nt-prime (string->symbol (string-append nt-string nt-string)))]
                            (values nt-prime nt))))

  (define prime->nt-ht (for/hasheq ([nt set-nts])
                         (let* [(nt-string (symbol->string nt))
                                (nt-prime (string->symbol (string-append nt-string nt-string)))]
                           (values nt nt-prime))))

  
  ;; (queueof (listof PATH)) (listof PATH) -> (listof PATH)
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invariant: paths = list of current paths
  ;;                        visited = set of a state, word, and stack that has
  ;;                                  been visited
  (define (find-paths-helper paths visited)
    (if (queue-empty? queue) (map (λ (x) (make-PATH (PATH-lor x)
                                                    (PATH-stack x)
                                                    (reverse (PATH-word x))  ;<- have to unreverse the word
                                                    (PATH-path-length x)
                                                    (PATH-destination-state x))) paths)
        (let* [(qfirst (dequeue! queue))
               ;; hash table of the state (<- the key) and the rules that COME OUT of that state (the source state is that state) ;; need to change when reverse paths      
               (rule-ht (for/hasheq ([state (sm-states a-machine)])
                          (values state (for/list ([i rules]
                                                   #:when (and (or (eq? (get-pop i) 'ε)
                                                                   (and (<= (PDA-rule-pop-length i) (length (PATH-stack qfirst)))
                                                                        (equal? (take (PATH-stack qfirst)
                                                                                      (PDA-rule-pop-length i));; <- this part makes sure that we the rules are able to be applied bc 
                                                                                (get-pop i))))
                                                               (eq? state (get-source-state i))))
                                          i))))
               (next-rules-first-path (get-next-rules (PATH-destination-state qfirst)
                                                      rule-ht))
               (paths-with-qfirst (cons qfirst paths))] ;; <-- the paths with the first of the queue included (the new accumulated paths)
          (if (empty? next-rules-first-path)
              (find-paths-helper paths-with-qfirst
                                 visited)
              (begin (map (λ (x) (enqueue! queue x)) (first (new-paths&visited qfirst next-rules-first-path visited set-nts nt->primes-ht prime->nt-ht max-length)))
                     (find-paths-helper paths-with-qfirst
                                        (second (new-paths&visited qfirst next-rules-first-path visited set-nts nt->primes-ht prime->nt-ht max-length))))
              ))))
  
  (begin
    (for/list ([i rules]
               #:when (and (eq? 'ε (get-pop i)) ;<-- need to make sure that the rule can be applied to an empty stack to start off
                           (eq? (get-source-state i) (sm-start a-machine))))
      (enqueue! queue (make-PATH #;(list i)(list (list start-nt '() (list start-nt)) (list start-nt '() '())) ;<- current lor of the path
                                 (if (eq? 'ε (get-push i))  ;;  ⚙️making/adding the first paths to the queue
                                     '()
                                     (get-push i))  ;; <- current stack of the path
                                 (if (eq? 'ε (get-elem-read i))
                                     '()                                 ;; <- the current word of the path 
                                     (list (get-elem-read i)))
                                 1    ;<- current length of the path
                                 (get-destination-state i))))
    
    (find-paths-helper '() (mutable-set)) ))



;; pda -> (listof PATH)
;; Purpose: Returns all the paths that lead to an accepting word of the given machine
(define (get-accepting-paths a-pda start-nt set-nts max-length)
  (define finals-set (list->set (sm-finals a-pda)))
  (define paths-that-end-in-finals (for/list ([i (find-paths-pda a-pda start-nt set-nts #:max-length max-length)]
                                              #:when (and (empty? (PATH-stack i))
                                                          (set-member? finals-set (PATH-destination-state i))))
                                     (make-PATH (reverse (PATH-lor i)) (PATH-stack i) (PATH-word i) (PATH-path-length i) (PATH-destination-state i))))

  paths-that-end-in-finals)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (define last-config-word (PATH-word path))

    ;; adding the words to the test suite of each nt
    (define (add-words path word prev-config-nt)
      (cond [(empty? path)
             null]
            [else ;(displayln path)
             ;(displayln word)
             (if (set-member? finals prev-config-nt)
                 (set-add! (hash-ref nts-hash start-nt) (filter (λ (x) (not (eq? x 'ε))) word))
                 (set-add! (hash-ref nts-hash (first (first path))) (filter (λ (x) (not (eq? x 'ε))) word)))
             (add-words (rest path) (if (empty? word)
                                        word
                                        (rest word)) (first (first path)))]))
    
    ;(displayln (path-with-hash-config-path path))
    ;(displayln last-config-word)
    (set-add! (hash-ref nts-hash start-nt) (filter (λ (x) (not (eq? x 'ε))) last-config-word))
    (add-words (rest (PATH-lor path)) (if (empty? last-config-word)
                                          last-config-word
                                          (rest last-config-word)) start-nt))


  (map process-path paths)
  nts-hash
  )


(define MAX-COMP-LENGTH 12)

(define (cfg-test-invs-impl2 a-cfg a-loi)
  (define a-loi-hash (for/hash ([inv (in-list a-loi)])
                       (values (car inv) (cadr inv))))

  (define inv-nts (map (λ (inv-pair) (first inv-pair)) a-loi));; <- all nts that need to be tested
  ;; turning the cfg into a pda
  (define cfg-machine (cfg->state-pda a-cfg (grammar-start a-cfg)))

  ;; gettng ht of nts and words to test them with
  ;(define test-word-ht (split-paths (find-paths cfg-machine REPETITION-LIMIT) (sm-states cfg-machine)))
  (define test-word-ht (new-split-paths (get-accepting-paths cfg-machine (grammar-start a-cfg)(list->set (grammar-nts a-cfg)) MAX-COMP-LENGTH)
                                        (grammar-nts a-cfg)
                                        (sm-finals cfg-machine)
                                        (grammar-start a-cfg))) ;<- with new-split-paths
  
  ;(displayln test-word-ht)
  ;;testing each nt in ht with the test words

  ;; nt (setof word) -> (listof (list nt (listof word))
  ;; purpose: to return a list of the nt and the words that fail (returns empty if list of fail words empty
  (define (test-nt-inv nt sow)
    (cond [(member nt inv-nts)
           (define nt-inv (hash-ref a-loi-hash nt (lambda (x) #t)))
           (define fail-word-set (mutable-set)) ;<- set of all the words that cause the inv to not hold

           ;; every word that the inv doesn't hold for gets put into the set
           (for ([word (in-set sow)]
                 #:when (not (nt-inv word)))
             (set-add! fail-word-set word))

           (define fail-word-list (set->list fail-word-set))

           (if (empty? fail-word-list)
               null
               (list nt fail-word-list))]
          [else null]))
 
  (for/list ([(nt sow) (in-hash test-word-ht)]
             #:do [(define test-result (test-nt-inv nt sow))]
             #:when (and (not (empty? test-result))
                         (hash-has-key? a-loi-hash nt)))
    test-result))



(define numb>numa
  (make-cfg '(S A)
            '(a b)
            `((S ,ARROW b)
              (S ,ARROW AbA)
              (A ,ARROW AaAbA)
              (A ,ARROW AbAaA)
              (A ,ARROW ,EMP)
              (A ,ARROW bA))
            'S))

#;(cfg-test-invs numb>numa 15 (list (list 'S (lambda (x) #f))
                                    (list 'A (lambda (x) #f))))


;; syntactic categories
;; S: word is a palindrome
(define palindrome
  (make-cfg '(S)
            '(a b)
            `((S ,ARROW ,EMP)
              (S ,ARROW aSa)
              (S ,ARROW bSb)
              (S ,ARROW a)
              (S ,ARROW b))
            'S))

(define (S-INV w)
  (equal? w (reverse w)))



;; Syntactic categories
;; S: word is c^mb^na^nd^m
;; A: word is b^na^n
(define c^mb^na^nd^m
  (make-cfg '(S A)
            '(a b c d)
            `((S -> cSd)
              ;(S -> cAd)
              (S -> A)
              (A -> ,EMP)
              (A -> bAa))
            'S))

;;unit tests 
;(check-derive? c^mb^na^nd^m '() '(c d) '(b a) '(c b a d) '(c c b a d d))
;(check-not-derive? c^mb^na^nd^m '(c) '(a) '(b) '(d) '(c d d) '(a d) '(c b b a d))

;; invariants

;; word -> boolean
;; purpose: to determine if given word ought to be generated by S
(define (S-INV-c^mb^na^nd^m w)
  (let* ([Cs (takef w (λ (x) (eq? 'c x)))]
         [Bs (takef (drop w (length Cs)) (λ (x) (eq? 'b x)))]
         [As (takef (drop w (+ (length Bs) (length Cs))) (λ (x) (eq? 'a x)))]
         [Ds (takef (drop w (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? 'd x)))])
    (and (append Cs Bs As Ds)
         (= (length Cs) (length Ds))
         (= (length Bs) (length As)))))


(define (S-INV-c^mb^na^nd^m-buggy w)
  (let* ([Cs (takef w (λ (x) (eq? 'c x)))]
         [Bs (takef (drop w (length Cs)) (λ (x) (eq? 'b x)))]
         [As (takef (drop w (+ (length Bs) (length Cs))) (λ (x) (eq? 'b x)))]
         [Ds (takef (drop w (+ (length As) (length Bs) (length Cs))) (λ (x) (eq? 'd x)))])
    (and (append Cs Bs As Ds)
         (= (length Cs) (length Ds))
         (= (length Bs) (length As)))))
  

;; word -> boolean
;; purpose: to determine if given word ought to be generated by A
(define (A-INV-c^mb^na^nd^m w)
  (let* ([Bs (takef w (λ (x) (eq? 'b x)))]
         [As (takef (drop w (length Bs)) (λ (x) (eq? 'a x)))])
    (and (append Bs As)
         (= (length Bs) (length As)))))
  


;; syntactic categories:
;; S: word is aibj, where i <= j
;; A: word has equal num as and bs
;; B: word is b*
(define aibj
  (make-cfg '(S A B)
            '(a b)
            `((S -> AB)
              (A -> aAb)
              (A -> ,EMP)
              (B -> bB)
              (B -> ,EMP))
            'S))




(define (order-output results)
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


#;(map order-output (cfg-test-invs c^mb^na^nd^m 15 (list (list 'S S-INV-c^mb^na^nd^m-buggy)
                                                         (list 'A A-INV-c^mb^na^nd^m)
                                                         )))

#;(cfg-test-invs c^mb^na^nd^m 15 (list (list 'S S-INV-c^mb^na^nd^m)
                                       (list 'A A-INV-c^mb^na^nd^m)
                                       ))