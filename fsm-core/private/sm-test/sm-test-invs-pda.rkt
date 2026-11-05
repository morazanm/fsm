#lang racket/base
(#%declare #:unsafe)
;; working on optimizations

(provide sm-test-invs-pda)
;(provide sm-test-invs-pda find-paths #;sm-all-possible-words)
(provide (all-defined-out))
(require (for-syntax racket/base
                     racket/set)
         racket/require
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
            (cond #;[(regexp-match #rx"^unsafe-fxvector" name) (regexp-replace #rx"unsafe-" name "")]
                  [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
                  [(regexp-match #rx"^unsafe-cons-list" name) "cons"]
                  #;[(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          racket/unsafe/ops)
         racket/performance-hint
         racket/list
         racket/set
         data/queue
         "../sm-getters.rkt"
         "../constants.rkt")



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

(struct stack (elems len) #:transparent)

(define-inline (unsafe-stack-elems a-path)
  (unsafe-struct*-ref a-path 0))

(define-inline (unsafe-stack-len a-path)
  (unsafe-struct*-ref a-path 1))

;; a PATH is a structure: (PATH (listof rule) (listof symbol)
(struct PATH (lor stack word path-length destination-state) #:transparent)

(define-inline (unsafe-PATH-lor a-path)
  (unsafe-struct*-ref a-path 0))

(define-inline (unsafe-PATH-stack a-path)
  (unsafe-struct*-ref a-path 1))

(define-inline (unsafe-PATH-word a-path)
  (unsafe-struct*-ref a-path 2))

(define-inline (unsafe-PATH-path-length a-path)
  (unsafe-struct*-ref a-path 3))

(define-inline (unsafe-PATH-destination-state a-path)
  (unsafe-struct*-ref a-path 4))

;; AUXILARY FUNCTIONS FOR PDA RULES
;;     pda rule: ((Source read pop) (Destination push))


;; a PDA-rule is a structure: (make-PDA-rule state symbol (list symbol) state (list symbol))
(struct PDA-rule (source read pop destination push push-len pop-length) #:transparent)

(define-inline (unsafe-PDA-rule-source a-path)
  (unsafe-struct*-ref a-path 0))

(define-inline (unsafe-PDA-rule-read a-path)
  (unsafe-struct*-ref a-path 1))

(define-inline (unsafe-PDA-rule-pop a-path)
  (unsafe-struct*-ref a-path 2))

(define-inline (unsafe-PDA-rule-destination a-path)
  (unsafe-struct*-ref a-path 3))

(define-inline (unsafe-PDA-rule-push a-path)
  (unsafe-struct*-ref a-path 4))

(define-inline (unsafe-PDA-rule-push-len a-path)
  (unsafe-struct*-ref a-path 5))

(define-inline (unsafe-PDA-rule-pop-length a-path)
  (unsafe-struct*-ref a-path 6))

;; pda rule: ((Source read pop) (Destination push))

;; a PDA-rule is a structure: (make-PDA-rule state symbol (list symbol) state (list symbol) natnum)

;; pda-rule -> PDA-rule
;; Purpose: To turn the given pda rule into PDA-rule structure
(define (rule->PDA-rule rule)
  (PDA-rule (car (car rule))
            (cadr (car rule))
            (caddr (car rule))
            (car (cadr rule))
            (cadr  (cadr  rule))
            (if (eq? EMP (cadr (cadr rule)))
                0
                (length (cadr (cadr rule))))
            (if (eq? EMP (caddr (car rule)))
                0
                (length (caddr (car rule))))))

(struct config (state word stack) #:transparent)

(define-inline (unsafe-config-state a-config)
  (unsafe-struct*-ref a-config 0))

(define-inline (unsafe-config-word a-config)
  (unsafe-struct*-ref a-config 1))

(define-inline (unsafe-config-stack a-config)
  (unsafe-struct*-ref a-config 2))

(define (pred a-stack next-rules)
  (or (eq? (unsafe-PDA-rule-pop next-rules) EMP)
      (and (fx<= (PDA-rule-pop-length next-rules) (stack-len a-stack))
           (equal? (take (unsafe-stack-elems a-stack)
                         (unsafe-PDA-rule-pop-length next-rules))
                   (unsafe-PDA-rule-pop next-rules)))))

(struct computation (loc length) #:transparent)

(define-inline (unsafe-computation-loc a-config-path)
  (unsafe-struct*-ref a-config-path 0))

(define-inline (unsafe-computation-length a-config-path)
  (unsafe-struct*-ref a-config-path 1))

(define (always-true-inv word stack)
  #t)
(define (always-true-inv-thunk)
  always-true-inv)

(define (make-new-stack new-rule curr-config-stack)
  (if (eq? (unsafe-PDA-rule-push new-rule) EMP)
      (if (eq? (unsafe-PDA-rule-pop new-rule) EMP)
          curr-config-stack
          (stack (drop (unsafe-stack-elems curr-config-stack)
                       (unsafe-PDA-rule-pop-length new-rule))
                 (fx- (unsafe-stack-len curr-config-stack)
                      (unsafe-PDA-rule-pop-length new-rule))))
      (if (eq? (unsafe-PDA-rule-pop new-rule) EMP)
          (stack (append (unsafe-PDA-rule-push new-rule)
                         (unsafe-stack-elems curr-config-stack))
                 (fx+ (unsafe-stack-len curr-config-stack)
                      (unsafe-PDA-rule-push-len new-rule)))
          (stack (append (unsafe-PDA-rule-push new-rule)
                         (drop (unsafe-stack-elems curr-config-stack)
                               (unsafe-PDA-rule-pop-length new-rule)))
                 (fx+ (unsafe-stack-len curr-config-stack)
                      (fx- (unsafe-PDA-rule-push-len new-rule)
                           (unsafe-PDA-rule-pop-length new-rule)))))))

(define (make-new-word new-rule curr-config-word)
  (if (eq? EMP (unsafe-PDA-rule-read new-rule))
      curr-config-word
      (cons (unsafe-PDA-rule-read new-rule) curr-config-word)))

(define (make-new-config new-rule curr-config-stack curr-config-word)
  (config (unsafe-PDA-rule-destination new-rule)
          (make-new-word new-rule curr-config-word)
          (make-new-stack new-rule curr-config-stack)))

;; pda -> (listof PATH)
;; Purpose: To return all the paths in the given pda 
(define (find-paths-pda a-machine #:max-length [max-length 12])
  (define queue (make-queue))
  (define visited (mutable-set))
  (define tested (mutable-set))
  (define rule-ht
    (for/hasheq ([state (in-set pda-states)])
      (values state
              (for/list ([a-rule (in-set pda-rules)]
                         #:when (eq? state (unsafe-PDA-rule-source a-rule)))
                a-rule))))

  (define (test-accepting-computation a-path accum)
    (cond [(null? a-path)
           accum]
          [else
           (cond [(set-member? tested (car a-path))
                  (test-accepting-computation (cdr a-path) accum)]
                 [else
                  (set-add! tested (car a-path))
                  (define unreversed-word (reverse (unsafe-config-word (car a-path))))
                  (cond [((hash-ref inv-ht
                                    (unsafe-config-state (car a-path))
                                    always-true-inv-thunk)
                          unreversed-word
                          (unsafe-stack-elems (unsafe-config-stack (car a-path))))
                         (test-accepting-computation (cdr a-path) accum)]
                        [else
                         (test-accepting-computation (cdr a-path)
                                                     (cons (list unreversed-word
                                                                 (unsafe-stack-elems (unsafe-config-stack (car a-path)))
                                                                 (unsafe-config-state (car a-path)))
                                                           accum))])])]))

  (define (enqueue-new-configs a-computation next-rules accum)
    (define a-config (car (unsafe-computation-loc a-computation)))
    (define new-config
      (make-new-config (car next-rules)
                       (unsafe-config-stack a-config)
                       (unsafe-config-word a-config)))
    (cond [(set-member? visited new-config)
           (found-new-config a-computation (cdr next-rules) accum)]
          [else
           (set-add! visited new-config)
           (enqueue! queue (computation
                            (cons new-config
                                  (unsafe-computation-loc a-computation))
                            (fx+ 1 (unsafe-computation-length a-computation))))
           (found-new-config a-computation (cdr next-rules) accum)]))

  (define (found-new-config a-computation next-rules accum)
    (cond [(null? next-rules) (find-configs a-computation next-rules accum)]
          [(pred (unsafe-config-stack (car (unsafe-computation-loc a-computation))) (car next-rules))
           (enqueue-new-configs a-computation next-rules accum)]
          [else
           (found-new-config a-computation (cdr next-rules) accum)]))

  (define (searching-for-new-config a-computation next-rules accum)
    (define qfirst (car (unsafe-computation-loc a-computation)))
    (cond [(null? next-rules)
           (cond [(and (set-member? pda-finals (unsafe-config-state qfirst))
                       (null? (unsafe-stack-elems (unsafe-config-stack qfirst))))
                  (find-configs a-computation next-rules (test-accepting-computation (unsafe-computation-loc a-computation) accum))]
                 [else (find-configs a-computation next-rules accum)])]
          [(pred (unsafe-config-stack qfirst) (car next-rules))
           (enqueue-new-configs a-computation next-rules accum)]
          [else (searching-for-new-config a-computation (cdr next-rules) accum)]))
  
  ;; (queueof (listof PATH)) (listof PATH) -> (listof PATH)
  ;; Purpose: To return all the paths of the given machine
  ;; Accumulator invariant: paths = list of current paths
  ;;                        visited = set of a state, word, and stack that has
  ;;                                  been visited
  (define (find-configs curr-computation next-rules accum)
    (cond [(queue-empty? queue)
           accum]
          [else
           (define qfirst (dequeue! queue))
           (define another-qfirst (car (unsafe-computation-loc qfirst)))
           (cond [(fx= max-length (unsafe-computation-length qfirst))
                  (cond [(and (null? (unsafe-stack-elems (unsafe-config-stack another-qfirst)))
                              (set-member? pda-finals (unsafe-config-state another-qfirst)))
                         (find-configs curr-computation next-rules (test-accepting-computation (unsafe-computation-loc qfirst) accum))]
                        [else
                         (find-configs curr-computation next-rules accum)])]
                 [else
                  (searching-for-new-config qfirst
                                            (hash-ref rule-ht (unsafe-config-state another-qfirst))
                                            accum)])]))
  
  (begin
    (for/list ([i rules]
               #:when (and (eq? 'ε (get-pop i)) ;<-- need to make sure that the rule can be applied to an empty stack to start off
                           (eq? (get-source-state i) (sm-start a-machine))))
      (enqueue! queue (make-PATH (list i) ;<- current lor of the path
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
(define (get-accepting-paths a-pda max-length)
  (define finals-set (list->set (sm-finals a-pda)))
  (define paths-that-end-in-finals (for/list ([i (find-paths-pda a-pda #:max-length max-length)]
                                              #:when (set-member? finals-set (PATH-destination-state i)))
                                     (make-PATH (reverse (PATH-lor i)) (PATH-stack i) (PATH-word i) (PATH-path-length i) (PATH-destination-state i))))

  ;; PATH -> Boolean
  ;; Purpose: To determine if the given path leads to an accept 
  (define (leads-to-accepting? a-path)
    (empty? (PATH-stack a-path)))

  ;; PATH -> (listof PATH)
  ;; Purpose: To return all the sub paths of the given
  ;;          path, including the given path
  (define (get-sub-paths a-path)

    (define a-m-set (mutable-set))
    ;; (listof pda-rules) -> (listof symbol)
    ;; Purpose: To get the stack of the given path
    (define (get-stack path-lor)
      ;; (listof pda-rules) (listof symbol) -> (listof symbol)
      ;; Purpose: To get the stack of the given path
      (define (get-stack-helper rules-left cur-stack)
        (if (empty? rules-left)
            cur-stack
            (get-stack-helper (rest rules-left)
                              (append (if (eq? 'ε (get-push (first rules-left)))
                                          '()
                                          (get-push (first rules-left)))
                                      (drop cur-stack
                                            (PDA-rule-pop-length (first rules-left)))))))
      (get-stack-helper path-lor '()))
    (define (get-word a-path)
      (for/fold ([accum '()])
                ([rule a-path]
                 #:when (not (eq? 'ε (get-elem-read rule))))
        (append accum (list (get-elem-read rule)))))
    
    ;; number (listof PATH) -> (setof PATH)
    ;; Purpose: To return all the sub paths of the given
    ;;          path, including the given path
    (define (get-sub-paths-helper length-cur-path)
      (let [(new-lor (take (PATH-lor a-path) length-cur-path))]
        (cond [(= length-cur-path (PATH-path-length a-path))
               (set-add! a-m-set (make-PATH '()
                                            (get-stack (take (PATH-lor a-path) length-cur-path))  ;;;<- the lor and path length is set to 0 
                                            (get-word (take (PATH-lor a-path) length-cur-path))    ;;   and '() to prevent duplicates from 
                                            0                                                      ;;   other sub paths from other computations
                                            (get-destination-state (last new-lor))
                                            ))
               a-m-set]
              [else
               (set-add! a-m-set (make-PATH '()
                                            (get-stack (take (PATH-lor a-path) length-cur-path))
                                            (get-word (take (PATH-lor a-path) length-cur-path))
                                            0
                                            (get-destination-state (last new-lor))
                                            ))
               (get-sub-paths-helper  (+ 1 length-cur-path))])))
    (get-sub-paths-helper 1))

  (define new-mutable-set (mutable-set))

  (apply (curry set-union! new-mutable-set)
         (for/list ([i paths-that-end-in-finals]
                    #:when (leads-to-accepting? i))
           (get-sub-paths i)))
  ;(display new-mutable-set)
  (set->list new-mutable-set))





;; pda -> pda
;; Purpose: Takes in pda and remove states and rules that can't reach a final state 
(define (remove-states-that-cannot-reach-finals a-pda max-length inv-ht accum)
  (define new-rules (mutable-set))
  (define new-states (mutable-set))

  (for ([path (filter (λ (x) (set-member? finals-set (get-destination-state (first (PATH-lor x))))) ;<- paths that end in finals
                                           (find-paths-pda a-pda #:max-length max-length))])
       (for ([rule (PATH-lor path)])
            (set-add! new-rules rule)
            (set-add! new-states (get-source-state rule))
            (set-add! new-states (get-destination-state rule))))
  (make-unchecked-ndpda (set->list new-states) (sm-sigma a-pda) (sm-gamma a-pda) (sm-start a-pda) (sm-finals a-pda) (map PDA-rule->rule (set->list new-rules))))


;; (listof pda-rule) -> word
;; Purpose: To return a word that is made from the given path
(define (word-of-path a-path)
  (for/fold ([accum '()])
            ([rule (PATH-lor a-path)]
             #:when (not (eq? 'ε (get-elem-read rule))))
    (append accum (list (get-elem-read rule)))))

;; PATH (listof (list state (word -> boolean))) -> Boolean
;; Purpose: To determine if a the invariant for a given path holds
(define (path-inv-not-hold? a-path a-loi)
  (not (invariant-holds? a-loi
                         (PATH-word a-path)
                         (PATH-stack a-path))))

  (define rule-ht
    (for/hasheq ([state (in-list (sm-states a-pda))])
      (values state (for/list ([i (in-list old-rules)]
                               #:when (eq? state (unsafe-PDA-rule-source i)))
                      i))))
  
  (define (enqueue-new-path qfirst next-rules)
    (define new-path-stack
      (make-new-stack (car next-rules) (unsafe-PATH-stack qfirst)))
    (define new-path-word
      (make-new-word (car next-rules) (unsafe-PATH-word qfirst)))
    (define new-config
      (config (unsafe-PDA-rule-destination (car next-rules))
              new-path-word
              new-path-stack))
    (cond [(or (fx< max-length (fx+ 1 (unsafe-PATH-path-length qfirst))) ;<- caps # of paths  
               (set-member? visited new-config))
           (found-paths-helper qfirst (cdr next-rules))]
          [else
           (set-add! visited
                     new-config)
           (enqueue! queue (PATH (cons (car next-rules)
                                       (unsafe-PATH-lor qfirst))
                                 new-path-stack
                                 new-path-word
                                 (fx+ 1 (unsafe-PATH-path-length qfirst))
                                 (unsafe-PDA-rule-destination (car next-rules))))
           (found-paths-helper qfirst (cdr next-rules))]))
  
  (define (found-paths-helper qfirst next-rules)
    (cond [(null? next-rules) (find-paths-helper)]
          [(pred (unsafe-PATH-stack qfirst) (car next-rules))
           (enqueue-new-path qfirst next-rules)]
          [else
           (found-paths-helper qfirst (cdr next-rules))]))
  
  (define (new-find-paths-helper qfirst next-rules)
    (cond [(null? next-rules)
           (cond [(set-member? finals-set (unsafe-PATH-destination-state qfirst))
                  (for ([rule (in-list (unsafe-PATH-lor qfirst))]
                        #:when (not (set-member? new-rules rule)))
                    (set-add! new-rules rule)
                    (set-add! new-states (unsafe-PDA-rule-source rule))
                    (set-add! new-states (unsafe-PDA-rule-destination rule)))
                  (find-paths-helper)]
                 [else (find-paths-helper)])]
          [(pred (unsafe-PATH-stack qfirst) (car next-rules))
           (enqueue-new-path qfirst next-rules)]
          [else (new-find-paths-helper qfirst (cdr next-rules))]))
  
  (define (find-paths-helper)
    (cond [(queue-empty? queue)
           (void)]
          [else
           (define qfirst (dequeue! queue))
           (new-find-paths-helper qfirst (hash-ref rule-ht (unsafe-PATH-destination-state qfirst)))]))
  
  (for ([i (in-list old-rules)]
        #:when (eq? (unsafe-PDA-rule-source i) (sm-start a-pda)))
    (enqueue! queue (PATH (list i) ;<- current lor of the path
                          (if (eq? EMP (unsafe-PDA-rule-push i))  ;;  ⚙️making/adding the first paths to the queue
                              (stack '() 0)
                              (stack (unsafe-PDA-rule-push i) (unsafe-PDA-rule-push-len i)))  ;; <- current stack of the path
                          (if (eq? EMP (unsafe-PDA-rule-read  i))
                              '()                                 ;; <- the current word of the path 
                              (list (unsafe-PDA-rule-read  i)))
                          1    ;<- current length of the path
                          (unsafe-PDA-rule-destination i))))
  (find-paths-helper)
  
  (get-accepting-paths new-states (sm-start a-pda) finals-set new-rules max-length inv-ht accum))

;; notes to get sm-possible-words to work propertly now:
;; - paths now have a word
;; - rules are now in a structure
;; - the paths returned by find-paths are reversed




;; pda (listof (list state (word -> boolean))) -> (listof (word stack state))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs-pda a-machine max-path-length ds-remove? a-loi)
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine (if ds-remove?
                          (remove-states-that-cannot-reach-finals a-machine max-path-length)  ;<- refactored
                          a-machine))
  ;; list of invariants that are reachable from the starting configuration
  (define reachable-inv (for/list ([i a-loi]
                                   #:when (member (first i) (sm-states new-machine)))
                          i))
  ;; all accepting paths of new-machine
  (define all-paths-new-machine (get-accepting-paths new-machine max-path-length))
  ;; (listof PATH) (listof (word stack state)) -> (listof (word stack state))
  ;; Purpose: To return a list of the invariants and the word that causes them not to hold
  ;; Accumulator Invariant: accum = list of lists of words and stacks that cause the invarient not to hold
  ;;                                & the state that it doesn't hold for
  (define (sm-test-invs-helper all-paths accum)
    (if (empty? all-paths)
        accum
        (sm-test-invs-helper (rest all-paths)
                             (if (path-inv-not-hold? (first all-paths)
                                                     (for/list ([i (map list-pair->inv-pair reachable-inv)] ; <- reachable inv pair structures
                                                                #:when (eq? (PATH-destination-state (first all-paths))
                                                                            (inv-pair-state i)))
                                                       i))
                                 (cons (list (PATH-word (first all-paths))
                                             (PATH-stack (first all-paths))
                                             (PATH-destination-state (first all-paths))) accum)
                                 accum))))
          
  (order-output (sm-test-invs-helper all-paths-new-machine
                       (let [(start-pair (assoc (sm-start a-machine) a-loi))]
                         (if (or (not (pair? start-pair))
                                 ((second start-pair) '() '()))
                             '()
                             (list (list '() '() (sm-start a-machine))))))))






;; this part organized the output:

;; '(((a) (a) S) (() () S))


;; the results is a list of lists, and these lists are the word and the state
(define (order-output results)
  ;; have to get the states of the results
  (define state-set (mutable-set))
  
  (for ([triple (in-list results)])
    (set-add! state-set (third triple)))  ;<-- adds all the states in the set

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

  ;; now adding in all the words and stacks to the lists

  ;; (listof (listof word state)) -> (listof (listof state (listof word)))
  ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
  (define (sort-words listof-all-words-&-states)
    ;; (listof (word state)) -> (listof word)
    ;; Purpose: To make the given (listof (word state)) into a list of words
    (define (make-low list-of-words-&-stacks-&-states)
      ;; (listof (word state)) -> (listof word)
      ;; Purpose: To make the given (listof (word state)) into a list of words
      ;; Accumulator Invariant: accum = current list of words for the state
      (define (make-low-helper list-of-words-&-states accum)
        (if (null? list-of-words-&-states) accum
            (make-low-helper (cdr list-of-words-&-states) (cons (list (car (car list-of-words-&-states))
                                                                      (cadr (car list-of-words-&-states))) accum))))
      (make-low-helper list-of-words-&-stacks-&-states '()))
    ;; (listof (listof word state)) -> (listof (listof state (listof word)))
    ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
    ;; Accumulator Invairant: accum = list of states with all the possible words in them 
    (define (sort-words-helper states-&-empty-low accum)
      (if (null? states-&-empty-low) accum
          (sort-words-helper (cdr states-&-empty-low)
                             (cons (list (car (car states-&-empty-low))
                                         (make-low (remove-duplicates
                                                    (filter (λ (x) (eq? (car (car states-&-empty-low)) (caddr x)))
                                                           listof-all-words-&-states)))) accum))))
            
    (sort-words-helper states-&-empty-low '()))

  (sort-words results))




