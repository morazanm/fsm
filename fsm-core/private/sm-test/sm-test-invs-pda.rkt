#lang racket/base
(#%declare #:unsafe)
;; working on optimizations

;(provide sm-test-invs-pda)
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
;; Purpose: Returns all the paths that lead to an accepting word of the given machine
(define (get-accepting-paths pda-states pda-start pda-finals pda-rules max-length inv-ht accum)
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
  
  (for ([i (in-set pda-rules)]
        #:when (eq? (unsafe-PDA-rule-source i) pda-start))
    (define new-config
      (config (unsafe-PDA-rule-destination i)
              (if (eq? EMP (unsafe-PDA-rule-read i))
                  '()                                 ;; <- the current word of the path 
                  (list (unsafe-PDA-rule-read i)))
              (if (eq? EMP (unsafe-PDA-rule-push i))  ;;  ⚙️making/adding the first paths to the queue
                  (stack '() 0)
                  (stack (unsafe-PDA-rule-push i) (unsafe-PDA-rule-push-len i)))))
    (set-add! visited
              new-config)
    (enqueue! queue (computation
                     (list new-config)
                     1)))
  
  (find-configs #f #f accum))

;; pda -> pda
;; Purpose: Takes in pda and remove states and rules that can't reach a final state 
(define (remove-states-that-cannot-reach-finals a-pda max-length inv-ht accum)
  (define new-rules (mutable-set))
  (define new-states (mutable-seteq))
  (define visited (mutable-set))
  (define queue (make-queue))
  (define finals-set (list->seteq (sm-finals a-pda)))
  (define old-rules
    (for/list ([rule (in-list (sm-rules a-pda))])
      (rule->PDA-rule rule)))

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
(define (sm-test-invs-pda a-machine max-path-length a-loi)
  
  (define inv-ht
    (for/hash ([inv-state-pair (in-list a-loi)])
      (values (car inv-state-pair) (car (cdr inv-state-pair)))))

  (define start-pair (assoc (sm-start a-machine) a-loi))
  (if ((hash-ref inv-ht
                 (sm-start a-machine)
                 always-true-inv-thunk)
       '()
       '())
      (remove-states-that-cannot-reach-finals a-machine max-path-length inv-ht '())
      (remove-states-that-cannot-reach-finals a-machine max-path-length inv-ht (list (list '() '() (sm-start a-machine))))))