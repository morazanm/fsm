#lang racket/base
(require "../../fsm-core/private/constants.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE)
         racket/list
         racket/set
         jobsched
          "default-informative-messages.rkt")
(struct rule (triple pair) #:transparent)

(struct triple (source read pop) #:transparent)

(struct pair (destination push) #:transparent)

(define (empty-rule? a-rule)
  (and (eq? (triple-read (rule-triple a-rule)) EMP)
       (eq? (triple-pop  (rule-triple a-rule)) EMP)
       (eq? (pair-push   (rule-pair a-rule))   EMP)))

(module+ worker
  (define (get-computations a-word lor start finals max-cmps)

  ;;config rule -> config
  ;;Purpose: Applys the given rule to the given config and returns the updated config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-rule a-comp a-rule)
  
    ;;config -> config
    ;;Purpose: Applies the read portion of given rule to the given config
    ;;ASSUMPTION: The given rule can be applied to the config
    #;(define (apply-read a-config)
        (if (eq? (triple-read (rule-triple a-rule)) EMP)
            (config (pair-destination (rule-pair a-rule)) (config-word a-config) (config-stack a-config) (config-index a-config))
            (config (pair-destination (rule-pair a-rule)) (rest (config-word a-config)) (config-stack a-config)
                    (config-index a-config))))
  
    ;;config -> config
    ;;Purpose: Applies the pop portion of given rule to the given config
    ;;ASSUMPTION: The given rule can be applied to the config
    #;(define (apply-pop a-config)
        (if (eq? (triple-pop (rule-triple a-rule)) EMP)
            a-config
            (struct-copy config a-config
                         [stack (drop (config-stack a-config) (length (triple-pop (rule-triple a-rule))))])))
  
    ;;config -> config
    ;;Purpose: Applies the push portion of given rule to the given config
    ;;ASSUMPTION: The given rule can be applied to the config
    #;(define (apply-push a-config)
        (if (eq? (pair-push (rule-pair a-rule)) EMP)
            a-config
            (struct-copy config a-config
                         [stack (append (pair-push (rule-pair a-rule)) (config-stack a-config))])))
  
    ;;config -> config
    ;;Purpose: Updates the config's number if something gets applied to the config (e.i. read/pop/push)
    ;;ASSUMPTION: The given rule can be applied to the config
    #;(define (update-count a-config)
        (if (empty-rule? a-rule)
            a-config
            (struct-copy config a-config [index (add1 (config-index a-config))])))

    (define (apply-rule-helper a-config)
      (let* ([apply-pop-result (if (eq? (triple-pop (rule-triple a-rule)) EMP)
                                   (config-stack a-config)
                                   (drop (config-stack a-config) (length (triple-pop (rule-triple a-rule)))))]
             [apply-push-result (if (eq? (pair-push (rule-pair a-rule)) EMP)
                                    apply-pop-result
                                    (append (pair-push (rule-pair a-rule)) apply-pop-result))]
             [apply-read-result (if (eq? (triple-read (rule-triple a-rule)) EMP)
                                    (config-word a-config)
                                    (rest (config-word a-config)))]
             [update-count-result (if (empty-rule? a-rule)
                                      (config-index a-config)
                                      (add1 (config-index a-config)))])
        (struct-copy config a-config
                     [state (pair-destination (rule-pair a-rule))]
                     [stack apply-push-result]
                     [word apply-read-result]
                     [index update-count-result])))
    
    (struct-copy computation a-comp
                 [LoC (cons (apply-rule-helper (first (computation-LoC a-comp)))
                            (computation-LoC a-comp))]
                 [LoR (cons a-rule (computation-LoR a-comp))]))


  (define visited-configuration-set (mutable-set))

  (define (update-visited a-config)
    (set-add! visited-configuration-set a-config))

  (define computation-number-hash (make-hash))
  
  (define (update-hash a-config a-word)
    (let ([curr-set (hash-ref computation-number-hash a-word #f)])
      (if curr-set
          (set-add! curr-set
                    a-config)
          (hash-set! computation-number-hash
                     a-word
                     (mutable-set)))))
  
  (define finals-set (list->seteq finals))
  
  ;;(listof rules) (queueof computation) (listof computation) number -> (listof computation)
  ;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
  ;;     that are within the bounds of the max computation limit
  (define (make-computations starting-computation)
    (define (make-computations-helper QoC path)
      (if (qempty? QoC)
          (list path computation-number-hash)
          (let* ([curr-config (first (computation-LoC (qfirst QoC)))]
                 [curr-word (config-word curr-config)]
                 [curr-stack (config-stack curr-config)]
                 [curr-state (config-state
                              curr-config)])
            (if (or (and (empty? curr-word)
                         (empty? curr-stack)
                         (set-member? finals-set curr-state))
                    (> (length (computation-LoC (qfirst QoC))) max-cmps))
                (begin
                  (update-hash curr-config curr-word)
                  (make-computations-helper (dequeue QoC) (cons (qfirst QoC) path)))
                #;(let ([new-configs (~>> (lor)
                                          (filter (lambda (rule) (eq? (triple-source (rule-triple rule))
                                                                      curr-state)))
                                          (tee (~>> (filter (lambda (rule)
                                                              (not (empty? curr-word))))
                                                    (filter (λ (rule)
                                                              (eq? (triple-read (rule-triple rule))
                                                                   (first curr-word))))
                                                    (filter (λ (rule)
                                                              (or (eq? (triple-pop (rule-triple rule)) EMP)
                                                                  (and (>= (length curr-stack)
                                                                           (length (triple-pop (rule-triple rule))))
                                                                       (equal? (take curr-stack
                                                                                     (length
                                                                                      (triple-pop (rule-triple rule))))
                                                                               (triple-pop (rule-triple rule))))))))
                                               (~>> (filter (λ (rule)
                                                              (eq? (triple-read (rule-triple rule))
                                                                   EMP)))
                                                    (filter (λ (rule)
                                                              (or (eq? (triple-pop (rule-triple rule)) EMP)
                                                                  (and (>= (length curr-stack)
                                                                           (length (triple-pop (rule-triple rule))))
                                                                       (equal? (take curr-stack
                                                                                     (length
                                                                                      (triple-pop (rule-triple rule))))
                                                                               (triple-pop (rule-triple rule)))))))))
                                          append
                                          (map (λ (rule) (apply-rule (qfirst QoC) rule)))
                                          (filter (λ (new-c) 
                                                    (not (set-member? visited-configuration-set
                                                                      (first (computation-LoC new-c)))))))])
                    (begin
                      (update-hash curr-config curr-word)
                      (update-visited curr-config)
                      (if (empty? new-configs)
                          (make-computations-helper (dequeue QoC) (cons (qfirst QoC) path))
                          (make-computations-helper (enqueue new-configs (dequeue QoC)) path))))
                                           
                (let* ([curr-rules (filter (lambda (rule) (eq? (triple-source (rule-triple rule))
                                                               curr-state))
                                           lor)]
                       ;;(listof rules)
                       ;;Purpose: Holds all rules that consume a first letter in the given configurations
                       [connected-read-rules (filter (λ (rule)
                                                       (and (not (empty? curr-word))
                                                            (eq? (triple-read (rule-triple rule))
                                                                 (first curr-word))))
                                                     curr-rules)]
                       ;;(listof rules)
                       ;;Purpose: Holds all rules that consume no input for the given configurations
                       [connected-read-E-rules (filter (λ (rule)
                                                         (eq? (triple-read (rule-triple rule))
                                                              EMP))
                                                       curr-rules)]
                       ;;(listof rules)
                       ;;Purpose: Holds all rules that can pop what is in the stack
                       [connected-pop-rules (filter (λ (rule)
                                                      (or (eq? (triple-pop (rule-triple rule)) EMP)
                                                          (and (>= (length curr-stack)
                                                                   (length (triple-pop (rule-triple rule))))
                                                               (equal? (take curr-stack
                                                                             (length
                                                                              (triple-pop (rule-triple rule))))
                                                                       (triple-pop (rule-triple rule))))))
                                                    (append connected-read-E-rules connected-read-rules))]
                       [new-configs (filter (λ (new-c) 
                                              (not (set-member? visited-configuration-set
                                                                (first (computation-LoC new-c)))))
                                            (map (λ (rule) (apply-rule (qfirst QoC) rule))
                                                 connected-pop-rules))])
                  (begin
                    (update-hash curr-config curr-word)
                    (update-visited curr-config)
                    (if (empty? new-configs)
                        (make-computations-helper (dequeue QoC) (cons (qfirst QoC) path))
                        (make-computations-helper (enqueue new-configs (dequeue QoC)) path))))))))
    (make-computations-helper (enqueue (list starting-computation) E-QUEUE) '()))
  
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (list (config start a-word '() 0))
                                           '())])
    (make-computations starting-computation)))
  
  (define (run-job jb)
    (get-computations (first (job-data jb))#;a-word
                      (second (job-data jb))#;lor
                      (third (job-data jb))#;start
                      (fourth (job-data jb))#;finals
                      (fifth (job-data jb))#;max-cmps))

  (start-worker run-job))

(module+ main

  )

