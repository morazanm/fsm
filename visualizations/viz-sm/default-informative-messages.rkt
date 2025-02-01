#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         2htdp/image
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/viz-constants.rkt"
         "david-imsg-state.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/misc.rkt")

(provide ndfa-create-draw-informative-message
         pda-create-draw-informative-message)

(define FONT-SIZE 18)

(define DARKGOLDENROD2 (make-color 238 173 14))

;(define INS-TOOLS-BUFFER 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-tape-img tape start-index color-pair)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let ([letter-imgs
         (build-list
          TAPE-SIZE
          (λ (i)
            (if (< (+ start-index i) (length tape))
                (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                               20
                               (cond [(empty? color-pair) 'black]
                                     [(and (not (empty? (first color-pair)))
                                           (< (+ start-index i) (first (first color-pair))))
                                      (second (first color-pair))]
                                     [(and (not (empty? (second color-pair)))
                                           (= (+ start-index i) (first (second color-pair))))
                                      (second (second color-pair))]
                                     [else 'black]))
                         (overlay (square 21 'solid 'white) (square (add1 21) 'solid 'white)))
                (overlay (square 21 'solid 'white) (square (add1 21) 'solid 'white)))))])
    (make-tape-img letter-imgs start-index)))

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rule) #:transparent)

#|
A rule is a structure:
(make-rule triple)
triple is the entire of the ndfa rule
|#
(struct rule (triple pair) #:transparent)


;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

(define qempty? empty?)

(define E-QUEUE '())

;; (qof X) → X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; (listof X) (qof X) → (qof X)
;; Purpose: Add the given list of X to the given
;;          queue of X
(define (enqueue a-lox a-qox)
  (append a-qox a-lox))

;; (qof X) → (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))


(struct computation (LoC LoR visited) #:transparent)


;;config rule -> config
;;Purpose: Applies the given rule to the given config
;;ASSUMPTION: the given rule is applicable to the given config
(define (apply-ndfa-rule config rule)
  (let* ([new-config (list (third rule)
                           (if (eq? (second rule) EMP)
                               (second (first (computation-LoC config)))
                               (rest (second (first (computation-LoC config))))))])
    (struct-copy computation config
                 [LoC (cons new-config (computation-LoC config))]
                 [LoR (cons rule (computation-LoR config))]
                 [visited (cons (first (computation-LoC config)) (computation-visited config))])))



;;(listof symbol) (listof rule) symbol -> (listof computation)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (trace-computations word lor start)
  (let (;;configuration
        ;;Purpose: The starting configuration
        [starting-config (computation (list (append (list start) (list word)))
                                      '()
                                      '())])
    (make-ndfa-computations lor
                       (enqueue (list starting-config) E-QUEUE)
                       '())))


;;(listof rule) -> (listof computation)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (make-ndfa-computations lor QoC path)
  (if (qempty? QoC)
      path
      (let* ([first-computation (first (computation-LoC (qfirst QoC)))]
             ;;(listof rules)
             ;;Purpose: Returns all rules that consume a letter using the given configurations
             [connected-read-rules (if (empty? (second first-computation))
                                       '()
                                       (filter (λ (rule)
                                                 (and (equal? (first rule) (first first-computation))
                                                      (equal? (second rule) (first (second first-computation)))))
                                               lor))]
             ;;(listof rules)
             ;;Purpose: Returns all rules that have an empty transition using the given configurations
             [connected-emp-rules
              (filter (λ (rule)
                        (and (equal? (first rule) (first first-computation))
                             (equal? (second rule) EMP)))
                      lor)]
             ;;(listof configurations)
             ;;Purpose: Makes new configurations using given word and connected-rules
             [new-configs (filter (λ (new-c)
                                    (not (member? (first (computation-LoC new-c)) (computation-visited new-c))))
                                  (map (λ (rule) (apply-ndfa-rule (qfirst QoC) rule))
                                       (append connected-read-rules connected-emp-rules)))])
        (if  (empty? new-configs)
             (make-ndfa-computations lor (dequeue QoC) (cons (qfirst QoC) path))
             (make-ndfa-computations lor (enqueue new-configs (dequeue QoC)) path)))))
  

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-ndfa-trace configs rules acc)
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first rules)) EMP)))
         (let* ([rle (rule (list EMP EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-ndfa-trace (rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (equal? (second (first rules)) EMP))
         (let* ([rle (rule (first rules))]
                [res (struct-copy trace (first acc)
                                  [rule (cons rle (trace-rule (first acc)))])])
           (make-ndfa-trace configs (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first rules))]
                     [res (trace (first configs)
                                 (list rle))])
                (make-ndfa-trace (rest configs) (rest rules) (cons res acc)))]))


;;rule -> boolean
;;Purpose: Determines if the given rule is an empty rule (e.i. reads, pops, and pushes empty)
(define (empty-rule? a-rule)
  (and (equal? (second (first a-rule)) EMP)
       (equal? (third (first a-rule)) EMP)
       (equal? (second (second a-rule)) EMP)))


;;config rule -> config
;;Purpose: Applys the given rule to the given config and returns the updated config
;;ASSUMPTION: The given rule can be applied to the config
(define (apply-rule a-comp a-rule)
  ;;config -> config
  ;;Purpose: Applies the read portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-read a-config)
    (if (equal? (second (first a-rule)) EMP)
        (list (first (second a-rule)) (second a-config) (third a-config) (fourth a-config))
        (list (first (second a-rule)) (rest (second a-config)) (third a-config) (fourth a-config))))
  ;;config -> config
  ;;Purpose: Applies the pop portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-pop a-config)
    (if (equal? (third (first a-rule)) EMP)
        a-config
        (list (first a-config) (second a-config)
              (drop (third a-config) (length (third (first a-rule)))) (fourth a-config))))
  ;;config -> config
  ;;Purpose: Applies the push portion of given rule to the given config
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (apply-push a-config)
    (if (equal? (second (second a-rule)) EMP)
        a-config
        (list (first a-config) (second a-config)
              (append (second (second a-rule)) (third a-config))
              (fourth a-config))))
  ;;config -> config
  ;;Purpose: Updates the config's number if something gets applied to the config (e.i. read/pop/push)
  ;;ASSUMPTION: The given rule can be applied to the config
  (define (update-count a-config)
    (if (empty-rule? a-rule)
        a-config
        (list (first a-config) (second a-config) (third a-config) (add1 (fourth a-config)))))
  (struct-copy computation a-comp
               [LoC (cons (update-count (apply-push (apply-pop (apply-read (first (computation-LoC a-comp))))))
                          (computation-LoC a-comp))]
               [LoR (cons a-rule (computation-LoR a-comp))]
               [visited (cons (first (computation-LoC a-comp)) (computation-visited a-comp))]))

;;word (listof rule) symbol number -> (listof computation)
;;Purpose: Returns all possible computations using the given word, (listof rule) and start symbol
;;   that are within the bounds of the max computation limit
(define (get-computations a-word lor start max-cmps)
  (let (;;computation
        ;;Purpose: The starting computation
        [starting-computation (computation (list (append (list start) (list a-word) (list '()) (list 0)))
                                           '()
                                           '())])
    (make-computations lor
                       (enqueue (list starting-computation) E-QUEUE)
                       '()
                       max-cmps)))


;;(listof rules) (queueof computation) (listof computation) number -> (listof computation)
;;Purpose: Makes all the computations based around the (queueof computation) and (listof rule)
;;     that are within the bounds of the max computation limit
(define (make-computations lor QoC path max-cmps)
  (cond [(qempty? QoC) path]
        [(> (length (computation-LoC (qfirst QoC))) max-cmps)
         (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)]
        [else (let* ([stack (third (first (computation-LoC (qfirst QoC))))]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume a first letter in the given configurations
                     [connected-read-rules (filter (λ (rule)
                                                     (and (not (empty? (second (first (computation-LoC (qfirst QoC))))))
                                                          (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                          (equal? (second (first rule)) (first (second (first (computation-LoC (qfirst QoC))))))))
                                                   lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume no input for the given configurations
                     [connected-read-E-rules (filter (λ (rule)
                                                       (and (equal? (first (first rule)) (first (first (computation-LoC (qfirst QoC)))))
                                                            (equal? (second (first rule)) EMP)))
                                                     lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that can pop what is in the stack
                     [connected-pop-rules (filter (λ (rule)
                                                    (or (equal? (third (first rule)) EMP)
                                                        (and (>= (length stack) (length (third (first rule))))
                                                             (equal? (take stack (length (third (first rule)))) (third (first rule))))))
                                                  (append connected-read-E-rules connected-read-rules))]
                     [new-configs (filter (λ (new-c) 
                                            (not (member? (first (computation-LoC new-c)) (computation-visited new-c))))
                                          (map (λ (rule) (apply-rule (qfirst QoC) rule)) connected-pop-rules))])
                (if (empty? new-configs)
                    (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)
                    (make-computations lor (enqueue new-configs (dequeue QoC)) path max-cmps)))]))

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-pda-trace configs rules acc)
  (cond [(empty? rules) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first (first rules))) EMP)))
         (let* ([rle (rule (list EMP EMP EMP) (list EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-pda-trace(rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (empty-rule? (first rules)))
         (let* ([rle (rule (first (first rules)) (second (first rules)))]
                [res (struct-copy trace (first acc)
                                  [rule (cons rle (trace-rule (first acc)))])])
           (make-pda-trace (rest configs) (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first (first rules)) (second (first rules)))]
                     [res (trace (first configs) (list rle))])
                (make-pda-trace (rest configs) (rest rules) (cons res acc)))]))

;;(listof symbols) (lisof configurations) -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs a-word configs)
  (append-map (λ (comp)
                (make-inv-configs-helper a-word (reverse (computation-LoC comp)) (length a-word)))
              configs))

;;(listof symbols) (lisof configurations) natnum -> (listof configurations)
;;Purpose: Makes configurations usable for invariant predicates
(define (make-inv-configs-helper a-word configs word-len)
  (let* ([config (filter (λ (config) (= (length (second config)) word-len)) configs)]
         [inv-config (map (λ (config)
                            (append (list (first config))
                                    (list (take a-word (- (length a-word) word-len)))
                                    (list (third config))
                                    (list (fourth config))))
                          config)])
    (if (empty? configs)
        '()
        (append inv-config
                (make-inv-configs-helper a-word (rest configs) (sub1 word-len))))))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration 
(define (get-inv-config-results inv-configs invs)
  (append-map (λ (comp)
                (get-inv-config-results-helper comp invs))
              inv-configs))

;;(listof configurations) (listof (listof symbol ((listof sybmols) -> boolean))) -> (listof configurations)
;;Purpose: Adds the results of each invariant oredicate to its corresponding invariant configuration
(define (get-inv-config-results-helper inv-configs invs)
  (if (empty? inv-configs)
      '()
      (let* ([get-inv-for-inv-config (filter (λ (inv)
                                               (equal? (first inv) (first inv-configs)))
                                             invs)]
             [inv-for-inv-config (if (empty? get-inv-for-inv-config)
                                     '()
                                     (second (first get-inv-for-inv-config)))]
             [inv-config-result (if (empty? inv-for-inv-config)
                                    '()
                                    (list (append inv-configs
                                                  (list (inv-for-inv-config (second inv-configs)
                                                                            (third inv-configs))))))])
        (append inv-config-result
                (get-inv-config-results-helper (rest inv-configs) invs)))))

;;(listof configurations) (listof sybmols) -> (listof configurations)
;;Purpose: Extracts all the invariant configurations that failed
(define (return-brk-inv-configs inv-config-results a-word)
  (remove-duplicates (filter (λ (config) (not (fifth config))) inv-config-results)))

;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (pda-last-fully-consumed a-word M max-cmps)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (second (first config))))
                     (map computation-LoC (get-computations a-word
                                                            (pda-getrules M)
                                                            (pda-getstart M)
                                                            max-cmps))))
         (pda-last-fully-consumed (take a-word (sub1 (length a-word))) M max-cmps)]
        [a-word]))

;;(listof X) (listof X) (listof X) -> (listof X)
;;Purpose: Removes all similiarities between lst1 and lst2
;;Acc = The differences between the previous path and the current path
(define (remove-similarities prev-path curr-path acc)
  (cond [(empty? prev-path) (append acc curr-path)]
        [(empty? curr-path) prev-path]
        [(equal? (first prev-path) (first curr-path))
         (remove-similarities (rest prev-path) (rest curr-path) acc)]
        [(remove-similarities (rest prev-path) (rest curr-path) (append acc (list (first curr-path))))]))

;;(listof rules) -> (listof rules)
;;Purpose: Converts the given (listof configurations)s to rules
(define (configs->rules curr-config)
  (make-rule-triples (remove-duplicates curr-config)))

;;word (listof configurations) (listof configurations) -> (listof configurations)
;;Purpose: Counts the number of unique configurations for each stage of the word
(define (count-computations a-word a-LoC acc)
  ;;word -> number
  ;;Purpose: Counts the number of unique configurations based on the given word
  (define (get-config a-word)
    (length (remove-duplicates
             (append-map (λ (configs)
                           (filter (λ (config)
                                     (equal? a-word (second config)))
                                   configs))
                         a-LoC))))
  (if (empty? a-word)
      (reverse (cons (get-config a-word) acc))
      (count-computations (rest a-word) a-LoC (cons (get-config a-word) acc))))

;;(listof rule-struct) -> (listof rule)
;;Purpose: Remakes the rules extracted from the rule-struct
(define (remake-rules trace-rules)
  (append-map (λ (lor)
                (map (λ (rule)
                       (list (rule-triple rule)
                             (rule-pair rule)))
                     lor))
              trace-rules))



;;X -> X
;;Purpose: Returns X
(define (id x) x)

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

;;(listof symbols) -> string
;;Purpose: Converts the given los into a string
(define (make-edge-label rule)
  (format "\n[~a ~a ~a]" (second (first rule)) (third (first rule)) (second (second rule))))

;;(listof rules)
;;Purpose: Transforms the pda rules into triples similiar to an ndfa 
(define (make-rule-triples rules)
  (map (λ (rule)
         (append (list (first (first rule)))
                 (list (string->symbol (make-edge-label rule)))
                 (list (first (second rule)))))
       rules))

;;(listof trace) (X -> Y) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-X LoT map-func)
  (filter-map-acc empty? map-func not first LoT))

;(listof symbol ((listof symbol) (listof symbol) -> boolean))) (X -> Y) -> (listof symbol ((listof symbol) (listof symbol) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) (listof symbols) -> boolean)))
(define (get-invariants LoI func)
  (filter-map-acc (λ (x) ((second (first x)) (second x) (third x))) first func first LoI))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not id LoT))

;;(listof symbols) (listof configurations) -> (listof configurations)
;;Purpose: Returns the configurations have the given word as unconsumed input
(define (get-portion-configs word full-configs)
  (append-map (λ (config)
                (filter (λ (configs)
                          (equal? (second configs) word))
                        (computation-LoC config)))
              full-configs))

;; (listof configuration) number -> (listof configuration)
;; Purpose: Returns the first configuration in a (listof configuration) if it exceeds the cut-off amount
(define (get-cut-off LoC max-cmps)
  (filter-map (λ (config)
                (and (>= (length config) max-cmps)
                     (first config)))
              LoC))

;; (listof configuration) (listof symbol) -> (listof symbol)
;; Purpose: Returns the most consumed input
(define (get-farthest-consumed LoC acc)
  (cond [(empty? LoC) acc]
        [(< (length (second (first (first LoC)))) (length acc))
         (get-farthest-consumed (rest LoC) (second (first (first LoC))))]
        [else (get-farthest-consumed (rest LoC) acc)]))



;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                     (trace-computations a-word (fsa-getrules M) (fsa-getstart M))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
        [a-word]))

;;(listof trace-rule) -> (listof rules)
;;Purpose: Remakes the rule extracted from the rule-struct
(define (extract-rules trace-rules)
  (map (λ (rule)
         (rule-triple rule))
       trace-rules))


;;(listof trace) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-rule LoT)
  (filter-map-acc empty? trace-rule not first LoT))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (ndfa-create-draw-informative-message imsg-st)
  (let* (

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;boolean
         ;;Purpose: Determines if the the machine should accept or reject the given word
         [machine-decision (if (not (zipper-empty? (imsg-state-acpt-trace imsg-st)))
                               'accept
                               'reject)]
         [FONT-SIZE 20]) 

   (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE 'red)))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'black)
                          (text (format "~a" EMP) FONT-SIZE 'white))))]
            [(and (not (empty? (imsg-state-pci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (eq? machine-decision 'reject))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length (imsg-state-pci imsg-st) ) 'gray)
                                               (list (length (imsg-state-pci imsg-st) ) 'red)))))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (empty? (imsg-state-pci imsg-st))
                          (text "" FONT-SIZE 'black)
                          (make-tape-img (imsg-state-pci imsg-st) 
                                         (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE 'white)
                                       (text "Word: " FONT-SIZE 'black)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE 'black)
                                       (make-tape-img (imsg-state-pci imsg-st)
                                                      (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (list-ref (imsg-state-comps-len imsg-st)
                                               (length (imsg-state-pci imsg-st)))))
             FONT-SIZE
             'brown)
      (cond [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'reject)
                   (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st)))
              (text "All computations end in a non-final state and the machine rejects." FONT-SIZE 'red)]
             [else (text "Word Status: accept " FONT-SIZE 'white)]))
    ))



(define (pda-create-draw-informative-message imsg-st)
  (let* (

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (if (zipper-empty? (imsg-state-stack imsg-st)) 
                            (imsg-state-stack imsg-st)
                            (third (zipper-current (imsg-state-stack imsg-st))))]
         [machine-decision (if (not (zipper-empty? (imsg-state-acpt-trace imsg-st)))
                               'accept
                               'reject)]
         [FONT-SIZE 20])
    (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE 'red)))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'black)
                          (text (format "~a" EMP) FONT-SIZE 'white))))]
            [(and (not (empty? (imsg-state-upci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  ;(<= (length (imsg-state-upci imsg-st)) (length (imsg-state-farthest-consumed imsg-st)))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                         (imsg-state-comps imsg-st)))
             (above/align 'left
                          (beside (text "aaaK" FONT-SIZE 'white)
                                  (text "Word: " FONT-SIZE 'black)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? (imsg-state-pci imsg-st))
                                                     '()
                                                     (list (list (length (imsg-state-pci imsg-st)) 'gray)
                                                           (list (length (imsg-state-pci imsg-st)) DARKGOLDENROD2)))))
                          (beside (text "Consumed: " FONT-SIZE 'black)
                                  (make-tape-img (imsg-state-pci imsg-st)
                                                 (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                     (imsg-state-word-img-offset imsg-st)
                                                     0)
                                                 '())))]
            [(and (not (empty? (imsg-state-pci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (eq? machine-decision 'reject))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length (imsg-state-pci imsg-st)) 'gray)
                                               (list (length (imsg-state-pci imsg-st)) 'red)))))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (empty? (imsg-state-pci imsg-st))
                          (text "" FONT-SIZE 'black)
                          (make-tape-img (imsg-state-pci imsg-st)
                                         (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE 'white)
                                       (text "Word: " FONT-SIZE 'black)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE 'black)
                                       (make-tape-img (imsg-state-pci imsg-st)
                                                      (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (cond [(zipper-empty? (imsg-state-stack imsg-st)) (text "aaaC" FONT-SIZE 'white)]
            [(empty? current-stack) (beside (text "aaak" FONT-SIZE 'white)
                                            (text "Stack: " FONT-SIZE 'black))]
            [else (beside (text "aaak" FONT-SIZE 'white)
                          (text "Stack: " FONT-SIZE 'black)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (if (= (length (imsg-state-pci imsg-st)) (imsg-state-max-cmps imsg-st))
                                        (list-ref (imsg-state-comps-len imsg-st)
                                                  (sub1 (length (imsg-state-pci imsg-st))))
                                        (list-ref (imsg-state-comps-len imsg-st)
                                                  (length (imsg-state-pci imsg-st))))))
            FONT-SIZE
            'brown)
      (cond [(and (not (empty? (imsg-state-upci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                         (imsg-state-comps imsg-st)))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
            [(and (empty? (imsg-state-upci imsg-st))
                  (or (zipper-empty? (imsg-state-stack imsg-st))
                      (zipper-at-end? (imsg-state-stack imsg-st)))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." FONT-SIZE 'forestgreen)]
            [(and (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (or (zipper-empty? (imsg-state-stack imsg-st))
                      (zipper-at-end? (imsg-state-stack imsg-st)))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE 'red)]
            [else (text "Word Status: accept " FONT-SIZE 'white)]))))







;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
