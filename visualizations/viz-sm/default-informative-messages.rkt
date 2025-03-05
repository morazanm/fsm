#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/viz-constants.rkt"
         "david-imsg-state.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/constants.rkt")

(provide ndfa-create-draw-informative-message
         pda-create-draw-informative-message
         tm-create-draw-informative-message
         trace trace-config trace-rules)

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rules) #:transparent)
(struct rule (read action) #:transparent)

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

(define qempty? empty?)

(define E-QUEUE '())

(define FONT-SIZE 20)

(define DARKGOLDENROD2 (make-color 238 173 14))

(define ACCEPT-COLOR (make-color 34 139 34)) ;;forestgreen

(define REJECT-COLOR 'red)

(define REJECT-COMPUTATION-COLOR 'violetred)

(define BLANK-COLOR 'white)

(define FONT-COLOR 'black)

(define COMPUTATION-LENGTH-COLOR 'brown)

(define accessor-func (compose fourth (compose trace-config zipper-current)))
(define ndfa-accessor-func (compose third (compose trace-config zipper-current)))

(define get-index (compose fourth zipper-current))
(define get-index-ndfa (compose third zipper-current))

(define get-next-index (compose fourth (compose zipper-current zipper-next)))
(define get-next-index-ndfa (compose third (compose zipper-current zipper-next)))

(define get-prev-index (compose fourth (compose zipper-current zipper-prev)))
(define get-prev-index-ndfa (compose third (compose zipper-current zipper-prev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (tm-getalphabet m) (m '() 0 'get-alphabet)) 
  
(define (tm-getstates m) (m '() 0 'get-states))
  
(define (tm-getfinals m) (m '() 0 'get-finals))

(define (tm-getdelta m) (m '() 0 'get-delta)) ;;; parsed rules

(define (tm-getrules m) (m '() 0 'get-rules))  ;;; unparsed rules

(define (tm-getstart m) (m '() 0 'get-start))
  
(define (tm-getaccept m) (m '() 0 'get-accept))

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
                (let [(tape-element (list-ref tape (+ start-index i)))]
                  (overlay (text (if (symbol? tape-element)
                                   (symbol->string tape-element)
                                   (number->string tape-element))
                               20
                               (cond [(empty? color-pair) FONT-COLOR]
                                     [(and (not (empty? (first color-pair)))
                                           (< (+ start-index i) (first (first color-pair))))
                                      (second (first color-pair))]
                                     [(and (not (empty? (second color-pair)))
                                           (= (+ start-index i) (first (second color-pair))))
                                      (second (second color-pair))]
                                     [else FONT-COLOR]))
                         (overlay (square 21 'solid BLANK-COLOR) (square (add1 21) 'solid BLANK-COLOR))))
                (overlay (square 21 'solid BLANK-COLOR) (square (add1 21) 'solid BLANK-COLOR)))))])
    (make-tape-img letter-imgs start-index)))


(define (draw-imsg imsg-st)
  (let* [(tape (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)) '(_)
                   (zipper-current (imsg-state-tm-tape imsg-st))))
         (start-index 0 #;(second a-tape))
         (head-pos (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)) -1
                       (zipper-current (imsg-state-tm-head-position imsg-st))))
         (TAPE-SIZE 24)]
    (define (make-tape-img loi start-index)
      (if (empty? (rest loi))
          (above (first loi)
                 (square 5 'solid BLANK-COLOR)
                 (text (number->string start-index) 10 (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                           BLANK-COLOR
                                                           FONT-COLOR)))
          (beside (above (first loi)
                         (square 5 'solid BLANK-COLOR)
                         (text (number->string start-index) 10 (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                   BLANK-COLOR
                                                                   FONT-COLOR)))
                  (make-tape-img (rest loi) (add1 start-index)))))
    (let [(letter-imgs (build-list TAPE-SIZE
                                   (λ (i) (if (< (+ start-index i) (length tape))
                                              (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                             24
                                                             (cond [(zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                    BLANK-COLOR]
                                                                   [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   [else FONT-COLOR])

                                                             #;(if (= i (- head-pos start-index))
                                                                   REJECT-COLOR
                                                                   FONT-COLOR))
                                                       (overlay (square 50 'solid BLANK-COLOR)
                                                                (square (add1 50) 'solid
                                                                        (if (zipper-empty?
                                                                             (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                            BLANK-COLOR
                                                                            FONT-COLOR)
                                                                        #;FONT-COLOR)))
                                              (overlay (text (symbol->string BLANK)
                                                             24
                                                             (cond [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   [(zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                    BLANK-COLOR]
                                                                   [else FONT-COLOR])
                                                             #;(if (= i (- head-pos start-index))
                                                                 REJECT-COLOR
                                                                 FONT-COLOR))
                                                       (square 50 'solid BLANK-COLOR)
                                                       (square (add1 50) 'solid
                                                               (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                 BLANK-COLOR
                                                                 FONT-COLOR) #;FONT-COLOR))))))]
      (make-tape-img letter-imgs start-index))))


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
                                                          (equal? (first (first rule))
                                                                  (first (first (computation-LoC (qfirst QoC)))))
                                                          (equal? (second (first rule))
                                                                  (first (second (first (computation-LoC (qfirst QoC))))))))
                                                   lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that consume no input for the given configurations
                     [connected-read-E-rules (filter (λ (rule)
                                                       (and (equal? (first (first rule))
                                                                    (first (first (computation-LoC (qfirst QoC)))))
                                                            (equal? (second (first rule)) EMP)))
                                                     lor)]
                     ;;(listof rules)
                     ;;Purpose: Holds all rules that can pop what is in the stack
                     [connected-pop-rules (filter (λ (rule)
                                                    (or (equal? (third (first rule)) EMP)
                                                        (and (>= (length stack) (length (third (first rule))))
                                                             (equal? (take stack (length (third (first rule))))
                                                                     (third (first rule))))))
                                                  (append connected-read-E-rules connected-read-rules))]
                     [new-configs (filter (λ (new-c) 
                                            (not (member? (first (computation-LoC new-c)) (computation-visited new-c))))
                                          (map (λ (rule) (apply-rule (qfirst QoC) rule)) connected-pop-rules))])
                (if (empty? new-configs)
                    (make-computations lor (dequeue QoC) (cons (qfirst QoC) path) max-cmps)
                    (make-computations lor (enqueue new-configs (dequeue QoC)) path max-cmps)))]))


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




;;(listof symbols) machine -> (listof symbols)
;;Purpose: Returns the last fully consumed word for the given machine
(define (last-fully-consumed a-word M)
  (cond [(empty? a-word) '()]
        [(not (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                     (trace-computations a-word (fsa-getrules M) (fsa-getstart M))))
         (last-fully-consumed (take a-word (sub1 (length a-word))) M)]
        [a-word]))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (ndfa-create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                                   (trace-computations (imsg-state-ndfa-pci imsg-st)
                                                (fsa-getrules (imsg-state-ndfa-M imsg-st))
                                                (fsa-getstart (imsg-state-ndfa-M imsg-st))))]
         
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed (imsg-state-ndfa-pci imsg-st) (imsg-state-ndfa-M imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-ndfa-pci imsg-st) (imsg-state-ndfa-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (drop entire-word (length last-consumed-word))]
         [machine-decision (if (not (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)]) 

   (above/align
      'left
      (cond [(and (empty? (imsg-state-ndfa-pci imsg-st))
                  (empty? (imsg-state-ndfa-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE REJECT-COLOR)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FONT-COLOR)
                          (text (format "~a" EMP) FONT-SIZE BLANK-COLOR))))]
            [(and (not (empty? (imsg-state-ndfa-pci imsg-st))) (not completed-config?))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-ndfa-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-ndfa-pci imsg-st))
                                         '()
                                         (list (list (length last-consumed-word) 'gray)
                                               (list (length last-consumed-word) REJECT-COLOR)))))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? last-consumed-word)
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img last-consumed-word
                                         (if (> (length last-consumed-word) TAPE-SIZE)
                                             (imsg-state-ndfa-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-ndfa-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-ndfa-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img (imsg-state-ndfa-pci imsg-st)
                                                      (if (> (length (imsg-state-ndfa-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0) 
                                                      (if (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-ndfa-pci imsg-st)) ACCEPT-COLOR) '())))))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (list-ref (imsg-state-ndfa-computation-lengths imsg-st)
                                               (length (imsg-state-ndfa-pci imsg-st)))))
             FONT-SIZE
             COMPUTATION-LENGTH-COLOR)
      (cond [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [(and (empty? (imsg-state-ndfa-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
             [(and (empty? (imsg-state-ndfa-upci imsg-st))
                   (equal? machine-decision 'reject))
              (text "All computations end in a non-final state and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))



(define (pda-create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (second (first config))))
                                   (map computation-LoC (get-computations (imsg-state-pda-pci imsg-st)
                                                                          (pda-getrules (imsg-state-pda-M imsg-st))
                                                                          (pda-getstart (imsg-state-pda-M imsg-st))
                                                                          (imsg-state-pda-max-cmps imsg-st))))]
         
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (pda-last-fully-consumed (imsg-state-pda-pci imsg-st)
                                                  (imsg-state-pda-M imsg-st)
                                                  (imsg-state-pda-max-cmps imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pda-pci imsg-st) (imsg-state-pda-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (drop entire-word (length last-consumed-word))]
         
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (if (zipper-empty? (imsg-state-pda-stack imsg-st)) 
                            (imsg-state-pda-stack imsg-st)
                            (third (zipper-current (imsg-state-pda-stack imsg-st))))]
         [machine-decision (if (not (zipper-empty? (imsg-state-pda-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)])
    (above/align
      'left
      (cond [(and (empty? (imsg-state-pda-pci imsg-st))
                  (empty? (imsg-state-pda-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaa" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE REJECT-COLOR)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FONT-COLOR)
                          (text (format "~a" EMP) FONT-SIZE BLANK-COLOR))))]
            [(and (not (empty? (imsg-state-pda-upci imsg-st)))
                  (eq? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-pda-max-cmps imsg-st)))
                         (imsg-state-pda-computations imsg-st)))
             (above/align 'left
                          (beside (text "aaaa" FONT-SIZE BLANK-COLOR)
                                  (text "Word: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? (imsg-state-pda-pci imsg-st))
                                                     '()
                                                     (list (list (length (imsg-state-pda-pci imsg-st)) 'gray)
                                                           (list (length (imsg-state-pda-pci imsg-st)) DARKGOLDENROD2)))))
                          (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img (imsg-state-pda-pci imsg-st)
                                                 (if (> (length (imsg-state-pda-pci imsg-st)) TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 '())))]
            [(and (not (empty? (imsg-state-pda-pci imsg-st))) (not completed-config?))
             (above/align
              'left
              (beside (text "aaaa" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-pda-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pda-pci imsg-st))
                                         '()
                                         (list (list (length last-consumed-word) 'gray)
                                               (list (length last-consumed-word) REJECT-COLOR)))))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? last-consumed-word)
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img last-consumed-word
                                         (if (> (length last-consumed-word) TAPE-SIZE)
                                             (imsg-state-pda-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaa" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pda-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pda-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img (imsg-state-pda-pci imsg-st)
                                                      (if (> (length (imsg-state-pda-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      (if (zipper-empty? (imsg-state-pda-shown-accepting-trace imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pda-pci imsg-st)) ACCEPT-COLOR) '())))))])
      (cond [(zipper-empty? (imsg-state-pda-stack imsg-st)) (text "aaaC" FONT-SIZE BLANK-COLOR)]
            [(empty? current-stack) (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                                            (text "Stack: " FONT-SIZE FONT-COLOR))]
            [else (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                          (text "Stack: " FONT-SIZE FONT-COLOR)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-pda-word-img-offset imsg-st)
                                             0)
                                         (if (zipper-empty? (imsg-state-pda-shown-accepting-trace imsg-st))
                                                          '()
                                                          (list (list (length current-stack) ACCEPT-COLOR) '()))))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (if (= (length (imsg-state-pda-pci imsg-st)) (imsg-state-pda-max-cmps imsg-st))
                                        (list-ref (imsg-state-pda-computation-lengths imsg-st)
                                                  (sub1 (length (imsg-state-pda-pci imsg-st))))
                                        (list-ref (imsg-state-pda-computation-lengths imsg-st)
                                                  (length (imsg-state-pda-pci imsg-st))))))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (not (empty? (imsg-state-pda-upci imsg-st)))
                  (eq? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-pda-max-cmps imsg-st)))
                         (imsg-state-pda-computations imsg-st)))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-pda-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
            [(not completed-config?)
             (text "All computations do not consume the entire word and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [(and (empty? (imsg-state-pda-upci imsg-st))
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
            [(and (empty? (imsg-state-pda-upci imsg-st))
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))


(define (tm-create-draw-informative-message imsg-st)
  (above/align
      'left
      (if (zipper-empty? (imsg-state-tm-rules-used imsg-st))
          (text "Tape: " 1 BLANK-COLOR)
          (beside (text "Last rule used: " FONT-SIZE FONT-COLOR)
                  (text (format "~a" (if (or (equal? (zipper-current (imsg-state-tm-rules-used imsg-st)) '((_ _)(_ _)))
                                             (zipper-empty? (imsg-state-tm-rules-used imsg-st)))
                                         ""
                                         (zipper-current (imsg-state-tm-rules-used imsg-st))))
                        FONT-SIZE
                        (if (equal? (imsg-state-tm-machine-decision imsg-st) 'accept)
                            ACCEPT-COLOR
                            REJECT-COMPUTATION-COLOR))))
              
      (text "Tape: " 1 BLANK-COLOR)
      (if (not (zipper-empty? (imsg-state-tm-tape imsg-st) #;(imsg-state-tm-shown-accepting-trace imsg-st)))
          (draw-imsg imsg-st)
          (text "Tape is not shown when there are multiple rejecting computations." FONT-SIZE FONT-COLOR))
      #;(if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
          (text "Tape: " 1 BLANK-COLOR)
          (draw-imsg imsg-st))
      #;(cond [(ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                         (imsg-state-computations imsg-st))
             (above (text "Tape: " 20 FONT-COLOR)
                    (draw-imsg imsg-st))]
            [else (above  (text "Tape: " 20 FONT-COLOR)
                          (draw-imsg imsg-st))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (zipper-current (imsg-state-tm-computation-lengths imsg-st))))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (not (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)))
                  (>= (accessor-func (imsg-state-tm-shown-accepting-trace imsg-st)) (imsg-state-tm-max-cmps imsg-st))
                  (not (equal? (first (trace-config (zipper-current (imsg-state-tm-shown-accepting-trace imsg-st))))
                               (tm-getaccept (imsg-state-tm-M imsg-st)))))
             #;(ormap (λ (comp) (>= (length comp) (imsg-state-tm-max-cmps imsg-st)))
                         (imsg-state-tm-computations imsg-st))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-tm-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
            [(and (zipper-at-end? (imsg-state-tm-tape imsg-st))
                  (zipper-at-end? (imsg-state-tm-head-position imsg-st))
                  (equal? (imsg-state-tm-machine-decision imsg-st) 'accept))
             (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
            [(and (zipper-at-end? (imsg-state-tm-tape imsg-st))
                  (zipper-at-end? (imsg-state-tm-head-position imsg-st))
                  (equal? (imsg-state-tm-machine-decision imsg-st) 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)])))

;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;!!!TO BE REINSTATED ONCE OPTIMIZATIONS ARE MADE!!!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(
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
                               (cond [(empty? color-pair) FONT-COLOR]
                                     [(and (not (empty? (first color-pair)))
                                           (< (+ start-index i) (first (first color-pair))))
                                      (second (first color-pair))]
                                     [(and (not (empty? (second color-pair)))
                                           (= (+ start-index i) (first (second color-pair))))
                                      (second (second color-pair))]
                                     [else FONT-COLOR]))
                         (overlay (square 21 'solid BLANK-COLOR) (square (add1 21) 'solid BLANK-COLOR)))
                (overlay (square 21 'solid BLANK-COLOR) (square (add1 21) 'solid BLANK-COLOR)))))])
    (make-tape-img letter-imgs start-index)))

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

(struct computation (LoC LoR visited) #:transparent)


;;Purpose: Determines which informative message is displayed to the user
(define (ndfa-create-draw-informative-message imsg-st)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-ndfa-pci imsg-st) (imsg-state-ndfa-upci imsg-st))]
         
         ;;boolean
         ;;Purpose: Determines if the the machine should accept or reject the given word
         [machine-decision (if (not (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)]
         [FONT-SIZE 20]) 

   (above/align
      'left
      (cond [(and (empty? (imsg-state-ndfa-pci imsg-st))
                  (empty? (imsg-state-ndfa-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE REJECT-COLOR)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FONT-COLOR)
                          (text (format "~a" EMP) FONT-SIZE BLANK-COLOR))))]
            [(and (not (empty? (imsg-state-ndfa-pci imsg-st)))
                  (eq? (imsg-state-ndfa-upci imsg-st) (imsg-state-ndfa-farthest-consumed-input imsg-st))
                  (eq? machine-decision 'reject))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-ndfa-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-ndfa-pci imsg-st))
                                         '()
                                         (list (list (length (imsg-state-ndfa-pci imsg-st) ) 'gray)
                                               (list (length (imsg-state-ndfa-pci imsg-st) ) REJECT-COLOR)))))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? (imsg-state-ndfa-pci imsg-st))
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img (imsg-state-ndfa-pci imsg-st) 
                                         (if (> (length (imsg-state-ndfa-pci imsg-st)) TAPE-SIZE)
                                             (imsg-state-ndfa-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-ndfa-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-ndfa-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img (imsg-state-ndfa-pci imsg-st)
                                                      (if (> (length (imsg-state-ndfa-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (list-ref (imsg-state-ndfa-computation-lengths imsg-st)
                                               (length (imsg-state-ndfa-pci imsg-st)))))
             FONT-SIZE
             COMPUTATION-LENGTH-COLOR)
      (cond [(and (empty? (imsg-state-ndfa-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
             [(and (equal? machine-decision 'reject)
                   (eq? (imsg-state-ndfa-upci imsg-st) (imsg-state-ndfa-farthest-consumed-input imsg-st)))
              (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))



(define (pda-create-draw-informative-message imsg-st)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pda-pci imsg-st) (imsg-state-pda-upci imsg-st))]
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (if (zipper-empty? (imsg-state-pda-stack imsg-st)) 
                            (imsg-state-pda-stack imsg-st)
                            (third (zipper-current (imsg-state-pda-stack imsg-st))))]
         [machine-decision (if (not (zipper-empty? (imsg-state-pda-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)]
         [FONT-SIZE 20])
    (above/align
      'left
      (cond [(and (empty? (imsg-state-pda-pci imsg-st))
                  (empty? (imsg-state-pda-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE REJECT-COLOR)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FONT-COLOR)
                          (text (format "~a" EMP) FONT-SIZE BLANK-COLOR))))]
            [(and (not (empty? (imsg-state-pda-upci imsg-st)))
                  (eq? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-pda-max-cmps imsg-st)))
                         (imsg-state-pda-computations imsg-st)))
             (above/align 'left
                          (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                  (text "Word: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? (imsg-state-pda-pci imsg-st))
                                                     '()
                                                     (list (list (length (imsg-state-pda-pci imsg-st)) 'gray)
                                                           (list (length (imsg-state-pda-pci imsg-st)) DARKGOLDENROD2)))))
                          (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img (imsg-state-pda-pci imsg-st)
                                                 (if (> (length (imsg-state-pda-pci imsg-st)) TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 '())))]
            [(and #;(not (empty? (imsg-state-pda-pci imsg-st)))
                  (eq? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (eq? machine-decision 'reject))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-pda-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pda-pci imsg-st))
                                         '()
                                         (list (list (length (imsg-state-pda-pci imsg-st)) 'gray)
                                               (list (length (imsg-state-pda-pci imsg-st)) REJECT-COLOR)))))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? (imsg-state-pda-pci imsg-st))
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img (imsg-state-pda-pci imsg-st)
                                         (if (> (length (imsg-state-pda-pci imsg-st)) TAPE-SIZE)
                                             (imsg-state-pda-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pda-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pda-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img (imsg-state-pda-pci imsg-st)
                                                      (if (> (length (imsg-state-pda-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (cond [(zipper-empty? (imsg-state-pda-stack imsg-st)) (text "aaaC" FONT-SIZE BLANK-COLOR)]
            [(empty? current-stack) (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                                            (text "Stack: " FONT-SIZE FONT-COLOR))]
            [else (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                          (text "Stack: " FONT-SIZE FONT-COLOR)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-pda-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (if (= (length (imsg-state-pda-pci imsg-st)) (imsg-state-pda-max-cmps imsg-st))
                                        (list-ref (imsg-state-pda-computation-lengths imsg-st)
                                                  (sub1 (length (imsg-state-pda-pci imsg-st))))
                                        (list-ref (imsg-state-pda-computation-lengths imsg-st)
                                                  (length (imsg-state-pda-pci imsg-st))))))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (not (empty? (imsg-state-pda-upci imsg-st)))
                  (eq? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-pda-max-cmps imsg-st)))
                         (imsg-state-pda-computations imsg-st)))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-pda-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
            [(and (empty? (imsg-state-pda-upci imsg-st))
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
            [(and (eq? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))

(define (tm-create-draw-informative-message imsg-st)
  (let ([machine-decision (if (not (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)])
    (overlay/align
     'left 'middle
     (above/align
      'left
      
      (cond [(ormap (λ (comp) (>= (length comp) (imsg-state-tm-max-cmps imsg-st)))
                         (imsg-state-tm-computations imsg-st))
             (beside (text "aaaa" 20 BLANK-COLOR)
                                  (text "Tape: " 20 FONT-COLOR)
                                  (make-tape-img (imsg-state-tm-tape imsg-st)
                                                 (if (> (length (imsg-state-tm-tape imsg-st)) TAPE-SIZE)
                                                     (imsg-state-tm-word-img-offset imsg-st)
                                                     0)
                                                 '()))]
            [else (beside (text "aaaa" 20 BLANK-COLOR)
                          (text "Tape: " 20 FONT-COLOR)
                          (make-tape-img (imsg-state-tm-tape imsg-st)
                                         (if (> (length (imsg-state-tm-tape imsg-st)) TAPE-SIZE)
                                             (imsg-state-tm-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string 0
                                    #;(if (= (length (imsg-state-tm-pci imsg-st)) (imsg-state-tm-max-cmps imsg-st))
                                        (list-ref (imsg-state-tm-computation-lengths imsg-st)
                                                  (sub1 (length (imsg-state-tm-pci imsg-st))))
                                        (list-ref (imsg-state-tm-computation-lengths imsg-st)
                                                  (length (imsg-state-tm-pci imsg-st))))))
            20
            COMPUTATION-LENGTH-COLOR)
      (cond [(ormap (λ (comp) (>= (length comp) (imsg-state-tm-max-cmps imsg-st)))
                         (imsg-state-tm-computations imsg-st))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-tm-max-cmps imsg-st)) 20 DARKGOLDENROD2)]
            [(and (empty? (imsg-state-tm-tape imsg-st))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." 20 ACCEPT-COLOR)]
            [(and (empty? (imsg-state-tm-tape imsg-st))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." 20 REJECT-COLOR)]
            [else (text "Word Status: accept " 20 BLANK-COLOR)]))
     (rectangle 1250 50 'solid BLANK-COLOR))))

;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
)
