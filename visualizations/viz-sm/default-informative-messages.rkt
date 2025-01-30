#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         2htdp/image
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/viz-constants.rkt"
         "david-imsg-state.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/misc.rkt")

(provide create-draw-informative-message)

(define FONT-SIZE 18)


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
(struct trace (config rules) #:transparent)

#|
A rule is a structure:
(make-rule triple)
triple is the entire of the ndfa rule
|#
(struct rule (triple) #:transparent)

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
(define (apply-rule config rule)
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
    (make-computations lor
                       (enqueue (list starting-config) E-QUEUE)
                       '())))


;;(listof rule) -> (listof computation)
;;Purpose: Traces all computations that the machine can make based on the starting state, word, and given rules
(define (make-computations lor QoC path)
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
                                  (map (λ (rule) (apply-rule (qfirst QoC) rule)) (append connected-read-rules connected-emp-rules)))])
        (if  (empty? new-configs)
             (make-computations lor (dequeue QoC) (cons (qfirst QoC) path))
             (make-computations lor (enqueue new-configs (dequeue QoC)) path)))))
  

;;(listof configurations) (listof rules) (listof configurations) -> (listof configurations)
;;Purpose: Returns a propers trace for the given (listof configurations) that accurately
;;         tracks each transition
(define (make-trace configs rules acc)
  (cond [(or (empty? rules)
             (empty? configs)) (reverse acc)]
        [(and (empty? acc)
              (not (equal? (second (first rules)) EMP)))
         (let* ([rle (rule (list EMP EMP EMP))]
                [res (trace (first configs) (list rle))])
           (make-trace (rest configs) rules (cons res acc)))]
        [(and (not (empty? acc))
              (equal? (second (first rules)) EMP))
         (let* ([rle (rule (first rules))]
                [res (struct-copy trace (first acc)
                                  [rules (cons rle (trace-rules (first acc)))])])
           (make-trace configs (rest rules) (cons res (rest acc))))]
        [else (let* ([rle (rule (first rules))]
                     [res (trace (first configs)
                                 (list rle))])
                (make-trace (rest configs) (rest rules) (cons res acc)))]))

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

;;(listof trace) -> (listof rule)
;;Purpose: Extracts the rule from the first trace in a (listof trace)
(define (get-trace-rule LoT)
  (filter-map-acc empty? trace-rules not first LoT))

;;(listof symbol ((listof symbols) -> boolean))) (X -> Y) -> (listof symbol ((listof symbols) -> boolean)))
;;Purpose: Extracts the invariants from the (listof symbol ((listof symbols) -> boolean)))
(define (get-invariants LoI func)
  (filter-map-acc (λ (x) ((second (first x)) (second x))) first func first LoI))

;;(listof trace) -> (listof trace)
;;Purpose: Extracts the empty trace from the (listof trace) and maps rest onto the non-empty trace
(define (get-next-traces LoT)
  (filter-map-acc empty? rest not id LoT))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (create-draw-informative-message imsg-st)
  (let* (;;boolean
         ;;Purpose: Determines if the pci can be can be fully consumed
         [completed-config? (ormap (λ (config) (empty? (second (first (computation-LoC config)))))
                                   (trace-computations (imsg-state-pci imsg-st)
                                                (fsa-getrules (imsg-state-M imsg-st))
                                                (fsa-getstart (imsg-state-M imsg-st))))]
         
         ;;(listof symbols)
         ;;Purpose: The last word that could be fully consumed by the ndfa
         [last-consumed-word (last-fully-consumed (imsg-state-pci imsg-st) (imsg-state-M imsg-st))]

         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;(listof symbols)
         ;;Purpose: The portion of the word that cannont be consumed
         [unconsumed-word (drop entire-word (length last-consumed-word))]
         [machine-decision (if (not (zipper-empty? (imsg-state-acpt-trace imsg-st)))
                               'accept
                               'reject)]) 

   
    (overlay/align
     'left 'middle
     (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaa" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE 'red)))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'black)
                          (text (format "~a" EMP) FONT-SIZE 'white))))]
            [(and (not (empty? (imsg-state-pci imsg-st))) (not completed-config?))
             (above/align
              'left
              (beside (text "aaaa" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length last-consumed-word) 'gray)
                                               (list (length last-consumed-word) 'red)))))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (empty? last-consumed-word)
                          (text "" FONT-SIZE 'black)
                          (make-tape-img last-consumed-word
                                         (if (> (length last-consumed-word) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaa" FONT-SIZE 'white)
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
      (cond [(not completed-config?)
              (text "All computations do not consume the entire word and the machine rejects." FONT-SIZE 'red)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE 'forestgreen)]
             [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'reject))
              (text "All computations end in a non-final state and the machine rejects." FONT-SIZE 'red)]
             [else (text "Word Status: accept " FONT-SIZE 'white)]))
     (rectangle 1250 50 'solid 'white))))


(define AB*B*UAB*
  (make-unchecked-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))


;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
