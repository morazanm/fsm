#lang racket/base

(require "cfexp-structs.rkt"
         racket/set
         racket/treelist)

(provide qempty?
         E-QUEUE
         qfirst
         enqueue
         dequeue
         tl-foldl
         (rename-out (extract-var-and-singles-cfe extract-box-and-singles-cfe))
         (struct-out extraction-results)
         (struct-out pda)
         (struct-out pda-rule)
         (struct-out pda-action))


#|
pda-struct is a structural representation of a pda
  states | The states for the given pda => (listof states)
  sigma  | The alphabet that the given pda works over => (listof symbol)
  gamma  | The stack alphabet that given pda works over => (listof symbol)
  start  | The starting state => symbol
  finals | The final states => (listof symbol)
  rules  | The transition relation for the given pda => (listof pda-rule)
|#
(struct pda (states sigma gamma start finals rules) #:transparent)

#|
pda-rule is a structural representation of a pda rule
  source | The state the rule is coming from => symbol
  action | The action the pda takes when using the rule => pda-action
  destin | The state the rule transitions to => symbol
|#
(struct pda-rule (source action destin) #:transparent)

#|
a pda-action is a structural representation of a pda action
  read | The element that the pda reads => symbol
  pop  | The element(s) that the pda pops of the stack => symbol / (listof symbol)
  push | The element(s) that the pda pushes to the stack => symbol / (listof symbol)
|#
(struct pda-action (read pop push) #:transparent)


;;vars    | the accumulated boxes found from traversing the given cfe      => (setof union-cfexp)
;;singles | the accumulated singletons found from traversing the given cfe => (setof singleton-cfexp)
(struct extraction-results (lang-boxes singles) #:transparent)

(define qempty? treelist-empty?)

(define E-QUEUE empty-treelist) 

;; (qof X) → X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (treelist-first a-qox)))

;; (tllistof X) (qof X) → (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (treelist-append a-qox a-lox))

;; (qof X) → (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (treelist-rest a-qox)))

;;(X -> Y) Z (treelistof X) -> Z
(define (tl-foldl f acc tl)
  (if (treelist-empty? tl)
      acc
      (tl-foldl f (f (treelist-first tl) acc) (treelist-rest tl))))
  
;;cfe -> extraction-results
;;Purpose: Extracts all var-cfexp and singleton-cfexp from the given cfe
(define (extract-var-and-singles-cfe cfe)
  ;;cfe -> (listof cfe)
  ;;Purpose: Extracts the sub-expressions from the given cfe
  (define (extract-cfe-data cfe)
    (cond [(mk-concat-cfexp? cfe) (vector->treelist (mk-concat-cfexp-locfe cfe))]
          [(mk-union-cfexp? cfe) (vector->treelist (mk-union-cfexp-locfe cfe))]
          [(mk-kleene-cfexp? cfe) (treelist (mk-kleene-cfexp-cfe cfe))]
          [(box? cfe) (treelist (unbox cfe))]
          [else empty-treelist]))
  ;;cfe extraction-results -> extraction-results
  ;;Purpose: Updates the given extraction-results to add the given cfe if it is a singleton or variable
  (define (update-extraction-results cfe extract-res)
    (cond [(or (mk-kleene-cfexp? cfe) (box? cfe))
           (struct-copy extraction-results
                        extract-res
                        [lang-boxes (set-add (extraction-results-lang-boxes extract-res) cfe)])]
          [(mk-singleton-cfexp? cfe)
           (struct-copy extraction-results
                        extract-res
                        [singles (set-add (extraction-results-singles extract-res)
                                          (string->symbol (mk-singleton-cfexp-char cfe)))])]
          [else extract-res]))
    
  ;;(queueof cfe) extraction-results (listof cfe) -> extraction-results
  ;;Purpose: Extracts the cfe and adds it to the extraction-results if its a singleton or variable
  (define (extract-var-and-singles qocfe extract-res visited)
    (if (qempty? qocfe)
        extract-res
        (let* ([cfe (qfirst qocfe)]
               [cfes-to-add (extract-cfe-data cfe)]
               [new-queue (enqueue (dequeue qocfe)
                                   (treelist-filter (λ (cfe) (not (set-member? visited cfe))) cfes-to-add))]
               [new-acc (update-extraction-results cfe extract-res)]
               [new-visited (set-add visited cfe)])
          (extract-var-and-singles new-queue new-acc new-visited))))
  (let ([init-queue (tl-foldl (λ (env acc)
                                (enqueue acc (treelist env)))
                              E-QUEUE
                              (extract-cfe-data cfe))])
    (extract-var-and-singles init-queue
                             (update-extraction-results cfe (extraction-results (set) (set)))
                             (set cfe))))