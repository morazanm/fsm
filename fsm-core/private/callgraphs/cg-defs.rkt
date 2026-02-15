#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 
 ;; ndfa-stucis
 ndfa-stuci ndfa-stuci? ndfa-stuci-state ndfa-stuci-ui
 ndfa-stucis-equal?
 
 ;; ndfa-Edges
 ndfa-edge  ndfa-edge?  ndfa-edge-fromst  ndfa-edge-read  ndfa-edge-tost
 ndfa-spedge  ndfa-spedge?  ndfa-spedge-fromst  ndfa-spedge-read  ndfa-spedge-tost
 ndfa-Edge-fromst ndfa-Edge-read ndfa-Edge-tost ndfa-Edges-equal?

 ;; ndfa-rule observers
 ndfa-rule-fromst ndfa-rule-read ndfa-rule-tost

 ;; pda-stucis
 pda-stuci pda-stuci? pda-stuci-state pda-stuci-ui pda-stuci-stack pda-stuci-int
 pda-stucis-equal?
 
 ;; pda-Edges
 pda-edge  pda-edge?  pda-edge-fromst  pda-edge-read  pda-edge-pop pda-edge-tost pda-edge-push
 pda-spedge  pda-spedge?  pda-spedge-fromst  pda-spedge-read  pda-spedge-pop pda-spedge-tost pda-spedge-push
 cutoff-edge  cutoff-edge?  cutoff-edge-fromst  cutoff-edge-read  cutoff-edge-pop cutoff-edge-tost cutoff-edge-push
 cutoff-spedge  cutoff-spedge?  cutoff-spedge-fromst  cutoff-spedge-read  cutoff-spedge-pop cutoff-spedge-tost cutoff-spedge-push
 pda-Edge-fromst pda-Edge-read pda-Edge-pop pda-Edge-tost pda-Edge-push pda-Edges-equal?

 ;; pda-rule observers
 pda-rule-fromst pda-rule-read pda-rule-pop pda-rule-tost pda-rule-push

 ;; tm-stucis
 tm-stuci tm-stuci? tm-stuci-state tm-stuci-tape tm-stuci-head tm-stuci-cl
 tm-stucis-equal?

 ;; tm-Edges
 tm-edge  tm-edge?  tm-edge-fromst  tm-edge-read  tm-edge-tost  tm-edge-action
 tm-spedge  tm-spedge?  tm-spedge-fromst  tm-spedge-read  tm-spedge-tost  tm-spedge-action
 tm-cutoff-edge  tm-cutoff-edge?  tm-cutoff-edge-fromst  tm-cutoff-edge-read  tm-cutoff-edge-tost  tm-cutoff-edge-action
 tm-cutoff-spedge  tm-cutoff-spedge?  tm-cutoff-spedge-fromst  tm-cutoff-spedge-read  tm-cutoff-spedge-tost  tm-cutoff-spedge-action
 tm-Edge-fromst tm-Edge-read tm-Edge-tost tm-Edge-action tm-Edges-equal?

 ;; tm-rule observers
 tm-rule-fromst tm-rule-read tm-rule-tost tm-rule-action

 ;; tm-tape
 mcons-set-i! tape-at-i tape-left-i tape-right-i create-tape add-blank create-tape-copy

 ;; mttm-stucis
 mttm-stuci mttm-stuci? mttm-stuci-state mttm-stuci-tapes mttm-stuci-heads mttm-stuci-cl
 mttm-stucis-equal?
 
 ;; mttm-Edges
 mttm-edge  mttm-edge?  mttm-edge-fromst  mttm-edge-reads  mttm-edge-tost  mttm-edge-actions
 mttm-spedge  mttm-spedge?  mttm-spedge-fromst  mttm-spedge-reads  mttm-spedge-tost  mttm-spedge-actions
 mttm-cutoff-edge  mttm-cutoff-edge?  mttm-cutoff-edge-fromst  mttm-cutoff-edge-reads  mttm-cutoff-edge-tost  mttm-cutoff-edge-actions
 mttm-cutoff-spedge  mttm-cutoff-spedge?  mttm-cutoff-spedge-fromst  mttm-cutoff-spedge-reads  mttm-cutoff-spedge-tost  mttm-cutoff-spedge-actions
 mttm-Edge-fromst mttm-Edge-tost mttm-Edge-actions mttm-Edge-reads mttm-Edges-equal?

 ;; mttm-rule observers
 mttm-rule-fromst mttm-rule-reads mttm-rule-tost mttm-rule-actions
 
 ;; mttm-tape
 tapes-at-i
 
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine Configurations
;; fsa

;; An fsa configuration is represented as a struct,
;; (ndfa-stuci state word), containing a state and the
;; unconsumed input.

(struct ndfa-stuci [state ui] #:transparent)

;.................................................
;; pda

;; A state and the unconsumed input when that state
;; is reached, is represented as a struct, called
;; pda-stuci (state, unconsumed input). The integer
;; shows the number of computations used to reach
;; the pda-stuci.

(struct pda-stuci [state ui stack int] #:transparent)

;.................................................
;; tm

;; A tm configuration is represented as a struct,
;; tm-stuci, containing a state, a tape, the head's
;; position, and a computation length.

(struct tm-stuci [state tape head cl] #:transparent)

;.................................................
;; mttm

;; A mttm configuration is represented as a struct,
;; mttm-stuci, containing a state, a list of tapes, the head's
;; positions, and a computation length.

(struct mttm-stuci [state tapes heads cl] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ndfa-Edges

;; We distinguish between regular and special
;; ndfa edges. An ndfa edge is special if its 
;; read element is the last ui of a stuci.

;; An ndfa-Edge is either:
;; 1. (ndfa-edge state symb state)
;; 2. (ndfa-spedge state symb state)

(struct ndfa-edge [fromst read tost] #:transparent)
(struct ndfa-spedge [fromst read tost] #:transparent)

;.................................................
;; pda-Edges

;; We will now distinguish between regular and special
;; pda edges. A pda edge is special if its read element
;; the last ui of a stuci. Additionally, a cutoff-edge
;; signifies if a state got stuck in a high number of
;; computations, possibly an infinite loop. A cutoff-spedge
;; points towards a state in which the cut-off is triggered
;; at the same time as the unconsumed input becomes empty.

;; A pda-Edge is either:
;; 1. (pda-edge state symb symb state symb)
;; 2. (pda-spedge state symb symb state symb)
;; 3. (cutoff-edge state symb symb state symb)
;; 4. (cutoff-spedge state symb symb state symb)

(struct pda-edge [fromst read pop tost push] #:transparent)
(struct pda-spedge [fromst read pop tost push] #:transparent)
(struct cutoff-edge [fromst read pop tost push] #:transparent)
(struct cutoff-spedge [fromst read pop tost push] #:transparent)

;.................................................
;; tm-Edges

;; For Turing machines we distinguis between regular and
;; special edges, the special edges signify in which state
;; the machine halts. Additionally, a cutoff-edge
;; signifies if a state got stuck in a high number of
;; computations, possibly an infinite loop. A cutoff-spedge
;; points towards a state in which the cut-off is triggered
;; at the same time as the machine halts.

;; An tm-Edge is either:
;; 1. (tm-edge state symb state action)
;; 2. (tm-spedge state symb state action)
;; 3. (tm-cutoff-edge state symb state action)
;; 4. (tm-cutoff-spedge state symb state action)

(struct tm-edge [fromst read tost action] #:transparent)
(struct tm-spedge [fromst read tost action] #:transparent)
(struct tm-cutoff-edge [fromst read tost action] #:transparent)
(struct tm-cutoff-spedge [fromst read tost action] #:transparent)

;.................................................
;; mttm-Edges

;; An mttm-Edge is either:
;; 1. (mttm-edge state (listof symb) state (listof symb))
;; 2. (mttm-spedge state (listof symb) state (listof symb))
;; 3. (mttm-cutoff-edge state (listof symb) state (listof symb))
;; 4. (mttm-cutoff-spedge state (listof symb) state (listof symb))

(struct mttm-edge [fromst reads tost actions] #:transparent)
(struct mttm-spedge [fromst reads tost actions] #:transparent)
(struct mttm-cutoff-edge [fromst reads tost actions] #:transparent)
(struct mttm-cutoff-spedge [fromst reads tost actions] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ndfa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ndfa-Edge auxiliary functions 

;; Edge -> state
;; Purpose: Extract the from state from the given Edge
(define (ndfa-Edge-fromst E)
  (if (ndfa-edge? E)
      (ndfa-edge-fromst E)
      (ndfa-spedge-fromst E)))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the read symbol from the given edge or spedge
(define (ndfa-Edge-read E)
  (if (ndfa-edge? E)
      (ndfa-edge-read E)
      (ndfa-spedge-read E)))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the to state from the given edge or spedge
(define (ndfa-Edge-tost E)
  (if (ndfa-edge? E)
      (ndfa-edge-tost E)
      (ndfa-spedge-tost E)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ndfa-Edge ndfa-Edge -> Boolean
;; Purpose: Determines if the two given ndfa-Edges are equal?
(define (ndfa-Edges-equal? e1 e2)
  (and (eq? (ndfa-Edge-fromst e1) (ndfa-Edge-fromst e2))
       (eq? (ndfa-Edge-read e1) (ndfa-Edge-read e2))
       (eq? (ndfa-Edge-tost e1) (ndfa-Edge-tost e2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ndfa-rule auxiliary functions 

;; rule -> symbol
;; Purpose: Given a rule, extracts the source state
(define (ndfa-rule-fromst r)
  (car r))

;.................................................

;; rule -> symbol
;; Purpose: Given a rule, extracts the destination state
(define (ndfa-rule-tost r)
  (caddr r))

;.................................................

;; rule -> symbol
;; Purpose: Given a rule, extracts the read input
(define (ndfa-rule-read r)
  (cadr r))

;.................................................

;; ndfa-stuci ndfa-stuci -> Boolean
;; Purpose: Determines if the two given ndfa-stucis are equal
(define (ndfa-stucis-equal? s1 s2)
  (and (equal? (ndfa-stuci-state s1) (ndfa-stuci-state s2))
       (equal? (ndfa-stuci-ui s1) (ndfa-stuci-ui s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pda-Edge auxiliary functions

;; Edge -> state
;; Purpose: Extract the from-state from the given Edge
(define (pda-Edge-fromst E)
  (cond [(pda-edge? E) (pda-edge-fromst E)]
        [(pda-spedge? E) (pda-spedge-fromst E)]
        [(cutoff-edge? E) (cutoff-edge-fromst E)]
        [else (cutoff-spedge-fromst E)]))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the read symbol from the given edge 
(define (pda-Edge-read E)
  (cond [(pda-edge? E) (pda-edge-read E)]
        [(pda-spedge? E) (pda-spedge-read E)]
        [(cutoff-edge? E) (cutoff-edge-read E)]
        [else (cutoff-spedge-read E)]))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the pop symbol from the given edge 
(define (pda-Edge-pop E)
  (cond [(pda-edge? E) (pda-edge-pop E)]
        [(pda-spedge? E) (pda-spedge-pop E)]
        [(cutoff-edge? E) (cutoff-edge-pop E)]
        [else (cutoff-spedge-pop E)]))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the to-state from the given edge 
(define (pda-Edge-tost E)
  (cond [(pda-edge? E) (pda-edge-tost E)]
        [(pda-spedge? E) (pda-spedge-tost E)]
        [(cutoff-edge? E) (cutoff-edge-tost E)]
        [else (cutoff-spedge-tost E)]))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the push symbol from the given edge 
(define (pda-Edge-push E)
  (cond [(pda-edge? E) (pda-edge-push E)]
        [(pda-spedge? E) (pda-spedge-push E)]
        [(cutoff-edge? E) (cutoff-edge-push E)]
        [else (cutoff-spedge-push E)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pda-Edge pda-Edge --> Boolean
;; Purpose: Determines if the two given pda-Edges are equal
(define (pda-Edges-equal? e1 e2)
  (and (equal? (pda-Edge-fromst e1) (pda-Edge-fromst e2))
       (equal? (pda-Edge-read e1) (pda-Edge-read e2))
       (equal? (pda-Edge-pop e1) (pda-Edge-pop e2))
       (equal? (pda-Edge-tost e1) (pda-Edge-tost e2))
       (equal? (pda-Edge-push e1) (pda-Edge-push e2))))

;.................................................

;; pda-stuci pda-stuci -> Boolean
;; Purpose: Determines if the two given pda-stucis are equal
(define (pda-stucis-equal? s1 s2)
  (and (eq? (pda-stuci-state s1) (pda-stuci-state s2))
       (equal? (pda-stuci-ui s1) (pda-stuci-ui s2))
       (equal? (pda-stuci-stack s1) (pda-stuci-stack s2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pda-rule auxiliary functions

;; rule -> symbol
;; Purpose: Given a rule, extracts the current state
(define (pda-rule-fromst r)
  (car (car r)))

;.................................................
;; pda-rule-tost

;; rule -> symbol
;; Purpose: Given a rule, extracts the next state
(define (pda-rule-tost r)
  (car (cadr r)))

;.................................................
;; pda-rule-read

;; rule -> symbol
;; Purpose: Given a rule, extracts the read input
(define (pda-rule-read r)
  (cadr (car r)))

;.................................................
;; pda-rule-pop

;; rule -> symbol
;; Purpose: Given a rule, extracts the popped input
(define (pda-rule-pop r)
  (caddr (car r)))

;.................................................
;; pda-rule-push

;; rule -> symbol
;; Purpose: Given a rule, extracts the pushed input
(define (pda-rule-push r)
  (cadr (cadr r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tm-Edge auxiliary functions

;; tm-Edge -> state
;; Purpose: Extract the from-state from the given edge
(define (tm-Edge-fromst E)
  (cond [(tm-edge? E) (tm-edge-fromst E)]
        [(tm-spedge? E) (tm-spedge-fromst E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-fromst E)]
        [else (tm-cutoff-spedge-fromst E)]))

;.................................................

;; tm-Edge -> symbol
;; Purpose: Extract the read symbol from the given edge
(define (tm-Edge-read E)
  (cond [(tm-edge? E) (tm-edge-read E)]
        [(tm-spedge? E) (tm-spedge-read E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-read E)]
        [else (tm-cutoff-spedge-read E)]))

;.................................................

;; tm-Edge -> symbol
;; Purpose: Extract the to-state from the given edge 
(define (tm-Edge-tost E)
  (cond [(tm-edge? E) (tm-edge-tost E)]
        [(tm-spedge? E) (tm-spedge-tost E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-tost E)]
        [else (tm-cutoff-spedge-tost E)]))

;.................................................

;; tm-Edge -> symbol
;; Purpose: Extract the action symbol from the given edge 
(define (tm-Edge-action E)
  (cond [(tm-edge? E) (tm-edge-action E)]
        [(tm-spedge? E) (tm-spedge-action E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-action E)]
        [else (tm-cutoff-spedge-action E)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tm-Edge tm-Edge --> Boolean
;; Purpose: Determines if the two given tm-Edges are equal
(define (tm-Edges-equal? e1 e2)
  (and (equal? (tm-Edge-fromst e1) (tm-Edge-fromst e2))
       (equal? (tm-Edge-read e1) (tm-Edge-read e2))
       (equal? (tm-Edge-tost e1) (tm-Edge-tost e2))
       (equal? (tm-Edge-action e1) (tm-Edge-action e2))))

;.................................................

;; tm-stuci tm-stuci -> Boolean
;; Purpose: Determines if the two given tm-stucis are equal
(define (tm-stucis-equal? s1 s2)
  (and (eq? (tm-stuci-state s1) (tm-stuci-state s2))
       (equal? (tm-stuci-tape s1) (tm-stuci-tape s2))
       (equal? (tm-stuci-head s1) (tm-stuci-head s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tm-rule auxiliary functions

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the source state
(define (tm-rule-fromst r)
  (car (car r)))
  
;.................................................
;; tm-rule-tost

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the destination state
(define (tm-rule-tost r)
  (car (cadr r)))

;.................................................
;; tm-rule-read

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the read input
(define (tm-rule-read r)
  (cadr (car r)))

;.................................................
;; tm-rule-action

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the action
(define (tm-rule-action r)
  (cadr (cadr r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mttm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mttm-Edge-fromst

;; mttm-Edge -> symbol
;; Purpose: Extract the from state from the given edge 
(define (mttm-Edge-fromst E)
  (cond [(mttm-edge? E) (mttm-edge-fromst E)]
        [(mttm-spedge? E) (mttm-spedge-fromst E)]
        [(mttm-cutoff-edge? E) (mttm-cutoff-edge-fromst E)]
        [else (mttm-cutoff-spedge-fromst E)]))

;.................................................
;; mttm-Edge-reads

;; mttm-Edge -> (listof symbol)
;; Purpose: Extract the read elements from the given edge 
(define (mttm-Edge-reads E)
  (cond [(mttm-edge? E) (mttm-edge-reads E)]
        [(mttm-spedge? E) (mttm-spedge-reads E)]
        [(mttm-cutoff-edge? E) (mttm-cutoff-edge-reads E)]
        [else (mttm-cutoff-spedge-reads E)]))

;.................................................
;; mttm-Edge-tost

;; mttm-Edge -> symbol
;; Purpose: Extract the to state from the given edge 
(define (mttm-Edge-tost E)
  (cond [(mttm-edge? E) (mttm-edge-tost E)]
        [(mttm-spedge? E) (mttm-spedge-tost E)]
        [(mttm-cutoff-edge? E) (mttm-cutoff-edge-tost E)]
        [else (mttm-cutoff-spedge-tost E)]))

;.................................................
;; mttm-Edge-actions

;; mttm-Edge -> (listof symbol)
;; Purpose: Extract the action elements from the given edge 
(define (mttm-Edge-actions E)
  (cond [(mttm-edge? E) (mttm-edge-actions E)]
        [(mttm-spedge? E) (mttm-spedge-actions E)]
        [(mttm-cutoff-edge? E) (mttm-cutoff-edge-actions E)]
        [else (mttm-cutoff-spedge-actions E)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mttm-Edges-equal?

;; mttm-Edge mttm-Edge -> Boolean
;; Purpose: Check whether two edges are the same
(define (mttm-Edges-equal? e1 e2)
  (and (equal? (mttm-Edge-fromst e1) (mttm-Edge-fromst e2))
       (equal? (mttm-Edge-reads e1) (mttm-Edge-reads e2))
       (equal? (mttm-Edge-tost e1) (mttm-Edge-tost e2))
       (equal? (mttm-Edge-actions e1) (mttm-Edge-actions e2))))

;.................................................
;; mttm-stucis-equal?

;; mttm-stuci mttm-stuci -> Boolean
;; Purpose: Check whether two stucis are the same
(define (mttm-stucis-equal? s1 s2)
  (and (eq? (mttm-stuci-state s1) (mttm-stuci-state s2))
       (equal? (mttm-stuci-tapes s1) (mttm-stuci-tapes s2))
       (equal? (mttm-stuci-heads s1) (mttm-stuci-heads s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mttm-rule-fromst

;; rule -> symbol
;; Purpose: Extract the from state from a rule
(define (mttm-rule-fromst r)
  (car (car r)))
  
;.................................................
;; mttm-rule-reads

;; rule -> (listof symbol)
;; Purpose: Extract the read elements from a rule
(define (mttm-rule-reads r)
  (cadr (car r)))

;.................................................
;; mttm-rule-tost

;; rule -> symbol
;; Purpose: Extract the to state from a rule
(define (mttm-rule-tost r)
  (car (cadr r)))

;.................................................
;; mttm-rule-actions

;; rule -> (listof symbol)
;; Purpose: Extract the action elements from a rule
(define (mttm-rule-actions r)
  (cadr (cadr r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcons-set-i!



;.................................................

;; (mlistof X) natnum X -> (void)
;; Purpose: Given a mutable list, a head's position, and a symbol,
;;          mutate the list at the head's position to the given symbol
(define (mcons-set-i! mlist i X)
  (if (= i 0)
      (set-mcar! mlist X)
      (mcons-set-i! (mcdr mlist) (sub1 i) X)))

;.................................................



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tape-at-i

;; tm-stuci -> symbol
;; Purpose: Given a tm-stuci return the element at the stuci's head position on the tape
(define (tape-at-i stuci)
  (let* [(tape (tm-stuci-tape stuci))
         (i (tm-stuci-head stuci))]         
    (cond [(null? tape) '_]
          [(= i 0) (mcar tape)]
          [else (tape-at-i (tm-stuci (tm-stuci-state stuci) (mcdr tape) (sub1 i) (tm-stuci-cl stuci)))])))

;.................................................
;; tapes-at-i

;; mttm-stuci -> symbol
;; Purpose: Given a mttm-stuci return the element at the stuci's head position on the tape
(define (tapes-at-i stuci)
  ;; (mlistof X) natnum -> X
  ;; Purpose: Find element at i on given mttm tape
  (define (mttm-tape-at-i tape i)
    (cond [(null? tape) '_]
          [(= i 0) (mcar tape)]
          [else (mttm-tape-at-i (mcdr tape) (sub1 i))]))
  (let [(tapes (mttm-stuci-tapes stuci))
        (is (mttm-stuci-heads stuci))]         
    (map (lambda (x y) (mttm-tape-at-i x y)) tapes is)))

;.................................................
;; tape-left-i

;; tm-stuci -> symbol
;; Purpose: Given a tm-stuci return the element to the left of the stuci's head position on the tape
(define (tape-left-i stuci)
  (let [(tape (mcons '() (tm-stuci-tape stuci)))
        (i (tm-stuci-head stuci))]         
    (cond [(= i 0) (mcar tape)]
          [else (tape-at-i (tm-stuci (tm-stuci-state stuci) (mcdr tape) (sub1 i) (tm-stuci-cl stuci)))])))

;.................................................
;; tape-right-i

;; tm-stuci -> symbol
;; Purpose: Given a tm-stuci return the element to the left of the stuci's head position on the tape
(define (tape-right-i stuci)
  (let [(tape (tm-stuci-tape stuci))
        (i (add1 (tm-stuci-head stuci)))]         
    (cond [(= i 0) (mcar tape)]
          [else (tape-at-i (tm-stuci (tm-stuci-state stuci) (mcdr tape) (sub1 i) (tm-stuci-cl stuci)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-tape

;; word -> (mlistof word)
;; Purpose: Given a word, creates a tape
(define (create-tape word)
  (define (create-tape-helper word)
    (if (null? word)
        '()
        (mcons (car word)
               (create-tape-helper (cdr word)))))
  (cond [(null? word) (mcons '@ '())]
        [(eq? '@ (car word)) (create-tape-helper word)]
        [else (mcons '@ (create-tape-helper word))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-blank

;; (mlistof X) -> (mlistof X)
;; Purpose: Given a tape, adds a blank at the end
(define (add-blank tape)
  (if (null? tape)
      (mcons '_ '())
      (mcons (mcar tape)
             (add-blank (mcdr tape)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-tape-copy

;; (mlistof X) -> (mlistof X)
;; Purpose: Create copy of tape
(define (create-tape-copy tape)
  (if (null? tape)
      '()
      (mcons (mcar tape)
             (create-tape-copy (mcdr tape)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
