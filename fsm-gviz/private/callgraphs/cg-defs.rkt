#lang fsm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 
 ;; ndfa-stucis
 ndfa-stuci ndfa-stuci? ndfa-stuci-state ndfa-stuci-ui
 
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

;; Tests for ndfa-Edge-fromst
(check-equal? (ndfa-Edge-fromst (ndfa-edge 'Y 'b 'T)) 'Y)
(check-equal? (ndfa-Edge-fromst (ndfa-edge 'Z 'ε 'R)) 'Z)
(check-equal? (ndfa-Edge-fromst (ndfa-spedge 'Q 'b 'ds)) 'Q)
(check-equal? (ndfa-Edge-fromst (ndfa-spedge 'A 'c 'ds)) 'A)

;.................................................

;; Edge -> symbol
;; Purpose: Extract the read symbol from the given edge or spedge
(define (ndfa-Edge-read E)
  (if (ndfa-edge? E)
      (ndfa-edge-read E)
      (ndfa-spedge-read E)))

;; Tests for ndfa-Edge-read
(check-equal? (ndfa-Edge-read (ndfa-edge 'Y 'b 'T)) 'b)
(check-equal? (ndfa-Edge-read (ndfa-edge 'Z 'ε 'R)) 'ε)
(check-equal? (ndfa-Edge-read (ndfa-spedge 'Q 'b 'ds)) 'b)
(check-equal? (ndfa-Edge-read (ndfa-spedge 'A 'c 'ds)) 'c)

;.................................................

;; Edge -> symbol
;; Purpose: Extract the to state from the given edge or spedge
(define (ndfa-Edge-tost E)
  (if (ndfa-edge? E)
      (ndfa-edge-tost E)
      (ndfa-spedge-tost E)))

;; Tests for ndfa-Edge-tost
(check-equal? (ndfa-Edge-tost (ndfa-edge 'Y 'b 'T)) 'T)
(check-equal? (ndfa-Edge-tost (ndfa-edge 'Z 'ε 'R)) 'R)
(check-equal? (ndfa-Edge-tost (ndfa-spedge 'Q 'b 'ds)) 'ds)
(check-equal? (ndfa-Edge-tost (ndfa-spedge 'A 'c 'ds)) 'ds)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ndfa-Edge ndfa-Edge -> Boolean
;; Purpose: Determines if the two given ndfa-Edges are equal?
(define (ndfa-Edges-equal? e1 e2)
  (and (eq? (ndfa-Edge-fromst e1) (ndfa-Edge-fromst e2))
       (eq? (ndfa-Edge-read e1) (ndfa-Edge-read e2))
       (eq? (ndfa-Edge-tost e1) (ndfa-Edge-tost e2))))

;; Tests for ndfa-Edges-equal?
(check-equal? (ndfa-Edges-equal? (ndfa-edge 'Y 'b 'T) (ndfa-edge 'Y 'b 'T)) #t)
(check-equal? (ndfa-Edges-equal? (ndfa-edge 'Z 'ε 'R) (ndfa-edge 'Y 'b 'T)) #f)
(check-equal? (ndfa-Edges-equal? (ndfa-spedge 'Q 'b 'ds) (ndfa-edge 'Q 'b 'ds)) #t)
(check-equal? (ndfa-Edges-equal? (ndfa-spedge 'A 'c 'ds) (ndfa-edge 'Q 'b 'ds)) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ndfa-rule auxiliary functions 

;; rule -> symbol
;; Purpose: Given a rule, extracts the source state
(define (ndfa-rule-fromst r)
  (first r))

;; Tests for ndfa-rule-fromst
(check-equal? (ndfa-rule-fromst '(S ε S)) 'S)
(check-equal? (ndfa-rule-fromst '(S a ds)) 'S)
(check-equal? (ndfa-rule-fromst '(X a ds)) 'X)
(check-equal? (ndfa-rule-fromst '(Y b X)) 'Y)

;.................................................

;; rule -> symbol
;; Purpose: Given a rule, extracts the destination state
(define (ndfa-rule-tost r)
  (third r))

;; Tests for ndfa-rule-tost
(check-equal? (ndfa-rule-tost '(S ε S)) 'S)
(check-equal? (ndfa-rule-tost '(S a ds)) 'ds)
(check-equal? (ndfa-rule-tost '(X a ds)) 'ds)
(check-equal? (ndfa-rule-tost '(Y b X)) 'X)

;.................................................

;; rule -> symbol
;; Purpose: Given a rule, extracts the read input
(define (ndfa-rule-read r)
  (second r))

;; Tests for ndfa-rule-read
(check-equal? (ndfa-rule-read '(S ε S)) 'ε)
(check-equal? (ndfa-rule-read '(S a ds)) 'a)
(check-equal? (ndfa-rule-read '(X a ds)) 'a)
(check-equal? (ndfa-rule-read '(Y b X)) 'b)

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

;; Tests for pda-Edge-fromst 
(check-equal? (pda-Edge-fromst (pda-edge 'S 'ε 'ε 'Q '(S))) 'S)
(check-equal? (pda-Edge-fromst (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'S)
(check-equal? (pda-Edge-fromst (cutoff-edge 'Q 'ε '(S) 'Q '(b))) 'Q)
(check-equal? (pda-Edge-fromst (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) 'Q)

;.................................................

;; Edge -> symbol
;; Purpose: Extract the read symbol from the given edge 
(define (pda-Edge-read E)
  (cond [(pda-edge? E) (pda-edge-read E)]
        [(pda-spedge? E) (pda-spedge-read E)]
        [(cutoff-edge? E) (cutoff-edge-read E)]
        [else (cutoff-spedge-read E)]))

;; Tests for pda-Edge-read 
(check-equal? (pda-Edge-read (pda-edge 'S 'ε 'ε 'Q '(S))) 'ε)
(check-equal? (pda-Edge-read (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ε)
(check-equal? (pda-Edge-read (cutoff-edge 'Q 'ε '(S) 'Q '(b))) 'ε)
(check-equal? (pda-Edge-read (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) 'ε)

;.................................................

;; Edge -> symbol
;; Purpose: Extract the pop symbol from the given edge 
(define (pda-Edge-pop E)
  (cond [(pda-edge? E) (pda-edge-pop E)]
        [(pda-spedge? E) (pda-spedge-pop E)]
        [(cutoff-edge? E) (cutoff-edge-pop E)]
        [else (cutoff-spedge-pop E)]))

;; Tests for pda-Edge-pop 
(check-equal? (pda-Edge-pop (pda-edge 'S 'ε 'ε 'Q '(S))) 'ε)
(check-equal? (pda-Edge-pop (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ε)
(check-equal? (pda-Edge-pop (cutoff-edge 'Q 'ε '(S) 'Q '(b))) '(S))
(check-equal? (pda-Edge-pop (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) '(S))

;.................................................

;; Edge -> symbol
;; Purpose: Extract the to-state from the given edge 
(define (pda-Edge-tost E)
  (cond [(pda-edge? E) (pda-edge-tost E)]
        [(pda-spedge? E) (pda-spedge-tost E)]
        [(cutoff-edge? E) (cutoff-edge-tost E)]
        [else (cutoff-spedge-tost E)]))

;; Tests for pda-Edge-tost 
(check-equal? (pda-Edge-tost (pda-edge 'S 'ε 'ε 'Q '(S))) 'Q)
(check-equal? (pda-Edge-tost (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ds)
(check-equal? (pda-Edge-tost (cutoff-edge 'Q 'ε '(S) 'Q '(b))) 'Q)
(check-equal? (pda-Edge-tost (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) 'Q)

;.................................................

;; Edge -> symbol
;; Purpose: Extract the push symbol from the given edge 
(define (pda-Edge-push E)
  (cond [(pda-edge? E) (pda-edge-push E)]
        [(pda-spedge? E) (pda-spedge-push E)]
        [(cutoff-edge? E) (cutoff-edge-push E)]
        [else (cutoff-spedge-push E)]))

;; Tests for pda-Edge-push 
(check-equal? (pda-Edge-push (pda-edge 'S 'ε 'ε 'Q '(S))) '(S))
(check-equal? (pda-Edge-push (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ε)
(check-equal? (pda-Edge-push (cutoff-edge 'Q 'ε '(S) 'Q '(b))) '(b))
(check-equal? (pda-Edge-push (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) '(A b A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pda-Edge pda-Edge --> Boolean
;; Purpose: Determines if the two given pda-Edges are equal
(define (pda-Edges-equal? e1 e2)
  (and (equal? (pda-Edge-fromst e1) (pda-Edge-fromst e2))
       (equal? (pda-Edge-read e1) (pda-Edge-read e2))
       (equal? (pda-Edge-pop e1) (pda-Edge-pop e2))
       (equal? (pda-Edge-tost e1) (pda-Edge-tost e2))
       (equal? (pda-Edge-push e1) (pda-Edge-push e2))))

;; Tests for pda-Edges-equal?
(check-equal? (pda-Edges-equal? (pda-edge 'S 'ε 'ε 'Q '(S)) (cutoff-edge 'S 'ε 'ε 'Q '(S))) #t)
(check-equal? (pda-Edges-equal? (pda-spedge 'S 'ε 'ε 'ds 'ε) (pda-edge 'S 'ε 'ε 'Q '(S))) #f)
(check-equal? (pda-Edges-equal? (cutoff-edge 'Q 'ε '(S) 'Q '(b)) (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) #f)
(check-equal? (pda-Edges-equal? (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A)) (pda-spedge 'Q 'ε '(S) 'Q '(A b A))) #t)

;.................................................

;; pda-stuci pda-stuci -> Boolean
;; Purpose: Determines if the two given pda-stucis are equal
(define (pda-stucis-equal? s1 s2)
  (and (eq? (pda-stuci-state s1) (pda-stuci-state s2))
       (equal? (pda-stuci-ui s1) (pda-stuci-ui s2))
       (equal? (pda-stuci-stack s1) (pda-stuci-stack s2))))

;; Tests for pda-stucis-equal?
(check-equal? (pda-stucis-equal? (pda-stuci 'S '(a b) '(a) 0) (pda-stuci 'S '(a) '(a) 0)) #f)
(check-equal? (pda-stucis-equal? (pda-stuci 'S '() '(b) 0) (pda-stuci 'S '() '() 1)) #f)
(check-equal? (pda-stucis-equal? (pda-stuci 'Q '() '(b) 0) (pda-stuci 'S '() '(b) 1)) #f)
(check-equal? (pda-stucis-equal? (pda-stuci 'S '(a b) '() 0) (pda-stuci 'S '(a b) '() 10)) #t)
(check-equal? (pda-stucis-equal? (pda-stuci 'R '(a b) '(a) 0) (pda-stuci 'R '(a b) '(a) 10)) #t)
(check-equal? (pda-stucis-equal? (pda-stuci 'S '() '() 0) (pda-stuci 'S '() '() 1)) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pda-rule auxiliary functions

;; rule -> symbol
;; Purpose: Given a rule, extracts the current state
(define (pda-rule-fromst r)
  (first (first r)))

;; Tests for pda-rule-fromst
(check-equal? (pda-rule-fromst '((S ε ε) (X ε))) 'S)
(check-equal? (pda-rule-fromst '((S a ε) (S (b b)))) 'S)
(check-equal? (pda-rule-fromst '((X a (a)) (ds ε))) 'X)
(check-equal? (pda-rule-fromst '((Y b (b b)) (ds ε))) 'Y)
  
;.................................................
;; pda-rule-tost

;; rule -> symbol
;; Purpose: Given a rule, extracts the next state
(define (pda-rule-tost r)
  (first (second r)))

;; Tests for pda-rule-tost
(check-equal? (pda-rule-tost '((S ε ε) (X ε))) 'X)
(check-equal? (pda-rule-tost '((S a ε) (S (b b)))) 'S)
(check-equal? (pda-rule-tost '((X a (a)) (ds ε))) 'ds)
(check-equal? (pda-rule-tost '((Y b (b b)) (ds ε))) 'ds)

;.................................................
;; pda-rule-read

;; rule -> symbol
;; Purpose: Given a rule, extracts the read input
(define (pda-rule-read r)
  (second (first r)))

;; Tests for read
(check-equal? (pda-rule-read '((S ε ε) (X ε))) 'ε)
(check-equal? (pda-rule-read '((S a ε) (S (b b)))) 'a)
(check-equal? (pda-rule-read '((X a (a)) (ds ε))) 'a)
(check-equal? (pda-rule-read '((Y b (b b)) (ds ε))) 'b)

;.................................................
;; pda-rule-pop

;; rule -> symbol
;; Purpose: Given a rule, extracts the popped input
(define (pda-rule-pop r)
  (third (first r)))

;; Tests for pda-rule-pop
(check-equal? (pda-rule-pop '((S ε ε) (X ε))) 'ε)
(check-equal? (pda-rule-pop '((S a ε) (S (b b)))) 'ε)
(check-equal? (pda-rule-pop '((X a (a)) (ds ε))) '(a))
(check-equal? (pda-rule-pop '((Y b (b b)) (ds ε))) '(b b))

;.................................................
;; pda-rule-push

;; rule -> symbol
;; Purpose: Given a rule, extracts the pushed input
(define (pda-rule-push r)
  (second (second r)))

;; Tests for pda-rule-push
(check-equal? (pda-rule-push '((S ε ε) (X ε))) 'ε)
(check-equal? (pda-rule-push '((S a ε) (S (b b)))) '(b b))
(check-equal? (pda-rule-push '((X a (a)) (ds ε))) 'ε)
(check-equal? (pda-rule-push '((Y b (b b)) (ds ε))) 'ε)

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

;; Tests for tm-Edge-fromst 
(check-equal? (tm-Edge-fromst (tm-edge 'S 'a 'Q '_)) 'S)
(check-equal? (tm-Edge-fromst (tm-spedge 'S 'b 'A 'R)) 'S)
(check-equal? (tm-Edge-fromst (tm-cutoff-edge 'Q '_ 'Q 'L)) 'Q)
(check-equal? (tm-Edge-fromst (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'Q)

;.................................................

;; tm-Edge -> symbol
;; Purpose: Extract the read symbol from the given edge
(define (tm-Edge-read E)
  (cond [(tm-edge? E) (tm-edge-read E)]
        [(tm-spedge? E) (tm-spedge-read E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-read E)]
        [else (tm-cutoff-spedge-read E)]))

;; Tests for tm-Edge-read
(check-equal? (tm-Edge-read (tm-edge 'S 'a 'Q '_)) 'a)
(check-equal? (tm-Edge-read (tm-spedge 'S 'b 'A 'R)) 'b)
(check-equal? (tm-Edge-read (tm-cutoff-edge 'Q '_ 'Q 'L)) '_)
(check-equal? (tm-Edge-read (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'b)

;.................................................

;; tm-Edge -> symbol
;; Purpose: Extract the to-state from the given edge 
(define (tm-Edge-tost E)
  (cond [(tm-edge? E) (tm-edge-tost E)]
        [(tm-spedge? E) (tm-spedge-tost E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-tost E)]
        [else (tm-cutoff-spedge-tost E)]))

;; Tests for tm-Edge-tost 
(check-equal? (tm-Edge-tost (tm-edge 'S 'a 'Q '_)) 'Q)
(check-equal? (tm-Edge-tost (tm-spedge 'S 'b 'A 'R)) 'A)
(check-equal? (tm-Edge-tost (tm-cutoff-edge 'Q '_ 'Q 'L)) 'Q)
(check-equal? (tm-Edge-tost (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'Q)

;.................................................

;; tm-Edge -> symbol
;; Purpose: Extract the action symbol from the given edge 
(define (tm-Edge-action E)
  (cond [(tm-edge? E) (tm-edge-action E)]
        [(tm-spedge? E) (tm-spedge-action E)]
        [(tm-cutoff-edge? E) (tm-cutoff-edge-action E)]
        [else (tm-cutoff-spedge-action E)]))

;; Tests for tm-Edge-action 
(check-equal? (tm-Edge-action (tm-edge 'S 'a 'Q '_)) '_)
(check-equal? (tm-Edge-action (tm-spedge 'S 'b 'A 'R)) 'R)
(check-equal? (tm-Edge-action (tm-cutoff-edge 'Q '_ 'Q 'L)) 'L)
(check-equal? (tm-Edge-action (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tm-Edge tm-Edge --> Boolean
;; Purpose: Determines if the two given tm-Edges are equal
(define (tm-Edges-equal? e1 e2)
  (and (equal? (tm-Edge-fromst e1) (tm-Edge-fromst e2))
       (equal? (tm-Edge-read e1) (tm-Edge-read e2))
       (equal? (tm-Edge-tost e1) (tm-Edge-tost e2))
       (equal? (tm-Edge-action e1) (tm-Edge-action e2))))

;; Tests for pda-Edges-equal?
(check-equal? (tm-Edges-equal? (tm-cutoff-edge 'Q 'b 'Q 'a) (tm-cutoff-spedge 'E '_ 'Q 'a)) #f)
(check-equal? (tm-Edges-equal? (tm-cutoff-spedge 'Q 'b 'Q 'a) (tm-cutoff-spedge 'Q 'b 'Q 'b)) #f)
(check-equal? (tm-Edges-equal? (tm-edge 'S 'a 'Q '_) (tm-cutoff-edge 'S 'a 'Q '_)) #t)
(check-equal? (tm-Edges-equal? (tm-cutoff-spedge 'Q 'b 'Q 'a) (tm-cutoff-spedge 'Q 'b 'Q 'a)) #t)

;.................................................

;; tm-stuci tm-stuci -> Boolean
;; Purpose: Determines if the two given tm-stucis are equal
(define (tm-stucis-equal? s1 s2)
  (and (eq? (tm-stuci-state s1) (tm-stuci-state s2))
       (equal? (tm-stuci-tape s1) (tm-stuci-tape s2))
       (equal? (tm-stuci-head s1) (tm-stuci-head s2))))

;; Tests for tm-stucis-equal?
(check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 1 0) (tm-stuci 'S '(@ _ a b c) 1 0)) #f)
(check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 3 0) (tm-stuci 'S '(@ _ a b) 1 0)) #f)
(check-equal? (tm-stucis-equal? (tm-stuci 'Q '(@ _ a b) 1 0) (tm-stuci 'S '(@ _ a b c) 1 0)) #f)
(check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 1 0) (tm-stuci 'S '(@ _ a b) 1 0)) #t)
(check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ a b c) 1 10) (tm-stuci 'S '(@ a b c) 1 0)) #t)
(check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 1 7) (tm-stuci 'S '(@ _ a b) 1 5)) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tm-rule auxiliary functions

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the source state
(define (tm-rule-fromst r)
  (first (first r)))

;; Tests for tm-rule-fromst
(check-equal? (tm-rule-fromst '((S a) (Q _))) 'S)
(check-equal? (tm-rule-fromst '((S b) (A R))) 'S)
(check-equal? (tm-rule-fromst '((Q _) (Q L))) 'Q)
(check-equal? (tm-rule-fromst '((Q b) (Q a))) 'Q)
  
;.................................................
;; tm-rule-tost

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the destination state
(define (tm-rule-tost r)
  (first (second r)))

;; Tests for tm-rule-tost
(check-equal? (tm-rule-tost '((S a) (Q _))) 'Q)
(check-equal? (tm-rule-tost '((S b) (A R))) 'A)
(check-equal? (tm-rule-tost '((Q _) (Q L))) 'Q)
(check-equal? (tm-rule-tost '((Q b) (Q a))) 'Q)

;.................................................
;; tm-rule-read

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the read input
(define (tm-rule-read r)
  (second (first r)))

;; Tests for tm-rule-read
(check-equal? (tm-rule-read '((S a) (Q _))) 'a)
(check-equal? (tm-rule-read '((S b) (A R))) 'b)
(check-equal? (tm-rule-read '((Q _) (Q L))) '_)
(check-equal? (tm-rule-read '((Q b) (Q a))) 'b)

;.................................................
;; tm-rule-action

;; tm-Edge -> symbol
;; Purpose: Given a rule, extracts the action
(define (tm-rule-action r)
  (second (second r)))

;; Tests for tm-rule-action
(check-equal? (tm-rule-action '((S a) (Q _))) '_)
(check-equal? (tm-rule-action '((S b) (A R))) 'R)
(check-equal? (tm-rule-action '((Q _) (Q L))) 'L)
(check-equal? (tm-rule-action '((Q b) (Q a))) 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcons-set-i!

;; Sample tape
(define TAPE (mcons '@
                  (mcons '_
                         (mcons 'x
                                (mcons 'x
                                       (mcons 'a
                                              (mcons 'x
                                                     (mcons 'x
                                                            (mcons '_ '())))))))))

;.................................................

;; (mlistof X) natnum X -> (void)
;; Purpose: Given a mutable list, a head's position, and a symbol,
;;          mutate the list at the head's position to the given symbol
(define (mcons-set-i! mlist i X)
  (if (= i 0)
      (set-mcar! mlist X)
      (mcons-set-i! (mcdr mlist) (sub1 i) X)))

;.................................................

;; Tests for mcons-set-i!
(check-equal? (begin
                (mcons-set-i! TAPE 4 'x)
                TAPE)
              (mcons '@
                  (mcons '_
                         (mcons 'x
                                (mcons 'x
                                       (mcons 'x
                                              (mcons 'x
                                                     (mcons 'x
                                                            (mcons '_ '())))))))))
(check-equal? (begin
                (mcons-set-i! TAPE 7 'x)
                TAPE)
              (mcons '@
                  (mcons '_
                         (mcons 'x
                                (mcons 'x
                                       (mcons 'x
                                              (mcons 'x
                                                     (mcons 'x
                                                            (mcons 'x '())))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tape-at-i

;; tm-stuci -> symbol
;; Purpose: Given a tm-stuci return the element at the stuci's head position on the tape
(define (tape-at-i stuci)
  (let* [(tape (tm-stuci-tape stuci))
         (i (tm-stuci-head stuci))]         
    (cond [(empty? tape) '_]
          [(= i 0) (mcar tape)]
          [else (tape-at-i (tm-stuci (tm-stuci-state stuci) (mcdr tape) (sub1 i) (tm-stuci-cl stuci)))])))

;; Tests for tape-at-i
(check-equal? (tape-at-i (tm-stuci 'S TAPE 0 10)) '@)
(check-equal? (tape-at-i (tm-stuci 'B TAPE 1 10)) '_)
(check-equal? (tape-at-i (tm-stuci 'Q TAPE 2 10)) 'x)
(check-equal? (tape-at-i (tm-stuci 'Q (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 8 10))
              '_)

;.................................................
;; tape-left-i

;; tm-stuci -> symbol
;; Purpose: Given a tm-stuci return the element to the left of the stuci's head position on the tape
(define (tape-left-i stuci)
  (let* [(tape (mcons '() (tm-stuci-tape stuci)))
         (i (tm-stuci-head stuci))]         
    (cond [(= i 0) (mcar tape)]
          [else (tape-at-i (tm-stuci (tm-stuci-state stuci) (mcdr tape) (sub1 i) (tm-stuci-cl stuci)))])))

;; Tests for tape-left-i
(check-equal? (tape-left-i (tm-stuci 'B
                                     (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))) 1 10))
              '@)
(check-equal? (tape-left-i (tm-stuci 'Q
                                     (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 2 10))
              '_)
(check-equal? (tape-left-i (tm-stuci 'Q
                                     (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 3 10))
              'a)
(check-equal? (tape-left-i (tm-stuci 'Q
                                     (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 0 10))
              '())

;.................................................
;; tape-right-i

;; tm-stuci -> symbol
;; Purpose: Given a tm-stuci return the element to the left of the stuci's head position on the tape
(define (tape-right-i stuci)
  (let* [(tape (tm-stuci-tape stuci))
         (i (add1 (tm-stuci-head stuci)))]         
    (cond [(= i 0) (mcar tape)]
          [else (tape-at-i (tm-stuci (tm-stuci-state stuci) (mcdr tape) (sub1 i) (tm-stuci-cl stuci)))])))

;; Tests for tape-right-i
(check-equal? (tape-right-i (tm-stuci 'B
                                      (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))) 0 10))
              '_)
(check-equal? (tape-right-i (tm-stuci 'Q
                                      (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 1 10))
              'a)
(check-equal? (tape-right-i (tm-stuci 'Q
                                      (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 2 10))
              'b)
(check-equal? (tape-right-i (tm-stuci 'Q
                                      (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 8 10))
              '_)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-tape

;; word -> (mlistof word)
;; Purpose: Given a word, creates a tape
(define (create-tape word)
  (local [(define (create-tape-helper word)
            (if (empty? word)
                '()
                (mcons (first word)
                       (create-tape-helper (rest word)))))]
    (mcons '@ (create-tape-helper word))))

;; Tests for create-tape
(check-equal? (create-tape '(_ x x x x x x))
              (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
(check-equal? (create-tape '(a b c d e f))
              (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
(check-equal? (create-tape '())
              (mcons '@ '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-blank

;; (mlistof X) -> (mlistof X)
;; Purpose: Given a tape, adds a blank at the end
(define (add-blank tape)
  (if (empty? tape)
      (mcons '_ '())
      (mcons (mcar tape)
             (add-blank (mcdr tape)))))

;; Tests for add-blank
(check-equal? (add-blank (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
              (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons '_ '()))))))))))
(check-equal? (add-blank (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
              (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f (mcons '_ '())))))))))
(check-equal? (add-blank (mcons '@ '()))
              (mcons '@ (mcons '_ '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-tape-copy

;; (mlistof X) -> (mlistof X)
;; Purpose: Create copy of tape
(define (create-tape-copy tape)
  (if (empty? tape)
      '()
      (mcons (mcar tape)
             (create-tape-copy (mcdr tape)))))

;; Tests for create-tape-copy
(check-equal? (create-tape-copy (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
              (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
(check-equal? (create-tape-copy (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
              (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
(check-equal? (create-tape-copy (mcons '@ '()))
              (mcons '@ '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








