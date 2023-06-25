#lang fsm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 
 ;; ndfa stucis
 ndfa-stuci ndfa-stuci? ndfa-stuci-state ndfa-stuci-ui
 
 ;; Edges
 ndfa-edge  ndfa-edge?  ndfa-edge-fromst  ndfa-edge-read  ndfa-edge-tost
 ndfa-spedge  ndfa-spedge?  ndfa-spedge-fromst  ndfa-spedge-read  ndfa-spedge-tost
 ndfa-Edge-fromst ndfa-Edge-read ndfa-Edge-tost ndfa-Edges-equal?

 ;; ndfa-rule observers
 ndfa-rule-fromst ndfa-rule-read ndfa-rule-tost

 ;; pda stucis
 pda-stuci pda-stuci? pda-stuci-state pda-stuci-ui pda-stuci-stack pda-stuci-int
 pda-stucis-equal?
 
 ;; Edges
 pda-edge  pda-edge?  pda-edge-fromst  pda-edge-read  pda-edge-pop pda-edge-tost pda-edge-push
 pda-spedge  pda-spedge?  pda-spedge-fromst  pda-spedge-read  pda-spedge-pop pda-spedge-tost pda-spedge-push
 cutoff-edge  cutoff-edge?  cutoff-edge-fromst  cutoff-edge-read  cutoff-edge-pop cutoff-edge-tost cutoff-edge-push
 cutoff-spedge  cutoff-spedge?  cutoff-spedge-fromst  cutoff-spedge-read  cutoff-spedge-pop cutoff-spedge-tost cutoff-spedge-push
 pda-Edge-fromst pda-Edge-read pda-Edge-pop pda-Edge-tost pda-Edge-push pda-Edges-equal?

 ;; ndfa-rule observers
 pda-rule-fromst pda-rule-read pda-rule-pop pda-rule-tost pda-rule-push
 
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine Configurations
;; fsa

;; An fsa configuration is represented by a struct,
;; (ndfa-stuci state word), containing a state and the
;; unconsumed input.

(struct ndfa-stuci [state ui] #:transparent)

;.................................................
;; pda

;; A state and the unconsumed input when that state
;; is reached, is represented by a struct, called
;; pda-stuci (state, unconsumed input). The integer
;; shows the number of computations used to reach
;; the pda-stuci.

(struct pda-stuci [state ui stack int] #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ndfa edges and spedges

;; We distinguish between regular and special
;; ndfa edges. An ndfa edge is special if its 
;; read element is the last ui of a stuci.

;; An ndfa-Edge is either:
;; 1. (ndfa-edge state symb state)
;; 2. (ndfa-spedge state symb state)

(struct ndfa-edge [fromst read tost] #:transparent)
(struct ndfa-spedge [fromst read tost] #:transparent)

;.................................................
;; pda-edges and -spedges and cutoff-edges and -spedges

;; We will now distinguish between regular and special
;; pda edges. A pda edge is special if its read element
;; the last ui of a stuci. Additionally, a cutoff-edge
;; signifies if a state got stuck in a high number of
;; computations, possibly an infinite loop. A cutoff-spedge
;; points towards a state in which a word ends that is
;; accepted. 

;; An edge can be an edge, special edge, or a cutoff edge:
;; 1. (pda-edge state symb symb state symb)
;; 2. (pda-spedge state symb symb state symb)
;; 3. (cutoff-edge state symb symb state symb)
;; 4. (cutoff-spedge state symb symb state symb)

(struct pda-edge [fromst read pop tost push] #:transparent)
(struct pda-spedge [fromst read pop tost push] #:transparent)
(struct cutoff-edge [fromst read pop tost push] #:transparent)
(struct cutoff-spedge [fromst read pop tost push] #:transparent)

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
;; Purpose: Extract the from state from the given Edge
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
;; Purpose: Extract the read symbol from the given edge or spedge
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
;; Purpose: Extract the pop symbol from the given edge or spedge
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
;; Purpose: Extract the to state from the given edge or spedge
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
;; Purpose: Extract the push symbol from the given edge or spedge
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











