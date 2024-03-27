#lang fsm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-free grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nts Definitions
;;  S-0: start nts representing the empty word, if it is in the language
;;  Y-n: terminals that are not in chomsky form
;;  X-n: non-terminals that are not in chomsky form
;;  A-n: Base form for Greibach

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove empties

;; cfg -> cfg
;; Purpose: Remove empties
(define (rm-empties cfg)

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is a terminal
  (define (terminal? sym)
    (contains? sym (grammar-sigma cfg)))

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is an non-terminal
  (define (nts? sym)
    (contains? sym (grammar-nts cfg)))

  ;; symbol (listof symbol) -> boolean
  ;; Purpose: Check whether list contains given symbol
  (define (contains? elem list)
    (cond ((empty? list) #f)
          ((equal? elem (car list)) #t)
          (else (contains? elem (cdr list)))))
  
  ;; rules -> (listof nts)
  ;; Purpose: Find nts that can be replaced by empty
  (define (emp-nts r)  
    ;; rhs (listof nts) -> boolean
    ;; Purpose: Check whether righthand-side can be replaced by empty
    (define (leads-to-empty? rhs empties)
      (let ((ts (filter terminal? (symbol->list rhs))))
        (if (not (empty? ts))
            #f
            (or (eq? EMP rhs)
                (andmap (lambda (x) (contains? x empties)) (symbol->list rhs))))))  
    ;; nts -> (listof nts)
    ;; Purpose: Check if one nt can be replaced by empty
    (define (get-empties-helper nts empties)
      (map (lambda (z) (car z))
           (filter (lambda (y) (leads-to-empty? (caddr y) empties))
                   (filter (lambda (x) (contains? nts (symbol->list (caddr x)))) r))))
    ;; (listof symbol) (listof nts) -> (listof nts)
    ;; Purpose: Find nts that can be replaced by empty
    ;; Accumulator Invariants:
    ;;  to-visit = list of nts that still need to be explored
    ;;  empties = list of all nts that can be replaced by empty
    (define (get-empties to-visit empties)
      (let ((new-empties (filter (lambda (x) (not (contains? x empties)))
                                 (append-map (lambda (x) (get-empties-helper x empties)) to-visit))))
        (if (empty? new-empties)
            '()
            (append new-empties (get-empties new-empties
                                             (append new-empties empties))))))
    
    (get-empties '(ε) '()))

  ;; rule -> (listof rule)
  ;; Purpose: Find the list of new combinations for every rule 
  (define (all-combinations rule)
  
    ;; (listof symbol) -> symbol
    ;; Purpose: Convert a list of symbols to one symbol
    (define (list->symbol list)
      ;; (listof symbol) -> string
      ;; Purpose: Convert a list of symbols to one string
      (define (list->symbol-helper l)
        (if (empty? l)
            ""
            (string-append (symbol->string (car l))
                           (list->symbol-helper (cdr l)))))
      (string->symbol (list->symbol-helper list)))
  
    ;; (listof rhs) (listof rhs) -> Boolean
    ;; Purpose: Check if all elements of the first list are in the second list
    (define (all-rules? rhs acc)
      (cond ((empty? rhs) #t)
            ((contains? (car rhs) acc) (all-rules? (cdr rhs) acc))
            (else #f))) 

    ;; rhs nts integer -> (listof integer)
    ;; Purpose: Finds the indices of the given empty nts
    (define (indices rhs empty acc)
      ;; rhs integer -> rhs
      ;; Purpose: Replaces a found empty nts with a dummy string, so that its
      ;;          index is not used again
      (define (replace-emp rhs i)
        (if (= 0 i)
            (cons "dummy" (cdr rhs))
            (cons (car rhs) (replace-emp (cdr rhs) (sub1 i)))))
      (if (= 0 acc)
          '()
          (cons (index-of rhs empty)
                (indices (replace-emp rhs (index-of rhs empty))
                         empty (sub1 acc)))))
  
    ;; (listof integer) integer integer -> (listof integer)
    ;; Purpose: Find the complement of the given list
    (define (complement list num length)
      (cond ((= num length) '())
            ((contains? num list)
             (complement list (add1 num) length))
            (else (cons num (complement list (add1 num) length)))))
    
    ;; rhs (listof (listof integer)) -> (listof rhs)
    ;; Purpose: Finds all combinations for a rhs for a specific empty nts
    (define (new-rhs rhs combs)
      ;; (listof integer) -> rhs
      ;; Purpose: Find the rhs of given list of indices
      (define (extract-indexed-elems is)
        (map (lambda (x)
               (list-ref rhs x))
             (complement is 0 (length rhs))))
      (map (lambda (y)
             (extract-indexed-elems y))
           combs))
    ;; rhs nts -> rhs 
    ;; Purpose: Remove all empty nts one by one
    (define (make-combs-rhs-helper rhs empties)
      (if (empty? rhs)
          '()
          (append (append-map (lambda (x)
                                (new-rhs (car rhs)
                                         (combinations
                                          (indices (car rhs) x (length (filter (lambda (y) (equal? x y)) (car rhs)))))))                          
                              empties)
                  (make-combs-rhs-helper (cdr rhs) empties))))
    ;; (listof rhs) nts (listof rhs) -> (listof rhs)
    ;; Purpose: Find all combinations for a given list of rhs 
    ;; Accumulator invariants:
    ;;  acc = stores the last found list of rhs 
    (define (make-combs-rhs rhs-list empties acc)
      (if (and (all-rules? rhs-list acc)
               (all-rules? acc rhs-list))
          '()
          (let ((new-combs (remove-duplicates (make-combs-rhs-helper rhs-list empties))))
            (append new-combs
                    (make-combs-rhs new-combs empties rhs-list)))))
    ;; Termination argument:
    ;;  The function finds new combinations of rhs by eliminating the
    ;;  empty nts one by one. An accumulator stores the list of rhs
    ;;  combinations found in the last step. Once that lists repeats,
    ;;  there are no new combinations to be found and the function halts.
  
    (let ((rhs (remove-duplicates
                (filter (lambda (x) (not (empty? x)))
                        (make-combs-rhs (list (symbol->list (caddr rule))) (emp-nts (grammar-rules cfg)) '())))))      
      (map (lambda (x) (list (car rule) '-> (list->symbol x))) rhs)))
  
  (let* ((empties (emp-nts (grammar-rules cfg)))
         (new-rules (if (contains? (grammar-start cfg) empties)
                        (cons '(S-0 -> ε) (cons `(S-0 -> ,(grammar-start cfg))
                                                (filter (lambda (x) (not (eq? EMP (caddr x))))
                                                        (append-map (lambda (x) (all-combinations x)) (grammar-rules cfg)))))
                        (filter (lambda (x) (not (eq? EMP (caddr x))))
                                (append-map (lambda (x) (all-combinations x)) (grammar-rules cfg)))))
         (new-nts (remove-duplicates (append (map (lambda (x) (car x)) new-rules)
                                             (filter nts? (append-map symbol->list (map (lambda (x) (caddr x))
                                                                                        (grammar-rules cfg)))))))
         (new-start (if (contains? (grammar-start cfg) empties)
                        'S-0
                        (grammar-start cfg))))   
    (if (empty? (emp-nts (grammar-rules cfg)))
        cfg    
        (make-cfg new-nts
                  (grammar-sigma cfg)
                  new-rules
                  new-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kettenregeln

;; cfg -> cfg
;; Purpose: Remove Kettenregeln
(define (rm-ketten cfg)

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is a terminal
  (define (terminal? sym)
    (contains? sym (grammar-sigma cfg)))

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is an non-terminal
  (define (nts? sym)
    (contains? sym (grammar-nts cfg)))

  ;; symbol (listof symbol) -> boolean
  ;; Purpose: Check whether list contains given symbol
  (define (contains? elem list)
    (cond ((empty? list) #f)
          ((equal? elem (car list)) #t)
          (else (contains? elem (cdr list)))))
  
  ;; rules -> Boolean
  ;; Purpose: Check if there are any Ketten in the given ruels
  (define (no-more-ketten? rules)
    (empty? (filter (lambda (x) (kette? (caddr x))) rules)))
  
  ;; rhs -> Boolean
  ;; Purpose: Check if given rhs is a Kette
  (define (kette? rhs)
    (and (not (eq? EMP rhs))
         (not (terminal? rhs))
         (eq? 1 (length (symbol->list rhs)))))
  
  ;; rule -> rules
  ;; Purpose: Find all rules from a given Kette
  (define (kettenregeln rule)
    (let* ((ketten-rules (filter (lambda (x) (eq? (car x) (caddr rule))) (grammar-rules cfg)))
           (new-rules (map (lambda (x) `(,(car rule) ,ARROW ,(caddr x))) ketten-rules)))
      new-rules))
  
  ;; rules -> rules
  ;; Purpose: Traverse a cfg's rules and remove the Ketten
  (define (make-rules rules)
    (cond ((empty? rules) '())
          ((kette? (caddr (car rules))) (append (kettenregeln (car rules)) (make-rules (cdr rules))))
          (else (cons (car rules) (make-rules (cdr rules))))))
  
  ;; rules -> rules
  ;; Purpose: Function to traverse a cfg's rules
  (define (rm-ketten-helper rules)
    (if (no-more-ketten? rules)
        rules
        (rm-ketten-helper (make-rules rules))))
  ;; Termination argument:
  ;;  Function removes all Ketten recursively until there are
  ;;  no more. Then, halts.
  
  (let* ((new-rules (remove-duplicates (rm-ketten-helper (grammar-rules cfg))))
         (new-nts (remove-duplicates (append (map (lambda (x) (car x)) new-rules)
                                             (filter nts? (append-map symbol->list (map (lambda (x) (caddr x))
                                                                                        (grammar-rules cfg))))))))
    (if (no-more-ketten? (grammar-rules cfg))
        cfg    
        (make-cfg new-nts
                  (grammar-sigma cfg)
                  new-rules
                  (grammar-start cfg)))))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chomsky

;; cfg -> cfg
;; Purpose: Convert a given cfg to Chomsky Normal form
(define (chomsky cfg)

  ;; symbol (listof symbol) -> boolean
  ;; Purpose: Check whether list contains given symbol
  (define (contains? elem list)
    (cond ((empty? list) #f)
          ((equal? elem (car list)) #t)
          (else (contains? elem (cdr list)))))

  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is a terminal
  (define (terminal? sym)
    (contains? sym (grammar-sigma cfg)))
  ;; symbol -> boolean
  ;; Purpose: Check whether symbol is an non-terminal
  (define (nts? sym)
    (contains? sym (grammar-nts cfg)))
  
  ;; cfg -> rules
  ;; Purpose: Helper function for chomsky, makes new rules
  (define (chomsky-helper cfg)

    ;; (listof symbol) -> symbol
    ;; Purpose: Convert a list of symbols to one symbol
    (define (list->symbol list)
      ;; (listof symbol) -> string
      ;; Purpose: Convert a list of symbols to one string
      (define (list->symbol-helper l)
        (if (empty? l)
            ""
            (string-append (symbol->string (car l)) 
                           (list->symbol-helper (cdr l)))))
      (string->symbol (list->symbol-helper list)))

    ;; sigma (listof sigma) integer -> integer
    ;; Purpose: Find the ref of the sigma element 
    (define (counter elem list int)
      (if (equal? elem (car list))
          int
          (counter elem (cdr list) (add1 int))))

    ;; rule -> Boolean?
    ;; Purpose: Check whether rule is already in chomsky 
    (define (valid? rule)
      (or (terminal? (caddr rule))
          (and (= 2 (length (symbol->list (caddr rule))))
               (nts? (car (symbol->list (caddr rule))))
               (nts? (cadr (symbol->list (caddr rule)))))))
    ;; rule -> Boolean?
    ;; Purpose: Check whether rule is not yet in chomsky
    (define (invalid? rule)
      (not (valid? rule)))

    ;; rhs -> rhs
    ;; Purpose: Replace the ts in a rhs
    (define (replace-ts rhs)
      ;; t -> nt
      ;; Purpose: Replace t with a nt
      (define (new-t t)
        (string->symbol (string-append "Y-" (number->string (counter t (grammar-sigma cfg) 0)))))
      (cond ((empty? rhs) '())
            ((terminal? (car rhs)) (cons (new-t (car rhs)) (replace-ts (cdr rhs))))
            (else (cons (car rhs) (replace-ts (cdr rhs))))))

    ;; rule integer -> rules
    ;; Purpose: For a nt rule, make new rules in which rhs is length 2
    (define (new-X rule int)
      ;; rhs integer -> rhs
      ;; Purpose: Make a new rhs
      (define (new-nt rhs int)
        (cons (car rhs) (list (string->symbol (string-append "X-" (number->string int))))))
      ;; rhs integer -> rhs
      ;; Purpose: Make new rhs for all rules
      (define (replace-nts rhs int)
        (if (= 2 (length rhs))
            (list rhs) 
            (cons (new-nt rhs int)
                  (replace-nts (cdr rhs) (add1 int)))))
      ;; rhs integer -> rules
      ;; Purpose: Make new nts that follow chomsky
      (define (nts rhs int)
        (if (= 2 (length rhs))
            '()
            (cons (string->symbol (string-append "X-" (number->string int)))
                  (nts (cdr rhs) (add1 int)))))
      (map (lambda (x y) `(,x -> ,(list->symbol y))) (cons (car rule) (nts (caddr rule) int)) (replace-nts (caddr rule) int)))
    ;; (listof integer) (listof integer) -> (listof integer)
    ;; Purpose: Sums all numbers in list before each number
    ;; Accumulator invariants:
    ;;  c = consumed integers as sum 
    (define (sum-list c uc)
      (if (empty? uc)
          '()
          (cons (+ (foldl + 0 c) (car uc))
                (sum-list (list (+ (foldl + 0 c) (car uc))) (cdr uc)))))
    ;; rules -> (listof integer)
    ;; Purpose: Integers for new nts
    (define (ints rules)
      (if (empty? rules)
          '()
          (sum-list '() (cons 0 (drop-right (map (lambda (x) (if (> (- (length (symbol->list (caddr x))) 2) 0)
                                                                 (- (length (symbol->list (caddr x))) 2)
                                                                 0))
                                                 rules) 1)))))

    ;; rules -> rules
    ;; Purpose: Make new rules that follow chomsky
    (define (make-X-rules rules)
      (append-map (lambda (y z) (new-X y z))
                  (map (lambda (x) `(,(car x) -> ,(replace-ts (symbol->list (caddr x))))) rules)
                  (ints rules)))

    ;; sigma natnum -> rules
    ;; Purpose: Create new rules for each sigma 
    (define (make-Y-rules rules)
      (let ((sigma (remove-duplicates
                    (filter (lambda (y) (terminal? y))
                            (append-map symbol->list (map (lambda (x) (caddr x)) rules))))))
        (map (lambda (x) `(,(string->symbol (string-append "Y-" (number->string (counter x (grammar-sigma cfg) 0)))) -> ,x)) sigma)))

    (let ((valid (filter (lambda (x) (or (valid? x)
                                         (eq? EMP (caddr x)))) (grammar-rules cfg)))
          (invalid (filter (lambda (x) (and (invalid? x)
                                            (not (eq? EMP (caddr x))))) (grammar-rules cfg))))
      (remove-duplicates (append valid (make-Y-rules invalid) (make-X-rules invalid)))))
 
  (let* ((new-cfg ((compose rm-ketten rm-empties) cfg))
         (new-rules (chomsky-helper new-cfg))
         (new-nts (remove-duplicates
                   (append (map (lambda (x) (car x)) new-rules)
                           (filter (lambda (z) (nts? z))
                                   (append-map (lambda (y) (symbol->fsmlos (caddr y))) new-rules))))))
    (make-cfg new-nts
              (grammar-sigma new-cfg)
              new-rules 
              (grammar-start new-cfg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for Greibach

(define CFG1
  (make-cfg '(A B C D)
            '(a b c d)
            '((A -> DA)
              (A -> a)
              (B -> AC)
              (B -> BD)
              (B -> b)
              (C -> DB)
              (C -> c)
              (D -> AD)
              (D -> d))
            'A))

;.................................................
;; for Chomsky

(define CFG2
  (make-cfg '(S T)
            '(a b)
            '((S -> aSTbb)
              (S -> b)
              (T -> aSa))
            'S))

(define CFG2-chomsky
  (make-cfg '(S Y-0 Y-1 X-0 X-1 X-2 T X-3)
            '(a b)
            '((S -> b)
              (Y-0 -> a)
              (Y-1 -> b)
              (S -> Y-0X-0)
              (X-0 -> SX-1)
              (X-1 -> TX-2)
              (X-2 -> Y-1Y-1)
              (T -> Y-0X-3)
              (X-3 -> SY-0))
            'S))

;.................................................
;; empty

(define CFG3
  (make-cfg '(S R T U)
            '(a b)
            '((S -> aUb) 
              (S -> TT)  
              (R -> bbS) 
              (R -> RSUa)
              (T -> bR)
              (T -> ε)
              (U -> STS) 
              (U -> ab))
            'S))

(define CFG3-emp
  (make-cfg '(S-0 S R T U)
            '(a b)
            '((S-0 -> ε)
              (S-0 -> S)
              (S -> aUb) (S -> ab)
              (S -> TT) (S -> T)
              (R -> bbS) (R -> bb)
              (R -> RSUa) (R -> RUa) (R -> RSa) (R -> Ra)
              (T -> bR)
              (U -> STS) (U -> SS) (U -> TS) (U -> ST) (U -> T) (U -> S)
              (U -> ab))
            'S-0))

(define CFG3-ketten
  (make-cfg '(S-0 S R T U)
            '(a b)
            '((S-0 -> ε)
              (S-0 -> aUb) (S-0 -> ab)
              (S-0 -> TT) (S-0 -> bR)
              (S -> aUb) (S -> ab)
              (S -> TT) (S -> bR)
              (R -> bbS) (R -> bb)
              (R -> RSUa) (R -> RUa) (R -> RSa) (R -> Ra)
              (T -> bR)
              (U -> STS) (U -> SS) (U -> TS) (U -> ST) (U -> bR) (U -> aUb) (U -> ab)
              (U -> TT))
            'S-0))

(define CFG3-chomsky
  (make-cfg '(S-0 S U Y-0 Y-1 X-0 X-1 R X-2 X-3 X-4 X-5 X-6 T X-7 X-8)
            '(a b)
            '((S-0 -> ε)
              (S-0 -> TT)
              (S -> TT)
              (U -> SS)
              (U -> TS)
              (U -> ST)
              (U -> TT)
              (Y-0 -> a)
              (Y-1 -> b)
              (S-0 -> Y-0X-0)
              (X-0 -> UY-1)
              (S-0 -> Y-0Y-1)
              (S-0 -> Y-1R)
              (S -> Y-0X-1)
              (X-1 -> UY-1)
              (S -> Y-0Y-1)
              (S -> Y-1R)
              (R -> Y-1X-2)
              (X-2 -> Y-1S)
              (R -> Y-1Y-1)
              (R -> RX-3)
              (X-3 -> SX-4)
              (X-4 -> UY-0)
              (R -> RX-5)
              (X-5 -> UY-0)
              (R -> RX-6)
              (X-6 -> SY-0)
              (R -> RY-0)
              (T -> Y-1R)
              (U -> SX-7)
              (X-7 -> TS)
              (U -> Y-1R)
              (U -> Y-0X-8)
              (X-8 -> UY-1)
              (U -> Y-0Y-1))
            'S-0))

;.................................................
;; Kettenregel

(define CFG4
  (make-cfg '(S T R U)
            '(a b)
            '((S -> Ta)
              (S -> T)
              (T -> bTb)
              (T -> a)
              (T -> R)
              (T -> U)  
              (R -> aSb)
              (R -> b)
              (U -> ab)
              (U -> ST))
            'S))

(define CFG4-ketten
  (make-cfg '(S T R U)
            '(a b)
            '((S -> Ta)
              (S -> bTb)
              (S -> a)
              (S -> aSb)
              (S -> b)
              (S -> ab)
              (S -> ST)
              (T -> bTb)
              (T -> a)
              (T -> aSb)
              (T -> b)
              (T -> ab)
              (T -> ST)  
              (R -> aSb)
              (R -> b)
              (U -> ab)
              (U -> ST))
            'S))
 
(define CFG4-chomsky
  (make-cfg
   '(S T R U Y-0 Y-1 X-0 X-1 X-2 X-3 X-4)
   '(a b)
   '((S -> a)
     (S -> b)
     (S -> ST)
     (T -> a)
     (T -> b)
     (T -> ST)
     (R -> b)
     (U -> ST)
     (Y-0 -> a)
     (Y-1 -> b)
     (S -> TY-0)
     (S -> Y-1X-0)
     (X-0 -> TY-1)
     (S -> Y-0X-1)
     (X-1 -> SY-1)
     (S -> Y-0Y-1)
     (T -> Y-1X-2)
     (X-2 -> TY-1)
     (T -> Y-0X-3)
     (X-3 -> SY-1)
     (T -> Y-0Y-1)
     (R -> Y-0X-4)
     (X-4 -> SY-1)
     (U -> Y-0Y-1))
   'S))

;.................................................

(define CFG5 (make-cfg '(S)
                       '(a b)
                       '((S -> ε)
                         (S -> aSb)) 
                       'S))

(define CFG5-chomsky 
  (make-cfg
   '(S-0 Y-0 Y-1 X-0 S X-1)
   '(a b)
   '((S-0 -> ε)
     (Y-0 -> a)
     (Y-1 -> b)
     (S-0 -> Y-0X-0)
     (X-0 -> SY-1)
     (S-0 -> Y-0Y-1)
     (S -> Y-0X-1)
     (X-1 -> SY-1)
     (S -> Y-0Y-1))
   'S-0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests for chomsky
(check-equal? (chomsky CFG1) CFG1)
(check-equal? (grammar-testequiv CFG1 (chomsky CFG1) 10) #t)
(check-equal? (last (grammar-derive CFG1 '(a d a)))
              (last (grammar-derive (chomsky CFG1) '(a d a))))

(check-equal? (chomsky CFG2) CFG2-chomsky)
(check-equal? (grammar-testequiv CFG2 (chomsky CFG2) 10) #t)
(check-equal? (last (grammar-derive CFG2 '(a b a b a b b)))
              (last (grammar-derive (chomsky CFG2) '(a b a b a b b)))) 

(check-equal? (rm-empties CFG3) CFG3-emp)
(check-equal? (rm-ketten CFG3-emp) CFG3-ketten) 
(check-equal? (chomsky CFG3) CFG3-chomsky)
;(check-equal? (grammar-testequiv CFG3 (chomsky CFG3) 10) #t)

(check-equal? (rm-ketten CFG4) CFG4-ketten)
(check-equal? (chomsky CFG4) CFG4-chomsky)
;(check-equal? (grammar-testequiv CFG4 (chomsky CFG4) 10) #t)

(check-equal? (chomsky CFG5) CFG5-chomsky)
(check-equal? (grammar-testequiv CFG5 (chomsky CFG5) 10) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Greibach
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CFG6
  (make-cfg '(A B C D)
            '(a b c d)
            '((A -> DA)
              (A -> a)
              (B -> AC)
              (B -> BD)
              (B -> b)
              (C -> DB)
              (C -> c)
              (D -> AD)
              (D -> d))
            'A))

(define CFG6-As
  (make-cfg '(A-0 A-1 A-2 A-3)
            '(a b c d)
            '((A-0 -> A-3A-0)
              (A-0 -> a)
              (A-1 -> A-0A-2)
              (A-1 -> A-1A-3)
              (A-1 -> b)
              (A-2 -> A-3A-1)
              (A-2 -> c)
              (A-3 -> A-0A-3)
              (A-3 -> d))
            'A-0))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol (listof symbol) -> boolean
;; Purpose: Check whether list contains given symbol
(define (contains? elem list)
  (cond ((empty? list) #f)
        ((equal? elem (car list)) #t)
        (else (contains? elem (cdr list)))))

;; symbol -> boolean
;; Purpose: Check whether symbol is a terminal
(define (terminal? cfg sym)
  (contains? sym (grammar-sigma cfg)))

;; symbol -> boolean
;; Purpose: Check whether symbol is an non-terminal
(define (nts? cfg sym)
  (contains? sym (grammar-nts cfg)))

;.................................................

;; (listof symbol) -> symbol
;; Purpose: Convert a list of symbols to one symbol
(define (list->symbol list)
  ;; (listof symbol) -> string
  ;; Purpose: Convert a list of symbols to one string
  (define (list->symbol-helper l)
    (if (empty? l)
        ""
        (string-append (symbol->string (car l)) 
                       (list->symbol-helper (cdr l)))))
  (string->symbol (list->symbol-helper list)))

;; string -> number
;; Purpose: Extract number from symbol
(define (string-last str)
  (if (= (string-length str) 1)
      (string->number str)
      (string-last (symbol->string (list->symbol (cdr (symbol->list (string->symbol str))))))))

;.................................................

;; (listof rule) -> (listof rule)
;; Purpose: Return the list of rules grouped by lhs 
(define (group-lhs rules)
  (if (empty? rules)
      '()
      (let* ((current-rule (car rules))
             (current-lhs (car current-rule))
             (new-current-list (filter (lambda (x) (equal? current-lhs (car x))) rules))
             (tovisit-list (filter (lambda (x) (not (equal? current-lhs (car x)))) rules)))
        (append new-current-list
                (group-lhs tovisit-list)))))

;.................................................

;; An A is a structure that contains:
;;  A nt (symbol)
;;  An int (integer)
;;  A representation (symbol)
(struct A (nt n rep) #:transparent)

;.................................................

;; cfg -> cfg
;; Purpose: Convert a given cfg to A-n representation
(define (convert-to-A-rep cfg)
  ;; (listof rule) -> (listof rule)
  ;; Purpose: Convert all nts to those that follow A-n notation
  ;; Accumulator invariants:
  ;;  acc = keeps track of n
  (define (make-As rules acc)
    (if (empty? rules)
        '()
        (let* ((current-rule (car rules))
               (current-lhs (car current-rule))
               (new-lhs (string->symbol (string-append "A-" (number->string acc))))
               (tovisit-list (filter (lambda (x) (not (equal? current-lhs (car x)))) rules)))
          (cons (A current-lhs acc new-lhs)
                (make-As tovisit-list (add1 acc))))))

  ;; symbol -> rule
  ;; Purpose: Find the right A struct for a nt
  (define (find-corresponding-A sym)
    (car (filter (lambda (x) (equal? sym (A-nt x))) (make-As (grammar-rules cfg) 0))))
  
  ;; (listof rule) (listof A) -> (listof rule)
  ;; Purpose: Convert the nts in the rules to A-n representation
  (define (make-A-rules rules)
    ;; (listof symbol) -> (listof symbol)
    ;; Purpose: Replace all nt in given list by A-n representation
    (define (new-A los)
      (cond ((empty? los) '())
            ((or (terminal? cfg (car los))
                 (equal? EMP (car los)))
             (cons (car los)
                   (new-A (cdr los))))
            (else (cons (A-rep (find-corresponding-A (car los)))
                        (new-A (cdr los))))))
    (if (empty? rules)
        '()
        (let* ((rule (car rules))
               (new-rule `(,(list->symbol (new-A (symbol->fsmlos (car rule)))) -> ,(list->symbol (new-A (symbol->fsmlos (caddr rule)))))))
          (cons new-rule
                (make-A-rules (cdr rules))))))
  
  (let* ((new-nts (map (lambda (x) (A-rep (find-corresponding-A x))) (grammar-nts cfg)))
         (new-rules (make-A-rules (grammar-rules cfg)))
         (new-start (A-rep (find-corresponding-A (grammar-start cfg)))))
    (make-cfg new-nts
              (grammar-sigma cfg)
              new-rules 
              new-start)))

(convert-to-A-rep CFG6)

;.................................................

;; steps is a structure that contains sure all combinations of As.
;; lhs is the Ai on the lhs.
;; rhs is the first Aj on the rhs.
;; bool is intialized as false. Becomes true if i>j. 
(struct steps (lhs rhs bool) #:transparent)

;.................................................



(define (greibach-rules cfg)
  ;; (listof rule) -> (listof rule)
  ;; Purpose: Convert all nts to those that follow A-n notation
  ;; Accumulator invariants:
  ;;  acc = keeps track of n
  (define (make-As rules acc)
    (if (empty? rules)
        '()
        (let* ((current-rule (car rules))
               (current-lhs (car current-rule))
               (new-lhs (string->symbol (string-append "A-" (number->string acc))))
               (tovisit-list (filter (lambda (x) (not (equal? current-lhs (car x)))) rules)))
          (cons (A current-lhs acc new-lhs)
                (make-As tovisit-list (add1 acc))))))

  ;; symbol -> rule
  ;; Purpose: Find the right A struct for a nt
  (define (find-corresponding-A sym)
    (car (filter (lambda (x) (equal? sym (A-nt x))) (make-As (grammar-rules cfg) 0))))
  (define (find-corresponding-A2 sym)
    (car (filter (lambda (x) (equal? sym (A-rep x))) (make-As (grammar-rules cfg) 0))))
  
  ;; (listof rule) (listof A) -> (listof rule)
  ;; Purpose: Convert the nts in the rules to A-n representation
  (define (make-A-rules rules)
    ;; (listof symbol) -> (listof symbol)
    ;; Purpose: Replace all nt in given list by A-n representation
    (define (new-A los)
      (cond ((empty? los) '())
            ((or (terminal? cfg (car los))
                 (equal? EMP (car los)))
             (cons (car los)
                   (new-A (cdr los))))
            (else (cons (A-rep (find-corresponding-A (car los)))
                        (new-A (cdr los))))))
    (if (empty? rules)
        '()
        (let* ((rule (car rules))
               (new-rule `(,(list->symbol (new-A (symbol->fsmlos (car rule)))) -> ,(list->symbol (new-A (symbol->fsmlos (caddr rule)))))))
          (cons new-rule
                (make-A-rules (cdr rules))))))
  
  ;; nts -> (listof steps)
  ;; Purpose: Make a list of steps for all nts
  ;; Accumulator invariants:
  ;;  acc = stores original nts
  (define (make-steps nts acc)
    (if (empty? nts)
        '()
        (let* ((i (A-n (car nts)))
               (applicable-nts (filter (lambda (x) (<= (A-n x) i)) acc)))
          (append (map (lambda (x) (steps (car nts) x #f)) applicable-nts)
                  (make-steps (cdr nts) acc)))))

  ;; steps -> bool
  ;; Purpose: Return true if all steps are done 
  (define (steps-true? st)
    (andmap (lambda (x) (steps-bool x)) st))

  (define (new r as st)

    (define (valid? sym rule)
      (let ((rhs (caddr rule)))
        (begin (displayln sym)
        (or (terminal? cfg rhs)
            (eq? EMP rhs)
            (> (A-n (find-corresponding-A2 (car (symbol->fsmlos rhs)))) (A-n (find-corresponding-A sym)))))))


    (define (find-rules-A rhs)
      (let ((rules (grammar-rules (convert-to-A-rep cfg)))
            #;(rules2 (filter (lambda (x) (equal? (car x) rhs)) rules)))
        (filter (lambda (x) (equal? (car x) rhs)) rules)))
    
    #;(define (make-valid-rules a rule)
      (let* ((rhs (car (symbol->fsmlos (caddr rule))))
             (Aint (A-n (find-corresponding-A2 rhs)))
             (int (A-n a))
             #;(Anextint (A-n (find-rule-A rhs))))
      (cond ((and (> Aint int)
                  (< Anextint int))
             (replace ...)
            (else (make-Bs ...))))))
    
    (define (new2 step rules)
      (cond ((empty? rules) '())
            ((valid? (A-nt (steps-lhs step)) (car rules)) (cons (car rules) (new2 step (cdr rules))))
            #;(else (cons (make-valid-rules (steps-lhs step) (car rule)) (new2 step (cdr rules))))))

    (if (empty? st)
        '()
        (cons (new2 (car st) (filter (lambda (x) (equal? (car x) (A-rep (steps-lhs (car st))))) r))
              (new r as (cdr st)))))

  (let* ((new-cfg (convert-to-A-rep cfg))
         (As (make-As (grammar-rules cfg) 0))
         (stepss (make-steps As As)))
    (new (grammar-rules new-cfg) As stepss)))

(greibach-rules CFG6)




















  







  
