#lang fsm
(provide chomsky greibach)
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

  ;; cfg -> Boolean
  ;; Purpose: Check if cfg is in Chomsky form
  (define (chomsky? cfg)
    ;; rule -> Boolean?
    ;; Purpose: Check whether rule is already in chomsky 
    (define (valid? rule)
      (or (terminal? (caddr rule))
          (equal? EMP (caddr rule))
          (and (= 2 (length (symbol->fsmlos (caddr rule))))
               (nts? (car (symbol->fsmlos (caddr rule))))
               (nts? (cadr (symbol->fsmlos (caddr rule)))))))
    (empty? (filter (lambda (x) (not (valid? x))) (grammar-rules cfg))))
  
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

  (if (chomsky? cfg)
      cfg
      (let* ((new-cfg ((compose rm-ketten rm-empties) cfg))
             (new-rules (chomsky-helper new-cfg))
             (new-nts (remove-duplicates
                       (append (map (lambda (x) (car x)) new-rules)
                               (filter (lambda (z) (nts? z))
                                       (append-map (lambda (y) (symbol->fsmlos (caddr y))) new-rules))))))
        (make-cfg new-nts
                  (grammar-sigma new-cfg)
                  new-rules 
                  (grammar-start new-cfg)))))

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
#;(define (group-lhs rules)
    (if (empty? rules)
        '()
        (let* ((current-rule (car rules))
               (current-lhs (car current-rule))
               (new-current-list (filter (lambda (x) (equal? current-lhs (car x))) rules))
               (tovisit-list (filter (lambda (x) (not (equal? current-lhs (car x)))) rules)))
          (append new-current-list
                  (group-lhs tovisit-list)))))

;.................................................

;; cfg -> cfg
;; Purpose: Convert a given cfg to A-n representation
#;(define (convert-to-A-rep cfg)
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

#;(convert-to-A-rep CFG6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An config is a struct that contains:
;; - a nonterminal (nt)
;; - a number (n)
;; - a representation (rep)
;; - rules (rules)
(define-struct config (nt n rep rules) #:transparent) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> (listof config)
;; Purpose: Given a cfg, create a list of configs
(define (make-configs cfg)
  (let* ((cfg-rules (grammar-rules cfg)))
    ;; (listof rule) integer -> (listof config)
    ;; Purpose: Convert the nts in the rules to A-n representation
    (define (make-A-rules rules n)
      (if (empty? rules)
          '()
          (let* ((rule (car rules))
                 (same-lhs-rules (filter (lambda (x) (equal? (car rule) (car x))) (cdr rules)))
                 (different-lhs-rules (filter (lambda (x) (not (equal? (car rule) (car x)))) (cdr rules)))
                 (rhs (append (list (caddr rule))
                              (map (lambda (x) (caddr x)) same-lhs-rules)))
                 (new-config (config (car rule) n (string->symbol (string-append "A-" (number->string n))) rhs)))
            (cons new-config
                  (make-A-rules different-lhs-rules (add1 n))))))
    (make-A-rules cfg-rules 0)))

;; Tests
(check-equal? (make-configs CFG6)
              (list (config 'A 0 'A-0 '(DA a))
                    (config 'B 1 'A-1 '(AC BD b))
                    (config 'C 2 'A-2 '(DB c))
                    (config 'D 3 'A-3 '(AD d))))

;.................................................

;; cfg (listof config) -> (listof config)
;; Purpose: Convert the right hands sides to be in proper A-notation and split up in lists
(define (convert-rhs-to-config cfg loA)
  ;; config -> (listof (listof symbol))
  ;; Purpose: Create new rhs lists, all elems are in A-notation and split up into sublists
  (define (convert-1config A)
    (let* ((rhs (config-rules A)))
      ;; symbol -> (listof symbol)
      ;; Purpose: Convert each rule on the right hand side
      (define (convert-1rhs elem)
        ;; (listof symbol) -> (listof symbol)
        ;; Purpose: Convert a list of symbols to A-notation
        (define (convert-to-A los)
          (cond ((empty? los) '())
                ((or (terminal? cfg (car los))
                     (equal? EMP (car los))) (cons (car los) (convert-to-A (cdr los))))
                (else
                 (let ((corresponding-A (car (filter (lambda (x) (equal? (car los) (config-nt x))) loA))))
                   (cons (config-rep corresponding-A) (convert-to-A (cdr los)))))))
        (if (or (terminal? cfg elem)
                (equal? EMP elem))
            (list elem)
            (convert-to-A (symbol->fsmlos elem))))
      (map convert-1rhs rhs)))
  (let ((new-rhs (map convert-1config loA)))
    (map (lambda (A rules) (config (config-nt A)
                                   (config-n A)
                                   (config-rep A)
                                   rules))
         loA new-rhs)))

;; Tests
(check-equal? (convert-rhs-to-config
               CFG6
               (list (config 'A 0 'A-0 '(DA a))
                     (config 'B 1 'A-1 '(AC BD b))
                     (config 'C 2 'A-2 '(DB c))
                     (config 'D 3 'A-3 '(AD d))))
              (list (config 'A 0 'A-0 '((A-3 A-0) (a)))
                    (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b)))
                    (config 'C 2 'A-2 '((A-3 A-1) (c)))
                    (config 'D 3 'A-3 '((A-0 A-3) (d)))))

;"Configs:"
#;(convert-rhs-to-config CFG6
                         (list (config 'A 0 'A-0 '(DA a))
                               (config 'B 1 'A-1 '(AC BD b))
                               (config 'C 2 'A-2 '(DB c))
                               (config 'D 3 'A-3 '(AD d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol -> (listof config)
;; Purpose: Find the respective config for an A-n symbol
(define (get-config rep configs)
  (car (filter (lambda (x) (equal? rep (config-rep x))) configs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions
;; 1. Convert structs one by one
;; 2. Write substitute function
;; 3. Write introduce-new-rules function
;; 4. Combine in new cfg

;; cfg config (listof (listof symbol)) (listof (listof symbol)) (listof config) (listof config) -> (listof config)
;; Purpose: Given one config, convert its rules to Greibach by
;;          substituting and introducing new rules in B-notation
(define (surgery-on-A cfg A rules new-rules new-A As)

  ;; symbol (listof rule) -> (listof (listof rule))
  ;; Purpose: Substitute a given symbol with its rules
  (define (substitute A-to-sub rest-rules)
    (let ((A-to-sub-rules (config-rules (get-config A-to-sub As)))) 
      (map (lambda (x) (append x rest-rules)) A-to-sub-rules)))

  ;; symbol (listof rule) -> (listof config)
  ;; Purpose: Introduce new configs 
  (define (introduce-new-configs A-to-sub rest-rules)
    (let* ((new-nt (string->symbol (string-append "B-" (number->string (config-n (get-config A-to-sub As))))) #;A-to-sub)
           (new-n #f #;(config-n (get-config A-to-sub As)))
           (new-rep (string->symbol (string-append "B-" (number->string (config-n (get-config A-to-sub As))))))
           (new-rules (cons (append rest-rules (list new-rep))
                            (list rest-rules))))
      (list (config new-nt new-n new-rep new-rules))))

  (cond ((empty? rules)
         (cons (config (config-nt A) (config-n A) (config-rep A) (remove-duplicates new-rules)) new-A))
        ((or (terminal? cfg (car (car rules)))
             (equal? EMP (car (car rules)))
             (< (config-n A)
                (config-n (get-config (car (car rules)) As))))
         (surgery-on-A cfg
                       A
                       (cdr rules)
                       (append (list (car rules)) new-rules)
                       new-A
                       As))
        ((> (config-n A)
            (config-n (get-config (car (car rules)) As)))
         (let ((first-sym (car (car rules)))
               (rest-syms (cdr (car rules))))
           (surgery-on-A cfg
                         A
                         (append (cdr rules) (substitute first-sym rest-syms))
                         new-rules
                         new-A
                         As)))
        (else
         (let ((new-rep (list (string->symbol (string-append "B-" (number->string (config-n A)))))))
           (surgery-on-A cfg
                         A
                         (append (cdr rules) (map (lambda (x) (append x new-rep)) (append (cdr rules) new-rules)))
                         new-rules
                         (append (introduce-new-configs (car (car rules)) (cdr (car rules))) new-A)
                         As)))))

;.................................................

;; (listof config) -> (listof config)
;; Purpose: Call surgery function
(define (combine-post-surgery-configs cfg configs acc)
  (if (empty? configs)
      (let ((Bconfigs (filter (lambda (x) (not (config-n x))) acc))
            (other (filter (lambda (x) (config-n x)) acc)))
        (append other (reverse Bconfigs)))
      (let ((new-config (surgery-on-A cfg (car configs) (group cfg (car configs) (append acc configs)) '() '() (append acc configs))))
        (combine-post-surgery-configs cfg (cdr configs) (append new-config acc)))))

;.................................................

;; (listof (listof symbol)) -> (listof (listof symbol))
;; Purpose: Group from terminal/empty to lowest j to highest j
(define (group cfg conf As)
  (let* ((rules (config-rules conf))
         (terminal/emp (filter (lambda (x) (or (terminal? cfg (car x))
                                               (equal? EMP (car x)))) rules))
         (other (filter (lambda (x) (not (or (terminal? cfg (car x))
                                             (equal? EMP (car x))))) rules))
         (i=j (filter (lambda (x) (= (config-n conf) (config-n (get-config (car x) As)))) other))
         (other2 (filter (lambda (x) (not (= (config-n conf) (config-n (get-config (car x) As))))) other)))
    (append terminal/emp other2 i=j)))

;.................................................

(define As2
  (list (config 'A 0 'A-0 '((A-3 A-0) (a)))
        (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b)))
        (config 'C 2 'A-2 '((A-3 A-1) (c)))
        (config 'D 3 'A-3 '((A-0 A-3) (d)))))

#|"A:"
(surgery-on-A CFG6
              (config 'A 0 'A-0 '((A-3 A-0) (a)))
              (group CFG6 (config 'A 0 'A-0 '((A-3 A-0) (a))) As2)
              '()
              '()
              As2)
"B:"
(surgery-on-A CFG6
              (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b)))
              (group CFG6 (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b))) As2)
              '()
              '()
              As2)
"C:"
(surgery-on-A CFG6
              (config 'C 2 'A-2 '((A-3 A-1) (c)))
              (group CFG6 (config 'C 2 'A-2 '((A-3 A-1) (c))) As2)
              '()
              '()
              As2)
"D:"
(surgery-on-A CFG6
              (config 'D 3 'A-3 '((A-0 A-3) (d)))
              (group CFG6 (config 'D 3 'A-3 '((A-0 A-3) (d))) As2)
              '()
              '()
              As2) |#

;.................................................
;; Tests

#;(define As2
    (list (config 'A 0 'A-0 '((A-3 A-0) (a)))
          (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b)))
          (config 'C 2 'A-2 '((A-3 A-1) (c)))
          (config 'D 3 'A-3 '((A-0 A-3) (d)))))

(check-equal?
 (surgery-on-A CFG6
               (config 'A 0 'A-0 '((A-3 A-0) (a)))
               (group CFG6 (config 'A 0 'A-0 '((A-3 A-0) (a))) As2)
               '()
               '()
               As2)
 (list (config 'A 0 'A-0 '((A-3 A-0) (a)))))

(check-equal?
 (surgery-on-A CFG6
               (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b)))
               (group CFG6 (config 'B 1 'A-1 '((A-0 A-2) (A-1 A-3) (b))) As2)
               '()
               '()
               As2)
 (list (config 'B 1 'A-1 '((b B-1) (a A-2 B-1) (A-3 A-0 A-2 B-1) (a A-2) (A-3 A-0 A-2) (b)))
       (config 'B-1 #f 'B-1 '((A-3 B-1) (A-3)))))

(check-equal?
 (surgery-on-A CFG6
               (config 'C 2 'A-2 '((A-3 A-1) (c)))
               (group CFG6 (config 'C 2 'A-2 '((A-3 A-1) (c))) As2)
               '()
               '()
               As2)
 (list (config 'C 2 'A-2 '((A-3 A-1) (c)))))

(check-equal?
 (surgery-on-A CFG6
               (config 'D 3 'A-3 '((A-0 A-3) (d)))
               (group CFG6 (config 'D 3 'A-3 '((A-0 A-3) (d))) As2)
               '()
               '()
               As2)
 (list (config 'D 3 'A-3 '((d B-3) (a A-3 B-3) (a A-3) (d)))
       (config 'B-3 #f 'B-3 '((A-0 A-3 B-3) (A-0 A-3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal?
 (combine-post-surgery-configs CFG6 As2 '())
 (list
  (config 'D 3 'A-3 '((d B-3) (a A-3 B-3) (a A-3) (d)))
  (config 'C 2 'A-2 '((A-3 A-1) (c)))
  (config 'B 1 'A-1 '((b B-1) (a A-2 B-1) (A-3 A-0 A-2 B-1) (a A-2) (A-3 A-0 A-2) (b)))
  (config 'A 0 'A-0 '((A-3 A-0) (a)))
  (config 'B-1 #f 'B-1 '((A-3 B-1) (A-3)))
  (config 'B-3 #f 'B-3 '((A-0 A-3 B-3) (A-0 A-3)))))

;"Mid-Result:"
;(combine-post-surgery-configs CFG6 As2 '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) (listof config) -> (listof config)
;; Purpose: Final substitutions
(define (final-subs cfg configs As)

  ;; symbol (listof rule) -> (listof (listof rule))
  ;; Purpose: Substitute a given symbol with its rules
  (define (substitute A-to-sub rest-rules)
    (let ((A-to-sub-rules (config-rules (get-config A-to-sub As)))) 
      (map (lambda (x) (append x rest-rules)) A-to-sub-rules)))

  ;; (listof (listof symbol)) -> (listof (listof symbol))
  ;; Purpose: Substitute if needed 
  (define (make-new-rules rules)
    (cond ((empty? rules) '())
          ((or (terminal? cfg (car (car rules)))
               (equal? EMP (car (car rules))))
           (cons (car rules) (make-new-rules (cdr rules))))
          (else 
           (append (substitute (car (car rules)) (cdr (car rules)))
                   (make-new-rules (cdr rules))))))

  (if (empty? configs)
      '()
      (let* ((c (car configs))
             (rules (config-rules c))
             (new-rules (make-new-rules rules))
             (new-config (config (config-nt c) (config-n c) (config-rep c) new-rules)))
        (cons new-config
              (final-subs cfg (cdr configs) (append (cdr As) (list new-config))))))) 


(define CFG6-greibach-As
  (list
   (config 'D 3 'A-3 '((d B-3) (a A-3 B-3) (a A-3) (d)))
   (config 'C 2 'A-2 '((d B-3 A-1) (a A-3 B-3 A-1) (a A-3 A-1) (d A-1) (c)))
   (config
    'B
    1
    'A-1
    '((b B-1)
      (a A-2 B-1)
      (d B-3 A-0 A-2 B-1)
      (a A-3 B-3 A-0 A-2 B-1)
      (a A-3 A-0 A-2 B-1)
      (d A-0 A-2 B-1)
      (a A-2)
      (d B-3 A-0 A-2)
      (a A-3 B-3 A-0 A-2)
      (a A-3 A-0 A-2)
      (d A-0 A-2)
      (b)))
   (config 'A 0 'A-0 '((d B-3 A-0) (a A-3 B-3 A-0) (a A-3 A-0) (d A-0) (a)))
   (config
    'B-1
    #f
    'B-1
    '((d B-3 B-1) (a A-3 B-3 B-1) (a A-3 B-1) (d B-1) (d B-3) (a A-3 B-3) (a A-3) (d)))
   (config
    'B-3
    #f
    'B-3
    '((d B-3 A-0 A-3 B-3)
      (a A-3 B-3 A-0 A-3 B-3)
      (a A-3 A-0 A-3 B-3)
      (d A-0 A-3 B-3)
      (a A-3 B-3)
      (d B-3 A-0 A-3)
      (a A-3 B-3 A-0 A-3)
      (a A-3 A-0 A-3)
      (d A-0 A-3)
      (a A-3)))))

(check-equal? 
 (final-subs CFG6
             (combine-post-surgery-configs CFG6 As2 '())
             (combine-post-surgery-configs CFG6 As2 '()))
 CFG6-greibach-As)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) (listof config) -> (listof config)
;; Purpose: Convert back to regular notation
(define (convert-back-to-regular-notation cfg configs As)
  
  ;; symbol -> symbol
  ;; Purpose: Substitute a symbol in A-notation back to regular notation
  (define (sub A-to-sub)
    (config-nt (get-config A-to-sub As)))

  ;; (listof symbol) -> (listof symbol)
  ;; Purpose: Sub each rule independently 
  (define (make-new-rule rule)
    (cond ((empty? rule) '())
          ((or (terminal? cfg (car rule))
               (equal? EMP (car rule)))
           (cons (car rule) (make-new-rule (cdr rule))))
          (else (cons (sub (car rule)) (make-new-rule (cdr rule))))))

  (if (empty? configs)
      '()
      (let* ((c (car configs))
             (rules (config-rules c))
             (new-rules (map make-new-rule rules))
             (new-config (config (config-nt c) (config-n c) (config-rep c) new-rules)))
        (cons new-config
              (convert-back-to-regular-notation cfg (cdr configs) As)))))

;(convert-back-to-regular-notation CFG6 CFG6-greibach-As CFG6-greibach-As)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) -> cfg
;; Purpose: Convert a list of configs to a cfg
(define (configs->cfg cfg configs)
  (let* ((new-nts (map config-nt configs))
         (new-rules
          (remove-duplicates
           (append-map (lambda (conf)
                         (map (lambda (rule) `(,(config-nt conf) -> ,(list->symbol rule))) (config-rules conf)))
                       configs))))
    (make-cfg new-nts
              (grammar-sigma cfg)
              new-rules
              (grammar-start cfg))))

;(configs->cfg CFG6 (convert-back-to-regular-notation CFG6 CFG6-greibach-As CFG6-greibach-As))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> cfg
;; Purpose: Convert a cfg to Greibach Normal Form
(define (greibach cfg)
  (let* ((chomsky-form (chomsky cfg))
         (init-As (convert-rhs-to-config chomsky-form (make-configs chomsky-form)))
         (As (final-subs chomsky-form
                         (combine-post-surgery-configs chomsky-form init-As '())
                         (combine-post-surgery-configs chomsky-form init-As '()))))
    (configs->cfg chomsky-form
                  (convert-back-to-regular-notation chomsky-form
                                                    As
                                                    As))))

;(greibach CFG6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

