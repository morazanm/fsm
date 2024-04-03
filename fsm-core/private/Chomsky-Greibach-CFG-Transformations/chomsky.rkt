#lang racket
(require "../../interface.rkt")
(provide chomsky rm-empties)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chomsky Normal Form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nts Definitions
;;  S-0: start nts representing the empty word, if it is in the language
;;  Y-n: terminals that are not in chomsky form
;;  X-n: non-terminals that are not in chomsky form

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
      (let ((ts (filter terminal? (symbol->fsmlos rhs))))
        (if (not (empty? ts))
            #f
            (or (eq? EMP rhs)
                (andmap (lambda (x) (contains? x empties)) (symbol->fsmlos rhs))))))  
    ;; nts -> (listof nts)
    ;; Purpose: Check if one nt can be replaced by empty
    (define (get-empties-helper nts empties)
      (map (lambda (z) (car z))
           (filter (lambda (y) (leads-to-empty? (caddr y) empties))
                   (filter (lambda (x) (contains? nts (symbol->fsmlos (caddr x)))) r))))
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
                        (make-combs-rhs (list (symbol->fsmlos (caddr rule))) (emp-nts (grammar-rules cfg)) '())))))      
      (map (lambda (x) (list (car rule) '-> (list->symbol x))) rhs)))

  (if (empty? (emp-nts (grammar-rules cfg)))
             cfg
      (let* ((empties (emp-nts (grammar-rules cfg)))
             (new-start (generate-symbol 'S (grammar-nts cfg)))
             (new-rules (if (contains? (grammar-start cfg) empties)
                            (cons `(,new-start -> ε) (cons `(,new-start -> ,(grammar-start cfg))
                                                    (filter (lambda (x) (not (eq? EMP (caddr x))))
                                                            (append-map (lambda (x) (all-combinations x)) (grammar-rules cfg)))))
                            (filter (lambda (x) (not (eq? EMP (caddr x))))
                                    (append-map (lambda (x) (all-combinations x)) (grammar-rules cfg)))))
             (new-nts (remove-duplicates (append (map (lambda (x) (car x)) new-rules)
                                                 (filter nts? (append-map symbol->fsmlos (map (lambda (x) (caddr x))
                                                                                              (grammar-rules cfg)))))))
             (new-start (if (contains? (grammar-start cfg) empties)
                            new-start
                            (grammar-start cfg))))   
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
         (eq? 1 (length (symbol->fsmlos rhs)))))
  
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
        (rm-ketten-helper (make-rules (remove-duplicates rules)))))
  ;; Termination argument:
  ;;  Function removes all Ketten recursively until there are
  ;;  no more. Then, halts.

  (if (no-more-ketten? (grammar-rules cfg))
             cfg
      (let* ((new-rules (remove-duplicates (rm-ketten-helper (grammar-rules cfg))))
             (new-nts (remove-duplicates (append (map (lambda (x) (car x)) new-rules)
                                                 (filter nts? (append-map symbol->fsmlos (map (lambda (x) (caddr x))
                                                                                              (grammar-rules cfg))))))))
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
          (and (= 2 (length (symbol->fsmlos (caddr rule))))
               (nts? (car (symbol->fsmlos (caddr rule))))
               (nts? (cadr (symbol->fsmlos (caddr rule)))))))
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
          (sum-list '() (cons 0 (drop-right (map (lambda (x) (if (> (- (length (symbol->fsmlos (caddr x))) 2) 0)
                                                                 (- (length (symbol->fsmlos (caddr x))) 2)
                                                                 0))
                                                 rules) 1)))))

    ;; rules -> rules
    ;; Purpose: Make new rules that follow chomsky
    (define (make-X-rules rules)
      (append-map (lambda (y z) (new-X y z))
                  (map (lambda (x) `(,(car x) -> ,(replace-ts (symbol->fsmlos (caddr x))))) rules)
                  (ints rules)))

    ;; sigma natnum -> rules
    ;; Purpose: Create new rules for each sigma 
    (define (make-Y-rules rules)
      (let ((sigma (remove-duplicates
                    (filter (lambda (y) (terminal? y))
                            (append-map symbol->fsmlos (map (lambda (x) (caddr x)) rules))))))
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







