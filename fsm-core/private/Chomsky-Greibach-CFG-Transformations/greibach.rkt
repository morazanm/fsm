#lang racket/base
(require "chomsky.rkt"
         "../grammar-getters.rkt"
         "../misc.rkt"
         "../constants.rkt"
         "../cfg-struct.rkt"
         racket/list)
(provide greibach)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol (listof symbol) -> boolean
;; Purpose: Check whether list contains given symbol
(define (contains? elem list)
  (cond ((null? list) #f)
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
    (if (null? l)
        ""
        (string-append (symbol->string (car l)) 
                       (list->symbol-helper (cdr l)))))
  (string->symbol (list->symbol-helper list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A config is a struct that contains:
;; - a nonterminal in the original grammar (nt)
;; - a unique number to rename the nts in the original grammar (n)
;; - a representation = renamed nt (rep)
;; - renamed rhs of rules for rep (rules)
(define-struct config (nt n rep rules) #:transparent)

;; renamed-config, newnt, rhss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg -> (listof config)
;; Purpose: Given a cfg, create a list of configs
(define (make-configs cfg)
  (let* ((cfg-rules (grammar-rules cfg)))
    ;; (listof rule) integer -> (listof config)
    ;; Purpose: Convert the nts in the rules to A-n representation
    ;;          without updating the rhss
    ;; Accumulator invariants:
    ;;  n = next natnum for renaming nts
    (define (make-A-rules rules n)
      (if (null? rules)
          '()
          (let* ((rule (car rules))
                 (same-lhs-rules (filter (lambda (x) (equal? (car rule) (car x))) (cdr rules)))
                 (different-lhs-rules (filter (lambda (x) (not (equal? (car rule) (car x)))) (cdr rules)))
                 (rhss (append (list (caddr rule))
                               (map (lambda (x) (caddr x)) same-lhs-rules)))
                 (new-config (config (car rule) n (string->symbol (string-append "A-" (number->string n))) rhss)))
            (cons new-config
                  (make-A-rules different-lhs-rules (add1 n))))))
    (make-A-rules cfg-rules 0)))

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
          (cond ((null? los) '())
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
    (let* ((new-nt (string->symbol (string-append "B-" (number->string (config-n (get-config A-to-sub As))))))
           (new-n #f)
           (new-rep (string->symbol (string-append "B-" (number->string (config-n (get-config A-to-sub As))))))
           (new-rules (filter (lambda (x) (or (< (length x) 2)
                                              (not (equal? new-rep (last (drop-right x 1))))))
                              (cons (append rest-rules (list new-rep))
                                    (list rest-rules)))))
      (list (config new-nt new-n new-rep new-rules))))

  (if (null? rules)
      (cons (config (config-nt A) (config-n A) (config-rep A) (remove-duplicates new-rules)) new-A)
      (let* ((first-rhs (car (car rules)))
             (i (config-n A)))
        (cond ((or (terminal? cfg first-rhs)
                   (equal? EMP first-rhs)
                   (< i
                      (config-n (get-config first-rhs As))))
               (surgery-on-A cfg
                             A
                             (cdr rules)
                             (append (list (car rules)) new-rules)
                             new-A
                             As))
              ((> i
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
                               (filter (lambda (x) (or (< (length x) 2)
                                                       (not (equal? (car new-rep) (last (drop-right x 1))))))
                                       (append (cdr rules) (map (lambda (x) (append x new-rep)) (append (cdr rules) new-rules))))
                               new-rules
                               (append (introduce-new-configs (car (car rules)) (cdr (car rules))) new-A)
                               As)))))))

;.................................................

;; (listof config) -> (listof config)
;; Purpose: Call surgery function
(define (combine-post-surgery-configs cfg configs acc)
  (if (null? configs)
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
    (cond ((null? rules) '())
          ((or (terminal? cfg (car (car rules)))
               (equal? EMP (car (car rules))))
           (cons (car rules) (make-new-rules (cdr rules))))
          (else 
           (append (substitute (car (car rules)) (cdr (car rules)))
                   (make-new-rules (cdr rules))))))

  (if (null? configs)
      '()
      (let* ((c (car configs))
             (rules (config-rules c))
             (new-rules (make-new-rules rules))
             (new-config (config (config-nt c) (config-n c) (config-rep c) new-rules)))
        (cons new-config
              (final-subs cfg (cdr configs) (append (cdr As) (list new-config))))))) 

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
    (cond ((null? rule) '())
          ((or (terminal? cfg (car rule))
               (equal? EMP (car rule)))
           (cons (car rule) (make-new-rule (cdr rule))))
          (else (cons (sub (car rule)) (make-new-rule (cdr rule))))))

  (if (null? configs)
      '()
      (let* ((c (car configs))
             (rules (config-rules c))
             (new-rules (map make-new-rule rules))
             (new-config (config (config-nt c) (config-n c) (config-rep c) new-rules)))
        (cons new-config
              (convert-back-to-regular-notation cfg (cdr configs) As)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfg (listof config) -> cfg
;; Purpose: Convert a list of configs to a cfg
(define (configs->cfg cfg configs)
  (let* ((new-nts (remove-duplicates (map config-nt configs)))
         (new-rules
          (remove-duplicates
           (append-map (lambda (conf)
                         (map (lambda (rule) `(,(config-nt conf) -> ,(list->symbol rule))) (config-rules conf)))
                       configs))))
    (make-unchecked-cfg new-nts
                        (grammar-sigma cfg)
                        new-rules
                        (grammar-start cfg))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







 



