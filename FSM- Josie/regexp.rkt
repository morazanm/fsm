; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

;;; REGULAR EXPRESSIONS

; A regular expression (regexp) is either
;  1. A singleton-regexp
;  2. A concat-regexp
;  3. A union-regexp
;  4. A kleenestar-regexp

(module regexp racket
  (require "constants.rkt")
  (provide null-regexp null-regexp? 
           empty-regexp empty-regexp? 
           make-unchecked-singleton singleton-regexp? singleton-regexp-a
           make-unchecked-concat concat-regexp? concat-regexp-r1 concat-regexp-r2
           make-unchecked-union union-regexp? union-regexp-r1 union-regexp-r2
           make-unchecked-kleenestar kleenestar-regexp? kleenestar-regexp-r1
           printable-regexp
           NULL-REGEXP-STRING EMPTY-REGEXP-STRING
           simplify-regexp
           regexp?
           )
  
  (define NULL-REGEXP-STRING "()")
  (define EMPTY-REGEXP-STRING (symbol->string EMP))
  
  ;   --> regexp
  (struct null-regexp () #:transparent) ;;; represents no transition
  
  ; --> empty-regexp
  (struct empty-regexp () #:transparent) ;;; represents empty transition
  
  ; single character string --> singleton-regexp
  (struct singleton-regexp (a) #:transparent)
  (define (make-unchecked-singleton a)
    (singleton-regexp a)
    )
  
  ; regexp regexp --> concat-regexp
  (struct concat-regexp (r1 r2) #:transparent)
  (define (make-unchecked-concat a b)
    (concat-regexp a b)
    )
  
  ; regexp regexp --> union-regexp
  (struct union-regexp (r1 r2) #:transparent)
  (define (make-unchecked-union a b)
    (union-regexp a b)
    )
  
  ; regexp --> kleenestar-regexp
  (struct kleenestar-regexp (r1) #:transparent)
  (define (make-unchecked-kleenestar a)
    (kleenestar-regexp a)
    )
  
  (define R1 (singleton-regexp "a"))
  (define R2 (singleton-regexp "b"))
  (define R3 (union-regexp  R1 R2))
  (define R4 (kleenestar-regexp R3))
  (define R5 (concat-regexp R2 R4))
  (define R6 (empty-regexp))
  (define R7 (null-regexp))
  
  ; union-regexp --> boolean
  (define (union-has-empty? r)
    (let ((r1 (union-regexp-r1 r))
          (r2 (union-regexp-r2 r)))
      (or (empty-regexp? r1) 
          (empty-regexp? r1))))
  
  (define (next-concat-element r)
    (cond [(null-regexp? r) r]
          [(singleton-regexp? r) r]
          [(union-regexp? r) r]
          [(kleenestar-regexp? r) r]
          [(concat-regexp? r) (next-concat-element (concat-regexp-r1 r))]))
          
          
  
  ;simplify-regexp: regexp --> regexp
  ;purpose: to simplify a regexp
  (define (simplify-regexp x)
    (local [;simplify-kleenestar: kleenestar --> regexp
            ;purpose: to simplify a kleenestar
            (define (simplify-kleenestar x)
              (local [(define simp-x (simplify-regexp (kleenestar-regexp-r1 x)))]
                (cond [(or (kleenestar-regexp? simp-x)
                           (empty-regexp? simp-x)) simp-x]
                      [else (kleenestar-regexp simp-x)])))

            ;simplify-concat: concat --> regexp
            ;purpose: to simplify a concat-regexp
            (define (simplify-concat x)
              (local [(define simp-lhs (simplify-regexp (concat-regexp-r1 x)))
                      (define simp-rhs (simplify-regexp (concat-regexp-r2 x)))]
                (cond [(empty-regexp? simp-lhs) simp-rhs]
                      [(empty-regexp? simp-rhs) simp-lhs]
                      [else (concat-regexp simp-lhs simp-rhs)])))
            ]
      (cond [(or (empty-regexp? x)
                 (null-regexp? x)
                 (singleton-regexp? x)) x]
            [(kleenestar-regexp? x) (simplify-kleenestar x)]
            [(concat-regexp? x) (simplify-concat x)]
            [(union-regexp? x) (simplify-union x)])))




  ;simplify-union: union --> regexp
  ;purpose: to simplify a union-regexp
  (define (simplify-union x)
    (local [;simplify the lhs and rhs
            (define simp-lhs (simplify-regexp (union-regexp-r1 x)))
            (define simp-rhs (simplify-regexp (union-regexp-r2 x)))

            ;return-compnants: regexp --> (listof regexp)
            ;purpose: return all the individual componants of a regexp
            (define (return-componants x)
              (cond [(or (empty-regexp? x)
                         (null-regexp? x)) empty]
                    [(or (singleton-regexp? x)
                         (kleenestar-regexp? x)
                         (concat-regexp? x)) (list x)]
                    [(union-regexp? x) (append (return-componants (union-regexp-r1 x))
                                               (return-componants (union-regexp-r2 x)))]
                    )
              )

            ;remove-duplicates: (listof regexp) --> (listof regexp)
            ;purpose: to remove the duplicates from a list of regular expressions
            (define (remove-duplicates a-list)
              (cond [(empty? a-list) empty]
                    [(or (member (first a-list) (rest a-list))
                         (ormap (lambda (x) (and (kleenestar-regexp? x)
                                                 (member (first a-list) (return-componants (kleenestar-regexp-r1 x))))) (rest a-list)))                              
                     (remove-duplicates (rest a-list))]
                    [(kleenestar-regexp? (first a-list)) (local [(define comps (return-componants (kleenestar-regexp-r1 (first a-list))))]
                                                           (cons (first a-list)
                                                                 (remove-duplicates (filter (lambda (x) (member x comps)) (rest a-list)))))]
                    [else (cons (first a-list) (remove-duplicates (rest a-list)))]))
          
            ;remove the duplicates from the union
            (define clean-list (remove-duplicates (reverse (remove-duplicates (append (return-componants simp-lhs)
                                                                                      (return-componants simp-rhs))))))

            ;re-union: (listof regexp) --> regexp
            ;purpose: to recreate a union-regexp
            (define (re-union a-list)
              (cond [(empty? a-list) (empty-regexp)]
                    [(empty? (rest a-list)) (first a-list)]
                    [else (union-regexp (first a-list) (re-union (rest a-list)))]))

          
            ;reunify the cleaned list
            (define clean-union (re-union clean-list))
            ]
      clean-union
      )
    )
  
  ; regexp --> string
  (define (printable-regexp r)
    ; regexp --> string
    (define (helper r)
      (cond [(null-regexp? r) NULL-REGEXP-STRING]
            [(empty-regexp? r) EMPTY-REGEXP-STRING]
            [(singleton-regexp? r) (singleton-regexp-a r)]
            [(concat-regexp? r) 
             (let ((r1 (helper (concat-regexp-r1 r)))
                   (r2 (helper (concat-regexp-r2 r))))
               (string-append r1 r2))]
            [(union-regexp? r) 
             (let ((r1 (helper (union-regexp-r1 r)))
                   (r2 (helper (union-regexp-r2 r))))
               (string-append "(" r1 " U " r2 ")"))]
            [(kleenestar-regexp? r) 
             (let ((r1 (helper (kleenestar-regexp-r1 r))))
               (string-append r1 "*"))]))
    (helper r))

  ;regexp?: element --> boolean
  (define (regexp? x)
    (or (concat-regexp? x)
        (union-regexp? x)
        (singleton-regexp? x)
        (null-regexp? x)
        (empty-regexp? x)
        (kleenestar-regexp? x)))
  
 
  ;;; END REGULAR EXPRESSIONS
  
  (define test (concat-regexp (null-regexp) 
                              (concat-regexp (kleenestar-regexp (union-regexp (empty-regexp) 
                                                                              (singleton-regexp "a"))) 
                                             (singleton-regexp "b")))) 
  
  
  )