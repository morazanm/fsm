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
           singleton-regexp singleton-regexp? singleton-regexp-a
           concat-regexp concat-regexp? concat-regexp-r1 concat-regexp-r2
           union-regexp union-regexp? union-regexp-r1 union-regexp-r2
           kleenestar-regexp kleenestar-regexp? kleenestar-regexp-r1
           printable-regexp
           NULL-REGEXP-STRING EMPTY-REGEXP-STRING
           simplify-regexp
           )
  
  (define NULL-REGEXP-STRING "()")
  (define EMPTY-REGEXP-STRING (symbol->string EMP))
  
  ;   --> regexp
  (struct null-regexp () #:transparent) ;;; represents no transition
  
  ; --> empty-regexp
  (struct empty-regexp () #:transparent) ;;; represents empty transition
  
  ; single character string --> singleton-regexp
  (struct singleton-regexp (a) #:transparent)
  
  ; regexp regexp --> concat-regexp
  (struct concat-regexp (r1 r2) #:transparent)
  
  ; regexp regexp --> union-regexp
  (struct union-regexp (r1 r2) #:transparent)
  
  ; regexp --> kleenestar-regexp
  (struct kleenestar-regexp (r1) #:transparent)
  
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
          
          
  
  ;regexp --> regexp
  (define (simplify-regexp r)
    (cond [(null-regexp? r) r]
          [(empty-regexp? r) r]
          [(singleton-regexp? r) r]
          [(concat-regexp? r) 
           (let ((r1 (simplify-regexp (concat-regexp-r1 r)))
                 (r2 (simplify-regexp (concat-regexp-r2 r))))
             (cond [(or (null-regexp? r1) (null-regexp? r2)) (null-regexp)]
                   [(empty-regexp? r1) r2]
                   [(empty-regexp? r2) r1]
                   [(and (kleenestar-regexp? (next-concat-element r2))
                         (equal? r1 (kleenestar-regexp-r1 (next-concat-element r2))) 
                         (union-regexp? r1)
                         (union-has-empty? r1))
                    r2]
                   [(and (kleenestar-regexp? (next-concat-element r1))
                         (equal? r2 (kleenestar-regexp-r1 (next-concat-element r1))) 
                         (union-regexp? r2)
                         (union-has-empty? r2))
                    r1]
                   [else (concat-regexp r1 r2)]))]
          [(union-regexp? r) 
           (let ((r1 (simplify-regexp (union-regexp-r1 r)))
                 (r2 (simplify-regexp (union-regexp-r2 r))))
             (cond [(null-regexp? r1) r2]
                   [(null-regexp? r2) r1]
                   [(and (empty-regexp? r1) (kleenestar-regexp? r2)) r2]
                   [(and (empty-regexp? r2) (kleenestar-regexp? r1)) r1]
                   [(equal? r1 r2) r1]
                   [else (union-regexp r1 r2)]))]
          [(kleenestar-regexp? r) 
           (let ((r1 (simplify-regexp (kleenestar-regexp-r1 r))))
             (cond [(empty-regexp? r1) r1]
                   [(null-regexp? r1) (empty-regexp)]; is this right?
                   [else (kleenestar-regexp r1)]))]))
  
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
  
  
 
  ;;; END REGULAR EXPRESSIONS
  
  (define test (concat-regexp (null-regexp) 
                              (concat-regexp (kleenestar-regexp (union-regexp (empty-regexp) 
                                                                              (singleton-regexp "a"))) 
                                             (singleton-regexp "b")))) 
  
  
  )