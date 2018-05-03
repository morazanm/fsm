; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module string racket
  (provide lostr->lostate lostr->string sort-strings los->lostr sort-symbols)
  
  ; (listof string) --> string
  (define (lostr->string l)
    (cond [(null? l) ""]
          [else (string-append (car l) (lostr->string (cdr l)))]))
  
  ; (listof string) --> (listof string)
  (define (sort-strings l)
    ; string (sorted (listof string)) --> (sorted (listof string))
    (define (insert s l)
      (cond [(null? l) (list s)]
            [(string<? s (car l)) (cons s l)]
            [else (cons (car l) (insert s (cdr l)))]))
    (cond [(null? l) '()]
          [else (insert (car l) (sort-strings (cdr l)))]))
  
  ; (listof string) --> (listof state)
(define (lostr->lostate l) (map string->symbol l))
  
   ; (listof state) --> (listof string)
  (define (los->lostr ss) (map symbol->string ss))
  
  ; los --> los
  (define (sort-symbols los)
    (lostr->lostate (sort-strings (los->lostr los))))
  )