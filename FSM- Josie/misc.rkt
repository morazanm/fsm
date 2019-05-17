; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module misc racket
  (require "string.rkt" "constants.rkt")
  (provide remove-duplicates los->symbol generate-symbol symbol-upcase equiv-los? 
           not-in-l2 symbol->list sublist subst-in-list gen-symbol get-differences
           symbol->fsmlos)
  
  ; string --> los
(define (string->fsmlos str)
  
  (define (end-of-fsm-suffix i)
    (cond [(= i (string-length str)) (- i 1)]
          [(or (string=? (substring str i (+ i 1)) "-")
               (and (string>=? (substring str i (+ i 1)) "0")
                    (string<=? (substring str i (+ i 1)) "9")))
           (end-of-fsm-suffix (+ i 1))]
          [else (- i 1)]))
  
  (define (helper i)
    
    ;(define d (if (< i (string-length str)) (displayln (substring str (+ i 1))) 0))
    
    (cond [(= i (string-length str)) null]
          [(= i (sub1 (string-length str))) 
           (let ((substr (substring str i (+ i 1)))) 
             (if (string=? substr (symbol->string EMP))
                 (list EMP)
                 (list (string->symbol substr))))]
          [(string=? "-" (substring str (+ i 1) (+ i 2)))
           (let* ((eos (end-of-fsm-suffix (+ i 1))))
             (cons (string->symbol (substring str i (+ eos 1)))
                   (helper (+ eos 1))))]
          [else (cons (string->symbol (substring str i (+ i 1)))
                      (helper (+ i 1)))]))
  (helper 0))
  
  ; symbol -> (listof fsm-symbol)
  (define (symbol->fsmlos s) (string->fsmlos (symbol->string s)))
  
  ; (listof symb) (listof symb) (listof words) --> (listof words)
  (define (get-differences r1 r2 low)
    (filter (lambda (n) (not (number? n)))
            (map (lambda (a b c) (if (equal? a b) 0 c))
                 r1 
                 r2
                 low)))
  
  ; (listof X) --> (listof X)
  (define (remove-duplicates r)
    (cond [(null? r) '()]
          [(member (car r) (cdr r)) (remove-duplicates (cdr r))]
          [else (cons (car r) (remove-duplicates (cdr r)))]))
  
  ; (listof symbol) --> symbol
  (define (los->symbol l)
    (string->symbol (lostr->string (los->lostr l))))
  
  ; symbol --> (listof symbol)
  (define (symbol->list s) 
    (let ((str (symbol->string s)))
      (build-list (string-length str) 
                  (lambda (i) (string->symbol (substring str i (+ i 1)))))))
  
  ; symbol (listof symbol) --> symbol
  (define (generate-symbol s los)
    (let ((new-symb (if (member s los)
                        (string->symbol (string-append (symbol->string (gensym (string->symbol (string-append (symbol->string s) "-"))))))
                        s)))
      (if (not (member new-symb los))
          new-symb
          (generate-symbol s los))))
  
  ; symbol -> symbol
  (define (symbol-upcase s) (string->symbol (string-upcase (symbol->string s))))
  
  ; (listof symbol) (listof symbol) --> boolean
  (define (equiv-los? l1 l2)
    (and (= (length l1) (length l2)) (null? (not-in-l2 l1 l2)) (null? (not-in-l2 l2 l1))))
  
  ; (listof symbol) (listof symbol) --> (listof symbol)
  ; Purpose: Return the symbols of the first list that are not in the second list
  (define (not-in-l2 l1 l2)
    (cond [(null? l1) '()]
          [(member (car l1) l2) (not-in-l2 (cdr l1) l2)]
          [else (cons (car l1) (not-in-l2 (cdr l1) l2))]))
  
  ; (listof X) natnum length --> (listof X) or error
  (define (sublist L i n)
    (define (get-n sl k)
      (if (= k 0) '() (cons (car sl) (get-n (cdr sl) (- k 1)))))
    (if (< (- (length L) i) n)
        (error (format "sublist: List too short. L = ~s, i = ~s, n = ~s" L i n))
        (get-n (list-tail L i) n)))
  
  ; (listof X) i --> (listof X)
  ; Purpose: returns the list of elements in the given list from 0..i-1
  (define (list-head w i)
    
    (define (helper ls k)
      (if (= k i) '() (cons (car ls) (helper (cdr ls) (+ k 1)))))
    
    (cond [(= i (length w))
           (error (format "list-head: invalid index, ~s, for the list ~s" i w))]
          [else (helper w 0)]))
  
  ; (listof symbol) natnum natnum (listof symbol) --> (listof symbol)
  (define (subst-in-list w i n tosub)
    (append (list-head w i) tosub (list-tail w (+ i n)))
    )
  
  ; symbol (listof symbol) --> symbol
  (define (gen-symbol  seed sigma)
    (define (helper)
      (let ((temp (generate-symbol seed sigma)))
        (if (member temp sigma)
            (helper)
            temp)))
    (if (not (member seed sigma))
        seed
        (helper)))
  
  ) ; closes module