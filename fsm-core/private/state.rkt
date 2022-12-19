; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

;;; STATE

; A state is a symbol.

; A superstate is a (listof state)

(module state racket
  (require "misc.rkt" "constants.rkt" "rules.rkt" "string.rkt")
  (provide get-result minus-set empties get-final-super-states compute-P 
           compute-super-states extract-sstates extract-final-ss superstate->state
           union-states)
  
  (define union-states append)
  
  (define (get-result s finals)
    (cond [(member s finals) (list 'accept)]
          [else (list 'reject)]))
  
  ; (listof state) (listof state) --> (listof state)
  (define (minus-set L1 L2) (filter (lambda (s) (not (member s L2))) L1))
  
  ; state (listof rule) --> (listof state)
  (define (empties s rules)
    (define (search visited to-visit result)
      (cond [(null? to-visit) result]
            [else 
             (let* ((next (car to-visit))
                    (rls (filter (lambda (r) (and (eq? (car r) next) (eq? (cadr r) EMP)))
                                 rules))
                    (new-states (filter (lambda (s) (not (member s (append visited to-visit))))
                                        (map (lambda (r) (caddr r)) rls)))
                    )
               (search (cons next visited)
                       (append (cdr to-visit) new-states)
                       (append result new-states)))]))  
    (search '() (list s) (list s)))
  
  ; (listof (listof string)) (listof state)--> (listof (listof string))
  (define (get-final-super-states K finals res)
    (cond [(null? finals) (remove-duplicates res)] ; remove duplicates to keep res a set
          [else
           (let ((new-super-finals (filter (lambda (ss) (member (symbol->string (car finals)) ss)) K)))
             (get-final-super-states K (cdr finals) (append new-super-finals res)))]))
  
  ; (listof string) symbol (listof rules) --> state
  (define (compute-P SS symb rules)
    ; (listof state) --> (listof state)
    (define (helper S)
      (cond [(null? S) '()]
            [else
             (let* ((current-state (car S))
                    (rls (filter-rules current-state symb rules))
                    (ps (remove-duplicates (append-map 
                                            (lambda (s) (empties s rules)) 
                                            (map caddr rls))))
                    )
               (append ps (helper (cdr S))))]))
    (string->symbol 
     (lostr->string 
      (sort-strings 
       (remove-duplicates (map symbol->string (helper SS)))))))
  
  ; state alphabet (listof rules) --> (listof (listof string))
  (define (compute-super-states start sigma rules)
    
    ; (listof (listof state)) (listof (listof state)) (listof (listof state)) --> (listof (listof state))
    (define (bfs visited tovisit SS)
      (if (null? tovisit)
          SS
          (bfs (cons (car SS) visited) 
               (cdr tovisit) 
               (append (compute-supers (car SS) sigma (append visited tovisit)) SS))))
    
    ; (listof state) alphabet (listof (listof state)) --> (listof state)
    (define (compute-supers SS sigma genSS)
      (map (lambda (a) (compute-superS SS a genSS)) sigma))
    
    ; (listof state) symbol (listof (listof state)) --> (listof state)
    (define (compute-superS SS a genSS)
      (let ((superS (append-map (lambda (s) (reachable-states s a genSS)) SS)))
        (begin
          ;(print superS) (newline)
        (if (member superS genSS) null superS)))  )
    
    ; state symbol (listof (listof state)) --> (listof state)
    (define (reachable-states s a genSS)
      (let ((rls (filter (lambda (r) (and (eq? (car r) s) (eq? (cadr r) a)))
                         rules)))
        (map (lambda (s) (empties s rules)) (map caddr rls))))
    
    (bfs '() (list (empties start rules)) (list (empties start rules))))
               
               
  ;;; superstates
  
  ; (listof (superstate symbol superstate) --> (list superstate)
  (define (extract-sstates rls)
    (remove-duplicates (append-map (lambda (r) (list (car r) (caddr r))) rls)))
  
  ; (listof (listof state)) --> (listof (listof state))
  (define (extract-final-ss sts finals)
    (define (has-final-state? SS)
      (ormap (lambda (s) (member s finals)) SS))
    (filter (lambda (SS) (has-final-state? SS)) sts))
  
  ; superstate --> state
  (define (superstate->state ss)
    (string->symbol (lostr->string (los->lostr ss))))
  
  )