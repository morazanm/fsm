; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

;;;; RULES

; A fsarule is (list state symbol state).

; A list of fsarules (lorules) is either
;  1. empty
;  2. (cons fsarule lorules)

; A superstate rule (ssr) is a (list superstate symbol superstate).

(module rules racket
  (require "string.rkt" "constants.rkt" "misc.rkt")
  
  (provide to-state-fsarule from-state-fsarule symb-fsarule get-applicable-fsarules 
           empty-rules? new-dead-rules filter-rules convert2rules mk-fsarule)
  
  (define (mk-fsarule fs s ts) (list fs s ts))
  
  (define to-state-fsarule caddr)
  
  (define from-state-fsarule car)
  
  (define symb-fsarule cadr)
  
  (define empty-rules? null?)
  
  (define (get-applicable-fsarules symb state rules)
    (filter (lambda (r) 
              (and (eq? (from-state-fsarule r) state) 
                   (or (eq? (symb-fsarule r) symb) (eq? (symb-fsarule r) EMP))))
            rules))
  
  ; (listof state) alphabet (listof rule) --> (listof rule)
  (define (new-dead-rules states sigma rules)
    ; state alphabet --> (listof pair)
    (define (create-s-pairs s sigma) (map (lambda (a) (list s a)) sigma))
    ; (listof state) alphabet --> (listof pair)
    (define (create-pairs states sigma) (append-map (lambda (s) (create-s-pairs s sigma)) states))
    
    (let ((pairs (create-pairs states sigma)))
      (append-map (lambda (p) 
                    (let ((rls (filter (lambda (r) (and (eq? (car p) (from-state-fsarule r))
                                                        (eq? (cadr p) (symb-fsarule r))))
                                       rules)))
                      (if (empty-rules? rls)
                          (list (list (car p) (cadr p) DEAD))
                          null)))
                  pairs)))
  
  ; string symbol (listof rule) --> (listof rule)
  (define (filter-rules s symb rules)
    (filter (lambda (r) 
              (and (equal? s (symbol->string (car r))) (eq? (cadr r) symb))) rules))
  
  ; (listof ssr) -> (listof rule)
  (define (convert2rules rls)
    (map (lambda (r) (list (los->symbol (car r))
                           (cadr r)
                           (los->symbol (caddr r))))
         rls))
  
  
  )