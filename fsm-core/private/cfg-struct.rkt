#lang racket/base

(require "constants.rkt"
         "misc.rkt"
         "word.rkt")

(provide (struct-out cfg-rule)
         (struct-out cfg)
         make-cfg
         make-unchecked-cfg
         cfg-get-v
         cfg-get-alphabet
         cfg-get-rules
         cfg-get-start
         parse-cfg-rules
         fsmlos->symbol
         cfg-get-the-rules
         )
; A cfg-rule is a structure, (make-srule L R), where L is a symbol (non-terminal) and R
; is a (listof symbol).
(struct cfg-rule (lhs rhs) #:transparent) ; simple rule

; A cfg is a structure, (cfg V sigma R S), where V and sigma are a (listof symbol), R
; is a (listof cfg-rule), and S is a symbol
(struct cfg (v sigma rules s) #:transparent)

;(listof (list symbol '-> symbol)) --> (listof cfg-rule)
(define (parse-cfg-rules R)
  (map (lambda (r) (cfg-rule (car r) (symbol->fsmlos (caddr r)))) R))

; (listof symbol) (listof symbol) (listof (list symbol '-> symbol)) symbol --> cfg
(define (make-cfg V sigma R S)
  (cfg V sigma (parse-cfg-rules R) S))

(define (make-unchecked-cfg v sigma rules s)
  (make-cfg v sigma rules s))


#;(define (cfg-get-the-rules g)  (cfg-rules g))

; cfg --> (listof symbol)
(define cfg-get-v cfg-v)
  
; cfg --> (listof symbol)
(define cfg-get-alphabet cfg-sigma)

(define (fsmlos->symbol l) 
  (define (lostr->string l)
    (cond [(null? l) ""]
          [else (string-append (car l) (lostr->string (cdr l)))]))
  (string->symbol (lostr->string (map symbol->string l))))

; (listof cfg-rule) --> (listof (list symbol -> symbol))
(define (unparse-cfg-rules rls)
  (map (lambda (r) (list (cfg-rule-lhs r) ARROW (fsmlos->symbol (cfg-rule-rhs r)))) rls))
 
; cfg --> (listof (list symbol -> symbol))
(define (cfg-get-rules g)  (unparse-cfg-rules (cfg-rules g)))
  
; cfg --> (listof cfg-rule)
(define (cfg-get-the-rules g)  (cfg-rules g))
  
; cfg --> symbol
(define cfg-get-start cfg-s)