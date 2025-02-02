#lang racket/base

;; ---------------------------------------------------------------------------------------------------
;; provides constants and functions for specifying the shape of clauses in big-bang and universe 

(provide
 port> nat> opt-nat> nat? proc> bool> num> ip> string> symbol> display-mode> string-or-symbol> any>
 K False True)

(require racket/format racket/string htdp/error "check-aux.rkt")

(define (K w . r) w)
(define (False w) #f)
(define (True w) #t)

;; Symbol X -> X : nat?
(define (port> tag x)
  (nat> tag x "port"))

;; Symbol X -> X : boolean? 
(define (bool> tag x)
  (check-arg tag (boolean? x) "boolean" "first" x)
  x)

;; Symbol X -> X : string?
(define (string> tag x)
  (check-arg tag (string? x) "string" "first" x)
  x)

;; Symbol X -> X : symbol? 
(define (symbol> tag x)
  (check-arg tag (symbol? x) "symbol" "second" x)
  x)

(define DMODE '(normal fullscreen))

;; Symbol X -> X : symbol? 
(define (display-mode> tag x)
  (define d (map (λ (x) (format "'~a" x)) DMODE))
  (define dd (string-append "display mode [" (string-join d ", ")  "]"))
  (check-arg tag (and (symbol? x) (memq x DMODE)) dd "first" x)
  x)

;; Symbol X -> X : symbol?  or string? 
(define (string-or-symbol> tag x)
  (check-arg tag (or (symbol? x) (string? x)) "symbol or string" "first" x)
  x)

(define ip> string>)

;; Symbol X Nat -> X
(define (proc> tag f ar #:place (place "first"))
  (check-proc tag f ar place (if (> ar 1) (format "~a arguments" ar) "one argument"))
  f)

;; Symbol X (Number -> Boolean) String String -> X
(define (num> tag x pred? spec which)
  (check-arg tag (and (number? x) (pred? x)) spec which x)
  x)

;; Symbol X String -> X
(define (nat> tag x spec)
  (check-arg tag (nat? x) "natural number" spec x)
  x)

;; Symbol X String -> X
(define (opt-nat> tag x)
  (check-arg tag (or (boolean? x) (nat? x)) "boolean or natural number" "first" x)
  x)

;; Symbol X String -> X
(define (any> tag x)
  x)
