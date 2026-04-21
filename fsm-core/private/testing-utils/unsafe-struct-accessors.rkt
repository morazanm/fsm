#lang racket/base
(require racket/performance-hint
         racket/unsafe/ops
         (only-in "structs.rkt"
                  config
                  stack
                  PDA-rule
                  computation
                  PATH))
(provide config
         stack
         PDA-rule
         computation
         PATH
         (all-defined-out))

(define-inline (unsafe-config-state a-config)
  (unsafe-struct*-ref a-config 0))

(define-inline (unsafe-config-word a-config)
  (unsafe-struct*-ref a-config 1))

(define-inline (unsafe-config-stack a-config)
  (unsafe-struct*-ref a-config 2))

(define-inline (unsafe-PATH-lor a-path)
  (unsafe-struct*-ref a-path 0))

(define-inline (unsafe-PATH-stack a-path)
  (unsafe-struct*-ref a-path 1))

(define-inline (unsafe-PATH-word a-path)
  (unsafe-struct*-ref a-path 2))

(define-inline (unsafe-PATH-path-length a-path)
  (unsafe-struct*-ref a-path 3))

(define-inline (unsafe-PATH-destination-state a-path)
  (unsafe-struct*-ref a-path 4))

(define-inline (unsafe-stack-elems a-path)
  (unsafe-struct*-ref a-path 0))

(define-inline (unsafe-stack-len a-path)
  (unsafe-struct*-ref a-path 1))

(define-inline (unsafe-PDA-rule-source a-path)
  (unsafe-struct*-ref a-path 0))

(define-inline (unsafe-PDA-rule-read a-path)
  (unsafe-struct*-ref a-path 1))

(define-inline (unsafe-PDA-rule-pop a-path)
  (unsafe-struct*-ref a-path 2))

(define-inline (unsafe-PDA-rule-destination a-path)
  (unsafe-struct*-ref a-path 3))

(define-inline (unsafe-PDA-rule-push a-path)
  (unsafe-struct*-ref a-path 4))

(define-inline (unsafe-PDA-rule-push-len a-path)
  (unsafe-struct*-ref a-path 5))

(define-inline (unsafe-PDA-rule-pop-length a-path)
  (unsafe-struct*-ref a-path 6))

(define-inline (unsafe-computation-loc a-config-path)
  (unsafe-struct*-ref a-config-path 0))

(define-inline (unsafe-computation-length a-config-path)
  (unsafe-struct*-ref a-config-path 1))