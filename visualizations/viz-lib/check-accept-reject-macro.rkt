#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/sm-apply.rkt"
         "../../fsm-core/private/tm.rkt"
         "viz-state.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")

(provide check-accept check-reject)


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can accept/process a given word
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M w)
     #'(if (equal? (sm-apply M w) 'accept)
           #t
           #f)]
    [(_ M w n)
     #'(if (equal? (sm-apply M w n) 'accept)
           #t
           #f)]))


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine cannot accept/process a given word
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M w)
     #'(if (equal? (sm-apply M w) 'reject)
           #t
           #f)]
    [(_ M w n)
     #'(if (equal? (sm-apply M w n) 'reject)
           #t
           #f)]))