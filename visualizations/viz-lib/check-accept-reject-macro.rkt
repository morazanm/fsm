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


;; check-accept M w [n] - returns #t if machine accepts, #f if rejects
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M w)
     #'(if (check-equal? (sm-apply M w) 'accept)
           #t
           #f)]
    [(_ M w n)
     #'(if (string? (tm-apply M w n))
           #f
           #t)]))


;; check-reject M w [n] - returns #t if machine accepts, #f if rejects
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M w)
     #'(if (check-equal? (sm-apply M w) 'accept)
           #f
           #t)]
    [(_ M w n)
     #'(if (check-equal? (not (string? (tm-apply M w n))) #t)
           #f
           #t)]))