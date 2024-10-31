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


;; check-accept
(define-check (check-accept? M w line column)
  (unless (equal? (sm-apply M w) 'accept)
    (with-check-info*
        (list (make-check-name 'check-accept)
              (make-check-location (list 'check-accept line column #f #f)))
      (lambda () (fail-check)))
    ))


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can accept/process a given word
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M w)
     #`(check-accept? M w #,(syntax-line stx) #,(syntax-column stx))]
    #;[(_ M w n)
       #'(check-accept? M w n)]))


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine cannot accept/process a given word
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M w)
     #'(check-equal? (sm-apply M w) 'reject)]
    [(_ M w n)
     #'(check-equal? (sm-apply M w n) 'reject)]))