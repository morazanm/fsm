#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     racket/match
                     syntax/to-string
                     "viz-state.rkt"
                     racket/struct-info)
         syntax/to-string
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/sm-apply.rkt"
         "../../fsm-core/private/tm.rkt"
         "viz-state.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")

(provide check-accept check-reject)


(struct exn:fail:check-accept-failed exn:fail
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:check-accept-failed msg marks a-srcloc)
       (list a-srcloc)])))


;; check-accept
(define-check (check-accept? M w line column)
  (unless (equal? (sm-apply M w) 'accept)
    (with-check-info*
        (list (make-check-name 'check-accept)
              (make-check-location (list 'check-accept line column #f #f)))
      (lambda () (begin
                   (fail-check (format "The machine does not accept ~s." w))
                   #f))
      )))


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can accept/process a given word
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M w)
     (quasisyntax/loc
         stx
       (unless (not (check-accept? M w #,(syntax-line stx)
                                   #,(syntax-column stx)))
         (lambda () (error-display-handler))
         #;(raise (exn:fail:check-accept-failed
                   "dummyyyyy"
                   (current-continuation-marks)
                   (srcloc '#,(syntax-source #'stx)
                           '#,(syntax-line #'stx)
                           '#,(syntax-column #'stx)
                           '#,(syntax-position #'stx)
                           '#,(syntax-span #'stx))))))]
            
    #;[(_ M w n)
       #'(check-accept? M w n)]))

;;

;; check-accept
(define-check (check-reject? M w line column)
  (unless (equal? (sm-apply M w) 'reject)
    (with-check-info*
        (list (make-check-name 'check-reject)
              (make-check-location (list 'check-reject line column #f #f))
              (make-check-params (list M w))
              (make-check-message (format "The machine does not reject ~s" w)))
      (lambda () (fail-check)))
    ))


;; machine word [head-pos] -> Boolean
;; Purpose: To determine whether a given machine can reject a given word
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M w)
     #`(check-reject? M w #,(syntax-line stx) #,(syntax-column stx))]
    #;[(_ M w n)
       #'(check-reject? M w n)]))


