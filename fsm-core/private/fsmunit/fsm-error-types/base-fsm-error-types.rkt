#lang racket/base
(require racket/generic
         racket/match
         racket/syntax-srcloc
         "../syntax-value-struct.rkt")

(provide (struct-out warn)
         (struct-out warn:fsm)
         (struct-out warn:fsm:app)
         gen:printable-fsm-err-vals
         named-multi-failure
         named-single-failure
         anonymous-multi-failure
         anonymous-single-failure
         create-err-str
         display-fsm-err)

(struct warn ())
(struct warn:fsm warn (fsm-expr))
(struct warn:fsm:app warn:fsm (err-exprs))

(struct exn:fail:check-failed exn:fail:user
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(exn:fail:check-failed msg marks (list a-srcloc ...))
       a-srcloc])))

(define (display-warning desc exn)
  ((error-display-handler) desc exn))

(define-generics printable-fsm-err-vals
  (named-multi-failure printable-fsm-err-vals)
  (named-single-failure printable-fsm-err-vals)
  (anonymous-multi-failure printable-fsm-err-vals)
  (anonymous-single-failure printable-fsm-err-vals)
  (create-err-str printable-fsm-err-vals)
  (display-fsm-err printable-fsm-err-vals)
  #:fallbacks [(define/generic named-multi-failure0 named-multi-failure)
               (define/generic named-single-failure0 named-single-failure)
               (define/generic anonymous-multi-failure0 anonymous-multi-failure)
               (define/generic anonymous-single-failure0 anonymous-single-failure)
               (define (create-err-str err-struct)
                 (cond [(warn:fsm:app? err-struct)
                        (if (identifier? (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
                            (if (null? (cdr (warn:fsm:app-err-exprs err-struct)))
                                (named-single-failure0 err-struct)
                                (named-multi-failure0 err-struct))
                            (if (null? (cdr (warn:fsm:app-err-exprs err-struct)))
                                (anonymous-single-failure0 err-struct)
                                (anonymous-multi-failure0 err-struct)))]
                       [(warn:fsm? err-struct)
                        (if (identifier? (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
                            (named-single-failure0 err-struct)
                            (anonymous-single-failure0 err-struct))]
                       [else (raise-user-error "Invalid error type raised. Please contact FSM developers if you see this message.")]))
               (define (display-fsm-err err-struct)
                 (cond [(warn:fsm:app? err-struct)
                        (let ([str (create-err-str err-struct)])
                          (display-warning str (exn:fail:check-failed
                                                str
                                                (current-continuation-marks)
                                                (map (lambda (x) (syntax-srcloc (val-stx-pair-stx x)))
                                                     (warn:fsm:app-err-exprs err-struct)))))]
                       [(warn:fsm? err-struct)
                        (let ([str (create-err-str err-struct)])
                          (display-warning str (exn:fail:check-failed
                                                str
                                                (current-continuation-marks)
                                                (list (syntax-srcloc (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))))))]
                       [else (raise-user-error "Invalid error type raised. Please contact FSM developers if you see this message.")]))])