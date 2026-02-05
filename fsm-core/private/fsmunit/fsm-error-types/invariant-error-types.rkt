#lang racket/base

(require "base-fsm-error-types.rkt"
         "../syntax-value-struct.rkt")

(provide warn:fsm:app:inv?
         (struct-out warn:fsm:app:inv:accept)
         (struct-out warn:fsm:app:inv:reject)
         (struct-out warn:fsm:app:inv:invalid-words)
         (struct-out warn:fsm:app:inv:invalid-pred))

(struct warn:fsm:app:inv warn:fsm:app ())

(struct warn:fsm:app:inv:invalid-pred warn:fsm:app:inv ()
  #:methods gen:printable-fsm-err-vals
  [#;(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "The design recipe has not been successfully completed. The invariant predicate, ~s, does not hold for the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (named-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The procedure, ~s, is not a valid invariant predicate."
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))))
   #;(define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "The design recipe has not been successfully completed. The invariant predicate does not hold for the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (anonymous-single-failure err-struct)
     "The design recipe has not been successfully completed. The procedure is not a valid invariant predicate.")])

(struct warn:fsm:app:inv:accept warn:fsm:app:inv ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "The design recipe has not been successfully completed. The invariant predicate, ~s, does not hold for the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (named-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The invariant predicate, ~s, does not hold for the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))
   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "The design recipe has not been successfully completed. The invariant predicate does not hold for the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (anonymous-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The invariant predicate does not hold for the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:inv:reject warn:fsm:app:inv ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "The design recipe has not been successfully completed. The invariant predicate, ~s, holds for the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (named-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The invariant predicate, ~s, holds for the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))
   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "The design recipe has not been successfully completed. The invariant predicate holds for the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (anonymous-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The invariant predicate holds for the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:inv:invalid-words warn:fsm:app:inv ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The following test case for the invariant predicate, ~s, is not a list of symbols:\n~a"
             (syntax-e (warn:fsm-fsm-expr err-struct))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "The design recipe has not been successfully completed. The following test cases for the invariant predicate, ~s, are not a list of symbols:"
                    (syntax-e (warn:fsm-fsm-expr err-struct)))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "The design recipe has not been successfully completed. The following test case is not a list of symbols:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "The design recipe has not been successfully completed. The following test cases are not a list of symbols:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])