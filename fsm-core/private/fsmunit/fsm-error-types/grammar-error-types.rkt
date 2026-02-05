#lang racket/base

(require "base-fsm-error-types.rkt"
         "../syntax-value-struct.rkt")

(provide warn:fsm:app:gmr?
         (struct-out warn:fsm:app:gmr:invalid-nt)
         (struct-out warn:fsm:app:gmr:invalid-word)
         (struct-out warn:fsm:app:gmr:accept)
         (struct-out warn:fsm:app:gmr:reject))

(struct warn:fsm:app:gmr warn:fsm:app ())

(struct warn:fsm:app:gmr:reject warn:fsm:app:gmr ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, derives the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~s" val)))
            (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, derives the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar derives the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~s" val)))
            "Step 6 of the design recipe has not been successfully completed. The constructed grammar derives the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])

(struct warn:fsm:app:gmr:accept warn:fsm:app:gmr ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, does not derive the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~s" val)))
            (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, does not derive the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar does not derive the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~s" val)))
            "Step 6 of the design recipe has not been successfully completed. The constructed grammar does not derive the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])

(struct warn:fsm:app:gmr:invalid-nt warn:fsm:app:gmr ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 4 of the design recipe has not been successfully completed. The following word contains terminals not in the constructed grammar, ~s's alphabet :\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 4 of the design recipe has not been successfully completed. The following words contain terminals not in the constructed grammar, ~s's alphabet:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 4 of the design recipe has not been successfully completed. The following word contains terminals not in the constructed grammar's alphabet :\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 4 of the design recipe has not been successfully completed. The following words contain terminals not in the constructed grammar's alphabet:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])

(struct warn:fsm:app:gmr:invalid-word warn:fsm:app:gmr ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 4 of the design recipe has not been successfully completed. The following test case for the constructed grammar, ~s, is not a word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 4 of the design recipe has not been successfully completed. The following test cases for the constructed grammar, ~s, are not words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 4 of the design recipe has not been successfully completed. The following test case for the constructed grammar is not a word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 4 of the design recipe has not been successfully completed. The following test cases for the constructed grammar are not words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])