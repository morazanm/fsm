#lang racket/base
(require "base-fsm-error-types.rkt"
         "../syntax-value-struct.rkt")

(provide warn:fsm:app:sm?
         (struct-out warn:fsm:app:sm:accept)
         (struct-out warn:fsm:app:sm:reject)
         (struct-out warn:fsm:app:sm:invalid-nt)
         (struct-out warn:fsm:app:sm:invalid-word))

(struct warn:fsm:app:sm warn:fsm:app ())

(struct warn:fsm:app:sm:accept warn:fsm:app:sm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (named-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))
   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (anonymous-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:sm:reject warn:fsm:app:sm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (named-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:sm:invalid-nt warn:fsm:app:sm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine, ~s:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine, ~s:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])

(struct warn:fsm:app:sm:invalid-word warn:fsm:app:sm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, is not a list of symbols in the machine's alphabet:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, are not a list of symbols in the machine's alphabet:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case is not a list of symbols in the machine's alphabet:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 2 of the design recipe has not been successfully completed. The following test cases are not a list of symbols in the machine's alphabet:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])